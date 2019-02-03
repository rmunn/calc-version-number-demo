#nowarn "25"
#r "paket:
nuget FSharp.Core < 4.6
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.Filesystem
nuget Fake.Tools.Git
nuget Fake.Core.Target //"

#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools.Git.CommandHelper

let findSemver (changes : Changelog.Change list) =
    changes |> List.filter (fun s -> s.ToString().Contains("semver:"))

let splitLines (text : string) =
    text
    |> String.convertTextToWindowsLineBreaks
    |> String.splitStr String.WindowsLineBreaks

let semVerRe = System.Text.RegularExpressions.Regex @"^\+semver:\s?(\S+)$"

let semverInstruction (ch : Changelog.Changelog) =
    match ch.Unreleased with
    | None -> "patch"
    | Some unreleased ->
        match unreleased.Description with
        | None -> "patch"
        | Some desc ->
            let x = semVerRe.Match desc
            if x.Success then
                x.Groups.[1].Value.ToLowerInvariant()
            else
                "patch"

let adjustUnreleasedDescription (changelog : Changelog.Changelog) =
    match changelog.Unreleased with
    | None -> changelog
    | Some unreleased ->
        match unreleased.Description with
        | None -> changelog
        | Some desc ->
            let m = semVerRe.Match desc
            if m.Success then
                let descWithoutSemver =
                    desc
                    |> splitLines
                    |> List.filter (fun line -> not (semVerRe.IsMatch line))
                    |> String.toLines
                let newUnreleased = { unreleased with Description = Some descWithoutSemver }
                let newChangelog =
                    Changelog.Changelog.New (changelog.Header, changelog.Description, Some newUnreleased, changelog.Entries)
                newChangelog
            else
                changelog

let incMajor (ver : SemVerInfo) = { ver with Major = ver.Major + 1u; Minor = 0u; Patch = 0u; Original = None }
let incMinor (ver : SemVerInfo) = { ver with Minor = ver.Minor + 1u; Patch = 0u; Original = None }
let incPatch (ver : SemVerInfo) = { ver with Patch = ver.Patch + 1u; Original = None }

let nextVer (ch : Changelog.Changelog) =
    let ver = SemVer.parse ch.LatestEntry.NuGetVersion
    match semverInstruction ch with
    | "breaking" | "major" -> incMajor ver
    | "feature"  | "minor" -> incMinor ver
    | "fix"      | "patch" -> incPatch ver
    | "none"     | "skip"  -> ver
    | other ->
        Trace.traceImportantfn "Warning: unrecognized semver command %s, assuming \"patch\"" other
        incPatch ver

let maybeLoadChangelog (filename : string) =
    try
        Changelog.load filename |> Some
    with :? System.IO.FileNotFoundException ->
        None

let bumpChangelog (filename : string) =
    try
        let changelog = Changelog.load filename
        let i = semverInstruction changelog
        if i = "none" || i = "skip" then
            ()
        else
            changelog
            |> adjustUnreleasedDescription
            |> Changelog.promoteUnreleased (nextVer changelog).AsString
            |> Changelog.save filename
    with :? System.IO.FileNotFoundException ->
        ()  // No changelog file? Then there's nothing to modify

let findProjectRoot fromDir =
    try
        let gitDir = findGitDir fromDir
        Some (gitDir.Parent.FullName)
    with :? System.NullReferenceException ->
        None

let mostRecentTag (projectDir : System.IO.DirectoryInfo) =
    let maybeProjRoot = findProjectRoot projectDir.FullName
    let recentTags =
        match maybeProjRoot with
        | None -> []
        | Some projRoot ->
            let relPath = projectDir.FullName |> Path.toRelativeFrom projRoot |> fixPath
            let tagPattern1 = sprintf "%s-v*" projectDir.Name
            let tagPattern2 = sprintf "%s-*"  projectDir.Name
            let _, result1, _ = runGitCommand projRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern1)
            if not (result1 |> List.isEmpty) then result1 else
            let _, result2, _ = runGitCommand projRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern2)
            result2
    recentTags |> List.tryHead

let mostRecentVersionFromTag (projectDir : System.IO.DirectoryInfo) =
    match mostRecentTag projectDir with
    | None -> None
    | Some tag ->
        let cnt = projectDir.Name.Length
        if tag |> String.startsWith (sprintf "%s-v" projectDir.Name) then
            Some tag.[cnt+2..]
        else
            Some tag.[cnt+1..]

let commitsSinceLastTag (projectDir : System.IO.DirectoryInfo) =
    match findProjectRoot projectDir.FullName with
    | None -> None
    | Some projRoot ->
        let relPath = projectDir.FullName |> Path.toRelativeFrom projRoot |> fixPath
        let mostRecentTags =
            let tagPattern1 = sprintf "%s-v*" projectDir.Name
            let tagPattern2 = sprintf "%s-*"  projectDir.Name
            let _, result1, _ = runGitCommand projRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern1)
            if not (result1 |> List.isEmpty) then result1 else
            let _, result2, _ = runGitCommand projRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern2)
            result2
        if mostRecentTags |> List.isEmpty then
            Trace.traceImportant "No tags found here"
            None
        else
            let recentTag = mostRecentTags |> List.head
            let commitCount = runSimpleGitCommand projRoot (sprintf "rev-list --count %s..HEAD -- %s" recentTag relPath)
            Trace.tracefn "Filtered count: %A" commitCount
            Some commitCount

let calcVersion (projectDir : System.IO.DirectoryInfo) =
    let changelogFname = projectDir.FullName @@ "CHANGELOG.md"  // Useful elsewhere
    let changelogVersion =
        match maybeLoadChangelog changelogFname with
        | None -> None
        | Some ch when ch.Entries |> List.isEmpty -> None
        | Some ch -> ch.LatestEntry.SemVer.NormalizeToShorter() |> Some  // TODO: Or just .SemVer if we want a SemVerInfo type here
    match changelogVersion with
    | Some v -> v
    | None ->
        match mostRecentVersionFromTag projectDir with
        | Some v -> v
        | None -> "0.0.1"

let calcPrereleaseVersion (projectDir : System.IO.DirectoryInfo) =
    Trace.tracefn "calcPrereleaseVersion %s" projectDir.FullName
    let version = calcVersion projectDir
    let commitCount =
        match commitsSinceLastTag projectDir with
        | Some n -> n
        | None ->
            if version = "0.0.1" then
                // Get all tags since history of repo started
                Trace.tracefn "findProjectRoot projectDir.FullName returns %A" (findProjectRoot projectDir.FullName)
                match findProjectRoot projectDir.FullName with
                | None -> ""
                | Some projRoot ->
                    runSimpleGitCommand projRoot "rev-list --count HEAD"
            else
                // How do we determine how many commits since a specific version... if there's no tag for that version? Answer: We can't, not reliably
                let projName = projectDir.Name
                Trace.traceErrorfn "Could not calculate prerelease number for version %s of project %s because no tag named %s-v%s or %s-%s exists. Create an appropriately named tag in the repo and try again."
                                   version projName projName version projName version
                failwith <| sprintf "Create tag named %s-%s or %s-v%s on an appropriate commit and run build again" projName version projName version
    if commitCount = "" then version else sprintf "%s-alpha%s" version (commitCount.PadLeft(4, '0'))
    // TODO: Change that to `sprintf "%s-alpha.%s" version commitCount` once we're sure we can use SemVer 2.0.0 (i.e., everyone consuming this has NuGet 4.3 or later)

(*
Logic we're looking for:

1. For ordinary builds, look at changelog to get previous version.
   Then calculate number of git commits since that version which touch
   this project's directory. That gives you the -alpha0001 part of the NuGet version.
   This is done with calcPrereleaseVersion
2. When you run the Release target, the changelog is bumped and rewritten.
   Then a NuGet package version *without* -alpha#### is created.
   This is done with calcVersion
*)

Target.create "Alpha" (fun _ ->
    Trace.trace "Figuring out alpha versions..."
    for proj in !! "**/*.csproj" do
        let dir = (Fake.IO.FileInfo.ofPath proj).Directory
        Trace.logfn "Prerelease version for project %s is %s" dir.Name (calcPrereleaseVersion dir)
)

Target.create "Normal" (fun _ ->
    Trace.trace "Figuring out alpha versions..."
    for proj in !! "**/*.csproj" do
        let dir = (Fake.IO.FileInfo.ofPath proj).Directory
        Trace.logfn "Release version for project %s is %s" dir.Name (calcVersion dir)
)

Target.create "Default" (fun _ ->
    Trace.trace "Hello world from build.fsx"
    let gitDir = findGitDir __SOURCE_DIRECTORY__
    Trace.tracefn "Found Git dir in %s" gitDir.FullName
    let ch = Changelog.load "CHANGELOG.md"
    bumpChangelog "FOO.md"
    Trace.tracefn "Last released version found in changelog for %s: %s" ch.Header ch.LatestEntry.NuGetVersion
    // Trace.tracefn "Parsed as: %d.%d.%d" ver.Major ver.Minor ver.Patch

    let pr = 5 |> sprintf "alpha.%d" |> PreRelease.TryParse
    let ver' = { nextVer ch with PreRelease = pr }
    Trace.tracefn "Next version: %s" ver'.AsString
    Trace.tracefn "Normalized: %s" (ver'.Normalize())
    Trace.tracefn "Normalized to shorter: %s" (ver'.NormalizeToShorter() + " and toString: " + ver'.ToString())
)

Target.create "Bar" (fun _ ->
    let maybeProjRoot = findProjectRoot __SOURCE_DIRECTORY__
    match maybeProjRoot with
    | None -> Trace.traceImportant "No git dir found"
    | Some projRoot ->
        let _, msgs, _ = runGitCommandf "%s" projRoot "version"
        Trace.trace "Git version was:"
        for msg in msgs do
            Trace.log msg
)

Target.create "Foo" (fun _ ->
    Trace.tracefn "Running Foo target"
    let projFiles = !! "**/*.csproj"
    let projRoot = findProjectRoot __SOURCE_DIRECTORY__
    if projRoot.IsNone then
        Trace.trace "No Git repo found"
    else
        let projRoot = projRoot.Value
        Trace.tracefn "Project root: %s" projRoot
        for f in projFiles do
            let dir = (Fake.IO.FileInfo.ofPath f).Directory
            Trace.tracefn "Relative path: %s" (dir.FullName |> Path.toRelativeFrom projRoot)
            Trace.tracefn "\"Fixed\" path: %s" (dir.FullName + "/" |> Path.toRelativeFrom projRoot |> Fake.Tools.Git.CommandHelper.fixPath)
            Trace.tracefn "Last part of directory: %s" dir.Name
            // let tag = "v*"
            let tag = sprintf "%s-v*" dir.Name
            // Example of how to look back through tags and pick out the *most recent* tag matching a certain pattern
            let _, mostRecentTags, _ = Fake.Tools.Git.CommandHelper.runGitCommand projRoot <| sprintf "tag -l %s --sort=-creatordate" tag
            if mostRecentTags |> List.isEmpty then
                Trace.tracefn "No tags found"
            else
                Trace.tracefn "Most recent tag matching pattern: %s" (mostRecentTags |> List.head)
                let recentTag = mostRecentTags |> List.head
                let num = Fake.Tools.Git.Branches.revisionsBetween projRoot recentTag "HEAD"
                Trace.tracefn "Looks like %d commits since then" num
                let commitCount = Fake.Tools.Git.CommandHelper.runSimpleGitCommand projRoot <| sprintf "rev-list --count %s..HEAD -- %s" recentTag (dir.FullName |> Path.toRelativeFrom projRoot)
                Trace.tracefn "Filtered count: %A" commitCount
            // TODO: But we might want to look back at when a certain change was made to CHANGELOG.md
)

Target.runOrDefault "Foo"
