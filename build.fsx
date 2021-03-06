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
open System.Text.RegularExpressions
open System.IO

// ===== Properties =====

let projectRoot = __SOURCE_DIRECTORY__

// ===== Utility functions =====

let findSemver (changes : Changelog.Change list) =
    changes |> List.filter (fun s -> s.ToString().Contains("semver:"))

let splitLines (text : string) =
    text
    |> String.convertTextToWindowsLineBreaks
    |> String.splitStr String.WindowsLineBreaks

let semVerRe = Regex(@"^\+semver:\s?(\S+)$")

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
    with :? FileNotFoundException ->
        None

let bumpChangelog (filename : string) (newFilename : string) =
    match maybeLoadChangelog filename with
    | None -> ()  // No changelog file? Then there's nothing to modify
    | Some changelog ->
        let i = semverInstruction changelog
        if i = "none" || i = "skip" then
            // We process "+semver: none/skip" as meaning "don't touch the changelog"
            // To change this, just remove the "if" statement and dedent the "else" block by one indent level
            filename |> Shell.copyFile newFilename
        else
            changelog
            |> adjustUnreleasedDescription
            |> Changelog.promoteUnreleased (nextVer changelog).AsString
            |> Changelog.save newFilename

let mostRecentTag (projectDir : DirectoryInfo) =
    let recentTags =
        let tagPattern1 = sprintf "%s-v*" projectDir.Name
        let tagPattern2 = sprintf "%s-*"  projectDir.Name
        let _, result1, _ = runGitCommand projectRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern1)
        if not (result1 |> List.isEmpty) then result1 else
        let _, result2, _ = runGitCommand projectRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern2)
        result2
    recentTags |> List.tryHead

let mostRecentVersionFromTag (projectDir : DirectoryInfo) =
    match mostRecentTag projectDir with
    | None -> None
    | Some tag ->
        let cnt = projectDir.Name.Length
        if tag |> String.startsWith (sprintf "%s-v" projectDir.Name) then
            Some tag.[cnt+2..]
        else
            Some tag.[cnt+1..]

let commitsSinceLastTag (projectDir : DirectoryInfo) =
    let relPath = projectDir.FullName |> Path.toRelativeFrom projectRoot |> fixPath
    let mostRecentTags =
        let tagPattern1 = sprintf "%s-v*" projectDir.Name
        let tagPattern2 = sprintf "%s-*"  projectDir.Name
        let _, result1, _ = runGitCommand projectRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern1)
        if not (result1 |> List.isEmpty) then result1 else
        let _, result2, _ = runGitCommand projectRoot (sprintf "tag -l %s --sort=-creatordate" tagPattern2)
        result2
    if mostRecentTags |> List.isEmpty then
        Trace.traceImportant "No tags found - can't calculate prerelease version"
        None
    else
        let recentTag = mostRecentTags |> List.head
        let commitCount = runSimpleGitCommand projectRoot (sprintf "rev-list --count %s..HEAD -- %s" recentTag relPath)
        Some commitCount

let calcNextVersion (projectDir : DirectoryInfo) =
    let changelogFname = projectDir.FullName @@ "CHANGELOG.md"  // TODO: Should we parameterize this?
    let changelogVersion =
        match maybeLoadChangelog changelogFname with
        | None -> None
        | Some ch when ch.Entries |> List.isEmpty -> None
        | Some ch -> (nextVer ch).AsString |> Some
    match changelogVersion with
    | Some v -> v
    | None ->
        match mostRecentVersionFromTag projectDir with
        | Some v -> v
        | None -> "0.0.1"

let calcNextPrereleaseVersion (projectDir : DirectoryInfo) =
    Trace.tracefn "calcPrereleaseVersion %s" projectDir.FullName
    let version = calcNextVersion projectDir
    let commitCount =
        match commitsSinceLastTag projectDir with
        | Some n -> n
        | None ->
            if version = "0.0.1" then
                // Get all tags since history of repo started
                runSimpleGitCommand projectRoot "rev-list --count HEAD"
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

// ===== Build targets =====

Target.create "Alpha" (fun _ ->
    Trace.trace "Figuring out alpha versions..."
    for proj in !! "**/*.csproj" do
        let dir = (Fake.IO.FileInfo.ofPath proj).Directory
        Trace.traceImportantfn "Prerelease version for project %s would be %s" dir.Name (calcNextPrereleaseVersion dir)
)

Target.create "Default" (fun _ ->
    Trace.trace "Figuring out release versions..."
    for proj in !! "**/*.csproj" do
        let dir = (Fake.IO.FileInfo.ofPath proj).Directory
        Trace.traceImportantfn "Release version for project %s would be %s" dir.Name (calcNextVersion dir)
)

Target.create "BumpChangelog" (fun _ ->
    Trace.trace "Writing updated changelog to NEW_CHANGELOG.md in all project directories..."
    for proj in !! "**/*.csproj" do
        let dir = (Fake.IO.FileInfo.ofPath proj).Directory
        let chFname = dir.FullName @@ "CHANGELOG.md"
        let newChFname= dir.FullName @@ "NEW_CHANGELOG.md"
        bumpChangelog chFname newChFname
        Trace.trace "\nOriginal changelog:"
        Trace.logItems "" (File.ReadAllLines(chFname))
        Trace.trace "Updated changelog:"
        Trace.logItems "" (File.ReadAllLines(newChFname))
)

Target.runOrDefault "Default"
