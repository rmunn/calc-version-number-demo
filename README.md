## To build

First make sure you have the dotnet SDK 2.1.300 or later (2.2 works too).
Also make sure that $HOME/.dotnet/tools is on your PATH.

Now install the FAKE (F# Make) CLI tool by running:

    dotnet tool install fake-cli -g

And now you should be able to run the `build.fsx` script by running:

    fake build -t TargetName

To build the default target:

    fake build

If you edit the NuGet dependencies of the build script, make sure to remove
the build.fsx.lock file; this will cause a NuGet restore to be re-run when you
next run the script.
