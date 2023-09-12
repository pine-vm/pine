using ElmTime.NativeDependency;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

namespace ElmTime.Git;

public class LibGit2Sharp
{
    private static readonly IReadOnlyDictionary<OSPlatform, IReadOnlyList<DependencyFile>> DependenciesFilesByOs =
        ImmutableDictionary<OSPlatform, IReadOnlyList<DependencyFile>>.Empty
        .Add(
            OSPlatform.Linux,
            [
                new DependencyFile(
                    HashBase16: "7ca026cf714e14fbab252d83974c04b843affe035e041aa1eda0d0bc258426be",
                    ExpectedFileName: "libgit2-e632535.so",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/LibGit2Sharp.NativeBinaries/2.0.320"])])
        .Add(
            OSPlatform.Windows,
            [
                new DependencyFile(
                    HashBase16: "76b97b411e73b7487825dd0f98cba7ce7f008bef3cbe8a35bf1af8c651a0f4a0",
                    ExpectedFileName: "git2-e632535.dll",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/LibGit2Sharp.NativeBinaries/2.0.320"])])
        .Add(
            OSPlatform.OSX,
            [
                new DependencyFile(
                    HashBase16: "9dc84237ca835b189636697cbe1439b0894303798efd95b55a3353ca4f12b1bb",
                    ExpectedFileName: "libgit2-e632535.dylib",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/LibGit2Sharp.NativeBinaries/2.0.320"])]);

    public static readonly Lazy<Task> SetupTask = new(() =>
    {
        EnsureNativeLibrariesAvailableForCurrentPlatform();
        return Task.CompletedTask;
    });

    public static void EnsureNativeLibrariesAvailableForCurrentPlatform()
    {
        var setupForCurrentOs =
            DependenciesFilesByOs.FirstOrDefault(c => RuntimeInformation.IsOSPlatform(c.Key)).Value
            ??
            throw new Exception("Unknown OS: " + RuntimeInformation.OSDescription);

        var cacheDirectory = Path.GetDirectoryName(DotNetAssembly.ProgramExecutableFileName.Value)!;

        foreach (var dependency in setupForCurrentOs)
        {
            NativeDependencies.SetUpDependency(
                cacheDirectory: cacheDirectory,
                dependency: dependency);
        }
    }
}
