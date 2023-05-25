using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using ElmTime.NativeDependency;

namespace ElmTime.JavaScript;

public class ClearScriptV8
{
    private static readonly IReadOnlyDictionary<OSPlatform, IReadOnlyList<DependencyFile>> DependenciesFilesByOs =
        ImmutableDictionary<OSPlatform, IReadOnlyList<DependencyFile>>.Empty
        .Add(
            OSPlatform.Linux,
            ImmutableList.Create(
                new DependencyFile(
                    HashBase16: "0a1aa724e021de144d26ceb846fe13d436db95425e8d2a151ad1583aa9ce0263",
                    ExpectedFileName: "ClearScriptV8.linux-x64.so",
                    RemoteSources: new[] { "https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.linux-x64/7.4.1" })))
        .Add(
            OSPlatform.Windows,
            ImmutableList.Create(
                new DependencyFile(
                    HashBase16: "eeda680f30f6f27ed57a20f41d6208f9022408e66f9e6092864586927fe6e4b4",
                    ExpectedFileName: "ClearScriptV8.win-x64.dll",
                    RemoteSources: new[] { "https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.win-x64/7.4.1" })))
        .Add(
            OSPlatform.OSX,
            ImmutableList.Create(
                new DependencyFile(
                    HashBase16: "3438d1f682e119ad0df6cb37c83c7c53d103d46cc333343de2f1dd5b89ba84ad",
                    ExpectedFileName: "ClearScriptV8.osx-x64.dylib",
                    RemoteSources: new[] { "https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.osx-x64/7.4.1" })));

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

        var cacheDirectory =
            Path.Combine(Filesystem.CacheDirectory, nameof(ClearScriptV8)).TrimEnd(Path.DirectorySeparatorChar) +
            Path.DirectorySeparatorChar;

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            cacheDirectory =
                Path.GetDirectoryName(DotNetAssembly.ProgramExecutableFileName.Value)!;
        }

        if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            cacheDirectory =
                Path.GetDirectoryName(DotNetAssembly.ProgramExecutableFileName.Value)!;
        }

        foreach (var dependency in setupForCurrentOs)
        {
            NativeDependencies.SetUpDependency(
                cacheDirectory: cacheDirectory,
                dependency: dependency);
        }

        AuxiliarySearchPathAsList =
            new[] { cacheDirectory }
            .Concat(AuxiliarySearchPathAsList ?? Array.Empty<string>())
            .Distinct()
            .ToList();
    }


    /// <summary>
    /// As declared at https://github.com/microsoft/ClearScript/blob/d9a58a66e6a2a39d01e2adea9071f6a460381c8a/ClearScript/Util/MiscHelpers.cs#L133
    /// 
    /// See documentation on <see cref="Microsoft.ClearScript.HostSettings.AuxiliarySearchPath"/>
    /// </summary>
    private static char AuxiliarySearchPathSeparatorChar => ';';

    public static IReadOnlyList<string>? AuxiliarySearchPathAsList
    {
        set =>
            Microsoft.ClearScript.HostSettings.AuxiliarySearchPath =
                value is null
                    ?
                    null
                    :
                    string.Join(AuxiliarySearchPathSeparatorChar, value);

        get =>
            Microsoft.ClearScript.HostSettings.AuxiliarySearchPath switch
            {
                { } value => value.Split(AuxiliarySearchPathSeparatorChar),
                null => null
            };
    }
}
