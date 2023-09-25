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
            [
                new DependencyFile(
                    HashBase16: "5c8f3ecc18a3799d1836631d51a4dec9c902d296bdd31903c861f345583bbea6",
                    ExpectedFileName: "ClearScriptV8.linux-x64.so",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.linux-x64/7.4.3"])])
        .Add(
            OSPlatform.Windows,
            [
                new DependencyFile(
                    HashBase16: "5c3fbbc83084ea31140c6ae3efa81890ca1a1defcd99043d6225b1e948c17c31",
                    ExpectedFileName: "ClearScriptV8.win-x64.dll",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.win-x64/7.4.3"])])
        .Add(
            OSPlatform.OSX,
            [
                new DependencyFile(
                    HashBase16: "c549e22c6cee7041cd28a856ea9f5990aecc61ba59691fdc0280b06032170249",
                    ExpectedFileName: "ClearScriptV8.osx-x64.dylib",
                    RemoteSources: ["https://www.nuget.org/api/v2/package/Microsoft.ClearScript.V8.Native.osx-x64/7.4.3"])]);

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
            .Concat(AuxiliarySearchPathAsList ?? [])
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
