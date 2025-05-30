using Pine;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace prebuild;

public class Program
{
    public const string DestinationFilePath = "./Pine.Core/" + ReusedInstances.EmbeddedResourceFilePath;

    public const string PreviousCompilerFilePath =
        "./history/2025-04-13-compiler-bundle/elm-syntax-parser-and-compiler.json.gzip";

    public static void Main()
    {
        Console.WriteLine(
            "Current working directory: " + Environment.CurrentDirectory);

        BuildAndSaveValueDictionary(
            additionalRoots: []);

        var elmCompilerValue = BundleElmCompiler();

        BuildAndSaveValueDictionary(
            additionalRoots: [elmCompilerValue]);
    }

    public static void BuildAndSaveValueDictionary(
        IEnumerable<PineValue> additionalRoots)
    {
        var fromFreshBuild =
            ReusedInstances.BuildPineListValueReusedInstances(
                ReusedInstances.ExpressionsSource(),
                additionalRoots: additionalRoots);

        var file =
            ReusedInstances.BuildPrecompiledDictFile(fromFreshBuild);

        var absolutePath =
            System.IO.Path.GetFullPath(DestinationFilePath) ??
            throw new Exception("Failed to resolve absolute path for " + DestinationFilePath);

        Console.WriteLine(
            "Resolved the destination path of " + DestinationFilePath +
            " to " + absolutePath);

        System.IO.Directory.CreateDirectory(
            System.IO.Path.GetDirectoryName(absolutePath) ??
            throw new Exception("Failed to get directory name for " + absolutePath));

        System.IO.File.WriteAllBytes(
            absolutePath,
            file.ToArray());

        Console.WriteLine(
            "Saved the prebuilt dictionary with " +
            CommandLineInterface.FormatIntegerForDisplay(fromFreshBuild.PineValueLists.Count) +
            " list values to " + absolutePath);
    }

    public static ElmCompiler LoadPreviousCompiler()
    {
        return
            ElmCompiler.LoadCompilerFromBundleFile(PreviousCompilerFilePath)
            .Extract(err => throw new Exception(err));
    }

    public static PineValue BundleElmCompiler()
    {
        var elmCompilerSource =
            ElmCompiler.CompilerSourceFilesDefault.Value;

        var clock = System.Diagnostics.Stopwatch.StartNew();

        var elmCompilerFirstValue =
            BuildElmCompilerFirstIteration(elmCompilerSource)
            .Extract(err => throw new Exception(err));

        clock.Stop();

        Console.WriteLine(
            "Built the first iteration in " +
            clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

        Console.WriteLine(
            "Env compiled in first iteration is " + elmCompilerFirstValue);

        /*
         * The first iteration can differ from the second iteration when the previous compiler
         * does not implement the same optimizations and transformations as the current compiler.
         * Therefore, the second iteration is necessary to ensure that the compiler is fully optimized.
         * */

        clock.Restart();

        var elmCompilerFirst =
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFirstValue)
            .Extract(err => throw new Exception(err));

        var elmCompilerSecond =
            ElmCompiler.BuildCompilerFromSourceFiles(
                elmCompilerSource,
                overrideElmCompiler: elmCompilerFirst)
            .Extract(err => throw new Exception(err));

        Console.WriteLine(
            "Built the second iteration in " +
            clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

        clock.Restart();

        Console.WriteLine(
            "Env compiled in second iteration is " + elmCompilerSecond.CompilerEnvironment);

        BundledElmEnvironments.CompressAndWriteBundleFile(
            ImmutableDictionary<TreeNodeWithStringPath, PineValue>.Empty
            .SetItem(elmCompilerSource, elmCompilerSecond.CompilerEnvironment));

        Console.WriteLine(
            "Compressed and wrote bundle file in " +
            clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

        return elmCompilerSecond.CompilerEnvironment;
    }

    public static Result<string, PineValue> BuildElmCompilerFirstIteration(
        TreeNodeWithStringPath elmCompilerSource)
    {
        if (true)
        {
            var previousCompiler = LoadPreviousCompiler();

            var elmCompilerFirst =
                ElmCompiler.BuildCompilerFromSourceFiles(
                    elmCompilerSource,
                    overrideElmCompiler: previousCompiler)
                .Extract(err => throw new Exception(err));

            return elmCompilerFirst.CompilerEnvironment;
        }
        else
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            var elmCompilerSourceZipArchivePath =
                "./elm-compiler-source.zip";

            var zipArchive =
                ZipArchive.ZipArchiveFromEntries(
                    elmCompilerSource.EnumerateBlobsTransitive()
                    .Select(entry => (string.Join("/", entry.path), entry.blobContent)));

            System.IO.File.WriteAllBytes(
                elmCompilerSourceZipArchivePath,
                zipArchive);

            var processArguments =
                string.Join(
                    "  ",
                    [
                        "compile-interactive-env"
                        ,"--env-source=" + elmCompilerSourceZipArchivePath
                        ,"--output-compact-build=compact-build.json"
                        , ..ElmCompiler.DefaultCompilerTreeRootModuleFilePaths
                        .Select(rootFilePath => "--root-file-path=" + string.Join('/', rootFilePath))
                        ,"--skip-lowering"
                        ]);

            var process = new System.Diagnostics.Process
            {
                StartInfo = new System.Diagnostics.ProcessStartInfo
                {
                    FileName = executableFilePathCached.Value,
                    WorkingDirectory = Environment.CurrentDirectory,
                    Arguments = processArguments,
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    StandardOutputEncoding = Encoding.UTF8,
                    StandardErrorEncoding = Encoding.UTF8
                }
            };

            process.OutputDataReceived += (sender, e) =>
            {
                // Only write if there is data
                if (e.Data != null)
                {
                    Console.WriteLine(e.Data);
                }
            };

            process.ErrorDataReceived += (sender, e) =>
            {
                // Only write if there is data
                if (e.Data != null)
                {
                    Console.Error.WriteLine(e.Data);
                }
            };

            // Start the process
            process.Start();

            // Begin reading async output
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();

            process.WaitForExit();

            Console.WriteLine(
                "Process " + process.Id + " exited with code " + process.ExitCode + " after " +
                clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

            var fileContent = System.IO.File.ReadAllBytes("compact-build.json");

            Console.WriteLine(
                "Read " + CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                " bytes from compact-build.json");

            BundledElmEnvironments.CompressAndWriteBundleFile(fileContent);

            var loadResult =
                BundledElmEnvironments.LoadBundledCompiledEnvironments(
                    new System.IO.MemoryStream(fileContent),
                    gzipDecompress: false);

            if (loadResult.IsErrOrNull() is { } err)
            {
                throw new Exception(err);
            }

            if (loadResult.IsOkOrNull() is not { } loadOk)
            {
                throw new Exception(
                    "Unexpected result type: " + loadResult);
            }

            clock.Stop();

            var firstElmCompilerValue =
                loadOk.Values
                .OfType<PineValue.ListValue>()
                .OrderByDescending(list => list.NodesCount)
                .First();

            return firstElmCompilerValue;
        }
    }

    public static readonly IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("8693c6fa6e9ea58756e39af0f2bd4a36ae7ad0e651721211c410d9c38f897305",
            @"https://github.com/pine-vm/pine/releases/download/v0.3.31/pine-bin-v0.3.31-linux-x64.zip"))
        .Add(
            OSPlatform.Windows,
            ("5cc7305bab9fc983084c8a2498c80d07969a73bc53d059dc9302d90c9830b12e",
            @"https://github.com/pine-vm/pine/releases/download/v0.3.31/pine-bin-v0.3.31-win-x64.zip"))
        .Add(
            OSPlatform.OSX,
            ("4e5aed63302cbcc43b98207e7d0426d7b0b59acc8add0240cea5b1ce8d9df74d",
            @"https://github.com/pine-vm/pine/releases/download/v0.3.31/pine-bin-v0.3.31-osx-x64.zip"));

    private readonly static Lazy<string> executableFilePathCached = new(() =>
    {
        /*
         * For now, we assume that the file stays the same for the lifetime of the current process.
         * This approach will break if the persistent cache is cleared while the current process is running.
         * We could make this more robust by checking if the file at the path still exists, and re-downloading if it doesn't.
         * */

        var executableFile =
            BlobLibrary.LoadFileForCurrentOs(ExecutableFileByOs)
            ??
            throw new Exception("Failed to load elm-format executable file");

        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            System.IO.File.SetUnixFileMode(
                executableFile.cacheFilePath,
                ExecutableFile.UnixFileModeForExecutableFile);
        }

        return executableFile.cacheFilePath;
    });
}