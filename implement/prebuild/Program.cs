using Pine;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Elm;
using Pine.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace prebuild;

using FileTree =
    IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>;

public class Program
{
    public const string DestinationDirectory = "./Pine.Core/";

    public const string PreviousCompilerFilePath =
        "./history/2025-11-22-compiler-bundle/elm-syntax-parser-and-compiler.bin.gzip";

    public const string LastTrainedCSharpArchiveFilePath =
        "./history/2025-11-22-distilled-csharp-files.tar.gz";

    public static void Main()
    {
        Console.WriteLine(
            "Current working directory: " + Environment.CurrentDirectory);

        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            Console.WriteLine("Building .NET assembly from CSharp files...");

            var assemblyBytes =
                Pine.Core.Bundle.BundledPineToDotnet.LoadCSharpFilesFromFileAndBuildBundleFileAssembly(
                    LastTrainedCSharpArchiveFilePath,
                    DestinationDirectory,
                    logger: Console.WriteLine);

            clock.Stop();

            Console.WriteLine(
                "Completed building .NET bundle assembly.dll in " +
                clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

            SetupVM.PrecompiledLeavesDefault =
                Pine.Core.Bundle.BundledPineToDotnet.LoadFromAssembly(assemblyBytes.ToArray())
                .Extract(err => throw new Exception("Failed loading from assembly: " + err))
                .BuildDictionary();
        }

        BuildAndSaveValueDictionary(
            elmCompilers: []);

        var elmCompilerValue = BuildElmCompiler();

        BuildAndSaveValueDictionary(
            elmCompilers:
            [
                new KeyValuePair<Pine.Core.Files.FileTree, PineValue>(
                    elmCompilerValue.sourceFiles, elmCompilerValue.compiled)
                ]);

        if (false)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            Console.WriteLine("Building .NET bundle C# and assembly...");

            var parsedEnvironment =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                    elmCompilerValue.compiled)
                .Extract(err => throw new Exception(err));

            Pine.Core.Bundle.BundledPineToDotnet.BuildAndWriteBundleFile(
                parsedEnvironment,
                DestinationDirectory,
                logger: Console.WriteLine,
                writeCSharpFilesArchive: true);

            clock.Stop();

            Console.WriteLine(
                "Completed compilation to C# and .NET bundle assembly.dll in " +
                clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");
        }
    }

    public static void BuildAndSaveValueDictionary(
        IEnumerable<KeyValuePair<Pine.Core.Files.FileTree, PineValue>> elmCompilers)
    {
        var fromFreshBuild =
            ReusedInstances.BuildPineListValueReusedInstances(
                ReusedInstances.ExpressionsSource(),
                additionalRoots: elmCompilers.Select(kvp => kvp.Value));

        BundledDeclarations.CompressAndWriteBundleFile(
            compiledEnvironments:
            elmCompilers.ToDictionary(),
            otherReusedValues:
            ReusedInstances.PrebuildListEntries(fromFreshBuild),
            destinationDirectory: DestinationDirectory);
    }

    public static ElmCompilerInElm LoadPreviousCompiler()
    {
        return
            ElmCompilerInElm.LoadCompilerFromBundleFile(PreviousCompilerFilePath)
            .Extract(err => throw new Exception(err));
    }

    public static FileTree LoadLastTrainedCSharpFiles()
    {
        return
            Pine.Core.Bundle.TarGZipArchive.ExtractArchive(
                System.IO.File.ReadAllBytes(LastTrainedCSharpArchiveFilePath));
    }

    public static (Pine.Core.Files.FileTree sourceFiles, PineValue compiled) BuildElmCompiler()
    {
        var elmCompilerSource =
            ElmCompilerInElm.CompilerSourceFilesDefault.Value;

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
            ElmCompilerInElm.ElmCompilerFromEnvValue(elmCompilerFirstValue)
            .Extract(err => throw new Exception(err));

        var elmCompilerSecond =
            ElmCompilerInElm.BuildCompilerFromSourceFiles(
                elmCompilerSource,
                overrideElmCompiler: elmCompilerFirst)
            .Extract(err => throw new Exception(err));

        Console.WriteLine(
            "Built the second iteration in " +
            clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

        clock.Restart();

        Console.WriteLine(
            "Env compiled in second iteration is " + elmCompilerSecond.CompilerEnvironment);

        Console.WriteLine(
            "Compressed and wrote bundle file in " +
            clock.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

        return (elmCompilerSource, elmCompilerSecond.CompilerEnvironment);
    }

    public static Result<string, PineValue> BuildElmCompilerFirstIteration(
        Pine.Core.Files.FileTree elmCompilerSource)
    {
        if (true)
        {
            var previousCompiler = LoadPreviousCompiler();

            var elmCompilerFirst =
                ElmCompilerInElm.BuildCompilerFromSourceFiles(
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
                ZipArchive.ZipArchiveFromFiles(
                    elmCompilerSource.EnumerateFilesTransitive()
                    .Select(entry => (string.Join("/", entry.path), entry.fileContent)));

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
                        , ..ElmCompilerInElm.DefaultCompilerTreeRootModuleFilePaths
                        .Select(rootFilePath => "--root-file-path=" + string.Join('/', rootFilePath))
                        ,"--skip-lowering"
                        ]);

            var process = new System.Diagnostics.Process
            {
                StartInfo = new System.Diagnostics.ProcessStartInfo
                {
                    FileName = s_executableFilePathCached.Value,
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

            var loadResult =
                BundledElmEnvironments.LoadBundledDeclarations(
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
            ("63844343516257983fae09726580477f01a099232dfa74bc11a1927010d161ac",
            @"https://github.com/pine-vm/pine/releases/download/v0.4.24/pine-bin-v0.4.24-linux-x64.zip"))
        .Add(
            OSPlatform.Windows,
            ("b616305ae05eb73437874c898cda7be1421851420da8d374e7cbb53be5c9fd6c",
            @"https://github.com/pine-vm/pine/releases/download/v0.4.24/pine-bin-v0.4.24-win-x64.zip"))
        .Add(
            OSPlatform.OSX,
            ("34f0abf678f9d2b0f55aadf1f6c24a97b8a70c273f65f5dbf52e5bc342519c84",
            @"https://github.com/pine-vm/pine/releases/download/v0.4.24/pine-bin-v0.4.24-osx-x64.zip"));

    private readonly static Lazy<string> s_executableFilePathCached = new(() =>
    {
        /*
         * For now, we assume that the file stays the same for the lifetime of the current process.
         * This approach will break if the persistent cache is cleared while the current process is running.
         * We could make this more robust by checking if the file at the path still exists, and re-downloading if it doesn't.
         * */

        var executableFile =
            BlobLibrary.LoadFileForCurrentOs(ExecutableFileByOs)
            ??
            throw new Exception("Failed to load Pine executable file");

        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            System.IO.File.SetUnixFileMode(
                executableFile.cacheFilePath,
                ExecutableFile.UnixFileModeForExecutableFile);
        }

        return executableFile.cacheFilePath;
    });
}
