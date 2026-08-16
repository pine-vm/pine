using ElmTime;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.Elm019;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;

namespace Pine.CLI;

public static class CompileCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "compile",
                "Compile app source code the same way as would be done when deploying a web service.");

        var sourceArgument = new Argument<string>("source");

        command.Add(sourceArgument);

        command.SetAction(
            (parseResult) =>
            {
                var source = parseResult.GetValue(sourceArgument);

                var compileReport = CompileAppAndSaveCompositionToZipArchive(source).report;

                PineCliCommand.WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        compileReport,
                        PineCliCommand.ReportJsonSerializerOptions),
                    reportKind: "compile.json");

                return 0;
            });

        return command;
    }

    private static (CompileAppReport report, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? compiledAppFiles)
        CompileAppAndSaveCompositionToZipArchive(string sourcePath)
    {
        var compileResult = CompileApp(sourcePath);

        if (compileResult.compiledAppFiles != null)
        {
            var compiledTree = FileTree.FromSetOfFilesWithStringPath(compileResult.compiledAppFiles);
            var compiledFiles = FileTreeExtensions.ToFlatDictionaryWithPathComparer(compiledTree);

            var compiledCompositionArchive = ZipArchive.ZipArchiveFromFiles(compiledFiles);

            var outputCompositionFileName = compileResult.report.compiledCompositionId + ".zip";

            var outputCompositionFilePath = Path.Combine(PineCliCommand.ReportFilePath, outputCompositionFileName);

            Directory.CreateDirectory(Path.GetDirectoryName(outputCompositionFilePath)!);
            File.WriteAllBytes(outputCompositionFilePath, compiledCompositionArchive);

            Console.WriteLine(
                "\nSaved compiled composition " + compileResult.report.compiledCompositionId + " to '" +
                outputCompositionFilePath +
                "'.");
        }

        return compileResult;
    }

    public record CompileAppReport(
        string engineVersion,
        string beginTime,
        string sourcePath,
        string? sourceCompositionId,
        SourceSummaryStructure? sourceSummary,
        IReadOnlyList<ElmAppCompilation.CompilationIterationReport>? compilationIterationsReports,
        IReadOnlyList<ElmAppCompilation.LocatedCompilationError>? compilationErrors,
        string? compilationException,
        int? compilationTimeSpentMilli,
        string? compiledCompositionId,
        int? totalTimeSpentMilli);

    public record SourceSummaryStructure(
        int numberOfFiles,
        int totalSizeOfFilesContents);

    public static (string compositionId, SourceSummaryStructure summary) CompileSourceSummary(FileTree sourceTree)
    {
        var compositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(sourceTree).Span);

        var allBlobs = sourceTree.EnumerateFilesTransitive().ToImmutableList();

        return
            (compositionId,
            summary: new SourceSummaryStructure(
                numberOfFiles: allBlobs.Count,
                totalSizeOfFilesContents: allBlobs.Sum(blob => blob.fileContent.Length)));
    }

    public static (CompileAppReport report, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? compiledAppFiles)
        CompileApp(string sourcePath)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var report =
            new CompileAppReport(
                beginTime: BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow),
                engineVersion: PineCliCommand.AppVersionId,
                sourcePath: sourcePath,
                sourceCompositionId: null,
                sourceSummary: null,
                compilationIterationsReports: null,
                compilationErrors: null,
                compilationException: null,
                compilationTimeSpentMilli: null,
                compiledCompositionId: null,
                totalTimeSpentMilli: null);

        var loadCompositionResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
            .LogToActions(Console.WriteLine)
            .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

        var (sourceCompositionId, sourceSummary) = CompileSourceSummary(loadCompositionResult.tree);

        report = report with { sourceCompositionId = sourceCompositionId, sourceSummary = sourceSummary };

        Console.WriteLine(
            "Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'. Starting to compile...");

        var compilationStopwatch = System.Diagnostics.Stopwatch.StartNew();

        IReadOnlyList<IReadOnlyList<string>>? readElmJsonSourceDirectories()
        {
            if (loadCompositionResult.tree.GetNodeAtPath(["elm.json"]) is not
                FileTree.FileNode elmJsonFile)
            {
                return null;
            }

            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

            if (elmJsonFileParsed?.SourceDirectories is not { } sourceDirs)
            {
                return null;
            }

            return
                [
                ..sourceDirs
                .Select(flat => flat.Split('/', '\\'))
                ];
        }

        try
        {
            var filteredSourceTree =
                loadCompositionResult.origin is LoadCompositionOrigin.FromLocalFileSystem
                ?
                LoadFromLocalFilesystem.RemoveNoiseFromTree(
                    loadCompositionResult.tree,
                    discardGitDirectory: true)
                :
                loadCompositionResult.tree;

            var discardedFiles =
                loadCompositionResult.tree
                .EnumerateFilesTransitive()
                .Where(originalBlob => filteredSourceTree.GetNodeAtPath(originalBlob.path) is not FileTree.FileNode)
                .ToImmutableArray();

            if (0 < discardedFiles.Length)
            {
                Console.WriteLine("Discarded " + discardedFiles.Length + " files from the input directory.");
            }

            var sourceFiles =
                FileTreeExtensions.ToFlatDictionaryWithPathComparer(filteredSourceTree);

            var elmJsonSourceDirectories =
                readElmJsonSourceDirectories() ?? [];

            bool filePathIsUnderElmJsonSourceDirectories(IReadOnlyList<string> filePath)
            {
                return
                    elmJsonSourceDirectories
                    .Any(sourceDir => filePath.Take(sourceDir.Count).SequenceEqual(sourceDir));
            }

            var compilationRootFilePath =
                sourceFiles.ContainsKey(ElmAppInterfaceConfig.Default.CompilationRootFilePath)
                ?
                ElmAppInterfaceConfig.Default.CompilationRootFilePath
                :
                sourceFiles
                .Where(c => c.Key[c.Key.Count - 1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                .OrderBy(c => c.Key.Count)
                .OrderBy(c => filePathIsUnderElmJsonSourceDirectories(c.Key) ? 0 : 1)
                .FirstOrDefault()
                .Key;

            var interfaceConfig =
                ElmAppInterfaceConfig.Default with
                {
                    CompilationRootFilePath = compilationRootFilePath
                };

            var compilationResult =
                ElmAppCompilation.AsCompletelyLoweredElmApp(
                    sourceFiles: sourceFiles,
                    workingDirectoryRelative: [],
                    interfaceConfig: interfaceConfig);

            var compilationTimeSpentMilli = compilationStopwatch.ElapsedMilliseconds;

            report = report with { compilationTimeSpentMilli = (int)compilationTimeSpentMilli };

            return
                compilationResult
                .Unpack(
                    fromErr: compilationErrors =>
                    {
                        Console.WriteLine(
                            "\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationErrors) + "\n");

                        return (report with { compilationErrors = compilationErrors, totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds }, null);
                    },
                    fromOk: compilationOk =>
                    {
                        var compiledAppFiles = compilationOk.Result.CompiledFiles;

                        var compiledTree = FileTree.FromSetOfFilesWithStringPath(compiledAppFiles);
                        var compiledComposition = FileTreeEncoding.Encode(compiledTree);

                        var compiledCompositionId =
                            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(compiledComposition).Span);

                        compilationStopwatch.Stop();

                        Console.WriteLine(
                            "\nCompilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds +
                            " seconds, resulting in composition " + compiledCompositionId + ".");

                        return
                            (report with
                            {
                                compilationIterationsReports = compilationOk.IterationsReports,
                                compiledCompositionId = compiledCompositionId,
                                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                            },
                            compiledAppFiles);
                    });
        }
        catch (Exception e)
        {
            report = report with { compilationTimeSpentMilli = (int)compilationStopwatch.Elapsed.TotalMilliseconds };

            Console.WriteLine("Compilation failed with runtime exception: " + e);

            return
                (report with { compilationException = e.ToString(), totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds },
                null);
        }
    }
}
