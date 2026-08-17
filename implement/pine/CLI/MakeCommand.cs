using ElmTime;
using ElmTime.Elm019;
using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.Elm;
using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Elm;
using Pine.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;

using CompilerSerialInterface = ElmTime.CompilerSerialInterface;

namespace Pine.CLI;

public static class MakeCommand
{
    public static Command Create()
    {
        var command = new Command("make", "Compile Elm code into JavaScript, HTML, or other files.");

        var entryPointElmFileArgument = new Argument<string>("path-to-elm-file");

        var outputOption = new Option<string?>("--output");

        var inputDirectoryOption = new Option<string?>("--input-directory");

        var debugOption = new Option<bool>("--debug");

        var optimizeOption = new Option<bool>("--optimize");

        command.Add(entryPointElmFileArgument);
        command.Add(outputOption);
        command.Add(inputDirectoryOption);
        command.Add(debugOption);
        command.Add(optimizeOption);

        command.SetAction(
            (parseResult) =>
            {
                var entryPoint = parseResult.GetValue(entryPointElmFileArgument);
                var output = parseResult.GetValue(outputOption);
                var inputDirectory = parseResult.GetValue(inputDirectoryOption);
                var debug = parseResult.GetValue(debugOption);
                var optimize = parseResult.GetValue(optimizeOption);

                var actualInputDirectory = inputDirectory ?? Environment.CurrentDirectory;
                var actualOutput = output ?? "make-default-output.html";

                return
                    ElmMakeCommandExecute(
                        inputDirectory: actualInputDirectory,
                        entryPointElmFile: entryPoint,
                        outputPathArgument: actualOutput,
                        enableDebug: debug,
                        enableOptimize: optimize);
            });

        return command;
    }

    private static int ElmMakeCommandExecute(
        string inputDirectory,
        string entryPointElmFile,
        string outputPathArgument,
        bool enableDebug,
        bool enableOptimize)
    {
        var loadInputDirectoryFailedFiles =
            new Dictionary<IReadOnlyList<string>, IOException>(
                comparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var loadInputDirectoryResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(
                inputDirectory,
                ignoreFileOnIOException: (filePath, ioException) =>
                {
                    loadInputDirectoryFailedFiles[filePath] = ioException;
                    return true;
                })
            .LogToActions(Console.WriteLine);

        var elmMakeCommandOptions = new List<string>();

        if (enableDebug)
            elmMakeCommandOptions.Add("--debug");

        if (enableOptimize)
            elmMakeCommandOptions.Add("--optimize");

        var elmMakeCommandAppendix = string.Join(" ", elmMakeCommandOptions);

        int ReturnLoadingFromSourceError(string errorDetail)
        {
            PineCliCommand.DotNetConsoleWriteProblemCausingAbort(
                "Failed to load from path '" + inputDirectory + "': " + errorDetail);

            return 10;
        }

        if (loadInputDirectoryResult.IsErrOrNull() is { } loadInputDirErr)
        {
            return ReturnLoadingFromSourceError(loadInputDirErr);
        }

        if (loadInputDirectoryResult.IsOkOrNullable() is not { } loadInputDirectoryOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from loading input directory: " + loadInputDirectoryResult);
        }

        Result<string, LoadForMakeResult> loadForElmMake()
        {
            if (0 < loadInputDirectoryFailedFiles.Count)
            {
                var shownPaths =
                    loadInputDirectoryFailedFiles
                    .Take(3)
                    .Select(
                        pathAndException =>
                        string.Join("/", pathAndException.Key) + " (" + pathAndException.Value.Message + ")")
                    .ToImmutableList();

                Console.WriteLine(
                    string.Join(
                        "\n",
                        "Ignored " + loadInputDirectoryFailedFiles.Count + " files due to IO exceptions:",
                        string.Join(
                            "\n",
                            [
                            .. shownPaths,
                                            shownPaths.Count < loadInputDirectoryFailedFiles.Count ? "..." : null
                            ]
                            )));
            }

            var filteredSourceTree =
                loadInputDirectoryOk.origin is LoadCompositionOrigin.FromLocalFileSystem
                ?
                LoadFromLocalFilesystem.RemoveNoiseFromTree(
                    loadInputDirectoryOk.tree,
                    discardGitDirectory: true)
                :
                loadInputDirectoryOk.tree;

            var discardedFiles =
                loadInputDirectoryOk.tree
                .EnumerateFilesTransitive()
                .Where(originalBlob => filteredSourceTree.GetNodeAtPath(originalBlob.path) is not FileTree.FileNode)
                .ToImmutableArray();

            if (0 < discardedFiles.Length)
            {
                Console.WriteLine("Discarded " + discardedFiles.Length + " files from the input directory.");
            }

            if (filteredSourceTree.GetNodeAtPath(["elm.json"]) is not
                FileTree.FileNode elmJsonFile)
            {
                return "Did not find elm.json file in that directory.";
            }

            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

            if (elmJsonFileParsed is null)
            {
                return "Failed to parse elm.json file.";
            }

            var elmJsonSourceDirectories =
                elmJsonFileParsed.ParsedSourceDirectories.ToImmutableList();

            var sourceDirectoriesNotInInputDirectory =
                elmJsonSourceDirectories
                .Where(relativeSourceDir => 0 < relativeSourceDir.ParentLevel)
                .ToImmutableList();

            var pathToElmFile = entryPointElmFile;

            if (string.IsNullOrEmpty(pathToElmFile))
            {
                return "The path to the entry point Elm file is empty.";
            }

            if (pathToElmFile.StartsWith("./"))
                pathToElmFile = pathToElmFile[2..];

            if (sourceDirectoriesNotInInputDirectory.IsEmpty)
            {
                return
                    new LoadForMakeResult(
                        filteredSourceTree,
                        [],
                        pathToElmFile.Replace('\\', '/').Split('/'));
            }

            if (loadInputDirectoryOk.origin is not LoadCompositionOrigin.FromLocalFileSystem)
            {
                return
                    "Failed to work with elm.json file containing directory which is not contained in input directory: This configuration is only supported when loading from a local file system";
            }

            string AbsoluteSourceDirectoryFromRelative(ElmJsonStructure.RelativeDirectory relDir)
            {
                var path = inputDirectory;

                if (!Path.IsPathFullyQualified(path))
                {
                    path = Path.GetFullPath(path);
                }

                for (var i = 0; i < relDir.ParentLevel; ++i)
                {
                    path =
                        Path.GetDirectoryName(path.TrimEnd('/', '\\'))
                        ??
                        throw new Exception("Failed to compute parent directory for " + path);
                }

                return
                    Path.Combine(path, string.Join('/', relDir.Subdirectories));
            }

            var outerSourceDirectoriesAbsolute =
                sourceDirectoriesNotInInputDirectory
                .Select(AbsoluteSourceDirectoryFromRelative)
                .ToImmutableList();

            var maxParentLevel =
                sourceDirectoriesNotInInputDirectory.Max(sd => sd.ParentLevel);

            var commonParentDirectory =
                AbsoluteSourceDirectoryFromRelative(
                    new ElmJsonStructure.RelativeDirectory(
                        ParentLevel: maxParentLevel,
                        Subdirectories: []));

            IReadOnlyList<string> PathRelativeToCommonParentFromAbsolute(string absolutePath) =>
                absolutePath[commonParentDirectory.Length..].Replace('\\', '/').Trim('/').Split('/');

            var inputDirectoryAbsolute = Path.GetFullPath(inputDirectory);

            var workingDirectoryRelative =
                PathRelativeToCommonParentFromAbsolute(inputDirectoryAbsolute);

            var pathToElmFileAbsolute = Path.GetFullPath(pathToElmFile);

            var pathToFileWithElmEntryPoint =
                PathRelativeToCommonParentFromAbsolute(pathToElmFileAbsolute);

            return
                outerSourceDirectoriesAbsolute
                .Select(
                    outerSourceDirectory =>
                    {
                        return
                            LoadComposition.LoadFromPathResolvingNetworkDependencies(outerSourceDirectory)
                            .LogToActions(Console.WriteLine)
                            .Map(
                                outerSourceDirLoadOk =>
                                (outerSourceDirLoadOk.tree,
                                relativePath: PathRelativeToCommonParentFromAbsolute(outerSourceDirectory)));
                    })
                .ListCombine()
                .Map(
                    outerSourceDirectories =>
                    {
                        var combinedTree =
                            outerSourceDirectories
                            .Aggregate(
                                seed:
                                FileTree.EmptyTree
                                .SetNodeAtPathSorted(workingDirectoryRelative, filteredSourceTree),
                                func:
                                (aggregate, nextSourceDir) =>
                                aggregate.SetNodeAtPathSorted(
                                    nextSourceDir.relativePath,
                                    nextSourceDir.tree));

                        return
                            new LoadForMakeResult(
                                SourceFiles: combinedTree,
                                workingDirectoryRelative,
                                pathToFileWithElmEntryPoint);
                    });
        }

        var loadSourceFilesResult = loadForElmMake();

        if (loadSourceFilesResult.IsErrOrNull() is { } loadSourceFilesErr)
        {
            return ReturnLoadingFromSourceError(loadSourceFilesErr);
        }

        if (loadSourceFilesResult.IsOkOrNull() is not { } loadSourceFilesOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from loading source files: " + loadSourceFilesResult);
        }

        var inputHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(loadSourceFilesOk.SourceFiles).Span);

        Console.WriteLine(
            "Loaded " + inputHash[..10] + " as input: " +
            string.Join(
                "\n",
                FileTreeExtensions.DescribeFileTreeForHumans(
                    loadSourceFilesOk.SourceFiles,
                    listFiles: false,
                    extractFileName: null)));

        var makeResult =
            Make(
                sourceFiles: FileTreeExtensions.ToFlatDictionaryWithPathComparer(loadSourceFilesOk.SourceFiles),
                workingDirectoryRelative: loadSourceFilesOk.WorkingDirectoryRelative,
                pathToFileWithElmEntryPoint: loadSourceFilesOk.PathToFileWithElmEntryPoint,
                outputFileName: Path.GetFileName(outputPathArgument),
                elmMakeCommandAppendix: elmMakeCommandAppendix);

        if (makeResult.IsErrOrNull() is { } makeErr)
        {
            PineCliCommand.DotNetConsoleWriteProblemCausingAbort(
                "Failed to make " + entryPointElmFile + ":\n" + makeErr);

            return 20;
        }

        if (makeResult.IsOkOrNull() is not { } makeOk)
        {
            throw new NotImplementedException(
                "Unexpected make result type: " + makeResult);
        }

        ReadOnlyMemory<byte> ComputeOutputFileContent()
        {
            if (makeOk.ProducedFiles is FileTree.FileNode blobNode)
            {
                Console.WriteLine(
                    "Make command produced a single blob with " +
                    CommandLineInterface.FormatIntegerForDisplay(blobNode.Bytes.Length) + " bytes.");

                return blobNode.Bytes;
            }

            if (makeOk.ProducedFiles is FileTree.DirectoryNode treeNode)
            {
                var blobs =
                    treeNode.EnumerateFilesTransitive()
                    .Select(entry => (string.Join("/", entry.path), entry.fileContent))
                    .ToImmutableList();

                Console.WriteLine(
                    "Make command produced tree node with " +
                    blobs.Count + " blobs (" +
                    CommandLineInterface.FormatIntegerForDisplay(blobs.Sum(entry => entry.fileContent.Length)) +
                    " aggregate bytes). Packaging these into zip archive...");

                var zipArchive = ZipArchive.ZipArchiveFromFiles(blobs);

                return zipArchive;
            }

            throw new NotImplementedException(
                "Unexpected produced files type: " + makeOk.ProducedFiles);
        }

        var outputFileContent = ComputeOutputFileContent();

        var outputPath = Path.GetFullPath(outputPathArgument);

        var outputDirectory = Path.GetDirectoryName(outputPath);

        if (outputDirectory is not null)
            Directory.CreateDirectory(outputDirectory);

        File.WriteAllBytes(outputPath, outputFileContent.Span);
        Console.WriteLine("Saved the output to " + outputPath);

        return 0;
    }

    private record LoadForMakeResult(
        FileTree SourceFiles,
        IReadOnlyList<string> WorkingDirectoryRelative,
        IReadOnlyList<string> PathToFileWithElmEntryPoint);

    /// <summary>
    /// Compiles Elm code as offered with the 'make' command on the CLI.
    /// </summary>
    public static Result<string, Elm019Binaries.ElmMakeOk> Make(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix)
    {
        workingDirectoryRelative ??= [];

        IReadOnlyList<string> pathToFileWithElmEntryPointFromWorkingDir =
            [.. pathToFileWithElmEntryPoint.Skip(workingDirectoryRelative.Count)];

        var loweringResult =
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles.ToImmutableDictionary(),
                workingDirectoryRelative: workingDirectoryRelative,
                interfaceConfig: new ElmAppInterfaceConfig(CompilationRootFilePath: pathToFileWithElmEntryPoint));

        var entryPointSourceFile =
            sourceFiles[pathToFileWithElmEntryPoint];

        var entryPointSourceFileText =
            Encoding.UTF8.GetString(entryPointSourceFile.Span);

        var entryPointModuleNameResult =
            ElmModule.ParseModuleName(entryPointSourceFileText);

        if (entryPointModuleNameResult.IsErrOrNull() is { } entryPointModuleNameErr)
        {
            return
                "Failed to parse module name from entry point file: " + entryPointModuleNameErr;
        }

        if (entryPointModuleNameResult.IsOkOrNull() is not { } entryPointModuleNameOk)
        {
            throw new Exception(
                "Unexpected entry point module name result type: " + entryPointModuleNameResult);
        }

        if (loweringResult.IsErrOrNull() is { } loweringErr)
        {
            return
                "Failed lowering Elm code with " + loweringErr.Count + " error(s):\n" +
                ElmAppCompilation.CompileCompilationErrorsDisplayText(loweringErr);
        }

        if (loweringResult.IsOkOrNull() is not { } loweringOk)
        {
            throw new Exception("Unexpected lowering result type: " + loweringResult);
        }

        var sourceFilesAfterLowering = loweringOk.Result.CompiledFiles;

        Result<string, Elm019Binaries.ElmMakeOk> ContinueWithClassicEntryPoint()
        {
            return
                Elm019Binaries.ElmMake(
                    sourceFilesAfterLowering,
                    workingDirectoryRelative: workingDirectoryRelative,
                    pathToFileWithElmEntryPoint: pathToFileWithElmEntryPointFromWorkingDir,
                    outputFileName: outputFileName.Replace('\\', '/').Split('/').Last(),
                    elmMakeCommandAppendix: elmMakeCommandAppendix);
        }

        Result<string, Elm019Binaries.ElmMakeOk> ContinueWithBlobEntryPoint()
        {
            var sourceFilesWithMergedPackages =
                ElmAppDependencyResolution.AppCompilationUnitsForEntryPoint(
                    FileTree.FromSetOfFilesWithStringPath(sourceFilesAfterLowering),
                    entryPointFilePath: pathToFileWithElmEntryPoint);

            var pineVMCache = new InvocationCache();

            var pineVM =
                SetupVM.Create(evalCache: pineVMCache);

            var parseCache = new global::Pine.Core.CodeAnalysis.PineVMParseCache();

            var elmCompilerCache = new ElmCompilerCache();

            var compileResult =
                InteractiveSessionPine.CompileInteractiveEnvironment(
                    appCodeTree: sourceFilesWithMergedPackages.files,
                    overrideSkipLowering: true,
                    entryPointsFilePaths: [pathToFileWithElmEntryPoint],
                    skipFilteringForSourceDirs: false);

            if (compileResult.IsErrOrNull() is { } compileErr)
            {
                return
                    "Failed to compile Elm interactive env: " + compileErr;
            }

            if (compileResult.IsOkOrNull() is not { } compileOk)
            {
                throw new Exception("Unexpected compile result type: " + compileResult);
            }

            var parseFromEnvResult =
                Core.CodeAnalysis.ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: compileOk,
                    moduleName: string.Join(".", entryPointModuleNameOk.ToArray()),
                    "blobMain",
                    parseCache);

            {
                if (parseFromEnvResult.IsErrOrNull() is { } parseErr)
                {
                    return "Failed to parse Elm module: " + parseErr;
                }
            }

            if (parseFromEnvResult.IsOkOrNullable() is not { } parseFromEnvOk)
            {
                throw new Exception("Unexpected parse result type: " + parseFromEnvResult);
            }

            var elmBytesValue = parseFromEnvOk.declValue;

            if (parseFromEnvOk.functionRecord.ParameterCount is 1)
            {
                /*
                 * Support alternative form as following to avoid eager (expensive) evaluation in the compiler:
                 * 
                 * blobMain : () -> Bytes.Bytes
                 * */

                var applyMainResult =
                    Core.CodeAnalysis.ElmInteractiveEnvironment.ApplyFunction(
                        pineVM,
                        functionRecord: parseFromEnvOk.functionRecord,
                        arguments: [PineValue.EmptyList]);

                if (applyMainResult.IsErrOrNull() is { } applyErr)
                {
                    return "Failed to apply Elm function: " + applyErr;
                }

                if (applyMainResult.IsOkOrNull() is not { } applyOk)
                {
                    throw new Exception("Unexpected apply result type: " + applyMainResult);
                }

                elmBytesValue = applyOk;
            }

            var parseDeclResult =
                ElmValueEncoding.PineValueAsElmValue(elmBytesValue, null, null);

            if (parseDeclResult.IsErrOrNull() is { } parseDeclErr)
            {
                return "Failed to parse Elm value: " + parseDeclErr;
            }

            if (parseDeclResult.IsOkOrNull() is not { } parseDeclOk)
            {
                throw new Exception("Unexpected parse result type: " + parseDeclResult);
            }

            return TryParseMakeOutput(parseDeclOk);
        }

        if (loweringOk.Result.RootModuleEntryPointKind.IsErrOrNull() is { } rootModuleEntryPointKindErr)
        {
            return "Failed to get entry point main declaration: " + rootModuleEntryPointKindErr;
        }

        if (loweringOk.Result.RootModuleEntryPointKind.IsOkOrNull() is not { } rootModuleEntryPointKind)
        {
            throw new NotImplementedException(
                "Unexpected root module entry point result type: " + loweringOk.Result.RootModuleEntryPointKind);
        }

        return
            rootModuleEntryPointKind switch
            {
                CompilerSerialInterface.ElmMakeEntryPointKind.ClassicMakeEntryPoint =>
                ContinueWithClassicEntryPoint(),

                CompilerSerialInterface.ElmMakeEntryPointKind.BlobMakeEntryPoint blob =>
                ContinueWithBlobEntryPoint(),

                _ =>
                throw new NotImplementedException(
                    "Unexpected root module entry point kind: " + rootModuleEntryPointKind),
            };
    }

    private static Result<string, Elm019Binaries.ElmMakeOk> TryParseMakeOutput(ElmValue elmValue)
    {
        if (elmValue is ElmValue.ElmBytes bytesValue)
        {
            return new Elm019Binaries.ElmMakeOk(ProducedFiles: FileTree.File(bytesValue.Value));
        }

        if (elmValue is ElmValue.ElmTag)
        {
            var asTreeResult = ParseAsFileTree(elmValue);

            if (asTreeResult.IsErrOrNull() is { } asTreeErr)
            {
                return "Failed to parse Elm tag value as file tree: " + asTreeErr;
            }

            if (asTreeResult.IsOkOrNull() is not { } asTreeOk)
            {
                throw new NotImplementedException("Unexpected result type: " + asTreeResult);
            }

            return new Elm019Binaries.ElmMakeOk(ProducedFiles: asTreeOk);
        }

        return "Unexpected Elm value type: " + elmValue;
    }

    private static Result<string, FileTree> ParseAsFileTree(ElmValue elmValue)
    {
        /*
         * Type declaration on Elm side looks like this:
         * 
        type FileTreeNode blobStructure
            = BlobNode blobStructure
            | TreeNode (TreeNodeStructure blobStructure)


        type alias TreeNodeStructure blobStructure =
            List (TreeNodeEntryStructure blobStructure)


        type alias TreeNodeEntryStructure blobStructure =
            ( String, FileTreeNode blobStructure )

         * */

        if (elmValue is not ElmValue.ElmTag elmTag)
        {
            return "Expected Elm tag value, but got: " + elmValue;
        }

        if (elmTag.TagName.StartsWith("Blob", StringComparison.OrdinalIgnoreCase))
        {
            if (elmTag.Arguments.Count is not 1)
            {
                return "Expected Elm tag with one argument, but got: " + elmTag.Arguments.Count;
            }

            var blob = elmTag.Arguments[0];

            if (elmTag.Arguments[0] is not ElmValue.ElmBytes bytes)
            {
                return "Expected Elm bytes value, but got: " + blob;
            }

            return FileTree.File(bytes.Value);
        }

        if (elmTag.TagName.StartsWith("Tree", StringComparison.OrdinalIgnoreCase))
        {
            if (elmTag.Arguments.Count is not 1)
            {
                return "Expected Elm tag with one argument, but got: " + elmTag.Arguments.Count;
            }

            if (elmTag.Arguments[0] is not ElmValue.ElmList elmList)
            {
                return "Expected Elm list value, but got: " + elmTag.Arguments[0];
            }

            var children = new (string name, FileTree item)[elmList.Items.Count];

            for (var i = 0; i < elmList.Items.Count; ++i)
            {
                var child = elmList.Items[i];

                if (child is not ElmValue.ElmList tuple)
                {
                    return "Child [" + i + "] is not a tuple: " + child;
                }

                if (tuple.Items.Count is not 2)
                {
                    return "Child [" + i + "]: Expected Elm tuple with two elements, but got: " + tuple.Items.Count;
                }

                if (tuple.Items[0] is not ElmValue.ElmString name)
                {
                    return "Child [" + i + "]: Expected Elm string value, but got: " + tuple.Items[0];
                }

                var childTreeResult = ParseAsFileTree(tuple.Items[1]);

                if (childTreeResult.IsErrOrNull() is { } childTreeErr)
                {
                    return
                        "Child [" + i + "] (" + name.Value + "): Failed to parse Elm tag value as file tree: " +
                        childTreeErr;
                }

                if (childTreeResult.IsOkOrNull() is not { } childTreeOk)
                {
                    throw new NotImplementedException("Unexpected result type: " + childTreeResult);
                }

                children[i] = (name.Value, childTreeOk);
            }

            var treeNode = FileTree.NonSortedDirectory(children);

            return treeNode;
        }

        return "Unexpected Elm tag value type: " + elmTag;
    }

}
