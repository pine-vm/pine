using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.Files;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;

namespace Pine.CLI;

public static class CompileInteractiveEnvCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "compile-interactive-env",
                "Compile an interactive environment from Elm modules into a Pine value");

        var envSourceOption =
            new Option<string[]>("--env-source")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var outputCompactBuildOption =
            new Option<string?>("--output-compact-build")
            {
                Arity = ArgumentArity.ZeroOrOne
            };

        var rootModuleFilePathOption =
            new Option<string[]>("--root-file-path")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var skipLoweringOption = new Option<bool>("--skip-lowering");

        var gzipOption = new Option<bool>("--gzip");

        var overrideCompilerOption = new Option<string?>("--override-compiler");

        command.Add(envSourceOption);
        command.Add(outputCompactBuildOption);
        command.Add(rootModuleFilePathOption);
        command.Add(skipLoweringOption);
        command.Add(gzipOption);
        command.Add(overrideCompilerOption);

        command.SetAction(
            (parseResult) =>
            {
                var envSources = parseResult.GetValue(envSourceOption);
                var outputCompactBuild = parseResult.GetValue(outputCompactBuildOption);
                var rootModuleFilePaths = parseResult.GetValue(rootModuleFilePathOption);
                var skipLowering = parseResult.GetValue(skipLoweringOption);
                var gzip = parseResult.GetValue(gzipOption);
                var overrideCompiler = parseResult.GetValue(overrideCompilerOption);

                IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
                    [
                    ..(rootModuleFilePaths ?? [])
                    .WhereNotNull()
                    .Select(flat => flat.Split('/', '\\'))
                    ];

                var environmentsSourceTrees =
                    envSources
                    .Select(
                        sourcePath =>
                        {
                            var loadCompositionResult =
                                LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
                                .LogToActions(Console.WriteLine)
                                .Extract(
                                    error =>
                                    throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

                            var fileTree = loadCompositionResult.tree;

                            if (fileTree is FileTree.FileNode sourceBlob)
                            {
                                var zipEntries = ZipArchive.EntriesFromZipArchive(sourceBlob.Bytes);

                                fileTree = FileTree.FromSetOfFilesWithCommonFilePath(zipEntries);
                            }

                            return fileTree;
                        })
                    .ToImmutableArray();

                var aggregateElmModuleFiles =
                    environmentsSourceTrees
                    .SelectMany(tree => tree.EnumerateFilesTransitive())
                    .Where(f => f.path.LastOrDefault()?.EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ?? false)
                    .ToImmutableArray();

                Console.WriteLine(
                    "Loaded " + environmentsSourceTrees.Length + " source trees with " +
                    aggregateElmModuleFiles.Length + " aggregate Elm module files.");

                Console.WriteLine(
                    "Compiling with lowering " +
                    (skipLowering ? "disabled" : "enabled"));

                Console.WriteLine(
                    "Limiting the compilation to " + rootFilePaths.Count + " root files: " +
                    string.Join(
                        ", ",
                        rootFilePaths.Select(path => string.Join("/", path))));

                ElmCompilerInElm? overrideElmCompiler = null;

                if (overrideCompiler is { } overrideCompilerPath)
                {
                    Console.WriteLine("Using Elm compiler from " + overrideCompilerPath);

                    overrideElmCompiler =
                        ElmCompilerInElm.LoadCompilerFromBundleFile(overrideCompilerPath)
                        .Extract(
                            err =>
                            throw new Exception("Failed to load Elm compiler from " + overrideCompilerPath + ": " + err));

                    var elmCompilerHash =
                        new ConcurrentPineValueHashCache()
                        .GetHash(overrideElmCompiler.CompilerEnvironment);

                    Console.WriteLine(
                        "Loaded Elm compiler with hash " + Convert.ToHexStringLower(elmCompilerHash.Span)[..8]);
                }

                var compiledEnvironments =
                    environmentsSourceTrees
                    .Select(
                        sourceTree =>
                        {
                            var compiledEnv =
                                ElmCompilerInElm.LoadOrCompileInteractiveEnvironment(
                                    sourceTree,
                                    rootFilePaths: rootFilePaths,
                                    skipLowering: skipLowering,
                                    overrideElmCompiler: overrideElmCompiler)
                                .Extract(err => throw new Exception("Failed compilation: " + err));

                            return new KeyValuePair<FileTree, PineValue>(sourceTree, compiledEnv);
                        })
                    .ToImmutableDictionary();

                foreach (var (sourceTree, compiledEnv) in compiledEnvironments)
                {
                    var sourceTreeHash = PineValueHashTree.ComputeHashSorted(sourceTree);

                    var sourceTreeAllFiles =
                        sourceTree
                        .EnumerateFilesTransitive()
                        .ToImmutableArray();

                    var sourceTreeElmModules =
                        sourceTreeAllFiles
                        .Where(f => f.path?.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ?? false)
                        .ToImmutableArray();

                    var environmentNodesCount =
                        compiledEnv is PineValue.ListValue compiledEnvList
                        ?
                        compiledEnvList.NodesCount
                        :
                        0;

                    Console.WriteLine(
                        "Compiled source tree " + Convert.ToHexStringLower(sourceTreeHash.Span)[..8] +
                        " containing " + sourceTreeAllFiles.Length +
                        " files and " + sourceTreeElmModules.Length +
                        " Elm modules into environment with " +
                        CommandLineInterface.FormatIntegerForDisplay(environmentNodesCount) + " nodes.");
                }

                var (allComponents, bundleResourceFile) =
                    BundledDeclarations.BuildBundleFile(
                        compiledEnvironments: compiledEnvironments,
                        otherReusedValues: ImmutableDictionary<string, PineValue>.Empty);

                Console.WriteLine(
                    "Built bundle containing " +
                    CommandLineInterface.FormatIntegerForDisplay(allComponents.Count) +
                    " component entries in " +
                    CommandLineInterface.FormatIntegerForDisplay(bundleResourceFile.Length) + " bytes.");

                var fileContent = bundleResourceFile;

                if (gzip)
                {
                    fileContent = BundledDeclarations.CompressResourceFile(fileContent);

                    Console.WriteLine(
                        "Applied gzip and compressed from " +
                        CommandLineInterface.FormatIntegerForDisplay(bundleResourceFile.Length) +
                        " to " +
                        CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) + " bytes");
                }

                if (outputCompactBuild != null)
                {
                    var destFilePath = outputCompactBuild.Length > 0 ? outputCompactBuild : "compact-build.bin";

                    if (Path.GetDirectoryName(destFilePath) is { } destDirectory && destDirectory.Length is not 0)
                    {
                        Directory.CreateDirectory(destDirectory);
                    }

                    File.WriteAllBytes(
                        destFilePath,
                        fileContent.ToArray());

                    Console.WriteLine(
                        "Saved compact build with " +
                        CommandLineInterface.FormatIntegerForDisplay(allComponents.Count) +
                        " component entries in " +
                        CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                        " bytes to " + destFilePath);
                }

                return 0;
            });

        return command;
    }
}
