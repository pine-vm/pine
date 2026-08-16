using ElmTime;
using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Elm;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;

using ElmInteractiveImplementation = ElmTime.ElmInteractive.ElmInteractive;

namespace Pine.CLI;

public static class InteractiveCommand
{
    private static (Argument<string> siteArgument, Option<string?> passwordOption, Func<ParseResult, string?, string?> getPassword)
        ProcessSiteArgumentAndPasswordOption(Command cmd, string? siteName = null)
    {
        siteName ??= "site";

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption =
            new Option<string?>(
                name: "--" + siteName + "-password")
            {
                Description = "Password for " + siteName + "."
            };

        cmd.Add(siteArgument);
        cmd.Add(sitePasswordOption);

        Func<ParseResult, string?, string?> getPassword =
            (parseResult, site) =>
            site == null ? null : (parseResult.GetValue(sitePasswordOption) ?? UserSecrets.LoadPasswordForSite(site));

        return (siteArgument, sitePasswordOption, getPassword);
    }

    private static (Option<string?> elmEngineOption, Func<ElmEngineType> parseElmEngineTypeFromOption)
        CreateElmEngineOption(
        DynamicPGOShare? dynamicPGOShare,
        string? defaultFromEnvironmentVariablePrefix,
        Func<ElmEngineTypeCLI?, ElmEngineTypeCLI> defaultEngineConsideringEnvironmentVariable)
    {
        var defaultEngineFromEnvironmentVariable =
            defaultFromEnvironmentVariablePrefix switch
            {
                { } variablePrefix => ElmEngineFromEnvironmentVariableWithPrefix(variablePrefix),
                null => null
            };

        var defaultEngine = defaultEngineConsideringEnvironmentVariable(defaultEngineFromEnvironmentVariable);

        var elmEngineOption =
            new Option<string?>(
                name: "--elm-engine" + string.Join(", ", Enum.GetNames<ElmEngineTypeCLI>()) + "). Defaults to " +
                    defaultEngine);

        ElmEngineType parseElmEngineTypeFromOption()
        {
            var cliName =
                defaultEngine; // Simplified for now - would need to get actual option value

            return
                ParseElmEngineType(
                    dynamicPGOShare,
                    cliName);
        }

        return (elmEngineOption, parseElmEngineTypeFromOption);
    }

    private static (Option<string?> elmCompilerOption, Func<IConsole, FileTree> loadElmCompilerFromOption)
        CreateElmCompilerOption()
    {
        var defaultCompiler = ElmCompilerInElm.CompilerSourceFilesDefault.Value;

        var elmCompilerOption = new Option<string?>("--elm-compiler");

        FileTree parseElmCompilerFromOption(IConsole console)
        {
            // Simplified - would need to handle option value
            return defaultCompiler;
        }

        return (elmCompilerOption, parseElmCompilerFromOption);
    }

    public static Command Create(DynamicPGOShare? dynamicPGOShare)
    {
        var command =
            new Command("interactive", "Enter environment for interactive exploration and composition of Elm programs.")
            {
                Aliases = { "repl" }
            };

        // Options
        var contextAppOption =
            new Option<string[]>("--context-app")
            {
                Description =
                "Path to an app to use as context. The Elm modules from this app will be available in the interactive environment.",
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var contextAppModuleNameFilterOption =
            new Option<string?>("--context-app-module-name-filter")
            {
                Description = "Filter on module names to apply on modules loaded via the '--context-app' option."
            };

        var initStepsOption =
            new Option<string?>("--init-steps")
            {
                Description = "Path to a list of submissions to start the session with."
            };

        var enableInspectionOption =
            new Option<bool>("--enable-inspection")
            {
                Description = "Display additional information to inspect the implementation."
            };

        var submitOption =
            new Option<string[]>("--submit")
            {
                Description = "Option to submit a string as if entered during the interactive session.",
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var saveToFileOption =
            new Option<string?>("--save-to-file")
            {
                Description =
                "Path to a file to save the session state to, after compiling context app and initial submissions."
            };

        var (elmCompilerOption, loadElmCompilerFromOption) = CreateElmCompilerOption(command);

        var (elmEngineOption, parseElmEngineTypeFromOption) =
            CreateElmEngineOption(
                dynamicPGOShare: null,
                command,
                defaultFromEnvironmentVariablePrefix: "interactive",
                defaultEngineConsideringEnvironmentVariable:
                fromEnv => fromEnv ?? IInteractiveSession.DefaultImplementation);

        command.Add(contextAppOption);
        command.Add(contextAppModuleNameFilterOption);
        command.Add(initStepsOption);
        command.Add(enableInspectionOption);
        command.Add(submitOption);
        command.Add(saveToFileOption);
        command.Add(elmCompilerOption);
        command.Add(elmEngineOption);

        // Test subcommand
        var testCommand =
            new Command("test", "Test the interactive automatically with given scenarios and reports timings.");

        var scenarioOption =
            new Option<string[]>("--scenario")
            {
                Description =
                "Test an interactive scenario from the given path. The scenario specifies the submissions and can also specify expectations.",
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var scenariosOption =
            new Option<string[]>("--scenarios")
            {
                Description =
                "Test a list of interactive scenarios from the given directory. Each scenario specifies the submissions and can also specify expectations.",
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        testCommand.Add(scenarioOption);
        testCommand.Add(scenariosOption);

        testCommand.SetAction(
            (parseResult) =>
            {
                var scenarioSources = parseResult.GetValue(scenarioOption) ?? [];
                var scenariosSources = parseResult.GetValue(scenariosOption) ?? [];

                var console = (IConsole)StaticConsole.Instance;

                console.WriteLine("Got " + scenarioSources.Length + " source(s) for an individual scenario to load...");

                console.WriteLine(
                    "Got " + scenariosSources.Length + " source(s) for a directory of scenarios to load...");

                var scenarioLoadResults =
                    scenarioSources
                    .ToImmutableDictionary(
                        scenarioSource => scenarioSource!,
                        scenarioSource =>
                        LoadComposition.LoadFromPathResolvingNetworkDependencies(scenarioSource!).LogToList());

                var scenariosLoadResults =
                    scenariosSources
                    .ToImmutableDictionary(
                        scenariosSource => scenariosSource!,
                        scenariosSource =>
                        LoadComposition.LoadFromPathResolvingNetworkDependencies(scenariosSource!).LogToList());

                var failedLoads =
                    scenarioLoadResults.Concat(scenariosLoadResults)
                    .Where(r => !r.Value.result.IsOk())
                    .ToImmutableList();

                if (!failedLoads.IsEmpty)
                {
                    var failedLoad = failedLoads.First();

                    console.WriteLine(
                        string.Join(
                            "\n",
                            "Failed to load from " + failedLoad.Key + ":",
                            string.Join("\n", failedLoad.Value.log),
                            failedLoad.Value.result.Unpack(
                                fromErr: error => error,
                                fromOk: _ => throw new NotImplementedException())),
                        color: IConsole.TextColor.Red);

                    return 1;
                }

                var namedDistinctScenarios =
                    scenarioLoadResults
                    .Select(
                        scenarioLoadResult =>
                        (name: scenarioLoadResult.Key.Split('/', '\\').Last(),
                        component: scenarioLoadResult.Value.result.Extract(error => throw new Exception(error)).tree))
                    .Concat(
                        scenariosLoadResults.SelectMany(
                            scenariosComposition =>
                            {
                                var asTree =
                                    scenariosComposition.Value.result.Extract(error => throw new Exception(error)).tree switch
                                    {
                                        FileTree.DirectoryNode tree => tree,

                                        _ =>
                                        null
                                    };

                                if (asTree is null)
                                    return ImmutableList<(string, FileTree)>.Empty;

                                return
                                    asTree.Items
                                    .Where(entry => entry.component is FileTree.DirectoryNode scenarioTree);
                            }))
                    .Select(
                        loadedScenario =>
                        {
                            var asComposition = FileTreeEncoding.Encode(loadedScenario.component);

                            var hashBase16 =
                                Convert.ToHexStringLower(PineValueHashTree.ComputeHash(asComposition).Span);

                            return
                                new
                                {
                                    loadedScenario,
                                    asComposition,
                                    hashBase16
                                };
                        })
                    .DistinctBy(loadedScenario => loadedScenario.hashBase16)
                    .ToImmutableDictionary(
                        keySelector: scenario => scenario.loadedScenario.name + "-" + scenario.hashBase16[..10],
                        elementSelector: scenario => scenario);

                var compileElmProgramCodeFiles = loadElmCompilerFromOption(parseResult, console);
                var elmEngineType = parseElmEngineTypeFromOption(parseResult);

                var aggregateCompositionTree =
                    FileTree.SortedDirectory(
                        [
                        .. namedDistinctScenarios.Select(
                            scenario => (scenario.Key, scenario.Value.loadedScenario.component))
                        ]);

                var parsedScenarios =
                    TestElmInteractive.ParseElmInteractiveScenarios(
                        aggregateCompositionTree,
                        console);

                IInteractiveSession newInteractiveSessionFromAppCode(FileTree? appCodeTree)
                {
                    return
                        IInteractiveSession.Create(
                            compilerSourceFiles: compileElmProgramCodeFiles,
                            appCodeTree: appCodeTree,
                            elmEngineType);
                }

                var interactiveConfig =
                    new InteractiveSessionConfig(
                        CompilerId:
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(compileElmProgramCodeFiles).Span)[..8],
                        newInteractiveSessionFromAppCode);

                {
                    var warmupStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    using var session = interactiveConfig.SessionFromAppCode(null);

                    session.Submit("1 + 3");

                    console.WriteLine(
                        "Warmup completed in " +
                        warmupStopwatch.Elapsed.TotalSeconds.ToString("0.##") + " seconds.");
                }

                var scenariosResults =
                    TestElmInteractive.TestElmInteractiveScenarios(
                        parsedScenarios,
                        interactiveConfig,
                        console: console,
                        asyncLogDelegate: null);

                return 0;
            });

        command.Add(testCommand);

        // Main interactive command handler
        command.SetAction(
            (parseResult) =>
            {
                var contextAppPaths = parseResult.GetValue(contextAppOption) ?? [];
                var contextAppModuleNameFilterPattern = parseResult.GetValue(contextAppModuleNameFilterOption);
                var initStepsPath = parseResult.GetValue(initStepsOption);
                var enableInspection = parseResult.GetValue(enableInspectionOption);
                var submitsFromOption = parseResult.GetValue(submitOption) ?? [];
                var saveToFile = parseResult.GetValue(saveToFileOption);

                var console = (IConsole)StaticConsole.Instance;

                var compileElmProgramCodeFiles = loadElmCompilerFromOption(parseResult, console);
                var elmEngineType = parseElmEngineTypeFromOption(parseResult);

                console.WriteLine(
                    "---- Elm Interactive v" + PineCliCommand.AppVersionId + " ----");

                FileTree loadContextAppCodeTreeFromPath(string contextAppPath)
                {
                    return
                        LoadComposition.LoadFromPathResolvingNetworkDependencies(contextAppPath)
                        .LogToActions(console.WriteLine)
                        .Map(loaded => loaded.tree)
                        .Unpack(
                            fromErr:
                            error => throw new Exception("Failed to load from path '" + contextAppPath + "': " + error),
                            fromOk: tree =>
                            {
                                if (!tree.EnumerateFilesTransitive().Take(1).Any())
                                    throw new Exception("Found no files under context app path '" + contextAppPath + "'.");

                                return tree;
                            });
                }

                var contextAppCodeTreeBeforeFilter =
                    contextAppPaths.Length == 0
                    ?
                    null
                    :
                    FileTreeExtensions.Union(contextAppPaths.Select(loadContextAppCodeTreeFromPath!));

                var contextAppModuleNameFilterIncluded =
                    contextAppModuleNameFilterPattern is null
                    ?
                    []
                    :
                    contextAppModuleNameFilterPattern
                    .Split(',')
                    .Select(moduleName => moduleName.ToLowerInvariant())
                    .ToImmutableHashSet();

                bool contextAppModuleNameFilter(IReadOnlyList<string> moduleName)
                {
                    if (contextAppModuleNameFilterPattern is null)
                        return true;

                    var flatModuleName = string.Join('.', moduleName).ToLowerInvariant();

                    return contextAppModuleNameFilterIncluded.Contains(flatModuleName);
                }

                var contextAppCodeTree =
                    contextAppCodeTreeBeforeFilter is null
                    ?
                    null
                    :
                    ElmModule.FilterAppCodeTreeForRootModulesAndDependencies(
                        contextAppCodeTreeBeforeFilter,
                        moduleNameIsRootModule: contextAppModuleNameFilter);

                var initStepsSubmission =
                    initStepsPath switch
                    {
                        null =>
                        [],

                        not null =>
                        LoadComposition.LoadFromPathResolvingNetworkDependencies(initStepsPath)
                        .LogToActions(console.WriteLine)
                        .Map(loaded => loaded.tree)
                        .Unpack(
                            fromErr:
                            error => throw new Exception("Failed to load from path '" + initStepsPath + "': " + error),
                            fromOk: treeNode =>
                            {
                                if (!treeNode.EnumerateFilesTransitive().Take(1).Any())
                                    throw new Exception("Found no files under context app path '" + initStepsPath + "'.");

                                return
                                    treeNode
                                    .Map(
                                        fromFile: _ => throw new Exception("Unexpected blob"),
                                        fromDirectory: tree =>
                                        tree.Select(
                                            stepDirectory =>
                                            TestElmInteractive.ParseScenarioStep(stepDirectory.itemValue)
                                            .Extract(fromErr: error => throw new Exception(error)).Submission))
                                    .ToImmutableList();
                            })
                    };

                using var interactiveSession =
                    IInteractiveSession.Create(
                        compilerSourceFiles: compileElmProgramCodeFiles,
                        appCodeTree: contextAppCodeTree,
                        engineType: elmEngineType);

                string? processSubmission(string submission)
                {
                    if (!(0 < submission?.Trim()?.Length))
                        return null;

                    var evalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var evalResult = interactiveSession.Submit(submission);

                    evalStopwatch.Stop();

                    return
                        evalResult
                        .Unpack(
                            fromErr: error =>
                            {
                                console.WriteLine("Failed to evaluate: " + error);
                                return submission;
                            },
                            fromOk: evalOk =>
                            {
                                if (enableInspection)
                                {
                                    console.WriteLine(
                                        "Processing this submission took " +
                                        CommandLineInterface.FormatIntegerForDisplay(evalStopwatch.ElapsedMilliseconds) +
                                        " ms.");

                                    console.WriteLine(
                                        "Inspection log has " + (evalOk.InspectionLog?.Count ?? 0) + " entries:\n" +
                                        string.Join("\n", evalOk.InspectionLog.EmptyIfNull()));
                                }

                                console.WriteLine(evalOk.InteractiveResponse.DisplayText);

                                return submission;
                            });
                }

                var promptPrefix = "> ";

                var allSubmissionsFromArguments =
                    initStepsSubmission
                    .Concat(submitsFromOption.EmptyIfNull()).WhereNotNull()
                    .ToImmutableList();

                if (0 < allSubmissionsFromArguments.Count)
                {
                    console.WriteLine(
                        allSubmissionsFromArguments.Count + " initial submission(s) from arguments in total...");
                }

                foreach (var submission in allSubmissionsFromArguments)
                {
                    console.WriteLine(promptPrefix + submission);

                    processSubmission(submission);
                }

                if (saveToFile is not null)
                {
                    console.WriteLine("Got option to save session state to " + saveToFile + "...");

                    if (interactiveSession is not InteractiveSessionPine pineSession)
                    {
                        console.WriteLine(
                            "Cannot save session state for this engine type: " + interactiveSession.GetType().Name);
                    }
                    else
                    {
                        var sessionState = pineSession.CurrentEnvironmentValue();

                        var (environmentJson, _) =
                            ElmInteractiveImplementation.FromPineValueBuildingDictionary(
                                sessionState,
                                ElmInteractiveImplementation.CompilationCache.Empty);

                        var environmentJsonString =
                            System.Text.Json.JsonSerializer.Serialize(
                                environmentJson.json,
                                options: ElmInteractiveImplementation.compilerInterfaceJsonSerializerOptions);

                        File.WriteAllText(saveToFile, environmentJsonString);

                        console.WriteLine(
                            "Saved session state to " + saveToFile + ", as JSON with total length of " +
                            environmentJsonString.Length);
                    }

                    return 0;
                }

                ReadLine.HistoryEnabled = true;

                while (true)
                {
                    var submission = ReadLine.Read(promptPrefix);

                    processSubmission(submission);
                }
            });

        return command;
    }

    private static (Option<string?> elmEngineOption, Func<ParseResult, ElmEngineType> parseElmEngineTypeFromOption)
        CreateElmEngineOption(
        DynamicPGOShare? dynamicPGOShare,
        Command cmd,
        string? defaultFromEnvironmentVariablePrefix,
        Func<ElmEngineTypeCLI?, ElmEngineTypeCLI> defaultEngineConsideringEnvironmentVariable)
    {
        var defaultEngineFromEnvironmentVariable =
            defaultFromEnvironmentVariablePrefix switch
            {
                { } variablePrefix => ElmEngineFromEnvironmentVariableWithPrefix(variablePrefix),
                null => null
            };

        var defaultEngine = defaultEngineConsideringEnvironmentVariable(defaultEngineFromEnvironmentVariable);

        var elmEngineOption =
            new Option<string?>("--elm-engine")
            {
                Description =
                "Select the engine for running Elm programs (" + string.Join(", ", Enum.GetNames<ElmEngineTypeCLI>()) +
                "). Defaults to " +
                defaultEngine,
                Arity = ArgumentArity.ZeroOrOne
            };

        ElmEngineType parseElmEngineTypeFromOption(ParseResult parseResult)
        {
            var cliName =
                parseResult.GetValue(elmEngineOption) switch
                {
                    { } asString => Enum.Parse<ElmEngineTypeCLI>(asString, ignoreCase: true),
                    null => defaultEngine,
                };

            return
                ParseElmEngineType(
                    dynamicPGOShare,
                    cliName);
        }

        return (elmEngineOption, parseElmEngineTypeFromOption);
    }

    private static (Option<string?> elmCompilerOption, Func<ParseResult, IConsole, FileTree> loadElmCompilerFromOption)
        CreateElmCompilerOption(Command cmd)
    {
        var defaultCompiler = ElmCompilerInElm.CompilerSourceFilesDefault.Value;

        var elmCompilerOption =
            new Option<string?>("--elm-compiler")
            {
                Description =
                "Select a program for compiling Elm programs. Defaults to the version integrated with Pine.",
                Arity = ArgumentArity.ZeroOrOne
            };

        FileTree parseElmCompilerFromOption(ParseResult parseResult, IConsole console)
        {
            if (parseResult.GetValue(elmCompilerOption) is { } compilerAsString)
            {
                console.WriteLine("Loading Elm compiler from " + compilerAsString);

                return
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(compilerAsString)
                    .LogToActions(console.WriteLine)
                    .Extract(
                        error => throw new Exception("Failed to load from path '" + compilerAsString + "': " + error))
                    .tree;
            }

            return ElmCompilerInElm.CompilerSourceFilesDefault.Value;
        }

        return (elmCompilerOption, parseElmCompilerFromOption);
    }

    public static ElmEngineTypeCLI? ElmEngineFromEnvironmentVariableWithPrefix(string? environmentVariablePrefix)
    {
        var environmentVariable =
            environmentVariablePrefix?.TrimEnd('_') +
            (environmentVariablePrefix is null ? "" : "_") +
            "elm_engine";

        if (Environment.GetEnvironmentVariable(environmentVariable) is not { } asString)
            return null;

        if (Enum.TryParse<ElmEngineTypeCLI>(asString, ignoreCase: true, out var cliName))
            return cliName;

        return null;
    }

    public static ElmEngineType ParseElmEngineType(
        DynamicPGOShare? dynamicPGOShare,
        ElmEngineTypeCLI elmEngineTypeCLI) =>
        elmEngineTypeCLI switch
        {
            ElmEngineTypeCLI.Pine =>
            new ElmEngineType.Pine(
                Caching: true,
                DynamicPGOShare: null),

            ElmEngineTypeCLI.Pine_without_cache =>
            new ElmEngineType.Pine(
                Caching: false,
                DynamicPGOShare: null),

            _ =>
            throw new NotImplementedException($"Unexpected engine type value: {elmEngineTypeCLI}"),
        };
}
