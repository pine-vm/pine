using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using ElmFullstack;
using Pine;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive;

public class ElmInteractive
{
    static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
        TreeWithStringPath appCodeTree,
        string submission,
        IReadOnlyList<string>? previousLocalSubmissions = null)
    {
        using var jsEngine = PrepareJsEngineToEvaluateElm();

        return EvaluateSubmissionAndGetResultingValue(
            jsEngine,
            appCodeTree: appCodeTree,
            submission: submission,
            previousLocalSubmissions: previousLocalSubmissions);
    }

    static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        TreeWithStringPath? appCodeTree,
        string submission,
        IReadOnlyList<string>? previousLocalSubmissions = null)
    {
        var modulesTexts =
            appCodeTree == null ? null
            :
            TreeToFlatDictionaryWithPathComparer(compileTree(appCodeTree)!)
            .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.ToArray()) : null)
            .WhereNotNull()
            .ToImmutableList();

        var argumentsJson = Newtonsoft.Json.JsonConvert.SerializeObject(
            new
            {
                modulesTexts = modulesTexts ?? ImmutableList<string>.Empty,
                submission = submission,
                previousLocalSubmissions = previousLocalSubmissions ?? ImmutableList<string>.Empty,
            }
        );

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("evaluateSubmissionInInteractive", argumentsJson).ToString()!;

        var responseStructure =
            Newtonsoft.Json.JsonConvert.DeserializeObject<EvaluateSubmissionResponseStructure>(responseJson);

        if (responseStructure.DecodedArguments == null)
            throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

        if (responseStructure.DecodedArguments.Evaluated == null)
            return Result<string, SubmissionResponseValueStructure>.err(responseStructure.DecodedArguments.FailedToEvaluate!);

        return Result<string, SubmissionResponseValueStructure>.ok(
            responseStructure.DecodedArguments.Evaluated.SubmissionResponseValue);
    }

    static TreeWithStringPath? compileTree(TreeWithStringPath? sourceTree)
    {
        if (sourceTree == null)
            return null;

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: TreeToFlatDictionaryWithPathComparer(sourceTree),
            ElmAppInterfaceConfig.Default);

        if (compilationResult.Ok == null)
        {
            var errorMessage = "\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationResult.Err) + "\n";

            Console.WriteLine(errorMessage);

            throw new Exception(errorMessage);
        }

        return SortedTreeFromSetOfBlobsWithStringPath(compilationResult.Ok.compiledAppFiles);
    }

    static public JavaScriptEngineSwitcher.Core.IJsEngine PrepareJsEngineToEvaluateElm()
    {
        var parseElmAppCodeFiles = ParseElmSyntaxAppCodeFiles();

        var javascriptFromElmMake =
            ProcessFromElm019Code.CompileElmToJavascript(
                parseElmAppCodeFiles,
                ImmutableList.Create("src", "Main.elm"));

        var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

        var listFunctionToPublish =
            new[]
            {
                    (functionNameInElm: "Main.evaluateSubmissionInInteractive",
                    publicName: "evaluateSubmissionInInteractive",
                    arity: 1),
            };

        var javascriptPreparedToRun =
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish);

        var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

        javascriptEngine.Evaluate(javascriptPreparedToRun);

        return javascriptEngine;
    }

    static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> ParseElmSyntaxAppCodeFiles() =>
        DotNetAssembly.LoadFromAssemblyManifestResourceStreamContents(
            filePaths: new[]
            {
                ImmutableList.Create("elm.json"),
                ImmutableList.Create("src", "Pine.elm"),
                ImmutableList.Create("src", "ElmInteractive.elm"),
                ImmutableList.Create("src", "Main.elm")
            },
            resourceNameCommonPrefix: "elm_fullstack.ElmInteractive.interpret_elm_program.",
            assembly: typeof(ElmInteractive).Assembly).Ok!;

    record EvaluateSubmissionResponseStructure
        (string? FailedToDecodeArguments = null,
        DecodedArgumentsSctructure? DecodedArguments = null);

    record DecodedArgumentsSctructure(
        string? FailedToEvaluate = null,
        EvaluatedSctructure? Evaluated = null);

    record EvaluatedSctructure(
        object? SubmissionResponseNoValue = null,
        SubmissionResponseValueStructure? SubmissionResponseValue = null);

    public record SubmissionResponseValueStructure(
        string valueAsElmExpressionText,
        string valueAsJsonString,
        string typeText);
}
