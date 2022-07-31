using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using ElmFullstack;
using Pine;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive;

public class ElmInteractive
{
    static public readonly Lazy<string> JavascriptToEvaluateElm = new(PrepareJavascriptToEvaluateElm, System.Threading.LazyThreadSafetyMode.ExecutionAndPublication);

    static public Result<string, EvaluatedSctructure> EvaluateSubmissionAndGetResultingValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        TreeWithStringPath? appCodeTree,
        string submission,
        IReadOnlyList<string>? previousLocalSubmissions = null)
    {
        var modulesTexts = ModulesTextsFromAppCodeTree(appCodeTree);

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
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
            System.Text.Json.JsonSerializer.Deserialize<EvaluateSubmissionResponseStructure>(responseJson)!;

        if (responseStructure.DecodedArguments == null)
            throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

        if (responseStructure.DecodedArguments.Evaluated == null)
            return Result<string, EvaluatedSctructure>.err(responseStructure.DecodedArguments.FailedToEvaluate!);

        return Result<string, EvaluatedSctructure>.ok(
            responseStructure.DecodedArguments.Evaluated);
    }

    static public Result<string, Component> PineEvalContextForElmInteractive(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        TreeWithStringPath? appCodeTree)
    {
        var modulesTexts = ModulesTextsFromAppCodeTree(appCodeTree);

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(modulesTexts ?? ImmutableList<string>.Empty);

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("pineEvalContextForElmInteractive", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<ResultFromJsonResult<string, PineValueFromJson>>(
                responseJson,
                new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        return
            responseStructure
            .AsResult()
            .map(fromJson => ParsePineComponentFromJson(fromJson!));
    }

    static public Result<string?, Component?> CompileInteractiveSubmissionIntoPineExpression(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        Component environment,
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        void logDuration(string label) =>
            addInspectionLogEntry?.Invoke(
                label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

        var requestJson =
            System.Text.Json.JsonSerializer.Serialize(
                new CompileInteractiveSubmissionIntoPineExpressionRequest
                (
                    environment: PineValueFromJson.FromComponentBuildingDictionary(environment),
                    submission: submission
                ),
                options: new System.Text.Json.JsonSerializerOptions
                {
                    DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
                    MaxDepth = 1000
                });

        logDuration("Serialize to JSON (" + CommandLineInterface.FormatIntegerForDisplay(requestJson.Length) + " chars)");

        clock.Restart();

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("compileInteractiveSubmissionIntoPineExpression", requestJson).ToString()!;

        logDuration("JavaScript function");

        clock.Restart();

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<ResultFromJsonResult<string?, PineValueFromJson?>>(
                responseJson,
                options: new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        var response =
            responseStructure
            .AsResult()
            .map(fromJson => (Component?)ParsePineComponentFromJson(fromJson!));

        logDuration("Deserialize (from " + CommandLineInterface.FormatIntegerForDisplay(responseJson.Length) + " chars) and " + nameof(ParsePineComponentFromJson));

        return response;
    }

    record CompileInteractiveSubmissionIntoPineExpressionRequest(
        PineValueFromJson environment,
        string submission);

    static public Result<string?, EvaluatedSctructure?> SubmissionResponseFromResponsePineValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        Component response)
    {
        var responseJson =
            evalElmPreparedJsEngine.CallFunction(
                "submissionResponseFromResponsePineValue",
                System.Text.Json.JsonSerializer.Serialize(PineValueFromJson.FromComponentWithoutBuildingDictionary(response))).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<ResultFromJsonResult<string?, EvaluatedSctructure?>>(responseJson)!;

        return responseStructure.AsResult();
    }

    public record ResultFromJsonResult<ErrT, OkT>
    {
        public IReadOnlyList<ErrT>? Err { set; get; }

        public IReadOnlyList<OkT>? Ok { set; get; }

        public Result<ErrT, OkT> AsResult()
        {
            if (Err?.Count == 1 && (Ok?.Count ?? 0) == 0)
                return Result<ErrT, OkT>.err(Err.Single());

            if ((Err?.Count ?? 0) == 0 && Ok?.Count == 1)
                return Result<ErrT, OkT>.ok(Ok.Single());

            throw new Exception("Unexpected shape: Err: " + Err?.Count + ", OK: " + Ok?.Count);
        }
    }

    record PineValueFromJson
    {
        public IReadOnlyCollection<DictionaryEntry>? Dictionary { init; get; } = null;

        public IReadOnlyList<PineValueFromJson>? List { init; get; }

        public IReadOnlyList<int>? Blob { init; get; }

        public string? ListAsString { init; get; }

        public string? Reference { init; get; }

        public record DictionaryEntry(string key, PineValueFromJson value);

        record PineValueFromJsonIntermediate(
            IReadOnlyList<PineValueFromJsonIntermediate>? List,
            ReadOnlyMemory<byte>? Blob,
            string? ListAsString)
            : IEquatable<PineValueFromJsonIntermediate>
        {
            public virtual bool Equals(PineValueFromJsonIntermediate? other) =>
                Equals(this, other);

            public override int GetHashCode()
            {
                if (ListAsString != null)
                    return ListAsString.GetHashCode();

                if (Blob.HasValue)
                    return Blob.GetHashCode();

                if (List != null)
                    return List.GetHashCode();

                return base.GetHashCode();
            }

            static public bool Equals(PineValueFromJsonIntermediate? left, PineValueFromJsonIntermediate? right)
            {
                if (left is null && right is null)
                    return true;

                if (left is null || right is null)
                    return false;

                if (left.ListAsString != null && right.ListAsString != null)
                    return left.ListAsString == right.ListAsString;

                if (left.List != null && right.List != null)
                {
                    if (left.List.Count != right.List.Count)
                        return false;

                    var leftEnumerator = left.List.GetEnumerator();
                    var rightEnumerator = right.List.GetEnumerator();

                    while (leftEnumerator.MoveNext())
                    {
                        rightEnumerator.MoveNext();

                        if (!Equals(leftEnumerator.Current, rightEnumerator.Current))
                            return false;
                    }

                    return true;
                }

                if (left.Blob != null && right.Blob != null)
                    return left.Blob.Value.Span.SequenceEqual(right.Blob.Value.Span);

                return false;
            }

            static public PineValueFromJsonIntermediate FromComponent(Component component)
            {
                var asStringResult = StringFromComponent(component);

                if (asStringResult.Ok is string asString)
                    return new PineValueFromJsonIntermediate(ListAsString: asString, Blob: null, List: null);

                if (component.ListContent != null)
                    return new PineValueFromJsonIntermediate
                    (
                        List: component.ListContent.Select(FromComponent).ToImmutableList(),
                        Blob: null,
                        ListAsString: null
                    );

                if (component.BlobContent != null)
                    return new PineValueFromJsonIntermediate(Blob: component.BlobContent, ListAsString: null, List: null);

                throw new NotImplementedException("Unexpected shape");
            }
        }


        static public PineValueFromJson FromComponentBuildingDictionary(Component component)
        {
            var intermediate = PineValueFromJsonIntermediate.FromComponent(component);

            var usageCount = new ConcurrentDictionary<PineValueFromJsonIntermediate, int>();

            void mutatingCountUsagesRecursive(PineValueFromJsonIntermediate component)
            {
                usageCount!.AddOrUpdate(component, addValue: 1, updateValueFactory: (_, countBefore) => countBefore + 1);

                if (component.List is not null)
                {
                    foreach (var item in component.List)
                    {
                        mutatingCountUsagesRecursive(item);
                    }
                }
            }

            mutatingCountUsagesRecursive(intermediate);

            var componentsUsedMultipleTimes =
                usageCount
                .Where(count => count.Value > 1)
                .ToImmutableDictionary(x => x.Key, x => x.Value);

            int keyIndex = 0;

            var dictionary = new ConcurrentDictionary<PineValueFromJsonIntermediate, string>();

            void mutatingBuildDictionaryRecursive(PineValueFromJsonIntermediate component)
            {
                if (dictionary!.ContainsKey(component))
                    return;

                if (componentsUsedMultipleTimes!.ContainsKey(component))
                {
                    dictionary[component] = keyIndex++.ToString();
                    return;
                }

                if (component.List is not null)
                {
                    foreach (var item in component.List)
                    {
                        mutatingBuildDictionaryRecursive(item);
                    }
                }
            }

            mutatingBuildDictionaryRecursive(intermediate);

            var dictionaryForSerial =
                dictionary
                .Select(entry => new DictionaryEntry(key: entry.Value, value: FromComponentWithoutBuildingDictionary(entry.Key)))
                .ToImmutableArray();

            return FromComponentWithoutBuildingDictionary(
                intermediate,
                dictionary: dictionary)
                with
            { Dictionary = dictionaryForSerial };
        }

        static public PineValueFromJson FromComponentWithoutBuildingDictionary(Component component) =>
            FromComponentWithoutBuildingDictionary(PineValueFromJsonIntermediate.FromComponent(component));

        static PineValueFromJson FromComponentWithoutBuildingDictionary(
            PineValueFromJsonIntermediate intermediate,
            IReadOnlyDictionary<PineValueFromJsonIntermediate, string>? dictionary = null)
        {
            if (dictionary?.TryGetValue(intermediate, out var result) ?? false)
                return new PineValueFromJson { Reference = result };

            if (intermediate.ListAsString != null)
                return new PineValueFromJson { ListAsString = intermediate.ListAsString };

            if (intermediate.List != null)
                return new PineValueFromJson
                {
                    List = intermediate.List.Select(
                        listElement => FromComponentWithoutBuildingDictionary(listElement, dictionary)).ToImmutableList()
                };

            if (intermediate.Blob != null)
                return new PineValueFromJson { Blob = intermediate.Blob.Value.ToArray().Select(b => (int)b).ToImmutableArray() };

            throw new NotImplementedException("Unexpected shape");
        }
    }

    static Component ParsePineComponentFromJson(PineValueFromJson fromJson)
    {
        if (fromJson.List != null)
            return Component.List(fromJson.List.Select(ParsePineComponentFromJson).ToImmutableList());

        if (fromJson.Blob != null)
            return Component.Blob(fromJson.Blob.Select(b => (byte)b).ToArray());

        if (fromJson.ListAsString != null)
            return ComponentFromString(fromJson.ListAsString);

        throw new NotImplementedException("Unexpected shape of Pine value from JSON");
    }

    static IReadOnlyCollection<string>? ModulesTextsFromAppCodeTree(TreeWithStringPath? appCodeTree) =>
        appCodeTree == null ? null
        :
        TreeToFlatDictionaryWithPathComparer(compileTree(appCodeTree)!)
        .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.ToArray()) : null)
        .WhereNotNull()
        .ToImmutableList();

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
        var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

        javascriptEngine.Evaluate(JavascriptToEvaluateElm.Value);

        return javascriptEngine;
    }

    static public string PrepareJavascriptToEvaluateElm()
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

                (functionNameInElm: "Main.pineEvalContextForElmInteractive",
                publicName: "pineEvalContextForElmInteractive",
                arity: 1),

                (functionNameInElm: "Main.compileInteractiveSubmissionIntoPineExpression",
                publicName: "compileInteractiveSubmissionIntoPineExpression",
                arity: 1),

                (functionNameInElm: "Main.submissionResponseFromResponsePineValue",
                publicName: "submissionResponseFromResponsePineValue",
                arity: 1),
            };

        return
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish);
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> ParseElmSyntaxAppCodeFiles() =>
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

    public record EvaluatedSctructure(
        string displayText);
}
