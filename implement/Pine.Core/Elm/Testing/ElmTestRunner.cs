using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.IntermediateVM;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;

using IPineVM = Pine.Core.PineVM.IPineVM;
using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Compiles, runs, and renders Elm tests.
/// </summary>
public static class ElmTestRunner
{
    /// <summary>
    /// Compiles and runs the tests in an Elm project.
    /// </summary>
    public static ElmTestRun CompileAndRunTests(
        string appDirectory,
        IPineVM? pineVm = null,
        string? filter = null)
    {
        appDirectory = Path.GetFullPath(appDirectory);

        if (!Directory.Exists(appDirectory))
            throw new DirectoryNotFoundException("Elm project directory not found: " + appDirectory);

        var appFiles =
            Filesystem.GetFilesFromDirectory(
                appDirectory,
                filterByRelativeName:
                path =>
                !path.Any(
                    segment =>
                    segment is ".git" or "elm-stuff"))
            .Select(file => (file.path, file.content))
            .ToList();

        appFiles.Add(
            (["elm-test-support", "Expect.elm"],
            Encoding.UTF8.GetBytes(ExpectModuleText)));

        appFiles.Add(
            (["elm-test-support", "Test.elm"],
            Encoding.UTF8.GetBytes(TestModuleText)));

        var appCodeTree = FileTree.FromSetOfFilesWithStringPath(appFiles);

        var testModules =
            appFiles
            .Where(
                file =>
                file.path.Count > 1 &&
                file.path[0] is "tests" &&
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(
                file =>
                {
                    var moduleHeader =
                        ElmSyntaxParser.ParseModuleHeader(
                            Encoding.UTF8.GetString(file.content.Span))
                        .Extract(
                            error =>
                            throw new InvalidOperationException(
                                "Failed parsing Elm test module header: " + error));

                    return
                        (file.path,
                        moduleName: moduleHeader.ModuleName,
                        moduleNameText: string.Join('.', moduleHeader.ModuleName));
                })
            .OrderBy(testModule => testModule.moduleNameText, StringComparer.Ordinal)
            .ToImmutableArray();

        if (testModules.Length is 0)
            return new ElmTestRun.NoTestModules(appDirectory);

        var suiteDeclarationNames =
            testModules
            .Select(testModule => DeclQualifiedName.Create(testModule.moduleName, "suite"))
            .ToImmutableArray();

        var (compiledEnvironment, _) =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [.. testModules.Select(testModule => testModule.path)],
                rootDeclarationsAsPlainValues: suiteDeclarationNames)
            .Extract(error => throw new InvalidOperationException("Failed compiling Elm tests: " + error));

        var parsedEnvironment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvironment)
            .Extract(error => throw new InvalidOperationException("Failed parsing compiled Elm tests: " + error));

        var discoveredTests = new List<DiscoveredTest>();

        foreach (var testModule in testModules)
        {
            var compiledTestModule =
                parsedEnvironment.Modules
                .FirstOrDefault(module => module.moduleName == testModule.moduleNameText);

            if (compiledTestModule.moduleContent is null)
            {
                throw new InvalidOperationException(
                    "Did not find compiled Elm module '" + testModule.moduleNameText + "'");
            }

            if (!compiledTestModule.moduleContent.FunctionDeclarations.TryGetValue("suite", out var suiteValue))
            {
                throw new InvalidOperationException(
                    "Did not find declaration '" + testModule.moduleNameText + ".suite'");
            }

            DiscoverTests(suiteValue, path: [], discoveredTests);
        }

        if (filter is { } filterText)
        {
            discoveredTests.RemoveAll(
                test =>
                !test.Path.Any(
                    name => name.Contains(filterText, StringComparison.OrdinalIgnoreCase)));
        }

        var parseCache = new PineVMParseCache();
        pineVm ??= CreatePineVm();
        var completedTests = new List<CompletedTest>(discoveredTests.Count);
        var stopwatch = Stopwatch.StartNew();

        foreach (var discoveredTest in discoveredTests)
        {
            if (discoveredTest.Kind is DiscoveredTestKind.Todo)
            {
                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.Todo,
                        failure: null));

                continue;
            }

            if (discoveredTest.Kind is DiscoveredTestKind.EmptyGroup)
            {
                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.FailedEmptyGroup,
                        failure: null));

                continue;
            }

            if (discoveredTest.Thunk is null)
                throw new InvalidOperationException("Runnable test has no thunk");

            var functionRecord =
                FunctionRecord.ParseFunctionRecordTagged(discoveredTest.Thunk, parseCache)
                .Extract(error => throw new InvalidOperationException("Failed parsing test thunk: " + error));

            var expectationValue =
                ElmInteractiveEnvironment.ApplyFunction(pineVm, functionRecord, [PineValue.EmptyList])
                .Extract(error => throw new InvalidOperationException("Failed evaluating test thunk: " + error));

            var (expectationTag, expectationArguments) = ParseTaggedValue(expectationValue);

            if (expectationTag is "Pass")
            {
                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.Passed,
                        failure: null));

                continue;
            }

            if (expectationTag is "ComparisonFailure")
            {
                if (expectationArguments.Length is not 3)
                    throw new InvalidOperationException("ComparisonFailure must contain three arguments");

                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.Failed,
                        new EqualityFailure(
                            description: ParseElmString(expectationArguments.Span[0]),
                            actual: ParseElmString(expectationArguments.Span[1]),
                            expected: ParseElmString(expectationArguments.Span[2]))));

                continue;
            }

            if (expectationTag is "Fail")
            {
                if (expectationArguments.Length is not 1)
                    throw new InvalidOperationException("Fail must contain one argument");

                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.Failed,
                        new MessageFailure(
                            message: ParseElmString(expectationArguments.Span[0]))));

                continue;
            }

            throw new InvalidOperationException(
                "Unsupported expectation tag: " + expectationTag);
        }

        stopwatch.Stop();

        return new ElmTestRun.Completed(completedTests, stopwatch.Elapsed);
    }


    /// <summary>
    /// Renders completed Elm tests as styled output fragments.
    /// </summary>
    public static StructuredTestOutput RenderTestResults(
        IReadOnlyList<CompletedTest> tests,
        bool includeTestDetails,
        TimeSpan? duration = null)
    {
        var fragments = new List<TestOutputFragment>();
        var passedCount = tests.Count(test => test.Kind is CompletedTestKind.Passed);

        var failedCount =
            tests.Count(
                test =>
                test.Kind is CompletedTestKind.Failed or
                CompletedTestKind.FailedEmptyGroup);

        var todoCount = tests.Count(test => test.Kind is CompletedTestKind.Todo);

        Append(
            "Running " + tests.Count + " test" +
            (tests.Count is 1 ? "." : "s.") + "\n\n",
            TestOutputStyle.Default);

        if (includeTestDetails)
        {
            foreach (var failedTest in tests.Where(test => test.Kind is not CompletedTestKind.Passed))
            {
                if (failedTest.Kind is CompletedTestKind.Todo)
                {
                    Append(
                        "◦ TODO: " + failedTest.Path[^1] + "\n",
                        TestOutputStyle.Default);

                    continue;
                }

                if (failedTest.Kind is CompletedTestKind.FailedEmptyGroup)
                {
                    Append(
                        "\n    This `describe " + failedTest.Path[^1] +
                        "` has no tests in it. Let's give it some!\n",
                        TestOutputStyle.Failure);

                    continue;
                }

                foreach (var groupName in failedTest.Path.SkipLast(1))
                    Append("↓ " + groupName + "\n", TestOutputStyle.Dark);

                Append("✗ " + failedTest.Path[^1] + "\n", TestOutputStyle.Failure);

                if (failedTest.Failure is { } failure)
                {
                    switch (failure)
                    {
                        case EqualityFailure equalityFailure:
                            Append("\n    ", TestOutputStyle.Default);
                            AppendEqualityValue(equalityFailure.Actual, equalityFailure.Expected);

                            Append(
                                "\n    ╷" +
                                "\n    │ " + equalityFailure.Description +
                                "\n    ╵" +
                                "\n    ",
                                TestOutputStyle.Default);

                            AppendEqualityValue(equalityFailure.Expected, equalityFailure.Actual);
                            Append("\n", TestOutputStyle.Default);
                            break;

                        case MessageFailure messageFailure:
                            Append("\n    " + messageFailure.Message + "\n", TestOutputStyle.Default);
                            break;

                        default:
                            throw new NotImplementedException(
                                "RenderTestResults does not handle test failure variant: " +
                                failure.GetType().Name);
                    }
                }
            }
        }
        else if (todoCount > 0)
        {
            foreach (var todo in tests.Where(test => test.Kind is CompletedTestKind.Todo))
                Append("◦ TODO: " + todo.Path[^1] + "\n", TestOutputStyle.Default);
        }
        else
        {
            foreach (var emptyGroup in tests.Where(test => test.Kind is CompletedTestKind.FailedEmptyGroup))
            {
                Append(
                    "\n    This `describe " + emptyGroup.Path[^1] +
                    "` has no tests in it. Let's give it some!\n",
                    TestOutputStyle.Failure);
            }
        }

        if (failedCount > 0)
        {
            Append("\n\nTEST RUN FAILED", TestOutputStyle.FailureHeadline);
            Append("\n\n", TestOutputStyle.Failure);
        }
        else if (todoCount > 0)
        {
            Append("\nTEST RUN INCOMPLETE", TestOutputStyle.TodoHeadline);

            Append(
                " because there " + (todoCount is 1 ? "is " : "are ") + todoCount +
                " TODO" + (todoCount is 1 ? "" : "s") + " remaining\n\n",
                TestOutputStyle.Todo);
        }
        else
        {
            Append("\nTEST RUN PASSED", TestOutputStyle.SuccessHeadline);
            Append("\n\n", TestOutputStyle.Success);
        }

        if (duration is { } elapsed)
        {
            Append("Duration: ", TestOutputStyle.Dark);

            Append(
                Math.Round(elapsed.TotalMilliseconds).ToString(System.Globalization.CultureInfo.InvariantCulture) +
                " ms\n",
                TestOutputStyle.Default);
        }

        Append("Passed:   ", TestOutputStyle.Dark);
        Append(passedCount + "\n", TestOutputStyle.Default);
        Append("Failed:   ", TestOutputStyle.Dark);
        Append(failedCount.ToString(), TestOutputStyle.Default);

        if (todoCount > 0)
        {
            Append("\nTodo:     ", TestOutputStyle.Dark);
            Append(todoCount.ToString(), TestOutputStyle.Default);
        }

        return new StructuredTestOutput(fragments);

        void Append(string text, TestOutputStyle style) =>
            fragments.Add(new TestOutputFragment(text, style));

        void AppendEqualityValue(string value, string other)
        {
            var commonPrefixLength = 0;

            while (commonPrefixLength < value.Length &&
                commonPrefixLength < other.Length &&
                value[commonPrefixLength] == other[commonPrefixLength])
            {
                commonPrefixLength++;
            }

            var commonSuffixLength = 0;

            while (commonSuffixLength < value.Length - commonPrefixLength &&
                commonSuffixLength < other.Length - commonPrefixLength &&
                value[value.Length - commonSuffixLength - 1] ==
                other[other.Length - commonSuffixLength - 1])
            {
                commonSuffixLength++;
            }

            Append(value[..commonPrefixLength], TestOutputStyle.Default);

            Append(
                value[commonPrefixLength..(value.Length - commonSuffixLength)],
                TestOutputStyle.Highlighted);

            if (commonSuffixLength > 0)
                Append(value[^commonSuffixLength..], TestOutputStyle.Default);
        }
    }


    private static IntermediatePineVM CreatePineVm() =>
        IntermediatePineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: false,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: true,
            parseCache: null,
            precompiledLeaves: SetupVM.DefaultPrecompiledLeaves,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);


    private static void DiscoverTests(
        PineValue testValue,
        IReadOnlyList<string> path,
        List<DiscoveredTest> discoveredTests)
    {
        var (tag, arguments) = ParseTaggedValue(testValue);

        if (tag is "Describe")
        {
            if (arguments.Length is not 2)
                throw new InvalidOperationException("Describe must contain two arguments");

            var groupName = ParseElmString(arguments.Span[0]);
            var groupPath = path.Append(groupName).ToImmutableArray();

            if (arguments.Span[1] is not PineValue.ListValue children)
                throw new InvalidOperationException("Describe children must be a list");

            if (children.Items.Length is 0)
            {
                discoveredTests.Add(
                    new DiscoveredTest(
                        groupPath,
                        DiscoveredTestKind.EmptyGroup,
                        Thunk: null));

                return;
            }

            foreach (var child in children.Items.Span)
                DiscoverTests(child, groupPath, discoveredTests);

            return;
        }

        if (tag is "TestCase")
        {
            if (arguments.Length is not 2)
                throw new InvalidOperationException("TestCase must contain two arguments");

            discoveredTests.Add(
                new DiscoveredTest(
                    [.. path, ParseElmString(arguments.Span[0])],
                    DiscoveredTestKind.Runnable,
                    arguments.Span[1]));

            return;
        }

        if (tag is "TodoCase")
        {
            if (arguments.Length is not 1)
                throw new InvalidOperationException("TodoCase must contain one argument");

            discoveredTests.Add(
                new DiscoveredTest(
                    [.. path, ParseElmString(arguments.Span[0])],
                    DiscoveredTestKind.Todo,
                    Thunk: null));

            return;
        }

        throw new InvalidOperationException("Unsupported test tag: " + tag);
    }


    private static (string tag, ReadOnlyMemory<PineValue> arguments) ParseTaggedValue(PineValue value)
    {
        var tagged =
            ElmValueEncoding.ParseAsTag(value)
            .Extract(error => throw new InvalidOperationException("Failed parsing tagged Elm value: " + error));

        return (tagged.tagName, tagged.tagArguments);
    }


    private static string ParseElmString(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Map(
            elmValue =>
            elmValue is ElmValue.ElmString elmString
            ?
            elmString.Value
            :
            throw new InvalidOperationException(
                "Expected Elm string, got " + elmValue.GetType().Name))
        .Extract(error => throw new InvalidOperationException("Failed parsing Elm string: " + error));


    private enum DiscoveredTestKind
    {
        Runnable,
        Todo,
        EmptyGroup,
    }


    private sealed record DiscoveredTest(
        IReadOnlyList<string> Path,
        DiscoveredTestKind Kind,
        PineValue? Thunk);


    private const string ExpectModuleText =
        """
        module Expect exposing
            ( Expectation
            , all
            , err
            , atLeast
            , atMost
            , equal
            , equalDicts
            , equalLists
            , equalSets
            , fail
            , FloatingPointTolerance(..)
            , greaterThan
            , lessThan
            , notEqual
            , notWithin
            , ok
            , onFail
            , pass
            , within
            )

        import Dict exposing (Dict)
        import Debug
        import Set exposing (Set)


        type Expectation
            = Pass
            | ComparisonFailure String String String
            | Fail String


        type FloatingPointTolerance
            = Absolute Float
            | Relative Float
            | AbsoluteOrRelative Float Float


        pass : Expectation
        pass =
            Pass


        fail : String -> Expectation
        fail message =
            Fail message


        equal : a -> a -> Expectation
        equal expected actual =
            equateWith "Expect.equal" (==) expected actual


        notEqual : a -> a -> Expectation
        notEqual expected actual =
            equateWith "Expect.notEqual" (/=) expected actual


        lessThan : comparable -> comparable -> Expectation
        lessThan expected actual =
            compareWith "Expect.lessThan" (<) expected actual


        atMost : comparable -> comparable -> Expectation
        atMost expected actual =
            compareWith "Expect.atMost" (<=) expected actual


        greaterThan : comparable -> comparable -> Expectation
        greaterThan expected actual =
            compareWith "Expect.greaterThan" (>) expected actual


        atLeast : comparable -> comparable -> Expectation
        atLeast expected actual =
            compareWith "Expect.atLeast" (>=) expected actual


        within : FloatingPointTolerance -> Float -> Float -> Expectation
        within tolerance expected actual =
            validateTolerance tolerance "within" <|
                compareWith
                    ("Expect.within " ++ Debug.toString tolerance)
                    (withinTolerance tolerance)
                    expected
                    actual


        notWithin : FloatingPointTolerance -> Float -> Float -> Expectation
        notWithin tolerance expected actual =
            validateTolerance tolerance "notWithin" <|
                compareWith
                    ("Expect.notWithin " ++ Debug.toString tolerance)
                    (\left right -> not (withinTolerance tolerance left right))
                    expected
                    actual


        ok : Result error value -> Expectation
        ok result =
            case result of
                Ok _ ->
                    Pass

                Err _ ->
                    ComparisonFailure "Expect.ok" (Debug.toString result) "Ok _"


        err : Result error value -> Expectation
        err result =
            case result of
                Ok _ ->
                    ComparisonFailure "Expect.err" (Debug.toString result) "Err _"

                Err _ ->
                    Pass


        equalLists : List a -> List a -> Expectation
        equalLists expected actual =
            compareWith "Expect.equalLists" (==) expected actual


        equalDicts : Dict comparable a -> Dict comparable a -> Expectation
        equalDicts expected actual =
            compareWith "Expect.equalDicts" (\left right -> Dict.toList left == Dict.toList right) expected actual


        equalSets : Set comparable -> Set comparable -> Expectation
        equalSets expected actual =
            compareWith "Expect.equalSets" (\left right -> Set.toList left == Set.toList right) expected actual


        onFail : String -> Expectation -> Expectation
        onFail message expectation =
            case expectation of
                Pass ->
                    Pass

                _ ->
                    Fail message


        all : List (subject -> Expectation) -> subject -> Expectation
        all expectations subject =
            case expectations of
                [] ->
                    Fail "Expect.all was given an empty list. You must make at least one expectation to have a valid test!"

                first :: remaining ->
                    allHelp first remaining subject


        allHelp : (subject -> Expectation) -> List (subject -> Expectation) -> subject -> Expectation
        allHelp current remaining subject =
            case current subject of
                Pass ->
                    case remaining of
                        [] ->
                            Pass

                        next :: rest ->
                            allHelp next rest subject

                failure ->
                    failure


        validateTolerance : FloatingPointTolerance -> String -> Expectation -> Expectation
        validateTolerance tolerance name expectation =
            let
                absoluteTolerance =
                    case tolerance of
                        Absolute value ->
                            value

                        AbsoluteOrRelative value _ ->
                            value

                        Relative _ ->
                            0

                relativeTolerance =
                    case tolerance of
                        Relative value ->
                            value

                        AbsoluteOrRelative _ value ->
                            value

                        Absolute _ ->
                            0
            in
            if absoluteTolerance < 0 && relativeTolerance < 0 then
                Fail ("Expect." ++ name ++ " was given negative absolute and relative tolerances")

            else if absoluteTolerance < 0 then
                Fail ("Expect." ++ name ++ " was given a negative absolute tolerance")

            else if relativeTolerance < 0 then
                Fail ("Expect." ++ name ++ " was given a negative relative tolerance")

            else
                expectation


        withinTolerance : FloatingPointTolerance -> Float -> Float -> Bool
        withinTolerance tolerance left right =
            case tolerance of
                Absolute value ->
                    abs (left - right) <= value

                Relative value ->
                    abs (left - right) <= max (abs left) (abs right) * value

                AbsoluteOrRelative absoluteValue relativeValue ->
                    abs (left - right) <= absoluteValue
                        || abs (left - right) <= max (abs left) (abs right) * relativeValue


        equateWith : String -> (a -> a -> Bool) -> a -> a -> Expectation
        equateWith description comparison expected actual =
            let
                isFloat value =
                    String.toFloat value /= Nothing && String.toInt value == Nothing

                usesFloats =
                    isFloat (Debug.toString actual) || isFloat (Debug.toString expected)
            in
            if usesFloats then
                if description == "Expect.notEqual" then
                    Fail "Do not use Expect.notEqual with floats. Use Expect.notWithin instead."

                else
                    Fail "Do not use Expect.equal with floats. Use Expect.within instead."

            else
                compareWith description comparison expected actual


        compareWith : String -> (a -> a -> Bool) -> a -> a -> Expectation
        compareWith description comparison expected actual =
            if comparison actual expected then
                Pass

            else
                ComparisonFailure description (Debug.toString actual) (Debug.toString expected)
        """;


    private const string TestModuleText =
        """
        module Test exposing (Test, describe, test, todo)

        import Expect exposing (Expectation)


        type Test
            = Describe String (List Test)
            | TestCase String (() -> Expectation)
            | TodoCase String


        describe : String -> List Test -> Test
        describe name children =
            Describe name children


        test : String -> (() -> Expectation) -> Test
        test name thunk =
            TestCase name thunk


        todo : String -> Test
        todo name =
            TodoCase name
        """;
}


/// <summary>
/// Identifies the outcome of a completed Elm test.
/// </summary>
public enum CompletedTestKind
{
    /// <summary>
    /// The test passed.
    /// </summary>
    Passed,

    /// <summary>
    /// The test failed.
    /// </summary>
    Failed,

    /// <summary>
    /// The test remains to be implemented.
    /// </summary>
    Todo,

    /// <summary>
    /// The test group failed because it contained no tests.
    /// </summary>
    FailedEmptyGroup,
}


/// <summary>
/// Identifies the presentation style of a test output fragment.
/// </summary>
public enum TestOutputStyle
{
    /// <summary>
    /// Uses the default presentation.
    /// </summary>
    Default,

    /// <summary>
    /// Uses subdued presentation.
    /// </summary>
    Dark,

    /// <summary>
    /// Presents a successful result.
    /// </summary>
    Success,

    /// <summary>
    /// Presents a successful result headline.
    /// </summary>
    SuccessHeadline,

    /// <summary>
    /// Presents a failed result.
    /// </summary>
    Failure,

    /// <summary>
    /// Presents a failed result headline.
    /// </summary>
    FailureHeadline,

    /// <summary>
    /// Presents an incomplete test.
    /// </summary>
    Todo,

    /// <summary>
    /// Presents an incomplete test headline.
    /// </summary>
    TodoHeadline,

    /// <summary>
    /// Highlights the differing part of a value.
    /// </summary>
    Highlighted,
}


/// <summary>
/// Describes the reason an Elm test failed.
/// </summary>
public abstract record TestFailure;


/// <summary>
/// Describes a failed equality expectation.
/// </summary>
public sealed record EqualityFailure : TestFailure
{
    /// <summary>
    /// Creates a failed equality expectation.
    /// </summary>
    public EqualityFailure(string actual, string expected)
        : this("Expect.equal", actual, expected)
    {
    }

    /// <summary>
    /// Creates a failed comparison expectation.
    /// </summary>
    public EqualityFailure(string description, string actual, string expected)
    {
        Description = description;
        Actual = actual;
        Expected = expected;
    }

    /// <summary>
    /// Gets the expectation function description.
    /// </summary>
    public string Description { get; init; }

    /// <summary>
    /// Gets the actual value.
    /// </summary>
    public string Actual { get; init; }

    /// <summary>
    /// Gets the expected value.
    /// </summary>
    public string Expected { get; init; }

    /// <summary>
    /// Deconstructs the failed equality expectation.
    /// </summary>
    public void Deconstruct(out string actual, out string expected)
    {
        actual = Actual;
        expected = Expected;
    }
}


/// <summary>
/// Describes a failed expectation with a message.
/// </summary>
public sealed record MessageFailure : TestFailure
{
    /// <summary>
    /// Creates a failed expectation with a message.
    /// </summary>
    public MessageFailure(string message)
    {
        Message = message;
    }

    /// <summary>
    /// Gets the failure message.
    /// </summary>
    public string Message { get; init; }

    /// <summary>
    /// Deconstructs the failed expectation.
    /// </summary>
    public void Deconstruct(out string message)
    {
        message = Message;
    }
}


/// <summary>
/// Describes a completed Elm test.
/// </summary>
public sealed record CompletedTest
{
    /// <summary>
    /// Creates a completed Elm test.
    /// </summary>
    public CompletedTest(
        IReadOnlyList<string> path,
        CompletedTestKind kind,
        TestFailure? failure)
    {
        Path = path;
        Kind = kind;
        Failure = failure;
    }

    /// <summary>
    /// Gets the nested path to the test.
    /// </summary>
    public IReadOnlyList<string> Path { get; init; }

    /// <summary>
    /// Gets the test outcome.
    /// </summary>
    public CompletedTestKind Kind { get; init; }

    /// <summary>
    /// Gets the failure details when the test failed.
    /// </summary>
    public TestFailure? Failure { get; init; }

    /// <summary>
    /// Deconstructs the completed Elm test.
    /// </summary>
    public void Deconstruct(
        out IReadOnlyList<string> path,
        out CompletedTestKind kind,
        out TestFailure? failure)
    {
        path = Path;
        kind = Kind;
        failure = Failure;
    }
}


/// <summary>
/// Contains a styled fragment of rendered test output.
/// </summary>
public sealed record TestOutputFragment
{
    /// <summary>
    /// Creates a styled test output fragment.
    /// </summary>
    public TestOutputFragment(string text, TestOutputStyle style)
    {
        Text = text;
        Style = style;
    }

    /// <summary>
    /// Gets the fragment text.
    /// </summary>
    public string Text { get; init; }

    /// <summary>
    /// Gets the fragment style.
    /// </summary>
    public TestOutputStyle Style { get; init; }

    /// <summary>
    /// Deconstructs the styled test output fragment.
    /// </summary>
    public void Deconstruct(out string text, out TestOutputStyle style)
    {
        text = Text;
        style = Style;
    }
}


/// <summary>
/// Contains rendered test output.
/// </summary>
public sealed record StructuredTestOutput
{
    /// <summary>
    /// Creates structured test output.
    /// </summary>
    public StructuredTestOutput(IReadOnlyList<TestOutputFragment> fragments)
    {
        Fragments = fragments;
    }

    /// <summary>
    /// Gets the styled output fragments.
    /// </summary>
    public IReadOnlyList<TestOutputFragment> Fragments { get; init; }

    /// <summary>
    /// Gets the output without style information.
    /// </summary>
    public string PlainText =>
        string.Concat(Fragments.Select(fragment => fragment.Text));

    /// <summary>
    /// Deconstructs the structured test output.
    /// </summary>
    public void Deconstruct(out IReadOnlyList<TestOutputFragment> fragments)
    {
        fragments = Fragments;
    }
}


/// <summary>
/// Contains the results of an Elm test run.
/// </summary>
public abstract record ElmTestRun
{
    /// <summary>
    /// Contains the results of a completed Elm test run.
    /// </summary>
    public sealed record Completed(
        IReadOnlyList<CompletedTest> Tests,
        TimeSpan Duration)
        : ElmTestRun;

    /// <summary>
    /// Represents a test run for a project without Elm test modules.
    /// </summary>
    public sealed record NoTestModules(string AppDirectory) : ElmTestRun;
}
