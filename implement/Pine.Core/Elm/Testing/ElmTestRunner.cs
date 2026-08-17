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
        IPineVM? pineVm = null)
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
                        (
                        file.path,
                        moduleName: moduleHeader.ModuleName,
                        moduleNameText: string.Join('.', moduleHeader.ModuleName));
                })
            .OrderBy(testModule => testModule.moduleNameText, StringComparer.Ordinal)
            .ToImmutableArray();

        if (testModules.Length is 0)
            throw new InvalidOperationException("Did not find Elm test modules in " + appDirectory);

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

            if (expectationTag is "EqualFailure")
            {
                if (expectationArguments.Length is not 2)
                    throw new InvalidOperationException("EqualFailure must contain two arguments");

                completedTests.Add(
                    new CompletedTest(
                        discoveredTest.Path,
                        CompletedTestKind.Failed,
                        new EqualityFailure(
                            actual: ParseElmString(expectationArguments.Span[0]),
                            expected: ParseElmString(expectationArguments.Span[1]))));

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

        return new ElmTestRun(completedTests, stopwatch.Elapsed);
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
                                "\n    │ Expect.equal" +
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
            ElmInteractiveEnvironment.ParseTagged(value)
            .Extract(error => throw new InvalidOperationException("Failed parsing tagged Elm value: " + error));

        if (tagged.value is not PineValue.ListValue arguments)
            throw new InvalidOperationException("Expected tagged Elm value arguments to be a list");

        return (tagged.name, arguments.Items);
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
            , atLeast
            , atMost
            , equal
            , fail
            , greaterThan
            , lessThan
            , pass
            )

        import Debug


        type Expectation
            = Pass
            | EqualFailure String String
            | Fail String


        pass : Expectation
        pass =
            Pass


        fail : String -> Expectation
        fail message =
            Fail message


        equal : a -> a -> Expectation
        equal expected actual =
            compareWith (==) expected actual


        lessThan : comparable -> comparable -> Expectation
        lessThan expected actual =
            compareWith (<) expected actual


        atMost : comparable -> comparable -> Expectation
        atMost expected actual =
            compareWith (<=) expected actual


        greaterThan : comparable -> comparable -> Expectation
        greaterThan expected actual =
            compareWith (>) expected actual


        atLeast : comparable -> comparable -> Expectation
        atLeast expected actual =
            compareWith (>=) expected actual


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


        compareWith : (a -> a -> Bool) -> a -> a -> Expectation
        compareWith comparison expected actual =
            if comparison actual expected then
                Pass

            else
                EqualFailure (Debug.toString actual) (Debug.toString expected)
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
    {
        Actual = actual;
        Expected = expected;
    }

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
public sealed record ElmTestRun
{
    /// <summary>
    /// Creates an Elm test run result.
    /// </summary>
    public ElmTestRun(IReadOnlyList<CompletedTest> tests, TimeSpan duration)
    {
        Tests = tests;
        Duration = duration;
    }

    /// <summary>
    /// Gets the completed tests.
    /// </summary>
    public IReadOnlyList<CompletedTest> Tests { get; init; }

    /// <summary>
    /// Gets the test run duration.
    /// </summary>
    public TimeSpan Duration { get; init; }

    /// <summary>
    /// Deconstructs the Elm test run result.
    /// </summary>
    public void Deconstruct(out IReadOnlyList<CompletedTest> tests, out TimeSpan duration)
    {
        tests = Tests;
        duration = Duration;
    }
}
