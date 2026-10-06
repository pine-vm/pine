using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Pine.Core.PineVM;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmFuzzTests
{
    [Fact]
    public void Fuzz_engine_runs_and_shrinks_real_generated_values()
    {
        using var project =
            new Project(
                """
                suite = Test.describe "Properties"
                    [ Test.fuzz (Fuzz.intRange 1 100) "always fails" (\n -> Expect.equal 0 n)
                    , Test.fuzz Fuzz.string "string identity" (\s -> Expect.equal s (String.reverse (String.reverse s)))
                    ]
                """);

        var result =
            ElmTestRunner.CompileAndRunTests(project.Directory, fuzzOptions: new() { Seed = 42, Runs = 10 })
            .Should().BeOfType<ElmTestRun.Completed>().Subject;

        result.Tests.Should().HaveCount(2);
        result.Tests[0].Kind.Should().Be(CompletedTestKind.Failed);
        result.Tests[0].Fuzz!.ShrunkInput.Should().Be("1");
        result.Tests[0].Fuzz!.ShrinkingCompleted.Should().BeTrue();
        result.Tests[0].Fuzz!.FailingIteration.Should().Be(1);
        result.Tests[1].Kind.Should().Be(CompletedTestKind.Passed);
        result.Tests[1].Fuzz!.RunsElapsed.Should().Be(10);

        result.ToDebugJson().Should().Contain("OriginalChoices").And.Contain("ShrunkChoices")
            .And.Contain("EffectiveSeedState").And.Contain("ResolutionFingerprint").And.Contain("EqualityFailure");
    }

    [Fact]
    public void Project_combinators_and_multiple_arguments_generate_valid_values()
    {
        using var project =
            new Project(
                """
                graph =
                    Fuzz.intRange 1 5 |> Fuzz.andThen (\size ->
                        Fuzz.listOfLengthBetween 1 size (Fuzz.oneOfValues [ 1, 2 ])
                            |> Fuzz.map (\nodes -> ( size, nodes )))
                suite = Test.describe "Supported"
                    [ Test.fuzz (Fuzz.list (Fuzz.maybe Fuzz.int)) "list maybe" (\xs -> Expect.equal xs (List.reverse (List.reverse xs)))
                    , Test.fuzz graph "dependent bounds" (\(size, nodes) ->
                        Expect.all [ \_ -> Expect.atLeast 1 (List.length nodes), \_ -> Expect.atMost size (List.length nodes) ] ())
                    , Test.fuzz2 Fuzz.int Fuzz.int "two" (\a b -> Expect.equal (a + b) (b + a))
                    , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "three" (\a b c -> Expect.equal (a + b + c) (c + b + a))
                    , Test.fuzzWith { runs = 7, distribution = Test.noDistribution } Fuzz.bool "override" (\_ -> Expect.pass)
                    , Test.fuzz (Fuzz.floatRange -10 10) "floats" (\n -> Expect.within (Expect.Absolute 0) n n)
                    , Test.fuzz Fuzz.niceFloat "finite floats" (\n -> Expect.equal False (isNaN n || isInfinite n))
                    ]
                """);

        var result = Run(project, runs: 3);
        result.Tests.Should().HaveCount(7).And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);
        result.Tests.Single(test => test.Path.Last() == "override").Fuzz!.RunsRequested.Should().Be(7);
        result.Tests.Single(test => test.Path.Last() == "override").Fuzz!.RunsElapsed.Should().Be(7);
    }

    [Fact]
    public void Seed_and_shrinking_are_independent_of_filtering_and_worker_scheduling()
    {
        using var project =
            new Project(
                """
                dependent = Fuzz.intRange 2 5 |> Fuzz.andThen (\n -> Fuzz.listOfLengthBetween n n (Fuzz.intRange 1 10))
                suite = Test.describe "Replay"
                    [ Test.fuzz Fuzz.int "before" (\_ -> Expect.pass)
                    , Test.fuzz dependent "failure" (\xs -> Expect.equal [] xs)
                    , Test.fuzz Fuzz.bool "after" (\_ -> Expect.pass)
                    ]
                """);

        var first = Run(project, runs: 2);

        var parallel =
            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                3,
                (_, _) => ElmCompilerTestHelper.PineVMForProfiling(_ => { }),
                fuzzOptions: new() { Seed = 42, Runs = 2 })
            .Should().BeOfType<ElmTestRun.Completed>().Subject;

        var filtered =
            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                filter: "failure",
                fuzzOptions: new() { Seed = 42, Runs = 2 })
            .Should().BeOfType<ElmTestRun.Completed>().Subject;

        var failure = first.Tests.Single(test => test.Path.Last() == "failure");
        failure.Fuzz!.ShrunkInput.Should().Be("[1,1]");
        failure.Fuzz.OriginalChoices.Should().NotBeEmpty();
        filtered.Tests.Should().ContainSingle().Which.Should().BeEquivalentTo(failure);
        parallel.Tests.Should().BeEquivalentTo(first.Tests, options => options.WithStrictOrdering());
    }

    [Fact]
    public void Listing_does_not_evaluate_properties_and_invalid_fuzzers_fail_explicitly()
    {
        using var project =
            new Project(
                """
                suite = Test.describe "Failures"
                    [ Test.fuzz (Fuzz.constant 1) "crash" (\_ -> Expect.pass)
                    , Test.fuzz (Fuzz.invalid "invalid generator") "invalid" (\_ -> Expect.pass)
                    , Test.fuzz (Fuzz.filter (\_ -> False) (Fuzz.constant 1)) "exhausted" (\_ -> Expect.pass)
                    ]
                """);

        var listed =
            ElmTestRunner.CompileAndRunTests(project.Directory, listTests: true)
            .Should().BeOfType<ElmTestRun.Listed>().Subject;

        listed.Tests.Should().HaveCount(3);
        var run = Run(project, runs: 1);

        run.Tests.Where(test => test.Path.Last() != "crash").Should().OnlyContain(
            test => test.Kind == CompletedTestKind.Failed);

        run.Tests.Single(test => test.Path.Last() == "invalid").Fuzz!.FailureReason.Should().Contain("InvalidFuzzer");
        run.Tests.Single(test => test.Path.Last() == "exhausted").Fuzz!.FailureReason.Should().Contain("InvalidFuzzer");

        var errored =
            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                pineVm: new ErrorVm(),
                filter: "crash",
                fuzzOptions: new() { Seed = 42, Runs = 1 }).Should().BeOfType<ElmTestRun.Completed>().Subject;

        errored.Tests.Single().Fuzz!.EvaluationError.Should().Contain("Invocation count limit");
        errored.Tests.Single().Fuzz!.ShrinkingCompleted.Should().BeFalse();
    }

    [Fact]
    public void Only_and_skip_are_incomplete_and_do_not_evaluate_excluded_properties()
    {
        using var project =
            new Project(
                """
                suite = Test.describe "Selection"
                    [ Test.only (Test.fuzz (Fuzz.constant 1) "chosen" (\_ -> Expect.pass))
                    , Test.skip (Test.test "skipped" (\_ -> Expect.fail "must not execute"))
                    , Test.test "unchosen" (\_ -> Expect.fail "must not execute")
                    ]
                """);

        var result = Run(project, runs: 1);
        result.Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
        result.IncompleteReason.Should().Contain("Test.only");
    }

    [Fact]
    public void Distribution_checks_retain_actual_counts_when_more_examples_are_needed()
    {
        using var project =
            new Project(
                """
                suite =
                    Test.fuzzWith
                        { runs = 1
                        , distribution = Test.expectDistribution [ ( Distribution.atLeast 50, "positive", \n -> n > 0 ) ]
                        }
                        (Fuzz.constant 1)
                        "distribution"
                        (\_ -> Expect.pass)
                """);

        var result = Run(project, runs: 1);
        result.Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
        var diagnostics = result.Tests.Single().Fuzz!;
        diagnostics.RunsRequested.Should().Be(1);
        diagnostics.RunsElapsed.Should().BeGreaterThan(1);
        diagnostics.DistributionReport.Should().Contain("DistributionCheckSucceeded");
    }

    private static ElmTestRun.Completed Run(Project project, uint runs) =>
        ElmTestRunner.CompileAndRunTests(project.Directory, fuzzOptions: new() { Seed = 42, Runs = runs })
        .Should().BeOfType<ElmTestRun.Completed>().Subject;

    [Fact]
    public void Float_lookup_adaptations_match_every_upstream_table_entry()
    {
        var tree =
            ElmTestRunner.DefaultResolutionConfiguration.Value.Substitutions
            .Single(package => package.PackageName == "elm-explorations/test").Sources;

        string[] floatPath = ["src", "Fuzz", "Float.elm"];
        string[] bitwisePath = ["src", "MicroBitwiseExtra.elm"];

        var floatText =
            Encoding.UTF8.GetString(((FileTree.FileNode)tree.GetNodeAtPath(floatPath)!).Bytes.Span)
            .Replace("\r\n", "\n")
            .Replace(", wellShrinkingFloat\n", ", wellShrinkingFloat, reorderExponent\n");

        var bitwiseText =
            Encoding.UTF8.GetString(((FileTree.FileNode)tree.GetNodeAtPath(bitwisePath)!).Bytes.Span)
            .Replace("\r\n", "\n")
            .Replace(", signedToUnsigned\n", ", signedToUnsigned, memoizedReverseByte\n");

        tree =
            tree.SetNodeAtPathSorted(floatPath, FileTree.File(Encoding.UTF8.GetBytes(floatText)))
            .SetNodeAtPathSorted(bitwisePath, FileTree.File(Encoding.UTF8.GetBytes(bitwiseText)));

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        var compiled =
            ElmCompiler.CompileInteractiveEnvironment(tree, [floatPath, bitwisePath], plainValueVm: vm)
            .Extract(error => throw new InvalidOperationException(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
            .Extract(error => throw new InvalidOperationException(error));

        var cache = new PineVMParseCache();
        var permutation = Function("Fuzz.Float", "reorderExponent");
        var reverse = Function("MicroBitwiseExtra", "memoizedReverseByte");

        var expected =
            Enumerable.Range(0, 2048).OrderBy(
                exponent =>
                exponent == 2047 ? int.MaxValue : exponent < 1023 ? 10000 - (exponent - 1023) : exponent - 1023).ToArray();

        for (var index = 0; index < 2048; ++index)
            Apply(permutation, index).Should().Be(expected[index]);

        for (var value = 0; value < 256; ++value)
            Apply(reverse, value).Should().Be(
                Enumerable.Range(0, 8).Aggregate(0, (bits, index) => (bits << 1) | ((value >> index) & 1)));

        Apply(permutation, -1).Should().Be(0);
        Apply(permutation, 2048).Should().Be(0);
        Apply(reverse, -1).Should().Be(0);
        Apply(reverse, 256).Should().Be(0);

        FunctionRecord Function(string module, string name) =>
            FunctionRecord.ParseFunctionRecordTagged(
                environment.Modules.Single(entry => entry.moduleName == module).moduleContent.FunctionDeclarations[name],
                cache)
            .Extract(error => throw new InvalidOperationException(error));

        long Apply(FunctionRecord function, int value) =>
            (long)IntegerEncoding.ParseSignedIntegerStrict(
                ElmInteractiveEnvironment.ApplyFunction(vm, function, [IntegerEncoding.EncodeSignedInteger(value)])
                .Extract(error => throw new InvalidOperationException(error)))
            .Extract(error => throw new InvalidOperationException(error));
    }

    private sealed class ErrorVm : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            "Invocation count limit exceeded: test budget";
    }

    internal sealed class Project : IDisposable
    {
        public string Directory { get; } =
            Path.Combine(Path.GetTempPath(), "pine-fuzz-tests-" + Guid.NewGuid().ToString("N"));

        public Project(string declarations)
        {
            System.IO.Directory.CreateDirectory(Path.Combine(Directory, "tests"));

            File.WriteAllText(
                Path.Combine(Directory, "elm.json"),
                """
                {
                    "type": "application", "source-directories": ["src"], "elm-version": "0.19.1",
                    "dependencies": { "direct": { "elm/core": "1.0.5" }, "indirect": {} },
                    "test-dependencies": {
                        "direct": { "elm-explorations/test": "2.2.1" },
                        "indirect": { "elm/random": "1.0.0", "elm/bytes": "1.0.8" }
                    }
                }
                """);

            File.WriteAllText(
                Path.Combine(Directory, "tests", "Tests.elm"),
                "module Tests exposing (suite)\nimport Test\nimport Expect\nimport Fuzz\nimport Test.Distribution as Distribution\n\n" +
                declarations +
                "\n");
        }

        public void Dispose() => System.IO.Directory.Delete(Directory, recursive: true);
    }
}
