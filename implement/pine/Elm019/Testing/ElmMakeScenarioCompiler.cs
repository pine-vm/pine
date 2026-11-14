using Pine.Core.Elm;
using Pine.Core.Functional;
using System;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019.Testing;

/// <summary>Testing-only adaptation; the compiler boundary returns semantic diagnostics, never snapshot text.</summary>
public static class ElmMakeScenarioCompiler
{
    public static async Task<string> CompileAsync(
        string directory,
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        CancellationToken cancellationToken = default,
        ElmMakeExecutionPath executionPath = ElmMakeExecutionPath.PineVM,
        ElmMakeCompilationCache? compilationCache = null) =>
        ElmMakeErrorSnapshots.SnapshotText(
            expectation,
            await CompileOutcomeAsync(
                directory,
                scenario,
                expectation,
                cancellationToken,
                executionPath,
                compilationCache));

    public static async Task<ElmMakeCompilation> CompileOutcomeAsync(
        string directory,
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        CancellationToken cancellationToken = default,
        ElmMakeExecutionPath executionPath = ElmMakeExecutionPath.PineVM,
        ElmMakeCompilationCache? compilationCache = null)
    {
        if (expectation.CompilerVersion != new ElmPackageVersion(0, 19, 1))
        {
            throw new NotSupportedException(
                $"ECMAScript snapshot compilation targeting Elm {expectation.CompilerVersion} is not supported. Only Elm 0.19.1 is implemented.");
        }

        var inputs =
            await new ElmMakePackageSnapshots(directory).PrepareAsync(
                scenario,
                expectation,
                cancellationToken: cancellationToken);

        var result =
            ElmMakeCompiler.Compile(
                scenario.AppFiles,
                inputs.ManifestPath,
                expectation.SourcePath.Split('/'),
                expectation.OutputType,
                expectation.Flavor,
                inputs.Packages,
                cancellationToken,
                executionPath,
                compilationCache);

        if (result.IsErrOrNull() is not { } error)
        {
            return
                new ElmMakeCompilation.Output(
                    result.Extract(failure => throw new InvalidOperationException(failure.ToString())));
        }

        return error switch
        {
            ElmMakeError.Diagnostics diagnostics =>
            new ElmMakeCompilation.CompilerErrors([.. diagnostics.Problems.Select(ElmMakeErrorSnapshots.Project)]),

            ElmMakeError.Unsupported unsupported =>
            throw new InvalidOperationException(
                "UnsupportedFeature: " + unsupported.Problem.Feature + " " +
                unsupported.Problem.Name),

            ElmMakeError.Internal failure =>
            throw new InvalidOperationException(
                "Internal compiler failure: " + failure.Problem.Kind + ": " + failure.Problem.Context),

            _ =>
            throw new NotImplementedException(
                nameof(CompileOutcomeAsync) + " does not handle error variant: " + error.GetType().Name),
        };
    }
}
