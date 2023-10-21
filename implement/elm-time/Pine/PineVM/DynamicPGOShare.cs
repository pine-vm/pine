using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.PineVM;

/// <summary>
/// Combines the observations from multiple VM instances to build a profile of the most frequently used expressions.
/// 
/// For information about PGO, see https://devblogs.microsoft.com/dotnet/conversation-about-pgo/
/// </summary>
public class DynamicPGOShare : IDisposable
{
    private readonly ConcurrentQueue<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> submissionsProfiles = new();

    private readonly ConcurrentQueue<Compilation> compilations = new();

    private readonly Task dynamicCompilationTask;

    private readonly CancellationTokenSource disposedCancellationTokenSource = new();

    public int SubmissionsProfilesCount => submissionsProfiles.Count;

    public int CompilationsCount => compilations.Count;

    public IReadOnlyList<Compilation> Compilations => compilations.ToImmutableList();

    public DynamicPGOShare()
    {
        dynamicCompilationTask = Task.Run(() =>
        {
            while (!disposedCancellationTokenSource.IsCancellationRequested)
            {
                Task.Delay(TimeSpan.FromSeconds(1)).Wait();

                CompileIfNewProfiles();
            }
        });
    }

    public IPineVM GetVMAutoUpdating(
        IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>? decodeExpressionOverrides = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        return new RedirectingVM(
            (expression, environment) =>
            EvaluateExpression(expression, environment, decodeExpressionOverrides, overrideEvaluateExpression));
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment,
        IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>? decodeExpressionOverrides = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var compiledDecodeExpressionOverrides =
            compilations
            .Reverse()
            .SelectWhereNotNull(compilation => compilation.DictionaryResult.WithDefault(null))
            .FirstOrDefault();

        var combinedDecodeExpressionOverrides =
            (decodeExpressionOverrides ?? ImmutableDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>.Empty)
            .Aggregate(
                seed: compiledDecodeExpressionOverrides?.ToImmutableDictionary() ?? ImmutableDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>.Empty,
                func: (dict, entry) => dict.SetItem(entry.Key, entry.Value));

        var decodeExpressionOverridesDict =
            combinedDecodeExpressionOverrides.Count < 1
            ?
            null
            :
            combinedDecodeExpressionOverrides
            .ToImmutableDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        var overrideDecodeExpression =
            decodeExpressionOverridesDict switch
            {
                null =>
                new PineVM.OverrideDecodeExprDelegate(originalHandler => originalHandler),

                not null =>
                originalHandler => value =>
                {
                    if (decodeExpressionOverridesDict.TryGetValue(value, out var delegatingExpression))
                    {
                        return Result<string, Expression>.ok(delegatingExpression);
                    }

                    return originalHandler(value);
                }
            };

        var profilingVM = ProfilingPineVM.BuildProfilingVM(
            overrideDecodeExpression: overrideDecodeExpression,
            overrideEvaluateExpression: overrideEvaluateExpression);

        var evalResult = profilingVM.PineVM.EvaluateExpression(expression, environment);

        Task.Run(() =>
        {
            var aggregateProfilesStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var usageProfiles = ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(profilingVM.ExpressionEvaluations);

            submissionsProfiles.Enqueue(usageProfiles);
        });

        return evalResult;
    }

    private void CompileIfNewProfiles()
    {
        var inputProfiles = submissionsProfiles.ToImmutableList();

        if (compilations.LastOrDefault()?.InputProfiles.Count == inputProfiles.Count)
            return;

        var maybeNewCompilation =
            Compile(
                inputProfiles,
                limitNumber: 100,
                previousCompilation: compilations.LastOrDefault());

        foreach (var compilation in maybeNewCompilation.Map(ImmutableList.Create).WithDefault([]))
            compilations.Enqueue(compilation);
    }

    private static Maybe<Compilation> Compile(
        IReadOnlyList<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> inputProfiles,
        int limitNumber,
        Compilation? previousCompilation)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(inputProfiles);

        var expressionsToCompile =
            aggregateExpressionsProfiles
            .Where(expressionProfile => 4 < expressionProfile.Value.UsageCount)
            .OrderByDescending(expressionAndProfile => expressionAndProfile.Value.UsageCount)
            .Take(limitNumber)
            .Select(expressionAndProfile => expressionAndProfile.Key)
            .ToImmutableHashSet();

        if (expressionsToCompile.SetEquals(previousCompilation?.CompiledExpressions ?? []))
            return Maybe<Compilation>.nothing();

        var selectExpressionsDuration = totalStopwatch.Elapsed;

        var compileStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var syntaxContainerConfig =
            new CompilePineToDotNet.SyntaxContainerConfig(
                containerTypeName: "container_type",
                dictionaryMemberName: "compiled_expressions_dictionary");

        var compileToAssemblyResult =
            CompilePineToDotNet.CompileToCSharp.CompileExpressionsToCSharpClass(
                expressionsToCompile,
                syntaxContainerConfig)
            .AndThen(CompilePineToDotNet.CompileToAssembly.Compile);

        var dictionary =
            compileToAssemblyResult
            .Unpack(
                fromErr: err =>
                Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>
                .err("Failed to compile to assembly: " + err),
                fromOk:
                ok => ok.BuildCompiledExpressionsDictionary());

        return
            Maybe<Compilation>.just(
                new Compilation(
                    inputProfiles,
                    expressionsToCompile,
                    compileToAssemblyResult,
                    dictionary,
                    TotalDuration: totalStopwatch.Elapsed,
                    SelectExpressionsDuration: selectExpressionsDuration,
                    CompileDuration: compileStopwatch.Elapsed));
    }

    public void Dispose()
    {
        disposedCancellationTokenSource.Cancel();
    }

    private record RedirectingVM(PineVM.EvalExprDelegate Delegate)
        : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            Delegate(expression, environment);
    }

    public record Compilation(
        IReadOnlyList<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> InputProfiles,
        ImmutableHashSet<Expression> CompiledExpressions,
        Result<string, CompilePineToDotNet.CompileToAssemblyResult> CompileToAssemblyResult,
        Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>
        DictionaryResult,
        TimeSpan TotalDuration,
        TimeSpan SelectExpressionsDuration,
        TimeSpan CompileDuration);
}
