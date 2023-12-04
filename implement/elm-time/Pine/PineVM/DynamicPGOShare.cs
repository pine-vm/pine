using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.PineVM;

/// <summary>
/// The <see cref="DynamicPGOShare"/> combines profiling and compilation functionality and offers automatic optimization of the evaluation of Pine expressions.
/// It combines the observations from multiple VM instances to build a profile of the most frequently used expressions.
/// Observing new frequently used expressions starts asynchronous compilation of these expressions.
/// 
/// For information about PGO, see https://devblogs.microsoft.com/dotnet/conversation-about-pgo/
/// </summary>
public class DynamicPGOShare : IDisposable
{
    public int CompiledExpressionsCountLimit { init; get; }

    private record SubmissionProfileMutableContainer(
        ConcurrentQueue<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> Iterations);

    private readonly ConcurrentQueue<SubmissionProfileMutableContainer> submissionsProfileContainers = new();

    private readonly ConcurrentQueue<Compilation> completedCompilations = new();

    private readonly Task dynamicCompilationTask;

    private readonly CancellationTokenSource disposedCancellationTokenSource = new();

    IEnumerable<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> SubmissionsProfiles =>
        submissionsProfileContainers
        .SelectMany(container => container.Iterations);

    public int SubmissionsProfilesCount => SubmissionsProfiles.Count();

    public int CompilationsCount => completedCompilations.Count;

    public IReadOnlyList<Compilation> Compilations =>
        completedCompilations.ToImmutableList();

    public DynamicPGOShare()
        :
        this(compiledExpressionsCountLimit: 100)
    {
    }

    public DynamicPGOShare(
        int compiledExpressionsCountLimit)
    {
        CompiledExpressionsCountLimit = compiledExpressionsCountLimit;

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
            EvaluateExpressionRestartingAfterCompilation(
                expression,
                environment,
                initialProfileAggregationDelay: TimeSpan.FromSeconds(8),
                decodeExpressionOverrides,
                overrideEvaluateExpression));
    }

    record EvaluateExpressionProfilingTask(
        ProfilingPineVM ProfilingPineVM,
        CancellationTokenSource EvalTaskCancellationTokenSource,
        Task<Result<string, PineValue>> EvalTask);

    public Result<string, PineValue> EvaluateExpressionRestartingAfterCompilation(
        Expression expression,
        PineValue environment,
        TimeSpan initialProfileAggregationDelay,
        IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>? decodeExpressionOverrides = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var profileContainer =
            new SubmissionProfileMutableContainer(
                Iterations: new ConcurrentQueue<IReadOnlyDictionary<Expression, ExpressionUsageProfile>>());

        submissionsProfileContainers.Enqueue(profileContainer);

        for (var iterationIndex = 0; true; ++iterationIndex)
        {
            var profileAggregationDelay = initialProfileAggregationDelay * Math.Pow(1.5, iterationIndex);

            var profilingEvalTask = StartEvaluateExpressionTaskBasedOnLastCompilation(
                expression,
                environment: environment,
                cancellationTokenSource: new CancellationTokenSource(),
                decodeExpressionOverrides: decodeExpressionOverrides,
                overrideEvaluateExpression: overrideEvaluateExpression);

            profilingEvalTask.EvalTask.Wait(profileAggregationDelay, disposedCancellationTokenSource.Token);

            Task.Run(() =>
            {
                var usageProfiles = ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(
                    profilingEvalTask.ProfilingPineVM.ExpressionEvaluations);

                profileContainer.Iterations.Enqueue(usageProfiles);
            });

            if (profilingEvalTask.EvalTask.IsCompleted)
            {
                return profilingEvalTask.EvalTask.Result;
            }

            var waitForNewCompilationTask = WaitForNextCompletedCompilation(disposedCancellationTokenSource.Token);

            Task.WaitAny(
                [profilingEvalTask.EvalTask, waitForNewCompilationTask],
                profileAggregationDelay);

            if (profilingEvalTask.EvalTask.IsCompletedSuccessfully)
            {
                return profilingEvalTask.EvalTask.Result;
            }

            if (profilingEvalTask.EvalTask.Exception is { } evalTaskException)
            {
                throw new Exception("Forwarding exception from eval task", evalTaskException);
            }

            profilingEvalTask.EvalTaskCancellationTokenSource.Cancel();
        }
    }

    private Task WaitForNextCompletedCompilation(CancellationToken cancellationToken) =>
        Task.Run(() =>
        {
            var lastCompletedCompilation = completedCompilations.LastOrDefault();

            while (completedCompilations.LastOrDefault() == lastCompletedCompilation)
            {
                cancellationToken.ThrowIfCancellationRequested();

                Task.Delay(TimeSpan.FromSeconds(1), cancellationToken).Wait();
            }
        }, cancellationToken);

    private EvaluateExpressionProfilingTask StartEvaluateExpressionTaskBasedOnLastCompilation(
        Expression expression,
        PineValue environment,
        CancellationTokenSource cancellationTokenSource,
        IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>? decodeExpressionOverrides = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var compiledDecodeExpressionOverrides =
            completedCompilations
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

        var newCancellationTokenSource = new CancellationTokenSource();

        var combinedCancellationTokenSource =
            CancellationTokenSource.CreateLinkedTokenSource(
                cancellationTokenSource.Token,
                disposedCancellationTokenSource.Token,
                newCancellationTokenSource.Token);

        PineVM.EvalExprDelegate OverrideEvalExprDelegate(PineVM.EvalExprDelegate evalExprDelegate)
        {
            return (Expression expression, PineValue environment) =>
            {
                combinedCancellationTokenSource.Token.ThrowIfCancellationRequested();

                return
                (overrideEvaluateExpression?.Invoke(evalExprDelegate) ?? evalExprDelegate).Invoke(expression, environment);
            };
        }

        var profilingVM = ProfilingPineVM.BuildProfilingVM(
            overrideDecodeExpression: overrideDecodeExpression,
            overrideEvaluateExpression: OverrideEvalExprDelegate);

        var evalTask =
            Task.Run(() => profilingVM.PineVM.EvaluateExpression(expression, environment));

        return new EvaluateExpressionProfilingTask(
            profilingVM,
            newCancellationTokenSource,
            evalTask);
    }

    private void CompileIfNewProfiles()
    {
        var inputProfiles = SubmissionsProfiles.ToImmutableList();

        if (Compilations.LastOrDefault()?.InputProfiles.Count == inputProfiles.Count)
            return;

        var compilation =
            GetOrCreateCompilationForProfiles(
                inputProfiles,
                limitNumber: CompiledExpressionsCountLimit,
                previousCompilations: completedCompilations.ToImmutableArray());

        if (!completedCompilations.Contains(compilation))
            completedCompilations.Enqueue(compilation);
    }

    private static Compilation GetOrCreateCompilationForProfiles(
        IReadOnlyList<IReadOnlyDictionary<Expression, ExpressionUsageProfile>> inputProfiles,
        int limitNumber,
        IReadOnlyList<Compilation> previousCompilations)
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

        if (previousCompilations.FirstOrDefault(c => expressionsToCompile.SetEquals(c.CompiledExpressions)) is { } matchigPrevious)
            return matchigPrevious;

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
            .AndThen(compiledToCSharp => CompilePineToDotNet.CompileToAssembly.Compile(
                compiledToCSharp, Microsoft.CodeAnalysis.OptimizationLevel.Release));

        var dictionary =
            compileToAssemblyResult
            .Unpack(
                fromErr: err =>
                Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>
                .err("Failed to compile to assembly: " + err),
                fromOk:
                ok => ok.BuildCompiledExpressionsDictionary());

        return
            new Compilation(
                inputProfiles,
                expressionsToCompile,
                compileToAssemblyResult,
                dictionary,
                TotalDuration: totalStopwatch.Elapsed,
                SelectExpressionsDuration: selectExpressionsDuration,
                CompileDuration: compileStopwatch.Elapsed);
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
