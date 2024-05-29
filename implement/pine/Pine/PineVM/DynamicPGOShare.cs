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
        ConcurrentQueue<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> Iterations);

    private readonly ConcurrentQueue<SubmissionProfileMutableContainer> submissionsProfileContainers = new();

    private readonly ConcurrentQueue<Compilation> completedCompilations = new();

    private readonly Task dynamicCompilationTask;

    private readonly CancellationTokenSource disposedCancellationTokenSource = new();

    IEnumerable<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> SubmissionsProfiles =>
        submissionsProfileContainers
        .SelectMany(container => container.Iterations);

    public int SubmissionsProfilesCount => SubmissionsProfiles.Count();

    public int CompilationsCount => completedCompilations.Count;

    public IReadOnlyList<Compilation> Compilations =>
        [.. completedCompilations];

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
        PineVM.OverrideParseExprDelegate? overrideParseExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        return new RedirectingVM(
            (expression, environment) =>
            EvaluateExpressionRestartingAfterCompilation(
                expression,
                environment,
                initialProfileAggregationDelay: TimeSpan.FromSeconds(8),
                overrideParseExpression,
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
        PineVM.OverrideParseExprDelegate? overrideParseExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var profileContainer =
            new SubmissionProfileMutableContainer(
                Iterations: new ConcurrentQueue<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>>());

        submissionsProfileContainers.Enqueue(profileContainer);

        for (var iterationIndex = 0; true; ++iterationIndex)
        {
            var profileAggregationDelay = initialProfileAggregationDelay * Math.Pow(1.5, iterationIndex);

            var profilingEvalTask = StartEvaluateExpressionTaskBasedOnLastCompilation(
                expression,
                environment: environment,
                cancellationTokenSource: new CancellationTokenSource(),
                overrideParseExpression: overrideParseExpression,
                overrideEvaluateExpression: overrideEvaluateExpression);

            profilingEvalTask.EvalTask.Wait(profileAggregationDelay, disposedCancellationTokenSource.Token);

            Task.Run(() =>
            {
                var exprUsageSamples = profilingEvalTask.ProfilingPineVM.DequeueAllSamples();

                var expressionUsages =
                exprUsageSamples
                .Select(sample => sample.Analysis.Value.ToMaybe())
                .WhereNotNothing()
                .SelectMany(usage => usage)
                .ToImmutableList();

                var usageProfiles = ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(expressionUsages);

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
        PineVM.OverrideParseExprDelegate? overrideParseExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var compiledParseExpressionOverrides =
            completedCompilations
            .Reverse()
            .SelectWhereNotNull(compilation => compilation.DictionaryResult.WithDefault(null))
            .FirstOrDefault();

        var parseExpressionOverridesDict =
            !(0 < compiledParseExpressionOverrides?.Count)
            ?
            null
            :
            compiledParseExpressionOverrides
            .ToImmutableDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        PineVM.OverrideParseExprDelegate overrideParseExpressionBeforeAddCompiled =
            overrideParseExpression ?? (originalHandler => originalHandler);

        var overrideParseExprIncludeCompiled =
            parseExpressionOverridesDict switch
            {
                null =>
                overrideParseExpressionBeforeAddCompiled,

                not null =>
                new PineVM.OverrideParseExprDelegate(
                originalHandler => value =>
                {
                    if (parseExpressionOverridesDict.TryGetValue(value, out var delegatingExpression))
                    {
                        return Result<string, Expression>.ok(delegatingExpression);
                    }

                    return overrideParseExpressionBeforeAddCompiled(originalHandler)(value);
                })
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

        var analysisParseCache = new PineVMCache();

        var profilingVM =
            new ProfilingPineVM(
                analysisParseCache: analysisParseCache,
                overrideParseExpression: overrideParseExprIncludeCompiled,
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
                previousCompilations: [.. completedCompilations]);

        if (!completedCompilations.Contains(compilation))
            completedCompilations.Enqueue(compilation);
    }

    private static Compilation GetOrCreateCompilationForProfiles(
        IReadOnlyList<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> inputProfiles,
        int limitNumber,
        IReadOnlyList<Compilation> previousCompilations)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(inputProfiles);

        var expressionsToCompile =
            FilterAndRankExpressionProfilesForCompilation(aggregateExpressionsProfiles)
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

    public static IEnumerable<KeyValuePair<ExpressionUsageAnalysis, ExpressionUsageProfile>> FilterAndRankExpressionProfilesForCompilation(
        IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile> aggregateExpressionsProfiles) =>
        aggregateExpressionsProfiles
        .Where(expressionProfile =>
        4 < expressionProfile.Value.UsageCount && ShouldIncludeExpressionInCompilation(expressionProfile.Key.Expression))
        .OrderByDescending(expressionAndProfile => expressionAndProfile.Value.UsageCount);

    public static bool ShouldIncludeExpressionInCompilation(Expression expression) =>
        expression switch
        {
            Expression.LiteralExpression =>
            false,

            Expression.DelegatingExpression =>
            false,

            _ =>
            true
        };


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
        IReadOnlyList<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> InputProfiles,
        ImmutableHashSet<ExpressionUsageAnalysis> CompiledExpressions,
        Result<string, CompilePineToDotNet.CompileToAssemblyResult> CompileToAssemblyResult,
        Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>
        DictionaryResult,
        TimeSpan TotalDuration,
        TimeSpan SelectExpressionsDuration,
        TimeSpan CompileDuration);
}
