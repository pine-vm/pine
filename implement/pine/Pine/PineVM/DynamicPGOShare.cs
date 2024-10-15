using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Pine.Core;
using Pine.Core.PineVM;

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
    public int LimitSampleCountPerSubmissionDefault { init; get; }

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
        this(
            compiledExpressionsCountLimit: 100,
            limitSampleCountPerSubmissionDefault: 100)
    {
    }

    public DynamicPGOShare(
        int compiledExpressionsCountLimit,
        int limitSampleCountPerSubmissionDefault)
    {
        CompiledExpressionsCountLimit = compiledExpressionsCountLimit;
        LimitSampleCountPerSubmissionDefault = limitSampleCountPerSubmissionDefault;

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
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null)
    {
        return new RedirectingVM(
            (expression, environment) =>
            EvaluateExpressionRestartingAfterCompilation(
                expression,
                environment,
                initialProfileAggregationDelay: TimeSpan.FromSeconds(8),
                overrideLimitSampleCountPerSubmission: null,
                evalCache: evalCache));
    }

    public record EvaluateExpressionProfilingTask(
        ProfilingPineVM ProfilingPineVM,
        CancellationTokenSource EvalTaskCancellationTokenSource,
        Task<Result<string, PineValue>> EvalTask);

    public Result<string, PineValue> EvaluateExpressionRestartingAfterCompilation(
        Expression expression,
        PineValue environment,
        TimeSpan initialProfileAggregationDelay,
        int? overrideLimitSampleCountPerSubmission,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null)
    {
        var limitSampleCount = overrideLimitSampleCountPerSubmission ?? LimitSampleCountPerSubmissionDefault;

        var profileContainer =
            new SubmissionProfileMutableContainer(
                Iterations: new ConcurrentQueue<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>>());

        submissionsProfileContainers.Enqueue(profileContainer);

        for (var iterationIndex = 0; true; ++iterationIndex)
        {
            var profileAggregationDelay = initialProfileAggregationDelay * Math.Pow(1.5, iterationIndex);

            var profilingEvalTask =
                StartEvaluateExpressionTaskBasedOnLastCompilation(
                    expression,
                    environment: environment,
                    cancellationTokenSource: new CancellationTokenSource(),
                    evalCache: evalCache);

            profilingEvalTask.EvalTask.Wait(profileAggregationDelay, disposedCancellationTokenSource.Token);

            Task.Run(() =>
            {
                var exprUsageSamples = profilingEvalTask.ProfilingPineVM.ExprEnvUsagesFlat;

                var exprUsageSamplesIncluded =
                SubsequenceWithEvenDistribution(
                    [.. exprUsageSamples],
                    limitSampleCount);

                var expressionUsages =
                exprUsageSamplesIncluded
                .Select(sample => sample.Value.Analysis.Value.ToMaybe())
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

    public static IReadOnlyList<T> SubsequenceWithEvenDistribution<T>(
        IReadOnlyList<T> source,
        int limitSampleCount) =>
        source.Count <= limitSampleCount
        ?
        [.. source]
        :
        [..source
        .Chunk(source.Count / limitSampleCount)
        .Select(chunk => chunk.First())
        .Take(limitSampleCount)];

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
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null)
    {
        var compiledParseExpressionOverrides =
            completedCompilations
            .Reverse()
            .SelectWhereNotNull(compilation => compilation.DictionaryResult.WithDefault(null))
            .FirstOrDefault();

        return
            StartEvaluateExpressionTask(
                expression,
                environment,
                CancellationTokenSource.CreateLinkedTokenSource(
                    cancellationTokenSource.Token,
                    disposedCancellationTokenSource.Token),
                evalCache,
                compiledParseExpressionOverrides);
    }

    public static EvaluateExpressionProfilingTask StartEvaluateExpressionTask(
        Expression expression,
        PineValue environment,
        CancellationTokenSource cancellationTokenSource,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>?
        compiledParseExpressionOverrides = null)
    {
        var newCancellationTokenSource = new CancellationTokenSource();

        var combinedCancellationTokenSource =
            CancellationTokenSource.CreateLinkedTokenSource(
                cancellationTokenSource.Token,
                newCancellationTokenSource.Token);

        var analysisEvalCache = new PineVMCache();

        var profilingVM =
            new ProfilingPineVM(
                evalCache: evalCache,
                analysisEvalCache: analysisEvalCache,
                overrideInvocations: compiledParseExpressionOverrides);

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
                limitCompiledExpressionsCount: CompiledExpressionsCountLimit,
                previousCompilations: [.. completedCompilations]);

        if (!completedCompilations.Contains(compilation))
            completedCompilations.Enqueue(compilation);
    }

    public static Compilation GetOrCreateCompilationForProfiles(
        IReadOnlyList<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> inputProfiles,
        int limitCompiledExpressionsCount,
        IReadOnlyList<Compilation> previousCompilations)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(inputProfiles);

        var expressionsToCompile =
            FilterAndRankExpressionProfilesForCompilation(aggregateExpressionsProfiles)
            .Take(limitCompiledExpressionsCount)
            .Select(expressionAndProfile => expressionAndProfile.Key)
            .ToImmutableHashSet();

        if (previousCompilations.FirstOrDefault(c => expressionsToCompile.SetEquals(c.CompiledExpressions)) is { } matchingPrevious)
            return matchingPrevious;

        var selectExpressionsDuration = totalStopwatch.Elapsed;

        var compileStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var syntaxContainerConfig =
            new CompilePineToDotNet.SyntaxContainerConfig(
                ContainerTypeName: "container_type",
                DictionaryMemberName: "compiled_expressions_dictionary");

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
                Result<string, IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>>
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
        .OrderByDescending(expressionAndProfile => expressionAndProfile.Value.UsageCount)
        .ThenBy(expressionAndProfile => expressionAndProfile.Key.CompiledExpressionId.ExpressionHashBase16);

    public static bool ShouldIncludeExpressionInCompilation(Expression expression) =>
        expression switch
        {
            Expression.Literal =>
            false,

            _ =>
            true
        };


    public void Dispose()
    {
        disposedCancellationTokenSource.Cancel();
    }

    private record RedirectingVM(EvalExprDelegate Delegate)
        : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            Delegate(expression, environment);
    }

    public record Compilation(
        IReadOnlyList<IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile>> InputProfiles,
        ImmutableHashSet<ExpressionUsageAnalysis> CompiledExpressions,
        Result<string, CompilePineToDotNet.CompileToAssemblyResult> CompileToAssemblyResult,
        Result<string, IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>>
        DictionaryResult,
        TimeSpan TotalDuration,
        TimeSpan SelectExpressionsDuration,
        TimeSpan CompileDuration);
}
