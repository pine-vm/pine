using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using KernelFunctionSpecialized = Pine.Core.Internal.KernelFunctionSpecialized;

namespace Pine.Core.Interpreter.IntermediateVM;

public class PineVM : IPineVM
{
    private IDictionary<EvalCacheEntryKey, PineValue>? EvalCache { init; get; }

    private readonly EvaluationConfig? _evaluationConfigDefault;

    private readonly Action<EvaluationReport>? _reportFunctionApplication;

    private readonly ReportExecutedStackInstruction? _reportExecutedStackInstruction;

    private readonly ReportEnteredStackFrame? _reportEnteredStackFrame;

    private readonly ReportTailLoopIteration? _reportTailLoopIteration;

    private readonly ReportExpressionCompiled? _reportExpressionCompiled;

    private readonly IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? _compilationEnvClasses;

    private readonly bool _disableReductionInCompilation;

    private readonly Func<Expression, PineValueInProcess, PineVMParseCache, Func<PrecompiledResult>?>? _selectPrecompiled =
        null;

    private readonly Func<Expression, bool> _skipInlineForExpression;

    private readonly bool _enableTailRecursionOptimization;

    private readonly int _pathMaxLowExclusive;

    private readonly int _pathMaxHighInclusive;

    private readonly bool _disableGenericApplicationChainConsolidation;

    public readonly PineVMParseCache ParseCache;

    private readonly Dictionary<Expression, PineValue> _encodeExpressionCache = [];

    private readonly IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? _precompiledLeaves;

    private readonly Action<PineValue, PineValue>? _reportEnterPrecompiledLeaf;

    private readonly Action<PineValue, PineValue, PineValue?>? _reportExitPrecompiledLeaf;

    private readonly OptimizationParametersSerial? _optimizationParametersSerial = null;

    private readonly StackFrameInputHash _stackFrameInputHash = new();

    private readonly IFileStore? _cacheFileStore;

    private readonly IReadOnlyDictionary<Expression, ExpressionCompilation>? _expressionCompilationOverrides;

    public static PineVM CreateCustom(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache,
        EvaluationConfig? evaluationConfigDefault,
        Action<EvaluationReport>? reportFunctionApplication,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses,
        bool disableReductionInCompilation,
        Func<Expression, PineValueInProcess, PineVMParseCache, Func<PrecompiledResult>?>? selectPrecompiled,
        Func<Expression, bool> skipInlineForExpression,
        bool enableTailRecursionOptimization,
        PineVMParseCache? parseCache,
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? precompiledLeaves,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf,
        Action<PineValue, PineValue, PineValue?>? reportExitPrecompiledLeaf,
        OptimizationParametersSerial? optimizationParametersSerial,
        IFileStore? cacheFileStore,
        ReportExecutedStackInstruction? reportExecutedStackInstruction = null,
        ReportEnteredStackFrame? reportEnteredStackFrame = null,
        IReadOnlyDictionary<Expression, ExpressionCompilation>? expressionCompilationOverrides = null,
        int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false,
        ReportTailLoopIteration? reportTailLoopIteration = null,
        ReportExpressionCompiled? reportExpressionCompiled = null)
    {
        return
            new PineVM(
                evalCache: evalCache,
                evaluationConfigDefault: evaluationConfigDefault,
                reportFunctionApplication: reportFunctionApplication,
                compilationEnvClasses: compilationEnvClasses,
                disableReductionInCompilation: disableReductionInCompilation,
                selectPrecompiled: selectPrecompiled,
                skipInlineForExpression: skipInlineForExpression,
                enableTailRecursionOptimization: enableTailRecursionOptimization,
                parseCache: parseCache,
                precompiledLeaves: precompiledLeaves,
                reportEnterPrecompiledLeaf: reportEnterPrecompiledLeaf,
                reportExitPrecompiledLeaf: reportExitPrecompiledLeaf,
                optimizationParametersSerial: optimizationParametersSerial,
                cacheFileStore: cacheFileStore,
                reportExecutedStackInstruction: reportExecutedStackInstruction,
                reportEnteredStackFrame: reportEnteredStackFrame,
                expressionCompilationOverrides: expressionCompilationOverrides,
                pathMaxLowExclusive: pathMaxLowExclusive,
                pathMaxHighInclusive: pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                reportTailLoopIteration: reportTailLoopIteration,
                reportExpressionCompiled: reportExpressionCompiled);

    }

    private PineVM(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache,
        EvaluationConfig? evaluationConfigDefault,
        Action<EvaluationReport>? reportFunctionApplication,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses,
        bool disableReductionInCompilation,
        Func<Expression, PineValueInProcess, PineVMParseCache, Func<PrecompiledResult>?>? selectPrecompiled,
        Func<Expression, bool> skipInlineForExpression,
        bool enableTailRecursionOptimization,
        PineVMParseCache? parseCache,
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? precompiledLeaves,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf,
        Action<PineValue, PineValue, PineValue?>? reportExitPrecompiledLeaf,
        OptimizationParametersSerial? optimizationParametersSerial,
        IFileStore? cacheFileStore,
        ReportExecutedStackInstruction? reportExecutedStackInstruction,
        ReportEnteredStackFrame? reportEnteredStackFrame,
        IReadOnlyDictionary<Expression, ExpressionCompilation>? expressionCompilationOverrides,
        int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false,
        ReportTailLoopIteration? reportTailLoopIteration = null,
        ReportExpressionCompiled? reportExpressionCompiled = null)
    {
        EvalCache = evalCache;

        _evaluationConfigDefault = evaluationConfigDefault;

        _reportFunctionApplication = reportFunctionApplication;

        _reportExecutedStackInstruction = reportExecutedStackInstruction;

        _reportEnteredStackFrame = reportEnteredStackFrame;

        _reportTailLoopIteration = reportTailLoopIteration;

        _reportExpressionCompiled = reportExpressionCompiled;

        _compilationEnvClasses = compilationEnvClasses;

        _disableReductionInCompilation = disableReductionInCompilation;

        _selectPrecompiled = selectPrecompiled;

        _skipInlineForExpression = skipInlineForExpression;

        _enableTailRecursionOptimization = enableTailRecursionOptimization;

        ParseCache =
            parseCache
            ??
            new PineVMParseCache();

        _precompiledLeaves = precompiledLeaves;
        _reportEnterPrecompiledLeaf = reportEnterPrecompiledLeaf;
        _reportExitPrecompiledLeaf = reportExitPrecompiledLeaf;

        _optimizationParametersSerial = optimizationParametersSerial;

        _expressionCompilationOverrides = expressionCompilationOverrides;

        _pathMaxLowExclusive = pathMaxLowExclusive;

        _pathMaxHighInclusive = pathMaxHighInclusive;

        _disableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation;
    }

    /// <inheritdoc/>
    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment)
    {
        var evalReportResult =
            EvaluateExpressionOnCustomStack(
                expression,
                environment,
                config:
                _evaluationConfigDefault ??
                new EvaluationConfig(InvocationCountLimit: null, LoopIterationCountLimit: null, StackDepthLimit: null));

        if (evalReportResult.IsErrOrNull() is { } err)
        {
            return err.Message;
        }

        if (evalReportResult.IsOkOrNull() is not { } evalReport)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + evalReportResult.GetType().FullName);
        }

        return evalReport.ReturnValue.Evaluate();
    }

    readonly Dictionary<Expression, ExpressionEntry> _expressionCompilationDict = [];

    readonly Dictionary<Expression, Expression> _reducedExpressionDict = [];

    private record struct ExpressionEntry(
        ExpressionCompilation Compilation,
        string ExpressionHashBase16,
        OptimizationParametersSerial.ExpressionConfig? OptimizationConfig);

    readonly static ConcurrentPineValueHashCache s_mutableCacheValueHash = new();

    static StackFrame BuildStackFrame(
        PineValue? expressionValue,
        Expression expression,
        StackFrameInstructions instructions,
        StackFrameInput stackFrameInput,
        string? cacheFileName,
        StackFrameProfilingBaseline profilingBaseline)
    {
        var localsValues =
            new PineValueInProcess[instructions.LocalsCount];

        for (var i = 0; i < stackFrameInput.Arguments.Count; ++i)
        {
            localsValues[i] = stackFrameInput.Arguments[i];
        }

        return
            new StackFrame(
                expressionValue,
                expression,
                instructions,
                InputValues: stackFrameInput,
                StackValues: new PineValueInProcess[instructions.MaxStackUsage],
                LocalsValues: localsValues,
                ProfilingBaseline: profilingBaseline,
                Specialization: null,
                CacheFileName: cacheFileName);
    }

    private ExpressionEntry GetExpressionEntry(
        Expression rootExpression)
    {
        if (_expressionCompilationDict.TryGetValue(rootExpression, out var cachedCompilation))
        {
            return cachedCompilation;
        }

        var compilation = ExpressionEntryLessCache(rootExpression);

        _expressionCompilationDict[rootExpression] = compilation;

        if (_reportExpressionCompiled is { } reportExpressionCompiled)
        {
            var compiledNotification =
                new ExpressionCompiled(
                    Expression: rootExpression,
                    ExpressionHashBase16: compilation.ExpressionHashBase16,
                    Compilation: compilation.Compilation);

            reportExpressionCompiled(in compiledNotification);
        }

        return compilation;
    }

    private ExpressionEntry ExpressionEntryLessCache(Expression rootExpression)
    {
        if (_expressionCompilationOverrides?.TryGetValue(rootExpression, out var overrideCompilation) is true)
        {
            var overrideExprValue = EncodeExpressionAsValue(rootExpression);

            var (overrideExprHashBytes, _) =
                PineValueHashFlat.ComputeHashForValue(overrideExprValue);

            return
                new ExpressionEntry(
                    Compilation: overrideCompilation,
                    ExpressionHashBase16: Convert.ToHexStringLower(overrideExprHashBytes.Span),
                    OptimizationConfig: null);
        }

        IReadOnlyList<PineValueClass>? specializations = null;

        _compilationEnvClasses?.TryGetValue(rootExpression, out specializations);

        bool SkipInlining(Expression expr, PineValueClass? envConstraintId)
        {
            if (_skipInlineForExpression(expr))
            {
                return true;
            }

            if (envConstraintId is null && (_compilationEnvClasses?.ContainsKey(expr) ?? false))
            {
                return true;
            }

            return false;
        }

        var compilation =
            ExpressionCompilation.CompileExpression(
                rootExpression,
                specializations ?? [],
                parseCache: ParseCache,
                disableReduction: _disableReductionInCompilation,
                skipInlining: SkipInlining,
                enableTailRecursionOptimization: _enableTailRecursionOptimization,
                reducedExpressionCache: _reducedExpressionDict,
                pathMaxLowExclusive: _pathMaxLowExclusive,
                pathMaxHighInclusive: _pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: _disableGenericApplicationChainConsolidation);

        OptimizationParametersSerial.ExpressionConfig? optimizationConfig = null;

        var exprValue = EncodeExpressionAsValue(rootExpression);

        var (exprHashBytes, _) =
            PineValueHashFlat.ComputeHashForValue(exprValue);

        var exprHashBase16 =
            Convert.ToHexStringLower(exprHashBytes.Span);

        if (_optimizationParametersSerial is not null)
        {
            optimizationConfig =
                OptimizationParametersSerial.ConfigForExpression(
                    _optimizationParametersSerial.Expressions,
                    exprHashBytes);
        }

        return
            new ExpressionEntry(
                Compilation: compilation,
                ExpressionHashBase16: exprHashBase16,
                OptimizationConfig: optimizationConfig);
    }

    /// <summary>
    /// Configuration controlling evaluation limits in the intermediate VM.
    /// </summary>
    /// <param name="InvocationCountLimit">
    /// Maximum number of invocations (both parse-and-eval and direct stack-frame invocations) allowed
    /// before the evaluation returns an error. When <c>null</c>, no invocation limit is enforced.
    /// </param>
    /// <param name="LoopIterationCountLimit">
    /// Maximum number of loop iterations (backward jumps) allowed before the evaluation returns an error.
    /// When <c>null</c>, no loop iteration limit is enforced.
    /// </param>
    /// <param name="StackDepthLimit">
    /// Maximum number of stack frames allowed on the evaluation stack before the evaluation returns an error.
    /// When <c>null</c>, no stack depth limit is enforced.
    /// </param>
    public record EvaluationConfig(
        int? InvocationCountLimit,
        int? LoopIterationCountLimit,
        int? StackDepthLimit);

    /// <summary>
    /// Number of main-loop iterations between two successive infinite-cycle detection
    /// checks performed by <see cref="EvaluateExpressionOnCustomStack"/>.
    /// <para />
    /// The cycle detection compares the current execution state against snapshots
    /// captured at previous checks; the cost of each check is non-trivial, so the VM
    /// does not perform it on every loop/jump/invocation. Instead it amortises the
    /// cost by running the check only after this many iterations have elapsed.
    /// In addition to the periodic checks, a cycle check is also performed whenever
    /// the VM is about to return one of the configured limit errors
    /// (<see cref="EvaluationConfig.InvocationCountLimit"/>,
    /// <see cref="EvaluationConfig.LoopIterationCountLimit"/>,
    /// <see cref="EvaluationConfig.StackDepthLimit"/>) so that an infinite cycle is
    /// reported as such even when execution would otherwise hit a configured limit.
    /// </summary>
    public const int InfiniteCycleCheckIterationInterval = 40_000;

    /// <summary>
    /// Evaluates an expression using the intermediate VM stack-frame machinery.
    /// </summary>
    public Result<EvaluationError, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config)
    {
        long instructionCount = 0;
        long invocationCount = 0;
        long loopIterationCount = 0;
        long parseAndEvalCount = 0;
        long buildListCount = 0;
        long stackFrameCount = 0;
        long stackFrameReplaceCount = 0;
        long lastCacheEntryInstructionCount = 0;
        long lastCacheEntryParseAndEvalCount = 0;
        long tailLoopIterationCount = 0;

        PerformanceCounters CurrentCounters() =>
            new(
                InstructionCount: instructionCount,
                InvocationCount: invocationCount,
                BuildListCount: buildListCount,
                LoopIterationCount: loopIterationCount);

        EvaluationError BuildEvaluationError(string message) =>
            new(
                Message: message,
                StackTrace: CompileStackTrace(100),
                Counters: CurrentCounters());

        EvaluationError? IncrementInvocationCountAndEnforceLimits()
        {
            ++invocationCount;

            if (config.InvocationCountLimit is { } limit && invocationCount > limit)
            {
                if (CheckForInfiniteCycle() is { } cycleErr)
                {
                    return cycleErr;
                }

                var stackTraceHashes =
                    CompileStackTrace(100)
                    .Select(expr => s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr)))
                    .ToArray();

                return
                    BuildEvaluationError(
                        "Invocation count limit exceeded: " +
                        CommandLineInterface.FormatIntegerForDisplay(limit) +
                        "\nLast stack frames expressions:\n" +
                        string.Join(
                            "\n",
                            stackTraceHashes.Select(hash => Convert.ToHexStringLower(hash.Span)[..8])));
            }

            return null;
        }

        EvaluationError? IncrementLoopIterationCountAndEnforceLimits(StackFrame frame)
        {
            loopIterationCount++;
            frame.LoopIterationCount++;

            FireTailLoopIteration(TailLoopIterationKind.BackwardJump, frame.Expression, frame.InputValues);

            if (config.LoopIterationCountLimit is { } limit && loopIterationCount > limit)
            {
                if (CheckForInfiniteCycle() is { } cycleErr)
                {
                    return cycleErr;
                }

                var stackTraceHashes =
                    CompileStackTrace(100)
                    .Select(expr => s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr)))
                    .ToArray();

                return
                    BuildEvaluationError(
                        "Loop iteration count limit exceeded: " +
                        CommandLineInterface.FormatIntegerForDisplay(limit) +
                        "\nLast stack frames expressions:\n" +
                        string.Join(
                            "\n",
                            stackTraceHashes.Select(hash => Convert.ToHexStringLower(hash.Span)[..8])));
            }

            return null;
        }

        // -----------------------------------------------------------------
        // Infinite cycle detection state and helpers are declared further
        // below, after `var stack` is initialised, so they can capture the
        // stack reference. They are intentionally referenced here from
        // limit-enforcement helpers via forward-reference of local functions.
        // -----------------------------------------------------------------

        var rootInstructions =
            GetExpressionEntry(rootExpression)
            .Compilation
            .SelectInstructionsForEnvironment(PineValueInProcess.Create(rootEnvironment));

        var rootStackFrameInput =
            StackFrameInput.FromEnvironmentValue(
                environmentValue: rootEnvironment,
                parameters: rootInstructions.Parameters);

        var stack = new Stack<StackFrame>();

        // -----------------------------------------------------------------
        // Infinite cycle detection.
        //
        // The VM detects three shapes of infinite cycles:
        //
        //   (a) Tail-call infinite recursion. Whenever a frame is REPLACED
        //       (tail-call optimisation) we append (Expression, InputValues)
        //       to a tail-call history. A periodic pattern in this history
        //       (period 1..maxPeriod, repeated >= minCycleRepetitions times)
        //       indicates the program is iterating in place forever. The
        //       history is cleared on any non-replacement push or any pop,
        //       because either of those events ends the current tail-call
        //       chain — a non-replacement push starts a new sub-evaluation
        //       that must return, and a pop unwinds to a parent that has
        //       its own tail-call context.
        //
        //   (b) Non-tail-call infinite recursion. The live call stack is
        //       scanned from the top; if the topmost N * minCycleRepetitions
        //       frames form a periodic pattern of period N (each frame's
        //       (Expression, InputValues) matches the corresponding frame
        //       N positions deeper), the program is recursing without ever
        //       returning. This catches direct, indirect and mutual
        //       recursion shapes that grow the stack.
        //
        //   (c) In-frame infinite loop. The full execution-state snapshot
        //       (instruction pointer, evaluation stack contents, locals
        //       contents) of the topmost frame is compared against snapshots
        //       captured at recent periodic checks; an exact match while
        //       the loop-iteration counter has advanced indicates the frame
        //       is stuck in a backward-jump cycle.
        //
        // Detection is amortised: it runs only every
        // InfiniteCycleCheckIterationInterval main-loop iterations, and
        // additionally once whenever the VM is about to return one of the
        // configured limit errors so that an infinite cycle is reported as
        // such even when execution would otherwise hit a configured limit.
        //
        // The cycle-detection thresholds are deliberately conservative
        // (require many repetitions before firing) so that legitimate
        // iteration patterns are not flagged.
        // -----------------------------------------------------------------

        const int tailCallHistoryCapacity = 1024;
        const int cycleMaxPeriod = 128;
        const int minCycleRepetitions = 8;

        var tailCallHistory =
            new List<(Expression Expression, StackFrameInput Input)>(tailCallHistoryCapacity);

        long iterationsSinceCycleCheck = 0;

        // Ring buffer of recent in-frame snapshots taken at successive periodic checks.
        // Holding multiple recent snapshots is required because the periodic check
        // interval need not be a multiple of the in-frame cycle period: with a single
        // snapshot we would only ever detect cycles whose period divides the check
        // interval. With a buffer we detect any cycle whose period in VM instructions
        // is at most `inFrameSnapshotBufferCapacity * InfiniteCycleCheckIterationInterval`
        // (because the residues of `k * InfiniteCycleCheckIterationInterval` modulo
        // the cycle period eventually repeat).
        const int inFrameSnapshotBufferCapacity = 64;

        var recentInFrameSnapshots = new List<InFrameSnapshot>(inFrameSnapshotBufferCapacity);

        void RecordTailCallInHistory(Expression expression, StackFrameInput input)
        {
            if (tailCallHistory.Count >= tailCallHistoryCapacity)
            {
                // Drop the oldest quarter to make room while keeping recent context.
                tailCallHistory.RemoveRange(0, tailCallHistoryCapacity / 4);
            }

            tailCallHistory.Add((expression, input));
        }

        static InFrameSnapshot CaptureInFrameSnapshot(StackFrame frame, long instructionCountAtCapture)
        {
            var stackValues = new PineValue[frame.StackPointer];

            for (var i = 0; i < frame.StackPointer; i++)
            {
                var slot = frame.StackValues.Span[i];

                stackValues[i] = slot is null ? PineValue.EmptyList : slot.Evaluate();
            }

            var localsValues = new PineValue[frame.LocalsValues.Length];

            for (var i = 0; i < frame.LocalsValues.Length; i++)
            {
                var slot = frame.LocalsValues.Span[i];

                localsValues[i] = slot is null ? PineValue.EmptyList : slot.Evaluate();
            }

            return
                new InFrameSnapshot(
                    Frame: frame,
                    InstructionPointer: frame.InstructionPointer,
                    StackPointer: frame.StackPointer,
                    InstructionCount: instructionCountAtCapture,
                    LoopIterationCount: frame.LoopIterationCount,
                    StackValues: stackValues,
                    LocalsValues: localsValues);
        }

        static bool InFrameSnapshotsEqual(InFrameSnapshot a, InFrameSnapshot b)
        {
            if (!ReferenceEquals(a.Frame, b.Frame))
                return false;

            if (a.InstructionPointer != b.InstructionPointer)
                return false;

            if (a.StackPointer != b.StackPointer)
                return false;

            if (a.StackValues.Length != b.StackValues.Length)
                return false;

            if (a.LocalsValues.Length != b.LocalsValues.Length)
                return false;

            for (var i = 0; i < a.StackValues.Length; i++)
            {
                if (!a.StackValues[i].Equals(b.StackValues[i]))
                    return false;
            }

            for (var i = 0; i < a.LocalsValues.Length; i++)
            {
                if (!a.LocalsValues[i].Equals(b.LocalsValues[i]))
                    return false;
            }

            return true;
        }

        // Snapshot of one live stack frame's identity for cycle scanning over
        // the live call stack. Captured per-check so we can compare frames by
        // their (Expression, InputValues) tuple without retaining references
        // to the live frames themselves.
        static (Expression Expression, StackFrameInput Input)[] SnapshotStackForScan(
            Stack<StackFrame> liveStack)
        {
            var n = liveStack.Count;
            var result = new (Expression, StackFrameInput)[n];

            var i = 0;

            foreach (var frame in liveStack)
            {
                // Stepwise-specialization frames have null InputValues; treat
                // them as opaque "wall" markers by using a sentinel Input the
                // pattern matching will treat as never-matching.
                if (frame.InputValues is null)
                {
                    // Use a fresh empty StackFrameInput so reference and
                    // structural equality both fail against any normal frame.
                    result[i] =
                        (frame.Expression,
                        StackFrameInput.FromArguments(StaticFunctionInterface.ZeroParameters, []));
                }
                else
                {
                    result[i] = (frame.Expression, frame.InputValues);
                }

                i++;
            }

            return result;
        }

        void FireTailLoopIteration(
            TailLoopIterationKind kind,
            Expression frameExpression,
            StackFrameInput frameInput)
        {
            if (_reportTailLoopIteration is not { } reportTailLoopIteration)
            {
                return;
            }

            var iteration =
                new TailLoopIteration(
                    IterationIndex: tailLoopIterationCount,
                    StackFrameDepth: stack.Count,
                    Kind: kind,
                    FrameExpression: frameExpression,
                    FrameInput: frameInput);

            tailLoopIterationCount++;

            reportTailLoopIteration(in iteration);
        }

        EvaluationError? CheckForInfiniteCycle()
        {
            // Reset the per-iteration counter regardless of outcome.
            iterationsSinceCycleCheck = 0;

            // (a) Tail-call infinite recursion: pattern in the tail-call history.
            {
                var hist = tailCallHistory;
                var n = hist.Count;

                if (n >= minCycleRepetitions)
                {
                    var maxPeriod = Math.Min(cycleMaxPeriod, n / minCycleRepetitions);

                    for (var p = 1; p <= maxPeriod; p++)
                    {
                        if (!HistoryHasPeriodicCycleAtTail(hist, p, minCycleRepetitions))
                        {
                            continue;
                        }

                        var cycleEntries = new (Expression Expression, StackFrameInput Input)[p];

                        for (var i = 0; i < p; i++)
                        {
                            cycleEntries[i] = hist[n - p + i];
                        }

                        return BuildTailCallCycleError(cycleEntries);
                    }
                }
            }

            // (b) Non-tail-call infinite recursion: pattern in the live stack.
            if (stack.Count >= minCycleRepetitions)
            {
                var snapshot = SnapshotStackForScan(stack);
                var n = snapshot.Length;
                var maxPeriod = Math.Min(cycleMaxPeriod, n / minCycleRepetitions);

                for (var p = 1; p <= maxPeriod; p++)
                {
                    var ok = true;

                    // The first p entries of `snapshot` are the topmost p frames.
                    // Compare each subsequent block of p frames against the topmost block.
                    for (var rep = 1; rep < minCycleRepetitions; rep++)
                    {
                        for (var i = 0; i < p; i++)
                        {
                            var top = snapshot[i];
                            var deeper = snapshot[i + rep * p];

                            if (!ReferenceEquals(top.Expression, deeper.Expression) &&
                                !top.Expression.Equals(deeper.Expression))
                            {
                                ok = false;
                                break;
                            }

                            if (!top.Input.Equals(deeper.Input))
                            {
                                ok = false;
                                break;
                            }
                        }

                        if (!ok)
                            break;
                    }

                    if (ok)
                    {
                        var cycleEntries = new (Expression Expression, StackFrameInput Input)[p];

                        // Pattern (innermost-first) is the topmost p frames.
                        for (var i = 0; i < p; i++)
                        {
                            cycleEntries[i] = snapshot[i];
                        }

                        // Truncate the live stack so the trace looks like the
                        // cycle was detected at the second occurrence (i.e. as
                        // if the check ran on every iteration). Keep prefix +
                        // exactly two cycle iterations on the stack.
                        TruncateStackForStackCycle(p);

                        return BuildStackRecursionCycleError(cycleEntries);
                    }
                }
            }

            // (c) In-frame loop: look for a previous snapshot of the topmost
            // frame whose full execution state is equal to the current snapshot,
            // while the frame has performed at least one backward jump in
            // the meantime. We compare against multiple recent snapshots
            // because the periodic check interval need not be a multiple
            // of the in-frame cycle period. Skip when the topmost frame is
            // a stepwise-specialization frame: it has no instruction pointer
            // of its own and cannot host an in-frame loop.
            if (stack.Count > 0 && stack.Peek().Specialization is null)
            {
                var top = stack.Peek();
                var current = CaptureInFrameSnapshot(top, instructionCount);

                // Drop entries that no longer refer to the current top frame
                // (the top frame changed since they were captured).
                while (recentInFrameSnapshots.Count > 0 &&
                    !ReferenceEquals(recentInFrameSnapshots[0].Frame, top))
                {
                    recentInFrameSnapshots.RemoveAt(0);
                }

                for (var i = 0; i < recentInFrameSnapshots.Count; i++)
                {
                    var prev = recentInFrameSnapshots[i];

                    if (InFrameSnapshotsEqual(prev, current) &&
                        top.LoopIterationCount > prev.LoopIterationCount)
                    {
                        var iterationsBetween = instructionCount - prev.InstructionCount;
                        var loopIterationsBetween = top.LoopIterationCount - prev.LoopIterationCount;

                        return
                            BuildInFrameLoopCycleError(
                                topFrame: top,
                                iterationsBetween: iterationsBetween,
                                loopIterationsBetween: loopIterationsBetween);
                    }
                }

                if (recentInFrameSnapshots.Count >= inFrameSnapshotBufferCapacity)
                {
                    // Drop the oldest snapshot to make room.
                    recentInFrameSnapshots.RemoveAt(0);
                }

                recentInFrameSnapshots.Add(current);
            }
            else
            {
                recentInFrameSnapshots.Clear();
            }

            return null;
        }

        static bool HistoryHasPeriodicCycleAtTail(
            List<(Expression Expression, StackFrameInput Input)> hist,
            int p,
            int minRepetitions)
        {
            var n = hist.Count;

            if (n < p * minRepetitions)
                return false;

            for (var rep = 1; rep < minRepetitions; rep++)
            {
                for (var i = 0; i < p; i++)
                {
                    var e0 = hist[n - 1 - i];
                    var er = hist[n - 1 - i - rep * p];

                    if (!ReferenceEquals(e0.Expression, er.Expression) &&
                        !e0.Expression.Equals(er.Expression))
                    {
                        return false;
                    }

                    if (!e0.Input.Equals(er.Input))
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        void TruncateStackForStackCycle(int cyclePeriod)
        {
            // Keep prefix + exactly `2 * cyclePeriod` topmost frames matching
            // the cycle. We have already verified (in the caller) that the
            // topmost `minCycleRepetitions * cyclePeriod` frames match. Pop
            // until the live stack contains only the prefix plus 2 cycle
            // iterations of the cycle (i.e. drop the extra repetitions).
            var keepFromTop = stack.Count - (minCycleRepetitions - 2) * cyclePeriod;

            while (stack.Count > keepFromTop)
            {
                stack.Pop();
            }
        }

        EvaluationError BuildTailCallCycleError(
            (Expression Expression, StackFrameInput Input)[] cycleEntries)
        {
            return
                BuildInvocationCycleErrorCore(
                    cycleEntries,
                    kindLabel: "infinite recursion",
                    cycleNote: " (tail-call cycle)");
        }

        EvaluationError BuildStackRecursionCycleError(
            (Expression Expression, StackFrameInput Input)[] cycleEntries)
        {
            return
                BuildInvocationCycleErrorCore(
                    cycleEntries,
                    kindLabel: "infinite recursion",
                    cycleNote: " (stack-growing cycle)");
        }

        EvaluationError BuildInvocationCycleErrorCore(
            (Expression Expression, StackFrameInput Input)[] cycleEntries,
            string kindLabel,
            string cycleNote)
        {
            var stackTrace = CompileStackTrace(100);

            // Include the topmost (currently executing) frame at the head of the
            // stack trace so the trace ends exactly where the cycle entered. This
            // matters in particular for tail-call cycles, where the topmost frame
            // is repeatedly replaced rather than pushed and the conventional
            // CompileStackTrace (which skips the topmost frame) would yield an
            // empty trace.
            Expression[] traceWithTop;

            if (stack.Count > 0)
            {
                var top = stack.Peek();

                traceWithTop = new Expression[stackTrace.Count + 1];
                traceWithTop[0] = top.Expression;

                for (var i = 0; i < stackTrace.Count; i++)
                {
                    traceWithTop[i + 1] = stackTrace[i];
                }
            }
            else
            {
                traceWithTop = stackTrace.ToArray();
            }

            var cycleHashes = new string[cycleEntries.Length];

            for (var i = 0; i < cycleEntries.Length; i++)
            {
                var hash =
                    s_mutableCacheValueHash.GetHash(
                        EncodeExpressionAsValue(cycleEntries[i].Expression));

                cycleHashes[i] = Convert.ToHexStringLower(hash.Span)[..8];
            }

            var message =
                "Detected " + kindLabel + ". " +
                "Cycle length: " + cycleEntries.Length + " invocation" +
                (cycleEntries.Length is 1 ? "" : "s") + cycleNote + ".\n" +
                "Cycle expressions (one entry per invocation in the cycle):\n" +
                string.Join(
                    "\n",
                    cycleHashes.Select((h, i) => "  [" + i + "] " + h)) +
                "\nStack trace ending where the cycle entered (innermost first, " +
                traceWithTop.Length + " frame" + (traceWithTop.Length is 1 ? "" : "s") + "):\n" +
                string.Join(
                    "\n",
                    traceWithTop.Select(
                        expr =>
                        {
                            var hash = s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr));
                            return "  " + Convert.ToHexStringLower(hash.Span)[..8];
                        }));

            return
                new EvaluationError(
                    Message: message,
                    StackTrace: traceWithTop,
                    Counters: CurrentCounters());
        }

        EvaluationError BuildInFrameLoopCycleError(
            StackFrame topFrame,
            long iterationsBetween,
            long loopIterationsBetween)
        {
            var stackTrace = CompileStackTrace(100);

            // Include the looping frame itself at the head of the trace so the trace
            // ends exactly where the cycle entered.
            var traceWithTop = new Expression[stackTrace.Count + 1];
            traceWithTop[0] = topFrame.Expression;

            for (var i = 0; i < stackTrace.Count; i++)
            {
                traceWithTop[i + 1] = stackTrace[i];
            }

            var topHash =
                s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(topFrame.Expression));

            var message =
                "Detected infinite loop. " +
                "Cycle length: " + loopIterationsBetween + " loop iteration" +
                (loopIterationsBetween is 1 ? "" : "s") +
                " (" + iterationsBetween + " VM instruction" +
                (iterationsBetween is 1 ? "" : "s") + ") within a single stack frame.\n" +
                "Looping frame expression: " + Convert.ToHexStringLower(topHash.Span)[..8] +
                " (instruction pointer " + topFrame.InstructionPointer + ").\n" +
                "Stack trace ending where the cycle entered (innermost first, " +
                traceWithTop.Length + " frame" + (traceWithTop.Length is 1 ? "" : "s") + "):\n" +
                string.Join(
                    "\n",
                    traceWithTop.Select(
                        expr =>
                        {
                            var hash = s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr));
                            return "  " + Convert.ToHexStringLower(hash.Span)[..8];
                        }));

            return
                new EvaluationError(
                    Message: message,
                    StackTrace: traceWithTop,
                    Counters: CurrentCounters());
        }

        EvaluationError? InvokePrecompiledOrBuildStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValueInProcess environmentValue,
            bool replaceCurrentFrame)
        {
            var currentFrame = stack.Peek();

            if (_selectPrecompiled is { } selectPrecompiled &&
                selectPrecompiled(expression, environmentValue, ParseCache) is { } precompiledDelegate)
            {
                var precompiledResult = precompiledDelegate();

                switch (precompiledResult)
                {
                    case PrecompiledResult.FinalValue finalValue:

                        stackFrameCount += finalValue.StackFrameCount;

                        currentFrame.ReturnFromChildFrame(PineValueInProcess.Create(finalValue.Value));

                        return null;

                    case PrecompiledResult.ContinueParseAndEval continueParseAndEval:
                        {
                            var contParseResult = ParseExpression(continueParseAndEval.ExpressionValue);

                            if (contParseResult.IsErrOrNull() is { } contParseErr)
                            {
                                return
                                    BuildEvaluationError(
                                        "Failed to parse expression from value: " + contParseErr +
                                        " - expressionValue is " +
                                        (expressionValue is null
                                        ?
                                        "null"
                                        :
                                        DescribeValueForErrorMessage(expressionValue)) +
                                        " - environmentValue is " +
                                        DescribeValueForErrorMessage(environmentValue.Evaluate()));
                            }

                            if (contParseResult.IsOkOrNull() is not { } contParseOk)
                            {
                                throw new NotImplementedException(
                                    "Unexpected result type: " + contParseResult.GetType().FullName);
                            }

                            return
                                InvokePrecompiledOrBuildStackFrame(
                                    expressionValue: continueParseAndEval.ExpressionValue,
                                    expression: contParseOk,
                                    environmentValue: PineValueInProcess.Create(continueParseAndEval.EnvironmentValue),
                                    replaceCurrentFrame: replaceCurrentFrame);
                        }

                    case PrecompiledResult.StepwiseSpecialization specialization:
                        {
                            var newFrame =
                                new StackFrame(
                                    ExpressionValue: expressionValue,
                                    Expression: expression,
                                    Instructions: null,
                                    InputValues: null,
                                    StackValues: null,
                                    LocalsValues: null,
                                    ProfilingBaseline:
                                    new StackFrameProfilingBaseline(
                                        BeginInstructionCount: instructionCount,
                                        BeginInvocationCount: invocationCount,
                                        BeginParseAndEvalCount: parseAndEvalCount,
                                        BeginStackFrameCount: stackFrameCount,
                                        BeginBuildListCount: buildListCount),
                                    Specialization: specialization.Stepwise,
                                    CacheFileName: null);

                            return
                                PushStackFrame(
                                    newFrame,
                                    replaceCurrentFrame: false);
                        }

                    default:
                        throw new Exception(
                            "Unexpected return type from precompiled: " + precompiledResult.GetType().FullName);
                }
            }
            else
            {
                if (_precompiledLeaves is not null && expressionValue is not null)
                {
                    if (_precompiledLeaves.TryGetValue(expressionValue, out var computeLeafDelegate))
                    {
                        var envValue = environmentValue.Evaluate();

                        _reportEnterPrecompiledLeaf?.Invoke(expressionValue, envValue);

                        var valueComputedInLeaf = computeLeafDelegate(envValue);

                        _reportExitPrecompiledLeaf?.Invoke(expressionValue, envValue, valueComputedInLeaf);

                        if (valueComputedInLeaf is { } computedValue)
                        {
                            currentFrame.PushInstructionResult(PineValueInProcess.Create(computedValue));

                            return null;
                        }
                    }
                }

                var exprEntry = GetExpressionEntry(expression);

                var instructions =
                    exprEntry.Compilation.SelectInstructionsForEnvironment(environmentValue);

                var stackFrameInput =
                    StackFrameInput.FromEnvironmentValue(
                        environmentValue: environmentValue,
                        parameters: instructions.Parameters);

                {
                    if (expressionValue is not null && EvalCache is { } evalCache)
                    {
                        var cacheKey = new EvalCacheEntryKey(expressionValue, stackFrameInput);

                        if (evalCache.TryGetValue(cacheKey, out var fromCache))
                        {
                            currentFrame.PushInstructionResult(PineValueInProcess.Create(fromCache));

                            return null;
                        }
                    }
                }

                string? cacheFileName = null;

                if (exprEntry.OptimizationConfig is { } optimizationConfig &&
                    _cacheFileStore is { } cacheFileStore)
                {
                    if (optimizationConfig.PersistentCachePredicate?.SatisfiedBy(
                        parameters: instructions.Parameters,
                        arguments: stackFrameInput.EvaluatedArguments) ?? false)
                    {
                        var inputPersistentHashBytes =
                            _stackFrameInputHash.ComposeHashBytes(stackFrameInput).HashBytes;

                        var stackFrameInputPersistentHash =
                            Convert.ToHexStringLower(inputPersistentHashBytes.Span);

                        cacheFileName =
                            exprEntry.ExpressionHashBase16[..16] + "_" +
                            stackFrameInputPersistentHash[..16];

                        if (cacheFileStore.GetFileContent([cacheFileName]) is { } cachedContent)
                        {
                            try
                            {
                                var cachedValue =
                                    ValueEncodingFlatDeterministic.DecodeRoot(cachedContent);

                                currentFrame.PushInstructionResult(PineValueInProcess.Create(cachedValue));

                                if (expressionValue is not null && EvalCache is { } evalCache)
                                {
                                    var cacheKey = new EvalCacheEntryKey(expressionValue, stackFrameInput);

                                    evalCache.TryAdd(cacheKey, cachedValue);
                                }

                                return null;
                            }
                            catch (Exception ex)
                            {
                                throw new Exception(
                                    "Failed to decode cached value for cache file '" + cacheFileName + "'.",
                                    ex);
                            }
                        }
                    }
                }

                return
                    BuildAndPushStackFrame
                    (
                        expressionValue: expressionValue,
                        expression: expression,
                        instructions: instructions,
                        stackFrameInput: stackFrameInput,
                        cacheFileName: cacheFileName,
                        replaceCurrentFrame: replaceCurrentFrame);
            }
        }

        EvaluationError? BuildAndPushStackFrame(
            PineValue? expressionValue,
            Expression expression,
            StackFrameInstructions instructions,
            StackFrameInput stackFrameInput,
            string? cacheFileName,
            bool replaceCurrentFrame)
        {
            var newFrameProfilingBaseline =
                replaceCurrentFrame
                ?
                stack.Peek().ProfilingBaseline
                :
                new StackFrameProfilingBaseline(
                    BeginInstructionCount: instructionCount,
                    BeginInvocationCount: invocationCount,
                    BeginParseAndEvalCount: parseAndEvalCount,
                    BeginStackFrameCount: stackFrameCount,
                    BeginBuildListCount: buildListCount);

            var newFrame =
                BuildStackFrame(
                    expressionValue: expressionValue,
                    expression: expression,
                    instructions: instructions,
                    stackFrameInput: stackFrameInput,
                    cacheFileName: cacheFileName,
                    profilingBaseline: newFrameProfilingBaseline);

            return PushStackFrame(newFrame, replaceCurrentFrame: replaceCurrentFrame);
        }

        EvaluationError? PushStackFrame(
            StackFrame newFrame,
            bool replaceCurrentFrame)
        {
            // Cycle-detection bookkeeping: the tail-call replacement history
            // captures only those invocations that REPLACE the current frame
            // (tail-call optimisation). Such replacements indicate the program
            // is conceptually iterating "in place" without growing the stack;
            // a cycle in this history corresponds to a true tail-call infinite
            // recursion. Any non-replacement push starts a new sub-evaluation
            // that will eventually return, so it does not extend any existing
            // tail-call chain — we clear the history in that case.
            // Stepwise-specialization frames have null InputValues/Instructions
            // and are treated as opaque; we leave the history untouched for them
            // to avoid both NREs and clearing useful context.
            if (newFrame.Specialization is null && newFrame.InputValues is not null)
            {
                if (replaceCurrentFrame)
                {
                    RecordTailCallInHistory(newFrame.Expression, newFrame.InputValues);
                }
                else if (stack.Count > 0)
                {
                    tailCallHistory.Clear();
                }
            }

            if (replaceCurrentFrame)
            {
                stack.Pop();

                ++stackFrameReplaceCount;
            }

            stack.Push(newFrame);

            ++stackFrameCount;

            if (replaceCurrentFrame &&
                newFrame.Specialization is null &&
                newFrame.InputValues is not null)
            {
                FireTailLoopIteration(
                    TailLoopIterationKind.TailCallReplace,
                    newFrame.Expression,
                    newFrame.InputValues);
            }

            if (config.StackDepthLimit is { } stackDepthLimit && stack.Count > stackDepthLimit)
            {
                if (CheckForInfiniteCycle() is { } cycleErr)
                {
                    return cycleErr;
                }

                var stackTraceHashes =
                    CompileStackTrace(100)
                    .Select(expr => s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr)))
                    .ToArray();

                return
                    BuildEvaluationError(
                        "Stack depth limit exceeded: " +
                        CommandLineInterface.FormatIntegerForDisplay(stackDepthLimit) +
                        "\nLast stack frames expressions:\n" +
                        string.Join(
                            "\n",
                            stackTraceHashes.Select(hash => Convert.ToHexStringLower(hash.Span)[..8])));
            }

            if (_reportEnteredStackFrame is { } reportEnteredStackFrame &&
                newFrame.Instructions is { } frameInstructions)
            {
                var enteredStackFrame =
                    new EnteredStackFrame(
                        FrameIndex: stackFrameCount - 1,
                        StackFrameDepth: stack.Count,
                        Instructions: frameInstructions,
                        FrameExpression: newFrame.Expression,
                        FrameInput: newFrame.InputValues);

                reportEnteredStackFrame(in enteredStackFrame);
            }

            return null;
        }

        EvaluationReport? ReturnFromStackFrame(PineValueInProcess frameReturnValue)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.ExpressionValue is { } currentFrameExprValue)
            {
                var frameTotalInstructionCount =
                    instructionCount - currentFrame.ProfilingBaseline.BeginInstructionCount;

                var frameInvocationCount =
                    invocationCount - currentFrame.ProfilingBaseline.BeginInvocationCount;

                var frameParseAndEvalCount = parseAndEvalCount - currentFrame.ProfilingBaseline.BeginParseAndEvalCount;
                var frameStackFrameCount = stackFrameCount - currentFrame.ProfilingBaseline.BeginStackFrameCount;
                var frameBuildListCount = buildListCount - currentFrame.ProfilingBaseline.BeginBuildListCount;

                if (currentFrame.CacheFileName is { } cacheFileName && _cacheFileStore is { } cacheFileStore)
                {
                    using var stream = new MemoryStream();

                    ValueEncodingFlatDeterministic.Encode(stream, frameReturnValue.Evaluate());

                    cacheFileStore.SetFileContent(
                        path: [cacheFileName],
                        fileContent: stream.ToArray());
                }

                if (frameTotalInstructionCount + frameStackFrameCount * 100 > 700 && EvalCache is { } evalCache)
                {
                    var parseAndEvalCountSinceLastCacheEntry =
                        parseAndEvalCount - lastCacheEntryParseAndEvalCount;

                    var instructionCountSinceLastCacheEntry =
                        instructionCount - lastCacheEntryInstructionCount;

                    if (instructionCountSinceLastCacheEntry + parseAndEvalCountSinceLastCacheEntry * 100 > 700)
                    {
                        if (evalCache.TryAdd(
                            new EvalCacheEntryKey(currentFrameExprValue, currentFrame.InputValues),
                            frameReturnValue.Evaluate()))
                        {
                            lastCacheEntryInstructionCount = instructionCount;
                            lastCacheEntryParseAndEvalCount = parseAndEvalCount;
                        }
                    }
                }

                _reportFunctionApplication?.Invoke(
                    new EvaluationReport(
                        ExpressionValue: currentFrameExprValue,
                        currentFrame.Expression,
                        currentFrame.InputValues,
                        Counters: new PerformanceCounters(
                            InstructionCount: frameTotalInstructionCount,
                            InvocationCount: frameInvocationCount,
                            BuildListCount: frameBuildListCount,
                            LoopIterationCount: currentFrame.LoopIterationCount),
                        ReturnValue: frameReturnValue,
                        StackTrace: CompileStackTrace(10)));
            }

            stack.Pop();

            // Clear the tail-call history: returning from a frame ends any
            // tail-call chain that may have been accumulated. The chain only
            // makes sense within a single contiguous run of replacements at one
            // logical depth.
            tailCallHistory.Clear();

            if (stack.Count is 0)
            {
                var rootExprValue = EncodeExpressionAsValue(rootExpression);

                return
                    new EvaluationReport(
                        ExpressionValue: rootExprValue,
                        Expression: rootExpression,
                        Input: rootStackFrameInput,
                        Counters: new PerformanceCounters(
                            InstructionCount: instructionCount,
                            InvocationCount: invocationCount,
                            BuildListCount: buildListCount,
                            LoopIterationCount: loopIterationCount),
                        ReturnValue: frameReturnValue,
                        StackTrace: []);
            }

            var previousFrame = stack.Peek();

            previousFrame.ReturnFromChildFrame(frameReturnValue);

            return null;
        }

        IReadOnlyList<Expression> CompileStackTrace(int frameCountMax)
        {
            var frameCount = Math.Min(frameCountMax, stack.Count - 1);

            var stackTrace = new Expression[frameCount];

            for (var i = 0; i < frameCount; i++)
            {
                stackTrace[i] = stack.ElementAt(i + 1).Expression;
            }

            return stackTrace;
        }

        if (BuildAndPushStackFrame(
            expressionValue: null,
            rootExpression,
            rootInstructions,
            rootStackFrameInput,
            cacheFileName: null,
            replaceCurrentFrame: false) is { } rootStackDepthError)
        {
            return rootStackDepthError;
        }

        static ExecutionErrorReport BuildErrorReport(StackFrame stackFrame)
        {
            return
                new(
                    FrameExpression: stackFrame.Expression,
                    InputValues: stackFrame.InputValues,
                    Instructions: stackFrame.Instructions,
                    FrameInstructionPointer: stackFrame.InstructionPointer);
        }

        while (true)
        {
            var currentFrame = stack.Peek();

            ++instructionCount;

            ++currentFrame.InstructionCount;

            // Amortised infinite-cycle detection: run the (relatively expensive)
            // check only once every InfiniteCycleCheckIterationInterval iterations.
            ++iterationsSinceCycleCheck;

            if (iterationsSinceCycleCheck >= InfiniteCycleCheckIterationInterval)
            {
                if (CheckForInfiniteCycle() is { } cycleErr)
                {
                    return cycleErr;
                }
            }

            try
            {
                if (currentFrame.Specialization is { } specializedFrame)
                {
                    var stepResult = specializedFrame.CurrentStep;

                    if (stepResult is ApplyStepwise.StepResult.Complete complete)
                    {
                        var returnOverall =
                            ReturnFromStackFrame(complete.PineValue);

                        if (returnOverall is not null)
                        {
                            return returnOverall;
                        }

                        continue;
                    }

                    if (stepResult is ApplyStepwise.StepResult.Continue cont)
                    {
                        if (InvokePrecompiledOrBuildStackFrame(
                            expressionValue: null,
                            expression: cont.Expression,
                            environmentValue: cont.EnvironmentValue,
                            replaceCurrentFrame: false) is { } error)
                        {
                            return error;
                        }

                        continue;
                    }

                    throw new NotImplementedException(
                        "Unexpected step result type: " + stepResult.GetType().FullName);
                }


                if (currentFrame.Instructions.Instructions.Count <= currentFrame.InstructionPointer)
                {
                    return
                        BuildEvaluationError(
                            "Instruction pointer out of bounds. Missing explicit return instruction.");
                }

                var currentInstruction =
                    currentFrame.Instructions.Instructions[currentFrame.InstructionPointer]
                    ??
                    throw new InvalidOperationException("currentInstruction is null");

                if (_reportExecutedStackInstruction is { } reportExecutedStackInstruction)
                {
                    var executedStackInstruction =
                        new ExecutedStackInstruction(
                            InstructionIndex: instructionCount - 1,
                            StackFrameDepth: stack.Count,
                            InstructionPointer: currentFrame.InstructionPointer,
                            EvaluationStackDepth: currentFrame.StackPointer,
                            Instruction: currentInstruction,
                            FrameExpression: currentFrame.Expression,
                            FrameInput: currentFrame.InputValues);

                    reportExecutedStackInstruction(in executedStackInstruction);
                }

                var instructionKind = currentInstruction.Kind;

                switch (instructionKind)
                {
                    case StackInstructionKind.Push_Literal:
                        {
                            currentFrame.PushInstructionResult(
                                PineValueInProcess.Create(
                                    currentInstruction.Literal
                                    ??
                                    throw new Exception("Invalid operation form: Missing literal value")));

                            continue;
                        }

                    case StackInstructionKind.Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual =
                                PineValueInProcess.AreEqual(left, right);

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(areEqual));

                            continue;
                        }

                    case StackInstructionKind.Equal_Binary_Const:
                        {
                            var right =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left =
                                currentFrame.PopTopmostFromStack();

                            var areEqual =
                                PineValueInProcess.AreEqual(left, right);

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(areEqual));

                            continue;
                        }

                    case StackInstructionKind.Not_Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual =
                                PineValueInProcess.AreEqual(left, right);

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(!areEqual));

                            continue;
                        }

                    case StackInstructionKind.Not_Equal_Binary_Const:
                        {
                            var right =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual =
                                PineValueInProcess.AreEqual(left, right);

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(!areEqual));

                            continue;
                        }

                    case StackInstructionKind.Length:
                        {
                            var sourceValue = currentFrame.PopTopmostFromStack();

                            var length = sourceValue.GetLength();

                            currentFrame.PushInstructionResult(PineValueInProcess.CreateInteger(length));

                            continue;
                        }

                    case StackInstructionKind.Length_Equal_Const:
                        {
                            var topmostValue = currentFrame.PopTopmostFromStack();

                            var length = topmostValue.GetLength();

                            var testedLength =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception(
                                    "Invalid operation form: Missing integer literal value for length comparison");

                            var areEqual = length == testedLength;

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(areEqual));

                            continue;
                        }

                    case StackInstructionKind.Skip_Head_Const:
                        {
                            var index =
                                currentInstruction.SkipCount
                                ??
                                throw new Exception("Invalid operation form: Missing index value");

                            var indexClamped =
                                index < 0 ? 0 : index;

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var fromIndexValue =
                                prevValue.GetElementAt(index);

                            currentFrame.PushInstructionResult(fromIndexValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Head_Binary:
                        {
                            var indexValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (indexValue.AsInteger() is { } skipCount)
                            {
                                var skipCountInt = (int)skipCount;

                                var skipCountClamped =
                                    skipCountInt < 0 ? 0 : skipCountInt;

                                resultValue =
                                    prevValue.GetElementAt((int)skipCount);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Head_Generic:
                        {
                            var prevValue = currentFrame.PopTopmostFromStack();

                            var headValue = prevValue.GetElementAt(0);

                            currentFrame.PushInstructionResult(headValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Binary:
                        {
                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.EmptyList;

                            if (skipCountValue.AsInteger() is { } skipCount)
                            {
                                resultValue =
                                    PineValueInProcess.Skip((int)skipCount, prevValue);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Const:
                        {
                            var skipCount =
                                currentInstruction.SkipCount
                                ??
                                throw new Exception("Invalid operation form: Missing skip count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.Skip(skipCount, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Binary:
                        {
                            var takeCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.EmptyList;

                            if (takeCountValue.AsInteger() is { } takeCount)
                            {
                                resultValue =
                                    PineValueInProcess.Take((int)takeCount, prevValue);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.Take(takeCount, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Last_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.TakeLast(takeCount, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Build_List:
                        {
                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var items = new PineValueInProcess[itemsCount];

                            for (var i = 0; i < itemsCount; ++i)
                            {
                                items[itemsCount - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateList(items));

                            ++buildListCount;

                            continue;
                        }

                    case StackInstructionKind.Build_List_Tagged_Const:
                        {
                            var tagValue =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal tag value");

                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var items = new PineValueInProcess[itemsCount];

                            for (var i = 0; i < itemsCount; ++i)
                            {
                                items[itemsCount - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            var taggedListValue =
                                PineValueInProcess.CreateTagged(
                                    PineValueInProcess.Create(tagValue),
                                    items);

                            currentFrame.PushInstructionResult(taggedListValue);
                            ++buildListCount;
                            continue;
                        }

                    case StackInstructionKind.Concat_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.ConcatBinary(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Prepend_List_Items:
                        {
                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count for Prepend_List_Items");

                            // Pop the target list first (it's on top after the items)
                            var targetList = currentFrame.PopTopmostFromStack();

                            // Pop items in reverse order (last pushed is first popped)
                            var items = new PineValueInProcess[itemsCount];

                            for (var i = 0; i < itemsCount; ++i)
                            {
                                items[itemsCount - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            var resultValue = PineValueInProcess.EmptyList;

                            if (targetList.IsList())
                            {
                                resultValue =
                                    PineValueInProcess.ConcatBinary(
                                        PineValueInProcess.CreateList(items),
                                        targetList);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Append_List_Items:
                        {
                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count for Append_List_Items");

                            // Pop items in reverse order (last pushed is first popped)
                            var items = new PineValueInProcess[itemsCount];

                            for (var i = 0; i < itemsCount; ++i)
                            {
                                items[itemsCount - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            // Pop the target list (it was pushed first)
                            var targetList = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (targetList.IsList())
                            {
                                resultValue =
                                    PineValueInProcess.ConcatBinary(
                                        targetList,
                                        PineValueInProcess.CreateList(items));
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Concat_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var concatenated = KernelFunction.concat(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(concatenated));

                            continue;
                        }

                    case StackInstructionKind.Slice_Skip_Var_Take_Var:
                        {
                            var takeCountValue = currentFrame.PopTopmostFromStack();
                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (takeCountValue.AsInteger() is { } takeCount)
                            {
                                if (skipCountValue.AsInteger() is { } skipCount)
                                {
                                    resultValue =
                                        PineValueInProcess.Take(
                                            (int)takeCount,
                                            PineValueInProcess.Skip((int)skipCount, prevValue));
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Slice_Skip_Var_Take_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.EmptyList;

                            if (skipCountValue.AsInteger() is { } skipCount)
                            {
                                resultValue =
                                    PineValueInProcess.Take(
                                        takeCount,
                                        PineValueInProcess.Skip((int)skipCount, prevValue));
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Reverse:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var reversed = KernelFunction.reverse(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(reversed));

                            continue;
                        }

                    case StackInstructionKind.Local_Set:
                        {
                            var fromStack = currentFrame.PeekTopmostFromStack();

                            currentFrame.LocalSet(
                                currentInstruction.LocalIndex
                                ??
                                throw new Exception("Invalid operation form: Missing local index"),
                                fromStack);

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    case StackInstructionKind.Local_Get:
                        {
                            var value =
                                currentFrame.LocalGet(
                                    currentInstruction.LocalIndex
                                    ??
                                    throw new Exception("Invalid operation form: Missing local index"));

                            currentFrame.PushInstructionResult(value);

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                if (right.AsInteger() is { } rightInt)
                                {
                                    resultValue =
                                        PineValueInProcess.CreateInteger(leftInt + rightInt);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Const:
                        {
                            var rightInt =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var leftValue = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (leftValue.AsInteger() is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateInteger(leftInt + rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Add_Const:
                        {
                            var rightInt =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var leftValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(leftValue) is { } leftInt)
                            {
                                resultValue = PineValueInProcess.CreateInteger(leftInt + rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Sub_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                if (right.AsInteger() is { } rightInt)
                                {
                                    resultValue =
                                        PineValueInProcess.CreateInteger(leftInt - rightInt);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                if (right.AsInteger() is { } rightInt)
                                {
                                    resultValue =
                                        PineValueInProcess.CreateInteger(leftInt * rightInt);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateInteger(leftInt * right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                if (right.AsInteger() is { } rightInt)
                                {
                                    resultValue =
                                        PineValueInProcess.CreateBool(leftInt < rightInt);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Or_Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt &&
                                right.AsInteger() is { } rightInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt <= rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt < right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left =
                                currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt <= right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left =
                                currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt <= right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Greater_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (left.AsInteger() is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt >= right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    PineValueInProcess.CreateBool(leftInt >= right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Negate:
                        {
                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = KernelFunction.negate(value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Return:
                        {
                            var frameReturnValue =
                                currentFrame.PopTopmostFromStack();

                            var returnOverall =
                                ReturnFromStackFrame(frameReturnValue);

                            if (returnOverall is not null)
                            {
                                return returnOverall;
                            }

                            continue;
                        }

                    case StackInstructionKind.Skip_Generic:
                        {
                            var genericValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = KernelFunction.skip(genericValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Take_Generic:
                        {
                            var genericValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = KernelFunction.take(genericValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Is_Sorted_Asc_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var isSorted = KernelFunction.int_is_sorted_asc(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(isSorted));

                            continue;
                        }

                    case StackInstructionKind.Parse_And_Eval_Binary:
                        {
                            ++parseAndEvalCount;

                            if (IncrementInvocationCountAndEnforceLimits() is { } limitError)
                            {
                                return limitError;
                            }

                            var expressionValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var environmentValue = currentFrame.PopTopmostFromStack();

                            var followingInstruction =
                                currentFrame.Instructions.Instructions[currentFrame.InstructionPointer + 1];

                            var replaceCurrentFrame =
                                followingInstruction.Kind is StackInstructionKind.Return;

                            var parseResult = ParseExpression(expressionValue);

                            if (parseResult.IsErrOrNull() is { } parseErr)
                            {
                                return
                                    BuildEvaluationError(
                                        "Failed to parse expression from value: " + parseErr +
                                        " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                                        " - environmentValue is " +
                                        DescribeValueForErrorMessage(environmentValue.Evaluate()));
                            }

                            if (parseResult.IsOkOrNull() is not { } parseOk)
                            {
                                throw new NotImplementedException(
                                    "Unexpected result type: " + parseResult.GetType().FullName);
                            }

                            {
                                if (InvokePrecompiledOrBuildStackFrame(
                                    expressionValue: expressionValue,
                                    parseOk,
                                    environmentValue,
                                    replaceCurrentFrame: replaceCurrentFrame) is { } error)
                                {
                                    return error;
                                }

                                continue;
                            }
                        }

                    case StackInstructionKind.Jump_Const:
                        {
                            var jumpOffset =
                                currentInstruction.JumpOffset
                                ??
                                throw new Exception("Invalid operation form: Missing jump offset");

                            currentFrame.InstructionPointer += jumpOffset;

                            if (jumpOffset < 0)
                            {
                                if (IncrementLoopIterationCountAndEnforceLimits(currentFrame) is { } loopLimitError)
                                {
                                    return loopLimitError;
                                }
                            }

                            continue;
                        }

                    case StackInstructionKind.Invoke_StackFrame_Const:
                        {
                            if (IncrementInvocationCountAndEnforceLimits() is { } limitError)
                            {
                                return limitError;
                            }

                            var targetInstructions =
                                currentInstruction.LinkedStackFrameInstructions
                                ??
                                throw new Exception(
                                    "Invalid operation form: Missing direct stack-frame invocation target");

                            var invocationExpression =
                                currentInstruction.OptimizedInvocation?.Expression
                                ??
                                throw new Exception(
                                    "Invalid operation form: Missing direct stack-frame invocation expression");

                            var forwardedValueCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception(
                                    "Invalid operation form: Missing take count for direct stack-frame invocation");

                            var forwardedArguments = new PineValueInProcess[forwardedValueCount];

                            for (var i = 0; i < forwardedValueCount; ++i)
                            {
                                var reverseIndex = forwardedValueCount - i - 1;

                                forwardedArguments[reverseIndex] =
                                    currentFrame.PopTopmostFromStack();
                            }

                            var directInput =
                                StackFrameInput.FromArguments(
                                    targetInstructions.Parameters,
                                    forwardedArguments);

                            var replaceCurrentFrame =
                                currentFrame.InstructionPointer + 1 < currentFrame.Instructions.Instructions.Count &&
                                currentFrame.Instructions.Instructions[currentFrame.InstructionPointer + 1].Kind
                                is StackInstructionKind.Return;

                            if (BuildAndPushStackFrame(
                                expressionValue: currentInstruction.OptimizedInvocation?.ExpressionEncoded,
                                expression: invocationExpression,
                                instructions: targetInstructions,
                                stackFrameInput: directInput,
                                cacheFileName: null,
                                replaceCurrentFrame: replaceCurrentFrame) is { } stackDepthError)
                            {
                                return stackDepthError;
                            }

                            continue;
                        }

                    case StackInstructionKind.Jump_If_Equal_Const:
                        {
                            var conditionValue = currentFrame.PopTopmostFromStack();

                            var literal =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            if (PineValueInProcess.AreEqual(conditionValue, literal))
                            {
                                var jumpOffset =
                                    currentInstruction.JumpOffset
                                    ??
                                    throw new Exception("Invalid operation form: Missing jump offset");

                                currentFrame.InstructionPointer += jumpOffset;

                                if (jumpOffset < 0)
                                {
                                    if (IncrementLoopIterationCountAndEnforceLimits(currentFrame) is { } loopLimitError)
                                    {
                                        return loopLimitError;
                                    }
                                }

                                continue;
                            }

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    case StackInstructionKind.Bit_And_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_and(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_And_Const:
                        {
                            var right =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_and(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_or(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Const:
                        {
                            var right =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_or(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Xor_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_xor(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Not:
                        {
                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = KernelFunction.bit_not(value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack().Evaluate();
                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                            {
                                resultValue =
                                    KernelFunctionSpecialized.bit_shift_left(shiftCount, value);
                            }

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Const:
                        {
                            var shiftCount =
                                currentInstruction.ShiftCount
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_shift_left(shiftCount, value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Right_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = PineValueInProcess.EmptyList;

                            if (shiftValue.AsInteger() is { } shiftCount)
                            {
                                resultValue =
                                    PineValueInProcess.Create(
                                        KernelFunctionSpecialized.bit_shift_right(shiftCount, prevValue));
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Right_Const:
                        {
                            var shiftCount =
                                currentInstruction.ShiftCount
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                KernelFunctionSpecialized.bit_shift_right(shiftCount, value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var sumValue = KernelFunction.int_add(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(sumValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var productValue = KernelFunction.int_mul(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(productValue));

                            continue;
                        }

                    case StackInstructionKind.Pop:
                        {
                            currentFrame.PopTopmostFromStack();

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    case StackInstructionKind.Logical_And_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = PineKernelValues.FalseValue;

                            if (left == PineKernelValues.TrueValue && right == PineKernelValues.TrueValue)
                            {
                                resultValue = PineKernelValues.TrueValue;
                            }

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Blob_Trim_Leading_Zeros:
                        {
                            var minRemainingCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing min remaining count");

                            var blobValue = currentFrame.PopTopmostFromStack().Evaluate();

                            PineValue resultValue = PineValue.EmptyList;

                            if (blobValue is PineValue.BlobValue blob)
                            {
                                var blobBytes = blob.Bytes.Span;

                                if (minRemainingCount <= blobBytes.Length)
                                {
                                    var sliceStartIndex = 0;

                                    while (sliceStartIndex < blobBytes.Length - minRemainingCount)
                                    {
                                        if (blobBytes[sliceStartIndex] is not 0)
                                        {
                                            break;
                                        }

                                        ++sliceStartIndex;
                                    }

                                    if (sliceStartIndex is 0)
                                    {
                                        resultValue = blobValue;
                                    }
                                    else if (sliceStartIndex < blob.Bytes.Length)
                                    {
                                        var trimmedBytes =
                                            blob.Bytes[sliceStartIndex..];

                                        resultValue = PineValue.Blob(trimmedBytes);
                                    }
                                    else
                                    {
                                        resultValue = PineValue.EmptyBlob;
                                    }
                                }
                            }

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Is_Blob_Value:
                        {
                            var topmostValue = currentFrame.PopTopmostFromStack();

                            var isBlob = topmostValue.IsBlob();

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(isBlob));

                            continue;
                        }

                    case StackInstructionKind.Is_List_Value:
                        {
                            var topmostValue = currentFrame.PopTopmostFromStack();

                            var isList = topmostValue.IsList();

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(isList));

                            continue;
                        }

                    case StackInstructionKind.Starts_With_Const_At_Offset_Var:
                        {
                            var prefixValue =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing prefix value");

                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var slicedValue = currentFrame.PopTopmostFromStack();

                            var resultValue = false;

                            if (skipCountValue.AsInteger() is { } skipCount)
                            {
                                var skipCountInt =
                                    skipCount < 0 ? 0 : (int)skipCount;

                                resultValue =
                                    slicedValue.StartsWithConstAtOffsetVar(
                                        offset: skipCountInt,
                                        prefix: prefixValue);
                            }
                            else
                            {
                                resultValue =
                                    prefixValue == PineValue.EmptyList;
                            }

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateBool(resultValue));

                            continue;
                        }

                    default:
                        throw new NotImplementedException(
                            "Unexpected instruction kind: " + instructionKind);
                }
            }
            catch (Exception e)
            {
                var errorReport = BuildErrorReport(currentFrame);

                throw new InvalidIntermediateCodeException(
                    e.Message,
                    innerException: e,
                    errorReport);
            }
        }
    }

    private PineValue EncodeExpressionAsValue(Expression expression)
    {
        if (_encodeExpressionCache.TryGetValue(expression, out var cachedValue))
        {
            return (PineValue.ListValue)cachedValue;
        }

        var expressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(expression);

        _encodeExpressionCache[expression] = expressionValue;

        return expressionValue;
    }

    private Result<string, Expression> ParseExpression(PineValue expressionValue)
    {
        var fromCache =
            ParseCache.ParseExpression(expressionValue);

        if (fromCache.IsOkOrNull() is { } parseOk)
        {
            _encodeExpressionCache[parseOk] = expressionValue;
        }

        return fromCache;
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        StringEncoding.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");
}
