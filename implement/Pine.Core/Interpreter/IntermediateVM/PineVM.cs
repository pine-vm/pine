using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

using BuiltinFunctionSpecialized = Pine.Core.Internal.BuiltinFunctionSpecialized;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Evaluates Pine expressions by compiling them to intermediate instructions and executing them with optional specialization, caching, and precompiled leaves.
/// </summary>
public class PineVM : ICancellablePineVM
{
    private readonly IInvocationCacheAccess? _invocationCache;

    private readonly InvocationCacheConfiguration _invocationCacheConfiguration;

    private readonly EvaluationConfig? _evaluationConfigDefault;

    private readonly Action<EvaluationReport>? _reportFunctionApplication;

    private readonly ReportExecutedStackInstruction? _reportExecutedStackInstruction;

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

    /// <summary>
    /// Caches parsed expressions so repeated decode-and-evaluate steps can reuse expression objects.
    /// </summary>
    public readonly PineVMParseCache ParseCache;

    private readonly Dictionary<Expression, PineValue> _encodeExpressionCache = [];

    private readonly IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? _precompiledLeaves;

    private readonly Action<PineValue, PineValue>? _reportEnterPrecompiledLeaf;

    private readonly Action<PineValue, PineValue, PineValue?>? _reportExitPrecompiledLeaf;

    private readonly OptimizationParametersSerial? _optimizationParametersSerial = null;

    private readonly IReadOnlyDictionary<Expression, ExpressionCompilation>? _expressionCompilationOverrides;

    private readonly bool _disableDirectContinueForSimpleEval;

    private readonly bool _disableDirectEvalForSimpleTemplate;

    /// <summary>
    /// Creates a PineVM with caller-supplied caches, precompiled leaves, optimization settings, and diagnostic callbacks.
    /// </summary>
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
        IReadOnlyDictionary<Expression, ExpressionCompilation>? expressionCompilationOverrides = null,
        int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false,
        bool disableDirectContinueForSimpleEval = false,
        bool disableDirectEvalForSimpleTemplate = false,
        ReportTailLoopIteration? reportTailLoopIteration = null,
        ReportExpressionCompiled? reportExpressionCompiled = null,
        IInvocationCacheAccess? invocationCache = null,
        InvocationCacheConfiguration? invocationCacheConfiguration = null)
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
                expressionCompilationOverrides: expressionCompilationOverrides,
                pathMaxLowExclusive: pathMaxLowExclusive,
                pathMaxHighInclusive: pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                disableDirectContinueForSimpleEval: disableDirectContinueForSimpleEval,
                disableDirectEvalForSimpleTemplate: disableDirectEvalForSimpleTemplate,
                reportTailLoopIteration: reportTailLoopIteration,
                reportExpressionCompiled: reportExpressionCompiled,
                invocationCache: invocationCache,
                invocationCacheConfiguration: invocationCacheConfiguration);

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
        IReadOnlyDictionary<Expression, ExpressionCompilation>? expressionCompilationOverrides,
        int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false,
        bool disableDirectContinueForSimpleEval = false,
        bool disableDirectEvalForSimpleTemplate = false,
        ReportTailLoopIteration? reportTailLoopIteration = null,
        ReportExpressionCompiled? reportExpressionCompiled = null,
        IInvocationCacheAccess? invocationCache = null,
        InvocationCacheConfiguration? invocationCacheConfiguration = null)
    {
        if (evalCache is not null && invocationCache is not null)
        {
            throw new ArgumentException(
                "Configure either evalCache or invocationCache, not both.");
        }

        var memoryInvocationCache =
            invocationCache
            ??
            (evalCache is null
            ?
            null
            :
            new InvocationCacheAccessFromDictionary(evalCache));

        _invocationCache =
            cacheFileStore is null || optimizationParametersSerial is null
            ?
            memoryInvocationCache
            :
            new PersistentInvocationCacheAccess(
                memoryInvocationCache,
                cacheFileStore,
                optimizationParametersSerial);

        _invocationCacheConfiguration =
            invocationCacheConfiguration
            ??
            InvocationCacheConfiguration.Default;

        _evaluationConfigDefault = evaluationConfigDefault;

        _reportFunctionApplication = reportFunctionApplication;

        _reportExecutedStackInstruction = reportExecutedStackInstruction;

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

        _disableDirectContinueForSimpleEval = disableDirectContinueForSimpleEval;
        _disableDirectEvalForSimpleTemplate = disableDirectEvalForSimpleTemplate;
    }

    /// <inheritdoc/>
    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) =>
        EvaluateExpression(expression, environment, cancellationToken: default);

    /// <inheritdoc/>
    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment,
        System.Threading.CancellationToken cancellationToken)
    {
        var evalReportResult =
            EvaluateExpressionOnCustomStack(
                expression,
                environment,
                config:
                _evaluationConfigDefault ??
                EvaluationConfig.Default,
                cancellationToken);

        if (evalReportResult.IsErrOrNull() is { } err)
        {
            if (err.Reason is EvaluationErrorReason.CancellationRequested)
            {
                throw new System.OperationCanceledException(cancellationToken);
            }

            return EvaluationError.RenderDisplayString(err);
        }

        if (evalReportResult.IsOkOrNull() is not { } evalReport)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + evalReportResult.GetType().FullName);
        }

        return evalReport.ReturnValue.Evaluate();
    }

    readonly Dictionary<Expression, ExpressionEntry> _expressionCompilationDict = [];

    readonly Dictionary<(Expression, ReductionConfig), Expression> _reducedExpressionDict = [];

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
                Specialization: null);
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
        int? StackDepthLimit)
    {
        /// <summary>
        /// Bounded defaults used by <see cref="EvaluateExpression(Expression, PineValue)"/>
        /// when the VM was not constructed with a custom default configuration.
        /// </summary>
        public static EvaluationConfig Default { get; } =
            new(
                InvocationCountLimit: 10_000_000,
                LoopIterationCountLimit: 10_000_000,
                StackDepthLimit: 100_000);

        /// <summary>
        /// A configuration that disables every quota.
        /// </summary>
        public static EvaluationConfig Unbounded { get; } =
            new(
                InvocationCountLimit: null,
                LoopIterationCountLimit: null,
                StackDepthLimit: null);
    }

    /// <summary>
    /// Evaluates an expression using the intermediate VM stack-frame machinery.
    /// </summary>
    public Result<EvaluationError, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config,
        ReportEnteredStackFrame? reportEnteredStackFrame = null) =>
        EvaluateExpressionOnCustomStack(
            rootExpression,
            rootEnvironment,
            config,
            reportEnteredStackFrame,
            cancellationToken: default);

    /// <summary>
    /// Evaluates an expression with cooperative cancellation.
    /// </summary>
    public Result<EvaluationError, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config,
        System.Threading.CancellationToken cancellationToken) =>
        EvaluateExpressionOnCustomStack(
            rootExpression,
            rootEnvironment,
            config,
            reportEnteredStackFrame: null,
            cancellationToken);

    /// <summary>
    /// Evaluates an expression with per-frame reporting and cooperative cancellation.
    /// </summary>
    public Result<EvaluationError, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config,
        ReportEnteredStackFrame? reportEnteredStackFrame,
        System.Threading.CancellationToken cancellationToken)
    {
        long instructionCount = 0;
        long invocationCount = 0;
        long loopIterationCount = 0;
        long evalCount = 0;
        long buildListCount = 0;
        long stackFrameCount = 0;
        long stackFrameReplaceCount = 0;
        long lastCacheEntryInstructionCount = 0;
        long lastCacheEntryEvalCount = 0;
        long tailLoopIterationCount = 0;

        PerformanceCounters CurrentCounters() =>
            new(
                InstructionCount: instructionCount,
                InvocationCount: invocationCount,
                BuildListCount: buildListCount,
                LoopIterationCount: loopIterationCount);

        EvaluationError BuildEvaluationError(EvaluationErrorReason reason) =>
            new(
                Reason: reason,
                StackTrace: CompileEvaluationErrorStackTrace(100),
                Counters: CurrentCounters());

        EvaluationError BuildParseExpressionError(
            string parseError,
            PineValue expressionValue,
            PineValueInProcess environmentValue) =>
            BuildEvaluationError(
                new EvaluationErrorReason.ParseExpressionFailed(
                    ParseError: parseError,
                    ExpressionValue: expressionValue,
                    EnvironmentValue: environmentValue));

        EvaluationError? CheckCancellation() =>
            cancellationToken.IsCancellationRequested
            ?
            BuildEvaluationError(new EvaluationErrorReason.CancellationRequested())
            :
            null;

        EvaluationError? EnforceInvocationCountLimit()
        {
            if (config.InvocationCountLimit is { } limit && invocationCount > limit)
            {
                return
                    BuildEvaluationError(
                        new EvaluationErrorReason.QuotaExhausted(
                            EvaluationQuotaKind.InvocationCount,
                            limit));
            }

            return null;
        }

        EvaluationError? EnforceLoopIterationCountLimit()
        {
            if (config.LoopIterationCountLimit is { } limit && loopIterationCount > limit)
            {
                return
                    BuildEvaluationError(
                        new EvaluationErrorReason.QuotaExhausted(
                            EvaluationQuotaKind.LoopIterationCount,
                            limit));
            }

            return null;
        }

        EvaluationError? EnforceCountLimits() =>
            EnforceInvocationCountLimit() ?? EnforceLoopIterationCountLimit();

        EvaluationError? IncrementInvocationCountAndEnforceLimits()
        {
            if (CheckCancellation() is { } cancellationError)
            {
                return cancellationError;
            }

            ++invocationCount;

            return EnforceInvocationCountLimit();
        }

        EvaluationError? IncrementLoopIterationCountAndEnforceLimits(StackFrame frame)
        {
            loopIterationCount++;
            frame.LoopIterationCount++;

            FireTailLoopIteration(TailLoopIterationKind.BackwardJump, frame.Expression, frame.InputValues);

            return EnforceLoopIterationCountLimit();
        }

        var stack = new Stack<StackFrame>();

        if (CheckCancellation() is { } initialCancellationError)
        {
            return initialCancellationError;
        }

        var rootInstructions =
            GetExpressionEntry(rootExpression)
            .Compilation
            .SelectInstructionsForEnvironment(PineValueInProcess.Create(rootEnvironment));

        var rootStackFrameInput =
            StackFrameInput.FromEnvironmentValue(
                environmentValue: rootEnvironment,
                parameters: rootInstructions.Parameters);

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

        EvaluationError? InvokePrecompiledOrBuildStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValueInProcess environmentValue,
            bool replaceCurrentFrame)
        {
            while (true)
            {
                if (CheckCancellation() is { } cancellationError)
                {
                    return cancellationError;
                }

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

                        case PrecompiledResult.ContinueEval continueEval:
                            {
                                if (IncrementInvocationCountAndEnforceLimits() is { } limitError)
                                {
                                    return limitError;
                                }

                                var contParseResult = ParseExpression(continueEval.ExpressionValue);

                                if (contParseResult.IsErrOrNull() is { } contParseErr)
                                {
                                    return
                                        BuildParseExpressionError(
                                            contParseErr,
                                            continueEval.ExpressionValue,
                                            PineValueInProcess.Create(continueEval.EnvironmentValue));
                                }

                                if (contParseResult.IsOkOrNull() is not { } contParseOk)
                                {
                                    throw new NotImplementedException(
                                        "Unexpected result type: " + contParseResult.GetType().FullName);
                                }

                                expressionValue = continueEval.ExpressionValue;
                                expression = contParseOk;
                                environmentValue = PineValueInProcess.Create(continueEval.EnvironmentValue);

                                continue;
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
                                            BeginEvalCount: evalCount,
                                            BeginStackFrameCount: stackFrameCount,
                                            BeginBuildListCount: buildListCount),
                                        Specialization: specialization.Stepwise);

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
                    if (currentFrame.StackValues.Length > 0 &&
                        _precompiledLeaves is not null && expressionValue is not null)
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

                    if (!_expressionCompilationDict.ContainsKey(expression) &&
                        !(_expressionCompilationOverrides?.ContainsKey(expression) ?? false))
                    {
                        if (!_disableDirectContinueForSimpleEval &&
                            DirectContinuationIfSimpleEnough(expression, environmentValue) is { } directContResult)
                        {
                            var encodedExprValueMaterialized = directContResult.EncodedExprValue.Evaluate();

                            var contParseResult = ParseExpression(encodedExprValueMaterialized);

                            if (contParseResult.IsErrOrNull() is { } contParseErr)
                            {
                                return
                                    BuildParseExpressionError(
                                        contParseErr,
                                        encodedExprValueMaterialized,
                                        environmentValue);
                            }

                            if (contParseResult.IsOkOrNull() is not { } contParseOk)
                            {
                                throw new NotImplementedException(
                                    "Unexpected result type: " + contParseResult.GetType().FullName);
                            }

                            buildListCount += directContResult.PerformanceCounters.BuildListCount;
                            invocationCount += directContResult.PerformanceCounters.InvocationCount + 1;
                            loopIterationCount += directContResult.PerformanceCounters.LoopIterationCount;
                            instructionCount += directContResult.PerformanceCounters.InstructionCount;

                            if (EnforceCountLimits() is { } quotaError)
                            {
                                return quotaError;
                            }

                            expressionValue = encodedExprValueMaterialized;
                            expression = contParseOk;
                            environmentValue = directContResult.EnvironmentValue;

                            continue;
                        }

                        if (!_disableDirectEvalForSimpleTemplate &&
                            currentFrame.StackValues.Length > 0 &&
                            DirectEvalIfSimpleTemplate(expression, environmentValue) is { } directEvalResult)
                        {
                            buildListCount += directEvalResult.perfCounts.BuildListCount;
                            invocationCount += directEvalResult.perfCounts.InvocationCount + 1;
                            loopIterationCount += directEvalResult.perfCounts.LoopIterationCount;
                            instructionCount += directEvalResult.perfCounts.InstructionCount;

                            if (EnforceCountLimits() is { } quotaError)
                            {
                                return quotaError;
                            }

                            currentFrame.PushInstructionResult(directEvalResult.value);

                            return null;
                        }
                    }

                    var exprEntry = GetExpressionEntry(expression);

                    var instructions =
                        exprEntry.Compilation.SelectInstructionsForEnvironment(environmentValue);

                    var stackFrameInput =
                        StackFrameInput.FromEnvironmentValue(
                            environmentValue: environmentValue,
                            parameters: instructions.Parameters);

                    if (currentFrame.StackValues.Length > 0)
                    {
                        if (expressionValue is not null &&
                            _invocationCache is { } invocationCache &&
                            invocationCache.MayContainExpression(expressionValue))
                        {
                            var cacheKey = new EvalCacheEntryKey(expressionValue, stackFrameInput);

                            if (invocationCache.TryGet(cacheKey, out var fromCache))
                            {
                                currentFrame.PushInstructionResult(PineValueInProcess.Create(fromCache));

                                return null;
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
                            replaceCurrentFrame: replaceCurrentFrame);
                }
            }
        }

        EvaluationError? BuildAndPushStackFrame(
            PineValue? expressionValue,
            Expression expression,
            StackFrameInstructions instructions,
            StackFrameInput stackFrameInput,
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
                    BeginEvalCount: evalCount,
                    BeginStackFrameCount: stackFrameCount,
                    BeginBuildListCount: buildListCount);

            var newFrame =
                BuildStackFrame(
                    expressionValue: expressionValue,
                    expression: expression,
                    instructions: instructions,
                    stackFrameInput: stackFrameInput,
                    profilingBaseline: newFrameProfilingBaseline);

            return PushStackFrame(newFrame, replaceCurrentFrame: replaceCurrentFrame);
        }

        EvaluationError? PushStackFrame(
            StackFrame newFrame,
            bool replaceCurrentFrame)
        {
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
                return
                    BuildEvaluationError(
                        new EvaluationErrorReason.QuotaExhausted(
                            EvaluationQuotaKind.StackDepth,
                            stackDepthLimit));
            }

            if (reportEnteredStackFrame is { } reportEnteredStackFrameLocal &&
                newFrame.Instructions is { } frameInstructions)
            {
                var enteredStackFrame =
                    new EnteredStackFrame(
                        FrameIndex: stackFrameCount - 1,
                        StackFrameDepth: stack.Count,
                        Instructions: frameInstructions,
                        FrameExpression: newFrame.Expression,
                        LoadFrameInput: () => newFrame.InputValues);

                reportEnteredStackFrameLocal(in enteredStackFrame);
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

                var frameEvalCount = evalCount - currentFrame.ProfilingBaseline.BeginEvalCount;
                var frameStackFrameCount = stackFrameCount - currentFrame.ProfilingBaseline.BeginStackFrameCount;
                var frameBuildListCount = buildListCount - currentFrame.ProfilingBaseline.BeginBuildListCount;

                var evalCountSinceLastCacheEntry =
                    evalCount - lastCacheEntryEvalCount;

                var instructionCountSinceLastCacheEntry =
                    instructionCount - lastCacheEntryInstructionCount;

                if (_invocationCache is { } invocationCache &&
                    _invocationCacheConfiguration.ShouldOfferEntry(
                        frameInstructionCount: frameTotalInstructionCount,
                        frameStackFrameCount: frameStackFrameCount,
                        instructionCountSinceLastEntry: instructionCountSinceLastCacheEntry,
                        evalCountSinceLastEntry: evalCountSinceLastCacheEntry))
                {
                    if (invocationCache.TryAdd(
                        new EvalCacheEntryKey(currentFrameExprValue, currentFrame.InputValues),
                        frameReturnValue.Evaluate()))
                    {
                        lastCacheEntryInstructionCount = instructionCount;
                        lastCacheEntryEvalCount = evalCount;
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

        IReadOnlyList<EvaluationStackTraceFrame> CompileEvaluationErrorStackTrace(int frameCountMax)
        {
            var frameCount = Math.Min(frameCountMax, stack.Count);
            var stackTrace = new EvaluationStackTraceFrame[frameCount];

            for (var i = 0; i < frameCount; i++)
            {
                var frame = stack.ElementAt(i);

                stackTrace[i] =
                    new EvaluationStackTraceFrame(
                        Expression: frame.Expression,
                        Input: frame.InputValues,
                        Instructions: frame.Instructions,
                        InstructionPointer: frame.InstructionPointer);
            }

            return stackTrace;
        }

        if (BuildAndPushStackFrame(
            expressionValue: null,
            rootExpression,
            rootInstructions,
            rootStackFrameInput,
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
                        if (IncrementInvocationCountAndEnforceLimits() is { } limitError)
                        {
                            return limitError;
                        }

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
                            new EvaluationErrorReason.InstructionPointerOutOfBounds());
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
                            LoadFrameInput: () => currentFrame.InputValues);

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

                    case StackInstructionKind.Build_List_With_Prefix:
                        {
                            var prefixValue =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal prefix value");

                            if (prefixValue is not PineValue.ListValue prefixList)
                            {
                                throw new Exception("Invalid operation form: Literal prefix value is not a list");
                            }

                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var prefixItems = prefixList.Items.Span;
                            var items = new PineValueInProcess[prefixItems.Length + itemsCount];

                            for (var i = 0; i < prefixItems.Length; ++i)
                            {
                                items[i] = PineValueInProcess.Create(prefixItems[i]);
                            }

                            for (var i = 0; i < itemsCount; ++i)
                            {
                                items[items.Length - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            currentFrame.PushInstructionResult(
                                PineValueInProcess.CreateList(items));

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

                            var concatenated = BuiltinFunction.concat(listValue);

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

                            var reversed = BuiltinFunction.reverse(listValue);

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

                            if (BuiltinFunction.UnsignedIntegerFromValueRelaxed(leftValue) is { } leftInt)
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

                            if (BuiltinFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
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

                            if (BuiltinFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
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

                            var resultValue = BuiltinFunction.negate(value);

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

                            var resultValue = BuiltinFunction.skip(genericValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Take_Generic:
                        {
                            var genericValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = BuiltinFunction.take(genericValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Is_Sorted_Asc_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var isSorted = BuiltinFunction.int_is_sorted_asc(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(isSorted));

                            continue;
                        }

                    case StackInstructionKind.Eval_Binary:
                        {
                            ++evalCount;

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
                                    BuildParseExpressionError(
                                        parseErr,
                                        expressionValue,
                                        environmentValue);
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
                            if (CheckCancellation() is { } cancellationError)
                            {
                                return cancellationError;
                            }

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
                                replaceCurrentFrame: replaceCurrentFrame) is { } stackDepthError)
                            {
                                return stackDepthError;
                            }

                            continue;
                        }

                    case StackInstructionKind.Jump_If_Equal_Const:
                        {
                            if (CheckCancellation() is { } cancellationError)
                            {
                                return cancellationError;
                            }

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

                    case StackInstructionKind.Switch_Jump_If_Equal_Const:
                        {
                            if (CheckCancellation() is { } cancellationError)
                            {
                                return cancellationError;
                            }

                            var conditionValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var jumpTable =
                                currentInstruction.SwitchJumpTable
                                ??
                                throw new Exception("Invalid operation form: Missing switch jump table");

                            if (jumpTable.TryGetValue(conditionValue, out var jumpOffset))
                            {
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

                    case StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const:
                        {
                            if (CheckCancellation() is { } cancellationError)
                            {
                                return cancellationError;
                            }

                            var skipCountValue = currentFrame.PopTopmostFromStack();
                            var slicedValue = currentFrame.PopTopmostFromStack();

                            var jumpTable =
                                currentInstruction.SwitchJumpTable
                                ??
                                throw new Exception("Invalid operation form: Missing switch jump table");

                            int? jumpOffset = null;

                            var skipCount = skipCountValue.AsInteger();

                            foreach (var switchCase in jumpTable)
                            {
                                var matches =
                                    skipCount is { } skipCountInteger
                                    ?
                                    slicedValue.SliceSkipVarEqualConst(
                                        skipCount: skipCountInteger < 0 ? 0 : (int)skipCountInteger,
                                        literal: switchCase.Key)
                                    :
                                    switchCase.Key == PineValue.EmptyList;

                                if (matches)
                                {
                                    jumpOffset = switchCase.Value;
                                    break;
                                }
                            }

                            if (jumpOffset is { } matchedJumpOffset)
                            {
                                currentFrame.InstructionPointer += matchedJumpOffset;

                                if (matchedJumpOffset < 0)
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
                                BuiltinFunctionSpecialized.bit_and(left, right);

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
                                BuiltinFunctionSpecialized.bit_and(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                BuiltinFunctionSpecialized.bit_or(left, right);

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
                                BuiltinFunctionSpecialized.bit_or(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Xor_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack().Evaluate();
                            var left = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue =
                                BuiltinFunctionSpecialized.bit_xor(left, right);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Not:
                        {
                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            var resultValue = BuiltinFunction.bit_not(value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack().Evaluate();
                            var value = currentFrame.PopTopmostFromStack().Evaluate();

                            PineValue resultValue = PineValue.EmptyList;

                            if (BuiltinFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                            {
                                resultValue =
                                    BuiltinFunctionSpecialized.bit_shift_left(shiftCount, value);
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
                                BuiltinFunctionSpecialized.bit_shift_left(shiftCount, value);

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
                                        BuiltinFunctionSpecialized.bit_shift_right(shiftCount, prevValue));
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
                                BuiltinFunctionSpecialized.bit_shift_right(shiftCount, value);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(resultValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var sumValue = BuiltinFunction.int_add(listValue);

                            currentFrame.PushInstructionResult(PineValueInProcess.Create(sumValue));

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack().Evaluate();

                            var productValue = BuiltinFunction.int_mul(listValue);

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

                    case StackInstructionKind.Slice_Skip_Var_Equal_Const:
                        {
                            var literalValue =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var slicedValue = currentFrame.PopTopmostFromStack();

                            var resultValue = false;

                            if (skipCountValue.AsInteger() is { } skipCount)
                            {
                                var skipCountInt =
                                    skipCount < 0 ? 0 : (int)skipCount;

                                resultValue =
                                    slicedValue.SliceSkipVarEqualConst(
                                        skipCount: skipCountInt,
                                        literal: literalValue);
                            }
                            else
                            {
                                resultValue =
                                    literalValue == PineValue.EmptyList;
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

    /// <summary>
    /// Formats a Pine value for error messages by showing its string contents when the value decodes as text.
    /// </summary>
    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        StringEncoding.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");

    private record struct DirectContinuation(
        PineValueInProcess EncodedExprValue,
        PineValueInProcess EnvironmentValue,
        PerformanceCounters PerformanceCounters);

    private static DirectContinuation? DirectContinuationIfSimpleEnough(
        Expression expression,
        PineValueInProcess envValue)
    {
        if (expression is not Expression.Eval evalExpr)
            return null;

        if (evalExpr.Encoded.EvalCount > 0 || evalExpr.Environment.EvalCount > 0)
            return null;

        var encodedExprValue = EvalDirect(evalExpr.Encoded, envValue);

        if (encodedExprValue is null)
            return null;

        var innerEnvValue = EvalDirect(evalExpr.Environment, envValue);

        if (innerEnvValue is null)
            return null;

        var aggregatePerformanceCounters =
            PerformanceCounters.Add(encodedExprValue.Value.perfCounts, innerEnvValue.Value.perfCounts);

        return
            new DirectContinuation(
                EncodedExprValue: encodedExprValue.Value.value,
                EnvironmentValue: innerEnvValue.Value.value,
                PerformanceCounters: aggregatePerformanceCounters);
    }

    private static (PineValueInProcess value, PerformanceCounters perfCounts)? DirectEvalIfSimpleTemplate(
        Expression expression,
        PineValueInProcess envValue)
    {
        if (expression.EvalCount > 0)
            return null;

        if (expression.BuiltinCount is not 0)
            return null;

        return EvalDirect(expression, envValue);
    }

    private static (PineValueInProcess value, PerformanceCounters perfCounts)? EvalDirect(
        Expression expression,
        PineValueInProcess envValue)
    {
        var performanceCounters = new MutablePerformanceCounters();

        var value = EvalDirect(expression, envValue, ref performanceCounters);

        if (value is null)
            return null;

        var perfCounts = performanceCounters.ToImmutable();

        return (value, perfCounts);
    }

    private record struct MutablePerformanceCounters(
        long InvocationCount,
        long BuildListCount,
        long LoopIterationCount,
        long InstructionCount)
    {
        public readonly PerformanceCounters ToImmutable() =>
            new(InvocationCount, BuildListCount, LoopIterationCount, InstructionCount);
    }

    private static PineValueInProcess? EvalDirect(
        Expression expression,
        PineValueInProcess envValue,
        ref MutablePerformanceCounters performanceCounters)
    {
        performanceCounters.InstructionCount++;

        if (expression is Expression.Litral literal)
        {
            return PineValueInProcess.Create(literal.Value);
        }

        if (expression is Expression.List listExpr)
        {
            performanceCounters.BuildListCount++;

            var items = new PineValueInProcess[listExpr.Items.Count];

            for (var i = 0; i < items.Length; ++i)
            {
                var itemExpr = listExpr.Items[i];

                var itemValue =
                    EvalDirect(itemExpr, envValue, ref performanceCounters);

                if (itemValue is null)
                {
                    return null;
                }

                items[i] = itemValue;
            }

            return PineValueInProcess.CreateList(items);
        }

        if (expression is Expression.Conditional conditionalExpr)
        {
            var conditionValue =
                EvalDirect(conditionalExpr.Condition, envValue, ref performanceCounters);

            if (conditionValue is null)
            {
                return null;
            }

            if (PineValueInProcess.AreEqual(conditionValue, PineValueInProcess.KernelTrueValue))
            {
                return EvalDirect(conditionalExpr.TrueBranch, envValue, ref performanceCounters);
            }
            else
            {
                return EvalDirect(conditionalExpr.FalseBranch, envValue, ref performanceCounters);
            }
        }

        if (expression is Expression.Environment)
        {
            return envValue;
        }

        if (expression is Expression.Builtin builtinExpr)
        {
            if (builtinExpr.Function is nameof(BuiltinFunction.length))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.CreateInteger(inputValue.GetLength());
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.equal))
            {
                if (builtinExpr.Input is Expression.List equalInputList)
                {
                    if (equalInputList.Items.Count < 2)
                        return PineValueInProcess.KernelTrueValue;

                    if (EvalDirect(equalInputList.Items[0], envValue, ref performanceCounters) is not { } firstItemValue)
                        return null;

                    for (var i = 1; i < equalInputList.Items.Count; ++i)
                    {
                        if (EvalDirect(equalInputList.Items[i], envValue, ref performanceCounters) is not { } nextItemValue)
                            return null;

                        if (!PineValueInProcess.AreEqual(firstItemValue, nextItemValue))
                            return PineValueInProcess.KernelFalseValue;
                    }

                    return PineValueInProcess.KernelTrueValue;
                }

                {
                    var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                    if (inputValue is null)
                        return null;

                    if (inputValue.IsBlob())
                        return PineValueInProcess.Create(BuiltinFunction.equal(inputValue.Evaluate()));

                    if (inputValue.GetLength() < 2)
                        return PineValueInProcess.KernelTrueValue;

                    var firstItemValue = inputValue.GetElementAt(0);

                    for (var i = 1; i < inputValue.GetLength(); ++i)
                    {
                        var nextItemValue = inputValue.GetElementAt(i);

                        if (!PineValueInProcess.AreEqual(firstItemValue, nextItemValue))
                            return PineValueInProcess.KernelFalseValue;
                    }

                    return PineValueInProcess.KernelTrueValue;
                }
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.head))
            {
                if (builtinExpr.Input is Expression.Builtin innerBuiltinExpr &&
                    innerBuiltinExpr.Function is nameof(BuiltinFunction.skip))
                {
                    if (innerBuiltinExpr.Input is Expression.List skipInputListExpr)
                    {
                        if (skipInputListExpr.Items.Count is not 2)
                            return null;

                        int? skipCount = null;

                        if (skipInputListExpr.Items[0] is Expression.Litral skipCountLiteral)
                        {
                            skipCount = (int?)BuiltinFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value);
                        }

                        if (!skipCount.HasValue)
                        {
                            if (EvalDirect(skipInputListExpr.Items[0], envValue, ref performanceCounters) is { } skipInputValue)
                            {
                                skipCount = (int?)skipInputValue.AsInteger();
                            }
                        }

                        if (skipCount.HasValue)
                        {
                            if (EvalDirect(skipInputListExpr.Items[1], envValue, ref performanceCounters) is { } skipSubjectValue)
                            {
                                if (skipCount <= 0)
                                {
                                    return PineValueInProcess.Head(skipSubjectValue);
                                }

                                return skipSubjectValue.GetElementAt((int)skipCount);
                            }
                        }
                    }
                    else
                    {
                        if (EvalDirect(innerBuiltinExpr.Input, envValue, ref performanceCounters) is { } skipInputValue)
                        {
                            var skipInputLength = skipInputValue.GetLength();

                            if (skipInputLength is 2)
                            {
                                var skipCountValue = skipInputValue.GetElementAt(0);

                                if (skipCountValue.AsInteger() is { } skipCount)
                                {
                                    if (skipInputValue.GetElementAt(1) is { } skipSubjectValue)
                                    {
                                        if (skipCount <= 0)
                                        {
                                            return PineValueInProcess.Head(skipSubjectValue);
                                        }

                                        return skipSubjectValue.GetElementAt((int)skipCount);
                                    }
                                }
                            }
                        }
                    }
                }

                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Head(inputValue);
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.skip))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                if (!inputValue.IsList() || inputValue.GetLength() is not 2)
                    return PineValueInProcess.EmptyList;

                var skipCount = inputValue.GetElementAt(0).AsInteger();

                if (!skipCount.HasValue)
                    return PineValueInProcess.EmptyList;

                var source = inputValue.GetElementAt(1);

                if (skipCount.Value <= 0)
                    return source;

                if (int.MaxValue < skipCount.Value)
                    return null;

                return PineValueInProcess.Skip((int)skipCount.Value, source);
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.take))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                if (!inputValue.IsList() || inputValue.GetLength() is not 2)
                    return PineValueInProcess.EmptyList;

                var takeCount = inputValue.GetElementAt(0).AsInteger();

                if (!takeCount.HasValue)
                    return PineValueInProcess.EmptyList;

                var source = inputValue.GetElementAt(1);

                if (takeCount.Value <= 0)
                {
                    return
                        source.IsBlob()
                        ?
                        PineValueInProcess.EmptyBlob
                        :
                        PineValueInProcess.EmptyList;
                }

                if (int.MaxValue < takeCount.Value)
                    return source;

                return PineValueInProcess.Take((int)takeCount.Value, source);
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.concat))
            {
                if (builtinExpr.Input is Expression.List concatList && concatList.Items.Count is 2)
                {
                    if (EvalDirect(concatList.Items[0], envValue, ref performanceCounters) is { } leftValue &&
                        EvalDirect(concatList.Items[1], envValue, ref performanceCounters) is { } rightValue)
                    {
                        return PineValueInProcess.ConcatBinary(leftValue, rightValue);
                    }
                }

                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Concat(inputValue);
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.int_is_sorted_asc))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_is_sorted_asc(inputValue.Evaluate()));
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.int_add))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_add(inputValue.Evaluate()));
            }

            if (builtinExpr.Function is nameof(BuiltinFunction.int_mul))
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref performanceCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_mul(inputValue.Evaluate()));
            }

            return null;
        }

        if (expression is Expression.Label stringTagExpr)
        {
            return EvalDirect(stringTagExpr.Tagged, envValue, ref performanceCounters);
        }

        return null;
    }
}
