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

    private readonly IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? _compilationEnvClasses;

    private readonly bool _disableReductionInCompilation;

    private readonly Func<Expression, PineValueInProcess, PineVMParseCache, Func<PrecompiledResult>?>? _selectPrecompiled = null;

    private readonly Func<Expression, bool> _skipInlineForExpression;

    private readonly bool _enableTailRecursionOptimization;

    public readonly PineVMParseCache ParseCache;

    private readonly Dictionary<Expression, PineValue> _encodeExpressionCache = [];

    private readonly IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? _precompiledLeaves;

    private readonly Action<PineValue, PineValue>? _reportEnterPrecompiledLeaf;

    private readonly Action<PineValue, PineValue, PineValue?>? _reportExitPrecompiledLeaf;

    private readonly OptimizationParametersSerial? _optimizationParametersSerial = null;

    private readonly StackFrameInputHash _stackFrameInputHash = new();

    private readonly IFileStore? _cacheFileStore;

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
        IFileStore? cacheFileStore)
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
                cacheFileStore: cacheFileStore);

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
        IFileStore? cacheFileStore)
    {
        EvalCache = evalCache;

        _evaluationConfigDefault = evaluationConfigDefault;

        _reportFunctionApplication = reportFunctionApplication;

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
                _evaluationConfigDefault ?? new EvaluationConfig(ParseAndEvalCountLimit: null));

        if (evalReportResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (evalReportResult.IsOkOrNull() is not { } evalReport)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + evalReportResult.GetType().FullName);
        }

        return evalReport.ReturnValue.Evaluate();
    }

    readonly Dictionary<Expression, ExpressionEntry> _expressionCompilationDict = [];

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

        return new StackFrame(
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

        return compilation;
    }

    private ExpressionEntry ExpressionEntryLessCache(Expression rootExpression)
    {
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
                enableTailRecursionOptimization: _enableTailRecursionOptimization);

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

    public record EvaluationConfig(
        int? ParseAndEvalCountLimit);

    public Result<string, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config)
    {
        long instructionCount = 0;
        long loopIterationCount = 0;
        long parseAndEvalCount = 0;
        long stackFrameCount = 0;
        long stackFrameReplaceCount = 0;
        long lastCacheEntryInstructionCount = 0;
        long lastCacheEntryParseAndEvalCount = 0;

        var stack = new Stack<StackFrame>();

        string? InvokePrecompiledOrBuildStackFrame(
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
                                    "Failed to parse expression from value: " + contParseErr +
                                    " - expressionValue is " +
                                    (expressionValue is null ? "null" : DescribeValueForErrorMessage(expressionValue)) +
                                    " - environmentValue is " + DescribeValueForErrorMessage(environmentValue.Evaluate());
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
                                        BeginParseAndEvalCount: parseAndEvalCount,
                                        BeginStackFrameCount: stackFrameCount),
                                    Specialization: specialization.Stepwise,
                                    CacheFileName: null);

                            PushStackFrame(
                                newFrame,
                                replaceCurrentFrame: false);

                            return null;
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
                                    "Failed to decode cached value for cache file '" + cacheFileName + "'.", ex);
                            }
                        }
                    }
                }

                BuildAndPushStackFrame
                (
                    expressionValue: expressionValue,
                    expression: expression,
                    instructions: instructions,
                    stackFrameInput: stackFrameInput,
                    cacheFileName: cacheFileName,
                    replaceCurrentFrame: replaceCurrentFrame
                );

                return null;
            }
        }

        void BuildAndPushStackFrame(
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
                    BeginParseAndEvalCount: parseAndEvalCount,
                    BeginStackFrameCount: stackFrameCount);

            var newFrame =
                BuildStackFrame(
                    expressionValue: expressionValue,
                    expression: expression,
                    instructions: instructions,
                    stackFrameInput: stackFrameInput,
                    cacheFileName: cacheFileName,
                    profilingBaseline: newFrameProfilingBaseline);

            PushStackFrame(newFrame, replaceCurrentFrame: replaceCurrentFrame);
        }

        void PushStackFrame(
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
        }

        EvaluationReport? ReturnFromStackFrame(PineValueInProcess frameReturnValue)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.ExpressionValue is { } currentFrameExprValue)
            {
                var frameTotalInstructionCount =
                    instructionCount - currentFrame.ProfilingBaseline.BeginInstructionCount;

                var frameParseAndEvalCount = parseAndEvalCount - currentFrame.ProfilingBaseline.BeginParseAndEvalCount;
                var frameStackFrameCount = stackFrameCount - currentFrame.ProfilingBaseline.BeginStackFrameCount;

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
                        InstructionCount: frameTotalInstructionCount,
                        LoopIterationCount: currentFrame.LoopIterationCount,
                        InvocationCount: frameParseAndEvalCount,
                        ReturnValue: frameReturnValue,
                        StackTrace: CompileStackTrace(10)));
            }

            stack.Pop();

            if (stack.Count is 0)
            {
                var rootExprValue = EncodeExpressionAsValue(rootExpression);

                return new EvaluationReport(
                    ExpressionValue: rootExprValue,
                    Expression: rootExpression,
                    Input: StackFrameInput.GenericFromEnvironmentValue(rootEnvironment),
                    InstructionCount: instructionCount,
                    LoopIterationCount: loopIterationCount,
                    InvocationCount: parseAndEvalCount,
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

        var compilation =
            GetExpressionEntry(rootExpression).Compilation;

        var instructions =
            compilation.SelectInstructionsForEnvironment(PineValueInProcess.Create(rootEnvironment));

        var stackFrameInput =
            StackFrameInput.FromEnvironmentValue(
                environmentValue: rootEnvironment,
                parameters: instructions.Parameters);

        BuildAndPushStackFrame(
            expressionValue: null,
            rootExpression,
            instructions,
            stackFrameInput,
            cacheFileName: null,
            replaceCurrentFrame: false);

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
                        "Instruction pointer out of bounds. Missing explicit return instruction.";
                }

                var currentInstruction =
                    currentFrame.Instructions.Instructions[currentFrame.InstructionPointer]
                    ??
                    throw new InvalidOperationException("currentInstruction is null");

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
                                        PineValueInProcess.CreateList(items), targetList);
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
                                        targetList, PineValueInProcess.CreateList(items));
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
                            var right = currentInstruction.IntegerLiteral
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
                            {
                                ++parseAndEvalCount;

                                if (config.ParseAndEvalCountLimit is { } limit && parseAndEvalCount > limit)
                                {
                                    var stackTraceHashes =
                                        CompileStackTrace(100)
                                        .Select(expr => s_mutableCacheValueHash.GetHash(EncodeExpressionAsValue(expr)))
                                        .ToArray();

                                    return
                                        "Parse and eval count limit exceeded: " +
                                        CommandLineInterface.FormatIntegerForDisplay(limit) +
                                        "\nLast stack frames expressions:\n" +
                                        string.Join("\n", stackTraceHashes.Select(hash => Convert.ToHexStringLower(hash.Span)[..8]));
                                }
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
                                    "Failed to parse expression from value: " + parseErr +
                                    " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                                    " - environmentValue is " + DescribeValueForErrorMessage(environmentValue.Evaluate());
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
                                loopIterationCount++;
                                currentFrame.LoopIterationCount++;
                            }

                            continue;
                        }

                    case StackInstructionKind.Jump_If_True_Const:
                        {
                            var conditionValue = currentFrame.PopTopmostFromStack();

                            if (PineValueInProcess.AreEqual(conditionValue, PineKernelValues.TrueValue))
                            {
                                var jumpOffset =
                                    currentInstruction.JumpOffset
                                    ??
                                    throw new Exception("Invalid operation form: Missing jump offset");

                                currentFrame.InstructionPointer += 1 + jumpOffset;

                                if (jumpOffset < 0)
                                {
                                    loopIterationCount++;
                                    currentFrame.LoopIterationCount++;
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
                            var right = currentInstruction.Literal
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
