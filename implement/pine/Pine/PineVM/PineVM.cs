using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public interface IPineVM
{
    Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment);
}

public record struct EvalCacheEntryKey(
    PineValue ExprValue,
    PineValue EnvValue);

public class PineVM : IPineVM
{
    public long EvaluateExpressionCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    private readonly ParseExprDelegate parseExpressionDelegate;

    private IDictionary<EvalCacheEntryKey, PineValue>? EvalCache { init; get; }

    private readonly Action<Expression, PineValue, TimeSpan, PineValue>? reportFunctionApplication;

    public static PineVM Construct(
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? parseExpressionOverrides = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null)
    {
        var parseExpressionOverridesDict =
            parseExpressionOverrides
            ?.ToFrozenDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        return new PineVM(
            overrideParseExpression:
            parseExpressionOverridesDict switch
            {
                null =>
                originalHandler => originalHandler,

                not null =>
                _ => value => ExpressionEncoding.ParseExpressionFromValue(value, parseExpressionOverridesDict)
            },
            evalCache: evalCache);
    }

    public PineVM(
        OverrideParseExprDelegate? overrideParseExpression = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Action<Expression, PineValue, TimeSpan, PineValue>? reportFunctionApplication = null)
    {
        parseExpressionDelegate =
            overrideParseExpression
            ?.Invoke(ExpressionEncoding.ParseExpressionFromValueDefault) ??
            ExpressionEncoding.ParseExpressionFromValueDefault;

        EvalCache = evalCache;

        this.reportFunctionApplication = reportFunctionApplication;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) => EvaluateExpressionDefault(expression, environment);

    public record StackFrameInstructions(
        IReadOnlyList<StackInstruction> Instructions)
    {
        public virtual bool Equals(StackFrameInstructions? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                Instructions.Count == notNull.Instructions.Count &&
                Instructions.SequenceEqual(notNull.Instructions);
        }

        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            foreach (var item in Instructions)
            {
                hashCode.Add(item.GetHashCode());
            }

            return hashCode.ToHashCode();
        }
    }

    record StackFrame(
        PineValue? ExpressionValue,
        Expression Expression,
        StackFrameInstructions Instructions,
        PineValue EnvironmentValue,
        Memory<PineValue> InstructionsResultValues,
        long BeginTimestamp)
    {
        public int InstructionPointer { get; set; } = 0;
    }

    readonly Dictionary<Expression, StackFrameInstructions> stackFrameDict = new();

    StackFrame StackFrameFromExpression(
        PineValue? expressionValue,
        Expression expression,
        PineValue environment)
    {
        var instructions = InstructionsFromExpression(expression);

        return new StackFrame(
            expressionValue,
            expression,
            instructions,
            EnvironmentValue: environment,
            new PineValue[instructions.Instructions.Count],
            BeginTimestamp: System.Diagnostics.Stopwatch.GetTimestamp());
    }

    public StackFrameInstructions InstructionsFromExpression(Expression rootExpression)
    {
        if (stackFrameDict.TryGetValue(rootExpression, out var cachedInstructions))
        {
            return cachedInstructions;
        }

        var instructions = InstructionsFromExpressionLessCache(rootExpression);

        stackFrameDict[rootExpression] = instructions;

        return instructions;
    }

    public static StackFrameInstructions InstructionsFromExpressionLessCache(Expression rootExpression)
    {
        var separatedExprs = InstructionsFromExpressionTransitive(rootExpression);

        if (separatedExprs.Count is 0)
        {
            return
                new StackFrameInstructions(
                    Instructions:
                    [StackInstruction.Eval(rootExpression), StackInstruction.Return]);
        }

        IReadOnlyList<StackInstruction> appendedForRoot =
            rootExpression switch
            {
                Expression.ConditionalExpression =>
                [],

                _ =>
                [StackInstruction.Eval(rootExpression),
                StackInstruction.Return]
            };

        var separatedExprsValuesBeforeFilter =
            separatedExprs
            .SelectMany(pair => pair.Value)
            .Concat(appendedForRoot)
            .ToImmutableArray();

        static IEnumerable<StackInstruction> SkipConsecutiveDuplicates(IEnumerable<StackInstruction> sequence)
        {
            StackInstruction? prevInstruction = null;

            foreach (var item in sequence)
            {
                if (
                    item is StackInstruction.EvalInstruction currentEvalInst &&
                    prevInstruction is StackInstruction.EvalInstruction prevEvalInst &&
                    currentEvalInst.Expression == prevEvalInst.Expression)
                {
                    continue;
                }

                prevInstruction = item;

                yield return item;
            }
        }

        var filteredInstructions =
            SkipConsecutiveDuplicates(separatedExprsValuesBeforeFilter)
            .ToImmutableArray();

        var instructionIndexFromExpr = new Dictionary<Expression, int>();

        var separatedExprsValues =
            filteredInstructions
            .Select((instruction, instructionIndex) =>
            {
                if (instruction is StackInstruction.EvalInstruction evalInst)
                {
                    if (instructionIndexFromExpr.TryGetValue(evalInst.Expression, out var earlierIndex))
                    {
                        return StackInstruction.Eval(
                            new Expression.StackReferenceExpression(earlierIndex - instructionIndex));
                    }

                    instructionIndexFromExpr.Add(evalInst.Expression, instructionIndex);
                }

                return instruction;
            })
            .ToImmutableArray();

        StackInstruction MapInstruction(
            StackInstruction originalInst,
            int selfIndex)
        {
            if (originalInst is StackInstruction.ConditionalJumpRefInstruction conditionalJumpRef)
            {
                if (!instructionIndexFromExpr.TryGetValue(conditionalJumpRef.IfTrueExpr, out var ifTrueExprIndex))
                {
                    throw new InvalidOperationException("Expected to find ifTrueExpr in map.");
                }

                return
                    new StackInstruction.ConditionalJumpInstruction(
                        Condition: conditionalJumpRef.Condition,
                        IfTrueOffset: ifTrueExprIndex - selfIndex);
            }

            return
                StackInstruction.TransformExpressionWithOptionalReplacement(
                    findReplacement:
                    descendant =>
                    {
                        if (originalInst is StackInstruction.EvalInstruction origEval && origEval.Expression == descendant)
                        {
                            return null;
                        }

                        if (instructionIndexFromExpr.TryGetValue(descendant, out var instructionIndex))
                        {
                            var offset = instructionIndex - selfIndex;

                            if (offset >= 0)
                            {
                                throw new Exception(
                                    "Found non-negative offset for stack ref expr: " + offset +
                                    " (selfIndex is " + selfIndex + ")");
                            }

                            return new Expression.StackReferenceExpression(offset: offset);
                        }

                        {
                            /*
                             * Skip over all expressions that we do not descend into when enumerating the components.
                             * (EnumerateComponentsOrderedForCompilation)
                             * */

                            if (descendant is Expression.ConditionalExpression)
                            {
                                // Return non-null value to stop descend.
                                return descendant;
                            }
                        }

                        return null;
                    },
                    instruction: originalInst);
        }

        var componentExprMapped =
            separatedExprsValues
            .Select(MapInstruction)
            .ToImmutableArray();

        return new StackFrameInstructions(Instructions: componentExprMapped);
    }

    static IReadOnlyList<KeyValuePair<Expression, IReadOnlyList<StackInstruction>>>
        InstructionsFromExpressionTransitive(Expression rootExpression)
    {
        var allExpressions =
            EnumerateComponentsOrderedForCompilation(rootExpression)
            .ToImmutableArray();

        var allExpressionsCount = new Dictionary<Expression, int>();

        foreach (var expression in allExpressions)
        {
            if (allExpressionsCount.TryGetValue(expression, out var count))
            {
                allExpressionsCount[expression] = count + 1;
            }
            else
            {
                allExpressionsCount[expression] = 1;
            }
        }

        static IReadOnlyList<StackInstruction>? instructionsFromExpression(Expression expression)
        {
            if (expression is Expression.ParseAndEvalExpression parseAndEval)
            {
                return [StackInstruction.Eval(expression)];
            }

            /*
             * At the moment we add a new stack frame to evaluate a conditional expression.
             * 
            if (expression is Expression.ConditionalExpression conditional)
            {
                return
                [
                    new StackInstruction.ConditionalJumpRefInstruction(
                        Condition: conditional.condition,
                        IfTrueExpr:conditional.ifTrue),
                    StackInstruction.Eval(conditional.ifFalse),
                    StackInstruction.Return,
                    StackInstruction.Eval(conditional.ifTrue),
                    StackInstruction.Return
                ];
            }
            */

            return null;
        }

        var separatedInstructions =
            allExpressions
            .SelectMany(expression =>
            {
                if (instructionsFromExpression(expression) is { } instructions)
                {
                    return
                    (KeyValuePair<Expression, IReadOnlyList<StackInstruction>>[])
                    [new KeyValuePair<Expression, IReadOnlyList<StackInstruction>>(expression, instructions)];
                }

                if (ExpressionLargeEnoughForCSE(expression) && allExpressionsCount[expression] > 1)
                {
                    return
                    (KeyValuePair<Expression, IReadOnlyList<StackInstruction>>[])
                    [new KeyValuePair<Expression, IReadOnlyList<StackInstruction>>(expression, [StackInstruction.Eval(expression)])];
                }

                return [];
            })
            .ToImmutableArray();

        return separatedInstructions;
    }

    public static IEnumerable<Expression> EnumerateComponentsOrderedForCompilation(Expression expression)
    {
        if (expression is Expression.ListExpression list)
        {
            foreach (var item in list.List)
            {
                foreach (var descendant in EnumerateComponentsOrderedForCompilation(item))
                {
                    yield return descendant;
                }
            }
        }

        if (expression is Expression.ParseAndEvalExpression parseAndEval)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(parseAndEval.expression))
            {
                yield return descendant;
            }

            foreach (var descendant in EnumerateComponentsOrderedForCompilation(parseAndEval.environment))
            {
                yield return descendant;
            }
        }

        if (expression is Expression.KernelApplicationExpression kernelApp)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(kernelApp.argument))
            {
                yield return descendant;
            }
        }

        if (expression is Expression.ConditionalExpression conditional)
        {
            /*
             * 
            foreach (var descendant in EnumerateSelfAndDescendantsOrderedForCompilation(conditional.condition))
            {
                yield return descendant;
            }
            */

            /*
             *
             * For now, we create a new stack frame for each conditional expression.
             * Therefore do not descend into the branches of the conditional expression.

            foreach (var descendant in EnumerateSelfAndDescendantsOrderedForCompilation(conditional.ifTrue))
            {
                yield return descendant;
            }

            foreach (var descendant in EnumerateSelfAndDescendantsOrderedForCompilation(conditional.ifFalse))
            {
                yield return descendant;
            }
            */
        }

        if (expression is Expression.StringTagExpression stringTag)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(stringTag.tagged))
            {
                yield return descendant;
            }
        }

        yield return expression;
    }

    static bool ExpressionLargeEnoughForCSE(Expression expression)
    {
        if (expression is Expression.KernelApplicationExpression)
            return true;

        if (expression is Expression.ListExpression list)
        {
            foreach (var item in list.List)
            {
                if (ExpressionLargeEnoughForCSE(item))
                {
                    return true;
                }
            }

            return false;
        }

        return false;
    }

    public Result<string, PineValue> EvaluateExpressionDefault(
        Expression rootExpression,
        PineValue rootEnvironment)
    {
        var stack = new Stack<StackFrame>();

        stack.Push(
            StackFrameFromExpression(
                expressionValue: null,
                rootExpression,
                rootEnvironment));

        while (true)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.Instructions.Instructions.Count <= currentFrame.InstructionPointer)
            {
                return
                    "Instruction pointer out of bounds. Missing explicit return instruction.";
            }

            ReadOnlyMemory<PineValue> stackPrevValues =
                currentFrame.InstructionsResultValues[..currentFrame.InstructionPointer];

            var currentInstruction = currentFrame.Instructions.Instructions[currentFrame.InstructionPointer];

            if (currentInstruction is StackInstruction.ReturnInstruction)
            {
                if (currentFrame.InstructionPointer is 0)
                {
                    return "Return instruction at beginning of frame";
                }

                var frameReturnValue = currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer - 1];

                if (currentFrame.ExpressionValue is { } currentFrameExprValue && EvalCache is { } evalCache)
                {
                    var frameElapsedTime = System.Diagnostics.Stopwatch.GetElapsedTime(currentFrame.BeginTimestamp);

                    if (frameElapsedTime.TotalMilliseconds > 3 && EvalCache is not null)
                    {
                        evalCache.TryAdd(new EvalCacheEntryKey(currentFrameExprValue, currentFrame.EnvironmentValue), frameReturnValue);
                    }

                    reportFunctionApplication?.Invoke(
                        currentFrame.Expression,
                        currentFrame.EnvironmentValue,
                        frameElapsedTime,
                        frameReturnValue);
                }

                stack.Pop();

                if (stack.Count is 0)
                {
                    return frameReturnValue;
                }

                var previousFrame = stack.Peek();

                previousFrame.InstructionsResultValues.Span[previousFrame.InstructionPointer] = frameReturnValue;

                previousFrame.InstructionPointer++;
                continue;
            }

            if (currentInstruction is StackInstruction.EvalInstruction evalInstr)
            {
                if (evalInstr.Expression is Expression.ParseAndEvalExpression parseAndEval)
                {
                    var evalExprValueResult =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.expression,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalExprValueResult is Result<string, PineValue>.Err parseFunctionError)
                    {
                        return "Failed to evaluate expr value for parse and eval: " + parseFunctionError;
                    }

                    if (evalExprValueResult is not Result<string, PineValue>.Ok evalExprValueOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalExprValueResult.GetType().FullName);
                    }

                    var evalEnvResult =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.environment,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalEnvResult is Result<string, PineValue>.Err evalEnvError)
                    {
                        return "Failed to evaluate environment: " + evalEnvError;
                    }

                    if (evalEnvResult is not Result<string, PineValue>.Ok evalEnvOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalEnvResult.GetType().FullName);
                    }

                    if (EvalCache is { } evalCache)
                    {
                        var evalCacheKey = new EvalCacheEntryKey(evalExprValueOk.Value, evalEnvOk.Value);

                        if (evalCache.TryGetValue(evalCacheKey, out var cachedValue) && cachedValue is not null)
                        {
                            currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = cachedValue;
                            currentFrame.InstructionPointer++;
                            continue;
                        }
                    }

                    var parseResult = parseExpressionDelegate(evalExprValueOk.Value);

                    if (parseResult is Result<string, Expression>.Err parseErr)
                    {
                        return
                            "Failed to parse expression from value: " + parseErr.Value +
                            " - expressionValue is " + DescribeValueForErrorMessage(evalExprValueOk.Value) +
                            " - environmentValue is " + DescribeValueForErrorMessage(evalExprValueOk.Value);
                    }

                    if (parseResult is not Result<string, Expression>.Ok parseOk)
                    {
                        throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
                    }

                    stack.Push(StackFrameFromExpression(expressionValue: evalExprValueOk.Value, parseOk.Value, evalEnvOk.Value));

                    continue;
                }

                if (evalInstr.Expression is Expression.ConditionalExpression conditionalExpr)
                {
                    var evalConditionResult =
                        EvaluateExpressionDefaultLessStack(
                            conditionalExpr.condition,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalConditionResult is Result<string, PineValue>.Err evalConditionError)
                    {
                        return "Failed to evaluate condition: " + evalConditionError;
                    }

                    if (evalConditionResult is not Result<string, PineValue>.Ok evalConditionOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalConditionResult.GetType().FullName);
                    }

                    var expressionToContinueWith =
                        evalConditionOk.Value == PineVMValues.TrueValue
                        ?
                        conditionalExpr.ifTrue
                        :
                        evalConditionOk.Value == PineVMValues.FalseValue
                        ?
                        conditionalExpr.ifFalse
                        :
                        null;

                    if (expressionToContinueWith is not null)
                    {
                        if (ExpressionShouldGetNewStackFrame(expressionToContinueWith))
                        {
                            stack.Push(
                                StackFrameFromExpression(
                                    expressionValue: null,
                                    expressionToContinueWith,
                                    currentFrame.EnvironmentValue));

                            continue;
                        }

                        var evalBranchResult =
                            EvaluateExpressionDefaultLessStack(
                                expressionToContinueWith,
                                currentFrame.EnvironmentValue,
                                stackPrevValues: stackPrevValues);

                        if (evalBranchResult is Result<string, PineValue>.Err evalBranchError)
                        {
                            return "Failed to evaluate conditional branch: " + evalBranchError;
                        }

                        if (evalBranchResult is not Result<string, PineValue>.Ok evalBranchOk)
                        {
                            throw new NotImplementedException(
                                "Unexpected result type: " + evalBranchResult.GetType().FullName);
                        }

                        currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = evalBranchOk.Value;
                        currentFrame.InstructionPointer++;
                        continue;
                    }
                    else
                    {
                        currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = PineValue.EmptyList;
                        currentFrame.InstructionPointer++;
                        continue;
                    }
                }

                var evalResult =
                    EvaluateExpressionDefaultLessStack(
                        evalInstr.Expression,
                        currentFrame.EnvironmentValue,
                        stackPrevValues: stackPrevValues);

                if (evalResult is Result<string, PineValue>.Err evalError)
                {
                    return "Failed to evaluate expression: " + evalError;
                }

                if (evalResult is not Result<string, PineValue>.Ok evalOk)
                {
                    throw new NotImplementedException("Unexpected result type: " + evalResult.GetType().FullName);
                }

                currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = evalOk.Value;
                currentFrame.InstructionPointer++;
                continue;
            }

            if (currentInstruction is StackInstruction.ConditionalJumpInstruction conditionalStatement)
            {
                var evalConditionResult =
                    EvaluateExpressionDefaultLessStack(
                        conditionalStatement.Condition,
                        currentFrame.EnvironmentValue,
                        stackPrevValues: stackPrevValues);

                if (evalConditionResult is Result<string, PineValue>.Err evalConditionError)
                {
                    return "Failed to evaluate condition: " + evalConditionError;
                }

                if (evalConditionResult is not Result<string, PineValue>.Ok evalConditionOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type: " + evalConditionResult.GetType().FullName);
                }

                if (evalConditionOk.Value == PineVMValues.TrueValue)
                {
                    currentFrame.InstructionPointer += conditionalStatement.IfTrueOffset;
                    continue;
                }

                currentFrame.InstructionPointer++;
                continue;
            }

            return "Unexpected instruction type: " + currentInstruction.GetType().FullName;
        }
    }

    private static bool ExpressionShouldGetNewStackFrame(Expression expression)
    {
        if (expression is Expression.LiteralExpression)
            return false;

        if (expression is Expression.EnvironmentExpression)
            return false;

        if (expression is Expression.ParseAndEvalExpression)
            return true;

        if (expression is Expression.KernelApplicationExpression kernelApp)
        {
            return ExpressionShouldGetNewStackFrame(kernelApp.argument);
        }

        if (expression is Expression.ConditionalExpression conditional)
        {
            return
                (ExpressionShouldGetNewStackFrame(conditional.condition) ||
                ExpressionShouldGetNewStackFrame(conditional.ifTrue) ||
                ExpressionShouldGetNewStackFrame(conditional.ifFalse));
        }

        if (expression is Expression.ListExpression list)
        {
            foreach (var item in list.List)
            {
                if (ExpressionShouldGetNewStackFrame(item))
                {
                    return true;
                }
            }

            return false;
        }

        if (expression is Expression.StringTagExpression stringTag)
            return ExpressionShouldGetNewStackFrame(stringTag.tagged);

        if (expression is Expression.DelegatingExpression)
            return false;

        if (expression is Expression.StackReferenceExpression)
            return false;

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    private Result<string, PineValue> EvaluateExpressionDefaultLessStack(
        Expression expression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        if (expression is Expression.LiteralExpression literalExpression)
            return literalExpression.Value;

        if (expression is Expression.ListExpression listExpression)
        {
            return EvaluateListExpression(listExpression, environment, stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.ParseAndEvalExpression applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(
                    applicationExpression,
                    environment,
                    stackPrevValues: stackPrevValues)
                switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate parse and evaluate: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(
                    environment,
                    kernelApplicationExpression,
                    stackPrevValues: stackPrevValues) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate kernel function application: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(
                environment,
                conditionalExpression,
                stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.EnvironmentExpression)
        {
            return environment;
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpressionDefaultLessStack(
                stringTagExpression.tagged,
                environment,
                stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.DelegatingExpression delegatingExpr)
        {
            return delegatingExpr.Delegate.Invoke(EvaluateExpressionDefault, environment);
        }

        if (expression is Expression.StackReferenceExpression stackRef)
        {
            var index = stackPrevValues.Length + stackRef.offset;

            if (index < 0 || index >= stackPrevValues.Length)
            {
                return
                    "Invalid stack reference: offset " + stackRef.offset +
                    " from " + stackPrevValues.Length +
                    " results in index " + index;
            }

            return stackPrevValues.Span[index];
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public Result<string, PineValue> EvaluateListExpression(
        Expression.ListExpression listExpression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var listItems = new List<PineValue>(listExpression.List.Count);

        for (var i = 0; i < listExpression.List.Count; i++)
        {
            var item = listExpression.List[i];

            var itemResult = EvaluateExpressionDefaultLessStack(
                item,
                environment,
                stackPrevValues: stackPrevValues);

            if (itemResult is Result<string, PineValue>.Err itemErr)
                return "Failed to evaluate list element [" + i + "]: " + itemErr.Value;

            if (itemResult is Result<string, PineValue>.Ok itemOk)
            {
                listItems.Add(itemOk.Value);
                continue;
            }

            throw new NotImplementedException("Unexpected result type: " + itemResult.GetType().FullName);
        }

        return PineValue.List(listItems);
    }

    public Result<string, PineValue> EvaluateParseAndEvalExpression(
        Expression.ParseAndEvalExpression parseAndEval,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        Result<string, PineValue> continueWithEnvValueAndFunction(
            PineValue environmentValue,
            Expression functionExpression)
        {
            if (environmentValue is PineValue.ListValue list)
            {
                FunctionApplicationMaxEnvSize =
                FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
            }

            return EvaluateExpression(environment: environmentValue, expression: functionExpression);
        }

        return
            EvaluateExpressionDefaultLessStack(
                parseAndEval.environment,
                environment,
                stackPrevValues: stackPrevValues) switch
            {
                Result<string, PineValue>.Err envErr =>
                "Failed to evaluate argument: " + envErr.Value,

                Result<string, PineValue>.Ok environmentValue =>
                EvaluateExpressionDefaultLessStack(
                    parseAndEval.expression,
                    environment,
                    stackPrevValues: stackPrevValues) switch
                {
                    Result<string, PineValue>.Err exprErr =>
                    "Failed to evaluate expression: " + exprErr.Value,

                    Result<string, PineValue>.Ok expressionValue =>
                    parseExpressionDelegate(expressionValue.Value) switch
                    {
                        Result<string, Expression>.Err parseErr =>
                        "Failed to parse expression from value: " + parseErr.Value +
                        " - expressionValue is " + DescribeValueForErrorMessage(expressionValue.Value) +
                        " - environmentValue is " + DescribeValueForErrorMessage(expressionValue.Value),

                        Result<string, Expression>.Ok functionExpression =>
                        continueWithEnvValueAndFunction(environmentValue.Value, functionExpression.Value),

                        var otherResult =>
                        throw new NotImplementedException("Unexpected result type for parse: " + otherResult.GetType().FullName)
                    },

                    var otherResult =>
                    throw new NotImplementedException("Unexpected result type for expr: " + otherResult.GetType().FullName)
                },

                var otherResult =>
                throw new NotImplementedException("Unexpected result type for env: " + otherResult.GetType().FullName)
            };
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");


    public Result<string, PineValue> EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application,
        ReadOnlyMemory<PineValue> stackPrevValues) =>
        EvaluateExpressionDefaultLessStack(application.argument, environment, stackPrevValues) switch
        {
            Result<string, PineValue>.Ok argument =>
            application.function(argument.Value),

            Result<string, PineValue>.Err error =>
            "Failed to evaluate argument: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };


    public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional,
        ReadOnlyMemory<PineValue> stackPrevValues) =>
        EvaluateExpressionDefaultLessStack(
            conditional.condition,
            environment,
            stackPrevValues: stackPrevValues)
        switch
        {
            Result<string, PineValue>.Ok conditionValue =>
            conditionValue == PineVMValues.TrueValue
            ?
            EvaluateExpressionDefaultLessStack(
                conditional.ifTrue,
                environment,
                stackPrevValues: stackPrevValues)
            :
            conditionValue == PineVMValues.FalseValue
            ?
            EvaluateExpressionDefaultLessStack(
                conditional.ifFalse,
                environment,
                stackPrevValues: stackPrevValues)
            :
            PineValue.EmptyList,

            Result<string, PineValue>.Err error =>
            "Failed to evaluate condition: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };
}
