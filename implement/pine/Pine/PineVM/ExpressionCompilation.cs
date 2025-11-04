using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using static Pine.PineVM.PineVM;

namespace Pine.PineVM;


public record ExpressionCompilation(
    StackFrameInstructions Generic,
    IReadOnlyList<(IReadOnlyList<EnvConstraintItem> constraint, StackFrameInstructions instructions)> Specialized)
{
    public StackFrameInstructions SelectInstructionsForEnvironment(PineValueInProcess environment)
    {
        for (var i = 0; i < Specialized.Count; i++)
        {
            var specialization = Specialized[i];

            var foundMismatch = false;

            for (var specializationIndex = 0; specializationIndex < specialization.constraint.Count; specializationIndex++)
            {
                var constraintItem = specialization.constraint[specializationIndex];

                if (PineValueInProcess.ValueFromPathOrNull(environment, constraintItem.Path.Span) is not { } pathValue)
                {
                    foundMismatch = true;
                    break;
                }

                if (!pathValue.Equals(constraintItem.Value))
                {
                    foundMismatch = true;
                    break;
                }
            }

            if (!foundMismatch)
            {
                return specialization.instructions;
            }
        }

        return Generic;
    }


    public static ExpressionCompilation CompileExpression(
        Expression rootExpression,
        IReadOnlyList<PineValueClass> specializations,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining)
    {
        var genericParameters =
            StaticFunctionInterface.FromExpression(rootExpression);

        var generic =
            new StackFrameInstructions(
                genericParameters,
                InstructionsFromExpressionTransitive(
                    rootExpression,
                    envConstraintId: null,
                    parametersAsLocals: genericParameters,
                    parseCache: parseCache,
                    disableReduction: disableReduction,
                    skipInlining: skipInlining,
                    enableTailRecursionOptimization: enableTailRecursionOptimization),
                TrackEnvConstraint: null);

        var specialized =
            specializations
            // Order to prefer more specific constraints when selecting at runtime.
            .OrderDescending(PineValueClassSpecificityComparer.Instance)
            .Select(
                specialization =>
                            ((IReadOnlyList<EnvConstraintItem>)
                            [..specialization.ParsedItems
                        .Select(envItem => new EnvConstraintItem(envItem.Key.ToArray(), envItem.Value))],
                            new StackFrameInstructions(
                                genericParameters,
                                InstructionsFromExpressionTransitive(
                                    rootExpression,
                                    envConstraintId: specialization,
                                    parametersAsLocals: genericParameters,
                                    parseCache: parseCache,
                                    disableReduction: disableReduction,
                        enableTailRecursionOptimization: enableTailRecursionOptimization,
                                    skipInlining: skipInlining),
                    TrackEnvConstraint: specialization)))
            .ToImmutableArray();

        return new ExpressionCompilation(
            Generic: generic,
            Specialized: specialized);
    }

    /*
     * 
     * In the older impl to compile to C#, we had CompilationUnitEnvExprEntry to inform which
     * declarations will be available in the environment to reference.
     * Here, we dont need that info for the first iteration:
     * Instead of a global shared representation, we can inline were referenced.
     * For recursive functions, we can stop inlining which will lead to a lookup in the dictionary for each iteration.
     * However, in some cases (like the adaptive partial application emitted by the Elm compiler),
     * inlining the same expression multiple times can be better.
     * For that specific case, expanding the environment constraint (collected by CA) to enable erasing conditionals might
     * improve overall efficiency a lot.
     * Optimizing for more runtime efficiency follows in later iterations.
     * 
     * (In addition to maybe being easier to implement and read, the inlining will also improve runtime efficiency in many cases.)
     * 
     * */

    public static IReadOnlyList<StackInstruction> InstructionsFromExpressionTransitive(
        Expression rootExpression,
        PineValueClass? envConstraintId,
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining)
    {
        var inlinedStaticInvocations =
            disableReduction || enableTailRecursionOptimization
            ?
            rootExpression
            :
            InlineStaticInvocationsAndReduceRecursive(
                rootExpression,
                inlinedParents: [],
                maxDepth: 6,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: e => skipInlining(e, null));

        var expressionWithEnvConstraint =
            envConstraintId is null || enableTailRecursionOptimization
            ?
            inlinedStaticInvocations
            :
            SubstituteSubexpressionsForEnvironmentConstraint(
                inlinedStaticInvocations,
                envConstraintId: envConstraintId);

        /*
         * Substituting subexpressions for the given environment constraint once at the root should be enough.
         * */

        var reducedExpression =
            disableReduction
            ?
            expressionWithEnvConstraint
            :
            ReduceExpressionAndInlineRecursive(
                currentExpression: expressionWithEnvConstraint,
                inlinedParents: [],
                maxDepth: 7,
                maxSubexpressionCount: 4_000,
                parseCache: parseCache,
                envConstraintId: envConstraintId,
                rootExprForms: [rootExpression],
                disableRecurseAfterInline: false,
                skipInlining: skipInlining);

        var allInstructionsBeforeReturn =
            InstructionsFromExpression(
                rootExpression: reducedExpression,
                rootExprAlternativeForms: [rootExpression],
                envClass: enableTailRecursionOptimization ? envConstraintId : null,
                parametersAsLocals: parametersAsLocals,
                parseCache)
            .ToArray();

        for (var instructionIndex = allInstructionsBeforeReturn.Length - 1; instructionIndex >= 0; instructionIndex--)
        {
            var instruction = allInstructionsBeforeReturn[instructionIndex];

            if (instruction.Kind is StackInstructionKind.Jump_Const)
            {
                var jumpOffset =
                    instruction.JumpOffset
                    ??
                    throw new InvalidOperationException(
                        "Jump instruction without offset: " + instruction);

                var destInstructionIndex =
                    instructionIndex + jumpOffset;

                if (destInstructionIndex >= allInstructionsBeforeReturn.Length)
                {
                    allInstructionsBeforeReturn[instructionIndex] =
                        StackInstruction.Return;

                    continue;
                }

                var destInstruction =
                    allInstructionsBeforeReturn[destInstructionIndex];

                if (destInstruction.Kind is StackInstructionKind.Return)
                {
                    // Remove jump to return instruction.
                    allInstructionsBeforeReturn[instructionIndex] =
                        StackInstruction.Return;
                }
            }
        }

        IReadOnlyList<StackInstruction> allInstructions =
            [.. allInstructionsBeforeReturn,
            StackInstruction.Return
            ];

        return allInstructions;
    }


    public static Expression InlineStaticInvocationsAndReduceRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, bool> skipInlining)
    {
        var expressionReduced =
            ReducePineExpression.ReduceExpressionBottomUp(
                currentExpression,
                parseCache);

        if (maxDepth <= 0)
        {
            return expressionReduced;
        }

        if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
        {
            return expressionReduced;
        }

        Expression? TryInlineParseAndEval(
            Expression.ParseAndEval parseAndEvalExpr)
        {
            if (parseAndEvalExpr.Encoded.ReferencesEnvironment)
            {
                return null;
            }

            if (ReducePineExpression.TryEvaluateExpressionIndependent(
                parseAndEvalExpr.Encoded, parseCache).IsOkOrNull() is not { } exprValue)
            {
                return null;
            }

            if (parseCache.ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)
            {
                return null;
            }

            if (skipInlining(parseOk))
            {
                return null;
            }

            if (inlinedParents.Contains(parseOk))
            {
                return null;
            }

            /*
             * For inlining, translate instances of EnvironmentExpression to the parent environment.
             * */

            var inlinedExpr =
                ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                    findReplacement:
                    descendant =>
                    {
                        if (descendant is Expression.Environment)
                        {
                            return parseAndEvalExpr.Environment;
                        }

                        return null;
                    },
                    parseOk).expr;

            if (disableRecurseAfterInline)
            {
                return inlinedExpr;
            }

            var inlinedExprReduced =
                ReducePineExpression.ReduceExpressionBottomUp(
                    inlinedExpr,
                    parseCache);

            var inlinedFinal =
                InlineStaticInvocationsAndReduceRecursive(
                    currentExpression: inlinedExprReduced,
                    inlinedParents: inlinedParents.Push(parseOk),
                    maxDepth: maxDepth - 1,
                    maxSubexpressionCount: maxSubexpressionCount,
                    parseCache: parseCache,
                    skipInlining: skipInlining,
                    disableRecurseAfterInline: disableRecurseAfterInline);

            return inlinedFinal;
        }

        Expression InlineParseAndEvalRecursive(
            Expression expression,
            bool underConditional)
        {
            return
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
            findReplacement: expr =>
            {
                /*
                 * Do not inline invocations that are still conditional after substituting for the environment constraint.
                 * Inlining these cases can lead to suboptimal overall performance for various reasons.
                 * One reason is that inlining in a generic wrapper causes us to miss an opportunity to select
                 * a more specialized implementation because this selection only happens on invocation.
                 * */

                /*
                 * 2024-07-20 Adaptation, for cases like specializations of `List.map`:
                 * When optimizing `List.map` (or its recursive helper function) (or `List.foldx` for example),
                 * better also inline the application of the generic partial application used with the function parameter.
                 * That application is conditional (list empty?), but we want to inline that to eliminate the generic wrapper for
                 * the function application and inline the parameter function directly.
                 * Thus, the new rule also enables inlining under conditional expressions unless it is recursive.
                 * */

                if (expr is Expression.Conditional conditional)
                {
                    var conditionInlined =
                    InlineParseAndEvalRecursive(
                        conditional.Condition,
                        underConditional: underConditional);

                    var falseBranchInlined =
                    InlineParseAndEvalRecursive(
                        conditional.FalseBranch,
                        underConditional: true);

                    var trueBranchInlined =
                    InlineParseAndEvalRecursive(
                        conditional.TrueBranch,
                        underConditional: true);

                    return Expression.ConditionalInstance(
                        condition: conditionInlined,
                        falseBranch: falseBranchInlined,
                        trueBranch: trueBranchInlined);
                }

                if (expr is Expression.ParseAndEval parseAndEval)
                {
                    if (TryInlineParseAndEval(parseAndEval) is { } inlined)
                    {
                        return inlined;
                    }
                }

                return null;
            },
            expression).expr;
        }

        var expressionInlined =
            InlineParseAndEvalRecursive(
                expressionReduced,
                underConditional: false);

        var expressionInlinedReduced =
            ReducePineExpression.ReduceExpressionBottomUp(expressionInlined, parseCache);

        return expressionInlinedReduced;
    }

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envConstraintId,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining) =>
        ReduceExpressionAndInlineRecursive(
            currentExpression: rootExpression,
            inlinedParents: [],
            envConstraintId: envConstraintId,
            rootExprForms: rootExprAlternativeForms.Add(rootExpression),
            maxDepth: maxDepth,
            maxSubexpressionCount: maxSubexpressionCount,
            parseCache: parseCache,
            disableRecurseAfterInline: disableRecurseAfterInline,
            skipInlining: skipInlining);

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        PineValueClass? envConstraintId,
        ImmutableHashSet<Expression> rootExprForms,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining)
    {
        var expressionSubstituted =
            envConstraintId is null
            ?
            currentExpression
            :
            SubstituteSubexpressionsForEnvironmentConstraint(
                currentExpression,
                envConstraintId);

        var expressionReduced =
            ReducePineExpression.ReduceExpressionBottomUp(expressionSubstituted, parseCache);

        if (maxDepth <= 0)
        {
            return expressionReduced;
        }

        if (envConstraintId is null)
        {
            /*
             * Adapt to observation 2024-07-14:
             * Stopping recursion here if envConstraintId is null resulted in significantly faster
             * completion times in a test compiling all modules of the Elm compiler.
             * */

            /*
             * 2024-11-30: Enable inlining also for cases without environment classes.
             * 
            return expressionReduced;
            */
        }


        /*
         * Install a limit after observing cases with more than a hundred million subexpressions.
         * */

        if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
        {
            return expressionReduced;
        }

        Expression? TryInlineParseAndEval(
            Expression.ParseAndEval parseAndEvalExpr,
            bool noRecursion)
        {
            Expression? ContinueReduceForKnownExprValue(PineValue exprValue)
            {
                if (parseCache.ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)
                {
                    return null;
                }

                if (skipInlining(parseOk, envConstraintId))
                {
                    return null;
                }

                if (noRecursion)
                {
                    if (inlinedParents.Contains(parseOk))
                    {
                        return null;
                    }
                }

                /*
                 * For inlining, translate instances of EnvironmentExpression to the parent environment.
                 * */

                var inlinedExpr =
                    ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                        findReplacement:
                        descendant =>
                        {
                            if (descendant is Expression.Environment)
                            {
                                return parseAndEvalExpr.Environment;
                            }

                            return null;
                        },
                        parseOk).expr;

                if (disableRecurseAfterInline)
                {
                    return inlinedExpr;
                }

                var inlinedExprSubstituted =
                    envConstraintId is null
                    ?
                    inlinedExpr
                    :
                    SubstituteSubexpressionsForEnvironmentConstraint(inlinedExpr, envConstraintId);

                var inlinedExprReduced =
                    ReducePineExpression.ReduceExpressionBottomUp(inlinedExprSubstituted, parseCache);

                {
                    if (500 < inlinedExprReduced.SubexpressionCount)
                    {
                        return null;
                    }

                    var conditionsCount = 0;
                    var invocationsCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedExprReduced))
                    {
                        if (subexpr is Expression.Conditional)
                        {
                            ++conditionsCount;

                            if (5 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEval)
                        {
                            ++invocationsCount;

                            if (5 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                var inlinedFinal =
                    ReduceExpressionAndInlineRecursive(
                        // currentExpression: inlinedExpr,
                        currentExpression: inlinedExprReduced,
                        inlinedParents: inlinedParents.Push(parseOk),
                        rootExprForms: rootExprForms,
                        envConstraintId: envConstraintId,
                        maxDepth: maxDepth - 1,
                        maxSubexpressionCount: maxSubexpressionCount,
                        parseCache: parseCache,
                        skipInlining: skipInlining,
                        disableRecurseAfterInline: disableRecurseAfterInline);

                {
                    if (500 < inlinedFinal.SubexpressionCount)
                    {
                        return null;
                    }

                    var conditionsCount = 0;
                    var invocationsCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedFinal))
                    {
                        if (subexpr is Expression.Conditional)
                        {
                            ++conditionsCount;

                            if (5 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEval)
                        {
                            ++invocationsCount;

                            if (5 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                return inlinedFinal;
            }

            if (!parseAndEvalExpr.Encoded.ReferencesEnvironment)
            {
                if (ReducePineExpression.TryEvaluateExpressionIndependent(
                    parseAndEvalExpr.Encoded, parseCache).IsOkOrNull() is
                    { } evalExprOk)
                {
                    return ContinueReduceForKnownExprValue(evalExprOk);
                }
            }

            return null;
        }

        Expression InlineParseAndEvalRecursive(
            Expression expression,
            bool underConditional)
        {
            return
                ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement: expr =>
                {
                    /*
                     * Do not inline invocations that are still conditional after substituting for the environment constraint.
                     * Inlining these cases can lead to suboptimal overall performance for various reasons.
                     * One reason is that inlining in a generic wrapper causes us to miss an opportunity to select
                     * a more specialized implementation because this selection only happens on invocation.
                     * */

                    /*
                     * 2024-07-20 Adaptation, for cases like specializations of `List.map`:
                     * When optimizing `List.map` (or its recursive helper function) (or `List.foldx` for example),
                     * better also inline the application of the generic partial application used with the function parameter.
                     * That application is conditional (list empty?), but we want to inline that to eliminate the generic wrapper for
                     * the function application and inline the parameter function directly.
                     * Thus, the new rule also enables inlining under conditional expressions unless it is recursive.
                     * */

                    if (expr is Expression.Conditional conditional)
                    {
                        var conditionInlined =
                        InlineParseAndEvalRecursive(
                            conditional.Condition,
                            underConditional: underConditional);

                        var falseBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.FalseBranch,
                            underConditional: true);

                        var trueBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.TrueBranch,
                            underConditional: true);

                        return Expression.ConditionalInstance(
                            condition: conditionInlined,
                            falseBranch: falseBranchInlined,
                            trueBranch: trueBranchInlined);
                    }

                    if (expr is Expression.ParseAndEval parseAndEval)
                    {
                        if (TryInlineParseAndEval(parseAndEval, noRecursion: underConditional) is { } inlined)
                        {
                            return inlined;
                        }
                    }

                    return null;
                },
                expression).expr;
        }

        var expressionInlined =
            InlineParseAndEvalRecursive(
                expressionReduced,
                underConditional: false);

        var expressionInlinedReduced =
            ReducePineExpression.ReduceExpressionBottomUp(expressionInlined, parseCache);

        return expressionInlinedReduced;
    }

    public static Expression SubstituteSubexpressionsForEnvironmentConstraint(
        Expression originalExpression,
        PineValueClass envConstraintId)
    {
        return
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement:
                descendant =>
                {
                    if (Core.CodeAnalysis.CodeAnalysis.TryParseAsLiteral(descendant) is { } literal)
                    {
                        return Expression.LiteralInstance(literal);
                    }

                    if (Core.CodeAnalysis.CodeAnalysis.TryParseExprAsPathInEnv(descendant) is { } pathInEnv)
                    {
                        if (envConstraintId.TryGetValue(pathInEnv) is { } value)
                        {
                            return Expression.LiteralInstance(value);
                        }
                    }

                    return null;
                },
                originalExpression).expr;
    }

    public static IReadOnlyList<StackInstruction> InstructionsFromExpression(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envClass,
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache)
    {
        return
            PineIRCompiler.CompileExpression(
                rootExpression,
                rootExprAlternativeForms: rootExprAlternativeForms,
                envClass,
                parametersAsLocals: parametersAsLocals,
                parseCache)
            .Instructions;
    }

    public static IEnumerable<Expression> ListComponentsOrderedForCompilation(
        Expression rootExpression,
        Func<Expression, bool>? skipDescendants)
    {
        var stack = new Stack<Expression>();
        var deepestDescendants = new Stack<Expression>();

        stack.Push(rootExpression);

        while (stack.TryPop(out var expression))
        {
            if (skipDescendants?.Invoke(expression) ?? false)
            {
                deepestDescendants.Push(expression);
                continue;
            }

            if (expression is Expression.List list)
            {
                for (var i = 0; i < list.Items.Count; ++i)
                {
                    stack.Push(list.Items[i]);
                }
            }

            if (expression is Expression.ParseAndEval parseAndEval)
            {
                stack.Push(parseAndEval.Encoded);
                stack.Push(parseAndEval.Environment);
            }

            if (expression is Expression.KernelApplication kernelApp)
            {
                stack.Push(kernelApp.Input);
            }

            if (expression is Expression.Conditional conditional)
            {
                stack.Push(conditional.Condition);

                /*
                 *
                 * For now, we create a new stack frame for each conditional expression.
                 * Therefore do not descend into the branches of the conditional expression.

                stack.Push(conditional.falseBranch);
                stack.Push(conditional.trueBranch);
                */
            }

            if (expression is Expression.StringTag stringTag)
            {
                stack.Push(stringTag.Tagged);
            }

            deepestDescendants.Push(expression);
        }

        while (deepestDescendants.Count > 0)
        {
            yield return deepestDescendants.Pop();
        }
    }
}

