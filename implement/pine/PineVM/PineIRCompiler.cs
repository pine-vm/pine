using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public class PineIRCompiler
{
    public record NodeCompilationResult(
        ImmutableList<StackInstruction> Instructions,
        ImmutableDictionary<Expression, int> LocalsSet)
    {
        public NodeCompilationResult ContinueWithExpression(
            Expression expression,
            CompilationContext context)
        {
            var exprResult =
                CompileExpressionTransitive(
                    expression,
                    context,
                    LocalsSet);

            return new NodeCompilationResult(
                Instructions.AddRange(exprResult.Instructions),
                LocalsSet.AddRange(exprResult.LocalsSet));
        }

        public NodeCompilationResult AppendInstruction(StackInstruction instruction) =>
            AppendInstructions([instruction]);

        public NodeCompilationResult AppendInstructions(IEnumerable<StackInstruction> instructions) =>
            this
            with
            {
                Instructions = Instructions.AddRange(instructions)
            };
    }

    public record CompilationContext(
        ImmutableHashSet<Expression> CopyToLocal,
        IReadOnlyDictionary<Expression.ParseAndEval, JumpToLoop> TailCallElimination,
        int InstructionOffset)
    {
        public static CompilationContext Init(
            IReadOnlyDictionary<Expression.ParseAndEval, JumpToLoop> tailCallElimination) =>
            new(
                CopyToLocal: [],
                TailCallElimination: tailCallElimination,
                InstructionOffset: 0);

        public CompilationContext AddInstructionOffset(int offset) =>
            this
            with
            {
                InstructionOffset = InstructionOffset + offset
            };
    }

    public record JumpToLoop(
        int DestinationInstructionIndex,
        int EnvironmentLocalIndex);


    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpression(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        EnvConstraintId? envClass,
        PineVMParseCache parseCache)
    {
        /*
         * Tail calls are invocations that are either the root expression of the root of a branch of a conditional that is the root.
         * Since an unconditional invocation makes no sense in practice, we can focus on the invocations in conditional branches.
         * */

        var allTailCalls =
            EnumerateTailCalls(rootExpression)
            .ToImmutableArray();

        /*
         * At the moment, we only optimize cases with direct recursion, not mutual recursion.
         * */

        JumpToLoop? TailCallElimination(Expression.ParseAndEval parseAndEval)
        {
            if (envClass is null)
                return null;

            if (!IsRecursiveCall(
                parseAndEval,
                envClass,
                rootExprForms: rootExprAlternativeForms.Add(rootExpression),
                parseCache))
            {
                return null;
            }

            return new JumpToLoop(
                DestinationInstructionIndex: 3,
                EnvironmentLocalIndex: 0);
        }

        var tailCallEliminationDict =
            allTailCalls
            .SelectWhere(callExpr =>
            Maybe.NothingFromNull(TailCallElimination(callExpr))
            .Map(jump => new KeyValuePair<Expression.ParseAndEval, JumpToLoop>(callExpr, jump)))
            .ToImmutableDictionary();

        var prior =
            tailCallEliminationDict.IsEmpty
            ?
            new NodeCompilationResult(
                Instructions: [],
                ImmutableDictionary<Expression, int>.Empty)
            :
            new NodeCompilationResult(
                Instructions:
                [
                    StackInstruction.Push_Environment,

                    // Use a form storing the environment in a local, so stack depth is zero when we loop:
                    StackInstruction.Local_Set(0),

                    StackInstruction.Pop,
                ],
                ImmutableDictionary<Expression, int>.Empty
                .SetItem(Expression.EnvironmentInstance, 0));

        return
            CompileExpressionTransitive(
                rootExpression,
                context:
                CompilationContext.Init(tailCallEliminationDict),
                prior: prior);
    }

    public static bool IsRecursiveCall(
        Expression.ParseAndEval parseAndEval,
        EnvConstraintId envClass,
        ImmutableHashSet<Expression> rootExprForms,
        PineVMParseCache parseCache)
    {
        if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is not
            ExprMappedToParentEnv.PathInParentEnv encodedPath)
        {
            return false;
        }

        var childPathEnvMap = CodeAnalysis.BuildPathMapFromChildToParentEnv(parseAndEval.Environment);

        if (childPathEnvMap(encodedPath.Path) is not { } encodedPathMapped)
        {
            return false;
        }

        if (!encodedPathMapped.Equals(encodedPath))
        {
            return false;
        }

        if (envClass.TryGetValue(encodedPath.Path) is not { } envClassValueAtPath)
        {
            return false;
        }

        if (parseCache.ParseExpression(envClassValueAtPath).IsOkOrNull() is not { } parsedExpr)
        {
            return false;
        }

        if (rootExprForms.Contains(parsedExpr))
        {
            return true;
        }

        return false;
    }

    public static IEnumerable<Expression.ParseAndEval> EnumerateTailCalls(Expression expression)
    {
        if (expression is Expression.ParseAndEval parseAndEval)
        {
            yield return parseAndEval;
        }

        if (expression is Expression.Conditional conditional)
        {
            foreach (var branch in new[] { conditional.Condition, conditional.TrueBranch, conditional.FalseBranch })
            {
                foreach (var tailCall in EnumerateTailCalls(branch))
                {
                    yield return tailCall;
                }
            }
        }
    }

    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        ImmutableDictionary<Expression, int> localIndexFromExpr)
    {
        return
            CompileExpressionTransitive(
                expression,
                context: context,
                new NodeCompilationResult(
                    Instructions: [],
                    localIndexFromExpr));
    }

    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        var subexprAppearingMultipleTimesIncludingConditional = new HashSet<Expression>();

        {
            var allSubexpressions = new HashSet<Expression>();

            foreach (
                var subexpression in
                Expression.EnumerateSelfAndDescendants(
                    expression,
                    skipDescendants:
                    subexpression =>
                    {
                        if (!ExpressionLargeEnoughForCSE(subexpression))
                        {
                            return true;
                        }

                        if (prior.LocalsSet.ContainsKey(subexpression))
                        {
                            return true;
                        }

                        if (subexprAppearingMultipleTimesIncludingConditional.Contains(subexpression))
                        {
                            return true;
                        }

                        if (allSubexpressions.Contains(subexpression))
                        {
                            subexprAppearingMultipleTimesIncludingConditional.Add(subexpression);

                            return true;
                        }

                        allSubexpressions.Add(subexpression);

                        return false;
                    },
                    skipConditionalBranches: false))
            {
            }
        }

        var allSubexpressionsUnconditional = new HashSet<Expression>();

        var subexprAppearingMultipleTimesUnconditional = new HashSet<Expression>();

        {
            foreach (
                var subexpression in
                Expression.EnumerateSelfAndDescendants(
                    expression,
                    skipDescendants:
                    subexpression =>
                    {
                        if (!ExpressionLargeEnoughForCSE(subexpression))
                        {
                            return true;
                        }

                        if (prior.LocalsSet.ContainsKey(subexpression))
                        {
                            return true;
                        }

                        if (subexprAppearingMultipleTimesUnconditional.Contains(subexpression))
                        {
                            return true;
                        }

                        if (allSubexpressionsUnconditional.Contains(subexpression))
                        {
                            subexprAppearingMultipleTimesUnconditional.Add(subexpression);

                            return true;
                        }

                        allSubexpressionsUnconditional.Add(subexpression);

                        return false;
                    },
                    skipConditionalBranches: true))
            {
            }
        }

        var copyToLocalNew =
            context.CopyToLocal
            .Union(subexprAppearingMultipleTimesUnconditional)
            .Union(subexprAppearingMultipleTimesIncludingConditional.Intersect(allSubexpressionsUnconditional));

        var lessCSE =
            CompileExpressionTransitiveLessCSE(
                expression,
                context
                with
                {
                    CopyToLocal = copyToLocalNew
                },
                prior);

        if (!prior.LocalsSet.ContainsKey(expression) && context.CopyToLocal.Contains(expression))
        {
            var newLocalIndex =
                lessCSE.LocalsSet.IsEmpty
                ?
                0
                :
                lessCSE.LocalsSet.Values.Max() + 1;

            return
                lessCSE
                .AppendInstruction(
                    StackInstruction.Local_Set(newLocalIndex))
                with
                {
                    LocalsSet = lessCSE.LocalsSet.Add(expression, newLocalIndex)
                };
        }

        return lessCSE;
    }

    public static NodeCompilationResult CompileExpressionTransitiveLessCSE(
        Expression expr,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (prior.LocalsSet.TryGetValue(expr, out var localIndex))
        {
            return
                prior
                .AppendInstruction(
                    StackInstruction.Local_Get(localIndex));
        }

        switch (expr)
        {
            case Expression.Literal literalExpr:
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(literalExpr.Value));

            case Expression.Environment:
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Environment);

            case Expression.List listExpr:
                {
                    if (listExpr.items.Count is 0)
                    {
                        return
                            prior
                            .AppendInstruction(
                                StackInstruction.Push_Literal(PineValue.EmptyList));
                    }

                    /*
                     * Assume that the subsequence for each item only leaves one value on the stack.
                     * 
                     * (When we need to reuse a value from a subexpression multiple times,
                     * we don't use a non-consuming instruction but use local_get instead to copy it)
                     * */

                    var lastItemResult = prior;

                    for (var i = 0; i < listExpr.items.Count; ++i)
                    {
                        var itemExpr = listExpr.items[i];

                        lastItemResult =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                lastItemResult);
                    }

                    return
                        lastItemResult
                        .AppendInstruction(
                            StackInstruction.Build_List(listExpr.items.Count));
                }

            case Expression.Conditional conditional:
                return
                    CompileConditional(
                        conditional,
                        context,
                        prior);

            case Expression.ParseAndEval pae:
                return
                    CompileParseAndEval(
                        pae,
                        context,
                        prior);

            case Expression.KernelApplication kernelApp:
                return
                    CompileKernelApplication(
                        kernelApp,
                        context,
                        prior);

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    public static NodeCompilationResult CompileConditional(
        Expression.Conditional conditional,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        var afterCondition =
            CompileExpressionTransitive(
                conditional.Condition,
                context,
                prior);

        var falseBranchInstructions =
            CompileExpressionTransitive(
                conditional.FalseBranch,
                context
                .AddInstructionOffset(afterCondition.Instructions.Count + 1),
                new NodeCompilationResult(
                    Instructions: [],
                    LocalsSet: afterCondition.LocalsSet))
            .Instructions;

        var trueBranchInstructions =
            CompileExpressionTransitive(
                conditional.TrueBranch,
                context
                .AddInstructionOffset(afterCondition.Instructions.Count + 1)
                .AddInstructionOffset(falseBranchInstructions.Count + 1),
                new NodeCompilationResult(
                    Instructions: [],
                    LocalsSet: afterCondition.LocalsSet))
            .Instructions;

        IReadOnlyList<StackInstruction> falseBranchInstructionsAndJump =
            [.. falseBranchInstructions,
                StackInstruction.Jump_Unconditional(trueBranchInstructions.Count + 1)
            ];

        var branchInstruction =
            StackInstruction.Jump_If_True(
                offset: falseBranchInstructionsAndJump.Count);

        var afterConditionAndJump =
            afterCondition
            .AppendInstruction(branchInstruction);

        return
            afterConditionAndJump
            .AppendInstructions(falseBranchInstructionsAndJump)
            .AppendInstructions(trueBranchInstructions);
    }

    public static NodeCompilationResult CompileParseAndEval(
        Expression.ParseAndEval parseAndEvalExpr,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (context.TailCallElimination.TryGetValue(parseAndEvalExpr, out var jumpToLoop))
        {
            var afterEnvironment =
                prior.ContinueWithExpression(
                    parseAndEvalExpr.Environment,
                    context);

            var jumpOffset =
                jumpToLoop.DestinationInstructionIndex - afterEnvironment.Instructions.Count -
                context.InstructionOffset - 2;

            return
                afterEnvironment
                .AppendInstructions(
                    [
                    StackInstruction.Local_Set(jumpToLoop.EnvironmentLocalIndex),

                    StackInstruction.Pop,

                    StackInstruction.Jump_Unconditional(jumpOffset)
                    ]);
        }

        {
            var afterEnvironment =
                prior.ContinueWithExpression(
                    parseAndEvalExpr.Environment,
                    context);

            var afterExpr =
                afterEnvironment.ContinueWithExpression(
                    parseAndEvalExpr.Encoded,
                    context);

            return
                afterExpr
                .AppendInstruction(
                    StackInstruction.Parse_And_Eval_Binary);
        }
    }

    public static NodeCompilationResult CompileKernelApplication(
        Expression.KernelApplication kernelApplication,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        return
            kernelApplication.Function switch
            {
                nameof(KernelFunction.length) =>
                CompileExpressionTransitive(
                    kernelApplication.Input,
                    context,
                    prior)
                .AppendInstruction(StackInstruction.Length),

                nameof(KernelFunction.negate) =>
                CompileKernelApplication_Negate(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.equal) =>
                CompileKernelApplication_Equal(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.head) =>
                CompileKernelApplication_Head(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.skip) =>
                CompileKernelApplication_Skip(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.take) =>
                CompileKernelApplication_Take(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.concat) =>
                CompileKernelApplication_Concat(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.reverse) =>
                CompileKernelApplication_Reverse(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.int_add) =>
                CompileKernelApplication_Int_Add(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.int_mul) =>
                CompileKernelApplication_Int_Mul(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.int_is_sorted_asc) =>
                CompileKernelApplication_Int_Is_Sorted_Asc(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_and) =>
                CompileKernelApplication_Bit_And(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_or) =>
                CompileKernelApplication_Bit_Or(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_xor) =>
                CompileKernelApplication_Bit_Xor(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_not) =>
                CompileKernelApplication_Bit_Not(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_shift_left) =>
                CompileKernelApplication_Bit_Shift_Left(
                    kernelApplication.Input,
                    context,
                    prior),

                nameof(KernelFunction.bit_shift_right) =>
                CompileKernelApplication_Bit_Shift_Right(
                    kernelApplication.Input,
                    context,
                    prior),

                _ =>
                throw new NotImplementedException(
                    "Unknown kernel function: " + kernelApplication.Function),
            };
    }

    public static NodeCompilationResult CompileKernelApplication_Equal(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            context,
                            prior);

                    return
                        afterRight
                        .AppendInstruction(
                            StackInstruction.Equal_Binary_Const(leftLiteralExpr.Value));
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            context,
                            prior);

                    return
                        afterLeft
                        .AppendInstruction(
                            StackInstruction.Equal_Binary_Const(rightLiteralExpr.Value));
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            context,
                            prior);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            context,
                            afterLeft);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Equal_Binary);
                }
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Equal_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Head(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.KernelApplication innerKernelApp && innerKernelApp.Function is "skip")
        {
            if (innerKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
            {
                var afterSource =
                    CompileExpressionTransitive(
                        skipList.items[1],
                        context,
                        prior);

                if (skipList.items[0] is Expression.Literal literalExpr)
                {
                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(literalExpr.Value).IsOkOrNullable() is { } skipCount)
                    {
                        return
                            afterSource
                            .AppendInstruction(
                                StackInstruction.Skip_Head_Const((int)skipCount));
                    }
                }

                var afterSkipCount =
                    CompileExpressionTransitive(
                        skipList.items[0],
                        context,
                        afterSource);

                return
                    afterSkipCount
                    .AppendInstruction(StackInstruction.Skip_Head_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Head_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Skip(
       Expression input,
       CompilationContext context,
       NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var afterSource =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    context,
                    prior);

            var afterSkipCount =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    context,
                    afterSource);

            return
                afterSkipCount
                .AppendInstruction(StackInstruction.Skip_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Skip_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Take(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var takeCountValueExpr = listExpr.items[0];

            if (listExpr.items[1] is Expression.KernelApplication sourceKernelApp &&
                sourceKernelApp.Function is nameof(KernelFunction.skip))
            {
                if (sourceKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
                {
                    var afterSource =
                        CompileExpressionTransitive(
                            skipList.items[1],
                            context,
                            prior);

                    /*
                     * Earlier reduction pass should already have reduced the contents in take to
                     * literal at this point, if at all possible, so don't reach for more general eval here.
                     */
                    if (takeCountValueExpr is Expression.Literal takeCountLiteral)
                    {
                        if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountLiteral.Value) is { } takeCount)
                        {
                            var afterSkipCount =
                                CompileExpressionTransitive(
                                    skipList.items[0],
                                    context,
                                    afterSource);

                            return
                                afterSkipCount
                                .AppendInstruction(
                                    StackInstruction.Slice_Skip_Var_Take_Const((int)takeCount));
                        }
                    }

                    {
                        var afterSkipCount =
                            CompileExpressionTransitive(
                                skipList.items[0],
                                context,
                                afterSource);

                        var afterTakeCount =
                            CompileExpressionTransitive(
                                takeCountValueExpr,
                                context,
                                afterSkipCount);

                        return
                            afterTakeCount
                            .AppendInstruction(
                                new StackInstruction(
                                    StackInstructionKind.Slice_Skip_Var_Take_Var));
                    }
                }
            }

            {
                var afterSource =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        context,
                        prior);

                var afterTakeCount =
                    CompileExpressionTransitive(
                        takeCountValueExpr,
                        context,
                        afterSource);

                return
                    afterTakeCount
                    .AppendInstruction(StackInstruction.Take_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Take_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Concat(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.List leftListExpr && leftListExpr.items.Count is 1)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            leftListExpr.items[0],
                            context,
                            prior);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            context,
                            afterLeft);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Prepend_List_Item_Binary);
                }
            }

            var concatOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        context,
                        concatOps);

                concatOps = itemOps;

                if (0 < i)
                {
                    concatOps =
                        concatOps.AppendInstruction(
                            StackInstruction.Concat_Binary);
                }
            }

            return concatOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(StackInstruction.Concat_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Reverse(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Reverse);
    }

    public static NodeCompilationResult CompileKernelApplication_Negate(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.KernelApplication innerKernelApp)
        {
            if (innerKernelApp.Function is nameof(KernelFunction.equal) &&
                innerKernelApp.Input is Expression.List equalList && equalList.items.Count is 2)
            {
                if (equalList.items[0] is Expression.Literal leftLiteralExpr)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.items[1],
                            context,
                            prior);
                    return
                        afterRight
                        .AppendInstruction(
                            StackInstruction.Not_Equal_Binary_Const(leftLiteralExpr.Value));
                }

                if (equalList.items[1] is Expression.Literal rightLiteralExpr)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.items[0],
                            context,
                            prior);
                    return
                        afterLeft
                        .AppendInstruction(
                            StackInstruction.Not_Equal_Binary_Const(rightLiteralExpr.Value));
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.items[0],
                            context,
                            prior);

                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.items[1],
                            context,
                            afterLeft);

                    return
                        afterRight
                        .AppendInstruction(new StackInstruction(StackInstructionKind.Not_Equal_Binary));
                }
            }

            if (innerKernelApp.Function is "int_is_sorted_asc" &&
                innerKernelApp.Input is Expression.List isSortedAscList && isSortedAscList.items.Count is 2)
            {
                /*
                 * not (int_is_sorted_asc [a, b]) = b <= a
                 * */

                var afterLeft =
                    CompileExpressionTransitive(
                        isSortedAscList.items[1],
                        context,
                        prior);

                var afterRight =
                    CompileExpressionTransitive(
                        isSortedAscList.items[0],
                        context,
                        afterLeft);

                return
                    afterRight
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Less_Than_Binary));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Negate));
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Add(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(0)));
            }

            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            context,
                            prior);

                    return
                        afterRight
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Int_Add_Const,
                                IntegerLiteral: leftInt));
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            context,
                            prior);
                    return
                        afterLeft
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Int_Add_Const,
                                IntegerLiteral: rightInt));
                }
            }

            var addOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                if (0 < i && TryParseExprAsIntNegation(itemExpr) is { } negatedItemExpr)
                {
                    var itemOps =
                        CompileExpressionTransitive(
                            negatedItemExpr,
                            context,
                            addOps);

                    addOps = itemOps;

                    if (0 < i)
                    {
                        addOps =
                            addOps.AppendInstruction(
                                new StackInstruction(StackInstructionKind.Int_Sub_Binary));
                    }
                }
                else
                {
                    var itemOps =
                        CompileExpressionTransitive(
                            itemExpr,
                            context,
                            addOps);

                    addOps = itemOps;

                    if (0 < i)
                    {
                        addOps =
                            addOps.AppendInstruction(
                                new StackInstruction(StackInstructionKind.Int_Add_Binary));
                    }
                }
            }

            return addOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Add_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Mul(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(1)));
            }

            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            context,
                            prior);

                    return
                        afterRight
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Int_Mul_Const,
                                IntegerLiteral: leftInt));
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            context,
                            prior);

                    return
                        afterLeft
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Int_Mul_Const,
                                IntegerLiteral: rightInt));
                }
            }

            var mulOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        mulOps);

                mulOps = itemOps;

                if (0 < i)
                {
                    mulOps =
                        mulOps.AppendInstruction(
                            new StackInstruction(StackInstructionKind.Int_Mul_Binary));
                }
            }

            return mulOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Mul_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Is_Sorted_Asc(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0 || listExpr.items.Count is 1)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineVMValues.TrueValue));
            }

            if (listExpr.items.Count is 2)
            {
                var afterLeft =
                    CompileExpressionTransitive(
                        listExpr.items[0],
                        context,
                        prior);

                var afterRight =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        context,
                        afterLeft);

                return
                    afterRight
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Less_Than_Or_Equal_Binary));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Is_Sorted_Asc_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_And(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob));
            }

            var andOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        andOps);

                andOps = itemOps;

                if (0 < i)
                {
                    andOps =
                        andOps.AppendInstruction(
                            new StackInstruction(StackInstructionKind.Bit_And_Binary));
                }
            }

            return andOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_And_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Or(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob));
            }

            var orOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        orOps);

                orOps = itemOps;

                if (0 < i)
                {
                    orOps =
                        orOps.AppendInstruction(
                            new StackInstruction(StackInstructionKind.Bit_Or_Binary));
                }
            }

            return orOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Or_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Xor(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob));
            }

            var xorOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        xorOps);

                xorOps = itemOps;

                if (0 < i)
                {
                    xorOps =
                        xorOps.AppendInstruction(
                            new StackInstruction(StackInstructionKind.Bit_Xor_Binary));
                }
            }

            return xorOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Xor_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Not(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Not));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Left(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    context,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    context,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Left_Binary));
        }
        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Left_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Right(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    context,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    context,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Right_Binary));
        }
        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Right_Generic));
    }

    public static Expression? TryParseExprAsIntNegation(Expression expression)
    {
        if (expression is not Expression.KernelApplication kernelApp)
        {
            return null;
        }

        if (kernelApp.Function is "negate")
        {
            return kernelApp.Input;
        }

        if (kernelApp.Function is "int_mul" &&
            kernelApp.Input is Expression.List mulList && mulList.items.Count is 2)
        {
            if (mulList.items[0] is Expression.Literal literalExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } literalValue &&
                literalValue == -1)
            {
                return mulList.items[1];
            }

            if (mulList.items[1] is Expression.Literal literalExpr2 &&
                KernelFunction.SignedIntegerFromValueRelaxed(literalExpr2.Value) is { } literalValue2 &&
                literalValue2 == -1)
            {
                return mulList.items[0];
            }
        }

        return null;
    }

    public static bool ExpressionLargeEnoughForCSE(Expression expression)
    {
        if (expression is Expression.Literal || expression is Expression.Environment)
            return false;

        if (expression is Expression.KernelApplication)
            return true;

        if (expression is Expression.ParseAndEval)
            return true;

        if (expression is Expression.List list)
        {
            for (int i = 0; i < list.items.Count; ++i)
            {
                if (ExpressionLargeEnoughForCSE(list.items[i]))
                    return true;
            }
        }

        if (expression is Expression.StringTag stringTag)
        {
            return ExpressionLargeEnoughForCSE(stringTag.Tagged);
        }

        return 3 < expression.SubexpressionCount;
    }
}