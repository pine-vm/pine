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
            ImmutableHashSet<Expression> copyToLocal)
        {
            var exprResult = CompileExpressionTransitive(expression, copyToLocal, LocalsSet);

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

    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        ImmutableHashSet<Expression> copyToLocal,
        ImmutableDictionary<Expression, int> localIndexFromExpr)
    {
        return
            CompileExpressionTransitive(
                expression,
                copyToLocal: copyToLocal,
                new NodeCompilationResult(
                    Instructions: [],
                    localIndexFromExpr));
    }

    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        ImmutableHashSet<Expression> copyToLocal,
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
            copyToLocal
            .Union(subexprAppearingMultipleTimesUnconditional)
            .Union(subexprAppearingMultipleTimesIncludingConditional.Intersect(allSubexpressionsUnconditional));

        var lessCSE =
            CompileExpressionTransitiveLessCSE(
                expression,
                copyToLocalNew,
                prior);

        if (!prior.LocalsSet.ContainsKey(expression) && copyToLocal.Contains(expression))
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
                    new StackInstruction(
                        Kind: StackInstructionKind.Local_Set,
                        LocalIndex: newLocalIndex))
                with
                {
                    LocalsSet = lessCSE.LocalsSet.Add(expression, newLocalIndex)
                };
        }

        return lessCSE;
    }

    public static NodeCompilationResult CompileExpressionTransitiveLessCSE(
        Expression expr,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (prior.LocalsSet.TryGetValue(expr, out var localIndex))
        {
            return
                prior
                .AppendInstruction(
                    new StackInstruction(
                        Kind: StackInstructionKind.Local_Get,
                        LocalIndex: localIndex));
        }

        switch (expr)
        {
            case Expression.Literal literalExpr:
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(
                            Kind: StackInstructionKind.Push_Literal,
                            Literal: literalExpr.Value));

            case Expression.Environment:
                return
                    prior
                    .AppendInstruction(
                        new StackInstruction(StackInstructionKind.Push_Environment));

            case Expression.List listExpr:
                {
                    if (listExpr.items.Count is 0)
                    {
                        return
                            prior
                            .AppendInstruction(
                                new StackInstruction(
                                    StackInstructionKind.Push_Literal,
                                    Literal: PineValue.EmptyList));
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
                                copyToLocal,
                                lastItemResult);
                    }

                    return
                        lastItemResult
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.BuildList,
                                TakeCount: listExpr.items.Count));
                }

            case Expression.Conditional conditional:
                return
                    CompileConditional(
                        conditional,
                        copyToLocal,
                        prior);

            case Expression.ParseAndEval pae:
                return
                    CompileParseAndEval(
                        pae,
                        copyToLocal,
                        prior);

            case Expression.KernelApplication kernelApp:
                return
                    CompileKernelApplication(
                        kernelApp,
                        copyToLocal,
                        prior);

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    public static NodeCompilationResult CompileConditional(
        Expression.Conditional conditional,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        var afterCondition =
            CompileExpressionTransitive(
                conditional.Condition,
                copyToLocal,
                prior);

        var falseBranchInstructions =
            CompileExpressionTransitive(
                conditional.FalseBranch,
                copyToLocal,
                new NodeCompilationResult(
                    Instructions: [],
                    LocalsSet: afterCondition.LocalsSet))
            .Instructions;

        var trueBranchInstructions =
            CompileExpressionTransitive(
                conditional.TrueBranch,
                copyToLocal,
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
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        var afterEnvironment =
            prior.ContinueWithExpression(
                parseAndEvalExpr.Environment,
                copyToLocal);

        var afterExpr =
            afterEnvironment.ContinueWithExpression(
                parseAndEvalExpr.Encoded,
                copyToLocal);

        return
            afterExpr
            .AppendInstruction(
                new StackInstruction(StackInstructionKind.Parse_And_Eval));
    }

    public static NodeCompilationResult CompileKernelApplication(
        Expression.KernelApplication kernelApplication,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        return
            kernelApplication.Function switch
            {
                nameof(KernelFunction.length) =>
                CompileExpressionTransitive(
                    kernelApplication.Input,
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Length)),

                nameof(KernelFunction.negate) =>
                CompileKernelApplication_Negate(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.equal) =>
                CompileKernelApplication_Equal(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.head) =>
                CompileKernelApplication_Head(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.skip) =>
                CompileKernelApplication_Skip(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.take) =>
                CompileKernelApplication_Take(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.concat) =>
                CompileKernelApplication_Concat(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.reverse) =>
                CompileKernelApplication_Reverse(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.int_add) =>
                CompileKernelApplication_Int_Add(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.int_mul) =>
                CompileKernelApplication_Int_Mul(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.int_is_sorted_asc) =>
                CompileKernelApplication_Int_Is_Sorted_Asc(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_and) =>
                CompileKernelApplication_Bit_And(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_or) =>
                CompileKernelApplication_Bit_Or(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_xor) =>
                CompileKernelApplication_Bit_Xor(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_not) =>
                CompileKernelApplication_Bit_Not(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_shift_left) =>
                CompileKernelApplication_Bit_Shift_Left(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                nameof(KernelFunction.bit_shift_right) =>
                CompileKernelApplication_Bit_Shift_Right(
                    kernelApplication.Input,
                    copyToLocal,
                    prior),

                _ =>
                throw new NotImplementedException(
                    "Unknown kernel function: " + kernelApplication.Function),
            };
    }

    public static NodeCompilationResult CompileKernelApplication_Equal(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                            copyToLocal,
                            prior);

                    return
                        afterRight
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Equal_Binary_Const,
                                Literal: leftLiteralExpr.Value));
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            copyToLocal,
                            prior);

                    return
                        afterLeft
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Equal_Binary_Const,
                                Literal: rightLiteralExpr.Value));
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.items[0],
                            copyToLocal,
                            prior);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.items[1],
                            copyToLocal,
                            afterLeft);

                    return
                        afterRight
                        .AppendInstruction(new StackInstruction(StackInstructionKind.Equal_Binary_Var));
                }
            }
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Equal_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Head(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.KernelApplication innerKernelApp && innerKernelApp.Function is "skip")
        {
            if (innerKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
            {
                var afterSource =
                    CompileExpressionTransitive(
                        skipList.items[1],
                        copyToLocal,
                        prior);

                if (skipList.items[0] is Expression.Literal literalExpr)
                {
                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(literalExpr.Value).IsOkOrNullable() is { } skipCount)
                    {
                        return
                            afterSource
                            .AppendInstruction(
                                new StackInstruction(
                                    StackInstructionKind.Skip_Head_Const,
                                    SkipCount: (int)skipCount));
                    }
                }

                var afterSkipCount =
                    CompileExpressionTransitive(
                        skipList.items[0],
                        copyToLocal,
                        afterSource);

                return
                    afterSkipCount
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Skip_Head_Var));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Head_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Skip(
       Expression input,
       ImmutableHashSet<Expression> copyToLocal,
       NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var afterSource =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    copyToLocal,
                    prior);

            var afterSkipCount =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    copyToLocal,
                    afterSource);

            return
                afterSkipCount
                .AppendInstruction(new StackInstruction(StackInstructionKind.Skip_Binary));
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Skip_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Take(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var takeCountValueExpr = listExpr.items[0];

            if (listExpr.items[1] is Expression.KernelApplication sourceKernelApp &&
                sourceKernelApp.Function is "skip")
            {
                if (sourceKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
                {
                    var afterSource =
                        CompileExpressionTransitive(
                            skipList.items[1],
                            copyToLocal,
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
                                    copyToLocal,
                                    afterSource);

                            return
                                afterSkipCount
                                .AppendInstruction(
                                    new StackInstruction(
                                        StackInstructionKind.Slice_Skip_Var_Take_Const,
                                        TakeCount: (int)takeCount));
                        }
                    }

                    {
                        var afterSkipCount =
                            CompileExpressionTransitive(
                                skipList.items[0],
                                copyToLocal,
                                afterSource);

                        var afterTakeCount =
                            CompileExpressionTransitive(
                                takeCountValueExpr,
                                copyToLocal,
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
                        copyToLocal,
                        prior);

                var afterTakeCount =
                    CompileExpressionTransitive(
                        takeCountValueExpr,
                        copyToLocal,
                        afterSource);

                return
                    afterTakeCount
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Take_Binary));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Take_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Concat(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr)
        {
            var concatOps = prior;

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        copyToLocal,
                        concatOps);

                concatOps = itemOps;

                if (0 < i)
                {
                    concatOps =
                        concatOps.AppendInstruction(
                            new StackInstruction(StackInstructionKind.Concat_Binary));
                }
            }

            return concatOps;
        }
        else
        {
            return
                CompileExpressionTransitive(
                    input,
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Concat_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Reverse(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Reverse));
    }

    public static NodeCompilationResult CompileKernelApplication_Negate(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.KernelApplication innerKernelApp)
        {
            if (innerKernelApp.Function is "equal" &&
                innerKernelApp.Input is Expression.List equalList && equalList.items.Count is 2)
            {
                if (equalList.items[0] is Expression.Literal leftLiteralExpr)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.items[1],
                            copyToLocal,
                            prior);
                    return
                        afterRight
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Not_Equal_Binary_Const,
                                Literal: leftLiteralExpr.Value));
                }

                if (equalList.items[1] is Expression.Literal rightLiteralExpr)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.items[0],
                            copyToLocal,
                            prior);
                    return
                        afterLeft
                        .AppendInstruction(
                            new StackInstruction(
                                StackInstructionKind.Not_Equal_Binary_Const,
                                Literal: rightLiteralExpr.Value));
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.items[0],
                            copyToLocal,
                            prior);

                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.items[1],
                            copyToLocal,
                            afterLeft);

                    return
                        afterRight
                        .AppendInstruction(new StackInstruction(StackInstructionKind.Not_Equal_Binary_Var));
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
                        copyToLocal,
                        prior);

                var afterRight =
                    CompileExpressionTransitive(
                        isSortedAscList.items[0],
                        copyToLocal,
                        afterLeft);

                return
                    afterRight
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Less_Than_Binary));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Negate));
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Add(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                            copyToLocal,
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
                            copyToLocal,
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
                            copyToLocal,
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
                            copyToLocal,
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
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Add_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Mul(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                            copyToLocal,
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
                            copyToLocal,
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
                        copyToLocal,
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
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Mul_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Is_Sorted_Asc(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                        copyToLocal,
                        prior);

                var afterRight =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        copyToLocal,
                        afterLeft);

                return
                    afterRight
                    .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Less_Than_Or_Equal_Binary));
            }
        }

        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Int_Is_Sorted_Asc_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_And(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                        copyToLocal,
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
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_And_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Or(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                        copyToLocal,
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
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Or_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Xor(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
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
                        copyToLocal,
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
                    copyToLocal,
                    prior)
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Xor_Generic));
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Not(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Not));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Left(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    copyToLocal,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    copyToLocal,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Left_Var));
        }
        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
                prior)
            .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Left_Generic));
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Right(
        Expression input,
        ImmutableHashSet<Expression> copyToLocal,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    copyToLocal,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    copyToLocal,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(new StackInstruction(StackInstructionKind.Bit_Shift_Right_Var));
        }
        return
            CompileExpressionTransitive(
                input,
                copyToLocal,
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
