using Pine.Core;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

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
        if (KernelApplication_Equal_TryParseAs_Starting_With(input) is { } startingWith)
        {
            var afterExpr =
                CompileExpressionTransitive(
                    startingWith.expr,
                    context,
                    prior);
            return
                afterExpr
                .AppendInstruction(
                    StackInstruction.Starts_With_Const(startingWith.start));
        }

        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr)
                {
                    if (IntegerEncoding.ParseSignedIntegerRelaxed(leftLiteralExpr.Value).IsOkOrNullable() is { } leftInteger &&
                        listExpr.items[1] is Expression.KernelApplication rightKernelApp &&
                        rightKernelApp.Function is nameof(KernelFunction.length))
                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightKernelApp.Input,
                                context,
                                prior);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Length_Equal_Const(leftInteger));
                    }

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
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr)
                {
                    if (IntegerEncoding.ParseSignedIntegerRelaxed(rightLiteralExpr.Value).IsOkOrNullable() is { } rightInteger &&
                        listExpr.items[0] is Expression.KernelApplication leftKernelApp &&
                        leftKernelApp.Function is nameof(KernelFunction.length))
                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftKernelApp.Input,
                                context,
                                prior);
                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Length_Equal_Const(rightInteger));
                    }

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

    private static (Expression expr, PineValue start)? KernelApplication_Equal_TryParseAs_Starting_With(Expression input)
    {
        if (input is not Expression.List inputList)
        {
            return null;
        }

        if (inputList.items.Count is not 2)
        {
            return null;
        }

        {
            if (TryEvaluateExpressionIndependent(inputList.items[0]) is { } leftValue &&
                inputList.items[1] is Expression.KernelApplication rightKernelApp &&
                rightKernelApp.Function is nameof(KernelFunction.take) &&
                rightKernelApp.Input is Expression.List rightInputList &&
                rightInputList.items.Count is 2 &&
                TryParseExprAsIndependentSignedIntegerRelaxed(rightInputList.items[0]) is { } takeCount)
            {
                if (leftValue is PineValue.BlobValue leftBlob &&
                    leftBlob.Bytes.Length == takeCount)
                {
                    return (rightInputList.items[1], leftBlob);
                }
            }
        }

        {
            if (inputList.items[0] is Expression.KernelApplication leftKernelApp &&
                leftKernelApp.Function is nameof(KernelFunction.take) &&
                leftKernelApp.Input is Expression.List leftInputList &&
                leftInputList.items.Count is 2 &&
                TryParseExprAsIndependentSignedIntegerRelaxed(leftInputList.items[0]) is { } takeCount &&
                    TryEvaluateExpressionIndependent(inputList.items[1]) is { } rightValue)
            {
                if (rightValue is PineValue.BlobValue rightBlob &&
                    rightBlob.Bytes.Length == takeCount)
                {
                    return (leftInputList.items[1], rightBlob);
                }
            }
        }

        return null;
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
                    if (IntegerEncoding.ParseSignedIntegerRelaxed(literalExpr.Value).IsOkOrNullable() is { } skipCount)
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
            var skipCountExpr = listExpr.items[0];

            var skipSourceExpr = listExpr.items[1];

            var afterSource =
                CompileExpressionTransitive(
                    skipSourceExpr,
                    context,
                    prior);

            if (TryParseExprAsIndependentSignedIntegerRelaxed(skipCountExpr) is { } skipCount)
            {
                if (skipCount.IsOne &&
                    skipSourceExpr is Expression.KernelApplication skipSourceKernelApp &&
                    skipSourceKernelApp.Function is nameof(KernelFunction.int_add) &&
                    skipSourceKernelApp.Input is Expression.List skipSourceAddInputList &&
                    skipSourceAddInputList.items.Count is 2)
                {
                    NodeCompilationResult? ContinueForAddZeroOperand(Expression addZeroOperand)
                    {
                        if (addZeroOperand is Expression.KernelApplication addOperandKernelApp &&
                            addOperandKernelApp.Function is nameof(KernelFunction.concat) &&
                            addOperandKernelApp.Input is Expression.List concatList &&
                            concatList.items.Count is 2 &&
                            TryEvaluateExpressionIndependent(concatList.items[0]) is { } prependValue &&
                            prependValue is PineValue.BlobValue prependBlob &&
                            prependBlob.Bytes.Length is 1 &&
                            prependBlob.Bytes.Span[0] is 2 or 4)
                        {
                            var afterSource =
                                CompileExpressionTransitive(
                                    concatList.items[1],
                                    context,
                                    prior);

                            return
                                afterSource
                                .AppendInstruction(
                                    StackInstruction.Blob_Trim_Leading_Zeros(minRemainingCount: 1));
                        }

                        return null;
                    }

                    {
                        if (TryParseExprAsIndependentSignedIntegerRelaxed(skipSourceAddInputList.items[0]) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(skipSourceAddInputList.items[1]) is { } specialized)
                        {
                            return specialized;
                        }
                    }

                    {
                        if (TryParseExprAsIndependentSignedIntegerRelaxed(skipSourceAddInputList.items[1]) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(skipSourceAddInputList.items[0]) is { } specialized)
                        {
                            return specialized;
                        }
                    }
                }

                return
                    afterSource
                    .AppendInstruction(
                        StackInstruction.Skip_Const((int)skipCount));
            }

            {
                var afterSkipCount =
                    CompileExpressionTransitive(
                        skipCountExpr,
                        context,
                        afterSource);

                return
                    afterSkipCount
                    .AppendInstruction(StackInstruction.Skip_Binary);
            }
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
                                StackInstruction.Slice_Skip_Var_Take_Var);
                    }
                }
            }

            {
                var afterSource =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        context,
                        prior);

                if (takeCountValueExpr is Expression.Literal takeCountLiteralExpr)
                {
                    if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountLiteralExpr.Value) is { } takeCount)
                    {
                        return
                            afterSource
                            .AppendInstruction(
                                StackInstruction.Take_Const((int)takeCount));
                    }
                }

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
        if (input is Expression.KernelApplication inputKernelApp &&
            inputKernelApp.Function is nameof(KernelFunction.take) &&
            inputKernelApp.Input is Expression.List takeInputList &&
            takeInputList.items.Count is 2 &&
            takeInputList.items[1] is Expression.KernelApplication innerKernelApp &&
            innerKernelApp.Function is nameof(KernelFunction.reverse))
        {
            var takeCountValue = takeInputList.items[0];

            if (!takeCountValue.ReferencesEnvironment)
            {
                if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(
                    takeCountValue).IsOkOrNull() is { } takeCountValueEvaluated)
                {
                    if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValueEvaluated) is { } takeCount)
                    {
                        var afterSource =
                            CompileExpressionTransitive(
                                innerKernelApp.Input,
                                context,
                                prior);

                        return
                            afterSource
                            .AppendInstruction(
                                StackInstruction.Take_Last_Const((int)takeCount));
                    }
                }
            }
        }

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
                        .AppendInstruction(StackInstruction.Not_Equal_Binary);
                }
            }

            if (innerKernelApp.Function is nameof(KernelFunction.int_is_sorted_asc) &&
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
                    .AppendInstruction(StackInstruction.Int_Less_Than_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Negate);
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
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(0)));
            }

            BigInteger constItemsSum = 0;
            var varItemsAdd = new List<Expression>();
            var varItemsSubtract = new List<Expression>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    if (KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is not { } intValue)
                    {
                        return
                            prior
                            .AppendInstruction(
                                StackInstruction.Push_Literal(PineValue.EmptyList));
                    }

                    constItemsSum += intValue;
                }
                else
                {
                    if (IsIntNegated(itemExpr) is { } subtractedExpr)
                    {
                        varItemsSubtract.Add(subtractedExpr);
                    }
                    else
                    {
                        varItemsAdd.Add(itemExpr);
                    }
                }
            }

            if (varItemsAdd.Count is 0)
            {
                var addOps =
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(constItemsSum)));

                for (var i = 0; i < varItemsSubtract.Count; ++i)
                {
                    var itemExpr = varItemsSubtract[i];

                    var itemOps =
                        CompileExpressionTransitive(
                            itemExpr,
                            context,
                            addOps);

                    addOps = itemOps;

                    addOps =
                        addOps.AppendInstruction(
                            StackInstruction.Int_Sub_Binary);
                }
            }
            else
            {
                var firstVarItemAdd = varItemsAdd[0];

                if (varItemsAdd.Count is 1 && varItemsSubtract.Count is 0)
                {
                    if (IsIntUnsigned(firstVarItemAdd) is { } unsignedExpr)
                    {
                        var addOps =
                            CompileExpressionTransitive(
                                unsignedExpr,
                                context,
                                prior);

                        return
                            addOps
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Add_Const(constItemsSum));
                    }
                }

                {
                    var addOps =
                        CompileExpressionTransitive(
                            firstVarItemAdd,
                            context,
                            prior);

                    for (var i = 1; i < varItemsAdd.Count; ++i)
                    {
                        var itemExpr = varItemsAdd[i];

                        var itemOps =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                addOps);

                        addOps = itemOps;

                        addOps =
                            addOps.AppendInstruction(
                                StackInstruction.Int_Add_Binary);
                    }

                    for (var i = 0; i < varItemsSubtract.Count; ++i)
                    {
                        var itemExpr = varItemsSubtract[i];

                        var itemOps =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                addOps);

                        addOps = itemOps;

                        addOps =
                            addOps.AppendInstruction(
                                StackInstruction.Int_Sub_Binary);
                    }

                    if (constItemsSum != 0 || varItemsAdd.Count + varItemsSubtract.Count < 2)
                    {
                        /*
                         * Using the 'add' function returns canonically encoded integer,
                         * so adding '0' does not always return the same value.
                         * */

                        addOps =
                            addOps
                            .AppendInstruction(
                                StackInstruction.Int_Add_Const(constItemsSum));
                    }

                    return addOps;
                }
            }
        }

        {
            return
                CompileExpressionTransitive(
                    input,
                    context,
                    prior)
                .AppendInstruction(StackInstruction.Int_Add_Generic);
        }
    }

    private static Expression? IsIntUnsigned(Expression expr)
    {
        if (expr is Expression.KernelApplication kernelApp &&
            kernelApp.Function is nameof(KernelFunction.concat) &&
            kernelApp.Input is Expression.List inputList &&
            inputList.items.Count is 2)
        {
            var prependedExpr = inputList.items[0];

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(
                    prependedExpr).IsOkOrNull() is not
                    { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 4)
                {
                    return inputList.items[1];
                }
            }
        }

        return null;
    }

    private static Expression? IsIntUnsignedNegated(Expression expr)
    {
        if (expr is Expression.KernelApplication kernelApp &&
            kernelApp.Function is nameof(KernelFunction.concat) &&
            kernelApp.Input is Expression.List inputList &&
            inputList.items.Count is 2)
        {
            var prependedExpr = inputList.items[0];

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(
                    prependedExpr).IsOkOrNull() is not
                    { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 2)
                {
                    return inputList.items[1];
                }
            }
        }

        return null;
    }

    private static Expression? IsIntNegated(Expression expr)
    {
        if (expr is Expression.KernelApplication kernelApp)
        {
            if (kernelApp.Function is nameof(KernelFunction.negate))
            {
                return kernelApp.Input;
            }


            if (kernelApp.Function is nameof(KernelFunction.int_mul) &&
               kernelApp.Input is Expression.List mulListExpr && mulListExpr.items.Count is 2)
            {
                {
                    if (mulListExpr.items[0] is Expression.Literal literalExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } leftInt &&
                        leftInt == -1)
                    {
                        return mulListExpr.items[1];
                    }
                }

                {
                    if (mulListExpr.items[1] is Expression.Literal literalExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } rightInt &&
                        rightInt == -1)
                    {
                        return mulListExpr.items[0];
                    }
                }
            }
        }

        return null;
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
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(1)));
            }

            BigInteger constItemsProduct = 1;
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    if (KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is not { } intValue)
                    {
                        return
                            prior
                            .AppendInstruction(
                                StackInstruction.Push_Literal(PineValue.EmptyList));
                    }

                    constItemsProduct *= intValue;
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            if (varItems.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(constItemsProduct)));
            }

            var firstVarItem = varItems[0];

            var mulOps =
                CompileExpressionTransitive(
                    firstVarItem,
                    context,
                    prior);

            for (var i = 1; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        mulOps);

                mulOps = itemOps;

                mulOps =
                    mulOps.AppendInstruction(
                        StackInstruction.Int_Mul_Binary);
            }

            if (constItemsProduct != 1 || varItems.Count < 2)
            {
                mulOps =
                    mulOps
                    .AppendInstruction(
                        StackInstruction.Int_Mul_Const(constItemsProduct));
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
                .AppendInstruction(StackInstruction.Int_Mul_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Is_Sorted_Asc(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (Int_Is_Sorted_Asc_TryParseIsIntegerSign(input) is { } isIntegerWithSign)
        {
            return
                CompileExpressionTransitive(
                    isIntegerWithSign.expr,
                    context,
                    prior)
                .AppendInstruction(
                    StackInstruction.Starts_With_Const(PineValue.BlobSingleByte(isIntegerWithSign.sign)));
        }

        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0 || listExpr.items.Count is 1)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineVMValues.TrueValue));
            }

            if (listExpr.items.Count is 2)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    var rightExpr = listExpr.items[1];

                    if (IsIntUnsigned(rightExpr) is { } rightUnsigned)
                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightUnsigned,
                                context,
                                prior);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(leftInt));
                    }

                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightExpr,
                                context,
                                prior);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Int_Greater_Than_Or_Equal_Const(leftInt));
                    }
                }

                if (listExpr.items[1] is Expression.Literal rightLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                {
                    var leftExpr = listExpr.items[0];

                    if (IsIntUnsigned(leftExpr) is { } leftUnsigned)
                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftUnsigned,
                                context,
                                prior);

                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(rightInt));
                    }

                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftExpr,
                                context,
                                prior);

                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Int_Less_Than_Or_Equal_Const(rightInt));
                    }
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
                        .AppendInstruction(StackInstruction.Int_Less_Than_Or_Equal_Binary);
                }
            }

            if (listExpr.items.Count is 3)
            {
                if (listExpr.items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    if (listExpr.items[2] is Expression.Literal rightLiteralExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                    {
                        var middleExpr = listExpr.items[1];

                        if (IsIntUnsigned(middleExpr) is { } middleUnsigned)
                        {
                            var contextSettingLocal =
                                context
                                with
                                {
                                    CopyToLocal =
                                        context.CopyToLocal.Add(middleUnsigned)
                                };

                            var afterMiddle =
                                CompileExpressionTransitive(
                                    middleUnsigned,
                                    contextSettingLocal,
                                    prior);

                            return
                                afterMiddle
                                .AppendInstruction(
                                    StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(leftInt))
                                .AppendInstruction(
                                    StackInstruction.Local_Get(afterMiddle.LocalsSet[middleUnsigned]))
                                .AppendInstruction(
                                    StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(rightInt))
                                .AppendInstruction(
                                    StackInstruction.Logical_And_Binary);
                        }

                        {
                            var contextSettingLocal =
                                context
                                with
                                {
                                    CopyToLocal =
                                        context.CopyToLocal.Add(middleExpr)
                                };

                            var afterMiddle =
                                CompileExpressionTransitive(
                                    middleExpr,
                                    contextSettingLocal,
                                    prior);

                            return
                                afterMiddle
                                .AppendInstruction(
                                    StackInstruction.Int_Greater_Than_Or_Equal_Const(leftInt))
                                .AppendInstruction(
                                    StackInstruction.Local_Get(afterMiddle.LocalsSet[middleExpr]))
                                .AppendInstruction(
                                    StackInstruction.Int_Less_Than_Or_Equal_Const(rightInt))
                                .AppendInstruction(
                                    StackInstruction.Logical_And_Binary);
                        }
                    }
                }
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Int_Is_Sorted_Asc_Generic);
    }

    private static (Expression expr, byte sign)? Int_Is_Sorted_Asc_TryParseIsIntegerSign(Expression input)
    {
        if (input is not Expression.List inputList)
        {
            return null;
        }

        if (inputList.items.Count is not 2)
        {
            return null;
        }

        if (TryParseExprAsIndependentSignedIntegerRelaxed(inputList.items[0]) == 0)
        {
            return (inputList.items[1], 4);
        }

        if (TryParseExprAsIndependentSignedIntegerRelaxed(inputList.items[1]) == -1)
        {
            return (inputList.items[0], 2);
        }

        return null;
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
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    constItems.Add(literalExpr.Value);
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            var andOps = prior;

            for (var i = 0; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

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
                            StackInstruction.Bit_And_Binary);
                }
            }

            for (var i = 0; i < constItems.Count; ++i)
            {
                var constItem = constItems[i];

                andOps =
                    andOps.AppendInstruction(
                        StackInstruction.Bit_And_Const(constItem));
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
                .AppendInstruction(StackInstruction.Bit_And_Generic);
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
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var itemExpr = listExpr.items[i];

                if (itemExpr is Expression.Literal literalExpr)
                {
                    constItems.Add(literalExpr.Value);
                }
                else
                {
                    varItems.Add(itemExpr);
                }
            }

            var orOps = prior;

            for (var i = 0; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

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
                            StackInstruction.Bit_Or_Binary);
                }
            }

            for (var i = 0; i < constItems.Count; ++i)
            {
                var constItem = constItems[i];

                orOps =
                    orOps.AppendInstruction(
                        StackInstruction.Bit_Or_Const(constItem));
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
                .AppendInstruction(StackInstruction.Bit_Or_Generic);
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
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
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
                            StackInstruction.Bit_Xor_Binary);
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
                .AppendInstruction(StackInstruction.Bit_Xor_Generic);
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
            .AppendInstruction(StackInstruction.Bit_Not);
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Left(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var shiftCountExpr =
                listExpr.items[0];

            var sourceExpr =
                listExpr.items[1];

            if (shiftCountExpr is Expression.Literal shiftCountLiteralExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(shiftCountLiteralExpr.Value) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Left_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Left_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Bit_Shift_Left_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Right(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var shiftCountExpr =
                listExpr.items[0];

            var sourceExpr =
                listExpr.items[1];

            if (shiftCountExpr is Expression.Literal shiftCountLiteralExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(shiftCountLiteralExpr.Value) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Right_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Right_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior)
            .AppendInstruction(StackInstruction.Bit_Shift_Right_Generic);
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

    private static BigInteger? TryParseExprAsIndependentSignedIntegerRelaxed(Expression expression)
    {
        if (TryEvaluateExpressionIndependent(expression) is not { } value)
        {
            return null;
        }

        return KernelFunction.SignedIntegerFromValueRelaxed(value);
    }

    private static PineValue? TryEvaluateExpressionIndependent(Expression expression)
    {
        if (expression.ReferencesEnvironment)
        {
            return null;
        }

        if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(expression).IsOkOrNull() is not { } value)
        {
            return null;
        }

        return value;
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
            for (var i = 0; i < list.items.Count; ++i)
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
