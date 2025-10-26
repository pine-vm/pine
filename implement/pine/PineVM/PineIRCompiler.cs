using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.PineVM;
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
            CompilationContext context,
            PineVMParseCache parseCache)
        {
            var exprResult =
                CompileExpressionTransitive(
                    expression,
                    context,
                    LocalsSet,
                    parseCache);

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
        PineValueClass? envClass,
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
                DestinationInstructionIndex: 0,
                EnvironmentLocalIndex: 0);
        }

        var tailCallEliminationDict =
            allTailCalls
            .SelectWhere(callExpr =>
            Maybe.NothingFromNull(TailCallElimination(callExpr))
            .Map(jump => new KeyValuePair<Expression.ParseAndEval, JumpToLoop>(callExpr, jump)))
            .ToImmutableDictionary();

        var priorBeforeParameters =
            tailCallEliminationDict.IsEmpty
            ?
            new NodeCompilationResult(
                Instructions: [],
                ImmutableDictionary<Expression, int>.Empty)
            :
            new NodeCompilationResult(
                Instructions:
                [
                    // 2025-10-26: Currently, we store in local(0) what used to be the 'environment'.
                    // TODO: Introduce mapping to individual parameters
                ],
                ImmutableDictionary<Expression, int>.Empty
                .SetItem(Expression.EnvironmentInstance, 0));

        var prior =
            priorBeforeParameters
            with
            {
                LocalsSet = priorBeforeParameters.LocalsSet.SetItem(
                    Expression.EnvironmentInstance,
                    0)
            };

        return
            CompileExpressionTransitive(
                rootExpression,
                context:
                CompilationContext.Init(tailCallEliminationDict),
                prior: prior,
                parseCache);
    }

    public static bool IsRecursiveCall(
        Expression.ParseAndEval parseAndEval,
        PineValueClass envClass,
        ImmutableHashSet<Expression> rootExprForms,
        PineVMParseCache parseCache)
    {
        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is not
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
        ImmutableDictionary<Expression, int> localIndexFromExpr,
        PineVMParseCache parseCache)
    {
        return
            CompileExpressionTransitive(
                expression,
                context: context,
                new NodeCompilationResult(
                    Instructions: [],
                    localIndexFromExpr),
                parseCache);
    }

    /// <summary>
    /// Recursively compile an expression into a flat list of instructions.
    /// </summary>
    public static NodeCompilationResult CompileExpressionTransitive(
        Expression expression,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
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
                prior,
                parseCache);

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
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
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
                        // 2025-10-26: Currently, we store in local(0) what used to be the 'environment'.
                        // TODO: Introduce mapping to individual parameters
                        StackInstruction.Local_Get(0));

            case Expression.List listExpr:
                {
                    if (listExpr.Items.Count is 0)
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

                    for (var i = 0; i < listExpr.Items.Count; ++i)
                    {
                        var itemExpr = listExpr.Items[i];

                        lastItemResult =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                lastItemResult,
                                parseCache);
                    }

                    return
                        lastItemResult
                        .AppendInstruction(
                            StackInstruction.Build_List(listExpr.Items.Count));
                }

            case Expression.Conditional conditional:
                return
                    CompileConditional(
                        conditional,
                        context,
                        prior,
                        parseCache);

            case Expression.ParseAndEval pae:
                return
                    CompileParseAndEval(
                        pae,
                        context,
                        prior,
                        parseCache);

            case Expression.KernelApplication kernelApp:
                return
                    CompileKernelApplication(
                        kernelApp,
                        context,
                        prior,
                        parseCache);

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    public static NodeCompilationResult CompileConditional(
        Expression.Conditional conditional,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        var afterCondition =
            CompileExpressionTransitive(
                conditional.Condition,
                context,
                prior,
                parseCache);

        var falseBranchInstructions =
            CompileExpressionTransitive(
                conditional.FalseBranch,
                context
                .AddInstructionOffset(afterCondition.Instructions.Count + 1),
                new NodeCompilationResult(
                    Instructions: [],
                    LocalsSet: afterCondition.LocalsSet),
                parseCache)
            .Instructions;

        var trueBranchInstructions =
            CompileExpressionTransitive(
                conditional.TrueBranch,
                context
                .AddInstructionOffset(afterCondition.Instructions.Count + 1)
                .AddInstructionOffset(falseBranchInstructions.Count + 1),
                new NodeCompilationResult(
                    Instructions: [],
                    LocalsSet: afterCondition.LocalsSet),
                parseCache)
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
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (context.TailCallElimination.TryGetValue(parseAndEvalExpr, out var jumpToLoop))
        {
            var afterEnvironment =
                prior.ContinueWithExpression(
                    parseAndEvalExpr.Environment,
                    context,
                    parseCache);

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
                    context,
                    parseCache);

            var afterExpr =
                afterEnvironment.ContinueWithExpression(
                    parseAndEvalExpr.Encoded,
                    context,
                    parseCache);

            return
                afterExpr
                .AppendInstruction(
                    StackInstruction.Parse_And_Eval_Binary);
        }
    }

    public static NodeCompilationResult CompileKernelApplication(
        Expression.KernelApplication kernelApplication,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        return
            kernelApplication.Function switch
            {
                nameof(KernelFunction.length) =>
                CompileExpressionTransitive(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Length),

                nameof(KernelFunction.negate) =>
                CompileKernelApplication_Negate(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.equal) =>
                CompileKernelApplication_Equal(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.head) =>
                CompileKernelApplication_Head(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.skip) =>
                CompileKernelApplication_Skip(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.take) =>
                CompileKernelApplication_Take(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.concat) =>
                CompileKernelApplication_Concat(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.reverse) =>
                CompileKernelApplication_Reverse(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.int_add) =>
                CompileKernelApplication_Int_Add(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.int_mul) =>
                CompileKernelApplication_Int_Mul(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.int_is_sorted_asc) =>
                CompileKernelApplication_Int_Is_Sorted_Asc(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_and) =>
                CompileKernelApplication_Bit_And(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_or) =>
                CompileKernelApplication_Bit_Or(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_xor) =>
                CompileKernelApplication_Bit_Xor(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_not) =>
                CompileKernelApplication_Bit_Not(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_shift_left) =>
                CompileKernelApplication_Bit_Shift_Left(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                nameof(KernelFunction.bit_shift_right) =>
                CompileKernelApplication_Bit_Shift_Right(
                    kernelApplication.Input,
                    context,
                    prior,
                    parseCache),

                _ =>
                throw new NotImplementedException(
                    "Unknown kernel function: " + kernelApplication.Function),
            };
    }

    public static NodeCompilationResult CompileKernelApplication_Equal(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (KernelApplication_Equal_TryParse_Starting_With(input, parseCache) is { } startingWith)
        {
            if (startingWith.expr is Expression.KernelApplication startingWithKernelApp &&
                startingWithKernelApp.Function is nameof(KernelFunction.skip) &&
                startingWithKernelApp.Input is Expression.List skipList &&
                skipList.Items.Count is 2)
            {
                var afterSliced =
                    CompileExpressionTransitive(
                        skipList.Items[1],
                        context,
                        prior,
                        parseCache);

                var afterSkipCount =
                    CompileExpressionTransitive(
                        skipList.Items[0],
                        context,
                        afterSliced,
                        parseCache);

                return
                    afterSkipCount
                    .AppendInstruction(
                        StackInstruction.Starts_With_Const_At_Offset_Var(startingWith.start));
            }

            {
                var afterSliced =
                    CompileExpressionTransitive(
                        startingWith.expr,
                        context,
                        prior,
                        parseCache);

                if (startingWith.start is PineValue.ListValue startList && startList.Items.Length is 0)
                {
                    return
                        afterSliced
                        .AppendInstruction(StackInstruction.Is_List_Value);
                }

                if (startingWith.start is PineValue.BlobValue startBlob && startBlob.Bytes.Length is 0)
                {
                    return
                        afterSliced
                        .AppendInstruction(StackInstruction.Is_Blob_Value);
                }

                return
                    afterSliced
                    .AppendInstruction(
                        StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(0)))
                    .AppendInstruction(
                        StackInstruction.Starts_With_Const_At_Offset_Var(startingWith.start));
            }
        }

        NodeCompilationResult ContinueForConst(
            PineValue partConst,
            Expression partVar)
        {
            if (IntegerEncoding.ParseSignedIntegerRelaxed(partConst).IsOkOrNullable() is { } constInteger &&
                partVar is Expression.KernelApplication varKernelApp &&
                varKernelApp.Function is nameof(KernelFunction.length))
            {
                var afterVar =
                    CompileExpressionTransitive(
                        varKernelApp.Input,
                        context,
                        prior,
                        parseCache);

                return
                    afterVar
                    .AppendInstruction(
                        StackInstruction.Length_Equal_Const(constInteger));
            }

            {
                var afterVar =
                    CompileExpressionTransitive(
                        partVar,
                        context,
                        prior,
                        parseCache);

                return
                    afterVar
                    .AppendInstruction(
                        StackInstruction.Equal_Binary_Const(partConst));
            }
        }

        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 2)
            {
                if (TryEvalIndependent(listExpr.Items[0], parseCache) is { } leftLiteralValue)
                {
                    return ContinueForConst(leftLiteralValue, listExpr.Items[1]);
                }

                if (TryEvalIndependent(listExpr.Items[1], parseCache) is { } rightLiteralValue)
                {
                    return ContinueForConst(rightLiteralValue, listExpr.Items[0]);
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.Items[0],
                            context,
                            prior,
                            parseCache);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.Items[1],
                            context,
                            afterLeft,
                            parseCache);

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
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Equal_Generic);
    }

    private static (Expression expr, PineValue start)? KernelApplication_Equal_TryParse_Starting_With(
        Expression equalInput,
        PineVMParseCache parseCache)
    {
        if (equalInput is not Expression.List inputList)
        {
            return null;
        }

        if (inputList.Items.Count is not 2)
        {
            return null;
        }

        (Expression expr, PineValue start)? ContinueWithConst(
            PineValue partConst,
            Expression partVar)
        {
            if (TryParse_KernelTake_Const(partVar, parseCache) is { } varTake)
            {
                if (partConst is PineValue.BlobValue leftBlob &&
                    leftBlob.Bytes.Length == varTake.takeCount)
                {
                    return (varTake.sourceExpr, leftBlob);
                }

                if (partConst is PineValue.ListValue leftList &&
                    leftList.Items.Length == varTake.takeCount)
                {
                    return (varTake.sourceExpr, leftList);
                }
            }

            return null;
        }

        {
            if (TryEvalIndependent(inputList.Items[0], parseCache) is { } leftValue)
            {
                return ContinueWithConst(leftValue, inputList.Items[1]);
            }
        }

        {
            if (TryEvalIndependent(inputList.Items[1], parseCache) is { } rightValue)
            {
                return ContinueWithConst(rightValue, inputList.Items[0]);
            }
        }

        return null;
    }

    public static NodeCompilationResult CompileKernelApplication_Head(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryParse_KernelSkip(input) is { } skip)
        {
            var afterSource =
                CompileExpressionTransitive(
                    skip.sourceExpr,
                    context,
                    prior,
                    parseCache);

            if (TryParse_IndependentSignedIntegerRelaxed(skip.skipCountExpr, parseCache) is { } skipCountConst)
            {
                return
                    afterSource
                    .AppendInstruction(
                        StackInstruction.Skip_Head_Const((int)skipCountConst));
            }

            var afterSkipCount =
                CompileExpressionTransitive(
                    skip.skipCountExpr,
                    context,
                    afterSource,
                    parseCache);

            return
                afterSkipCount
                .AppendInstruction(StackInstruction.Skip_Head_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Head_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Skip(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr && listExpr.Items.Count is 2)
        {
            var skipCountExpr = listExpr.Items[0];

            var skipSourceExpr = listExpr.Items[1];

            var afterSource =
                CompileExpressionTransitive(
                    skipSourceExpr,
                    context,
                    prior,
                    parseCache);

            if (TryParse_IndependentSignedIntegerRelaxed(skipCountExpr, parseCache) is { } skipCount)
            {
                if (skipCount.IsOne &&
                    skipSourceExpr is Expression.KernelApplication skipSourceKernelApp &&
                    skipSourceKernelApp.Function is nameof(KernelFunction.int_add) &&
                    skipSourceKernelApp.Input is Expression.List skipSourceAddInputList &&
                    skipSourceAddInputList.Items.Count is 2)
                {
                    NodeCompilationResult? ContinueForAddZeroOperand(Expression addZeroOperand)
                    {
                        if (addZeroOperand is Expression.KernelApplication addOperandKernelApp &&
                            addOperandKernelApp.Function is nameof(KernelFunction.concat) &&
                            addOperandKernelApp.Input is Expression.List concatList &&
                            concatList.Items.Count is 2 &&
                            TryEvalIndependent(concatList.Items[0], parseCache) is { } prependValue &&
                            prependValue is PineValue.BlobValue prependBlob &&
                            prependBlob.Bytes.Length is 1 &&
                            prependBlob.Bytes.Span[0] is 2 or 4)
                        {
                            var afterSource =
                                CompileExpressionTransitive(
                                    concatList.Items[1],
                                    context,
                                    prior,
                                    parseCache);

                            return
                                afterSource
                                .AppendInstruction(
                                    StackInstruction.Blob_Trim_Leading_Zeros(minRemainingCount: 1));
                        }

                        return null;
                    }

                    {
                        if (TryParse_IndependentSignedIntegerRelaxed(skipSourceAddInputList.Items[0], parseCache) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(skipSourceAddInputList.Items[1]) is { } specialized)
                        {
                            return specialized;
                        }
                    }

                    {
                        if (TryParse_IndependentSignedIntegerRelaxed(skipSourceAddInputList.Items[1], parseCache) is { } skipSourceAddValue &&
                            skipSourceAddValue.IsZero &&
                            ContinueForAddZeroOperand(skipSourceAddInputList.Items[0]) is { } specialized)
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
                        afterSource,
                        parseCache);

                return
                    afterSkipCount
                    .AppendInstruction(StackInstruction.Skip_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Skip_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Take(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr && listExpr.Items.Count is 2)
        {
            var takeCountValueExpr = listExpr.Items[0];

            if (TryParse_KernelSkip(listExpr.Items[1]) is { } sourceSkip)
            {
                var afterSource =
                    CompileExpressionTransitive(
                        sourceSkip.sourceExpr,
                        context,
                        prior,
                        parseCache);

                var afterSkipCount =
                    CompileExpressionTransitive(
                        sourceSkip.skipCountExpr,
                        context,
                        afterSource,
                        parseCache);

                if (TryParse_IndependentSignedIntegerRelaxed(takeCountValueExpr, parseCache) is { } takeCount)
                {
                    return
                        afterSkipCount
                        .AppendInstruction(
                            StackInstruction.Slice_Skip_Var_Take_Const((int)takeCount));
                }

                {
                    var afterTakeCount =
                        CompileExpressionTransitive(
                            takeCountValueExpr,
                            context,
                            afterSkipCount,
                            parseCache);

                    return
                        afterTakeCount
                        .AppendInstruction(
                            StackInstruction.Slice_Skip_Var_Take_Var);
                }
            }

            {
                var afterSource =
                    CompileExpressionTransitive(
                        listExpr.Items[1],
                        context,
                        prior,
                        parseCache);

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
                        afterSource,
                        parseCache);

                return
                    afterTakeCount
                    .AppendInstruction(StackInstruction.Take_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Take_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Concat(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 2)
            {
                if (listExpr.Items[0] is Expression.List leftListExpr && leftListExpr.Items.Count is 1)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            leftListExpr.Items[0],
                            context,
                            prior,
                            parseCache);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.Items[1],
                            context,
                            afterLeft,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Prepend_List_Item_Binary);
                }
            }

            var concatOps = prior;

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var item = listExpr.Items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        context,
                        concatOps,
                        parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Concat_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Reverse(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (TryParse_KernelTake_Const(input, parseCache) is { } takeConst &&
            takeConst.sourceExpr is Expression.KernelApplication takeSourceKernelApp &&
            takeSourceKernelApp.Function is nameof(KernelFunction.reverse))
        {
            var afterSource =
                CompileExpressionTransitive(
                    takeSourceKernelApp.Input,
                    context,
                    prior,
                    parseCache);

            return
                afterSource
                .AppendInstruction(StackInstruction.Take_Last_Const((int)takeConst.takeCount));
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Reverse);
    }

    public static NodeCompilationResult CompileKernelApplication_Negate(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.KernelApplication innerKernelApp)
        {
            if (innerKernelApp.Function is nameof(KernelFunction.equal) &&
                innerKernelApp.Input is Expression.List equalList && equalList.Items.Count is 2)
            {
                if (equalList.Items[0] is Expression.Literal leftLiteralExpr)
                {
                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.Items[1],
                            context,
                            prior,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(
                            StackInstruction.Not_Equal_Binary_Const(leftLiteralExpr.Value));
                }

                if (equalList.Items[1] is Expression.Literal rightLiteralExpr)
                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.Items[0],
                            context,
                            prior,
                            parseCache);

                    return
                        afterLeft
                        .AppendInstruction(
                            StackInstruction.Not_Equal_Binary_Const(rightLiteralExpr.Value));
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            equalList.Items[0],
                            context,
                            prior,
                            parseCache);

                    var afterRight =
                        CompileExpressionTransitive(
                            equalList.Items[1],
                            context,
                            afterLeft,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Not_Equal_Binary);
                }
            }

            if (innerKernelApp.Function is nameof(KernelFunction.int_is_sorted_asc) &&
                innerKernelApp.Input is Expression.List isSortedAscList && isSortedAscList.Items.Count is 2)
            {
                /*
                 * not (int_is_sorted_asc [a, b]) = b <= a
                 * */

                var afterLeft =
                    CompileExpressionTransitive(
                        isSortedAscList.Items[1],
                        context,
                        prior,
                        parseCache);

                var afterRight =
                    CompileExpressionTransitive(
                        isSortedAscList.Items[0],
                        context,
                        afterLeft,
                        parseCache);

                return
                    afterRight
                    .AppendInstruction(StackInstruction.Int_Less_Than_Binary);
            }
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Negate);
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Add(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0)
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

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var itemExpr = listExpr.Items[i];

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
                            addOps,
                            parseCache);

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
                    if (IsIntUnsigned(firstVarItemAdd, parseCache) is { } unsignedExpr)
                    {
                        var addOps =
                            CompileExpressionTransitive(
                                unsignedExpr,
                                context,
                                prior,
                                parseCache);

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
                            prior,
                            parseCache);

                    for (var i = 1; i < varItemsAdd.Count; ++i)
                    {
                        var itemExpr = varItemsAdd[i];

                        var itemOps =
                            CompileExpressionTransitive(
                                itemExpr,
                                context,
                                addOps,
                                parseCache);

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
                                addOps,
                                parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Int_Add_Generic);
        }
    }

    private static Expression? IsIntUnsigned(
        Expression expr,
        PineVMParseCache parseCache)
    {
        if (expr is Expression.KernelApplication kernelApp &&
            kernelApp.Function is nameof(KernelFunction.concat) &&
            kernelApp.Input is Expression.List inputList &&
            inputList.Items.Count is 2)
        {
            var prependedExpr = inputList.Items[0];

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (ReducePineExpression.TryEvaluateExpressionIndependent(
                    prependedExpr, parseCache).IsOkOrNull() is not
                    { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 4)
                {
                    return inputList.Items[1];
                }
            }
        }

        return null;
    }

    private static Expression? IsIntUnsignedNegated(
        Expression expr,
        PineVMParseCache parseCache)
    {
        if (expr is Expression.KernelApplication kernelApp &&
            kernelApp.Function is nameof(KernelFunction.concat) &&
            kernelApp.Input is Expression.List inputList &&
            inputList.Items.Count is 2)
        {
            var prependedExpr = inputList.Items[0];

            if (!prependedExpr.ReferencesEnvironment)
            {
                if (ReducePineExpression.TryEvaluateExpressionIndependent(
                    prependedExpr, parseCache).IsOkOrNull() is not
                    { } prependedValue)
                {
                    return null;
                }

                if (prependedValue is PineValue.BlobValue prependedBlob &&
                    prependedBlob.Bytes.Length is 1 &&
                    prependedBlob.Bytes.Span[0] is 2)
                {
                    return inputList.Items[1];
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
               kernelApp.Input is Expression.List mulListExpr && mulListExpr.Items.Count is 2)
            {
                {
                    if (mulListExpr.Items[0] is Expression.Literal literalExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } leftInt &&
                        leftInt == -1)
                    {
                        return mulListExpr.Items[1];
                    }
                }

                {
                    if (mulListExpr.Items[1] is Expression.Literal literalExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } rightInt &&
                        rightInt == -1)
                    {
                        return mulListExpr.Items[0];
                    }
                }
            }
        }

        return null;
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Mul(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(1)));
            }

            BigInteger constItemsProduct = 1;
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var itemExpr = listExpr.Items[i];

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
                    prior,
                    parseCache);

            for (var i = 1; i < varItems.Count; ++i)
            {
                var itemExpr = varItems[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        mulOps,
                        parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Int_Mul_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Int_Is_Sorted_Asc(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (Int_Is_Sorted_Asc_TryParseIsIntegerSign(input, parseCache) is { } isIntegerWithSign)
        {
            return
                CompileExpressionTransitive(
                    isIntegerWithSign.expr,
                    context,
                    prior,
                    parseCache)
                .AppendInstruction(
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(0)))
                .AppendInstruction(
                    StackInstruction.Starts_With_Const_At_Offset_Var(PineValue.BlobSingleByte(isIntegerWithSign.sign)));
        }

        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0 || listExpr.Items.Count is 1)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineKernelValues.TrueValue));
            }

            if (listExpr.Items.Count is 2)
            {
                if (listExpr.Items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    var rightExpr = listExpr.Items[1];

                    if (IsIntUnsigned(rightExpr, parseCache) is { } rightUnsigned)
                    {
                        var afterRight =
                            CompileExpressionTransitive(
                                rightUnsigned,
                                context,
                                prior,
                                parseCache);

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
                                prior,
                                parseCache);

                        return
                            afterRight
                            .AppendInstruction(
                                StackInstruction.Int_Greater_Than_Or_Equal_Const(leftInt));
                    }
                }

                if (listExpr.Items[1] is Expression.Literal rightLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                {
                    var leftExpr = listExpr.Items[0];

                    if (IsIntUnsigned(leftExpr, parseCache) is { } leftUnsigned)
                    {
                        var afterLeft =
                            CompileExpressionTransitive(
                                leftUnsigned,
                                context,
                                prior,
                                parseCache);

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
                                prior,
                                parseCache);

                        return
                            afterLeft
                            .AppendInstruction(
                                StackInstruction.Int_Less_Than_Or_Equal_Const(rightInt));
                    }
                }

                {
                    var afterLeft =
                        CompileExpressionTransitive(
                            listExpr.Items[0],
                            context,
                            prior,
                            parseCache);

                    var afterRight =
                        CompileExpressionTransitive(
                            listExpr.Items[1],
                            context,
                            afterLeft,
                            parseCache);

                    return
                        afterRight
                        .AppendInstruction(StackInstruction.Int_Less_Than_Or_Equal_Binary);
                }
            }

            if (listExpr.Items.Count is 3)
            {
                if (listExpr.Items[0] is Expression.Literal leftLiteralExpr &&
                    KernelFunction.SignedIntegerFromValueRelaxed(leftLiteralExpr.Value) is { } leftInt)
                {
                    if (listExpr.Items[2] is Expression.Literal rightLiteralExpr &&
                        KernelFunction.SignedIntegerFromValueRelaxed(rightLiteralExpr.Value) is { } rightInt)
                    {
                        var middleExpr = listExpr.Items[1];

                        if (IsIntUnsigned(middleExpr, parseCache) is { } middleUnsigned)
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
                                    prior,
                                    parseCache);

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
                                    prior,
                                    parseCache);

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
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Int_Is_Sorted_Asc_Generic);
    }

    private static (Expression expr, byte sign)? Int_Is_Sorted_Asc_TryParseIsIntegerSign(
        Expression input,
        PineVMParseCache parseCache)
    {
        if (input is not Expression.List inputList)
        {
            return null;
        }

        if (inputList.Items.Count is not 2)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(inputList.Items[0], parseCache) == 0)
        {
            return (inputList.Items[1], 4);
        }

        if (TryParse_IndependentSignedIntegerRelaxed(inputList.Items[1], parseCache) == -1)
        {
            return (inputList.Items[0], 2);
        }

        return null;
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_And(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var itemExpr = listExpr.Items[i];

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
                        andOps,
                        parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_And_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Or(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var constItems = new List<PineValue>();
            var varItems = new List<Expression>();

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var itemExpr = listExpr.Items[i];

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
                        orOps,
                        parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_Or_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Xor(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.Items.Count is 0)
            {
                return
                    prior
                    .AppendInstruction(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob));
            }

            var xorOps = prior;

            for (var i = 0; i < listExpr.Items.Count; ++i)
            {
                var itemExpr = listExpr.Items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        itemExpr,
                        context,
                        xorOps,
                        parseCache);

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
                    prior,
                    parseCache)
                .AppendInstruction(StackInstruction.Bit_Xor_Generic);
        }
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Not(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Not);
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Left(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr && listExpr.Items.Count is 2)
        {
            var shiftCountExpr =
                listExpr.Items[0];

            var sourceExpr =
                listExpr.Items[1];

            if (shiftCountExpr is Expression.Literal shiftCountLiteralExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(shiftCountLiteralExpr.Value) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior,
                        parseCache)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Left_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior,
                    parseCache);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps,
                    parseCache);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Left_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Shift_Left_Generic);
    }

    public static NodeCompilationResult CompileKernelApplication_Bit_Shift_Right(
        Expression input,
        CompilationContext context,
        NodeCompilationResult prior,
        PineVMParseCache parseCache)
    {
        if (input is Expression.List listExpr && listExpr.Items.Count is 2)
        {
            var shiftCountExpr =
                listExpr.Items[0];

            var sourceExpr =
                listExpr.Items[1];

            if (shiftCountExpr is Expression.Literal shiftCountLiteralExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(shiftCountLiteralExpr.Value) is { } shiftCount)
            {
                return
                    CompileExpressionTransitive(
                        sourceExpr,
                        context,
                        prior,
                        parseCache)
                    .AppendInstruction(
                        StackInstruction.Bit_Shift_Right_Const((int)shiftCount));
            }

            var sourceOps =
                CompileExpressionTransitive(
                    sourceExpr,
                    context,
                    prior,
                    parseCache);

            var shiftCountOps =
                CompileExpressionTransitive(
                    shiftCountExpr,
                    context,
                    sourceOps,
                    parseCache);

            return
                shiftCountOps
                .AppendInstruction(StackInstruction.Bit_Shift_Right_Binary);
        }

        return
            CompileExpressionTransitive(
                input,
                context,
                prior,
                parseCache)
            .AppendInstruction(StackInstruction.Bit_Shift_Right_Generic);
    }

    public static Expression? TryParse_IntNegation(Expression expression)
    {
        if (expression is not Expression.KernelApplication kernelApp)
        {
            return null;
        }

        if (kernelApp.Function is nameof(KernelFunction.negate))
        {
            return kernelApp.Input;
        }

        if (kernelApp.Function is nameof(KernelFunction.int_mul) &&
            kernelApp.Input is Expression.List mulList && mulList.Items.Count is 2)
        {
            if (mulList.Items[0] is Expression.Literal literalExpr &&
                KernelFunction.SignedIntegerFromValueRelaxed(literalExpr.Value) is { } literalValue &&
                literalValue == -1)
            {
                return mulList.Items[1];
            }

            if (mulList.Items[1] is Expression.Literal literalExpr2 &&
                KernelFunction.SignedIntegerFromValueRelaxed(literalExpr2.Value) is { } literalValue2 &&
                literalValue2 == -1)
            {
                return mulList.Items[0];
            }
        }

        return null;
    }

    private static (BigInteger skipCount, Expression sourceExpr)? TryParse_KernelSkip_Const(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (TryParse_KernelSkip(expression) is not { } parsed)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(parsed.skipCountExpr, parseCache) is not { } skipCount)
        {
            return null;
        }

        return (skipCount, parsed.sourceExpr);
    }

    private static (Expression skipCountExpr, Expression sourceExpr)? TryParse_KernelSkip(Expression expression)
    {
        if (expression is not Expression.KernelApplication kernelApp ||
            kernelApp.Function is not nameof(KernelFunction.skip) ||
            kernelApp.Input is not Expression.List skipList ||
            skipList.Items.Count is not 2)
        {
            return null;
        }

        var skipCountExpr = skipList.Items[0];
        var sourceExpr = skipList.Items[1];

        return (skipCountExpr, sourceExpr);
    }

    private static (BigInteger takeCount, Expression sourceExpr)? TryParse_KernelTake_Const(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (TryParse_KernelTake(expression) is not { } parsed)
        {
            return null;
        }

        if (TryParse_IndependentSignedIntegerRelaxed(parsed.takeCountExpr, parseCache) is not { } takeCount)
        {
            return null;
        }

        return (takeCount, parsed.sourceExpr);
    }

    private static (Expression takeCountExpr, Expression sourceExpr)? TryParse_KernelTake(
        Expression expression)
    {
        if (expression is not Expression.KernelApplication kernelApp ||
            kernelApp.Function is not nameof(KernelFunction.take) ||
            kernelApp.Input is not Expression.List takeList ||
            takeList.Items.Count is not 2)
        {
            return null;
        }

        var takeCountExpr = takeList.Items[0];

        var sourceExpr = takeList.Items[1];

        return (takeCountExpr, sourceExpr);
    }

    private static BigInteger? TryParse_IndependentSignedIntegerRelaxed(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (TryEvalIndependent(expression, parseCache) is not { } value)
        {
            return null;
        }

        return KernelFunction.SignedIntegerFromValueRelaxed(value);
    }

    private static PineValue? TryEvalIndependent(
        Expression expression,
        PineVMParseCache parseCache)
    {
        if (expression.ReferencesEnvironment)
        {
            return null;
        }

        return
            ReducePineExpression.TryEvaluateExpressionIndependent(expression, parseCache)
            .IsOkOrNull();
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
            for (var i = 0; i < list.Items.Count; ++i)
            {
                if (ExpressionLargeEnoughForCSE(list.Items[i]))
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
