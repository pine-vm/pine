using Pine.Core;
using System.Collections.Generic;
using System;

namespace Pine.PineVM;

public class PineIRCompiler
{

    /// <summary>
    /// Recursively compile an expression into a flat list of PineIROp instructions.
    /// </summary>
    public static IReadOnlyList<StackInstruction> CompileExpressionTransitive(
        Expression expr,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (localIndexFromExpr(expr) is { } localIndex)
        {
            return
                [new StackInstruction(
                    Kind: StackInstructionKind.Local_Get,
                    LocalIndex: localIndex)
                ];
        }

        switch (expr)
        {
            case Expression.Literal literalExpr:
                return
                    [new StackInstruction(
                    Kind: StackInstructionKind.Push_Literal,
                    Literal: literalExpr.Value)
                    ];

            case Expression.Environment:
                return
                    [
                    StackInstruction.PushEnvironment
                    ];

            case Expression.List listExpr:
                {
                    /*
                     * Assume that the subsequence for each item only leaves one value on the stack.
                     * 
                     * (When we need to reuse a value from a subexpression multiple times,
                     * we don't use a non-consuming instruction but use local_get instead to copy it)
                     * */

                    var itemsInstructions = new List<StackInstruction>();

                    for (var i = 0; i < listExpr.items.Count; ++i)
                    {
                        var itemExpr = listExpr.items[i];

                        var itemInstructions =
                            CompileExpressionTransitive(
                                itemExpr,
                                localIndexFromExpr);

                        itemsInstructions.AddRange(itemInstructions);
                    }

                    return
                        [
                        ..itemsInstructions,
                        new StackInstruction(
                            StackInstructionKind.BuildList,
                            TakeCount: listExpr.items.Count)
                        ];
                }

            case Expression.Conditional:

                throw new InvalidOperationException(
                    "Conditional should be filtered out earlier");

            case Expression.ParseAndEval pae:
                return
                    CompileParseAndEval(
                        pae,
                        localIndexFromExpr);

            case Expression.KernelApplication kernelApp:
                return
                    CompileKernelApplication(
                        kernelApp,
                        localIndexFromExpr);

            default:
                throw new NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    public static IReadOnlyList<StackInstruction> CompileParseAndEval(
        Expression.ParseAndEval parseAndEvalExpr,
        Func<Expression, int?> localIndexFromExpr)
    {
        var environmentOps =
            CompileExpressionTransitive(
                parseAndEvalExpr.Environment,
                localIndexFromExpr);

        var encodedOps =
            CompileExpressionTransitive(
                parseAndEvalExpr.Encoded,
                localIndexFromExpr);

        return
            [
            .. environmentOps,
            .. encodedOps,
            new StackInstruction(StackInstructionKind.Parse_And_Eval)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication(
        Expression.KernelApplication kernelApplication,
        Func<Expression, int?> localIndexFromExpr)
    {
        return
            kernelApplication.Function switch
            {
                nameof(KernelFunction.length) =>
                [
                    .. CompileExpressionTransitive(
                        kernelApplication.Input,
                        localIndexFromExpr),
                    new StackInstruction(StackInstructionKind.Length)
                ],

                nameof(KernelFunction.negate) =>
                [
                    .. CompileExpressionTransitive(
                        kernelApplication.Input,
                        localIndexFromExpr),
                    new StackInstruction(StackInstructionKind.Negate)
                ],

                nameof(KernelFunction.equal) =>
                CompileKernelApplication_Equal(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.head) =>
                CompileKernelApplication_Head(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.skip) =>
                CompileKernelApplication_Skip(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.take) =>
                CompileKernelApplication_Take(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.concat) =>
                CompileKernelApplication_Concat(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.reverse) =>
                CompileKernelApplication_Reverse(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.int_add) =>
                CompileKernelApplication_Int_Add(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.int_mul) =>
                CompileKernelApplication_Int_Mul(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.int_is_sorted_asc) =>
                CompileKernelApplication_Int_Is_Sorted_Asc(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_and) =>
                CompileKernelApplication_Bit_And(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_or) =>
                CompileKernelApplication_Bit_Or(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_xor) =>
                CompileKernelApplication_Bit_Xor(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_not) =>
                CompileKernelApplication_Bit_Not(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_shift_left) =>
                CompileKernelApplication_Bit_Shift_Left(
                    kernelApplication.Input,
                    localIndexFromExpr),

                nameof(KernelFunction.bit_shift_right) =>
                CompileKernelApplication_Bit_Shift_Right(
                    kernelApplication.Input,
                    localIndexFromExpr),

                _ =>
                throw new NotImplementedException(
                    "Unknown kernel function: " + kernelApplication.Function),
            };
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Equal(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 2)
            {
                var leftOps =
                    CompileExpressionTransitive(
                        listExpr.items[0],
                        localIndexFromExpr);

                var rightOps =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        localIndexFromExpr);

                return
                    [
                    .. rightOps,
                    .. leftOps,
                    new StackInstruction(StackInstructionKind.Equal_Binary)
                    ];
            }
        }

        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Equal_List)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Head(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.KernelApplication innerKernelApp && innerKernelApp.Function is "skip")
        {
            if (innerKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
            {
                var sourceOps =
                    CompileExpressionTransitive(
                        skipList.items[1],
                        localIndexFromExpr);

                if (skipList.items[0] is Expression.Literal literalExpr)
                {
                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(literalExpr.Value).IsOkOrNullable() is { } skipCount)
                    {
                        return
                            [
                            .. sourceOps,
                            new StackInstruction(StackInstructionKind.Skip_Head_Const, SkipCount: (int)skipCount)
                            ];
                    }
                }

                var skipCountOps =
                    CompileExpressionTransitive(
                        skipList.items[0],
                        localIndexFromExpr);

                return
                    [
                    .. sourceOps,
                    .. skipCountOps,
                    new StackInstruction(StackInstructionKind.Skip_Head_Var)
                    ];
            }
        }

        var inputOps =
            CompileExpressionTransitive(
                input,
                localIndexFromExpr);

        return
            [.. inputOps,
            new StackInstruction(StackInstructionKind.Head_Generic)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Skip(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var skipCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    localIndexFromExpr);

            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    localIndexFromExpr);

            return
                [
                .. sourceOps,
                .. skipCountOps,
                new StackInstruction(StackInstructionKind.Skip_Binary)
                ];
        }

        var inputOps =
            CompileExpressionTransitive(
                input,
                localIndexFromExpr);

        return
            [.. inputOps,
            new StackInstruction(StackInstructionKind.Skip_Generic)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Take(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var takeCountValueExpr = listExpr.items[0];

            var takeCountOps =
                CompileExpressionTransitive(
                    takeCountValueExpr,
                    localIndexFromExpr);

            if (listExpr.items[1] is Expression.KernelApplication sourceKernelApp &&
                sourceKernelApp.Function is "skip")
            {
                if (sourceKernelApp.Input is Expression.List skipList && skipList.items.Count is 2)
                {
                    var skipCountOps =
                        CompileExpressionTransitive(
                            skipList.items[0],
                            localIndexFromExpr);

                    var sourceOps =
                        CompileExpressionTransitive(
                            skipList.items[1],
                            localIndexFromExpr);

                    /*
                     * Earlier reduction pass should already have reduced the contents in take to
                     * literal at this point, if at all possible, so don't reach for more general eval here.
                     */
                    if (takeCountValueExpr is Expression.Literal takeCountLiteral)
                    {
                        if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountLiteral.Value) is { } takeCount)
                        {
                            return
                                [
                                .. sourceOps,
                                .. skipCountOps,
                                new StackInstruction(
                                    StackInstructionKind.Slice_Skip_Var_Take_Const,
                                    TakeCount: (int)takeCount)
                                ];
                        }
                    }

                    return
                        [
                        .. sourceOps,
                        .. skipCountOps,
                        .. takeCountOps,
                        new StackInstruction(StackInstructionKind.Slice_Skip_Var_Take_Var)
                        ];
                }
            }

            {
                var sourceOps =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        localIndexFromExpr);

                return
                    [
                    .. sourceOps,
                    .. takeCountOps,
                    new StackInstruction(StackInstructionKind.Take_Binary)
                    ];
            }
        }

        var inputOps =
            CompileExpressionTransitive(
                input,
                localIndexFromExpr);

        return
            [
            .. inputOps,
            new StackInstruction(StackInstructionKind.Take_Generic)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Concat(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            var concatOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                concatOps.AddRange(itemOps);

                if (0 < i)
                {
                    concatOps.Add(new StackInstruction(StackInstructionKind.Concat_Binary));
                }
            }

            return concatOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Concat_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Reverse(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {

        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Reverse)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Int_Add(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValueAsInteger.ValueFromSignedInteger(0))
                    ];
            }

            var addOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                addOps.AddRange(itemOps);

                if (0 < i)
                {
                    addOps.Add(new StackInstruction(StackInstructionKind.Int_Add_Binary));
                }
            }

            return addOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Int_Add_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Int_Mul(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValueAsInteger.ValueFromSignedInteger(1))
                    ];
            }

            var mulOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                mulOps.AddRange(itemOps);

                if (0 < i)
                {
                    mulOps.Add(new StackInstruction(StackInstructionKind.Int_Mul_Binary));
                }
            }

            return mulOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Int_Mul_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Int_Is_Sorted_Asc(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0 || listExpr.items.Count is 1)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineVMValues.TrueValue)
                    ];
            }

            if (listExpr.items.Count is 2)
            {
                var leftOps =
                    CompileExpressionTransitive(
                        listExpr.items[0],
                        localIndexFromExpr);

                var rightOps =
                    CompileExpressionTransitive(
                        listExpr.items[1],
                        localIndexFromExpr);

                return
                    [
                    .. leftOps,
                    .. rightOps,
                    new StackInstruction(StackInstructionKind.Int_Less_Than_Or_Equal_Binary)
                    ];
            }
        }

        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Int_Is_Sorted_Asc_List)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_And(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValue.EmptyBlob)
                    ];
            }

            var andOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                andOps.AddRange(itemOps);

                if (0 < i)
                {
                    andOps.Add(new StackInstruction(StackInstructionKind.Bit_And_Binary));
                }
            }

            return andOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Bit_And_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_Or(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValue.EmptyBlob)
                    ];
            }

            var orOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                orOps.AddRange(itemOps);

                if (0 < i)
                {
                    orOps.Add(new StackInstruction(StackInstructionKind.Bit_Or_Binary));
                }
            }

            return orOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Bit_Or_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_Xor(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr)
        {
            if (listExpr.items.Count is 0)
            {
                return
                    [
                    new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValue.EmptyBlob)
                    ];
            }

            var xorOps = new List<StackInstruction>();

            for (var i = 0; i < listExpr.items.Count; ++i)
            {
                var item = listExpr.items[i];

                var itemOps =
                    CompileExpressionTransitive(
                        item,
                        localIndexFromExpr);

                xorOps.AddRange(itemOps);

                if (0 < i)
                {
                    xorOps.Add(new StackInstruction(StackInstructionKind.Bit_Xor_Binary));
                }
            }

            return xorOps;
        }
        else
        {
            return
                [
                .. CompileExpressionTransitive(input, localIndexFromExpr),
                new StackInstruction(StackInstructionKind.Bit_Xor_List)
                ];
        }
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_Not(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Bit_Not)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_Shift_Left(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    localIndexFromExpr);

            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    localIndexFromExpr);

            return
                [
                .. sourceOps,
                .. shiftCountOps,
                new StackInstruction(StackInstructionKind.Bit_Shift_Left_Var)
                ];
        }

        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Bit_Shift_Left_List)
            ];
    }

    public static IReadOnlyList<StackInstruction> CompileKernelApplication_Bit_Shift_Right(
        Expression input,
        Func<Expression, int?> localIndexFromExpr)
    {
        if (input is Expression.List listExpr && listExpr.items.Count is 2)
        {
            var shiftCountOps =
                CompileExpressionTransitive(
                    listExpr.items[0],
                    localIndexFromExpr);

            var sourceOps =
                CompileExpressionTransitive(
                    listExpr.items[1],
                    localIndexFromExpr);

            return
                [
                .. sourceOps,
                .. shiftCountOps,
                new StackInstruction(StackInstructionKind.Bit_Shift_Right_Var)
                ];
        }

        return
            [
            .. CompileExpressionTransitive(input, localIndexFromExpr),
            new StackInstruction(StackInstructionKind.Bit_Shift_Right_List)
            ];
    }
}
