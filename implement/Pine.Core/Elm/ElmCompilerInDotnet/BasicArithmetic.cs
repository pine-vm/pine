using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Arithmetic functions corresponding to the `Basics` module in the Elm core libraries:
/// <see href="https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/Basics.elm"/>
/// <para>
/// The number encodings supported here are the same as implemented in <see cref="ElmValueEncoding"/>.
/// </para>
/// <para>
/// The variants encoding function values use the format for generic partial function application as
/// specified in elm-compiler-implementation-guide.md
/// </para>
/// <para>
/// Does not contain addition or multiplication for integers, as those are translated directly to Pine_builtin functions.
/// </para>
/// </summary>
public class BasicArithmetic
{
    /// <summary>
    /// (-) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(-)"/>
    /// </para>
    /// </summary>
    public static Expression Int_sub(
        Expression minuend,
        Expression subtrahend)
    {
        return
            BuiltinAdd(
                minuend,
                BuiltinMul(
                    LiteralInt(-1),
                    subtrahend)
                );
    }

    /// <summary>
    /// (//) : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(//)"/>
    /// </para>
    /// </summary>
    public static Expression Int_div(
        Expression dividend,
        Expression divisor)
    {
        /*
        idiv : Int -> Int -> Int
        idiv dividend divisor =
            if Pine_kernel.equal [ divisor, 0 ] then
                0
            else ...
         
        Implementation approach:
        - If divisor is 0, return 0
        - Compute absolute values and track signs
        - Use repeated subtraction (simplified from the optimized Elm version)
        - Apply sign to result
        * */

        var zero = LiteralInt(0);
        var negativeOne = LiteralInt(-1);

        // Check if divisor is 0
        var divisorIsZero =
            BuiltinHelpers.ApplyBuiltinEqualBinary(divisor, zero);

        // Check signs: int_is_sorted_asc [0, x] means 0 <= x (x is non-negative)
        var dividendNonNegative = BuiltinIntIsSortedAsc(zero, dividend);
        var divisorNonNegative = BuiltinIntIsSortedAsc(zero, divisor);

        // Absolute values
        var absDividend =
            Expression.ConditionalInstance(
                condition: dividendNonNegative,
                trueBranch: dividend,
                falseBranch: BuiltinMul(dividend, negativeOne));

        var absDivisor =
            Expression.ConditionalInstance(
                condition: divisorNonNegative,
                trueBranch: divisor,
                falseBranch: BuiltinMul(divisor, negativeOne));

        // Compute absolute quotient using recursive helper
        var absQuotient = Int_divHelper(absDividend, absDivisor);

        // Signs match if both are non-negative or both are negative
        var signsMatch = BuiltinHelpers.ApplyBuiltinEqualBinary(dividendNonNegative, divisorNonNegative);

        // Apply sign to result
        var signedQuotient =
            Expression.ConditionalInstance(
                condition: signsMatch,
                trueBranch: absQuotient,
                falseBranch: BuiltinMul(absQuotient, negativeOne));

        // Final result: 0 if divisor is 0, otherwise the computed quotient
        return Expression.ConditionalInstance(
            condition: divisorIsZero,
            trueBranch: zero,
            falseBranch: signedQuotient);
    }

    /// <summary>
    /// Helper for integer division that computes abs(dividend) / abs(divisor)
    /// using a recursive approach with scaling by 16 for better performance.
    /// </summary>
    /// <remarks>
    /// The algorithm works by:
    /// 1. Scaling the divisor by 16 and recursing if scaledDivisor &lt;= dividend
    /// 2. If divisor &lt;= dividend, subtract divisor and increment quotient
    /// 3. Return accumulated quotient when divisor &gt; dividend
    /// </remarks>
    private static Expression Int_divHelper(Expression absDividend, Expression absDivisor)
    {
        /*
        idivHelper : Int -> Int -> Int -> Int
        idivHelper dividend divisor quotient =
            let
                scaledDivisor =
                    Pine_kernel.int_mul [ divisor, 16 ]
            in
            if Pine_kernel.int_is_sorted_asc [ scaledDivisor, dividend ] then
                let
                    scaledQuotient =
                        idivHelper dividend scaledDivisor 0

                    scaledQuotientSum =
                        Pine_kernel.int_mul [ scaledQuotient, 16 ]

                    remainder =
                        Pine_kernel.int_add
                            [ dividend
                            , Pine_kernel.int_mul [ scaledQuotient, scaledDivisor, -1 ]
                            ]

                    remainderQuotient =
                        idivHelper remainder divisor 0
                in
                Pine_kernel.int_add [ scaledQuotientSum, remainderQuotient ]

            else if Pine_kernel.int_is_sorted_asc [ divisor, dividend ] then
                idivHelper
                    (Pine_kernel.int_add [ dividend, Pine_kernel.int_mul [ divisor, -1 ] ])
                    divisor
                    (Pine_kernel.int_add [ quotient, 1 ])

            else
                quotient
        */

        // Build the recursive helper function using ParseAndEval
        // Environment structure: [envFunctions, [dividend, divisor, quotient]]
        // envFunctions[0] = the helper function itself (for recursion)

        // References within the recursive function body:
        // env[0] = envFunctions list, env[0][0] = self (idivHelper)
        // env[1] = args list, env[1][0] = dividend, env[1][1] = divisor, env[1][2] = quotient

        var envFunctionsExpr = ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);
        var selfFunctionExpr = ExpressionBuilder.BuildExpressionForPathInExpression([0, 0], Expression.EnvironmentInstance);
        var dividendExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1, 0], Expression.EnvironmentInstance);
        var divisorExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1, 1], Expression.EnvironmentInstance);
        var quotientExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1, 2], Expression.EnvironmentInstance);

        var zero = LiteralInt(0);
        var one = LiteralInt(1);
        var negativeOne = LiteralInt(-1);
        var sixteen = LiteralInt(16);

        // scaledDivisor = divisor * 16
        var scaledDivisor = BuiltinMul(divisorExpr, sixteen);

        // Build recursive call helper: invoke self with new args [dividend, divisor, quotient]
        static Expression RecursiveCall(
            Expression selfFunc,
            Expression envFuncs,
            Expression dividend,
            Expression divisor,
            Expression quotient)
        {
            // Build environment: [envFunctions, [dividend, divisor, quotient]]
            var newEnv = Expression.ListInstance([
                envFuncs,
                Expression.ListInstance([dividend, divisor, quotient])
            ]);

            return new Expression.ParseAndEval(
                encoded: selfFunc,
                environment: newEnv);
        }

        // Case 1: scaledDivisor <= dividend (use scaling approach)
        // scaledQuotient = idivHelper dividend scaledDivisor 0
        var scaledQuotient = RecursiveCall(selfFunctionExpr, envFunctionsExpr, dividendExpr, scaledDivisor, zero);

        // scaledQuotientSum = scaledQuotient * 16
        var scaledQuotientSum = BuiltinMul(scaledQuotient, sixteen);

        // remainder = dividend - scaledQuotient * scaledDivisor
        //           = dividend + (-1) * scaledQuotient * scaledDivisor
        var remainder = BuiltinAdd(
            dividendExpr,
            BuiltinMul(scaledQuotient, BuiltinMul(scaledDivisor, negativeOne)));

        // remainderQuotient = idivHelper remainder divisor 0
        var remainderQuotient = RecursiveCall(selfFunctionExpr, envFunctionsExpr, remainder, divisorExpr, zero);

        // result = scaledQuotientSum + remainderQuotient
        var scalingResult = BuiltinAdd(scaledQuotientSum, remainderQuotient);

        // Case 2: divisor <= dividend (subtract once and increment)
        // newDividend = dividend - divisor
        var newDividend = BuiltinAdd(dividendExpr, BuiltinMul(divisorExpr, negativeOne));

        // newQuotient = quotient + 1
        var newQuotient = BuiltinAdd(quotientExpr, one);

        // recursive call with (dividend - divisor, divisor, quotient + 1)
        var subtractResult = RecursiveCall(selfFunctionExpr, envFunctionsExpr, newDividend, divisorExpr, newQuotient);

        // Case 3: divisor > dividend (base case, return quotient)
        var baseCase = quotientExpr;

        // Build the conditional:
        // if scaledDivisor <= dividend then scalingResult
        // else if divisor <= dividend then subtractResult
        // else baseCase

        // int_is_sorted_asc [a, b] means a <= b
        var scaledDivisorLeDividend = BuiltinIntIsSortedAsc(scaledDivisor, dividendExpr);
        var divisorLeDividend = BuiltinIntIsSortedAsc(divisorExpr, dividendExpr);

        var innerBody = Expression.ConditionalInstance(
            condition: scaledDivisorLeDividend,
            trueBranch: scalingResult,
            falseBranch: Expression.ConditionalInstance(
                condition: divisorLeDividend,
                trueBranch: subtractResult,
                falseBranch: baseCase));

        // Encode the function body as a value
        var encodedBody = ExpressionEncoding.EncodeExpressionAsValue(innerBody);

        // Now we need to invoke this function with the initial arguments
        // The function expects: [envFunctions, [dividend, divisor, quotient]]
        // envFunctions[0] = the function itself

        // Create a self-referential structure by putting the encoded body in envFunctions
        var envFunctions = Expression.ListInstance([Expression.LiteralInstance(encodedBody)]);
        var initialArgs = Expression.ListInstance([absDividend, absDivisor, zero]);
        var initialEnv = Expression.ListInstance([envFunctions, initialArgs]);

        return new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(encodedBody),
            environment: initialEnv);
    }

    /// <summary>
    /// modBy : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy"/>
    /// </para>
    /// </summary>
    public static Expression Int_modBy(
        Expression divisor,
        Expression dividend)
    {
        /*
        modBy : Int -> Int -> Int
        modBy divisor dividend =
            if Pine_kernel.equal [ divisor, 1 ] then
                0
            else
                let
                    remainder =
                        remainderBy divisor dividend
                in
                if Pine_kernel.int_is_sorted_asc [ 0, remainder ] then
                    remainder
                else
                    Pine_kernel.int_add [ remainder, divisor ]
         * */

        var zero = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0));
        var one = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1));

        // Check if divisor is 1
        var divisorIsOne = BuiltinHelpers.ApplyBuiltinEqualBinary(divisor, one);

        // Compute remainder
        var remainder = Int_remainderBy(divisor, dividend);

        // Check if remainder is non-negative
        var remainderNonNegative = BuiltinIntIsSortedAsc(zero, remainder);

        // If remainder is non-negative, return it; otherwise add divisor
        var adjustedRemainder =
            Expression.ConditionalInstance(
                condition: remainderNonNegative,
                trueBranch: remainder,
                falseBranch: BuiltinAdd(remainder, divisor));

        // If divisor is 1, return 0; otherwise return adjusted remainder
        return Expression.ConditionalInstance(
            condition: divisorIsOne,
            trueBranch: zero,
            falseBranch: adjustedRemainder);
    }

    /// <summary>
    /// remainderBy : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#remainderBy"/>
    /// </para>
    /// </summary>
    public static Expression Int_remainderBy(
        Expression divisor,
        Expression dividend)
    {
        /*
        remainderBy : Int -> Int -> Int
        remainderBy divisor dividend =
            if Pine_kernel.equal [ divisor, 1 ] then
                0
            else
                Pine_kernel.int_add
                    [ dividend
                    , Pine_kernel.int_mul
                        [ -1
                        , Pine_kernel.int_mul
                            [ divisor
                            , idiv dividend divisor
                            ]
                        ]
                    ]
         * */

        var zero = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0));
        var one = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1));
        var negativeOne = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(-1));

        // Check if divisor is 1
        var divisorIsOne = BuiltinHelpers.ApplyBuiltinEqualBinary(divisor, one);

        // Compute quotient using Int_div
        var quotient = Int_div(dividend, divisor);

        // remainder = dividend - divisor * quotient
        // = dividend + (-1) * (divisor * quotient)
        var remainder =
            BuiltinAdd(
                dividend,
                BuiltinMul(
                    negativeOne,
                    BuiltinMul(divisor, quotient)));

        // If divisor is 1, return 0; otherwise return remainder
        return Expression.ConditionalInstance(
            condition: divisorIsOne,
            trueBranch: zero,
            falseBranch: remainder);
    }

    /// <summary>
    /// negate : number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#negate"/>
    /// </para>
    /// </summary>
    public static Expression Number_negate(Expression number)
    {
        return
            ApplyUnary(
                encodedFunctionValue: Negate_FunctionValue(),
                argument: number);
    }

    /// <summary>
    /// negate : number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#negate"/>
    /// </para>
    /// </summary>
    public static PineValue Negate_FunctionValue()
    {
        /*
        negate : number -> number
        negate n =
            case n of
                Elm_Float numerator denominator ->
                    Elm_Float
                        (Pine_kernel.int_mul [ -1, numerator ])
                        denominator

                _ ->
                    Pine_kernel.int_mul [ -1, n ]
         * */

        var n = Expression.EnvironmentInstance;

        // Check if the value is a float by checking if head(n) equals "Elm_Float"
        var isFloatCondition =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(n),
                s_elmFloatTypeTagNameLiteral);

        // For float: access numerator and denominator from [Elm_Float, [numerator, denominator]]
        // numerator is at path head(head(skip(1, n)))
        // denominator is at path head(skip(1, head(skip(1, n))))
        var tagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, n));
        var numerator = BuiltinHelpers.ApplyBuiltinHead(tagArgs);
        var denominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, tagArgs));

        // Negate the numerator
        var negatedNumerator =
            BuiltinMul(
                LiteralInt(-1),
                numerator);

        // Construct the negated float: [Elm_Float, [negatedNumerator, denominator]]
        var negatedFloat =
            Expression.ListInstance(
                [
                    s_elmFloatTypeTagNameLiteral,
                    Expression.ListInstance([negatedNumerator, denominator])
                ]);

        // For non-float: just multiply by -1
        var negatedInt =
            BuiltinMul(
                LiteralInt(-1),
                n);

        var asExpr =
            Expression.ConditionalInstance(
                condition: isFloatCondition,
                trueBranch: negatedFloat,
                falseBranch: negatedInt);

        return
            ExpressionEncoding.EncodeExpressionAsValue(asExpr);
    }

    /// <summary>
    /// (-) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(-)"/>
    /// </para>
    /// </summary>
    public static Expression Generic_Subtract(
        Expression minuend,
        Expression subtrahend)
    {
        /*
         * Float subtraction:
         * a/b - c/d = (a*d - c*b) / (b*d)
         * 
         * Float - Int:
         * a/b - c = (a - c*b) / b
         * 
         * Int - Float:
         * a - c/d = (a*d - c) / d
         */

        // Check if minuend is a float
        var minuendIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(minuend),
                s_elmFloatTypeTagNameLiteral);

        // Check if subtrahend is a float
        var subtrahendIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(subtrahend),
                s_elmFloatTypeTagNameLiteral);

        // Extract float components for minuend
        var minuendTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, minuend));
        var minuendNumerator = BuiltinHelpers.ApplyBuiltinHead(minuendTagArgs);
        var minuendDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, minuendTagArgs));

        // Extract float components for subtrahend
        var subtrahendTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, subtrahend));
        var subtrahendNumerator = BuiltinHelpers.ApplyBuiltinHead(subtrahendTagArgs);
        var subtrahendDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, subtrahendTagArgs));

        // Int - Int case (original implementation)
        var intSubtraction =
            BuiltinAdd(
                minuend,
                BuiltinMul(
                    subtrahend,
                    LiteralInt(-1)));

        // Float - Float case: (numA * denomB - numB * denomA) / (denomA * denomB)
        var floatFloatNumerator =
            BuiltinAdd(
                BuiltinMul(minuendNumerator, subtrahendDenominator),
                BuiltinMul(
                    LiteralInt(-1),
                    BuiltinMul(subtrahendNumerator, minuendDenominator)));
        var floatFloatDenominator = BuiltinMul(minuendDenominator, subtrahendDenominator);
        var floatFloatResult =
            Expression.ListInstance(
                [
                    s_elmFloatTypeTagNameLiteral,
                    Expression.ListInstance([floatFloatNumerator, floatFloatDenominator])
                ]);

        // Float - Int case: (numA - intB * denomA) / denomA
        var floatIntNumerator =
            BuiltinAdd(
                minuendNumerator,
                BuiltinMul(
                    LiteralInt(-1),
                    BuiltinMul(subtrahend, minuendDenominator)));
        var floatIntResult =
            Expression.ListInstance(
                [
                    s_elmFloatTypeTagNameLiteral,
                    Expression.ListInstance([floatIntNumerator, minuendDenominator])
                ]);

        // Int - Float case: (intA * denomB - numB) / denomB
        var intFloatNumerator =
            BuiltinAdd(
                BuiltinMul(minuend, subtrahendDenominator),
                BuiltinMul(
                    LiteralInt(-1),
                    subtrahendNumerator));
        var intFloatResult =
            Expression.ListInstance(
                [
                    s_elmFloatTypeTagNameLiteral,
                    Expression.ListInstance([intFloatNumerator, subtrahendDenominator])
                ]);

        // When minuend is float
        var minuendFloatBranch =
            Expression.ConditionalInstance(
                condition: subtrahendIsFloat,
                trueBranch: floatFloatResult,
                falseBranch: floatIntResult);

        // When minuend is not float
        var minuendNotFloatBranch =
            Expression.ConditionalInstance(
                condition: subtrahendIsFloat,
                trueBranch: intFloatResult,
                falseBranch: intSubtraction);

        return
            Expression.ConditionalInstance(
                condition: minuendIsFloat,
                trueBranch: minuendFloatBranch,
                falseBranch: minuendNotFloatBranch);
    }

    /// <summary>
    /// (-) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(-)"/>
    /// </para>
    /// </summary>
    public static PineValue Subtract_FunctionValue()
    {
        var leftExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1, 0],
                Expression.EnvironmentInstance);

        var rightExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1, 1],
                Expression.EnvironmentInstance);

        var asExpr =
            Generic_Subtract(leftExpr, rightExpr);

        var wrappedExpr =
            FunctionValueBuilder.EmitFunctionValue(
                asExpr,
                parameterCount: 2,
                envFunctions: []);

        return wrappedExpr;
    }

    private static Expression BuiltinAdd(Expression left, Expression right)
    {
        return
            BuiltinHelpers.ApplyBuiltinIntAdd([left, right,]);
    }

    private static Expression BuiltinMul(Expression left, Expression right)
    {
        return
            BuiltinHelpers.ApplyBuiltinIntMul([left, right]);
    }

    private static Expression.Literal LiteralInt(long value) =>
        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(value));

    private static Expression.KernelApplication BuiltinIntIsSortedAsc(Expression left, Expression right)
    {
        return
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_is_sorted_asc),
                input:
                Expression.ListInstance(
                    [
                    left,
                    right
                    ]));
    }

    /// <summary>
    /// (+) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(+)"/>
    /// </para>
    /// </summary>
    public static PineValue Add_FunctionValue()
    {
        var leftExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1, 0],
                Expression.EnvironmentInstance);

        var rightExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1, 1],
                Expression.EnvironmentInstance);

        var asExpr = BuiltinAdd(leftExpr, rightExpr);

        var wrappedExpr =
            FunctionValueBuilder.EmitFunctionValue(
                asExpr,
                parameterCount: 2,
                envFunctions: []);

        return wrappedExpr;
    }

    private static Expression.ParseAndEval ApplyUnary(
        PineValue encodedFunctionValue,
        Expression argument)
    {
        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(encodedFunctionValue),
                environment: argument);
    }

    private readonly static Expression s_envFirstItem =
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [0],
            Expression.EnvironmentInstance);

    private readonly static Expression s_envSecondItem =
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [1],
            Expression.EnvironmentInstance);

    private static readonly Expression s_elmFloatTypeTagNameLiteral =
        Expression.LiteralInstance(ElmValue.ElmFloatTypeTagNameAsValue);
}
