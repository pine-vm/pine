using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

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
/// The variants producing 'number' results, like <see cref="Generic_Add"/>, support both integers and floats,
/// and reduce the result to a plain integer when possible.
/// </para>
/// <para>
/// Does not contain addition or multiplication for integers, as those are translated directly to Pine_builtin functions.
/// </para>
/// </summary>
public class BasicArithmetic
{
    /// <summary>
    /// Identifies a complete binary function application expression.
    /// This method recognizes expressions like <c>Basics.add(x, 41)</c> in their compiled form.
    /// </summary>
    /// <param name="expr">The expression to analyze.</param>
    /// <returns>
    /// A tuple containing the function name and argument expressions if recognized,
    /// or null if the expression is not a recognized arithmetic operation.
    /// </returns>
    /// <remarks>
    /// This method expects a complete application structure:
    /// <code>
    /// ParseAndEval(
    ///     encoded: ParseAndEval(
    ///         encoded: Literal(functionValue),
    ///         environment: leftArg),
    ///     environment: rightArg)
    /// </code>
    /// For identifying just the function value itself (without application),
    /// use <see cref="IdentifyFunctionValue"/> instead.
    /// </remarks>
    public static (string declName, IReadOnlyList<Expression> argsExprs)? Identify(
        Expression expr)
    {
        if (TryParseAsBinaryApplication(expr) is { } binaryApp)
        {
            var (functionValue, leftExpr, rightExpr) = binaryApp;

            // Delegate to IdentifyFunctionValue to avoid duplicate comparison logic
            if (IdentifyFunctionValue(functionValue) is { } declName)
            {
                return (declName, [leftExpr, rightExpr]);
            }
        }

        return null;
    }

    /// <summary>
    /// Identifies a function value as one of the known Basics arithmetic function values.
    /// This is the primary identification method used during parsing to recognize
    /// function references in compiled expressions.
    /// </summary>
    /// <param name="functionValue">The Pine value to identify.</param>
    /// <returns>The name of the Basics function if recognized, null otherwise.</returns>
    /// <remarks>
    /// This method is used by the static program parser when it encounters a function
    /// reference and needs to identify which Basics function it represents.
    /// For identifying complete function applications (function + arguments),
    /// use <see cref="Identify"/> instead.
    /// </remarks>
    public static string? IdentifyFunctionValue(PineValue functionValue)
    {
        if (functionValue == Add_FunctionValue())
        {
            return "add";
        }

        if (functionValue == Sub_FunctionValue())
        {
            return "sub";
        }

        if (functionValue == Mul_FunctionValue())
        {
            return "mul";
        }

        if (functionValue == Int_div_FunctionValue())
        {
            return "idiv";
        }

        if (functionValue == Int_modBy_FunctionValue())
        {
            return "modBy";
        }

        if (functionValue == RemainderBy_FunctionValue())
        {
            return "remainderBy";
        }

        return null;
    }

    /// <summary>
    /// Gets function information for a Basics module function by name.
    /// This provides type information for type inference and a compilation function for code generation.
    /// </summary>
    /// <param name="functionName">The name of the Basics function (e.g., "add", "modBy").</param>
    /// <returns>
    /// A <see cref="CoreFunctionInfo"/> containing type and compilation information,
    /// or null if the function name is not recognized.
    /// </returns>
    public static CoreFunctionInfo? GetBasicsFunctionInfo(string functionName)
    {
        return functionName switch
        {
            // (+) : number -> number -> number
            "add" => new CoreFunctionInfo(
                [TypeInference.InferredType.Number(), TypeInference.InferredType.Number(), TypeInference.InferredType.Number()],
                args => Generic_Add(args[0], args[1])),

            // (-) : number -> number -> number
            "sub" => new CoreFunctionInfo(
                [TypeInference.InferredType.Number(), TypeInference.InferredType.Number(), TypeInference.InferredType.Number()],
                args => Generic_Sub(args[0], args[1])),

            // (*) : number -> number -> number
            "mul" => new CoreFunctionInfo(
                [TypeInference.InferredType.Number(), TypeInference.InferredType.Number(), TypeInference.InferredType.Number()],
                args => Generic_Mul(args[0], args[1])),

            // (//) : Int -> Int -> Int
            "idiv" => new CoreFunctionInfo(
                [TypeInference.InferredType.Int(), TypeInference.InferredType.Int(), TypeInference.InferredType.Int()],
                args => Int_div(args[0], args[1])),

            // modBy : Int -> Int -> Int
            "modBy" => new CoreFunctionInfo(
                [TypeInference.InferredType.Int(), TypeInference.InferredType.Int(), TypeInference.InferredType.Int()],
                args => Int_modBy(args[0], args[1])),

            // remainderBy : Int -> Int -> Int
            "remainderBy" => new CoreFunctionInfo(
                [TypeInference.InferredType.Int(), TypeInference.InferredType.Int(), TypeInference.InferredType.Int()],
                args => Int_remainderBy(args[0], args[1])),

            _ => null
        };
    }

    /// <summary>
    /// Maps an infix operator symbol to the corresponding Basics function name.
    /// </summary>
    /// <param name="operatorSymbol">The operator symbol (e.g., "+", "-", "*", "//").</param>
    /// <returns>The Basics function name, or null if not a recognized operator.</returns>
    public static string? OperatorToFunctionName(string operatorSymbol)
    {
        return operatorSymbol switch
        {
            "+" => "add",
            "-" => "sub",
            "*" => "mul",
            "//" => "idiv",
            _ => null
        };
    }

    /// <summary>
    /// Gets the encoded function value for a Basics function by name.
    /// This is used when emitting function values as literals for prefix operator syntax.
    /// </summary>
    /// <param name="functionName">The name of the Basics function (e.g., "add", "mul").</param>
    /// <returns>The encoded function value, or null if not recognized.</returns>
    public static PineValue? GetFunctionValue(string functionName)
    {
        return functionName switch
        {
            "add" =>
            Add_FunctionValue(),

            "sub" =>
            Sub_FunctionValue(),

            "mul" =>
            Mul_FunctionValue(),

            "idiv" =>
            Int_div_FunctionValue(),

            "modBy" =>
            Int_modBy_FunctionValue(),

            "remainderBy" =>
            RemainderBy_FunctionValue(),

            _ =>
            null
        };
    }

    /// <summary>
    /// Gets a compiled expression for a prefix operator (e.g., "(+)", "(-)").
    /// This returns the function value as a literal expression, ready for application.
    /// </summary>
    /// <param name="operatorSymbol">The operator symbol (e.g., "+", "-", "*", "//").</param>
    /// <returns>The function value as an Expression, or null if not a recognized operator.</returns>
    public static Expression? GetPrefixOperatorExpression(string operatorSymbol)
    {
        if (OperatorToFunctionName(operatorSymbol) is not { } functionName)
            return null;

        if (GetFunctionValue(functionName) is not { } functionValue)
            return null;

        return Expression.LiteralInstance(functionValue);
    }

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
    /// (+) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(+)"/>
    /// </para>
    /// </summary>
    public static Expression Generic_Add(
        Expression left,
        Expression right)
    {
        return
            BinaryApplication(
                functionValue: Add_FunctionValue(),
                left: left,
                right: right);
    }

    /// <summary>
    /// (+) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(+)"/>
    /// </para>
    /// </summary>
    public static PineValue Add_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Generic_Add);
    }

    /// <summary>
    /// (-) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(-)"/>
    /// </para>
    /// </summary>
    public static Expression Generic_Sub(
        Expression minuend,
        Expression subtrahend)
    {
        return
            BinaryApplication(
                functionValue: Sub_FunctionValue(),
                left: minuend,
                right: subtrahend);
    }

    /// <summary>
    /// (-) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(-)"/>
    /// </para>
    /// </summary>
    public static PineValue Sub_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Generic_Sub);
    }

    /// <summary>
    /// (*) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(*)"/>
    /// </para>
    /// </summary>
    public static Expression Generic_Mul(
        Expression left,
        Expression right)
    {
        return
            BinaryApplication(
                functionValue: Mul_FunctionValue(),
                left: left,
                right: right);
    }

    /// <summary>
    /// (*) : number -> number -> number
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(*)"/>
    /// </para>
    /// </summary>
    public static PineValue Mul_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Generic_Mul);
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
        return
            BinaryApplication(
                functionValue: Int_div_FunctionValue(),
                left: dividend,
                right: divisor);
    }

    /// <summary>
    /// (//) : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#(//)"/>
    /// </para>
    /// </summary>
    public static PineValue Int_div_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Int_div);
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
        return
            BinaryApplication(
                functionValue: Int_modBy_FunctionValue(),
                left: divisor,
                right: dividend);
    }

    /// <summary>
    /// modBy : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy"/>
    /// </para>
    /// </summary>
    public static PineValue Int_modBy_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Int_modBy);
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
        return
            BinaryApplication(
                functionValue: RemainderBy_FunctionValue(),
                left: divisor,
                right: dividend);
    }

    /// <summary>
    /// remainderBy : Int -> Int -> Int
    /// <para>
    /// <see href="https://package.elm-lang.org/packages/elm/core/latest/Basics#remainderBy"/>
    /// </para>
    /// </summary>
    public static PineValue RemainderBy_FunctionValue()
    {
        return BinaryFunctionValue(Internal_Int_remainderBy);
    }

    private static Expression Internal_Int_div(
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
    public static Expression Internal_Int_modBy(
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
        var remainder = Internal_Int_remainderBy(divisor, dividend);

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

    private static Expression Internal_Int_remainderBy(
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

    private static Expression Internal_Generic_Add(
        Expression augend,
        Expression addend)
    {
        /*
         * Float addition:
         * a/b + c/d = (a*d + c*b) / (b*d)
         * 
         * Float + Int:
         * a/b + c = (a + c*b) / b
         * 
         * Int + Float:
         * a + c/d = (a*d + c) / d
         */

        // Check if augend is a float
        var augendIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(augend),
                s_elmFloatTypeTagNameLiteral);

        // Check if addend is a float
        var addendIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(addend),
                s_elmFloatTypeTagNameLiteral);

        // Extract float components for augend
        var augendTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, augend));
        var augendNumerator = BuiltinHelpers.ApplyBuiltinHead(augendTagArgs);
        var augendDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, augendTagArgs));

        // Extract float components for addend
        var addendTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, addend));
        var addendNumerator = BuiltinHelpers.ApplyBuiltinHead(addendTagArgs);
        var addendDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, addendTagArgs));

        // Int + Int case: use builtin addition
        var intAddition = BuiltinAdd(augend, addend);

        // Float + Float case: (numA * denomB + numB * denomA) / (denomA * denomB)
        var floatFloatNumerator =
            BuiltinAdd(
                BuiltinMul(augendNumerator, addendDenominator),
                BuiltinMul(addendNumerator, augendDenominator));

        var floatFloatDenominator = BuiltinMul(augendDenominator, addendDenominator);
        var floatFloatResult = NormalizeFloatResult(floatFloatNumerator, floatFloatDenominator);

        // Float + Int case: (numA + intB * denomA) / denomA
        var floatIntNumerator =
            BuiltinAdd(
                augendNumerator,
                BuiltinMul(addend, augendDenominator));

        var floatIntResult = NormalizeFloatResult(floatIntNumerator, augendDenominator);

        // Int + Float case: (intA * denomB + numB) / denomB
        var intFloatNumerator =
            BuiltinAdd(
                BuiltinMul(augend, addendDenominator),
                addendNumerator);
        var intFloatResult = NormalizeFloatResult(intFloatNumerator, addendDenominator);

        // When augend is float
        var augendFloatBranch =
            Expression.ConditionalInstance(
                condition: addendIsFloat,
                trueBranch: floatFloatResult,
                falseBranch: floatIntResult);

        // When augend is not float
        var augendNotFloatBranch =
            Expression.ConditionalInstance(
                condition: addendIsFloat,
                trueBranch: intFloatResult,
                falseBranch: intAddition);

        return
            Expression.ConditionalInstance(
                condition: augendIsFloat,
                trueBranch: augendFloatBranch,
                falseBranch: augendNotFloatBranch);
    }

    private static Expression Internal_Generic_Mul(
        Expression left,
        Expression right)
    {
        /*
         * Float multiplication:
         * (a/b) * (c/d) = (a*c) / (b*d)
         * 
         * Float * Int:
         * (a/b) * c = (a*c) / b
         * 
         * Int * Float:
         * a * (c/d) = (a*c) / d
         */

        // Check if multiplicand is a float
        var multiplicandIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(left),
                s_elmFloatTypeTagNameLiteral);

        // Check if multiplier is a float
        var multiplierIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(right),
                s_elmFloatTypeTagNameLiteral);

        // Extract float components for multiplicand
        var multiplicandTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, left));
        var multiplicandNumerator = BuiltinHelpers.ApplyBuiltinHead(multiplicandTagArgs);
        var multiplicandDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, multiplicandTagArgs));

        // Extract float components for multiplier
        var multiplierTagArgs = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, right));
        var multiplierNumerator = BuiltinHelpers.ApplyBuiltinHead(multiplierTagArgs);
        var multiplierDenominator = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, multiplierTagArgs));

        // Int * Int case: use builtin multiplication
        var intMultiplication = BuiltinMul(left, right);

        // Float * Float case: (numA * numB) / (denomA * denomB)
        var floatFloatNumerator = BuiltinMul(multiplicandNumerator, multiplierNumerator);
        var floatFloatDenominator = BuiltinMul(multiplicandDenominator, multiplierDenominator);
        var floatFloatResult = NormalizeFloatResult(floatFloatNumerator, floatFloatDenominator);

        // Float * Int case: (numA * intB) / denomA
        var floatIntNumerator = BuiltinMul(multiplicandNumerator, right);
        var floatIntResult = NormalizeFloatResult(floatIntNumerator, multiplicandDenominator);

        // Int * Float case: (intA * numB) / denomB
        var intFloatNumerator = BuiltinMul(left, multiplierNumerator);
        var intFloatResult = NormalizeFloatResult(intFloatNumerator, multiplierDenominator);

        // When multiplicand is float
        var multiplicandFloatBranch =
            Expression.ConditionalInstance(
                condition: multiplierIsFloat,
                trueBranch: floatFloatResult,
                falseBranch: floatIntResult);

        // When multiplicand is not float
        var multiplicandNotFloatBranch =
            Expression.ConditionalInstance(
                condition: multiplierIsFloat,
                trueBranch: intFloatResult,
                falseBranch: intMultiplication);

        return
            Expression.ConditionalInstance(
                condition: multiplicandIsFloat,
                trueBranch: multiplicandFloatBranch,
                falseBranch: multiplicandNotFloatBranch);
    }

    private static Expression Internal_Generic_Sub(
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
        var floatFloatResult = NormalizeFloatResult(floatFloatNumerator, floatFloatDenominator);

        // Float - Int case: (numA - intB * denomA) / denomA
        var floatIntNumerator =
            BuiltinAdd(
                minuendNumerator,
                BuiltinMul(
                    LiteralInt(-1),
                    BuiltinMul(subtrahend, minuendDenominator)));

        var floatIntResult = NormalizeFloatResult(floatIntNumerator, minuendDenominator);

        // Int - Float case: (intA * denomB - numB) / denomB
        var intFloatNumerator =
            BuiltinAdd(
                BuiltinMul(minuend, subtrahendDenominator),
                BuiltinMul(
                    LiteralInt(-1),
                    subtrahendNumerator));
        var intFloatResult = NormalizeFloatResult(intFloatNumerator, subtrahendDenominator);

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

    private static PineValue BinaryFunctionValue(
        System.Func<Expression, Expression, Expression> buildFunctionBody)
    {
        // For WithoutEnvFunctions variant, env is directly the args list: [arg0, arg1]
        var leftExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [0],
                Expression.EnvironmentInstance);

        var rightExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1],
                Expression.EnvironmentInstance);

        var asExpr =
            buildFunctionBody(leftExpr, rightExpr);

        var wrappedExpr =
            FunctionValueBuilder.EmitFunctionValueWithoutEnvFunctions(
                asExpr,
                parameterCount: 2);

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
    /// Normalizes a float result by reducing it to an integer if the denominator divides evenly into the numerator.
    /// When numerator % denominator == 0, returns numerator / denominator as an integer.
    /// Otherwise, returns the float structure [Elm_Float, [numerator, denominator]].
    /// </summary>
    private static Expression NormalizeFloatResult(Expression numerator, Expression denominator)
    {
        // Check if denominator is 1
        var denominatorIsOne = BuiltinHelpers.ApplyBuiltinEqualBinary(denominator, LiteralInt(1));

        // Compute remainder of numerator / denominator to check if it divides evenly
        var remainder = Internal_Int_remainderBy(denominator, numerator);
        var dividesEvenly = BuiltinHelpers.ApplyBuiltinEqualBinary(remainder, LiteralInt(0));

        // Compute the quotient (numerator / denominator) for when it divides evenly
        var quotient = Int_div(numerator, denominator);

        // Float result: [Elm_Float, [numerator, denominator]]
        var floatResult =
            Expression.ListInstance(
                [
                    s_elmFloatTypeTagNameLiteral,
                    Expression.ListInstance([numerator, denominator])
                ]);

        // If denominator is 1, just return numerator as integer
        // If it divides evenly, return the quotient as integer
        // Otherwise, return the float structure
        return Expression.ConditionalInstance(
            condition: denominatorIsOne,
            trueBranch: numerator,
            falseBranch: Expression.ConditionalInstance(
                condition: dividesEvenly,
                trueBranch: quotient,
                falseBranch: floatResult));
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

    private static (PineValue functionValue, Expression left, Expression right)?
        TryParseAsBinaryApplication(Expression expr)
    {
        if (expr is Expression.ParseAndEval parseAndEvalRight)
        {
            var right = parseAndEvalRight.Environment;

            if (parseAndEvalRight.Encoded is Expression.ParseAndEval parseAndEvalLeft)
            {
                var left = parseAndEvalLeft.Environment;

                if (parseAndEvalLeft.Encoded is Expression.Literal functionLiteral)
                {
                    var functionValue = functionLiteral.Value;

                    return (functionValue, left, right);
                }
            }
        }

        return null;
    }

    private static Expression BinaryApplication(
        PineValue functionValue,
        Expression left,
        Expression right)
    {
        var leftApplied =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(functionValue),
                environment: left);

        var fullExpr =
            new Expression.ParseAndEval(
                encoded: leftApplied,
                environment: right);

        return fullExpr;
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
