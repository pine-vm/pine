using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class PartialApplicationTests
{
    [Fact]
    public void Simple_partial_application_two_parameters()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl a b =
                [ a, b, b, a ]


            partiallyApply a =
                decl a

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "partiallyApply");

        var invokeDecl = ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(declValue.Value, parseCache);

        // Apply first argument only

        var firstApplied =
            invokeDecl([IntegerEncoding.EncodeSignedInteger(13)]);

        {
            // Per compiler spec, return value from partial application should be an encoded expression.

            var parseAsExprResult =
                ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

            if (parseAsExprResult.IsErrOrNull() is { } parseErr)
            {
                throw new System.Exception(
                    $"Expected partially applied function to return an encoded expression, but parsing failed: {parseErr}");
            }

            if (parseAsExprResult.IsOkOrNull() is not { } parsedExpr)
            {
                throw new System.Exception(
                    $"Expected partially applied function to return an encoded expression, but parsing returned null.");
            }
        }

        // Apply second argument

        var (finalResult, _) =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
            ([IntegerEncoding.EncodeSignedInteger(17)]);

        finalResult.Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17),
                IntegerEncoding.EncodeSignedInteger(17),
                IntegerEncoding.EncodeSignedInteger(13)
                ]));
    }

    [Fact]
    public void Partial_application_three_parameters()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl a b c =
                [ a, c, b, a ]


            applyFirst a =
                decl a


            applyFirstAndSecond a b =
                decl a b


            applyTwoArgs func a b =
                func a b


            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var applyFirstDeclValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "applyFirst");

        var applyFirstInvokeDecl =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstDeclValue.Value, parseCache);

        var applyFirstAndSecondDeclValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "applyFirstAndSecond");

        var applyFirstAndSecondInvokeDecl =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstAndSecondDeclValue.Value, parseCache);

        var applyTwoArgsDeclValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "applyTwoArgs");

        var applyTwoArgsInvokeDecl =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyTwoArgsDeclValue.Value, parseCache);

        {
            // Test case 1: First apply with a single argument, then with two further arguments

            var firstApplied =
                applyFirstInvokeDecl([IntegerEncoding.EncodeSignedInteger(13)]);

            {
                // Use the test helper for the second batch of arguments

                var (finalResult, _) =
                    ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)(
                        [
                        IntegerEncoding.EncodeSignedInteger(17),
                        IntegerEncoding.EncodeSignedInteger(19)
                        ]);

                finalResult.Should().Be(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(13),
                        IntegerEncoding.EncodeSignedInteger(19),
                        IntegerEncoding.EncodeSignedInteger(17),
                        IntegerEncoding.EncodeSignedInteger(13)
                        ]));
            }

            {
                // Use the applyTwoArgs function to apply the two remaining arguments

                var (finalResult, _) =
                    applyTwoArgsInvokeDecl(
                        [
                        firstApplied.returnValue,
                        IntegerEncoding.EncodeSignedInteger(17),
                        IntegerEncoding.EncodeSignedInteger(19)
                        ]);


                finalResult.Should().Be(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(13),
                        IntegerEncoding.EncodeSignedInteger(19),
                        IntegerEncoding.EncodeSignedInteger(17),
                        IntegerEncoding.EncodeSignedInteger(13)
                        ]));
            }
        }
    }

    [Fact]
    public void Partial_application_emits_compact_template_without_generic_parse_and_eval_chain()
    {
        // For a partial application of a known (cross-SCC) function the compiler now emits a compact
        // template that builds the partially-applied function value structurally, instead of the generic
        // form that wraps the callee's function value in one ParseAndEval per applied argument. The
        // template producer is built purely from List/Literal nodes, so the emitted partial-application
        // expression contains no ParseAndEval nodes - regardless of how many leading arguments are
        // already supplied.
        var elmModuleText =
            """"
            module Test exposing (..)

            decl a b c =
                [ a, c, b, a ]

            applyFirst x =
                decl x

            applyFirstAndSecond x y =
                decl x y

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var testModule =
            parsedEnv.Modules.First(c => c.moduleName is "Test");

        static int CountParseAndEval(Expression expression) =>
            Expression.EnumerateSelfAndDescendants(expression)
            .Count(sub => sub is Expression.ParseAndEval);

        Expression DecodeDecl(string name)
        {
            var declValue =
                testModule.moduleContent.FunctionDeclarations
                .First(decl => decl.Key == name)
                .Value;

            return
                parseCache.ParseExpression(declValue)
                .Extract(err => throw new System.Exception("Failed parsing declaration expression: " + err));
        }

        // applyFirst (one of three arguments supplied) compiles to a single-parameter wrapper
        // ParseAndEval(Literal(bodyEncoded), [envFunctions, Environment]); the body, once decoded, is the
        // compact template producer and contains no ParseAndEval nodes.
        var applyFirstWrapper = DecodeDecl("applyFirst");
        applyFirstWrapper.Should().BeOfType<Expression.ParseAndEval>();

        var applyFirstBodyEncoded =
            ((Expression.Literal)((Expression.ParseAndEval)applyFirstWrapper).Encoded).Value;

        var applyFirstBody =
            parseCache.ParseExpression(applyFirstBodyEncoded)
            .Extract(err => throw new System.Exception("Failed parsing applyFirst body: " + err));

        CountParseAndEval(applyFirstBody)
            .Should().Be(0);

        // applyFirstAndSecond (two of three arguments supplied) compiles to a multi-parameter wrapper
        // that is itself a structural producer (a List expression building an encoding), again with no
        // ParseAndEval nodes anywhere in the emitted expression.
        CountParseAndEval(DecodeDecl("applyFirstAndSecond"))
            .Should().Be(0);
    }

    [Fact]
    public void Case_block_returning_various_functions()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl a =
                case a of
                    71 ->
                        beta

                    _ ->
                        gamma


            beta x =
                Pine_builtin.int_add [ x, 13 ]


            gamma x =
                Pine_builtin.int_mul [ x, 11 ]

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        // (returning function values produces dynamic expressions that can't be statically analyzed)
        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var invokeDecl = ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(declValue.Value, parseCache);

        {
            // Apply first argument only

            var firstApplied =
                invokeDecl([IntegerEncoding.EncodeSignedInteger(71)]);

            {
                // Per compiler spec, return value from partial application should be an encoded expression.

                var parseAsExprResult =
                    ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

                if (parseAsExprResult.IsErrOrNull() is { } parseErr)
                {
                    throw new System.Exception(
                        $"Expected partially applied function to return an encoded expression, but parsing failed: {parseErr}");
                }

                if (parseAsExprResult.IsOkOrNull() is not { } parsedExpr)
                {
                    throw new System.Exception(
                        $"Expected partially applied function to return an encoded expression, but parsing returned null.");
                }
            }

            // Apply second argument

            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
                ([IntegerEncoding.EncodeSignedInteger(17)]);

            finalResult.Should().Be(
                IntegerEncoding.EncodeSignedInteger(17 + 13));
        }

        {
            var firstApplied =
                invokeDecl([IntegerEncoding.EncodeSignedInteger(42)]);

            {
                // Per compiler spec, return value from partial application should be an encoded expression.
                var parseAsExprResult =
                    ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

                if (parseAsExprResult.IsErrOrNull() is { } parseErr)
                {
                    throw new System.Exception(
                        $"Expected partially applied function to return an encoded expression, but parsing failed: {parseErr}");
                }

                if (parseAsExprResult.IsOkOrNull() is not { } parsedExpr)
                {
                    throw new System.Exception(
                        $"Expected partially applied function to return an encoded expression, but parsing returned null.");
                }

                // Apply second argument

                var (finalResult, _) =
                    ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
                    ([IntegerEncoding.EncodeSignedInteger(17)]);

                finalResult.Should().Be(
                    IntegerEncoding.EncodeSignedInteger(17 * 11));
            }
        }
    }
}
