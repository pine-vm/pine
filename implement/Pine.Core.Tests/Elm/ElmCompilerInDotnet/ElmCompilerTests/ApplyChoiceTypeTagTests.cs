using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class ApplyChoiceTypeTagTests
{
    [Fact]
    public void Simple_function_applying_tag()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = NoArgs
                | SingleArgInt Int
                | TwoArgsStringAndInt String Int
            

            alfa : ChoiceType
            alfa =
                NoArgs


            beta : Int -> ChoiceType
            beta x =
                SingleArgInt x


            gamma : Int -> String -> ChoiceType
            gamma x s =
                TwoArgsStringAndInt s x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.Namespaces.SequenceEqual(["Test"]),
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                NoArgs


            Test.beta param_1_0 =
                [ SingleArgInt
                , [ param_1_0
                  ]
                ]


            Test.gamma param_1_0 param_1_1 =
                [ TwoArgsStringAndInt
                , [ param_1_1
                  , param_1_0
                  ]
                ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Partial_application_choice_type_tag_single_argument()
    {
        // Test that a single-argument choice type tag can be used as a function value
        // and applied to an argument
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = NoArgs
                | SingleArgInt Int


            applyTag x =
                SingleArgInt x


            getTagAsFunction =
                SingleArgInt

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test getTagAsFunction - it's declared with no explicit parameters,
        // so calling it with 0 arguments returns a function value (the tag constructor)
        var getTagAsFunctionDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "getTagAsFunction");

        var invokeGetTagAsFunction =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(getTagAsFunctionDecl.Value, parseCache);

        // Call with 0 arguments to get the function value
        var (functionValue, _) = invokeGetTagAsFunction([]);

        // Now apply the argument to the function value
        var (result, _) =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(functionValue, parseCache)
            ([IntegerEncoding.EncodeSignedInteger(42)]);

        // The result should be [SingleArgInt, [42]]
        result.Should().Be(
            PineValue.List(
                [
                    StringEncoding.ValueFromString("SingleArgInt"),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(42)])
                ]));
    }

    [Fact]
    public void Partial_application_choice_type_tag_two_arguments()
    {
        // Test that a two-argument choice type tag can be partially applied
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = TwoArgs String Int


            applyFirstArg s =
                TwoArgs s


            applyBothArgs s x =
                TwoArgs s x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test applyFirstArg - should return a function value that accepts the second argument
        var applyFirstArgDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "applyFirstArg");

        var invokeApplyFirstArg =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstArgDecl.Value, parseCache);

        // Apply first argument (the string)
        var firstApplied =
            invokeApplyFirstArg([StringEncoding.ValueFromString("hello")]);

        // The return value should be an encoded expression (a function that accepts more arguments)
        var parseAsExprResult =
            ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

        if (parseAsExprResult.IsErrOrNull() is { } parseErr)
        {
            throw new System.Exception(
                $"Expected partially applied tag to return an encoded expression, but parsing failed: {parseErr}");
        }

        if (parseAsExprResult.IsOkOrNull() is not { } parsedExpr)
        {
            throw new System.Exception(
                $"Expected partially applied tag to return an encoded expression, but parsing returned null.");
        }

        // Apply second argument (the int)
        var (finalResult, _) =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
            ([IntegerEncoding.EncodeSignedInteger(42)]);

        // The result should be [TwoArgs, ["hello", 42]]
        finalResult.Should().Be(
            PineValue.List(
                [
                    StringEncoding.ValueFromString("TwoArgs"),
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString("hello"),
                            IntegerEncoding.EncodeSignedInteger(42)
                        ])
                ]));
    }

    [Fact]
    public void Partial_application_choice_type_tag_three_arguments()
    {
        // Test that a three-argument choice type tag can be partially applied
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = ThreeArgs Int String Int


            applyFirstArg a =
                ThreeArgs a


            applyFirstTwoArgs a b =
                ThreeArgs a b


            applyAllArgs a b c =
                ThreeArgs a b c

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test applyFirstArg - apply one arg to function, then apply remaining two args at once
        {
            var applyFirstArgDecl =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "applyFirstArg");

            var invokeApplyFirstArg =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstArgDecl.Value, parseCache);

            // Apply first argument to get a function value waiting for 2 more args
            var firstApplied =
                invokeApplyFirstArg([IntegerEncoding.EncodeSignedInteger(10)]);

            // Should be an encoded expression (function value)
            var parseAsExprResult1 =
                ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

            if (parseAsExprResult1.IsErrOrNull() is { } parseErr1)
            {
                throw new System.Exception(
                    $"Expected partially applied tag after first arg to return an encoded expression, but parsing failed: {parseErr1}");
            }

            // Apply remaining two arguments at once
            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
                ([
                    StringEncoding.ValueFromString("middle"),
                    IntegerEncoding.EncodeSignedInteger(30)
                ]);

            // The result should be [ThreeArgs, [10, "middle", 30]]
            finalResult.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("ThreeArgs"),
                        PineValue.List(
                            [
                                IntegerEncoding.EncodeSignedInteger(10),
                                StringEncoding.ValueFromString("middle"),
                                IntegerEncoding.EncodeSignedInteger(30)
                            ])
                    ]));
        }

        // Test applyFirstTwoArgs - apply two args, then the third
        {
            var applyFirstTwoArgsDecl =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "applyFirstTwoArgs");

            var invokeApplyFirstTwoArgs =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstTwoArgsDecl.Value, parseCache);

            // Apply first two arguments
            var twoArgsApplied =
                invokeApplyFirstTwoArgs(
                    [
                        IntegerEncoding.EncodeSignedInteger(100),
                        StringEncoding.ValueFromString("test")
                    ]);

            // Should be an encoded expression
            var parseAsExprResult =
                ExpressionEncoding.ParseExpressionFromValue(twoArgsApplied.returnValue);

            if (parseAsExprResult.IsErrOrNull() is { } parseErr)
            {
                throw new System.Exception(
                    $"Expected partially applied tag after two args to return an encoded expression, but parsing failed: {parseErr}");
            }

            // Apply third argument
            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(twoArgsApplied.returnValue, parseCache)
                ([IntegerEncoding.EncodeSignedInteger(200)]);

            // The result should be [ThreeArgs, [100, "test", 200]]
            finalResult.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("ThreeArgs"),
                        PineValue.List(
                            [
                                IntegerEncoding.EncodeSignedInteger(100),
                                StringEncoding.ValueFromString("test"),
                                IntegerEncoding.EncodeSignedInteger(200)
                            ])
                    ]));
        }
    }

    [Fact]
    public void Cross_module_choice_type_tag_single_argument()
    {
        // Test that a single-argument choice type tag imported from another module
        // can be used as a function value
        var alfaModuleText =
            """"
            module Alfa exposing (..)


            type ChoiceType
                = NoArgs
                | SingleArgInt Int

            """";

        var testModuleText =
            """"
            module Test exposing (..)

            import Alfa exposing (..)


            getTagAsFunction =
                SingleArgInt


            applyTag x =
                SingleArgInt x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [alfaModuleText, testModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test getTagAsFunction - call with 0 args to get function value, then apply the argument
        var getTagAsFunctionDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "getTagAsFunction");

        var invokeGetTagAsFunction =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(getTagAsFunctionDecl.Value, parseCache);

        // Call with 0 arguments to get the function value
        var (functionValue, _) = invokeGetTagAsFunction([]);

        // Now apply the argument to the function value
        var (result, _) =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(functionValue, parseCache)
            ([IntegerEncoding.EncodeSignedInteger(42)]);

        // The result should be [SingleArgInt, [42]]
        result.Should().Be(
            PineValue.List(
                [
                    StringEncoding.ValueFromString("SingleArgInt"),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(42)])
                ]));
    }

    [Fact]
    public void Cross_module_choice_type_tag_two_arguments()
    {
        // Test that a two-argument choice type tag imported from another module
        // can be partially applied
        var alfaModuleText =
            """"
            module Alfa exposing (..)


            type ChoiceType
                = TwoArgs String Int

            """";

        var testModuleText =
            """"
            module Test exposing (..)

            import Alfa exposing (..)


            applyFirstArg s =
                TwoArgs s


            applyBothArgs s x =
                TwoArgs s x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [alfaModuleText, testModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test applyFirstArg - should return a function value that accepts the second argument
        var applyFirstArgDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "applyFirstArg");

        var invokeApplyFirstArg =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstArgDecl.Value, parseCache);

        // Apply first argument (the string)
        var firstApplied =
            invokeApplyFirstArg([StringEncoding.ValueFromString("hello")]);

        // The return value should be an encoded expression (a function that accepts more arguments)
        var parseAsExprResult =
            ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

        if (parseAsExprResult.IsErrOrNull() is { } parseErr)
        {
            throw new System.Exception(
                $"Expected partially applied tag to return an encoded expression, but parsing failed: {parseErr}");
        }

        // Apply second argument (the int)
        var (finalResult, _) =
            ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
            ([IntegerEncoding.EncodeSignedInteger(42)]);

        // The result should be [TwoArgs, ["hello", 42]]
        finalResult.Should().Be(
            PineValue.List(
                [
                    StringEncoding.ValueFromString("TwoArgs"),
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString("hello"),
                            IntegerEncoding.EncodeSignedInteger(42)
                        ])
                ]));
    }

    [Fact]
    public void Cross_module_choice_type_tag_three_arguments()
    {
        // Test that a three-argument choice type tag imported from another module
        // can be partially applied
        var alfaModuleText =
            """"
            module Alfa exposing (..)


            type ChoiceType
                = ThreeArgs Int String Int

            """";

        var testModuleText =
            """"
            module Test exposing (..)

            import Alfa exposing (..)


            applyFirstArg a =
                ThreeArgs a


            applyFirstTwoArgs a b =
                ThreeArgs a b


            applyAllArgs a b c =
                ThreeArgs a b c

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [alfaModuleText, testModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test applyFirstArg - apply one arg to function, then apply remaining two args at once
        {
            var applyFirstArgDecl =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "applyFirstArg");

            var invokeApplyFirstArg =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstArgDecl.Value, parseCache);

            // Apply first argument to get a function value waiting for 2 more args
            var firstApplied =
                invokeApplyFirstArg([IntegerEncoding.EncodeSignedInteger(10)]);

            // Should be an encoded expression (function value)
            var parseAsExprResult1 =
                ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

            if (parseAsExprResult1.IsErrOrNull() is { } parseErr1)
            {
                throw new System.Exception(
                    $"Expected partially applied tag after first arg to return an encoded expression, but parsing failed: {parseErr1}");
            }

            // Apply remaining two arguments at once
            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
                ([
                    StringEncoding.ValueFromString("middle"),
                    IntegerEncoding.EncodeSignedInteger(30)
                ]);

            // The result should be [ThreeArgs, [10, "middle", 30]]
            finalResult.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("ThreeArgs"),
                        PineValue.List(
                            [
                                IntegerEncoding.EncodeSignedInteger(10),
                                StringEncoding.ValueFromString("middle"),
                                IntegerEncoding.EncodeSignedInteger(30)
                            ])
                    ]));
        }

        // Test applyFirstTwoArgs - apply two args, then the third
        {
            var applyFirstTwoArgsDecl =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "applyFirstTwoArgs");

            var invokeApplyFirstTwoArgs =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(applyFirstTwoArgsDecl.Value, parseCache);

            // Apply first two arguments
            var twoArgsApplied =
                invokeApplyFirstTwoArgs(
                    [
                        IntegerEncoding.EncodeSignedInteger(100),
                        StringEncoding.ValueFromString("test")
                    ]);

            // Should be an encoded expression
            var parseAsExprResult =
                ExpressionEncoding.ParseExpressionFromValue(twoArgsApplied.returnValue);

            if (parseAsExprResult.IsErrOrNull() is { } parseErr)
            {
                throw new System.Exception(
                    $"Expected partially applied tag after two args to return an encoded expression, but parsing failed: {parseErr}");
            }

            // Apply third argument
            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(twoArgsApplied.returnValue, parseCache)
                ([IntegerEncoding.EncodeSignedInteger(200)]);

            // The result should be [ThreeArgs, [100, "test", 200]]
            finalResult.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("ThreeArgs"),
                        PineValue.List(
                            [
                                IntegerEncoding.EncodeSignedInteger(100),
                                StringEncoding.ValueFromString("test"),
                                IntegerEncoding.EncodeSignedInteger(200)
                            ])
                    ]));
        }
    }

    [Fact]
    public void Cross_module_choice_type_tag_qualified_reference()
    {
        // Test that a choice type tag can be referenced with qualified module name
        var alfaModuleText =
            """"
            module Alfa exposing (..)


            type ChoiceType
                = SingleArgInt Int
                | TwoArgs String Int

            """";

        var testModuleText =
            """"
            module Test exposing (..)

            import Alfa


            applyQualifiedSingleArg x =
                Alfa.SingleArgInt x


            applyQualifiedTwoArgs s x =
                Alfa.TwoArgs s x


            partialApplyQualifiedTwoArgs s =
                Alfa.TwoArgs s

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [alfaModuleText, testModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        // Test applyQualifiedSingleArg
        {
            var declValue =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "applyQualifiedSingleArg");

            var invokeDecl =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(declValue.Value, parseCache);

            var (result, _) = invokeDecl([IntegerEncoding.EncodeSignedInteger(42)]);

            result.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("SingleArgInt"),
                        PineValue.List([IntegerEncoding.EncodeSignedInteger(42)])
                    ]));
        }

        // Test partialApplyQualifiedTwoArgs - partial application with qualified tag reference
        {
            var declValue =
                testModule.moduleContent.FunctionDeclarations
                .FirstOrDefault(decl => decl.Key is "partialApplyQualifiedTwoArgs");

            var invokeDecl =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(declValue.Value, parseCache);

            // Apply first argument
            var firstApplied =
                invokeDecl([StringEncoding.ValueFromString("hello")]);

            // Should be an encoded expression (function value)
            var parseAsExprResult =
                ExpressionEncoding.ParseExpressionFromValue(firstApplied.returnValue);

            if (parseAsExprResult.IsErrOrNull() is { } parseErr)
            {
                throw new System.Exception(
                    $"Expected partially applied qualified tag to return an encoded expression, but parsing failed: {parseErr}");
            }

            // Apply second argument
            var (finalResult, _) =
                ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(firstApplied.returnValue, parseCache)
                ([IntegerEncoding.EncodeSignedInteger(99)]);

            finalResult.Should().Be(
                PineValue.List(
                    [
                        StringEncoding.ValueFromString("TwoArgs"),
                        PineValue.List(
                            [
                                StringEncoding.ValueFromString("hello"),
                                IntegerEncoding.EncodeSignedInteger(99)
                            ])
                    ]));
        }
    }
}
