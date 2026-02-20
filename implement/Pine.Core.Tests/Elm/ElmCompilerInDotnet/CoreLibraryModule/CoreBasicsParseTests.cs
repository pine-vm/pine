using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreBasicsParseTests
{
    [Fact]
    public void Function_number_add_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x + 41

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.add
                    param_1_0
                    41
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer and float values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.Integer(51));
        }

        // Test with another integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(-50));

            resultValue.Should().Be(ElmValue.Integer(-9));
        }

        // Test with float value (which results in float)
        {
            var resultValue = ApplyForElmArgument(ElmValue.ElmFloat.Convert(1.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(42.5));
        }

        // Test with another float value
        {
            var resultValue = ApplyForElmArgument(ElmValue.ElmFloat.Convert(-0.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(40.5));
        }
    }

    [Fact]
    public void Function_number_sub_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x - 17

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.sub
                    param_1_0
                    17
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer and float values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(50));

            resultValue.Should().Be(ElmValue.Integer(33));
        }

        // Test with another integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.Integer(-7));
        }

        // Test with float value
        {
            var resultValue = ApplyForElmArgument(ElmValue.ElmFloat.Convert(20.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(3.5));
        }
    }

    [Fact]
    public void Function_number_mul_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x * 7

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.mul
                    param_1_0
                    7
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer and float values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(6));

            resultValue.Should().Be(ElmValue.Integer(42));
        }

        // Test with another integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(-3));

            resultValue.Should().Be(ElmValue.Integer(-21));
        }

        // Test with float value
        {
            var resultValue = ApplyForElmArgument(ElmValue.ElmFloat.Convert(1.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(10.5));
        }
    }

    [Fact]
    public void Function_int_idiv_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                x // 5

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.idiv
                    param_1_0
                    5
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(27));

            resultValue.Should().Be(ElmValue.Integer(5));
        }

        // Test with another integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(-13));

            resultValue.Should().Be(ElmValue.Integer(-2));
        }

        // Test with zero
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(3));

            resultValue.Should().Be(ElmValue.Integer(0));
        }
    }

    [Fact]
    public void Function_int_modBy_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                modBy 7 x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.modBy
                    7
                    param_1_0
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with positive integer
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.Integer(1));
        }

        // Test with another integer
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(21));

            resultValue.Should().Be(ElmValue.Integer(0));
        }

        // Test with negative integer
        // Elm's modBy always returns non-negative: modBy 7 (-3) = (-3) % 7 + 7 = 4
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(-3));

            resultValue.Should().Be(ElmValue.Integer(4));
        }
    }

    [Fact]
    public void Function_int_remainderBy_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                remainderBy 7 x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.remainderBy
                    7
                    param_1_0
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with positive integer
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.Integer(1));
        }

        // Test with another integer
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(21));

            resultValue.Should().Be(ElmValue.Integer(0));
        }

        // Test with negative integer (remainderBy can return negative in Elm)
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(-3));

            resultValue.Should().Be(ElmValue.Integer(-3));
        }
    }

    [Fact]
    public void Function_number_add_prefix_operator()
    {
        // Test using the prefix operator syntax: (+) x 41
        // This should compile to the same code as: x + 41
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (+) x 41

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        // Should compile to the same code as the infix form
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.add
                    param_1_0
                    41
            
            """"
            .Trim());

        // Dynamic tests: invoke the function with integer and float values
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.Integer(51));
        }

        // Test with float value
        {
            var resultValue = ApplyForElmArgument(ElmValue.ElmFloat.Convert(1.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(42.5));
        }
    }

    [Fact]
    public void Function_number_sub_prefix_operator()
    {
        // Test using the prefix operator syntax: (-) 100 x
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (-) 100 x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.sub
                    100
                    param_1_0
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value: 100 - 30 = 70
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(30));

            resultValue.Should().Be(ElmValue.Integer(70));
        }
    }

    [Fact]
    public void Function_number_mul_prefix_operator()
    {
        // Test using the prefix operator syntax: (*) x 7
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (*) x 7

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.mul
                    param_1_0
                    7
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value: 6 * 7 = 42
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(6));

            resultValue.Should().Be(ElmValue.Integer(42));
        }
    }

    [Fact]
    public void Function_int_idiv_prefix_operator()
    {
        // Test using the prefix operator syntax: (//) x 5
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                (//) x 5

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.idiv
                    param_1_0
                    5
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with integer value: 27 // 5 = 5
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(27));

            resultValue.Should().Be(ElmValue.Integer(5));
        }
    }

    // ========== Tests for comparison operators ==========

    [Fact]
    public void Function_eq_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x == 42

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.eq
                    param_1_0
                    42
            
            """"
            .Trim());

        // Dynamic tests: invoke the function
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with matching integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(42));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with non-matching integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(41));

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_neq_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x /= 42

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.neq
                    param_1_0
                    42
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with non-matching integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(41));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with matching integer value
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(42));

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_lt_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x < 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.lt
                    param_1_0
                    10
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with value less than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with value equal to 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.FalseValue);
        }

        // Test with value greater than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_gt_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x > 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.gt
                    param_1_0
                    10
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with value less than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.FalseValue);
        }

        // Test with value greater than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.TrueValue);
        }
    }

    [Fact]
    public void Function_le_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x <= 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.le
                    param_1_0
                    10
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with value less than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with value equal to 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with value greater than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_ge_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                x >= 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.ge
                    param_1_0
                    10
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with value less than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.FalseValue);
        }

        // Test with value equal to 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test with value greater than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.TrueValue);
        }
    }

    [Fact]
    public void Function_compare_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                compare x 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.compare
                    param_1_0
                    10
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test with value less than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.TagInstance("LT", []));
        }

        // Test with value equal to 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(10));

            resultValue.Should().Be(ElmValue.TagInstance("EQ", []));
        }

        // Test with value greater than 10
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(15));

            resultValue.Should().Be(ElmValue.TagInstance("GT", []));
        }
    }

    // ========== Tests for prefix operator syntax ==========

    [Fact]
    public void Function_eq_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (==) x 42

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.eq
                    param_1_0
                    42
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_neq_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (/=) x 42

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.neq
                    param_1_0
                    42
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_lt_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (<) x 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.lt
                    param_1_0
                    10
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_gt_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (>) x 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.gt
                    param_1_0
                    10
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_le_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (<=) x 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.le
                    param_1_0
                    10
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_ge_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                (>=) x 10

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.ge
                    param_1_0
                    10
            
            """"
            .Trim());
    }

    // ========== Tests for && operator ==========

    [Fact]
    public void Function_and_infix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                x && y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , True
                        ]
                then
                    param_1_1

                else
                    False
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue x, ElmValue y)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x), ElmValueEncoding.ElmValueAsPineValue(y)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test True && True = True
        {
            var resultValue = ApplyForElmArguments(ElmValue.TrueValue, ElmValue.TrueValue);

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test True && False = False
        {
            var resultValue = ApplyForElmArguments(ElmValue.TrueValue, ElmValue.FalseValue);

            resultValue.Should().Be(ElmValue.FalseValue);
        }

        // Test False && True = False
        {
            var resultValue = ApplyForElmArguments(ElmValue.FalseValue, ElmValue.TrueValue);

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_and_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                (&&) x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , True
                        ]
                then
                    param_1_1

                else
                    False
            
            """"
            .Trim());
    }

    // ========== Tests for || operator ==========

    [Fact]
    public void Function_or_infix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                x || y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , True
                        ]
                then
                    True

                else
                    param_1_1
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue x, ElmValue y)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x), ElmValueEncoding.ElmValueAsPineValue(y)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test True || True = True
        {
            var resultValue = ApplyForElmArguments(ElmValue.TrueValue, ElmValue.TrueValue);

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test True || False = True
        {
            var resultValue = ApplyForElmArguments(ElmValue.TrueValue, ElmValue.FalseValue);

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test False || True = True
        {
            var resultValue = ApplyForElmArguments(ElmValue.FalseValue, ElmValue.TrueValue);

            resultValue.Should().Be(ElmValue.TrueValue);
        }

        // Test False || False = False
        {
            var resultValue = ApplyForElmArguments(ElmValue.FalseValue, ElmValue.FalseValue);

            resultValue.Should().Be(ElmValue.FalseValue);
        }
    }

    [Fact]
    public void Function_or_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                (||) x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , True
                        ]
                then
                    True

                else
                    param_1_1
            
            """"
            .Trim());
    }

    // ========== Tests for xor, not, min, max, negate, identity, always ==========

    [Fact]
    public void Function_xor()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                xor x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , param_1_1
                        ]
                then
                    False

                else
                    True
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_not()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                not x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.not
                    param_1_0
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_min()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                min x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Basics.min
                    param_1_0
                    param_1_1
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_max()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                max x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Basics.max
                    param_1_0
                    param_1_1
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_negate()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                negate x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.negate
                    param_1_0
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_identity()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                Basics.identity x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Basics.identity
                    param_1_0
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_always()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                always x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Basics.always
                    param_1_0
                    param_1_1
            
            """"
            .Trim());
    }

    // ========== Tests for ++ (append) operator ==========

    [Fact]
    public void Function_append_infix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                x ++ y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Basics.append
                    param_1_0
                    param_1_1
            
            """"
            .Trim());

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArguments(ElmValue x, ElmValue y)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x), ElmValueEncoding.ElmValueAsPineValue(y)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // Test string append: "hello" ++ " world" == "hello world"
        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.StringInstance("hello"),
                    ElmValue.StringInstance(" world"));

            resultValue.Should().Be(ElmValue.StringInstance("hello world"));
        }

        // Test list append: [1, 2] ++ [3, 4] == [1, 2, 3, 4]
        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]),
                    ElmValue.ListInstance([ElmValue.Integer(3), ElmValue.Integer(4)]));

            resultValue.Should().Be(
                ElmValue.ListInstance(
                    [
                    ElmValue.Integer(1),
                    ElmValue.Integer(2),
                    ElmValue.Integer(3),
                    ElmValue.Integer(4)
                    ]));
        }

        // Test empty string append: "" ++ "" == ""
        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.StringInstance(""),
                    ElmValue.StringInstance(""));

            resultValue.Should().Be(ElmValue.StringInstance(""));
        }

        // Test empty list append: [] ++ [] == []
        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ListInstance([]),
                    ElmValue.ListInstance([]));

            resultValue.Should().Be(ElmValue.ListInstance([]));
        }
    }

    [Fact]
    public void Function_append_prefix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x y =
                (++) x y

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Basics.append
                    param_1_0
                    param_1_1
            
            """"
            .Trim());
    }

    // ========== Tests for >> (composeR) operator ==========

    [Fact]
    public void Function_composeR_infix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            double x =
                x * 2


            addOne x =
                x + 1


            doubleAndAdd =
                double >> addOne


            alfa x =
                doubleAndAdd x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // double >> addOne: double(5) = 10, addOne(10) = 11
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.Integer(11));
        }

        // double >> addOne: double(0) = 0, addOne(0) = 1
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(0));

            resultValue.Should().Be(ElmValue.Integer(1));
        }
    }

    [Fact]
    public void Function_composeL_infix_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            double x =
                x * 2


            addOne x =
                x + 1


            addOneAndDouble =
                double << addOne


            alfa x =
                addOneAndDouble x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        // Dynamic tests
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        ElmValue ApplyForElmArgument(ElmValue x)
        {
            var pineValue =
                invokeFunction([ElmValueEncoding.ElmValueAsPineValue(x)]);

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue.evalReport.ReturnValue.Evaluate(), null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        // double << addOne: addOne(5) = 6, double(6) = 12
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(5));

            resultValue.Should().Be(ElmValue.Integer(12));
        }

        // double << addOne: addOne(0) = 1, double(1) = 2
        {
            var resultValue = ApplyForElmArgument(ElmValue.Integer(0));

            resultValue.Should().Be(ElmValue.Integer(2));
        }
    }
}
