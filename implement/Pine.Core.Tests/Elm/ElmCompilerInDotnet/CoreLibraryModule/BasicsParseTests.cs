using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class BasicsParseTests
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
}
