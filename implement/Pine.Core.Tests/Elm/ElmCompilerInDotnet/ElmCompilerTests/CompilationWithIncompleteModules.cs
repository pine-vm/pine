using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class CompilationWithIncompleteModules
{
    [Fact]
    public void Incomplete_module_not_imported_does_not_cause_failure()
    {
        var mainModuleText =
            """"
            module Test exposing (..)

            import Alfa

            decl n =
                Alfa.someFunction n

            """";

        var alfaModuleText =
            """"
            module Alfa exposing (..)

            someFunction x =
                Pine_builtin.int_add
                    [ x
                    , 13
                    ]

            """";

        // With incomplete 'import' syntax

        var betaModuleText =
            """"
            module Beta exposing (..)

            import Gamma exposing (

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [mainModuleText, alfaModuleText, betaModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);
            return applyRunResult.ReturnValue.Evaluate();
        }

        {
            var result =
                ApplyForArgument(IntegerEncoding.EncodeSignedInteger(29));

            var resultAsInteger =
                IntegerEncoding.ParseSignedIntegerStrict(result)
                .Extract(err => throw new Exception("Failed decoding result as integer: " + err));

            resultAsInteger.Should().Be(42);
        }
    }

    [Fact]
    public void Incomplete_module_not_imported_by_dependencies_does_not_cause_failure()
    {
        var mainModuleText =
            """"
            module Test exposing (..)

            import Alfa

            decl n =
                Alfa.someFunction n

            """";

        var alfaModuleText =
            """"
            module Alfa exposing (..)

            someFunction x =
                Pine_builtin.int_add
                    [ x
                    , 13
                    ]

            """";

        // With incomplete 'import' syntax

        var betaModuleText =
            """"
            module Beta exposing (..)

            import Gamma exposing (

            """";

        // Gamma imports Beta, but Root does not import Beta or Gamma

        var gammaModuleText =
            """"
            module Gamma exposing (..)

            import Beta exposing (..)
            
            decl x =
                Beta.someOtherFunction x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [mainModuleText, alfaModuleText, betaModuleText, gammaModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);
            return applyRunResult.ReturnValue.Evaluate();
        }

        {
            var result =
                ApplyForArgument(IntegerEncoding.EncodeSignedInteger(29));

            var resultAsInteger =
                IntegerEncoding.ParseSignedIntegerStrict(result)
                .Extract(err => throw new Exception("Failed decoding result as integer: " + err));

            resultAsInteger.Should().Be(42);
        }
    }
}
