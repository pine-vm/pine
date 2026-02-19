using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
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

        // Gamma imports Beta, but Root does not import Beta or Gamma.
        // Gamma and Beta are present in the file tree but are explicitly excluded
        // from the root file paths — so they are not in the dependency graph of Root,
        // and their failures do not affect compilation.

        var gammaModuleText =
            """"
            module Gamma exposing (..)

            import Beta exposing (..)
            
            decl x =
                Beta.someOtherFunction x

            """";

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [mainModuleText, alfaModuleText, betaModuleText, gammaModuleText]);

        // Only compile Test and Alfa; Gamma and Beta are in the tree but not roots.
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Test.elm"], ["src", "Alfa.elm"]];

        var parseCache = new PineVMParseCache();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                disableInlining: false)
            .Extract(err => throw new Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));

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
