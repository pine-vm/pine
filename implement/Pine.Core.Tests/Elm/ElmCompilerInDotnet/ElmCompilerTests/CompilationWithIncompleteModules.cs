using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class CompilationWithIncompleteModules
{
    [Theory]
    [InlineData("""'\u{D800}'""", "U+D800 is a UTF-16 surrogate")]
    [InlineData("\"\\u{D83D}\\u{DE00}\"", "Replace them with the single escape \\u{1F600}")]
    public void Selected_module_reports_invalid_escape_instead_of_losing_its_root(string literal, string message)
    {
        var source =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                ["module Test exposing (main)\n\nmain = " + literal + "\n"]);

        var byName = ElmCompiler.CompileInteractiveEnvironment(source, [DeclQualifiedName.Create(["Test"], "main")]);
        var byFile = ElmSourceCompilation.CompileInteractiveEnvironmentFromFiles(source, [["src", "Test.elm"]]);

        foreach (var error in new[] { byName.IsErrOrNull(), byFile.IsErrOrNull() })
        {
            error.Should().Contain("src/Test.elm").And.Contain("3:9:").And.Contain(message).And.NotContain(
                "was not found");
        }
    }

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

        var sourceTree =
            TestCase.FileTreeFromElmModulesWithoutPackages([mainModuleText, alfaModuleText, betaModuleText]);

        var compiledEnvironment =
            ElmCompiler.CompileInteractiveEnvironment(
                sourceTree,
                rootDeclarations: [DeclQualifiedName.Create(["Test"], "decl")])
            .Extract(error => throw new Exception(error)).compiledEnvValue;

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvironment)
            .Extract(error => throw new Exception(error));

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
            ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths: rootFilePaths)
            .Map(r => r.compiledEnvValue)
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
