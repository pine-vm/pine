using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Coverage for record update expressions whose record name refers to a module-level
/// declaration (in the same module or imported via an exposing list) instead of a local
/// binding or parameter, as in <c>{ initSetup | field = value }</c>.
/// <para>
/// Mirrors the failure deploying the Elm app from
/// <see href="https://github.com/Arcitectus/Sanderling/tree/main/implement/alternate-ui/source"/>:
/// <c>Unresolved reference 'initSetup' in module 'Backend.Main'</c>.
/// </para>
/// </summary>
public class RecordUpdateOnModuleLevelValueTests
{
    [Fact]
    public void Record_update_on_module_level_declaration_in_same_module()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            initSetup : { x : Int, y : Int }
            initSetup =
                { x = 11, y = 13 }


            decl : Int -> { x : Int, y : Int }
            decl b =
                { initSetup | y = b }

            """";

        var rendered =
            CompileAndApplyDecl(
                [elmModuleText],
                IntegerEncoding.EncodeSignedInteger(17));

        rendered.Should().Be("{ x = 11, y = 17 }");
    }

    [Fact]
    public void Record_update_on_module_level_declaration_without_type_annotation()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            initSetup =
                { x = 11, y = 13 }


            decl b =
                { initSetup | x = b }

            """";

        var rendered =
            CompileAndApplyDecl(
                [elmModuleText],
                IntegerEncoding.EncodeSignedInteger(19));

        rendered.Should().Be("{ x = 19, y = 13 }");
    }

    [Fact]
    public void Record_update_on_module_level_declaration_inside_lambda()
    {
        // Shape from Sanderling Backend.Main.maintainVolatileProcessTaskFromState:
        // the record update on a module-level value appears inside a lambda that
        // lambda lifting promotes to a top-level declaration.
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Setup =
                { x : Int, y : Int }


            initSetup : Setup
            initSetup =
                { x = 11, y = 13 }


            decl : List Int -> List ( Setup, List Int )
            decl list =
                List.map
                    (\b ->
                        ( { initSetup | y = b }
                        , []
                        )
                    )
                    list

            """";

        var rendered =
            CompileAndApplyDecl(
                [elmModuleText],
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(3),
                    IntegerEncoding.EncodeSignedInteger(5),
                    ]));

        rendered.Should().Be("[ ({ x = 11, y = 3 }, []), ({ x = 11, y = 5 }, []) ]");
    }

    [Fact]
    public void Record_update_on_value_imported_via_exposing_list()
    {
        var otherModuleText =
            """"
            module Other exposing (initSetup)


            initSetup : { x : Int, y : Int }
            initSetup =
                { x = 23, y = 29 }

            """";

        var elmModuleText =
            """"
            module Test exposing (..)

            import Other exposing (initSetup)


            decl : Int -> { x : Int, y : Int }
            decl b =
                { initSetup | y = b }

            """";

        var rendered =
            CompileAndApplyDecl(
                [otherModuleText, elmModuleText],
                IntegerEncoding.EncodeSignedInteger(31));

        rendered.Should().Be("{ x = 23, y = 31 }");
    }

    [Fact]
    public void Record_update_on_parameter_named_like_declaration_in_other_module()
    {
        // A parameter with the same name as a module-level declaration from another
        // module must keep referring to the parameter.
        var otherModuleText =
            """"
            module Other exposing (setup)


            setup : { x : Int, y : Int }
            setup =
                { x = 23, y = 29 }

            """";

        var elmModuleText =
            """"
            module Test exposing (..)

            import Other


            decl : { x : Int, y : Int } -> { x : Int, y : Int }
            decl setup =
                { setup | y = 41 }

            """";

        var rendered =
            CompileAndApplyDecl(
                [otherModuleText, elmModuleText],
                ElmValueEncoding.ElmRecordAsPineValue(
                    [
                    ("x", IntegerEncoding.EncodeSignedInteger(37)),
                    ("y", IntegerEncoding.EncodeSignedInteger(39)),
                    ]));

        rendered.Should().Be("{ x = 37, y = 41 }");
    }

    [Fact]
    public void Record_update_on_let_binding_named_like_module_level_declaration()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            initSetup : { x : Int, y : Int }
            initSetup =
                { x = 11, y = 13 }


            decl : Int -> { x : Int, y : Int }
            decl b =
                let
                    local =
                        { x = 43, y = 47 }
                in
                { local | y = b }

            """";

        var rendered =
            CompileAndApplyDecl(
                [elmModuleText],
                IntegerEncoding.EncodeSignedInteger(53));

        rendered.Should().Be("{ x = 43, y = 53 }");
    }

    private static string CompileAndApplyDecl(
        IReadOnlyList<string> elmModulesTexts,
        PineValue argument)
    {
        var parseCache = new PineVMParseCache();

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                elmModulesTexts,
                disableInlining: false);

        var declValue =
            parsedEnv.Modules
            .First(c => c.moduleName is "Test")
            .moduleContent.FunctionDeclarations
            .First(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing decl: " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        var (applyRunResult, _) = invokeFunction([argument]);

        var resultPine = applyRunResult.ReturnValue.Evaluate();

        return
            ElmValue.RenderAsElmExpression(
                ElmValueEncoding.PineValueAsElmValue(resultPine, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err)))
            .expressionString;
    }
}
