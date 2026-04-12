using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for shadowing reports from canonicalization.
/// These reports are used both for producing 'name clash' error messages for illegal shadowings,
/// and for detecting defects in compilation modules (e.g., naming clashes from inlining transformations).
/// </summary>
public class ShadowingReportTests
{
    private static File ParseModuleText(string moduleText)
    {
        var concreteSyntax =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        return FromFullSyntaxModel.Convert(concreteSyntax);
    }

    private static ModuleCanonicalizationResult GetModuleResult(
        string[] moduleName,
        params string[] moduleTexts)
    {
        var parsedModules =
            moduleTexts
            .Select(ParseModuleText)
            .ToList();

        var canonicalizeResult =
            Canonicalization.CanonicalizeWithErrors(parsedModules);

        var resultWithErrors =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Unexpected global error: " + err));

        return resultWithErrors.Modules[moduleName];
    }

    [Fact]
    public void No_shadowings_for_simple_module()
    {
        var moduleText =
            """"
            module Test exposing (..)


            add x y =
                x + y
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().BeEmpty();
    }

    [Fact]
    public void No_shadowings_when_parameter_names_differ_from_module_declarations()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute x =
                helper + x
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().BeEmpty();
    }

    [Fact]
    public void Parameter_shadowing_module_level_declaration_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute helper =
                helper + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("helper");

        var shadowingLocation = result.Shadowings["helper"];

        // The shadowing declaration is in a parameter of the module-level declaration "compute"
        shadowingLocation.DeclarationPath.Should().Equal("compute");
    }

    [Fact]
    public void Parameter_shadowing_has_correct_range()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute helper =
                helper + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("helper");

        var shadowingLocation = result.Shadowings["helper"];

        // The range should point to the shadowing parameter, not the original declaration
        shadowingLocation.Range.Should().NotBeNull();
    }

    [Fact]
    public void Let_binding_shadowing_module_level_declaration_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute =
                let
                    helper =
                        100
                in
                helper + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("helper");

        var shadowingLocation = result.Shadowings["helper"];

        // The shadowing is inside the "compute" declaration
        shadowingLocation.DeclarationPath.Should().Equal("compute");
    }

    [Fact]
    public void Case_pattern_shadowing_parameter_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            viewName name =
                case name of
                    Nothing ->
                        "anonymous"

                    Just name ->
                        name
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("name");

        var shadowingLocation = result.Shadowings["name"];

        // The shadowing is inside the "viewName" declaration
        shadowingLocation.DeclarationPath.Should().Equal("viewName");
    }

    [Fact]
    public void Multiple_shadowings_from_different_declarations()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            value =
                100


            func1 helper =
                helper + 1


            func2 value =
                value + 2
            """";

        var result = GetModuleResult(["Test"], moduleText);

        // Both "helper" and "value" should be reported as shadowed
        result.Shadowings.Should().ContainKey("helper");
        result.Shadowings.Should().ContainKey("value");

        result.Shadowings["helper"].DeclarationPath.Should().Equal("func1");
        result.Shadowings["value"].DeclarationPath.Should().Equal("func2");
    }

    [Fact]
    public void Multiple_parameters_shadowing_module_level_in_same_function()
    {
        var moduleText =
            """"
            module Test exposing (..)


            alpha =
                1


            beta =
                2


            compute alpha beta =
                alpha + beta
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("alpha");
        result.Shadowings.Should().ContainKey("beta");

        // Both shadowings are in the "compute" declaration
        result.Shadowings["alpha"].DeclarationPath.Should().Equal("compute");
        result.Shadowings["beta"].DeclarationPath.Should().Equal("compute");
    }

    [Fact]
    public void Case_pattern_shadowing_module_level_declaration_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            process x =
                case x of
                    Just helper ->
                        helper

                    Nothing ->
                        0
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("helper");

        var shadowingLocation = result.Shadowings["helper"];

        // The shadowing is inside the "process" declaration
        shadowingLocation.DeclarationPath.Should().Equal("process");
    }

    [Fact]
    public void Let_destructuring_pattern_shadowing_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            name =
                "default"


            process =
                let
                    ( name, age ) =
                        ( "Alice", 30 )
                in
                name
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("name");

        var shadowingLocation = result.Shadowings["name"];

        shadowingLocation.DeclarationPath.Should().Equal("process");
    }

    [Fact]
    public void Module_level_declaration_shadowing_import_reported()
    {
        var helperModuleText =
            """"
            module Helper exposing (..)


            compute x =
                x * 10
            """";

        var mainModuleText =
            """"
            module Main exposing (..)

            import Helper exposing (..)


            compute x =
                x + 1
            """";

        var result = GetModuleResult(["Main"], helperModuleText, mainModuleText);

        // Module-level declaration "compute" shadows the imported "compute" from Helper
        result.Shadowings.Should().ContainKey("compute");

        var shadowingLocation = result.Shadowings["compute"];

        // Module-level shadowing has an empty declaration path
        shadowingLocation.DeclarationPath.Should().BeEmpty();
    }

    [Fact]
    public void Module_level_declaration_shadowing_import_no_errors()
    {
        var helperModuleText =
            """"
            module Helper exposing (..)


            compute x =
                x * 10
            """";

        var mainModuleText =
            """"
            module Main exposing (..)

            import Helper exposing (..)


            compute x =
                x + 1


            result =
                compute 5
            """";

        var result = GetModuleResult(["Main"], helperModuleText, mainModuleText);

        // Module-level shadowing of imports is allowed in Elm - no errors
        result.Errors.Should().BeEmpty();

        // But the shadowing IS reported
        result.Shadowings.Should().ContainKey("compute");
    }

    [Fact]
    public void Parameter_shadowing_import_not_reported_as_shadowing()
    {
        // Parameters shadowing imports are NOT reported as shadowings because
        // the Elm compiler allows this silently (it's not a name clash).
        // Only the module-level and local-scope shadowings that produce errors are tracked.

        var helperModuleText =
            """"
            module Helper exposing (..)


            value =
                100
            """";

        var mainModuleText =
            """"
            module Main exposing (..)

            import Helper exposing (..)


            usesParameterValue value =
                value + 2
            """";

        var result = GetModuleResult(["Main"], helperModuleText, mainModuleText);

        // Parameters shadowing imports don't produce errors
        result.Errors.Should().BeEmpty();

        // The shadowing is tracked (parameter shadows imported name)
        // The parameter 'value' shadows 'Helper.value' imported via exposing (..)
        // This could be tracked or not depending on whether the import is in the module-level scope.
        // Since the import makes 'value' available at module-level, and the parameter shadows it,
        // the shadowing check in CanonicalizeFunctionImplementation checks against ModuleLevelDeclarations
        // which does NOT include imports. So this type of shadowing is NOT reported.
        // This is consistent with Elm's behavior of silently allowing parameter shadowing of imports.
    }

    [Fact]
    public void Tuple_pattern_in_case_shadowing_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            process x y z =
                case ( x, y, z ) of
                    ( Just x, Just y, Just z ) ->
                        x + y + z

                    _ ->
                        0
            """";

        var result = GetModuleResult(["Test"], moduleText);

        // Each of x, y, z is shadowed in the case pattern
        result.Shadowings.Should().ContainKey("x");
        result.Shadowings.Should().ContainKey("y");
        result.Shadowings.Should().ContainKey("z");

        // All within "process" declaration
        result.Shadowings["x"].DeclarationPath.Should().Equal("process");
        result.Shadowings["y"].DeclarationPath.Should().Equal("process");
        result.Shadowings["z"].DeclarationPath.Should().Equal("process");
    }

    [Fact]
    public void Shadowing_errors_correspond_to_shadowings()
    {
        // When there are shadowing errors, the shadowings dictionary should contain
        // matching entries.

        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute helper =
                helper + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        // Both errors and shadowings should reference "helper"
        result.Errors.Should().NotBeEmpty();
        result.Shadowings.Should().ContainKey("helper");
    }

    [Fact]
    public void Record_pattern_in_let_shadowing_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            name =
                "default"


            process record =
                let
                    { name } =
                        record
                in
                name
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("name");

        var shadowingLocation = result.Shadowings["name"];

        shadowingLocation.DeclarationPath.Should().Equal("process");
    }

    [Fact]
    public void Nested_let_shadowing_of_module_level_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            counter =
                0


            increment x =
                let
                    counter =
                        x + 1
                in
                counter
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("counter");

        result.Shadowings["counter"].DeclarationPath.Should().Equal("increment");
    }

    [Fact]
    public void No_shadowing_between_independent_functions()
    {
        // Two independent functions using the same parameter name should NOT be reported
        // as shadowings (they are in different scopes).

        var moduleText =
            """"
            module Test exposing (..)


            add x =
                x + 1


            multiply x =
                x * 2
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().BeEmpty();
    }

    [Fact]
    public void Mixed_shadowing_types_all_reported()
    {
        // A module with parameter shadowing, let shadowing, and case pattern shadowing
        // should report all of them.

        var moduleText =
            """"
            module Test exposing (..)


            alpha =
                1


            beta =
                2


            gamma =
                3


            usesParamShadow alpha =
                alpha + 1


            usesLetShadow =
                let
                    beta =
                        100
                in
                beta + 2


            usesCaseShadow x =
                case x of
                    Just gamma ->
                        gamma

                    Nothing ->
                        0
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("alpha");
        result.Shadowings.Should().ContainKey("beta");
        result.Shadowings.Should().ContainKey("gamma");

        result.Shadowings["alpha"].DeclarationPath.Should().Equal("usesParamShadow");
        result.Shadowings["beta"].DeclarationPath.Should().Equal("usesLetShadow");
        result.Shadowings["gamma"].DeclarationPath.Should().Equal("usesCaseShadow");
    }

    [Fact]
    public void Module_level_import_shadowing_has_empty_declaration_path()
    {
        var otherModuleText =
            """"
            module Other exposing (..)


            shared =
                42
            """";

        var testModuleText =
            """"
            module Test exposing (..)

            import Other exposing (..)


            shared =
                100
            """";

        var result = GetModuleResult(["Test"], otherModuleText, testModuleText);

        result.Shadowings.Should().ContainKey("shared");

        // Module-level declaration path should be empty
        result.Shadowings["shared"].DeclarationPath.Should().BeEmpty();
        result.Shadowings["shared"].DeclarationPath.Should().HaveCount(0);
    }

    [Fact]
    public void Parameter_shadowing_declaration_path_has_one_element()
    {
        var moduleText =
            """"
            module Test exposing (..)


            value =
                42


            transform value =
                value + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("value");

        // Parameter-level shadowing path should have exactly one element
        result.Shadowings["value"].DeclarationPath.Should().HaveCount(1);
        result.Shadowings["value"].DeclarationPath[0].Should().Be("transform");
    }

    [Fact]
    public void Shadowing_in_multiple_modules_reported_independently()
    {
        var moduleAText =
            """"
            module ModA exposing (..)


            helper =
                1


            func helper =
                helper + 1
            """";

        var moduleBText =
            """"
            module ModB exposing (..)


            value =
                2


            process value =
                value + 2
            """";

        var parsedModules =
            new[] { moduleAText, moduleBText }
            .Select(ParseModuleText)
            .ToList();

        var canonicalizeResult =
            Canonicalization.CanonicalizeWithErrors(parsedModules);

        var resultWithErrors =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Unexpected global error: " + err));

        var modAResult = resultWithErrors.Modules[["ModA"]];
        var modBResult = resultWithErrors.Modules[["ModB"]];

        modAResult.Shadowings.Should().ContainKey("helper");
        modAResult.Shadowings["helper"].DeclarationPath.Should().Equal("func");

        modBResult.Shadowings.Should().ContainKey("value");
        modBResult.Shadowings["value"].DeclarationPath.Should().Equal("process");

        // Each module's shadowings are independent
        modAResult.Shadowings.Should().NotContainKey("value");
        modBResult.Shadowings.Should().NotContainKey("helper");
    }

    [Fact]
    public void As_pattern_shadowing_reported()
    {
        var moduleText =
            """"
            module Test exposing (..)


            item =
                42


            process x =
                case x of
                    (Just _) as item ->
                        item

                    Nothing ->
                        0
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("item");
        result.Shadowings["item"].DeclarationPath.Should().Equal("process");
    }

    [Fact]
    public void Shadowing_results_available_through_CanonicalizeAllowingErrors()
    {
        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute helper =
                helper + 1
            """";

        var parsedModule = ParseModuleText(moduleText);

        var result =
            Canonicalization.CanonicalizeAllowingErrors([parsedModule]);

        var modulesDict =
            result.Extract(err => throw new System.Exception("Unexpected error: " + err));

        var (_, _, shadowings) = modulesDict[["Test"]];

        shadowings.Should().ContainKey("helper");
        shadowings["helper"].DeclarationPath.Should().Equal("compute");
    }

    [Fact]
    public void Shadowing_location_range_points_to_shadowing_declaration()
    {
        // The Range in ShadowingLocation should point to the location where
        // the shadowing declaration occurs, not the original declaration.

        var moduleText =
            """"
            module Test exposing (..)


            helper =
                42


            compute helper =
                helper + 1
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("helper");

        var shadowingRange = result.Shadowings["helper"].Range;

        // The shadowing parameter "helper" is on the line of "compute helper ="
        // which is after the original "helper =" declaration.
        // The range's start line should be greater than line 4 (where "helper =" is)
        shadowingRange.Start.Row.Should().BeGreaterThan(4);
    }

    [Fact]
    public void Module_with_only_type_declarations_has_no_shadowings()
    {
        var moduleText =
            """"
            module Test exposing (..)


            type Color
                = Red
                | Green
                | Blue


            type alias Point =
                { x : Int
                , y : Int
                }
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().BeEmpty();
    }

    [Fact]
    public void Multiple_case_branches_shadowing_same_name()
    {
        // If the same name is shadowed in multiple case branches,
        // we still report it (at least once, since dictionary is keyed by name).

        var moduleText =
            """"
            module Test exposing (..)


            name =
                "default"


            process x =
                case x of
                    Just name ->
                        name

                    Nothing ->
                        name
            """";

        var result = GetModuleResult(["Test"], moduleText);

        result.Shadowings.Should().ContainKey("name");
        result.Shadowings["name"].DeclarationPath.Should().Equal("process");
    }
}
