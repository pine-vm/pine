using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for <see cref="ElmSyntaxTransformations.RenameBindingsAvoidingCapture(Stil4mElmSyntax7.FunctionImplementation, IReadOnlySet{string})"/>
/// and related methods.
///
/// These methods perform capture-avoiding renaming of local bindings (function parameters,
/// let-bindings, case-pattern variables, lambda parameters) to avoid shadowing names
/// already in scope. When a local binding name collides with a name in the
/// <c>namesAlreadyInScope</c> set, it is renamed by appending <c>_0</c>, <c>_1</c>, etc.
///
/// Each test provides an Elm source module, calls the rename pass, and asserts
/// the rendered output matches an expected snapshot.
/// </summary>
public class RenameBindingsAvoidingCaptureTests
{
    private static SyntaxTypes.File ParseModuleText(string moduleText)
    {
        var concreteSyntax =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        return FromFullSyntaxModel.Convert(concreteSyntax);
    }

    /// <summary>
    /// Parses the module, applies <see cref="ElmSyntaxTransformations.RenameBindingsAvoidingCapture"/>
    /// to each function declaration with the given <paramref name="namesInScope"/>,
    /// and returns the formatted module text.
    /// </summary>
    private static string RenameAndFormat(
        string inputModuleText,
        IReadOnlySet<string> namesInScope)
    {
        var parsedModule = ParseModuleText(inputModuleText);

        var renamedDeclarations =
            parsedModule.Declarations
            .Select(
                declNode =>
                {
                    if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                        return declNode;

                    var impl = funcDecl.Function.Declaration.Value;

                    var renamedImpl =
                        ElmSyntaxTransformations.RenameBindingsAvoidingCapture(impl, namesInScope);

                    if (ReferenceEquals(renamedImpl, impl))
                        return declNode;

                    var newFunc =
                        funcDecl.Function with
                        {
                            Declaration =
                            new Node<SyntaxTypes.FunctionImplementation>(
                                funcDecl.Function.Declaration.Range,
                                renamedImpl)
                        };

                    return
                        new Node<SyntaxTypes.Declaration>(
                            declNode.Range,
                            new SyntaxTypes.Declaration.FunctionDeclaration(newFunc));
                })
            .ToImmutableArray();

        var renamedModule = parsedModule with { Declarations = renamedDeclarations };

        return
            Avh4Format.FormatToString(
                ToFullSyntaxModel.Convert(renamedModule));
    }

    /// <summary>
    /// When no names in scope conflict with the function's local bindings,
    /// the output should be identical to the input (no renaming needed).
    /// </summary>
    [Fact]
    public void No_rename_when_no_conflicts()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            add x y =
                x + y
            """";

        var result = RenameAndFormat(inputModuleText, ImmutableHashSet<string>.Empty);

        result.Trim().Should().Be(inputModuleText.Trim());
    }

    /// <summary>
    /// A function parameter named <c>x</c> conflicts with module-level name <c>x</c>.
    /// The parameter must be renamed to <c>x_0</c>, and all references in the body
    /// must also use the new name.
    ///
    /// Input:
    /// <code>
    /// double x =
    ///     x + x
    /// </code>
    ///
    /// With namesInScope = { "x" }, output becomes:
    /// <code>
    /// double x_0 =
    ///     x_0 + x_0
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_function_parameter_that_shadows_module_level_name()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            double x =
                x + x
            """";

        var namesInScope = ImmutableHashSet.Create("x");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            double x_0 =
                x_0 + x_0
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// A let-binding name <c>helper</c> conflicts with a module-level name <c>helper</c>.
    /// The let-binding is renamed to <c>helper_0</c> and all references in its scope
    /// are updated.
    ///
    /// Input:
    /// <code>
    /// compute x =
    ///     let
    ///         helper = x * 2
    ///     in
    ///     helper + 1
    /// </code>
    ///
    /// With namesInScope = { "helper" }, output becomes:
    /// <code>
    /// compute x =
    ///     let
    ///         helper_0 = x * 2
    ///     in
    ///     helper_0 + 1
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_let_binding_that_shadows_module_level_name()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            compute x =
                let
                    helper =
                        x * 2
                in
                helper + 1
            """";

        var namesInScope = ImmutableHashSet.Create("helper");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            compute x =
                let
                    helper_0 =
                        x * 2
                in
                helper_0 + 1
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// A lambda parameter <c>y</c> conflicts with the module-level name <c>y</c>.
    /// The lambda parameter must be renamed to <c>y_0</c>.
    ///
    /// Input:
    /// <code>
    /// applyFn f =
    ///     f (\y -> y + 1)
    /// </code>
    ///
    /// With namesInScope = { "y" }, output becomes:
    /// <code>
    /// applyFn f =
    ///     f (\y_0 -> y_0 + 1)
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_lambda_parameter_that_shadows_module_level_name()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            applyFn f =
                f (\y -> y + 1)
            """";

        var namesInScope = ImmutableHashSet.Create("y");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            applyFn f =
                f (\y_0 -> y_0 + 1)
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// A case-pattern variable <c>n</c> conflicts with the module-level name <c>n</c>.
    /// The case-pattern binding must be renamed to <c>n_0</c>.
    ///
    /// Input:
    /// <code>
    /// describe val =
    ///     case val of
    ///         Just n ->
    ///             n + 1
    ///
    ///         Nothing ->
    ///             0
    /// </code>
    ///
    /// With namesInScope = { "n" }, output becomes:
    /// <code>
    /// describe val =
    ///     case val of
    ///         Just n_0 ->
    ///             n_0 + 1
    ///
    ///         Nothing ->
    ///             0
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_case_pattern_variable_that_shadows_module_level_name()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            describe val =
                case val of
                    Just n ->
                        n + 1

                    Nothing ->
                        0
            """";

        var namesInScope = ImmutableHashSet.Create("n");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            describe val =
                case val of
                    Just n_0 ->
                        n_0 + 1

                    Nothing ->
                        0
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Multiple conflicting names are all renamed independently.
    /// Parameters <c>x</c> and <c>y</c> both conflict with module-level names.
    ///
    /// Input:
    /// <code>
    /// add x y =
    ///     x + y
    /// </code>
    ///
    /// With namesInScope = { "x", "y" }, output becomes:
    /// <code>
    /// add x_0 y_0 =
    ///     x_0 + y_0
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_multiple_conflicting_parameters()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            add x y =
                x + y
            """";

        var namesInScope = ImmutableHashSet.Create("x", "y");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            add x_0 y_0 =
                x_0 + y_0
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// When a generated name like <c>x_0</c> is also already in scope,
    /// the suffix increments to <c>x_1</c>.
    ///
    /// Input:
    /// <code>
    /// identity x =
    ///     x
    /// </code>
    ///
    /// With namesInScope = { "x", "x_0" }, output becomes:
    /// <code>
    /// identity x_1 =
    ///     x_1
    /// </code>
    /// </summary>
    [Fact]
    public void Suffix_increments_when_first_candidate_also_in_scope()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            identity x =
                x
            """";

        var namesInScope = ImmutableHashSet.Create("x", "x_0");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            identity x_1 =
                x_1
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// A let-function with parameters: the function name <c>inner</c> conflicts with
    /// module-level scope. The function's own parameters remain unchanged (they don't
    /// conflict), but the function name is renamed throughout.
    ///
    /// Input:
    /// <code>
    /// outer x =
    ///     let
    ///         inner a = a + x
    ///     in
    ///     inner 10
    /// </code>
    ///
    /// With namesInScope = { "inner" }, output becomes:
    /// <code>
    /// outer x =
    ///     let
    ///         inner_0 a = a + x
    ///     in
    ///     inner_0 10
    /// </code>
    /// </summary>
    [Fact]
    public void Rename_let_function_name_that_conflicts()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            outer x =
                let
                    inner a =
                        a + x
                in
                inner 10
            """";

        var namesInScope = ImmutableHashSet.Create("inner");

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            outer x =
                let
                    inner_0 a =
                        a + x
                in
                inner_0 10
            """";

        var result = RenameAndFormat(inputModuleText, namesInScope);

        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Verifies that <see cref="ElmSyntaxTransformations.GenerateUniqueLocalName"/>
    /// produces names with incrementing suffixes until an unused name is found.
    /// </summary>
    [Fact]
    public void GenerateUniqueLocalName_finds_first_available_suffix()
    {
        var usedNames = new HashSet<string> { "x", "x_0", "x_1" };

        var result = ElmSyntaxTransformations.GenerateUniqueLocalName("x", usedNames);

        result.Should().Be("x_2");
    }

    /// <summary>
    /// Verifies that <see cref="ElmSyntaxTransformations.GenerateUniqueLocalName"/>
    /// returns <c>_0</c> suffix when the base name is used but <c>_0</c> is available.
    /// </summary>
    [Fact]
    public void GenerateUniqueLocalName_returns_zero_suffix_when_available()
    {
        var usedNames = new HashSet<string> { "y" };

        var result = ElmSyntaxTransformations.GenerateUniqueLocalName("y", usedNames);

        result.Should().Be("y_0");
    }
}
