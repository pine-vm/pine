using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using Xunit;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Tests.Elm.ElmSyntax.SyntaxModel;

public class ReportSyntaxNodesTests
{
    #region Helper methods

    /// <summary>
    /// Parse an Elm module and return the first declaration as a FunctionDeclaration.
    /// </summary>
    private static Declaration.FunctionDeclaration ParseFirstFunctionDeclaration(string moduleText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var decl = parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        decl.Should().NotBeNull("Expected first declaration to be a FunctionDeclaration");

        return decl!;
    }

    /// <summary>
    /// Run ReportViaCallback on the given declaration and collect results.
    /// </summary>
    private static (
        List<ExpressionSyntax.LambdaExpression> Lambdas,
        List<ExpressionSyntax.LetBlock> LetBlocks,
        List<ExpressionSyntax.LetDeclaration.LetFunction> LetFunctions)
        CollectReports(Declaration declaration)
    {
        var lambdas = new List<ExpressionSyntax.LambdaExpression>();
        var letBlocks = new List<ExpressionSyntax.LetBlock>();
        var letFunctions = new List<ExpressionSyntax.LetDeclaration.LetFunction>();

        ReportSyntaxNodes.ReportViaCallback(
            declaration,
            reportLambda: lambdas.Add,
            reportLetBlock: letBlocks.Add,
            reportLetFunction: letFunctions.Add);

        return (lambdas, letBlocks, letFunctions);
    }

    /// <summary>
    /// Run ReportExpression on the given expression and collect results.
    /// </summary>
    private static (
        List<ExpressionSyntax.LambdaExpression> Lambdas,
        List<ExpressionSyntax.LetBlock> LetBlocks,
        List<ExpressionSyntax.LetDeclaration.LetFunction> LetFunctions)
        CollectReportsFromExpression(ExpressionSyntax expression)
    {
        var lambdas = new List<ExpressionSyntax.LambdaExpression>();
        var letBlocks = new List<ExpressionSyntax.LetBlock>();
        var letFunctions = new List<ExpressionSyntax.LetDeclaration.LetFunction>();

        ReportSyntaxNodes.ReportExpression(
            expression,
            reportLambda: lambdas.Add,
            reportLetBlock: letBlocks.Add,
            reportLetFunction: letFunctions.Add);

        return (lambdas, letBlocks, letFunctions);
    }

    #endregion

    #region Leaf expressions (no sub-nodes reported)

    [Fact]
    public void Simple_integer_literal_reports_nothing()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                42
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void String_literal_reports_nothing()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                "hello"
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void Function_or_value_reference_reports_nothing()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                someOtherValue
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    #endregion

    #region Lambda expressions

    [Fact]
    public void Single_lambda_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                \x -> x
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void Nested_lambdas_are_both_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                \x -> \y -> x
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void Lambda_inside_parentheses_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                (\x -> x)
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    #endregion

    #region Let expressions

    [Fact]
    public void Let_block_with_function_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    helper x =
                        x
                in
                helper 1
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().HaveCount(1);
        letFunctions.Should().HaveCount(1);
    }

    [Fact]
    public void Let_block_with_multiple_functions_reports_all()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    a x =
                        x

                    b y =
                        y
                in
                a (b 1)
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().HaveCount(1);
        letFunctions.Should().HaveCount(2);
    }

    [Fact]
    public void Nested_let_blocks_are_both_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    a =
                        let
                            b =
                                1
                        in
                        b
                in
                a
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().BeEmpty();
        letBlocks.Should().HaveCount(2);
    }

    [Fact]
    public void Let_with_lambda_reports_both()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    f =
                        \x -> x
                in
                f 1
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
        letBlocks.Should().HaveCount(1);
    }

    #endregion

    #region If expressions

    [Fact]
    public void Lambda_inside_if_condition_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                if (\x -> x) True then
                    1
                else
                    2
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    [Fact]
    public void Lambda_inside_if_branches_are_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                if True then
                    \x -> x
                else
                    \y -> y
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
    }

    #endregion

    #region Application expressions

    [Fact]
    public void Lambda_inside_application_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                List.map (\x -> x) items
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    #endregion

    #region Case expressions

    [Fact]
    public void Lambda_inside_case_branch_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                case something of
                    0 ->
                        \x -> x

                    _ ->
                        \y -> y
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
    }

    [Fact]
    public void Lambda_inside_case_scrutinee_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                case (\x -> x) of
                    _ ->
                        1
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    #endregion

    #region Operator expressions

    [Fact]
    public void Lambda_inside_operator_application_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                (\x -> x) |> (\y -> y)
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
    }

    #endregion

    #region Tuple expressions

    [Fact]
    public void Lambda_inside_tuple_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                ( \x -> x, \y -> y )
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
    }

    #endregion

    #region List expressions

    [Fact]
    public void Lambda_inside_list_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                [ \x -> x, \y -> y ]
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
    }

    #endregion

    #region Record expressions

    [Fact]
    public void Lambda_inside_record_field_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                { field = \x -> x }
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    [Fact]
    public void Lambda_inside_record_update_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                { record | field = \x -> x }
            """);

        var (lambdas, _, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
    }

    #endregion

    #region Negation expressions

    [Fact]
    public void Negation_with_nested_lambda_in_parens()
    {
        // This tests that negation's subexpression is traversed.
        // While `-(\\x -> x)` isn't valid Elm, the parser may still build it.
        // We use a simpler case: negation of a parenthesized let.
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                -(let a = 1 in a)
            """);

        var (_, letBlocks, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        letBlocks.Should().HaveCount(1);
    }

    #endregion

    #region Record access expressions

    [Fact]
    public void Lambda_inside_record_access_base_is_reported()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                (let f = \x -> x in { field = f }).field
            """);

        var (lambdas, letBlocks, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(1);
        letBlocks.Should().HaveCount(1);
    }

    #endregion

    #region Non-function declarations

    [Fact]
    public void Choice_type_declaration_reports_nothing()
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(
                """
                module Test exposing (..)


                type Color
                    = Red
                    | Green
                    | Blue
                """)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var decl = parsedFile.Declarations[0].Value;

        var (lambdas, letBlocks, letFunctions) = CollectReports(decl);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void Alias_declaration_reports_nothing()
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(
                """
                module Test exposing (..)


                type alias Point =
                    { x : Int, y : Int }
                """)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var decl = parsedFile.Declarations[0].Value;

        var (lambdas, letBlocks, letFunctions) = CollectReports(decl);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    #endregion

    #region Complex scenarios

    [Fact]
    public void Complex_nested_structure_reports_all_nodes()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    helper x =
                        case x of
                            0 ->
                                \y -> y

                            _ ->
                                let
                                    inner z =
                                        z
                                in
                                inner x
                in
                List.map (\a -> helper a) [ 1, 2, 3 ]
            """);

        var (lambdas, letBlocks, letFunctions) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        // Two lambdas: \y -> y and \a -> helper a
        lambdas.Should().HaveCount(2);

        // Two let blocks: the outer one and the inner one
        letBlocks.Should().HaveCount(2);

        // Two let functions: helper and inner
        letFunctions.Should().HaveCount(2);
    }

    [Fact]
    public void Let_destructuring_body_is_traversed()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                let
                    ( a, b ) =
                        ( \x -> x, \y -> y )
                in
                a b
            """);

        var (lambdas, letBlocks, _) = CollectReports(new Declaration.FunctionDeclaration(funcDecl.Function));

        lambdas.Should().HaveCount(2);
        letBlocks.Should().HaveCount(1);
    }

    #endregion

    #region ReportExpression directly

    [Fact]
    public void ReportExpression_on_unit_reports_nothing()
    {
        var expr = new ExpressionSyntax.UnitExpr();

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_integer_literal_reports_nothing()
    {
        var expr = new ExpressionSyntax.Integer("42");

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_char_literal_reports_nothing()
    {
        var expr = new ExpressionSyntax.CharLiteral('a');

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_float_literal_reports_nothing()
    {
        var expr = new ExpressionSyntax.Floatable("3.14");

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_prefix_operator_reports_nothing()
    {
        var expr = new ExpressionSyntax.PrefixOperator("+");

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_record_access_function_reports_nothing()
    {
        var expr = new ExpressionSyntax.RecordAccessFunction(".field");

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportExpression_on_glsl_reports_nothing()
    {
        var expr = new ExpressionSyntax.GLSLExpression("void main() {}");

        var (lambdas, letBlocks, letFunctions) = CollectReportsFromExpression(expr);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    #endregion

    #region ReportFunctionDeclaration directly

    [Fact]
    public void ReportFunctionDeclaration_with_simple_body()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            identity x =
                x
            """);

        var lambdas = new List<ExpressionSyntax.LambdaExpression>();
        var letBlocks = new List<ExpressionSyntax.LetBlock>();
        var letFunctions = new List<ExpressionSyntax.LetDeclaration.LetFunction>();

        ReportSyntaxNodes.ReportFunctionDeclaration(
            funcDecl,
            reportLamba: lambdas.Add,
            reportLetBlock: letBlocks.Add,
            reportLetFunction: letFunctions.Add);

        lambdas.Should().BeEmpty();
        letBlocks.Should().BeEmpty();
        letFunctions.Should().BeEmpty();
    }

    [Fact]
    public void ReportFunctionDeclaration_with_lambda_body()
    {
        var funcDecl = ParseFirstFunctionDeclaration(
            """
            module Test exposing (..)


            value =
                \x -> x
            """);

        var lambdas = new List<ExpressionSyntax.LambdaExpression>();
        var letBlocks = new List<ExpressionSyntax.LetBlock>();
        var letFunctions = new List<ExpressionSyntax.LetDeclaration.LetFunction>();

        ReportSyntaxNodes.ReportFunctionDeclaration(
            funcDecl,
            reportLamba: lambdas.Add,
            reportLetBlock: letBlocks.Add,
            reportLetFunction: letFunctions.Add);

        lambdas.Should().HaveCount(1);
    }

    #endregion
}
