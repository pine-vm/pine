using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers let-block expressions where the final expression depends on one or more declarations
/// introduced in the "let" part.
/// </summary>
public class LetBlockExpressionsTests
{
    [Fact]
    public void Single_let_binding_used_in_final_expression()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    answer =
                        42
                in
                answer
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Multiple_let_bindings_combined_in_final_expression()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    a =
                        3

                    b =
                        4
                in
                Pine_builtin.int_add [ a, b ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Later_let_binding_depends_on_earlier_binding()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    a =
                        10

                    b =
                        Pine_builtin.int_mul [ a, 2 ]
                in
                Pine_builtin.int_add [ a, b ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(30));
    }
}
