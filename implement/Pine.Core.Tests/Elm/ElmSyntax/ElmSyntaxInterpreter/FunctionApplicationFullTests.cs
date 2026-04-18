using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers full application of user-defined functions, i.e. argument counts equal parameter counts.
/// </summary>
public class FunctionApplicationFullTests
{
    [Fact]
    public void Call_unary_top_level_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            double x =
                Pine_builtin.int_mul [ x, 2 ]


            main =
                double 21
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Call_binary_top_level_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]


            main =
                add 13 29
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Nested_full_applications()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]


            triple x =
                Pine_builtin.int_mul [ x, 3 ]


            main =
                add (triple 4) (triple 2)
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(18));
    }
}
