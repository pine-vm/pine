using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

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
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("double 21", elmModuleText)
            .Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Call_binary_top_level_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("add 13 29", elmModuleText)
            .Should().Be(ElmValue.Integer(42));
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
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("add (triple 4) (triple 2)", elmModuleText)
            .Should().Be(ElmValue.Integer(18));
    }
}
