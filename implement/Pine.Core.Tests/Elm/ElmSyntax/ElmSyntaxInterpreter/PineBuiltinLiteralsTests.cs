using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers the class of scenarios where the interpreter only needs the <c>Pine_builtin</c> /
/// <c>Pine_kernel</c> resolver to evaluate an expression composed of literals forwarded to
/// <see cref="Pine.Core.KernelFunction"/> operations and converted back to <see cref="ElmValue"/>.
/// </summary>
public class PineBuiltinLiteralsTests
{
    [Fact]
    public void Int_add_on_integer_literals()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                Pine_builtin.int_add [ 3, 4 ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result =
            ElmInterpreter.ParseAndInterpret("main", declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Negate_on_integer_literal()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                Pine_builtin.negate 5
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result =
            ElmInterpreter.ParseAndInterpret("main", declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(-5));
    }

    [Fact]
    public void Int_mul_on_integer_literals()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                Pine_builtin.int_mul [ 6, 7 ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result =
            ElmInterpreter.ParseAndInterpret("main", declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Length_on_list_literal()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                Pine_builtin.length [ 10, 20, 30, 40 ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result =
            ElmInterpreter.ParseAndInterpret("main", declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(4));
    }
}
