using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;
using System;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class DebugBuiltinTests
{
    private static readonly ElmSyntaxInterpreter.Prepared s_empty =
        new(ImmutableDictionary<DeclQualifiedName, ElmSyntaxInterpreter.PreparedDeclaration>.Empty);

    private static readonly Lazy<ElmInterpreterEnvironment> s_library =
        new(() => ElmInterpreterSourcePreparation.Prepare(null).Extract(error => throw new Exception(error)));

    [Theory]
    [MemberData(nameof(DebugToStringTests.GenericValues), MemberType = typeof(DebugToStringTests))]
    public void ToString_matches_compiler_rendering_for_generic_values(PineValue value, string expected)
    {
        var result =
            ElmSyntaxInterpreter.Interpret(
                DeclQualifiedName.Create(["Debug"], "toString"),
                [PineValueInProcess.Create(value)],
                s_empty)
            .Extract(error => throw new Exception(error.ToString()));

        result.Evaluate().Should().Be(ElmValueEncoding.StringAsPineValue(expected));
    }

    [Theory]
    [InlineData("Debug.toString 123", "\"123\"")]
    [InlineData("Debug.toString True", "\"True\"")]
    [InlineData("Debug.toString False", "\"False\"")]
    [InlineData("Debug.toString 'a'", "\"'a'\"")]
    [InlineData("Debug.toString '\\u{1F600}'", "\"'😀'\"")]
    [InlineData("Debug.toString 0.1", "\"0.1\"")]
    [InlineData("Debug.toString -0.5", "\"-0.5\"")]
    [InlineData("Debug.toString [ 1, 2, 3 ]", "\"[1,2,3]\"")]
    [InlineData("Debug.toString (Just (Just 42))", "\"Just (Just 42)\"")]
    [InlineData("Debug.toString { a = [ 1, 2 ], b = True }", "\"{ a = [1,2], b = True }\"")]
    [InlineData("List.map Debug.toString [ 1, 2 ]", "[ \"1\", \"2\" ]")]
    [InlineData("let render = Debug.toString in render 42", "\"42\"")]
    [InlineData("Debug.toString identity", "\"<function>\"")]
    [InlineData("Debug.toString [ identity ]", "\"[<function>]\"")]
    [InlineData("Debug.toString (Just [ identity, identity, identity ])", "\"Just [<function>,<function>,<function>]\"")]
    [InlineData("let logged = Debug.log \"ignored\" in logged 42", "42")]
    public void Native_Debug_functions_support_primitives_composites_and_function_values(
        string expression,
        string expected)
    {
        var value = s_library.Value.Evaluate(expression).Extract(error => throw new Exception(error.ToString()));
        ElmSyntaxInterpreter.RenderAsElmExpression(value).expressionString.Should().Be(expected);
    }

    [Fact]
    public void Log_returns_the_exact_value_without_materializing_it()
    {
        var function = s_library.Value.Evaluate("identity").Extract(error => throw new Exception(error.ToString()));

        var result =
            ElmSyntaxInterpreter.Interpret(
                DeclQualifiedName.Create(["Debug"], "log"),
                [PineValueInProcess.Create(ElmValueEncoding.StringAsPineValue("ignored")), function],
                s_empty)
            .Extract(error => throw new Exception(error.ToString()));

        result.Should().BeSameAs(function);
    }

    [Fact]
    public void Callable_constructors_can_be_rendered_directly_and_inside_composites()
    {
        var environment =
            s_library.Value.AddDeclaration("type alias Point = { x : Int, y : Int }")
            .Extract(error => throw new Exception(error)).Environment;

        foreach (var expression in new[] { "Point 1", "[ Point 1 ]", "Just (Point 1)", ".x" })
        {
            var value = environment.Evaluate(expression).Extract(error => throw new Exception(error.ToString()));
            ElmSyntaxInterpreter.RenderAsElmExpression(value).expressionString.Should().Contain("function");
        }
    }

    [Fact]
    public void Unqualified_pattern_constructors_must_be_uniquely_exported()
    {
        var unique =
            ElmInterpreterEnvironment.Create(
                ["module Syntax exposing (Node(..))\ntype Node a = Node a"])
            .Extract(error => throw new Exception(error));

        unique.Evaluate("case Syntax.Node 42 of\n    Node x -> x")
            .Extract(error => throw new Exception(error.ToString())).AsInteger()
            .Should().Be(new System.Numerics.BigInteger(42));

        var ambiguous =
            ElmInterpreterEnvironment.Create(
                ["module A exposing (Node(..))\ntype Node = Node", "module B exposing (Node(..))\ntype Node = Node"])
            .Extract(error => throw new Exception(error));

        ambiguous.PrepareExpression("Node").IsErrOrNull().Should().NotBeNull();

        var hidden =
            ElmInterpreterEnvironment.Create(["module Hidden exposing (Node)\ntype Node = Node"])
            .Extract(error => throw new Exception(error));

        hidden.PrepareExpression("Node").IsErrOrNull().Should().NotBeNull();
    }
}
