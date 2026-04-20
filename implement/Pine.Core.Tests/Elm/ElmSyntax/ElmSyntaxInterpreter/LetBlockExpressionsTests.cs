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

    /// <summary>
    /// Earlier let binding references a name introduced by a later let binding.
    /// In Elm, every <c>let</c> is one mutually-recursive binding group, so binding
    /// declaration order in source must not affect the result. This case is
    /// exactly the shape that occurs in <c>Elm.Parser.Declarations.functionAfterDocumentation</c>
    /// (see the language-service gap analysis): one binding's RHS forward-references
    /// the name introduced by the next sibling binding.
    /// </summary>
    [Fact]
    public void Earlier_let_binding_forward_references_later_binding()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    b =
                        Pine_builtin.int_mul [ a, 2 ]

                    a =
                        10
                in
                Pine_builtin.int_add [ a, b ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(30));
    }

    /// <summary>
    /// Forward reference into a destructured let binding, mirroring the exact shape
    /// of the failing fragment in <c>Elm.Parser.Declarations.functionAfterDocumentation</c>:
    /// the first destructure pattern references the name introduced by the second.
    /// </summary>
    [Fact]
    public void Earlier_let_destructure_forward_references_later_destructure()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    ( leftAlias, _ ) =
                        right

                    ( _, right ) =
                        ( 0, ( 21, 21 ) )
                in
                Pine_builtin.int_add [ leftAlias, leftAlias ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    /// <summary>
    /// A <c>let</c> binding with parameters (i.e. a local function) is callable from
    /// the body of the let. Removes the historical
    /// "Let bindings with parameters are not implemented yet." restriction.
    /// </summary>
    [Fact]
    public void Let_binding_with_parameters_is_callable_from_body()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    increment x =
                        Pine_builtin.int_add [ x, 1 ]
                in
                increment 41
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(42));
    }

    /// <summary>
    /// A let-bound function may recurse on itself.
    /// </summary>
    [Fact]
    public void Let_binding_with_parameters_can_be_self_recursive()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    sumTo n =
                        if Pine_kernel.equal [ n, 0 ] then
                            0
                        else
                            Pine_builtin.int_add [ n, sumTo (Pine_builtin.int_add [ n, -1 ]) ]
                in
                sumTo 5
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(15));
    }

    /// <summary>
    /// Two let-bound functions may call each other (mutual recursion).
    /// </summary>
    [Fact]
    public void Let_bindings_with_parameters_can_be_mutually_recursive()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    isEven n =
                        if Pine_kernel.equal [ n, 0 ] then
                            1
                        else
                            isOdd (Pine_builtin.int_add [ n, -1 ])

                    isOdd n =
                        if Pine_kernel.equal [ n, 0 ] then
                            0
                        else
                            isEven (Pine_builtin.int_add [ n, -1 ])
                in
                Pine_builtin.int_add [ isEven 6, isOdd 7 ]
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(2));
    }

    /// <summary>
    /// A let-bound function may close over another let binding's value, even when the
    /// value is introduced later in source order. The closure body resolves the value
    /// at invocation time against the fully-populated let-group environment.
    /// </summary>
    [Fact]
    public void Let_function_closes_over_later_value_binding()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    addBase n =
                        Pine_builtin.int_add [ n, base ]

                    base =
                        100
                in
                addBase 7
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(107));
    }

    /// <summary>
    /// A non-function let binding cycle (where two value bindings depend on each other
    /// without a function intermediary) must surface as a runtime error rather than
    /// silently looping or returning a bogus value.
    /// </summary>
    [Fact]
    public void Cyclic_non_function_let_bindings_report_runtime_error()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                let
                    a =
                        Pine_builtin.int_add [ b, 1 ]

                    b =
                        Pine_builtin.int_add [ a, 1 ]
                in
                a
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.Interpret(mainBody, declarations);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        error.Message.Should().Contain("Cyclic let binding");
    }
}
