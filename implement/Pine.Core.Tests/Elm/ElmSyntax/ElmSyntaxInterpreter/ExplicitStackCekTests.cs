using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers capabilities that the explicit-stack refactor (CEK machine) introduced:
/// evaluating deeply recursive programs without exhausting the .NET thread stack,
/// and capturing an Elm-level call stack on runtime errors via
/// <see cref="ElmInterpretationError"/>.
/// </summary>
public class ExplicitStackCekTests
{
    /// <summary>
    /// Self-recursive (tail-call) function iterated many more times than the .NET
    /// thread stack could accommodate under plain C# recursion. Relies on the
    /// trampoline's tail-call replacement to run in O(1) explicit-stack space.
    /// </summary>
    [Fact]
    public void Deep_tail_recursion_does_not_overflow_stack()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            loop n acc =
                if Pine_builtin.int_is_sorted_asc [ n, 0 ] then
                    acc

                else
                    loop (Pine_builtin.int_add [ n, -1 ]) (Pine_builtin.int_add [ acc, 1 ])


            main =
                loop 100000 0
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        result.Should().Be(ElmValue.Integer(100000));
    }

    /// <summary>
    /// When a user-defined resolver throws, the interpreter wraps the error into an
    /// <see cref="ElmInterpretationError"/> on the error branch of the returned
    /// <see cref="Result{ErrT, OkT}"/>, listing the active Elm declarations innermost first.
    /// </summary>
    [Fact]
    public void Runtime_error_reports_elm_call_stack()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                outer 1


            outer x =
                middle x


            middle x =
                inner x


            inner x =
                Debug.todo "boom"
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        // Custom resolver: Debug.todo raises an ordinary exception that the interpreter
        // must catch and rewrap with the current Elm call stack.
        ElmInterpreter.ApplicationResolution FailingResolver(
            ElmInterpreter.Application application)
        {
            if (application.FunctionName.Namespaces is ["Debug"]
                && application.FunctionName.DeclName == "todo")
            {
                throw new System.InvalidOperationException("Debug.todo was called");
            }

            var interpreter = new ElmInterpreter();

            return
                interpreter.PineBuiltinResolver(application)
                ?? interpreter.UserDefinedResolver(application, declarations)
                ?? throw new System.InvalidOperationException(
                    "No resolver matched: " + application.FunctionName.FullName);
        }

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.Interpret(mainBody, FailingResolver);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        error.CallStack
            .Should().BeEquivalentTo(
            new List<ElmCallStackFrame>
            {
                new(new DeclQualifiedName([], "inner"), [ElmValue.Integer(1)]),
                new(new DeclQualifiedName([], "middle"), [ElmValue.Integer(1)]),
                new(new DeclQualifiedName([], "outer"), [ElmValue.Integer(1)]),
            },
            options => options.WithStrictOrdering());

        error.ToString().Should().Contain("inner");
        error.ToString().Should().Contain("middle");
        error.ToString().Should().Contain("outer");
    }

    /// <summary>
    /// A runtime error raised from a kernel-argument pattern mismatch (negation of a
    /// non-integer) is caught by the trampoline and surfaced on the error branch with the
    /// call stack captured, even though the erroring site is not a user function call.
    /// </summary>
    [Fact]
    public void Negation_type_error_reports_call_stack()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                outer


            outer =
                negateString


            negateString =
                -"not a number"
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.Interpret(mainBody, declarations);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        // "negateString" is the innermost active call, followed by "outer".
        error.CallStack.Should().ContainInOrder(
            new ElmCallStackFrame(new DeclQualifiedName([], "negateString"), []),
            new ElmCallStackFrame(new DeclQualifiedName([], "outer"), []));
    }
}
