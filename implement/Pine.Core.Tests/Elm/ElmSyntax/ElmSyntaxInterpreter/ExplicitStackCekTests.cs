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

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.InterpretAsElmValue(mainBody, declarations)
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

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

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

            return
                ElmInterpreter.PineBuiltinResolver(application)
                ?? ElmInterpreter.UserDefinedResolver(application, declarations)
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
            .Should().Equal(
            new List<ElmCallStackFrame>
            {
                new(DeclQualifiedName.Create([], "inner"), [ElmInterpreter.ToProcess(ElmValue.Integer(1))]),
                new(DeclQualifiedName.Create([], "middle"), [ElmInterpreter.ToProcess(ElmValue.Integer(1))]),
                new(DeclQualifiedName.Create([], "outer"), [ElmInterpreter.ToProcess(ElmValue.Integer(1))]),
            });

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

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.InterpretAsElmValue(mainBody, declarations);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        // "negateString" is the innermost active call, followed by "outer".
        error.CallStack.Should().ContainInOrder(
            new ElmCallStackFrame(DeclQualifiedName.Create([], "negateString"), []),
            new ElmCallStackFrame(DeclQualifiedName.Create([], "outer"), []));
    }

    /// <summary>
    /// Each argument rendering in a stack-trace frame is truncated to a configurable length
    /// limit (default <see cref="ElmCallStackFrame.DefaultArgumentRenderLengthLimit"/>). The
    /// truncated rendering keeps the value's prefix and records the value's full character length
    /// so the information most useful for debugging is preserved. A negative limit disables
    /// truncation.
    /// </summary>
    [Fact]
    public void Stack_frame_argument_rendering_is_truncated_to_configurable_limit()
    {
        // A long string whose Elm rendering comfortably exceeds the default 400-char limit.
        var longText = new string('a', 5000);

        var frame =
            new ElmCallStackFrame(
                DeclQualifiedName.Create([], "consume"),
                [ElmInterpreter.ToProcess(ElmValue.StringInstance(longText))]);

        // Full (untruncated) rendering: the entire string plus the surrounding quotes.
        var full = frame.Render(argumentRenderLengthLimit: -1);

        full.Should().Contain(longText);
        full.Should().NotContain("truncated");

        // Default rendering (via ToString) truncates each argument to 400 characters and records
        // the full length so the value's size remains visible.
        var defaultRendered = frame.ToString();

        defaultRendered.Should().Contain("consume");
        defaultRendered.Should().Contain("truncated, " + (longText.Length + 2) + " chars total");
        defaultRendered.Length.Should().BeLessThan(full.Length);

        // A custom, smaller limit truncates even more aggressively.
        var smallRendered = frame.Render(argumentRenderLengthLimit: 20);

        smallRendered.Length.Should().BeLessThan(defaultRendered.Length);
        smallRendered.Should().Contain("truncated, " + (longText.Length + 2) + " chars total");
    }

    /// <summary>
    /// A runtime error raised while an anonymous lambda is the innermost active frame surfaces a
    /// stack frame whose name carries the containing/originating top-level declaration (rather than
    /// a bare <c>&lt;lambda&gt;</c>), so developers can tell which declaration the lambda came from.
    /// </summary>
    [Fact]
    public void Lambda_frame_includes_containing_declaration_name()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            main =
                callWithLambda


            callWithLambda =
                (\y -> -"not a number") 1
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.InterpretAsElmValue(mainBody, declarations);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        // The innermost frame is the anonymous lambda, named after the declaration it originated in.
        error.CallStack[0].FunctionName
            .Should().Be(DeclQualifiedName.Create(["callWithLambda"], "<lambda>"));

        error.ToString().Should().Contain("callWithLambda.<lambda>");
    }
}
