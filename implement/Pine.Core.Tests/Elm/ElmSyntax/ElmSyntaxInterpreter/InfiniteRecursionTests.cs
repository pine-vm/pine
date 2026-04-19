using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Verifies that the interpreter detects non-terminating recursion and reports an
/// <see cref="ElmInterpretationError"/> whose rendered string contains the error message
/// on the first line followed by the Elm call stack truncated to the first cycle.
/// </summary>
public class InfiniteRecursionTests
{
    private static string InterpretAndRenderError(string elmModuleText)
    {
        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result = ElmInterpreter.Interpret(mainBody, declarations);

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        return error.ToString();
    }

    [Fact]
    public void Direct_recursion_with_identical_arguments_reports_one_frame_cycle()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            loopSame x =
                loopSame x


            main =
                loopSame 7
            """;

        var errorString = InterpretAndRenderError(elmModuleText);

        errorString.Should().Be(
            """
            Infinite recursion detected: the call stack contains a repeated (function, arguments) pair.
            Elm call stack (innermost first):
              at loopSame 7
              at loopSame 7
            """);
    }

    [Fact]
    public void Self_recursion_with_alternating_arguments_reports_two_frame_cycle()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            flip n =
                if Pine_builtin.int_is_sorted_asc [ n, 0 ] then
                    flip 1

                else
                    flip 0


            main =
                flip 0
            """;

        var errorString = InterpretAndRenderError(elmModuleText);

        errorString.Should().Be(
            """
            Infinite recursion detected: the call stack contains a repeated (function, arguments) pair.
            Elm call stack (innermost first):
              at flip 1
              at flip 0
              at flip 1
            """);
    }

    [Fact]
    public void Mutual_recursion_reports_alternating_frame_cycle()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            ping n =
                pong n


            pong n =
                ping n


            main =
                ping 42
            """;

        var errorString = InterpretAndRenderError(elmModuleText);

        errorString.Should().Be(
            """
            Infinite recursion detected: the call stack contains a repeated (function, arguments) pair.
            Elm call stack (innermost first):
              at pong 42
              at ping 42
              at pong 42
            """);
    }
}
