using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Functional;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class SuccessPathAllocationTests
{
    [Theory]
    [InlineData(false, 65_000)]
    [InlineData(true, 100_000)]
    public void Repeated_successful_calls_do_not_allocate_outcome_objects_or_result_wrappers(
        bool continueWithFunction,
        long allocationLimit)
    {
        const int count = 1000;
        var root = DeclQualifiedName.FromString("Test.root");
        var leaf = DeclQualifiedName.FromString("Test.leaf");
        var literal = new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.CreateInteger(7));

        var body =
            new Engine.PreparedExpression.ListExpr(
                [
                .. Enumerable.Repeat<Engine.PreparedExpression>(new Engine.PreparedExpression.Identifier(leaf), count)
                ]);

        var rootResolution =
            new Engine.ApplicationResolution.ContinueWithFunction(new("root", [], body), root);

        Engine.ApplicationResolution leafResolution =
            continueWithFunction
            ?
            new Engine.ApplicationResolution.ContinueWithFunction(new("leaf", [], literal), leaf)
            :
            new Engine.ApplicationResolution.Resolved(literal.Value);

        // A custom resolver deliberately avoids nullary-value memoization.
        Engine.ApplicationResolution Resolve(Engine.Application application) =>
            application.FunctionName == root ? rootResolution : leafResolution;

        for (var index = 0; index < 5; index++)
            Engine.Interpret(root, [], Resolve, null);

        var before = GC.GetAllocatedBytesForCurrentThread();
        var result = Engine.Interpret(root, [], Resolve, null);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;

        var values = result.IsOkOrNull()!;
        values.GetLength().Should().Be(count);
        values.GetElementAt(0).Should().BeSameAs(literal.Value);
        values.GetElementAt(count - 1).Should().BeSameAs(literal.Value);
        allocated.Should().BeLessThan(allocationLimit);
    }

    [Fact]
    public void Function_resolution_reuses_immutable_metadata_without_caching_invocation_arguments()
    {
        var name = DeclQualifiedName.FromString("Test.identity");

        var function =
            new Engine.PreparedFunctionImplementation(
                "identity",
                [new Pattern.VarPattern("value")],
                Engine.PreparedExpression.Identifier.Create([], "value"));

        var resolver =
            Engine.BuildResolvers(
                new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>
                {
                    [name] = new Engine.PreparedDeclaration.FunctionDeclaration(new(null, function))
                });

        var application =
            new Engine.Application(
                name,
                [PineValueInProcess.CreateInteger(1)],
                new Engine.ApplicationContext(name, new Dictionary<string, PineValueInProcess>()));

        var resolution = resolver(application);
        var other = application with { Arguments = [PineValueInProcess.CreateInteger(2)] };

        resolver(other).Should().BeSameAs(resolution);

        for (var index = 0; index < 100; index++)
            resolver(application);

        var before = GC.GetAllocatedBytesForCurrentThread();

        for (var index = 0; index < 10_000; index++)
            resolver(application);

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().BeLessThan(1024);

        Engine.Interpret(name, application.Arguments, resolver, null)
            .IsOkOrNull().Should().BeSameAs(application.Arguments[0]);

        Engine.Interpret(name, other.Arguments, resolver, null)
            .IsOkOrNull().Should().BeSameAs(other.Arguments[0]);
    }

    [Theory]
    [InlineData("bad []", "Cannot apply")]
    [InlineData("(\\x -> x 1) []", "Cannot apply")]
    [InlineData("let f = \\x -> x 1 in f []", "Cannot apply")]
    [InlineData("returnsValue 0 1", "Cannot apply")]
    [InlineData("let x = bad [] in x", "Cannot apply")]
    [InlineData("let (x, y) = (bad [], 0) in x", "Cannot apply")]
    [InlineData("needsRecord 1", ElmRuntimeErrorMessages.RecordAccessOnNonRecord)]
    [InlineData("(\\{ value } -> value) 1", ElmRuntimeErrorMessages.RecordAccessOnNonRecord)]
    public void Call_errors_are_propagated_from_every_application_and_let_continuation(
        string expression,
        string expectedMessage)
    {
        var result =
            InterpreterTestHelper.EvaluateInModule(
                expression,
                """
                module Test exposing (..)

                bad x = x 1

                returnsValue x = x

                needsRecord { value } = value
                """);

        result.IsOkOrNull().Should().BeNull();
        result.IsErrOrNull().Should().NotBeNull();
        result.IsErrOrNull()!.ToString().Should().Contain(expectedMessage);
    }

    [Fact]
    public void Direct_resolved_entry_returns_without_entering_the_instruction_loop()
    {
        var expected = PineValueInProcess.CreateInteger(7);
        var resolution = new Engine.ApplicationResolution.Resolved(expected);

        var result =
            Engine.Interpret(
                DeclQualifiedName.FromString("Test.value"),
                [],
                _ => resolution,
                infixOperators: null,
                new Engine.EvaluationConfig(InstructionCountLimit: 0, ContinuationDepthLimit: 0));

        result.IsOkOrNull().Should().BeSameAs(expected);
    }
}
