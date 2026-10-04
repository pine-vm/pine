using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class RecordAliasSpecializationTests
{
    private const string ModuleText =
        """
        module Test exposing (..)

        type alias ParserState =
            { source : String, offset : Int, row : Int, column : Int }

        type alias WrappedState =
            { state : ParserState }

        type alias OpenWrappedState r =
            { state : { r | column : Int } }

        parseIdentifier : ParserState -> ParserState
        parseIdentifier state =
            { state | offset = state.offset + 1, column = state.column + 1 }

        updateLocal : ParserState -> ParserState
        updateLocal state =
            let
                stateAtFirst =
                    state
            in
            { stateAtFirst | offset = stateAtFirst.offset + 1 }

        accessLocal : ParserState -> Int
        accessLocal state =
            let
                stateAtFirst =
                    state
            in
            stateAtFirst.offset

        skipTrivia : ParserState -> ParserState
        skipTrivia state =
            state

        accessAfterFunction : ParserState -> Int
        accessAfterFunction state =
            let
                stateAtArgument =
                    skipTrivia state
            in
            stateAtArgument.column + state.column

        accessNested : WrappedState -> Int
        accessNested wrapped =
            wrapped.state.column

        accessNestedBinding : WrappedState -> Int
        accessNestedBinding wrapped =
            let
                stateAtArgument =
                    wrapped.state
            in
            stateAtArgument.column

        updateNestedBinding : WrappedState -> ParserState
        updateNestedBinding wrapped =
            let
                stateAtArgument =
                    wrapped.state
            in
            { stateAtArgument | column = 7 }

        accessLiteral : Int
        accessLiteral =
            { column = 1, offset = 2 }.column

        accessUpdated : ParserState -> Int
        accessUpdated state =
            ({ state | column = 7 }).column

        accessOpenNested : OpenWrappedState r -> Int
        accessOpenNested wrapped =
            wrapped.state.column

        updateOpen : { r | offset : Int } -> { r | offset : Int }
        updateOpen state =
            { state | offset = state.offset + 1 }

        accessOpen : { r | offset : Int } -> Int
        accessOpen state =
            state.offset
        """;

    [Fact]
    public void ParseIdentifier_closed_record_alias_uses_direct_record_update()
    {
        var function = CompileFunction("parseIdentifier");

        HasGenericRecordOperation(function, RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Should().BeFalse();

        var state =
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(4)),
                ("offset", ElmValue.Integer(2)),
                ("row", ElmValue.Integer(3)),
                ("source", ElmValue.StringInstance("ab"))
                ]);

        var (result, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                state,
                ElmCompilerTestHelper.PineVMForProfiling(_ => { }));

        result.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(5)),
                ("offset", ElmValue.Integer(3)),
                ("row", ElmValue.Integer(3)),
                ("source", ElmValue.StringInstance("ab"))
                ]));
    }

    [Fact]
    public void Local_binding_of_closed_record_alias_uses_direct_update_and_access()
    {
        HasGenericRecordOperation(
            CompileFunction("updateLocal"),
            RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Should().BeFalse();

        HasGenericRecordOperation(
            CompileFunction("accessLocal"),
            RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Should().BeFalse();
    }

    [Fact]
    public void Parser_state_and_state_at_argument_use_direct_record_access()
    {
        HasGenericRecordOperation(
            CompileFunction("parseIdentifier"),
            RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Should().BeFalse();

        HasGenericRecordOperation(
            CompileFunction("accessAfterFunction"),
            RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Should().BeFalse();
    }

    [Fact]
    public void Closed_record_expressions_use_direct_record_access()
    {
        foreach (var name in new[] { "accessNested", "accessNestedBinding", "accessLiteral", "accessUpdated" })
        {
            HasGenericRecordOperation(
                CompileFunction(name),
                RecordRuntime.PineFunctionForRecordAccessAsValue)
                .Should().BeFalse("the type of {0} is a closed record", name);
        }

        HasGenericRecordOperation(
            CompileFunction("updateNestedBinding"),
            RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Should().BeFalse();

        var state =
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(4)),
                ("offset", ElmValue.Integer(2)),
                ("row", ElmValue.Integer(3)),
                ("source", ElmValue.StringInstance("ab"))
                ]);

        var wrapped = new ElmValue.ElmRecord([("state", state)]);

        foreach (var name in new[] { "accessNested", "accessNestedBinding" })
        {
            var (value, _) =
                CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                    CompileFunction(name),
                    wrapped,
                    ElmCompilerTestHelper.PineVMForProfiling(_ => { }));

            value.Should().Be(ElmValue.Integer(4));
        }

        var (updatedColumn, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                CompileFunction("accessUpdated"),
                state,
                ElmCompilerTestHelper.PineVMForProfiling(_ => { }));

        updatedColumn.Should().Be(ElmValue.Integer(7));

        var (updatedState, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                CompileFunction("updateNestedBinding"),
                wrapped,
                ElmCompilerTestHelper.PineVMForProfiling(_ => { }));

        updatedState.Should().Be(
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(7)),
                ("offset", ElmValue.Integer(2)),
                ("row", ElmValue.Integer(3)),
                ("source", ElmValue.StringInstance("ab"))
                ]));
    }

    [Fact]
    public void Open_record_still_uses_generic_update()
    {
        HasGenericRecordOperation(
            CompileFunction("updateOpen"),
            RecordRuntime.PineFunctionForRecordUpdateAsValue)
            .Should().BeTrue();

        HasGenericRecordOperation(
            CompileFunction("accessOpen"),
            RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Should().BeTrue();

        HasGenericRecordOperation(
            CompileFunction("accessOpenNested"),
            RecordRuntime.PineFunctionForRecordAccessAsValue)
            .Should().BeTrue();
    }

    private static bool HasGenericRecordOperation(PineValue functionValue, PineValue operation)
    {
        var parsed =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, new PineVMParseCache())
            .Extract(err => throw new Exception(err));

        return
            Expression.EnumerateSelfAndDescendants(parsed.InnerFunction)
            .OfType<Expression.Eval>()
            .Any(eval => eval.Encoded is Expression.Litral literal && literal.Value == operation);
    }

    private static PineValue CompileFunction(string name) =>
        ElmCompilerTestHelper.CompileElmModules([ModuleText], disableInlining: false)
        .parsedEnv.Modules.Single(module => module.moduleName is "Test")
        .moduleContent.FunctionDeclarations[name];
}
