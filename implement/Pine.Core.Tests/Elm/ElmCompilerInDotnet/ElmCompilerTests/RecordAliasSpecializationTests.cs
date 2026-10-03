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

        updateOpen : { r | offset : Int } -> { r | offset : Int }
        updateOpen state =
            { state | offset = state.offset + 1 }
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
    public void Open_record_still_uses_generic_update()
    {
        HasGenericRecordOperation(
            CompileFunction("updateOpen"),
            RecordRuntime.PineFunctionForRecordUpdateAsValue)
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
