using Pine;
using Pine.Json;
using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace ElmTime.StateShim.InterfaceToHost;

[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record StateShimRequestStruct
{
    public record ListExposedFunctionsShimRequest : StateShimRequestStruct;

    public record ApplyFunctionShimRequest(ApplyFunctionShimRequestStruct ApplyFunction)
        : StateShimRequestStruct;

    public record SerializeStateShimRequest(StateSource StateSource)
        : StateShimRequestStruct;

    public record SetBranchesStateShimRequest(StateSource StateSource, IReadOnlyList<string> Branches)
        : StateShimRequestStruct;

    public record EstimateSerializedStateLengthShimRequest(StateSource StateSource)
        : StateShimRequestStruct;

    public record ListBranchesShimRequest
        : StateShimRequestStruct;

    public record RemoveBranchesShimRequest(IReadOnlyList<string> BranchesNames)
        : StateShimRequestStruct;

    public record TestAreStatesEqualRequest(IReadOnlyList<StateSource> StatesSources)
        : StateShimRequestStruct;

    public string SerializeToJsonString() =>
        JsonSerializer.Serialize(this);
}

[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record StateShimResponseStruct
{
    public record AppEventShimResponse(FunctionApplicationResult FunctionApplicationResult)
        : StateShimResponseStruct;

    public record ListExposedFunctionsShimResponse(IReadOnlyList<NamedExposedFunction> Functions)
        : StateShimResponseStruct;

    public record ApplyFunctionShimResponse(Result<string, FunctionApplicationResult> Result)
        : StateShimResponseStruct;

    public record SerializeStateShimResponse(Result<string, JsonElement> Result)
        : StateShimResponseStruct;

    public record SetBranchesStateShimResponse(Result<string, string> Result)
        : StateShimResponseStruct;

    public record EstimateSerializedStateLengthShimResponse(Result<string, long> Result)
        : StateShimResponseStruct;

    public record ListBranchesShimResponse(IReadOnlyList<string> BranchesNames)
        : StateShimResponseStruct;

    public record RemoveBranchesShimResponse(RemoveBranchesShimResponseStruct ResponseStruct)
        : StateShimResponseStruct;

    public record TestAreStatesEqualResponse(Result<string, bool> Result)
        : StateShimResponseStruct;
}

public record ApplyFunctionShimRequestStruct(
    string functionName,
    ApplyFunctionArguments<Maybe<StateSource>> arguments,
    IReadOnlyList<string> stateDestinationBranches);

public record ApplyFunctionArguments<StateT>(
    StateT stateArgument,
    IReadOnlyList<JsonElement> serializedArgumentsJson)
{
    public ApplyFunctionArguments<NewStateT> MapStateArgument<NewStateT>(Func<StateT, NewStateT> map) =>
        new(
            stateArgument: map(stateArgument),
            serializedArgumentsJson: serializedArgumentsJson);
}

public record RemoveBranchesShimResponseStruct(int removedCount);


[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record StateSource
{
    public record JsonStateSource(JsonElement Json) : StateSource;

    public record BranchStateSource(string Branch) : StateSource;
}

public record FunctionApplicationResult(
    Maybe<JsonElement> resultLessStateJson,
    bool producedStateDifferentFromStateArgument);

public record NamedExposedFunction(
    string functionName,
    ExposedFunctionDescription functionDescription);

public record ExposedFunctionDescription(
    ExposedFunctionReturnTypeDescription returnType,
    IReadOnlyList<ExposedFunctionParameterDescription> parameters);

public record ExposedFunctionReturnTypeDescription(
    string sourceCodeText,
    bool containsAppStateType);

public record ExposedFunctionParameterDescription(
    string patternSourceCodeText,
    string typeSourceCodeText,
    bool typeIsAppStateType);
