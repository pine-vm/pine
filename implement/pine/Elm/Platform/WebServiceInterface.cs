using ElmTime;
using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PineVM;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm.Platform;

public class WebServiceInterface
{
    /*
     * 
module Platform.WebService exposing (..)

{-| This module contains the types describing the Pine / Elm web service platform.
To build a web service app in Elm, copy this module file into your project and add a declaration with the name `webServiceMain` to an Elm module.

For the latest version of the documentation, see <https://pine-vm.org>

-}


{-| Use the type `WebServiceConfig` on a declaration named `webServiceMain` to declare a web service program in an Elm module.
A web service can subscribe to incoming HTTP requests and respond to them. It can also start and manage volatile processes to integrate other software.
-}
type alias WebServiceConfig state =
    { init : ( state, Commands state )
    , subscriptions : state -> Subscriptions state
    }


type alias Subscriptions state =
    { httpRequest : HttpRequestEventStruct -> state -> ( state, Commands state )
    , posixTimeIsPast :
        Maybe
            { minimumPosixTimeMilli : Int
            , update : { currentPosixTimeMilli : Int } -> state -> ( state, Commands state )
            }
    }


type alias Commands state =
    List (Command state)


type Command state
    = RespondToHttpRequest RespondToHttpRequestStruct
    | CreateVolatileProcess (CreateVolatileProcessStruct state)
    | RequestToVolatileProcess (RequestToVolatileProcessStruct state)
    | TerminateVolatileProcess TerminateVolatileProcessStruct


type alias HttpRequestEventStruct =
    { httpRequestId : String
    , posixTimeMilli : Int
    , requestContext : HttpRequestContext
    , request : HttpRequestProperties
    }


type alias HttpRequestContext =
    { clientAddress : Maybe String
    }


type alias HttpRequestProperties =
    { method : String
    , uri : String
    , bodyAsBase64 : Maybe String
    , headers : List HttpHeader
    }


type alias RespondToHttpRequestStruct =
    { httpRequestId : String
    , response : HttpResponse
    }


type alias HttpResponse =
    { statusCode : Int
    , bodyAsBase64 : Maybe String
    , headersToAdd : List HttpHeader
    }


type alias HttpHeader =
    { name : String
    , values : List String
    }


type alias CreateVolatileProcessStruct state =
    { programCode : String
    , update : CreateVolatileProcessResult -> state -> ( state, Commands state )
    }


type alias CreateVolatileProcessResult =
    Result CreateVolatileProcessErrorStruct CreateVolatileProcessComplete


type alias CreateVolatileProcessErrorStruct =
    { exceptionToString : String
    }


type alias CreateVolatileProcessComplete =
    { processId : String }


type alias RequestToVolatileProcessStruct state =
    { processId : String
    , request : String
    , update : RequestToVolatileProcessResult -> state -> ( state, Commands state )
    }


type alias RequestToVolatileProcessResult =
    Result RequestToVolatileProcessError RequestToVolatileProcessComplete


type RequestToVolatileProcessError
    = ProcessNotFound


type alias RequestToVolatileProcessComplete =
    { exceptionToString : Maybe String
    , returnValueToString : Maybe String
    , durationInMilliseconds : Int
    }


type alias TerminateVolatileProcessStruct =
    { processId : String }

     * */

    public record WebServiceEventResponse(
        PineValue State,
        IReadOnlyList<Command> Commands);

    public record WebServiceConfig(
        WebServiceEventResponse Init,
        ElmInteractiveEnvironment.FunctionRecord Subscriptions,
        ElmTimeJsonAdapter.Parsed JsonAdapter)
    {
        public static WebServiceEventResponse
            EventHttpRequest(
            Subscriptions subscriptions,
            HttpRequestEventStruct httpRequest,
            PineValue stateBefore,
            IPineVM pineVM)
        {
            var functionRecord = subscriptions.HttpRequest;

            if (functionRecord.functionParameterCount is not 2)
            {
                throw new Exception("Expected httpRequest function to have two parameters.");
            }

            var inputEncoded = EncodeHttpRequest(httpRequest);

            return
                ApplyUpdate(functionRecord, [inputEncoded], stateBefore, pineVM);
        }

        public static WebServiceEventResponse?
            EventPosixTime(
            Subscriptions subscriptions,
            long posixTimeMilli,
            PineValue stateBefore,
            IPineVM pineVM)
        {
            if (subscriptions.PosixTimeIsPast is not { } posixTimeSub)
            {
                return null;
            }

            if (posixTimeMilli < posixTimeSub.MinimumPosixTimeMilli)
            {
                return null;
            }

            var functionRecord = posixTimeSub.Update;

            if (functionRecord.functionParameterCount is not 2)
            {
                throw new Exception("Expected posixTimeIsPast function to have two parameters.");
            }

            var inputEncoded =
                ElmValueEncoding.ElmValueAsPineValue(
                    new ElmValue.ElmRecord(
                        [
                        ("currentPosixTimeMilli",
                        ElmValue.Integer(posixTimeMilli))
                        ]));

            return
                ApplyUpdate(functionRecord, [inputEncoded], stateBefore, pineVM);
        }

        public static Subscriptions ParseSubscriptions(
            WebServiceConfig config,
            PineValue stateBefore,
            IPineVM pineVM)
        {
            var subscriptionsValue =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    config.Subscriptions,
                    [stateBefore])
                .Extract(err => throw new Exception("Failed applying function subscriptions: " + err));

            var subscriptionsElmRecord =
                ElmValueEncoding.ParsePineValueAsRecordTagged(subscriptionsValue)
                .Extract(err => throw new Exception("Failed parsing subscriptions value as Elm record: " + err));

            var httpRequestFieldValue =
                subscriptionsElmRecord
                .First(field => field.fieldName is "httpRequest").fieldValue;

            if (httpRequestFieldValue is null)
            {
                throw new Exception("Missing field: httpRequest");
            }

            var httpRequestFunctionRecord =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    httpRequestFieldValue,
                    parseCache)
                .Extract(err => throw new Exception("Failed parsing httpRequest function record: " + err));

            var posixTimeIsPastFieldValue =
                subscriptionsElmRecord
                .First(field => field.fieldName is "posixTimeIsPast").fieldValue;

            if (posixTimeIsPastFieldValue is null)
            {
                throw new Exception("Missing field: posixTimeIsPast");
            }

            var posixTimeIsPastSubscription =
                ElmValueInterop.ParseElmMaybeValue<PosixTimeIsPastSubscription?>(
                    posixTimeIsPastFieldValue,
                    nothing: () => null,
                    just: ParsePosixTimeIsPastSubscription,
                    invalid: err => throw new Exception("Failed parsing posixTimeIsPast: " + err));

            return new Subscriptions(
                HttpRequest: httpRequestFunctionRecord,
                PosixTimeIsPast: posixTimeIsPastSubscription);
        }

        public static PosixTimeIsPastSubscription ParsePosixTimeIsPastSubscription(
            PineValue posixTimeIsPastValue)
        {
            var posixTimeIsPastElmRecord =
                ElmValueEncoding.ParsePineValueAsRecordTagged(posixTimeIsPastValue)
                .Extract(err => throw new Exception("Failed parsing posixTimeIsPast value as Elm record: " + err));

            var minimumPosixTimeMilliField =
                posixTimeIsPastElmRecord
                .First(field => field.fieldName is "minimumPosixTimeMilli").fieldValue;

            if (minimumPosixTimeMilliField is null)
            {
                throw new Exception("Missing field: minimumPosixTimeMilli");
            }

            var minimumPosixTimeMilli =
                PineValueAsInteger.SignedIntegerFromValueRelaxed(minimumPosixTimeMilliField)
                .Extract(err => throw new Exception("Failed parsing minimumPosixTimeMilli: " + err));

            var updateField =
                posixTimeIsPastElmRecord
                .First(field => field.fieldName is "update").fieldValue;

            if (updateField is null)
            {
                throw new Exception("Missing field: update");
            }

            var updateFunctionRecord =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    updateField,
                    parseCache)
                .Extract(err => throw new Exception("Failed parsing update function record: " + err));

            if (updateFunctionRecord.functionParameterCount is not 2)
            {
                throw new Exception("Expected posixTimeIsPast function to have one parameter.");
            }

            return new PosixTimeIsPastSubscription(
                MinimumPosixTimeMilli: (long)minimumPosixTimeMilli,
                Update: updateFunctionRecord);
        }

        public static WebServiceEventResponse
            ApplyUpdate(
            ElmInteractiveEnvironment.FunctionRecord functionRecord,
            IReadOnlyList<PineValue> argumentsBeforeState,
            PineValue stateBefore,
            IPineVM pineVM)
        {
            var responseValue =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    functionRecord,
                    [.. argumentsBeforeState, stateBefore])
                .Extract(err => throw new Exception("Failed applying function: " + err));

            var parseResponseResult =
                ParseWebServiceEventResponse(responseValue, elmCompilerCache, parseCache);

            {
                if (parseResponseResult.IsErrOrNull() is { } err)
                {
                    throw new Exception("Failed to parse response: " + err);
                }
            }

            if (parseResponseResult.IsOkOrNull() is not { } response)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parseResponseResult);
            }

            return response;
        }
    }

    public abstract record Command
    {
        /*
         * 
        type Command state
            = RespondToHttpRequest RespondToHttpRequestStruct
            | CreateVolatileProcess (CreateVolatileProcessStruct state)
              {-
                 We use the `runtimeIdentifier` and `osPlatform` properties to select the right executable files when creating a (native) volatile process.
                 The properties returned by this command comes from the `RuntimeInformation` documented at <https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.runtimeinformation>
              -}
            | ReadRuntimeInformationCommand (ReadRuntimeInformationCommandStruct state)
            | CreateVolatileProcessNativeCommand (CreateVolatileProcessNativeCommandStruct state)
            | RequestToVolatileProcess (RequestToVolatileProcessStruct state)
            | WriteToVolatileProcessNativeStdInCommand (WriteToVolatileProcessNativeStdInStruct state)
            | ReadAllFromVolatileProcessNativeCommand (ReadAllFromVolatileProcessNativeStruct state)
            | TerminateVolatileProcess TerminateVolatileProcessStruct


        type alias ReadRuntimeInformationCommandStruct state =
            Result String RuntimeInformationRecord -> state -> ( state, Commands state )


        type alias RuntimeInformationRecord =
            { runtimeIdentifier : String
            , osPlatform : Maybe String
            }


        type alias CreateVolatileProcessNativeCommandStruct state =
            { request : CreateVolatileProcessNativeRequestStruct
            , update : CreateVolatileProcessResult -> state -> ( state, Commands state )
            }


        type alias CreateVolatileProcessNativeRequestStruct =
            { executableFile : LoadDependencyStruct
            , arguments : String
            , environmentVariables : List ProcessEnvironmentVariableStruct
            }


        type alias ProcessEnvironmentVariableStruct =
            { key : String
            , value : String
            }


        type alias LoadDependencyStruct =
            { hashSha256Base16 : String
            , hintUrls : List String
            }


        type alias WriteToVolatileProcessNativeStdInStruct state =
            { processId : String
            , stdInBase64 : String
            , update :
                Result RequestToVolatileProcessError ()
                -> state
                -> ( state, Commands state )
            }


        type alias ReadAllFromVolatileProcessNativeStruct state =
            { processId : String
            , update :
                Result RequestToVolatileProcessError ReadAllFromVolatileProcessNativeSuccessStruct
                -> state
                -> ( state, Commands state )
            }


        type alias ReadAllFromVolatileProcessNativeSuccessStruct =
            { stdOutBase64 : String
            , stdErrBase64 : String
            , exitCode : Maybe Int
            }

         * */

        public sealed record RespondToHttpRequest(
            RespondToHttpRequestStruct Respond)
            : Command;

        public sealed record CreateVolatileProcess(
            CreateVolatileProcessStruct Create)
            : Command;

        public sealed record ReadRuntimeInformationCommand(
            ReadRuntimeInformationCommandStruct Read)
            : Command;

        public sealed record CreateVolatileProcessNativeCommand(
            CreateVolatileProcessNativeCommandStruct Create)
            : Command;

        public sealed record RequestToVolatileProcess(
            RequestToVolatileProcessStruct Request)
            : Command;

        public sealed record TerminateVolatileProcess(
            string ProcessId)
            : Command;

        public sealed record WriteToVolatileProcessNativeStdInCommand(
            WriteToVolatileProcessNativeStdInStruct Write)
            : Command;

        public sealed record ReadAllFromVolatileProcessNativeCommand(
            ReadAllFromVolatileProcessNativeStruct Read)
            : Command;
    }

    public record ReadRuntimeInformationCommandStruct(
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record ReadAllFromVolatileProcessNativeSuccessStruct(
        string StdOutBase64,
        string StdErrBase64,
        int? ExitCode);

    public record CreateVolatileProcessNativeCommandStruct(
        CreateVolatileProcessNativeRequestStruct Request,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record CreateVolatileProcessNativeRequestStruct(
        LoadDependencyStruct ExecutableFile,
        string Arguments,
        IReadOnlyList<ProcessEnvironmentVariableStruct> EnvironmentVariables);

    public record ProcessEnvironmentVariableStruct(
        string Key,
        string Value);

    public record LoadDependencyStruct(
        string HashSha256Base16,
        IReadOnlyList<string> HintUrls);

    public record RuntimeInformationRecord(
        string RuntimeIdentifier,
        string? OsPlatform);

    public record Subscriptions(
        ElmInteractiveEnvironment.FunctionRecord HttpRequest,
        PosixTimeIsPastSubscription? PosixTimeIsPast);

    public record HttpRequestEventStruct(
        string HttpRequestId,
        long PosixTimeMilli,
        HttpRequestContext RequestContext,
        HttpRequestProperties Request);

    public record PosixTimeIsPastSubscription(
        long MinimumPosixTimeMilli,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record HttpRequestContext(
        string? ClientAddress);

    public record HttpRequestProperties(
        string Method,
        string Uri,
        string? BodyAsBase64,
        IReadOnlyList<HttpHeader> Headers);

    public record RespondToHttpRequestStruct(
        string HttpRequestId,
        HttpResponse Response);

    public record HttpResponse(
        int StatusCode,
        string? BodyAsBase64,
        IReadOnlyList<HttpHeader> HeadersToAdd);

    public record HttpHeader(
        string Name,
        IReadOnlyList<string> Values);

    public record CreateVolatileProcessStruct(
        string ProgramCode,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record RequestToVolatileProcessStruct(
        string ProcessId,
        string Request,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record WriteToVolatileProcessNativeStdInStruct(
        string ProcessId,
        string StdInBase64,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record ReadAllFromVolatileProcessNativeStruct(
        string ProcessId,
        ElmInteractiveEnvironment.FunctionRecord Update);

    public record CreateVolatileProcessErrorStruct(
        string ExceptionToString);

    public record CreateVolatileProcessComplete(
        string ProcessId);

    public abstract record RequestToVolatileProcessError
    {
        public sealed record ProcessNotFound
            : RequestToVolatileProcessError;

        public sealed record RequestToVolatileProcessOtherError(
            string Error)
            : RequestToVolatileProcessError;

        public static readonly RequestToVolatileProcessError ProcessNotFoundInstance =
            new ProcessNotFound();
    }

    public record RequestToVolatileProcessComplete(
        string? ExceptionToString,
        string? ReturnValueToString,
        int DurationInMilliseconds);

    public static PineValue EncodeHttpRequest(HttpRequestEventStruct httpRequest)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("httpRequestId", ElmValue.StringInstance(httpRequest.HttpRequestId)),
                ("posixTimeMilli", ElmValue.Integer(httpRequest.PosixTimeMilli)),
                ("requestContext", EncodeHttpRequestContext(httpRequest.RequestContext)),
                ("request", EncodeHttpRequestProperties(httpRequest.Request))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static ElmValue EncodeHttpRequestContext(HttpRequestContext requestContext)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("clientAddress", requestContext.ClientAddress is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.StringInstance(requestContext.ClientAddress)]))
                ]);

        return asElmValue;
    }

    public static ElmValue EncodeHttpRequestProperties(HttpRequestProperties request)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("method", ElmValue.StringInstance(request.Method)),
                ("uri", ElmValue.StringInstance(request.Uri)),
                ("bodyAsBase64", request.BodyAsBase64 is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.StringInstance(request.BodyAsBase64)])),
                ("headers", new ElmValue.ElmList([.. request.Headers.Select(EncodeHttpHeader)]))
                ]);
        return asElmValue;
    }

    public static ElmValue EncodeHttpHeader(HttpHeader header)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("name", ElmValue.StringInstance(header.Name)),
                ("values", new ElmValue.ElmList([.. header.Values.Select(value => ElmValue.StringInstance(value))]))
                ]);

        return asElmValue;
    }

    public static PineValue EncodeCreateVolatileProcessResult(
        Result<CreateVolatileProcessErrorStruct, CreateVolatileProcessComplete> result)
    {
        return
            EncodeResult(
                result,
                encodeErr: EncodeCreateVolatileProcessErrorStruct,
                encodeOk: EncodeCreateVolatileProcessComplete);
    }

    public static PineValue EncodeReadRuntimeInformationResult(
        Result<string, RuntimeInformationRecord> result)
    {
        return
            EncodeResult(
                result,
                encodeErr: PineValueAsString.ValueFromString,
                encodeOk: EncodeRuntimeInformationRecord);
    }

    public static PineValue EncodeRuntimeInformationRecord(
        RuntimeInformationRecord record)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("runtimeIdentifier", ElmValue.StringInstance(record.RuntimeIdentifier)),
                ("osPlatform", record.OsPlatform is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.StringInstance(record.OsPlatform)]))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static PineValue EncodeResult<ErrT, OkT>(
        Result<ErrT, OkT> result,
        Func<ErrT, PineValue> encodeErr,
        Func<OkT, PineValue> encodeOk)
    {
        (string tagName, PineValue tagArg) DeconstructResult(
            Result<ErrT, OkT> result)
        {
            return result switch
            {
                Result<ErrT, OkT>.Err err =>
                    ("Err", encodeErr(err.Value)),

                Result<ErrT, OkT>.Ok ok =>
                    ("Ok", encodeOk(ok.Value)),

                _ =>
                throw new NotImplementedException("Unexpected result type: " + result)
            };
        }

        var (tagString, argValue) = DeconstructResult(result);

        return
            PineValue.List(
                [
                PineValueAsString.ValueFromString(tagString),
                PineValue.List(
                    [
                    argValue
                    ])
                ]);
    }

    public static PineValue EncodeCreateVolatileProcessErrorStruct(
        CreateVolatileProcessErrorStruct error)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("exceptionToString", ElmValue.StringInstance(error.ExceptionToString))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static PineValue EncodeCreateVolatileProcessComplete(
        CreateVolatileProcessComplete complete)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("processId", ElmValue.StringInstance(complete.ProcessId))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static PineValue EncodeRequestToVolatileProcessResult(
        Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete> result)
    {
        return
            EncodeResult(
                result,
                encodeErr: EncodeRequestToVolatileProcessError,
                encodeOk: EncodeRequestToVolatileProcessComplete);
    }

    public static PineValue EncodeWriteToVolatileProcessNativeStdInResult<T>(
        Result<RequestToVolatileProcessError, T> result)
    {
        return
            EncodeResult(
                result,
                encodeErr: EncodeRequestToVolatileProcessError,
                encodeOk: _ => ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Ok", [])));
    }

    public static PineValue EncodeRequestToVolatileProcessError(
        RequestToVolatileProcessError error)
    {
        return error switch
        {
            RequestToVolatileProcessError.ProcessNotFound =>
                ElmValueEncoding.ElmValueAsPineValue(
                    ElmValue.TagInstance(nameof(RequestToVolatileProcessError.ProcessNotFound), [])),

            RequestToVolatileProcessError.RequestToVolatileProcessOtherError otherError =>
                ElmValueEncoding.ElmValueAsPineValue(
                    ElmValue.TagInstance(
                        nameof(RequestToVolatileProcessError.RequestToVolatileProcessOtherError),
                        [
                            ElmValue.StringInstance(otherError.Error)
                        ])),

            _ =>
            throw new NotImplementedException(
                "Unexpected error type: " + error)
        };
    }

    public static PineValue EncodeRequestToVolatileProcessComplete(
        RequestToVolatileProcessComplete complete)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("exceptionToString", complete.ExceptionToString is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.StringInstance(complete.ExceptionToString)])),

                ("returnValueToString", complete.ReturnValueToString is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.StringInstance(complete.ReturnValueToString)])),

                ("durationInMilliseconds", ElmValue.Integer(complete.DurationInMilliseconds))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static PineValue EncodeEncodeReadAllFromVolatileProcessNativeResult(
        Result<RequestToVolatileProcessError, ReadAllFromVolatileProcessNativeSuccessStruct> result)
    {
        return
            EncodeResult(
                result,
                encodeErr: EncodeRequestToVolatileProcessError,
                encodeOk: EncodeReadAllFromVolatileProcessNativeSuccessStruct);
    }

    public static PineValue EncodeReadAllFromVolatileProcessNativeSuccessStruct(
        ReadAllFromVolatileProcessNativeSuccessStruct complete)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("stdOutBase64", ElmValue.StringInstance(complete.StdOutBase64)),
                ("stdErrBase64", ElmValue.StringInstance(complete.StdErrBase64)),
                ("exitCode", complete.ExitCode is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [ElmValue.Integer(complete.ExitCode.Value)]))
                ]);

        return ElmValueEncoding.ElmValueAsPineValue(asElmValue);
    }

    public static Result<string, WebServiceEventResponse> ParseWebServiceConfigInit(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        return ParseWebServiceEventResponse(pineValue, elmCompilerCache, parseCache);
    }

    public static Result<string, WebServiceEventResponse> ParseWebServiceEventResponse(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        if (pineValue is not PineValue.ListValue listValue)
        {
            return "Expected list value but got: " + pineValue;
        }

        if (listValue.Elements.Length is not 2)
        {
            return "Expected 2 elements but got: " + listValue.Elements.Length;
        }

        var stateValue = listValue.Elements.Span[0];

        var commandsValue = listValue.Elements.Span[1];

        if (commandsValue is not PineValue.ListValue commandsListValue)
        {
            return "Expected list value for commands but got: " + commandsValue;
        }

        var commands = new Command[commandsListValue.Elements.Length];

        for (var i = 0; i < commands.Length; i++)
        {
            var commandValue = commandsListValue.Elements.Span[i];

            var parsedCommandResult =
                ParseCommand(
                    commandValue,
                    elmCompilerCache,
                    parseCache);

            {
                if (parsedCommandResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse command: " + err;
                }
            }

            if (parsedCommandResult.IsOkOrNull() is not { } command)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedCommandResult);
            }

            commands[i] = command;
        }

        return new WebServiceEventResponse(stateValue, commands);
    }

    public static Result<string, Command> ParseCommand(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var asTagResult = ElmValueEncoding.ParseAsTag(pineValue);

        {
            if (asTagResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as tag: " + err;
            }
        }

        if (asTagResult.IsOkOrNullable() is not { } tag)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asTagResult);
        }

        if (tag.tagName is nameof(Command.RespondToHttpRequest))
        {
            var parsedRespondToHttpRequest = ParseRespondToHttpRequestStruct(tag.tagArguments.Span[0], elmCompilerCache);

            {
                if (parsedRespondToHttpRequest.IsErrOrNull() is { } err)
                {
                    return "Failed to parse RespondToHttpRequest: " + err;
                }
            }

            if (parsedRespondToHttpRequest.IsOkOrNull() is not { } respondToHttpRequest)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedRespondToHttpRequest);
            }

            return new Command.RespondToHttpRequest(respondToHttpRequest);
        }

        if (tag.tagName is nameof(Command.CreateVolatileProcess))
        {
            var parsedCreateVolatileProcess =
                ParseCreateVolatileProcessStruct(
                    tag.tagArguments.Span[0],
                    elmCompilerCache,
                    parseCache);

            {
                if (parsedCreateVolatileProcess.IsErrOrNull() is { } err)
                {
                    return "Failed to parse CreateVolatileProcess: " + err;
                }
            }

            if (parsedCreateVolatileProcess.IsOkOrNull() is not { } createVolatileProcess)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedCreateVolatileProcess);
            }

            return new Command.CreateVolatileProcess(createVolatileProcess);
        }

        if (tag.tagName is nameof(Command.ReadRuntimeInformationCommand))
        {
            if (tag.tagArguments.Length is not 1)
            {
                return "Expected 1 argument but got: " + tag.tagArguments.Length;
            }

            var runtimeInformationRecordValue =
                tag.tagArguments.Span[0];

            var parsedRuntimeInformationRecord =
                ParseRuntimeInformationRecord(runtimeInformationRecordValue);

            {
                if (parsedRuntimeInformationRecord.IsErrOrNull() is { } err)
                {
                    return "Failed to parse RuntimeInformationRecord: " + err;
                }
            }

            if (parsedRuntimeInformationRecord.IsOkOrNull() is not { } runtimeInformationRecord)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedRuntimeInformationRecord);
            }

            return new Command.ReadRuntimeInformationCommand(runtimeInformationRecord);
        }

        if (tag.tagName is nameof(Command.CreateVolatileProcessNativeCommand))
        {
            var parsedCreateVolatileProcessNativeCommand =
                ParseCreateVolatileProcessNativeCommandStruct(
                    tag.tagArguments.Span[0],
                    elmCompilerCache,
                    parseCache);

            {
                if (parsedCreateVolatileProcessNativeCommand.IsErrOrNull() is { } err)
                {
                    return "Failed to parse CreateVolatileProcessNativeCommand: " + err;
                }
            }

            if (parsedCreateVolatileProcessNativeCommand.IsOkOrNull() is not { } createVolatileProcessNativeCommand)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedCreateVolatileProcessNativeCommand);
            }

            return new Command.CreateVolatileProcessNativeCommand(createVolatileProcessNativeCommand);
        }

        if (tag.tagName is nameof(Command.RequestToVolatileProcess))
        {
            var parsedRequestToVolatileProcess =
                ParseRequestToVolatileProcessStruct(
                    tag.tagArguments.Span[0],
                    elmCompilerCache,
                    parseCache);
            {
                if (parsedRequestToVolatileProcess.IsErrOrNull() is { } err)
                {
                    return "Failed to parse RequestToVolatileProcess: " + err;
                }
            }

            if (parsedRequestToVolatileProcess.IsOkOrNull() is not { } requestToVolatileProcess)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedRequestToVolatileProcess);
            }

            return new Command.RequestToVolatileProcess(requestToVolatileProcess);
        }

        if (tag.tagName is nameof(Command.WriteToVolatileProcessNativeStdInCommand))
        {
            var parsedWriteToVolatileProcessNativeStdIn =
                ParseWriteToVolatileProcessNativeStdInStruct(
                    tag.tagArguments.Span[0],
                    elmCompilerCache,
                    parseCache);

            {
                if (parsedWriteToVolatileProcessNativeStdIn.IsErrOrNull() is { } err)
                {
                    return "Failed to parse WriteToVolatileProcessNativeStdIn: " + err;
                }
            }

            if (parsedWriteToVolatileProcessNativeStdIn.IsOkOrNull() is not { } writeToVolatileProcessNativeStdIn)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedWriteToVolatileProcessNativeStdIn);
            }

            return new Command.WriteToVolatileProcessNativeStdInCommand(writeToVolatileProcessNativeStdIn);
        }

        if (tag.tagName is nameof(Command.ReadAllFromVolatileProcessNativeCommand))
        {
            var parsedReadAllFromVolatileProcessNative =
                ParseReadAllFromVolatileProcessNativeStruct(
                    tag.tagArguments.Span[0],
                    elmCompilerCache,
                    parseCache);

            {
                if (parsedReadAllFromVolatileProcessNative.IsErrOrNull() is { } err)
                {
                    return "Failed to parse ReadAllFromVolatileProcessNative: " + err;
                }
            }

            if (parsedReadAllFromVolatileProcessNative.IsOkOrNull() is not { } readAllFromVolatileProcessNative)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedReadAllFromVolatileProcessNative);
            }

            return new Command.ReadAllFromVolatileProcessNativeCommand(readAllFromVolatileProcessNative);
        }

        if (tag.tagName is nameof(Command.TerminateVolatileProcess))
        {
            if (tag.tagArguments.Length is not 1)
            {
                return "Expected 1 argument but got: " + tag.tagArguments.Length;
            }

            var processIdAsElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(tag.tagArguments.Span[0]);

            {
                if (processIdAsElmValueResult.IsErrOrNull() is { } err)
                {
                    return "Failed to decode processId: " + err;
                }
            }

            if (processIdAsElmValueResult.IsOkOrNull() is not { } processIdValue)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + processIdAsElmValueResult);
            }

            if (processIdValue is not ElmValue.ElmString processIdString)
            {
                return "Unexpected type in processId: " + processIdValue;
            }

            return new Command.TerminateVolatileProcess(processIdString.Value);
        }

        return "Unexpected tag: " + tag.tagName;
    }

    public static Result<string, ReadRuntimeInformationCommandStruct>
        ParseRuntimeInformationRecord(
        PineValue pineValue)
    {
        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(pineValue, parseCache);

        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                return err;
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } parseUpdateOk)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        var remainingParamCount =
            parseUpdateOk.functionParameterCount -
            parseUpdateOk.argumentsAlreadyCollected.Length;

        if (remainingParamCount is not 2)
        {
            return "Expected function to have two remaining parameters but got: " + remainingParamCount;
        }

        return new ReadRuntimeInformationCommandStruct(parseUpdateOk);
    }

    public static Result<string, CreateVolatileProcessNativeCommandStruct>
        ParseCreateVolatileProcessNativeCommandStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var parseRecordResult =
            ElmValueEncoding.ParsePineValueAsRecordTagged(pineValue);

        {
            if (parseRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as record: " + err;
            }
        }

        if (parseRecordResult.IsOkOrNull() is not { } elmRecordValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseRecordResult);
        }

        var requestValue =
            elmRecordValue
            .FirstOrDefault(field => field.fieldName is "request").fieldValue;

        if (requestValue is null)
        {
            return "Missing field: request";
        }

        var parsedRequest =
            ParseCreateVolatileProcessNativeRequestStruct(requestValue, elmCompilerCache);

        {
            if (parsedRequest.IsErrOrNull() is { } err)
            {
                return "Failed to parse request: " + err;
            }
        }

        if (parsedRequest.IsOkOrNull() is not { } request)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parsedRequest);
        }

        var updateValue =
            elmRecordValue
            .FirstOrDefault(field => field.fieldName is "update").fieldValue;

        if (updateValue is null)
        {
            return "Missing field: update";
        }

        var parsedUpdate =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(updateValue, parseCache);

        {
            if (parsedUpdate.IsErrOrNull() is { } err)
            {
                return "Failed to parse update: " + err;
            }
        }

        if (parsedUpdate.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parsedUpdate);
        }

        var remainingParamCount =
            update.functionParameterCount -
            update.argumentsAlreadyCollected.Length;

        if (remainingParamCount is not 2)
        {
            return "Expected function to have two remaining parameters but got: " + remainingParamCount;
        }

        return new CreateVolatileProcessNativeCommandStruct(request, update);
    }

    public static Result<string, CreateVolatileProcessNativeRequestStruct>
        ParseCreateVolatileProcessNativeRequestStruct(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
    {
        var asElmValueResult =
            elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

        {
            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode Elm value: " + err;
            }
        }

        if (asElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asElmValueResult);
        }

        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 3)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var executableFileValue = elmRecordValue["executableFile"];

        if (executableFileValue is null)
        {
            return "Missing field: executableFile";
        }

        var parsedExecutableFile =
            ParseLoadDependencyStruct(executableFileValue);

        {
            if (parsedExecutableFile.IsErrOrNull() is { } err)
            {
                return "Failed to parse executableFile: " + err;
            }
        }

        if (parsedExecutableFile.IsOkOrNull() is not { } executableFile)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parsedExecutableFile);
        }

        var argumentsValue = elmRecordValue["arguments"];

        if (argumentsValue is null)
        {
            return "Missing field: arguments";
        }

        if (argumentsValue is not ElmValue.ElmString argumentsString)
        {
            return "Unexpected type in arguments: " + argumentsValue;
        }

        var environmentVariablesValue = elmRecordValue["environmentVariables"];

        if (environmentVariablesValue is null)
        {
            return "Missing field: environmentVariables";
        }

        if (environmentVariablesValue is not ElmValue.ElmList environmentVariablesList)
        {
            return "Unexpected type in environmentVariables: " + environmentVariablesValue;
        }

        var environmentVariables =
            new ProcessEnvironmentVariableStruct[environmentVariablesList.Elements.Count];

        for (var i = 0; i < environmentVariables.Length; i++)
        {
            var environmentVariableValue = environmentVariablesList.Elements[i];

            var parsedEnvironmentVariable =
                ParseProcessEnvironmentVariableStruct(environmentVariableValue, elmCompilerCache);

            {
                if (parsedEnvironmentVariable.IsErrOrNull() is { } err)
                {
                    return "Failed to parse environment variable: " + err;
                }
            }

            if (parsedEnvironmentVariable.IsOkOrNull() is not { } environmentVariable)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedEnvironmentVariable);
            }

            environmentVariables[i] = environmentVariable;
        }

        return new CreateVolatileProcessNativeRequestStruct(
            executableFile,
            argumentsString.Value,
            environmentVariables);
    }

    public static Result<string, LoadDependencyStruct>
        ParseLoadDependencyStruct(ElmValue elmValue)
    {
        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 2)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var hashSha256Base16Value = elmRecordValue["hashSha256Base16"];

        if (hashSha256Base16Value is null)
        {
            return "Missing field: hashSha256Base16";
        }

        if (hashSha256Base16Value is not ElmValue.ElmString hashSha256Base16String)
        {
            return "Unexpected type in hashSha256Base16: " + hashSha256Base16Value;
        }

        var hintUrlsValue = elmRecordValue["hintUrls"];

        if (hintUrlsValue is null)
        {
            return "Missing field: hintUrls";
        }

        if (hintUrlsValue is not ElmValue.ElmList hintUrlsList)
        {
            return "Unexpected type in hintUrls: " + hintUrlsValue;
        }

        var hintUrls = new string[hintUrlsList.Elements.Count];

        for (var i = 0; i < hintUrls.Length; i++)
        {
            var hintUrlValue = hintUrlsList.Elements[i];

            if (hintUrlValue is not ElmValue.ElmString hintUrlString)
            {
                return "Unexpected type in hintUrl: " + hintUrlValue;
            }

            hintUrls[i] = hintUrlString.Value;
        }

        return new LoadDependencyStruct(
            hashSha256Base16String.Value,
            hintUrls);
    }

    public static Result<string, ProcessEnvironmentVariableStruct>
        ParseProcessEnvironmentVariableStruct(
            ElmValue elmValue,
            ElmCompilerCache elmCompilerCache)
    {
        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 2)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var keyValue = elmRecordValue["key"];

        if (keyValue is null)
        {
            return "Missing field: key";
        }

        if (keyValue is not ElmValue.ElmString keyString)
        {
            return "Unexpected type in key: " + keyValue;
        }

        var valueValue = elmRecordValue["value"];

        if (valueValue is null)
        {
            return "Missing field: value";
        }

        if (valueValue is not ElmValue.ElmString valueString)
        {
            return "Unexpected type in value: " + valueValue;
        }

        return new ProcessEnvironmentVariableStruct(
            keyString.Value,
            valueString.Value);
    }

    public static Result<string, RespondToHttpRequestStruct> ParseRespondToHttpRequestStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache)
    {
        var asElmValueResult =
            elmCompilerCache.PineValueDecodedAsElmValue(pineValue);
        {
            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode Elm value: " + err;
            }
        }

        if (asElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asElmValueResult);
        }

        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 2)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var httpRequestIdValue = elmRecordValue["httpRequestId"];

        if (httpRequestIdValue is null)
        {
            return "Missing field: httpRequestId";
        }

        if (httpRequestIdValue is not ElmValue.ElmString httpRequestIdString)
        {
            return "Unexpected type in httpRequestId: " + httpRequestIdValue;
        }

        var responseValue = elmRecordValue["response"];

        if (responseValue is null)
        {
            return "Missing field: response";
        }

        var parsedResponse = ParseHttpResponse(responseValue, elmCompilerCache);

        {
            if (parsedResponse.IsErrOrNull() is { } err)
            {
                return "Failed to parse response: " + err;
            }
        }

        if (parsedResponse.IsOkOrNull() is not { } response)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parsedResponse);
        }

        return new RespondToHttpRequestStruct(
            httpRequestIdString.Value,
            response);
    }

    public static Result<string, HttpResponse> ParseHttpResponse(
        ElmValue elmValue,
        ElmCompilerCache elmCompilerCache)
    {
        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 3)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var statusCodeValue = elmRecordValue["statusCode"];

        if (statusCodeValue is null)
        {
            return "Missing field: statusCode";
        }

        if (statusCodeValue is not ElmValue.ElmInteger statusCodeInt)
        {
            return "Unexpected type in statusCode: " + statusCodeValue;
        }

        var bodyAsBase64Value = elmRecordValue["bodyAsBase64"];

        if (bodyAsBase64Value is null)
        {
            return "Missing field: bodyAsBase64";
        }

        if (bodyAsBase64Value is not ElmValue.ElmTag bodyAsBase64Tag)
        {
            return "Unexpected type in bodyAsBase64: " + bodyAsBase64Value;
        }

        string? bodyAsBase64 = null;

        if (bodyAsBase64Tag.TagName is "Just")
        {
            if (bodyAsBase64Tag.Arguments.Count is not 1)
            {
                return "Unexpected number of arguments in Just: " + bodyAsBase64Tag.Arguments.Count;
            }

            if (bodyAsBase64Tag.Arguments[0] is not ElmValue.ElmString elmStringValue)
            {
                return "Unexpected argument type in Just: " + bodyAsBase64Tag.Arguments[0];
            }

            bodyAsBase64 = elmStringValue.Value;
        }
        else if (bodyAsBase64Tag.TagName is not "Nothing")
        {
            return "Unexpected tag in bodyAsBase64: " + bodyAsBase64Tag.TagName;
        }

        var headersToAddValue = elmRecordValue["headersToAdd"];

        if (headersToAddValue is null)
        {
            return "Missing field: headersToAdd";
        }

        if (headersToAddValue is not ElmValue.ElmList headersToAddList)
        {
            return "Unexpected type in headersToAdd: " + headersToAddValue;
        }

        var headersToAdd = new HttpHeader[headersToAddList.Elements.Count];

        for (var i = 0; i < headersToAdd.Length; i++)
        {
            var headerValue = headersToAddList.Elements[i];

            var parsedHeader = ParseHttpHeader(headerValue, elmCompilerCache);
            {
                if (parsedHeader.IsErrOrNull() is { } err)
                {
                    return "Failed to parse header: " + err;
                }
            }

            if (parsedHeader.IsOkOrNull() is not { } header)
            {
                throw new NotImplementedException(
                    "Unexpected return type: " + parsedHeader);
            }

            headersToAdd[i] = header;
        }

        return new HttpResponse(
            (int)statusCodeInt.Value,
            bodyAsBase64,
            headersToAdd);
    }

    public static Result<string, HttpHeader> ParseHttpHeader(
        ElmValue elmValue,
        ElmCompilerCache elmCompilerCache)
    {
        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 2)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var nameValue = elmRecordValue["name"];

        if (nameValue is null)
        {
            return "Missing field: name";
        }

        if (nameValue is not ElmValue.ElmString nameString)
        {
            return "Unexpected type in name: " + nameValue;
        }

        var valuesValue = elmRecordValue["values"];

        if (valuesValue is null)
        {
            return "Missing field: values";
        }

        if (valuesValue is not ElmValue.ElmList valuesList)
        {
            return "Unexpected type in values: " + valuesValue;
        }

        var values = new string[valuesList.Elements.Count];

        for (var i = 0; i < values.Length; i++)
        {
            var valueValue = valuesList.Elements[i];

            if (valueValue is not ElmValue.ElmString valueString)
            {
                return "Unexpected type in values: " + valueValue;
            }

            values[i] = valueString.Value;
        }

        return new HttpHeader(nameString.Value, values);
    }

    public static Result<string, CreateVolatileProcessStruct> ParseCreateVolatileProcessStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(pineValue);

        {
            if (asRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as record: " + err;
            }
        }

        if (asRecordResult.IsOkOrNull() is not { } record)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asRecordResult);
        }

        if (record.Count is not 2)
        {
            return "Unexpected number of fields: " + record.Count;
        }

        var programCodeValue = record.FirstOrDefault(field => field.fieldName is "programCode");

        if (programCodeValue.fieldValue is null)
        {
            return "Missing field: programCode";
        }

        var programCodeAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(programCodeValue.fieldValue);
        {
            if (programCodeAsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode programCode: " + err;
            }
        }

        if (programCodeAsElmValue.IsOkOrNull() is not { } programCodeElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + programCodeAsElmValue);
        }

        if (programCodeElmValue is not ElmValue.ElmString programCodeString)
        {
            return "Unexpected type in programCode: " + programCodeElmValue;
        }

        var updateValue = record.FirstOrDefault(field => field.fieldName is "update");

        if (updateValue.fieldValue is null)
        {
            return "Missing field: update";
        }

        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(updateValue.fieldValue, parseCache);

        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse update: " + err;
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        return new CreateVolatileProcessStruct(
            programCodeString.Value,
            update);
    }

    public static Result<string, RequestToVolatileProcessStruct> ParseRequestToVolatileProcessStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(pineValue);

        {
            if (asRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as record: " + err;
            }
        }

        if (asRecordResult.IsOkOrNull() is not { } record)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asRecordResult);
        }

        if (record.Count is not 3)
        {
            return "Unexpected number of fields: " + record.Count;
        }

        var processIdValue = record.FirstOrDefault(field => field.fieldName is "processId");

        if (processIdValue.fieldValue is null)
        {
            return "Missing field: processId";
        }

        var processIdAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(processIdValue.fieldValue);
        {
            if (processIdAsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode processId: " + err;
            }
        }

        if (processIdAsElmValue.IsOkOrNull() is not { } processIdElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + processIdAsElmValue);
        }

        if (processIdElmValue is not ElmValue.ElmString processIdString)
        {
            return "Unexpected type in processId: " + processIdElmValue;
        }

        var requestValue = record.FirstOrDefault(field => field.fieldName is "request");

        if (requestValue.fieldValue is null)
        {
            return "Missing field: request";
        }

        var requestAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(requestValue.fieldValue);
        {
            if (requestAsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode request: " + err;
            }
        }

        if (requestAsElmValue.IsOkOrNull() is not { } requestElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + requestAsElmValue);
        }

        if (requestElmValue is not ElmValue.ElmString requestString)
        {
            return "Unexpected type in request: " + requestElmValue;
        }

        var updateValue = record.FirstOrDefault(field => field.fieldName is "update");

        if (updateValue.fieldValue is null)
        {
            return "Missing field: update";
        }

        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(updateValue.fieldValue, parseCache);
        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse update: " + err;
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        return new RequestToVolatileProcessStruct(
            processIdString.Value,
            requestString.Value,
            update);
    }

    public static Result<string, WriteToVolatileProcessNativeStdInStruct> ParseWriteToVolatileProcessNativeStdInStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(pineValue);

        {
            if (asRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as record: " + err;
            }
        }

        if (asRecordResult.IsOkOrNull() is not { } record)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asRecordResult);
        }

        if (record.Count is not 3)
        {
            return "Unexpected number of fields: " + record.Count;
        }

        var processIdValue = record.FirstOrDefault(field => field.fieldName is "processId");

        if (processIdValue.fieldValue is null)
        {
            return "Missing field: processId";
        }

        var processIdAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(processIdValue.fieldValue);
        {
            if (processIdAsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode processId: " + err;
            }
        }

        if (processIdAsElmValue.IsOkOrNull() is not { } processIdElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + processIdAsElmValue);
        }

        if (processIdElmValue is not ElmValue.ElmString processIdString)
        {
            return "Unexpected type in processId: " + processIdElmValue;
        }

        var stdInBase64Value = record.FirstOrDefault(field => field.fieldName is "stdInBase64");

        if (stdInBase64Value.fieldValue is null)
        {
            return "Missing field: stdInBase64";
        }

        var stdInBase64AsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(stdInBase64Value.fieldValue);
        {
            if (stdInBase64AsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode stdInBase64: " + err;
            }
        }

        if (stdInBase64AsElmValue.IsOkOrNull() is not { } stdInBase64ElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + stdInBase64AsElmValue);
        }

        if (stdInBase64ElmValue is not ElmValue.ElmString stdInBase64String)
        {
            return "Unexpected type in stdInBase64: " + stdInBase64ElmValue;
        }

        var updateValue = record.FirstOrDefault(field => field.fieldName is "update");

        if (updateValue.fieldValue is null)
        {
            return "Missing field: update";
        }

        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(updateValue.fieldValue, parseCache);
        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse update: " + err;
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        return new WriteToVolatileProcessNativeStdInStruct(
            processIdString.Value,
            stdInBase64String.Value,
            update);
    }

    public static Result<string, ReadAllFromVolatileProcessNativeStruct> ParseReadAllFromVolatileProcessNativeStruct(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache,
        PineVMParseCache parseCache)
    {
        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(pineValue);

        {
            if (asRecordResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as record: " + err;
            }
        }

        if (asRecordResult.IsOkOrNull() is not { } record)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asRecordResult);
        }

        if (record.Count is not 2)
        {
            return "Unexpected number of fields: " + record.Count;
        }

        var processIdValue = record.FirstOrDefault(field => field.fieldName is "processId");

        if (processIdValue.fieldValue is null)
        {
            return "Missing field: processId";
        }

        var processIdAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(processIdValue.fieldValue);
        {
            if (processIdAsElmValue.IsErrOrNull() is { } err)
            {
                return "Failed to decode processId: " + err;
            }
        }

        if (processIdAsElmValue.IsOkOrNull() is not { } processIdElmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + processIdAsElmValue);
        }

        if (processIdElmValue is not ElmValue.ElmString processIdString)
        {
            return "Unexpected type in processId: " + processIdElmValue;
        }

        var updateValue = record.FirstOrDefault(field => field.fieldName is "update");

        if (updateValue.fieldValue is null)
        {
            return "Missing field: update";
        }

        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(updateValue.fieldValue, parseCache);
        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse update: " + err;
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        return new ReadAllFromVolatileProcessNativeStruct(
            processIdString.Value,
            update);
    }

    public static Result<string, Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete>>
        ParseRequestToVolatileProcessResult(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
    {
        return
            ElmValueInterop.ParseElmResultValue(
                pineValue,
                err:
                errValue =>
                {
                    var parsedErr = ParseRequestToVolatileProcessError(errValue, elmCompilerCache);

                    {
                        if (parsedErr.IsErrOrNull() is { } err)
                        {
                            return "Failed to parse error: " + err;
                        }
                    }

                    {
                        if (parsedErr.IsOkOrNull() is not { } err)
                        {
                            throw new NotImplementedException(
                                "Unexpected return type: " + parsedErr);
                        }

                        return Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete>.err(err);
                    }
                },

                ok:
                okValue =>
                {
                    var parsedOk = ParseRequestToVolatileProcessComplete(okValue, elmCompilerCache);
                    {
                        if (parsedOk.IsErrOrNull() is { } err)
                        {
                            return "Failed to parse ok: " + err;
                        }
                    }
                    if (parsedOk.IsOkOrNull() is not { } ok)
                    {
                        throw new NotImplementedException(
                            "Unexpected return type: " + parsedOk);
                    }
                    return Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete>.ok(ok);
                },

                invalid:
                err =>
                Result<string, Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete>>.err(
                    "Invalid: " + err));
    }

    public static Result<string, RequestToVolatileProcessError> ParseRequestToVolatileProcessError(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache)
    {
        var asElmValueResult = elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

        {
            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode Elm value: " + err;
            }
        }

        if (asElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asElmValueResult);
        }

        if (elmValue is not ElmValue.ElmTag elmTagValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmTagValue.TagName is "ProcessNotFound")
        {
            return RequestToVolatileProcessError.ProcessNotFoundInstance;
        }

        return "Unexpected Elm tag: " + elmTagValue.TagName;
    }

    public static Result<string, RequestToVolatileProcessComplete> ParseRequestToVolatileProcessComplete(
        PineValue pineValue,
        ElmCompilerCache elmCompilerCache)
    {
        var asElmValueResult = elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

        {
            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode Elm value: " + err;
            }
        }

        if (asElmValueResult.IsOkOrNull() is not { } elmValue)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + asElmValueResult);
        }

        if (elmValue is not ElmValue.ElmRecord elmRecordValue)
        {
            return "Unexpected Elm value: " + elmValue;
        }

        if (elmRecordValue.Fields.Count is not 3)
        {
            return "Unexpected number of fields: " + elmRecordValue.Fields.Count;
        }

        var maybeExceptionToStringValue = elmRecordValue["exceptionToString"];

        if (maybeExceptionToStringValue is null)
        {
            return "Missing field: exceptionToString";
        }

        if (maybeExceptionToStringValue is not ElmValue.ElmTag maybeExceptionToStringTag)
        {
            return "Unexpected type in exceptionToString: " + maybeExceptionToStringValue;
        }

        string? exceptionToString = null;

        if (maybeExceptionToStringTag.TagName is "Just")
        {
            if (maybeExceptionToStringTag.Arguments.Count is not 1)
            {
                return "Unexpected number of arguments in Just: " + maybeExceptionToStringTag.Arguments.Count;
            }

            if (maybeExceptionToStringTag.Arguments[0] is not ElmValue.ElmString elmStringValue)
            {
                return "Unexpected argument type in Just: " + maybeExceptionToStringTag.Arguments[0];
            }

            exceptionToString = elmStringValue.Value;
        }
        else if (maybeExceptionToStringTag.TagName is not "Nothing")
        {
            return "Unexpected tag in exceptionToString: " + maybeExceptionToStringTag.TagName;
        }

        var maybeReturnValueToStringValue = elmRecordValue["returnValueToString"];

        if (maybeReturnValueToStringValue is null)
        {
            return "Missing field: returnValueToString";
        }

        if (maybeReturnValueToStringValue is not ElmValue.ElmTag maybeReturnValueToStringTag)
        {
            return "Unexpected type in returnValueToString: " + maybeReturnValueToStringValue;
        }

        string? returnValueToString = null;

        if (maybeReturnValueToStringTag.TagName is "Just")
        {
            if (maybeReturnValueToStringTag.Arguments.Count is not 1)
            {
                return "Unexpected number of arguments in Just: " + maybeReturnValueToStringTag.Arguments.Count;
            }
            if (maybeReturnValueToStringTag.Arguments[0] is not ElmValue.ElmString elmStringValue)
            {
                return "Unexpected argument type in Just: " + maybeReturnValueToStringTag.Arguments[0];
            }

            returnValueToString = elmStringValue.Value;
        }
        else if (maybeReturnValueToStringTag.TagName is not "Nothing")
        {
            return "Unexpected tag in returnValueToString: " + maybeReturnValueToStringTag.TagName;
        }

        var durationInMillisecondsValue = elmRecordValue["durationInMilliseconds"];

        if (durationInMillisecondsValue is null)
        {
            return "Missing field: durationInMilliseconds";
        }

        if (durationInMillisecondsValue is not ElmValue.ElmInteger durationInMillisecondsInt)
        {
            return "Unexpected type in durationInMilliseconds: " + durationInMillisecondsValue;
        }

        var durationInMilliseconds = durationInMillisecondsInt.Value;

        return new RequestToVolatileProcessComplete(
            exceptionToString,
            returnValueToString,
            (int)durationInMilliseconds);
    }

    private static readonly ElmCompilerCache elmCompilerCache = new();
    private static readonly PineVMParseCache parseCache = new();

    public static WebServiceConfig ConfigFromSourceFilesAndEntryFileName(
        TreeNodeWithStringPath sourceFiles,
        IReadOnlyList<string> entryFileName)
    {
        var loweringResult =
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                PineValueComposition.TreeToFlatDictionaryWithPathComparer(sourceFiles),
                workingDirectoryRelative: [],
                ElmAppInterfaceConfig.Default
                with
                {
                    compilationRootFilePath = entryFileName
                });

        if (loweringResult.IsErrOrNull() is { } loweringErr)
        {
            throw new Exception(
                "Failed lowering with " + loweringErr.Count + " errors:\n" +
                ElmAppCompilation.CompileCompilationErrorsDisplayText(loweringErr));
        }

        if (loweringResult.IsOkOrNull() is not { } loweringOk)
        {
            throw new Exception("Unexpected result type: " + loweringResult);
        }

        var loweredTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(loweringOk.result.compiledFiles);

        var loweredTreeCleaned =
            ElmTimeJsonAdapter.CleanUpFromLoweredForJavaScript(loweredTree);

        var compilationUnitsPrepared =
            ElmAppDependencyResolution.AppCompilationUnitsForEntryPoint(
                loweredTreeCleaned,
                ["src", "Backend", "InterfaceToHost_Root.elm"]);

        PineValue build()
        {
            using var interactiveSession =
                new InteractiveSessionPine(
                    ElmCompiler.CompilerSourceContainerFilesDefault.Value,
                    appCodeTree: compilationUnitsPrepared.files,
                    overrideSkipLowering: true,
                    entryPointsFilePaths: null,
                    caching: true,
                    autoPGO: null);

            return interactiveSession.CurrentEnvironmentValue();
        }

        var compiledModulesValue = build();

        var (declValue, _) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledModulesValue,
                moduleName: "Backend.Main",
                declarationName: "webServiceMain",
                parseCache)
            .Extract(err => throw new Exception(
                $"Failed parsing webServiceMain declaration from module {string.Join(".", compilationUnitsPrepared.entryModuleName)}: {err}"));

        var parseJsonAdapterResult =
            ElmTimeJsonAdapter.Parsed.ParseFromCompiled(
                compiledModulesValue,
                parseCache);

        var parsedJsonAdapter =
            parseJsonAdapterResult
            .Extract(err => throw new Exception("Failed parsing JsonAdapter: " + err));

        return ConfigFromDeclarationValue(declValue, parsedJsonAdapter);
    }

    public static WebServiceConfig ConfigFromDeclarationValue(
        PineValue webServiceMainDeclValue,
        ElmTimeJsonAdapter.Parsed jsonAdapter)
    {
        var webServiceMainRecordResult =
            ElmValueEncoding.ParsePineValueAsRecordTagged(webServiceMainDeclValue);

        if (webServiceMainRecordResult.IsErrOrNull() is { } recordErr)
            throw new Exception($"Failed to parse webServiceMain record: {recordErr}");

        if (webServiceMainRecordResult.IsOkOrNull() is not { } webServiceMainRecord)
        {
            throw new Exception("Unexpected return type: " + webServiceMainRecordResult);
        }

        var initField =
            webServiceMainRecord.FirstOrDefault(f => f.fieldName is "init").fieldValue;

        if (initField is null)
        {
            throw new Exception("Missing init field in webServiceMain declaration");
        }

        var initResult =
            ParseWebServiceConfigInit(initField, elmCompilerCache, parseCache)
            .Extract(err => throw new Exception("Failed parsing init function: " + err));

        var subscriptionsField =
            webServiceMainRecord.FirstOrDefault(f => f.fieldName is "subscriptions").fieldValue;

        if (subscriptionsField is null)
        {
            throw new Exception("Missing subscriptions field in webServiceMain declaration");
        }

        var subscriptionsFunctionRecord =
            ElmInteractiveEnvironment
            .ParseFunctionRecordFromValueTagged(subscriptionsField, parseCache)
            .Extract(err => throw new Exception("Failed parsing subscriptions function: " + err));

        return new WebServiceConfig(
            Init: initResult,
            Subscriptions: subscriptionsFunctionRecord,
            jsonAdapter);
    }
}
