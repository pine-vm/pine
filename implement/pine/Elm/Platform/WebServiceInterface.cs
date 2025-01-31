using ElmTime.ElmInteractive;
using ElmTime.ElmSyntax;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.ElmInteractive;
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
        ElmInteractiveEnvironment.FunctionRecord Subscriptions)
    {
        public static WebServiceEventResponse
            EventHttpRequest(
            WebServiceConfig config,
            HttpRequestEventStruct httpRequest,
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

            var functionRecord =
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    httpRequestFieldValue,
                    parseCache)
                .Extract(err => throw new Exception("Failed parsing httpRequest function record: " + err));

            if (functionRecord.functionParameterCount is not 2)
            {
                throw new Exception("Expected httpRequest function to have two parameters.");
            }

            var inputEncoded = EncodeHttpRequest(httpRequest);

            var responseValue =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    functionRecord,
                    [inputEncoded, stateBefore])
                .Extract(err => throw new Exception("Failed applying function httpRequest: " + err));

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
        public sealed record RespondToHttpRequest(
            RespondToHttpRequestStruct Respond)
            : Command;

        public sealed record CreateVolatileProcess(
            CreateVolatileProcessStruct Create)
            : Command;

        public sealed record RequestToVolatileProcess(
            RequestToVolatileProcessStruct Request)
            : Command;

        public sealed record TerminateVolatileProcess(
            string ProcessId)
            : Command;
    }

    public record Subscriptions(
        ElmInteractiveEnvironment.FunctionRecord HttpRequest,
        Maybe<PosixTimeIsPastSubscription> PosixTimeIsPast);

    public record HttpRequestEventStruct(
        string HttpRequestId,
        int PosixTimeMilli,
        HttpRequestContext RequestContext,
        HttpRequestProperties Request);

    public record PosixTimeIsPastSubscription(
        int MinimumPosixTimeMilli,
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

    public record CreateVolatileProcessErrorStruct(
        string ExceptionToString);

    public record CreateVolatileProcessComplete(
        string ProcessId);

    public abstract record RequestToVolatileProcessError
    {
        public sealed record ProcessNotFound
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
                ("httpRequestId", new ElmValue.ElmString(httpRequest.HttpRequestId)),
                ("posixTimeMilli", new ElmValue.ElmInteger(httpRequest.PosixTimeMilli)),
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
                    : ElmValue.TagInstance("Just", [new ElmValue.ElmString(requestContext.ClientAddress)]))
                ]);

        return asElmValue;
    }

    public static ElmValue EncodeHttpRequestProperties(HttpRequestProperties request)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("method", new ElmValue.ElmString(request.Method)),
                ("uri", new ElmValue.ElmString(request.Uri)),
                ("bodyAsBase64", request.BodyAsBase64 is null
                    ? ElmValue.TagInstance("Nothing", [])
                    : ElmValue.TagInstance("Just", [new ElmValue.ElmString(request.BodyAsBase64)])),
                ("headers", new ElmValue.ElmList(request.Headers.Select(EncodeHttpHeader).ToArray()))
                ]);
        return asElmValue;
    }

    public static ElmValue EncodeHttpHeader(HttpHeader header)
    {
        var asElmValue =
            new ElmValue.ElmRecord(
                [
                ("name", new ElmValue.ElmString(header.Name)),
                ("values", new ElmValue.ElmList(header.Values.Select(value => new ElmValue.ElmString(value)).ToArray()))
                ]);
        return asElmValue;
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
        var asRecordResult = ElmValueEncoding.ParsePineValueAsRecord(pineValue);

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
        var compilationUnitsPrepared =
            ElmAppDependencyResolution.AppCompilationUnitsForEntryPoint(
                sourceFiles,
                entryFileName);

        using var interactiveSession =
            new InteractiveSessionPine(
                ElmCompiler.CompilerSourceContainerFilesDefault.Value,
                appCodeTree: compilationUnitsPrepared.files,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        var compiledModulesValue = interactiveSession.CurrentEnvironmentValue();

        var (declValue, _) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledModulesValue,
                moduleName: string.Join(".", compilationUnitsPrepared.entryModuleName),
                declarationName: "webServiceMain",
                parseCache)
            .Extract(err => throw new Exception(
                $"Failed parsing webServiceMain declaration from module {string.Join(".", compilationUnitsPrepared.entryModuleName)}: {err}"));

        return ConfigFromDeclarationValue(declValue);
    }

    public static WebServiceConfig ConfigFromDeclarationValue(
        PineValue webServiceMainDeclValue)
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
            Subscriptions: subscriptionsFunctionRecord);
    }
}
