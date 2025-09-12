using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

using ElmInteractiveEnvironment = Pine.Core.CodeAnalysis.ElmInteractiveEnvironment;

namespace Pine.Elm.Platform;

/// <summary>
/// Mutating container for an Elm command-line interface app.
/// </summary>
public class MutatingCommandLineApp
{
    public int? ExitCode { private set; get; } = null;

    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> _stdOut = new();

    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> _stdErr = new();

    private readonly PineVM.PineVMCache _pineVMCache = new();

    private readonly PineVM.PineVM _pineVM;

    private readonly CommandLineAppConfig _appConfig;

    private PineValue _appState;

    public IReadOnlyList<ReadOnlyMemory<byte>> DequeueStdOut() =>
        [.. _stdOut.DequeueAllEnumerable()];

    public IReadOnlyList<ReadOnlyMemory<byte>> DequeueStdErr() =>
        [.. _stdErr.DequeueAllEnumerable()];

    public MutatingCommandLineApp(
        CommandLineAppConfig appConfig,
        CommandLineAppConfig.CommandLineAppInitEnvironment environment)
    {
        _appConfig = appConfig;

        _pineVM = new PineVM.PineVM(_pineVMCache.EvalCache);

        (_appState, var initResponse) =
            CommandLineAppConfig.Init(
                appConfig,
                environment,
                _pineVM);

        MutateConsolidatingAppResponse(initResponse);
    }

    public CommandLineAppConfig.CommandLineAppEventResponse? EventStdIn(ReadOnlyMemory<byte> input)
    {
        var eventHandled =
            CommandLineAppConfig.EventStdIn(
                _appConfig,
                input,
                _appState,
                _pineVM);

        if (eventHandled is null)
        {
            return null;
        }

        (_appState, var eventResponse) = eventHandled.Value;

        MutateConsolidatingAppResponse(eventResponse);

        return eventResponse;
    }

    private void MutateConsolidatingAppResponse(CommandLineAppConfig.CommandLineAppEventResponse eventResponse)
    {
        ExitCode ??= eventResponse.Exit;

        foreach (var cmd in eventResponse.Commands)
        {
            if (cmd is CommandLineAppConfig.CommandLineAppCommand.SendToStdOutCmd sendToStdOutCmd)
            {
                _stdOut.Enqueue(sendToStdOutCmd.Output);

                continue;
            }

            if (cmd is CommandLineAppConfig.CommandLineAppCommand.SendToStdErrCmd sendToStdErrCmd)
            {
                _stdErr.Enqueue(sendToStdErrCmd.Output);

                continue;
            }

            throw new NotImplementedException(
                "Unknown command type: " + cmd.GetType().Name);
        }
    }
}

/*

{-| Use the type `CommandLineAppConfig` on a declaration named `runRoot` to declare a command-line program in an Elm module.
A command-line program can subscribe to incoming bytes from standard input and can write to standard output and standard error.
-}
type alias CommandLineAppConfig state =
    { init : InitEnvironment -> ( state, EventResponse state )
    , subscriptions : state -> Subscriptions state
    }


type alias InitEnvironment =
    { commandLine : String
    , environmentVariables : List ( String, String )
    }


type alias Subscriptions state =
    { stdIn : Maybe (Bytes.Bytes -> state -> ( state, EventResponse state ))
    , posixTimeIsPast :
        Maybe
            { minimumPosixTimeMilli : Int
            , update : { currentPosixTimeMilli : Int } -> state -> ( state, EventResponse state )
            }
    }


type alias EventResponse state =
    { commands : List (Command state)
    , exit : Maybe Int
    }


type Command state
    = SendToStdOutCmd Bytes.Bytes
    | SendToStdErrCmd Bytes.Bytes

 * */

/// <summary>
/// Configuration of an Elm command-line interface (CLI) app.
/// </summary>
public record CommandLineAppConfig(
    ElmInteractiveEnvironment.FunctionRecord InitFromEnvironment,
    ElmInteractiveEnvironment.FunctionRecord SubscriptionsFromState)
{
    private static readonly Core.CodeAnalysis.PineVMParseCache s_parseCache = new();

    public static CommandLineAppConfig ConfigFromSourceFilesAndModuleName(
        BlobTreeWithStringPath sourceFiles,
        IReadOnlyList<string> moduleName)
    {
        using var interactiveSession =
            new ElmTime.ElmInteractive.InteractiveSessionPine(
                ElmCompiler.CompilerSourceContainerFilesDefault.Value,
                appCodeTree: sourceFiles,
                // TODO: Migrate lowering implementation for portability.
                overrideSkipLowering: true,
                // TODO: Forward entry point.
                entryPointsFilePaths: null,
                caching: true,
                autoPGO: null);

        var compiledModulesValue =
            interactiveSession.CurrentEnvironmentValue();

        var (declValue, functionRecord) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledModulesValue,
                moduleName: string.Join(".", moduleName),
                declarationName: "runRoot",
                s_parseCache)
            .Extract(err => throw new Exception(
                "Failed parsing runRoot declaration from module " + string.Join(".", moduleName) + ": " + err));

        return ConfigFromDeclarationValue(declValue);
    }

    public static CommandLineAppConfig ConfigFromDeclarationValue(PineValue runRootDeclValue)
    {
        var runRootRecord =
            ElmValueEncoding.ParsePineValueAsRecordTagged(runRootDeclValue)
            .Extract(err => throw new Exception("Failed parsing runRoot record: " + err));

        var initValue =
            runRootRecord
            .First(field => field.fieldName is "init").fieldValue;

        if (initValue is not PineValue.ListValue initList)
        {
            throw new Exception("Expected init to be a list.");
        }

        if (initList.Items.Length is not 2)
        {
            throw new Exception("Expected init list to have two elements.");
        }

        var runRootInitFunctionValue =
            runRootRecord
            .First(field => field.fieldName is "init").fieldValue;

        var runRootSubscriptionsFunctionValue =
            runRootRecord
            .First(field => field.fieldName is "subscriptions").fieldValue;

        var initFunctionRecord =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                runRootInitFunctionValue,
                s_parseCache)
            .Extract(err => throw new Exception("Failed parsing init function: " + err));

        var subscriptionsFunctionRecord =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                runRootSubscriptionsFunctionValue,
                s_parseCache)
            .Extract(err => throw new Exception("Failed parsing subscriptions function: " + err));

        return new CommandLineAppConfig(
            InitFromEnvironment: initFunctionRecord,
            SubscriptionsFromState: subscriptionsFunctionRecord);
    }

    public static (PineValue, CommandLineAppEventResponse) Init(
        CommandLineAppConfig config,
        CommandLineAppInitEnvironment environment,
        Core.PineVM.IPineVM pineVM)
    {
        var initRecord =
            new ElmValue.ElmRecord(
                [
                ("commandLine",
                ElmValue.StringInstance(environment.CommandLine)),
                ("environmentVariables",
                ElmValue.ListInstance(
                    [..environment.EnvironmentVariables
                    .Select(kv => ElmValue.TupleInstance(
                        ElmValue.StringInstance(kv.Key),
                        ElmValue.StringInstance(kv.Value)))]))
                ]);

        var initRecordValue =
            ElmValueEncoding.ElmValueAsPineValue(initRecord);

        var initFunctionReturnValue =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                config.InitFromEnvironment,
                [initRecordValue])
            .Extract(err => throw new Exception("Failed applying function init: " + err));

        if (initFunctionReturnValue is not PineValue.ListValue initList)
        {
            throw new Exception(
                "Unexpected return type from init function: " + initFunctionReturnValue.ToString());
        }

        if (initList.Items.Length is not 2)
        {
            throw new Exception(
                "Unexpected number of elements in init function return value: " + initList.Items.Length);
        }

        var responseRecord =
            ParseEventResponse(initList.Items.Span[1]);

        return (initList.Items.Span[0], responseRecord);
    }

    public static (PineValue, CommandLineAppEventResponse)?
        EventStdIn(
        CommandLineAppConfig config,
        ReadOnlyMemory<byte> input,
        PineValue stateBefore,
        Core.PineVM.IPineVM pineVM)
    {
        var subscriptionsValue =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                config.SubscriptionsFromState,
                [stateBefore])
            .Extract(err => throw new Exception("Failed applying function subscriptions: " + err));

        var subscriptionsElmRecord =
            ElmValueEncoding.ParsePineValueAsRecordTagged(subscriptionsValue)
            .Extract(err => throw new Exception("Failed parsing subscriptions value as Elm record: " + err));

        var stdInFieldValue =
            subscriptionsElmRecord
            .First(field => field.fieldName is "stdIn").fieldValue;

        var functionRecordValue =
            ElmValueInterop.ParseElmMaybeValue<PineValue?>(
                stdInFieldValue,
                nothing: () => null,
                just: recordVal => recordVal,
                invalid: err => throw new Exception("Failed parsing stdIn field value as maybe: " + err));

        if (functionRecordValue is null)
        {
            return null;
        }

        var functionRecord =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                functionRecordValue,
                s_parseCache)
            .Extract(err => throw new Exception("Failed parsing stdIn function record: " + err));

        if (functionRecord.ParameterCount is not 2)
        {
            throw new Exception("Expected stdIn function to have two parameters.");
        }

        var inputEncoded =
            ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmBytes(input));

        var responseValue =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                functionRecord,
                [inputEncoded, stateBefore])
            .Extract(err => throw new Exception("Failed applying function stdIn: " + err));

        if (responseValue is not PineValue.ListValue responseList)
        {
            throw new Exception("Expected response from stdIn to be a list.");
        }

        if (responseList.Items.Length is not 2)
        {
            throw new Exception("Expected response from stdIn to have two elements.");
        }

        var responseRecord =
            ParseEventResponse(responseList.Items.Span[1]);

        return (responseList.Items.Span[0], responseRecord);
    }

    public static CommandLineAppEventResponse ParseEventResponse(PineValue responseValue)
    {
        if (ElmValueEncoding.ParsePineValueAsRecordTagged(responseValue)
            is not Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Ok responseRecord)
        {
            throw new Exception("Failed parsing event response as record.");
        }

        var commandsValue =
            responseRecord.Value
            .First(field => field.fieldName is "commands").fieldValue;

        var maybeExitValue =
            responseRecord.Value
            .First(field => field.fieldName is "exit").fieldValue;

        if (commandsValue is not PineValue.ListValue commandsListValue)
        {
            throw new Exception("Expected commands to be a list value.");
        }

        var commands = new CommandLineAppCommand[commandsListValue.Items.Length];

        for (var i = 0; i < commands.Length; ++i)
        {
            commands[i] = ParseCommand(commandsListValue.Items.Span[i]);
        }

        var maybeExitCode =
            ElmValueInterop.ParseElmMaybeValue<int?>(
                maybeExitValue,
                nothing: () => null,
                just:
                exitCodeValue =>
                (int)
                IntegerEncoding.ParseSignedIntegerRelaxed(exitCodeValue)
                .Extract(err => throw new Exception("Failed parsing exit code as integer")),
                invalid:
                err => throw new Exception("Failed parsing exit code as maybe: " + err));

        return new CommandLineAppEventResponse(Commands: commands, maybeExitCode);
    }

    public static CommandLineAppCommand ParseCommand(PineValue commandValue)
    {
        var commandElmValue =
            ElmValueEncoding.PineValueAsElmValue(commandValue, null, null)
            .Extract(err => throw new Exception("Failed parsing command value as Elm value: " + err));

        if (commandElmValue is not ElmValue.ElmTag commandTag)
        {
            throw new Exception("Expected command value to be a tag.");
        }

        if (commandTag.TagName is "SendToStdOutCmd")
        {
            if (commandTag.Arguments.Count is not 1)
            {
                throw new Exception("Expected SendToStdOutCmd to have one argument.");
            }

            if (commandTag.Arguments[0] is not ElmValue.ElmBytes outputBytes)
            {
                throw new Exception("Expected SendToStdOutCmd argument to be bytes.");
            }

            return new CommandLineAppCommand.SendToStdOutCmd(outputBytes.Value);
        }

        if (commandTag.TagName is "SendToStdErrCmd")
        {
            if (commandTag.Arguments.Count is not 1)
            {
                throw new Exception("Expected SendToStdErrCmd to have one argument.");
            }

            if (commandTag.Arguments[0] is not ElmValue.ElmBytes outputBytes)
            {
                throw new Exception("Expected SendToStdErrCmd argument to be bytes.");
            }

            return new CommandLineAppCommand.SendToStdErrCmd(outputBytes.Value);
        }

        throw new NotImplementedException(
            "Unknown command tag: " + commandTag.TagName);
    }

    public record CommandLineAppInitEnvironment(
        string CommandLine,
        IReadOnlyList<KeyValuePair<string, string>> EnvironmentVariables);

    public record CommandLineAppEventResponse(
        IReadOnlyList<CommandLineAppCommand> Commands,
        int? Exit);

    public abstract record CommandLineAppCommand
    {
        public record SendToStdOutCmd(ReadOnlyMemory<byte> Output)
            : CommandLineAppCommand;

        public record SendToStdErrCmd(ReadOnlyMemory<byte> Output)
            : CommandLineAppCommand;
    }
}
