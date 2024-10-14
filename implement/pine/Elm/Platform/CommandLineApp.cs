using Pine.Core;
using Pine.ElmInteractive;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Elm.Platform;

/// <summary>
/// A mutating container for an Elm command-line interface app.
/// </summary>
public class MutatingCommandLineApp
{
    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> stdOut = new();

    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> stdErr = new();

    private readonly PineVM.PineVMCache pineVMCache = new();

    private readonly PineVM.PineVM pineVM;

    private readonly CommandLineAppConfig appConfig;

    private PineValue appState;

    public IReadOnlyList<ReadOnlyMemory<byte>> DequeueStdOut() =>
        [.. stdOut.DequeueAllEnumerable()];

    public IReadOnlyList<ReadOnlyMemory<byte>> DequeueStdErr() =>
        [.. stdErr.DequeueAllEnumerable()];

    public MutatingCommandLineApp(
        CommandLineAppConfig appConfig,
        CommandLineAppConfig.CommandLineAppInitEnvironment environment)
    {
        this.appConfig = appConfig;

        pineVM = new PineVM.PineVM(pineVMCache.EvalCache);

        (appState, var initResponse) =
            CommandLineAppConfig.Init(
                appConfig,
                environment,
                pineVM);

        MutateConsolidatingAppResponse(initResponse);
    }

    public CommandLineAppConfig.CommandLineAppEventResponse? EventStdIn(ReadOnlyMemory<byte> input)
    {
        var eventHandled =
            CommandLineAppConfig.EventStdIn(
                appConfig,
                input,
                appState,
                pineVM);

        if (eventHandled is null)
        {
            return null;
        }

        (appState, var eventResponse) = eventHandled.Value;

        MutateConsolidatingAppResponse(eventResponse);

        return eventResponse;
    }

    private void MutateConsolidatingAppResponse(CommandLineAppConfig.CommandLineAppEventResponse eventResponse)
    {

        foreach (var cmd in eventResponse.Commands)
        {
            if (cmd is CommandLineAppConfig.CommandLineAppCommand.SendToStdOutCmd sendToStdOutCmd)
            {
                stdOut.Enqueue(sendToStdOutCmd.Output);

                continue;
            }

            if (cmd is CommandLineAppConfig.CommandLineAppCommand.SendToStdErrCmd sendToStdErrCmd)
            {
                stdErr.Enqueue(sendToStdErrCmd.Output);

                continue;
            }

            throw new NotImplementedException(
                "Unknown command type: " + cmd.GetType().Name);
        }
    }
}

/// <summary>
/// Configuration of an Elm command-line interface (CLI) app.
/// </summary>
public record CommandLineAppConfig(
    ElmTime.ElmInteractive.ElmInteractiveEnvironment.FunctionRecord InitFromEnvironment,
    ElmTime.ElmInteractive.ElmInteractiveEnvironment.FunctionRecord SubscriptionsFromState)
{
    private static readonly PineVM.PineVMParseCache parseCache = new();

    public static CommandLineAppConfig ConfigFromSourceFilesAndModuleName(
        TreeNodeWithStringPath sourceFiles,
        IReadOnlyList<string> moduleName)
    {
        using var interactiveSession =
            new ElmTime.ElmInteractive.InteractiveSessionPine(
                ElmCompiler.CompilerSourceContainerFilesDefault.Value,
                appCodeTree: sourceFiles,
                caching: true,
                autoPGO: null);

        var compiledModulesValue =
            interactiveSession.CurrentEnvironmentValue();

        var (declValue, functionRecord) =
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledModulesValue,
                moduleName: string.Join(".", moduleName),
                declarationName: "runRoot",
                parseCache)
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

        if (initList.Elements.Count is not 2)
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
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                runRootInitFunctionValue,
                parseCache)
            .Extract(err => throw new Exception("Failed parsing init function: " + err));

        var subscriptionsFunctionRecord =
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                runRootSubscriptionsFunctionValue,
                parseCache)
            .Extract(err => throw new Exception("Failed parsing subscriptions function: " + err));

        return new CommandLineAppConfig(
            InitFromEnvironment: initFunctionRecord,
            SubscriptionsFromState: subscriptionsFunctionRecord);
    }

    public static (PineValue, CommandLineAppEventResponse) Init(
        CommandLineAppConfig config,
        CommandLineAppInitEnvironment environment,
        PineVM.IPineVM pineVM)
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
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                config.InitFromEnvironment,
                [initRecordValue])
            .Extract(err => throw new Exception("Failed applying function init: " + err));

        if (initFunctionReturnValue is not PineValue.ListValue initList)
        {
            throw new Exception(
                "Unexpected return type from init function: " + initFunctionReturnValue.ToString());
        }

        if (initList.Elements.Count is not 2)
        {
            throw new Exception(
                "Unexpected number of elements in init function return value: " + initList.Elements.Count);
        }

        var responseRecord =
            ParseEventResponse(initList.Elements[1]);

        return (initList.Elements[0], responseRecord);
    }

    public static (PineValue, CommandLineAppEventResponse)?
        EventStdIn(
        CommandLineAppConfig config,
        ReadOnlyMemory<byte> input,
        PineValue stateBefore,
        PineVM.IPineVM pineVM)
    {
        var subscriptionsValue =
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ApplyFunction(
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
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                functionRecordValue,
                parseCache)
            .Extract(err => throw new Exception("Failed parsing stdIn function record: " + err));

        if (functionRecord.functionParameterCount is not 2)
        {
            throw new Exception("Expected stdIn function to have two parameters.");
        }

        var inputEncoded =
            ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmBytes(input));

        var responseValue =
            ElmTime.ElmInteractive.ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                functionRecord,
                [inputEncoded, stateBefore])
            .Extract(err => throw new Exception("Failed applying function stdIn: " + err));

        if (responseValue is not PineValue.ListValue responseList)
        {
            throw new Exception("Expected response from stdIn to be a list.");
        }

        if (responseList.Elements.Count is not 2)
        {
            throw new Exception("Expected response from stdIn to have two elements.");
        }

        var responseRecord =
            ParseEventResponse(responseList.Elements[1]);

        return (responseList.Elements[0], responseRecord);
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

        var commands =
            commandsListValue
            .Elements
            .Select(ParseCommand)
            .ToImmutableArray();

        var maybeExitCode =
            ElmValueInterop.ParseElmMaybeValue<int?>(
                maybeExitValue,
                nothing: () => null,
                just:
                exitCodeValue =>
                (int)
                PineValueAsInteger.SignedIntegerFromValueRelaxed(exitCodeValue)
                .Extract(err => throw new Exception("Failed parsing exit code as integer")),
                invalid:
                err => throw new Exception("Failed parsing exit code as maybe: " + err));

        return new CommandLineAppEventResponse(Commands: commands, maybeExitCode);
    }

    public static CommandLineAppCommand ParseCommand(PineValue commandValue)
    {
        var commandElmValue =
            ElmValueEncoding.PineValueAsElmValue(commandValue)
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
