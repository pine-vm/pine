using ElmTime.JavaScript;
using ElmTime.ProcessStore;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace ElmTime.PersistentProcess;

public interface IPersistentProcess
{
    (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash))
        ProcessEvents(IReadOnlyList<string> serializedEvents);

    (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash) SetState(string state);

    ReductionRecord ReductionRecordForCurrentState();
}

//  A provisional special case for a process from an elm app. Migrations give an example of why the elm code should be modeled on the history as well.
public class PersistentProcessWithHistoryOnFileFromElm019Code : IPersistentProcess, IDisposable
{
    private static readonly JsonSerializerOptions recordSerializationSettings = ProcessStoreInFileStore.RecordSerializationSettings;

    private byte[] lastStateHash = CompositionRecordInFile.HashFromSerialRepresentation([]);

    private readonly IDisposableProcessWithStringInterface process;

    public PersistentProcessWithHistoryOnFileFromElm019Code(
        IProcessStoreReader storeReader,
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmAppFiles,
        Action<string> logger,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var prepareProcessResult =
            ProcessFromElm019Code.ProcessFromElmCodeFiles(
                elmAppFiles,
                overrideElmAppInterfaceConfig,
                overrideJavaScriptEngineFactory: overrideJavaScriptEngineFactory);

        process = prepareProcessResult.startProcess();

        var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

        logger?.Invoke("Begin to restore the process state using the storeReader.");

        var emptyInitHash = CompositionRecordInFile.HashFromSerialRepresentation([]);

        static string dictKeyForHash(byte[] hash) => Convert.ToBase64String(hash);

        var compositionRecords = new Dictionary<string, (byte[] compositionRecordHash, CompositionRecord compositionRecord)>();

        var compositionChain = new Stack<(byte[] hash, CompositionRecord composition)>();

        foreach (var serializedCompositionRecord in storeReader.EnumerateSerializedCompositionsRecordsReverse())
        {
            {
                var compositionRecordFromFile = JsonSerializer.Deserialize<CompositionRecordInFile>(
                    Encoding.UTF8.GetString(serializedCompositionRecord))!;

                var compositionRecordHash = CompositionRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord);

                var compositionRecord =
                    new CompositionRecord
                    (
                        ParentHash:
                            CommonConversion.ByteArrayFromStringBase16(compositionRecordFromFile.ParentHashBase16),

                        SetStateLiteralString: compositionRecordFromFile.SetState?.LiteralString,

                        AppendedEventsLiteralString:
                            compositionRecordFromFile.AppendedEvents?.Select(@event => @event.LiteralString!)?.ToImmutableList()
                    );

                var compositionChainElement = (compositionRecordHash, compositionRecord);

                if (!compositionChain.Any())
                    compositionChain.Push(compositionChainElement);
                else
                    compositionRecords[dictKeyForHash(compositionRecordHash)] = compositionChainElement;
            }

            while (true)
            {
                var (compositionRecordHash, compositionRecord) = compositionChain.Peek();

                var reduction = storeReader.GetReduction(compositionRecordHash);

                if (reduction != null || emptyInitHash.SequenceEqual(compositionRecord.ParentHash))
                {
                    if (reduction != null)
                    {
                        compositionChain.Pop();
                        StateShim.StateShim.SetAppStateOnMainBranch(
                            process,
                            stateJson: JsonSerializer.Deserialize<JsonElement>(reduction.ReducedValueLiteralString)!)
                            .Extract(err => throw new Exception(err));
                        lastStateHash = reduction.ReducedCompositionHash;
                    }

                    foreach (var followingComposition in compositionChain)
                    {
                        if (followingComposition.composition.SetStateLiteralString != null)
                        {
                            StateShim.StateShim.SetAppStateOnMainBranch(
                                process,
                                stateJson: JsonSerializer.Deserialize<JsonElement>(followingComposition.composition.SetStateLiteralString!))
                                .Extract(err => throw new Exception(err));
                        }

                        foreach (var appendedEvent in followingComposition.composition.AppendedEventsLiteralString ?? [])
                            process.ProcessEvent(appendedEvent);

                        lastStateHash = followingComposition.hash;
                    }

                    logger?.Invoke("Restored the process state in " + (int)restoreStopwatch.Elapsed.TotalSeconds + " seconds.");
                    return;
                }

                var parentKey = dictKeyForHash(compositionRecord.ParentHash);

                if (!compositionRecords.TryGetValue(parentKey, out var compositionChainElementFromPool))
                    break;

                compositionChain.Push(compositionChainElementFromPool);
                compositionRecords.Remove(parentKey);
            }
        }

        if (compositionChain.Any())
            throw new NotImplementedException(
                "I did not find a reduction for any composition on the chain to the last composition (" +
                CommonConversion.StringBase16FromByteArray(compositionChain.Last().hash) +
                ").");

        logger?.Invoke("Found no composition record, default to initial state.");
    }

    private static string Serialize(CompositionRecordInFile composition) =>
        JsonSerializer.Serialize(composition, recordSerializationSettings);

    public (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash))
        ProcessEvents(IReadOnlyList<string> serializedEvents)
    {
        lock (process)
        {
            var responses =
                serializedEvents.Select(process.ProcessEvent)
                .ToImmutableList();

            var compositionRecord = new CompositionRecordInFile
            (
                ParentHashBase16: CommonConversion.StringBase16FromByteArray(lastStateHash),
                AppendedEvents: serializedEvents.Select(@event => new ValueInFile(LiteralString: @event)).ToImmutableList()
            );

            var serializedCompositionRecord =
                Encoding.UTF8.GetBytes(Serialize(compositionRecord));

            var compositionHash = CompositionRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord);

            lastStateHash = compositionHash;

            return (responses, (serializedCompositionRecord, compositionHash));
        }
    }

    public void Dispose() => process?.Dispose();

    public ReductionRecord ReductionRecordForCurrentState()
    {
        lock (process)
        {
            var appState =
                StateShim.StateShim.GetAppStateFromMainBranch(process)
                .Extract(err => throw new Exception("Failed to get app state: " + err));

            var appStateSerialized = appState.ToString();

            return
                new ReductionRecord
                (
                    ReducedCompositionHash: lastStateHash,
                    ReducedValueLiteralString: appStateSerialized
                );
        }
    }

    public (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash) SetState(string state)
    {
        lock (process)
        {
            StateShim.StateShim.SetAppStateOnMainBranch(process, stateJson: JsonSerializer.Deserialize<JsonElement>(state))
                .Extract(err => throw new Exception(err));

            var compositionRecord = new CompositionRecordInFile
            (
                ParentHashBase16: CommonConversion.StringBase16FromByteArray(lastStateHash),
                SetState: new ValueInFile(LiteralString: state)
            );

            var serializedCompositionRecord =
                Encoding.UTF8.GetBytes(Serialize(compositionRecord));

            var compositionHash = CompositionRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord);

            lastStateHash = compositionHash;

            return (serializedCompositionRecord, compositionHash);
        }
    }
}
