using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using ElmFullstack.ProcessStore;
using Newtonsoft.Json;
using Pine;

namespace ElmFullstack.PersistentProcess
{
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
        static readonly Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings =
            ProcessStore.ProcessStoreInFileStore.RecordSerializationSettings;

        byte[] lastStateHash;

        IDisposableProcessWithStringInterface process;

        public readonly string JavascriptFromElmMake;

        public readonly string JavascriptPreparedToRun;

        public PersistentProcessWithHistoryOnFileFromElm019Code(
            IProcessStoreReader storeReader,
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> elmAppFiles,
            Action<string> logger,
            ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null)
        {
            (process, (JavascriptFromElmMake, JavascriptPreparedToRun)) =
                ProcessFromElm019Code.ProcessFromElmCodeFiles(elmAppFiles, overrideElmAppInterfaceConfig);

            var restoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

            logger?.Invoke("Begin to restore the process state using the storeReader.");

            var emptyInitHash = CompositionRecordInFile.HashFromSerialRepresentation(new byte[0]);

            string dictKeyForHash(byte[] hash) => Convert.ToBase64String(hash);

            var compositionRecords = new Dictionary<string, (byte[] compositionRecordHash, CompositionRecord compositionRecord)>();

            var compositionChain = new Stack<(byte[] hash, CompositionRecord composition)>();

            foreach (var serializedCompositionRecord in storeReader.EnumerateSerializedCompositionsRecordsReverse())
            {
                {
                    var compositionRecordFromFile = JsonConvert.DeserializeObject<CompositionRecordInFile>(
                        System.Text.Encoding.UTF8.GetString(serializedCompositionRecord));

                    var compositionRecordHash = CompositionRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord);

                    var compositionRecord =
                        new CompositionRecord
                        {
                            ParentHash =
                                CommonConversion.ByteArrayFromStringBase16(compositionRecordFromFile.ParentHashBase16),

                            SetStateLiteralString = compositionRecordFromFile.SetState?.LiteralString,

                            AppendedEventsLiteralString =
                                compositionRecordFromFile.AppendedEvents?.Select(@event => @event.LiteralString)?.ToImmutableList(),
                        };

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
                            process.SetSerializedState(reduction.ReducedValueLiteralString);
                            lastStateHash = reduction.ReducedCompositionHash;
                        }

                        foreach (var followingComposition in compositionChain)
                        {
                            if (followingComposition.composition.SetStateLiteralString != null)
                                process.SetSerializedState(followingComposition.composition.SetStateLiteralString);

                            foreach (var appendedEvent in followingComposition.composition.AppendedEventsLiteralString.EmptyIfNull())
                                process.ProcessEvent(appendedEvent);

                            lastStateHash = followingComposition.hash;
                        }

                        logger?.Invoke("Restored the process state in " + ((int)restoreStopwatch.Elapsed.TotalSeconds) + " seconds.");
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

            lastStateHash = emptyInitHash;
        }

        static string Serialize(CompositionRecordInFile composition) =>
            JsonConvert.SerializeObject(composition, recordSerializationSettings);

        public (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash))
            ProcessEvents(IReadOnlyList<string> serializedEvents)
        {
            lock (process)
            {
                var responses =
                    serializedEvents.Select(serializedEvent => process.ProcessEvent(serializedEvent))
                    .ToImmutableList();

                var compositionRecord = new CompositionRecordInFile
                {
                    ParentHashBase16 = CommonConversion.StringBase16FromByteArray(lastStateHash),
                    AppendedEvents = serializedEvents.Select(@event => new ValueInFile { LiteralString = @event }).ToImmutableList(),
                };

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
                return
                    new ReductionRecord
                    {
                        ReducedCompositionHash = lastStateHash,
                        ReducedValueLiteralString = process.GetSerializedState(),
                    };
            }
        }

        public (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash) SetState(string state)
        {
            lock (process)
            {
                process.SetSerializedState(state);

                var compositionRecord = new CompositionRecordInFile
                {
                    ParentHashBase16 = CommonConversion.StringBase16FromByteArray(lastStateHash),
                    SetState = new ValueInFile { LiteralString = state },
                };

                var serializedCompositionRecord =
                    Encoding.UTF8.GetBytes(Serialize(compositionRecord));

                var compositionHash = CompositionRecordInFile.HashFromSerialRepresentation(serializedCompositionRecord);

                lastStateHash = compositionHash;

                return (serializedCompositionRecord, compositionHash);
            }
        }
    }

    public class PersistentProcessWithControlFlowOverStoreWriter : IDisposableProcessWithStringInterface
    {
        IPersistentProcess process;

        IProcessStoreWriter storeWriter;

        public PersistentProcessWithControlFlowOverStoreWriter(
            IPersistentProcess process,
            IProcessStoreWriter storeWriter)
        {
            this.process = process;
            this.storeWriter = storeWriter;
        }

        public string ProcessEvent(string serializedEvent)
        {
            lock (process)
            {
                var (responses, (serializedCompositionRecord, serializedCompositionRecordHash)) =
                    process.ProcessEvents(new[] { serializedEvent });

                var response = responses.Single();

                storeWriter.AppendSerializedCompositionRecord(serializedCompositionRecord);
                storeWriter.StoreReduction(process.ReductionRecordForCurrentState());

                return response;
            }
        }

        string IProcess<string, string>.GetSerializedState() => process.ReductionRecordForCurrentState().ReducedValueLiteralString;

        string IProcess<string, string>.SetSerializedState(string serializedState)
        {
            lock (process)
            {
                var (serializedCompositionRecord, serializedCompositionRecordHash) =
                    process.SetState(serializedState);

                storeWriter.AppendSerializedCompositionRecord(serializedCompositionRecord);

                return null;
            }
        }

        public void Dispose() => (process as IDisposable)?.Dispose();
    }
}
