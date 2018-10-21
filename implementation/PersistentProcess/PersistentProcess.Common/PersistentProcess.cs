using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Kalmit.ProcessStore;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess
{
    public interface IPersistentProcess
    {
        (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash))
            ProcessEvents(IReadOnlyList<string> serializedEvents);

        ReductionRecord ReductionRecordForCurrentState();
    }

    //  A provisional special case for a process from an elm app. Migrations give an example of why the elm code should be modeled on the history as well.
    public class PersistentProcessWithHistoryOnFileFromElm019Code : IPersistentProcess, IDisposable
    {
        byte[] lastStateHash;

        IDisposableProcessWithCustomSerialization process;

        public PersistentProcessWithHistoryOnFileFromElm019Code(
            IProcessStoreReader storeReader,
            byte[] elmAppFile)
        {
            var elmApp =
                ElmAppWithEntryConfig.FromFiles(ZipArchive.EntriesFromZipArchive(elmAppFile).ToList());

            process =
                ProcessFromElm019Code.WithCustomSerialization(
                elmApp.ElmAppFiles,
                elmApp.EntryConfig.Value.WithCustomSerialization.Value);

            var emptyInitHash = CompositionRecord.HashFromSerialRepresentation(new byte[0]);

            var compositionChain = new Stack<(byte[] hash, CompositionRecord composition)>();

            foreach (var serializedCompositionRecord in storeReader.EnumerateSerializedCompositionsRecordsReverse())
            {
                var compositionRecord = JsonConvert.DeserializeObject<CompositionRecord>(
                    System.Text.Encoding.UTF8.GetString(serializedCompositionRecord));

                var compositionRecordHash = CompositionRecord.HashFromSerialRepresentation(serializedCompositionRecord);

                var compositionChainElement = (compositionRecordHash, compositionRecord);

                if (compositionChain.Any() && !compositionRecordHash.SequenceEqual(compositionChain.Peek().composition.ParentHash))
                    continue;   //  Not connected to the chain from the last composition record.

                var reduction = storeReader.GetReduction(compositionRecordHash);

                if (reduction != null || emptyInitHash.SequenceEqual(compositionRecord.ParentHash))
                {
                    if (reduction != null)
                    {
                        process.SetSerializedState(reduction.ReducedValue);
                        lastStateHash = reduction.ReducedCompositionHash;
                    }
                    else
                    {
                        compositionChain.Push(compositionChainElement);
                    }

                    foreach (var followingComposition in compositionChain)
                    {
                        foreach (var appendedEvent in followingComposition.composition.AppendedEvents)
                            process.ProcessEvent(appendedEvent);

                        lastStateHash = followingComposition.hash;
                    }

                    return;
                }

                compositionChain.Push(compositionChainElement);
            }

            if (compositionChain.Any())
                throw new NotImplementedException(
                    "I did not find a reduction for any composition on the chain to the last composition (" +
                    JsonConvert.SerializeObject(compositionChain.Last().hash) +
                    ").");

            lastStateHash = emptyInitHash;
        }

        static string Serialize(CompositionRecord composition) =>
            JsonConvert.SerializeObject(composition);

        public (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash))
            ProcessEvents(IReadOnlyList<string> serializedEvents)
        {
            lock (process)
            {
                var responses =
                    serializedEvents.Select(serializedEvent => process.ProcessEvent(serializedEvent))
                    .ToList();

                var compositionRecord = new CompositionRecord
                {
                    ParentHash = lastStateHash,
                    AppendedEvents = serializedEvents,
                };

                var serializedCompositionRecord =
                    Encoding.UTF8.GetBytes(Serialize(compositionRecord));

                var compositionHash = CompositionRecord.HashFromSerialRepresentation(serializedCompositionRecord);

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
                        ReducedValue = process.GetSerializedState(),
                    };
            }
        }
    }

    public class PersistentProcessWithControlFlowOverStoreWriter : IDisposableProcessWithCustomSerialization
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

        string IProcess<string, string>.GetSerializedState() => process.ReductionRecordForCurrentState().ReducedValue;

        string IProcess<string, string>.SetSerializedState(string serializedState)
        {
            throw new NotImplementedException();
        }

        public void Dispose() => (process as IDisposable)?.Dispose();
    }
}
