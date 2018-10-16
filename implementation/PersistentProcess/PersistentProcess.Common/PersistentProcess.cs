using System;
using System.IO;
using System.Linq;
using System.Text;
using Kalmit.ProcessStore;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess
{
    //  A provisional special case for a process from an elm app. Migrations give an example of why the elm code should be modeled on the history as well.
    public class PersistentProcessWithHistoryOnFileFromElm019Code : IDisposableProcessWithCustomSerialization
    {
        IProcessStoreSink storeSink;

        byte[] lastStateHash;

        IDisposableProcessWithCustomSerialization process;

        public PersistentProcessWithHistoryOnFileFromElm019Code(
            IProcessStoreSource storeSource,
            IProcessStoreSink storeSink,
            string elmAppFilePath)
        {
            this.storeSink = storeSink;

            var elmApp =
                ElmAppWithEntryConfig.FromFiles(ZipArchive.EntriesFromZipArchive(File.ReadAllBytes(elmAppFilePath)).ToList());

            process =
                ProcessFromElm019Code.WithCustomSerialization(
                elmApp.ElmAppFiles,
                elmApp.EntryConfig.Value.WithCustomSerialization.Value);

            var lastHashAndComposition =
                storeSource.EnumerateCompositionsReverse().FirstOrDefault();

            if (lastHashAndComposition.hash != null)
            {
                var historyEntryForReduction =
                    storeSource.GetReduction(lastHashAndComposition.hash);

                if (historyEntryForReduction == null)
                    throw new NotImplementedException(
                        "I did not find a reduction of the last composition (" +
                        JsonConvert.SerializeObject(lastHashAndComposition.hash) +
                        "). Processing events for restoring is not yet implemented.");

                process.SetSerializedState(historyEntryForReduction.ReducedValue);
                lastStateHash = lastHashAndComposition.hash;
            }
            else
            {
                lastStateHash = null;
            }
        }

        public void Dispose() => process?.Dispose();

        public string ProcessEvent(string serializedEvent)
        {
            lock (process)
            {
                var response = process.ProcessEvent(serializedEvent);

                var composition = new CompositionRecord
                {
                    ParentHash = lastStateHash,
                    AppendedEvents = new[] { serializedEvent },
                };

                var compositionHash = storeSink.AppendComposition(composition);

                var reduction = new ReductionRecord
                {
                    ReducedCompositionHash = compositionHash,
                    ReducedValue = process.GetSerializedState(),
                };

                storeSink.StoreReduction(reduction);

                lastStateHash = compositionHash;

                return response;
            }
        }

        string IProcess<string, string>.GetSerializedState() => process?.GetSerializedState();

        string IProcess<string, string>.SetSerializedState(string serializedState)
        {
            throw new NotImplementedException();
        }
    }
}
