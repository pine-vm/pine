using System;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Kalmit
{
    class ProcessHistoryEntry
    {
        public HistoryEntryBranch Branch;

        public HistoryEntryReduction Reduction;

        public class HistoryEntryBranch
        {
            public byte[] ParentHash;

            public string[] AddedEvents;
        }

        public class HistoryEntryReduction
        {
            public byte[] BranchHash;

            public string ReducedValue;
        }
    }

    //  A provisional special case for a process from an elm app. Migrations give an example of why the elm code should be modeled on the history as well.
    public class PersistentProcessWithHistoryOnFileFromElm019Code : IDisposableProcessWithCustomSerialization
    {
        string processHistoryFilePath;

        byte[] lastBranchHash;

        IDisposableProcessWithCustomSerialization process;

        static byte[] HashFromEntry(byte[] serializedHistoryEntry) =>
            new System.Security.Cryptography.SHA256Managed().ComputeHash(serializedHistoryEntry);

        static ProcessHistoryEntry HistoryEntryFromSerializedEntry(string serializedEntry) =>
            JsonConvert.DeserializeObject<ProcessHistoryEntry>(serializedEntry);

        static string Serialize(ProcessHistoryEntry historyEntry) =>
            JsonConvert.SerializeObject(historyEntry);

        public PersistentProcessWithHistoryOnFileFromElm019Code(string processHistoryFilePath, string elmAppFilePath)
        {
            this.processHistoryFilePath = processHistoryFilePath;

            var elmAppFile = File.ReadAllBytes(elmAppFilePath);

            var appStatic = ElmAppWithEntryConfig.FromFiles(ZipArchive.EntriesFromZipArchive(elmAppFile).ToList());

            process =
                ProcessFromElm019Code.WithCustomSerialization(
                appStatic.ElmAppFiles,
                appStatic.EntryConfig.Value.WithCustomSerialization.Value);

            var historyLines =
                File.Exists(processHistoryFilePath) ?
                File.ReadAllLines(processHistoryFilePath, Encoding.UTF8) : new string[0];

            var lastHistoryEntryWithBranch =
                historyLines.Reverse()
                .Select(historyLine =>
                {
                    var historyEntry = HistoryEntryFromSerializedEntry(historyLine);

                    if (historyEntry.Branch != null)
                        return (historyLine: historyLine, branch: historyEntry.Branch);

                    return (null, null);
                }).FirstOrDefault(t => t.historyLine != null);

            if (lastHistoryEntryWithBranch.branch != null)
            {
                var lastHistoryEntryWithBranchHash =
                    HashFromEntry(Encoding.UTF8.GetBytes(lastHistoryEntryWithBranch.historyLine));

                var historyEntryForReduction =
                    historyLines.Reverse()
                    .Select(historyLine =>
                    {
                        var historyEntry = HistoryEntryFromSerializedEntry(historyLine);

                        if (lastHistoryEntryWithBranchHash.SequenceEqual(historyEntry.Reduction?.BranchHash))
                            return historyEntry.Reduction;

                        return null;
                    }).WhereNotNull().FirstOrDefault();

                if (historyEntryForReduction == null)
                    throw new NotImplementedException(
                        "I did not find a reduction of the last branch (" +
                        JsonConvert.SerializeObject(lastHistoryEntryWithBranchHash) +
                        "). Processing events for restoring is not yet implemented.");

                process.SetSerializedState(historyEntryForReduction.ReducedValue);
                lastBranchHash = lastHistoryEntryWithBranchHash;
            }
            else
            {
                lastBranchHash = HashFromEntry(new byte[0]);
            }
        }

        public void Dispose() => process?.Dispose();

        public string ProcessEvent(string serializedEvent)
        {
            lock (process)
            {
                var response = process.ProcessEvent(serializedEvent);

                var historyEntryBranch = new ProcessHistoryEntry
                {
                    Branch = new ProcessHistoryEntry.HistoryEntryBranch
                    {
                        ParentHash = lastBranchHash,
                        AddedEvents = new[] { serializedEvent },
                    },
                };

                var historyEntryBranchSerialized =
                    Encoding.UTF8.GetBytes(Serialize(historyEntryBranch));

                var historyEntryBranchHash = HashFromEntry(historyEntryBranchSerialized);

                var historyEntryReduce = new ProcessHistoryEntry
                {
                    Reduction = new ProcessHistoryEntry.HistoryEntryReduction
                    {
                        BranchHash = historyEntryBranchHash,
                        ReducedValue = process.GetSerializedState(),
                    },
                };

                var linesToWrite = new[]
                {
                    Encoding.UTF8.GetString(historyEntryBranchSerialized),
                    Serialize(historyEntryReduce),
                };

                File.AppendAllLines(processHistoryFilePath, linesToWrite);

                lastBranchHash = historyEntryBranchHash;

                return response;
            }
        }

        string IProcess<string, string>.GetSerializedState()
        {
            throw new NotImplementedException();
        }

        string IProcess<string, string>.SetSerializedState(string serializedState)
        {
            throw new NotImplementedException();
        }
    }
}
