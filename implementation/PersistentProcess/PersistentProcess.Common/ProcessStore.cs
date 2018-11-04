using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Kalmit.ProcessStore
{
    public class CompositionRecord
    {
        public byte[] ParentHash;

        public IReadOnlyList<string> AppendedEvents;

        static public byte[] HashFromSerialRepresentation(byte[] serialized) =>
            new System.Security.Cryptography.SHA256Managed().ComputeHash(serialized);
    }

    public class ReductionRecord
    {
        public byte[] ReducedCompositionHash;

        public string ReducedValue;
    }

    public interface IProcessStoreWriter
    {
        void AppendSerializedCompositionRecord(byte[] serializedCompositionRecord);

        void StoreReduction(ReductionRecord reduction);
    }

    public interface IProcessStoreReader
    {
        IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse();

        ReductionRecord GetReduction(byte[] reducedCompositionHash);
    }

    public class ProcessStoreInFileDirectory : IProcessStoreWriter, IProcessStoreReader
    {
        string directory;

        Func<string> getCompositionLogRequestedNextFileName;

        readonly object appendSerializedCompositionRecordLock = new object();
        string appendSerializedCompositionRecordLastFileName = null;

        string CompositionDirectoryPath => Path.Combine(directory, "composition");

        string ReductionDirectoryPath => Path.Combine(directory, "reduction");

        static public string ReductionFileNameFromReducedCompositionHash(byte[] hash) =>
            BitConverter.ToString(hash).Replace("-", "");

        string ReductionFilePathFromReducedCompositionHash(byte[] hash) =>
            Path.Combine(ReductionDirectoryPath, ReductionFileNameFromReducedCompositionHash(hash));

        public ProcessStoreInFileDirectory(
            string directory,
            Func<string> getCompositionLogRequestedNextFileName)
        {
            this.directory = directory;
            this.getCompositionLogRequestedNextFileName = getCompositionLogRequestedNextFileName;
        }

        static IEnumerable<string> CompositionLogFileOrder(IEnumerable<string> logFilesNames) =>
            logFilesNames?.OrderBy(fileName => fileName);

        public IEnumerable<string> EnumerateCompositionsLogFilesNames() =>
            Directory.Exists(CompositionDirectoryPath) ?
            CompositionLogFileOrder(Directory.GetFiles(CompositionDirectoryPath)) :
            (IEnumerable<string>)Array.Empty<string>();

        public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
            EnumerateCompositionsLogFilesNames().Reverse()
            .SelectMany(compositionFilePath =>
                File.ReadAllLines(compositionFilePath, Encoding.UTF8).Reverse().Select(Encoding.UTF8.GetBytes));

        public ReductionRecord GetReduction(byte[] reducedCompositionHash)
        {
            var filePath = ReductionFilePathFromReducedCompositionHash(reducedCompositionHash);

            if (!File.Exists(filePath))
                return null;

            var reduction =
                JsonConvert.DeserializeObject<ReductionRecord>(
                    File.ReadAllText(filePath, Encoding.UTF8));

            if (!reducedCompositionHash.SequenceEqual(reduction.ReducedCompositionHash))
                throw new Exception("Unexpected content in file " + filePath + ", hash does not match");

            return reduction;
        }

        public void AppendSerializedCompositionRecord(byte[] record)
        {
            lock (appendSerializedCompositionRecordLock)
            {
                var compositionLogRequestedFileNameInDirectory =
                    getCompositionLogRequestedNextFileName?.Invoke() ?? appendSerializedCompositionRecordLastFileName ?? "composition";

                var compositionLogRequestedFileName =
                    Path.Combine(CompositionDirectoryPath, compositionLogRequestedFileNameInDirectory);

                var compositionLogFileName = appendSerializedCompositionRecordLastFileName;

                if (!compositionLogRequestedFileName.Equals(compositionLogFileName))
                {
                    // When reading the composition log, we depend on file names to determine the order of records.
                    // Therefore, only switch to the requested filename if it will be the last in that order.

                    var lastFileNameIfAddRequestedFileName =
                        CompositionLogFileOrder(
                            EnumerateCompositionsLogFilesNames().Concat(new[] { compositionLogRequestedFileName })).Last();

                    if (compositionLogRequestedFileName.Equals(lastFileNameIfAddRequestedFileName))
                        compositionLogFileName = compositionLogRequestedFileName;
                }

                Directory.CreateDirectory(Path.GetDirectoryName(compositionLogFileName));
                File.AppendAllLines(compositionLogFileName, new[] { Encoding.UTF8.GetString(record) });

                appendSerializedCompositionRecordLastFileName = compositionLogFileName;
            }
        }

        public void StoreReduction(ReductionRecord record)
        {
            var filePath = ReductionFilePathFromReducedCompositionHash(record.ReducedCompositionHash);

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.WriteAllText(filePath, JsonConvert.SerializeObject(record), Encoding.UTF8);
        }

        public IEnumerable<string> ReductionsFilesNames() =>
            Directory.Exists(ReductionDirectoryPath) ?
            Directory.EnumerateFiles(ReductionDirectoryPath, "*", SearchOption.AllDirectories) :
            Array.Empty<string>();
    }
}
