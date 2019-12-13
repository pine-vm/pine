using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Kalmit.ProcessStore
{
    public class ValueInFile
    {
        public string LiteralString;

        /*
        Future: Reduce operation expenses by sharing objects: Support reusing values here by reference.
        public string HashBase16;
        */

        /*
        Future: Switch to a simpler literal model.
        public byte[] Literal;
        */
    }

    public class CompositionRecord
    {
        public byte[] ParentHash;

        public string SetStateLiteralString;

        public IReadOnlyList<string> AppendedEventsLiteralString;
    }

    public class CompositionRecordInFile
    {
        public string ParentHashBase16;

        public ValueInFile SetState;

        public IReadOnlyList<ValueInFile> AppendedEvents;

        static public byte[] HashFromSerialRepresentation(byte[] serialized) =>
            CommonConversion.HashSHA256(serialized);
    }

    public class ReductionRecord
    {
        public byte[] ReducedCompositionHash;

        public string ReducedValueLiteralString;
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
        private class ReductionRecordInFile
        {
            public string ReducedCompositionHashBase16;

            public ValueInFile ReducedValue;
        }

        string directory;

        Func<string> getCompositionLogRequestedNextFileName;

        readonly object appendSerializedCompositionRecordLock = new object();
        string appendSerializedCompositionRecordLastFileName = null;

        string CompositionDirectoryPath => Path.Combine(directory, "composition");

        string ReductionDirectoryPath => Path.Combine(directory, "reduction");

        static public Newtonsoft.Json.JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

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
            CompositionLogFileOrder(Directory.GetFiles(CompositionDirectoryPath, "*", SearchOption.AllDirectories)) :
            (IEnumerable<string>)Array.Empty<string>();

        public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
            EnumerateCompositionsLogFilesNames().Reverse()
            .SelectMany(compositionFilePath =>
                File.ReadAllLines(compositionFilePath, Encoding.UTF8).Reverse().Select(Encoding.UTF8.GetBytes));

        public ReductionRecord GetReduction(byte[] reducedCompositionHash)
        {
            var reducedCompositionHashBase16 = CommonConversion.StringBase16FromByteArray(reducedCompositionHash);

            var filePath = Path.Combine(ReductionDirectoryPath, reducedCompositionHashBase16);

            if (!File.Exists(filePath))
                return null;

            var reductionRecordFromFile =
                JsonConvert.DeserializeObject<ReductionRecordInFile>(
                    File.ReadAllText(filePath, Encoding.UTF8));

            if (reducedCompositionHashBase16 != reductionRecordFromFile.ReducedCompositionHashBase16)
                throw new Exception("Unexpected content in file " + filePath + ", composition hash does not match.");

            return new ReductionRecord
            {
                ReducedCompositionHash = reducedCompositionHash,
                ReducedValueLiteralString = reductionRecordFromFile.ReducedValue?.LiteralString,
            };
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
            var recordInFile = new ReductionRecordInFile
            {
                ReducedCompositionHashBase16 = CommonConversion.StringBase16FromByteArray(record.ReducedCompositionHash),
                ReducedValue = new ValueInFile { LiteralString = record.ReducedValueLiteralString },
            };

            var fileName = recordInFile.ReducedCompositionHashBase16;

            var filePath = Path.Combine(ReductionDirectoryPath, fileName);

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.WriteAllText(filePath, JsonConvert.SerializeObject(recordInFile, RecordSerializationSettings), Encoding.UTF8);
        }

        public IEnumerable<string> ReductionsFilesNames() =>
            Directory.Exists(ReductionDirectoryPath) ?
            Directory.EnumerateFiles(ReductionDirectoryPath, "*", SearchOption.AllDirectories) :
            Array.Empty<string>();
    }
}
