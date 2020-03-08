using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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

    public class ProcessStoreInFileDirectory : ProcessStoreInFileStore
    {
        public ProcessStoreInFileDirectory(string directoryPath, Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath)
            : base(new FileStoreFromSystemIOFile(directoryPath), getCompositionLogRequestedNextFilePath)
        {
        }
    }

    public class ProcessStoreInFileStore : IProcessStoreWriter, IProcessStoreReader
    {
        private class ReductionRecordInFile
        {
            public string ReducedCompositionHashBase16;

            public ValueInFile ReducedValue;
        }

        IFileStore fileStore;

        Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath;

        readonly object appendSerializedCompositionRecordLock = new object();
        IImmutableList<string> appendSerializedCompositionRecordLastFilePath = null;

        IFileStore compositionFileStore => new FileStoreFromSubdirectory(fileStore, "composition");

        IFileStore reductionFileStore => new FileStoreFromSubdirectory(fileStore, "reduction");

        static public Newtonsoft.Json.JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

        public ProcessStoreInFileStore(
            IFileStore fileStore,
            Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath)
        {
            this.fileStore = fileStore;
            this.getCompositionLogRequestedNextFilePath = getCompositionLogRequestedNextFilePath;
        }

        static IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesNames) =>
            logFilesNames?.OrderBy(filePath => string.Join("", filePath));

        public IEnumerable<IImmutableList<string>> EnumerateCompositionsLogFilesPaths() =>
            CompositionLogFileOrder(
                compositionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty));

        public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
            EnumerateCompositionsLogFilesPaths().Reverse()
            .SelectMany(compositionFilePath =>
                Encoding.UTF8.GetString(compositionFileStore.GetFileContent(compositionFilePath))
                .Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).Reverse()
                .Select(compositionRecord => Encoding.UTF8.GetBytes(compositionRecord)));

        public ReductionRecord GetReduction(byte[] reducedCompositionHash)
        {
            var reducedCompositionHashBase16 = CommonConversion.StringBase16FromByteArray(reducedCompositionHash);

            var filePath = ImmutableList.Create(reducedCompositionHashBase16);

            var fileContent = reductionFileStore.GetFileContent(filePath);

            if (fileContent == null)
                return null;

            var reductionRecordFromFile =
                JsonConvert.DeserializeObject<ReductionRecordInFile>(Encoding.UTF8.GetString(fileContent));

            if (reducedCompositionHashBase16 != reductionRecordFromFile.ReducedCompositionHashBase16)
                throw new Exception("Unexpected content in file " + string.Join("/", filePath) + ", composition hash does not match.");

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
                var lastOrDefaultPath = appendSerializedCompositionRecordLastFilePath ?? ImmutableList.Create("composition");

                var compositionLogRequestedFilePathInDirectory =
                    getCompositionLogRequestedNextFilePath?.Invoke() ?? lastOrDefaultPath;

                var compositionLogFilePath = lastOrDefaultPath;

                if (!compositionLogRequestedFilePathInDirectory.SequenceEqual(compositionLogFilePath))
                {
                    // When reading the composition log, we depend on file names to determine the order of records.
                    // Therefore, only switch to the requested filename if it will be the last in that order.

                    var lastFileNameIfAddRequestedFileName =
                        CompositionLogFileOrder(
                            EnumerateCompositionsLogFilesPaths().Concat(new[] { compositionLogRequestedFilePathInDirectory }))
                        .Last();

                    if (compositionLogRequestedFilePathInDirectory.Equals(lastFileNameIfAddRequestedFileName))
                        compositionLogFilePath = compositionLogRequestedFilePathInDirectory;
                }

                compositionFileStore.AppendFileContent(compositionLogFilePath, record.Concat(Encoding.UTF8.GetBytes("\n")).ToArray());

                appendSerializedCompositionRecordLastFilePath = compositionLogFilePath;
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

            var filePath = ImmutableList.Create(fileName);

            reductionFileStore.SetFileContent(
                filePath, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(recordInFile, RecordSerializationSettings)));
        }

        public IEnumerable<string> ReductionsFilesNames() =>
            reductionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
            .Select(Enumerable.Last);
    }
}
