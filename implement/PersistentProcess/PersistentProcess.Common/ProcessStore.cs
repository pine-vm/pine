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

    public class EmptyProcessStoreReader : IProcessStoreReader
    {
        public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse()
        {
            yield break;
        }

        public ReductionRecord GetReduction(byte[] reducedCompositionHash) => null;
    }

    public class ProcessStoreInFileStore
    {
        static public Newtonsoft.Json.JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static readonly protected Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

        protected class ReductionRecordInFile
        {
            public string ReducedCompositionHashBase16;

            public ValueInFile ReducedValue;
        }

        protected IFileStore fileStore;

        protected IFileStore compositionFileStore => new FileStoreFromSubdirectory(fileStore, "composition");

        protected IFileStore reductionFileStore => new FileStoreFromSubdirectory(fileStore, "reduction");

        static protected IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesNames) =>
            logFilesNames?.OrderBy(filePath => string.Join("", filePath));

        public IEnumerable<IImmutableList<string>> EnumerateCompositionsLogFilesPaths() =>
            CompositionLogFileOrder(
                compositionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty));

        public ProcessStoreInFileStore(IFileStore fileStore)
        {
            this.fileStore = fileStore;
        }
    }

    public class ProcessStoreReaderInFileStore : ProcessStoreInFileStore, IProcessStoreReader
    {
        public ProcessStoreReaderInFileStore(IFileStore fileStore)
            : base(fileStore)
        {
        }

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

            try
            {
                var payloadStartIndex =
                    /*
                    Previous implementation used `File.WriteAllText`:
                    https://github.com/elm-fullstack/elm-fullstack/blob/1cd3f00bdf5a05e9bda479c534b0458b2496393c/implement/PersistentProcess/PersistentProcess.Common/ProcessStore.cs#L183
                    Looking at the files from stores in production, it seems like that caused addition of BOM.
                    */
                    fileContent.Take(3).SequenceEqual(new byte[] { 0xEF, 0xBB, 0xBF })
                    ?
                    3
                    :
                    0;

                var reductionRecordFromFile =
                    JsonConvert.DeserializeObject<ReductionRecordInFile>(Encoding.UTF8.GetString(fileContent.AsSpan(payloadStartIndex)));

                if (reducedCompositionHashBase16 != reductionRecordFromFile.ReducedCompositionHashBase16)
                    throw new Exception("Unexpected content in file " + string.Join("/", filePath) + ", composition hash does not match.");

                return new ReductionRecord
                {
                    ReducedCompositionHash = reducedCompositionHash,
                    ReducedValueLiteralString = reductionRecordFromFile.ReducedValue?.LiteralString,
                };
            }
            catch (Exception e)
            {
                throw new Exception("Failed to read reduction from file '" + string.Join("/", filePath) + "'.", e);
            }
        }

        public IEnumerable<string> ReductionsFilesNames() =>
            reductionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
            .Select(Enumerable.Last);
    }

    public class ProcessStoreWriterInFileStore : ProcessStoreInFileStore, IProcessStoreWriter
    {
        Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath;

        readonly object appendSerializedCompositionRecordLock = new object();

        IImmutableList<string> appendSerializedCompositionRecordLastFilePath = null;

        public ProcessStoreWriterInFileStore(
            IFileStore fileStore,
            Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath)
            : base(fileStore)
        {
            this.getCompositionLogRequestedNextFilePath = getCompositionLogRequestedNextFilePath;
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
                filePath, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(recordInFile, recordSerializationSettings)));
        }
    }
}
