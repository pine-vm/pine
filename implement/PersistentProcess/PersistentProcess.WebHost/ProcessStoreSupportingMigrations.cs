using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations
{
    public interface IProcessStoreWriter
    {
        void AppendSerializedCompositionLogRecord(byte[] serializedCompositionRecord);

        void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction);

        void StoreComponent(Composition.Component component);
    }

    public interface IProcessStoreReader
    {
        IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse();

        ProvisionalReductionRecordInFile LoadProvisionalReduction(string reducedCompositionLogRecordHash);

        Composition.Component LoadComponent(string componentHash);

        static public IProcessStoreReader ProjectReaderForAppendedCompositionLogEvent(
            IProcessStoreReader originalStore,
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
        {
            return new DelegatingProcessStoreReader
            {
                LoadComponentDelegate = originalStore.LoadComponent,
                LoadProvisionalReductionDelegate = originalStore.LoadProvisionalReduction,
                EnumerateSerializedCompositionLogRecordsReverseDelegate = () =>
                    AppendCompositionEventToCompositionLogChain(
                        originalStore.EnumerateSerializedCompositionLogRecordsReverse(),
                        compositionLogEvent)
            };
        }

        static public IProcessStoreReader EmptyProcessStoreReader()
        {
            return new DelegatingProcessStoreReader
            {
                LoadComponentDelegate = _ => null,
                LoadProvisionalReductionDelegate = _ => null,
                EnumerateSerializedCompositionLogRecordsReverseDelegate = () => ImmutableList<byte[]>.Empty
            };
        }

        static IEnumerable<byte[]> AppendCompositionEventToCompositionLogChain(
            IEnumerable<byte[]> originalCompositionLogRecordsReverse,
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
        {
            byte[] buildRecordForParentHash(string parentHashBase16)
            {
                var compositionLogRecord = new ProcessStoreSupportingMigrations.CompositionLogRecordInFile
                {
                    parentHashBase16 = parentHashBase16,
                    events = ImmutableList.Create(compositionLogEvent),
                };

                return ProcessStoreInFileStore.Serialize(compositionLogRecord);
            }

            bool isLast = true;

            foreach (var originalRecordSerialized in originalCompositionLogRecordsReverse)
            {
                if (isLast)
                {
                    yield return buildRecordForParentHash(
                        CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(originalRecordSerialized)));

                    isLast = false;
                }

                yield return originalRecordSerialized;
            }

            if (isLast)
                yield return buildRecordForParentHash(CompositionLogRecordInFile.compositionLogEmptyInitHashBase16);
        }
    }

    class DelegatingProcessStoreReader : IProcessStoreReader
    {
        public Func<IEnumerable<byte[]>> EnumerateSerializedCompositionLogRecordsReverseDelegate;

        public Func<string, Composition.Component> LoadComponentDelegate;

        public Func<string, ProvisionalReductionRecordInFile> LoadProvisionalReductionDelegate;

        public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse() =>
            EnumerateSerializedCompositionLogRecordsReverseDelegate();

        public Composition.Component LoadComponent(string componentHash) =>
            LoadComponentDelegate(componentHash);

        public ProvisionalReductionRecordInFile LoadProvisionalReduction(string reducedCompositionLogRecordHash) =>
            LoadProvisionalReductionDelegate(reducedCompositionLogRecordHash);
    }

    public class DelegatingProcessStoreWriter : IProcessStoreWriter
    {
        public Action<byte[]> AppendSerializedCompositionLogRecordDelegate;

        public Action<Composition.Component> StoreComponentDelegate;

        public Action<ProvisionalReductionRecordInFile> StoreProvisionalReductionDelegate;

        public void AppendSerializedCompositionLogRecord(byte[] serializedCompositionRecord) =>
            AppendSerializedCompositionLogRecordDelegate(serializedCompositionRecord);

        public void StoreComponent(Composition.Component component) =>
            StoreComponentDelegate(component);

        public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction) =>
            StoreProvisionalReductionDelegate(reduction);
    }

    public class ValueInFileStructure
    {
        public string HashBase16;
    }

    public class CompositionLogRecordInFile
    {
        static public string compositionLogEmptyInitHashBase16 =>
            CommonConversion.StringBase16FromByteArray(
                CompositionLogRecordInFile.HashFromSerialRepresentation(new byte[0]));

        public string parentHashBase16;

        public IReadOnlyList<CompositionEvent> events;

        static public byte[] HashFromSerialRepresentation(byte[] serialized) =>
            CommonConversion.HashSHA256(serialized);

        public class CompositionEvent
        {
            public ValueInFileStructure SetElmAppState;

            public ValueInFileStructure UpdateElmAppStateForEvent;

            public ValueInFileStructure DeployAppConfigAndInitElmAppState;

            public ValueInFileStructure DeployAppConfigAndMigrateElmAppState;
        }
    }

    public class ProvisionalReductionRecordInFile
    {
        public string reducedCompositionHashBase16;

        public ValueInFileStructure elmAppState;

        public ValueInFileStructure appConfig;
    }

    public class ProcessStoreInFileStore
    {
        static public Newtonsoft.Json.JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static readonly protected Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

        static public IImmutableList<string> GetFilePathForComponentInComponentFileStore(string componentHash) =>
            ImmutableList.Create(componentHash.Substring(0, 2), componentHash);

        protected IFileStore fileStore;

        //  Plain Kalmit component.
        protected IFileStore componentFileStore => new FileStoreFromSubdirectory(fileStore, "component");

        protected IFileStore compositionLogFileStore => new FileStoreFromSubdirectory(fileStore, "composition-log");

        protected IFileStore provisionalReductionFileStore => new FileStoreFromSubdirectory(fileStore, "provisional-reduction");

        static protected IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesNames) =>
            logFilesNames?.OrderBy(filePath => string.Join("", filePath));

        static public byte[] Serialize(CompositionLogRecordInFile record) =>
            Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(record, recordSerializationSettings));

        public IEnumerable<IImmutableList<string>> EnumerateCompositionsLogFilesPaths() =>
            CompositionLogFileOrder(
                compositionLogFileStore.ListFilesInDirectory(ImmutableList<string>.Empty));

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

        public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse() =>
            EnumerateCompositionsLogFilesPaths().Reverse()
            .SelectMany(compositionFilePath =>
                Encoding.UTF8.GetString(compositionLogFileStore.GetFileContent(compositionFilePath))
                .Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).Reverse()
                .Select(compositionRecord => Encoding.UTF8.GetBytes(compositionRecord)));

        IReadOnlyList<byte> LoadComponentSerialRepresentationForHash(IReadOnlyList<byte> componentHash) =>
            componentFileStore.GetFileContent(
                GetFilePathForComponentInComponentFileStore(CommonConversion.StringBase16FromByteArray(componentHash.ToArray())));

        public Composition.Component LoadComponent(string componentHashBase16)
        {
            var fromComponentStore = componentFileStore.GetFileContent(
                GetFilePathForComponentInComponentFileStore(componentHashBase16));

            if (fromComponentStore == null)
                return null;

            var loadComponentResult =
                Composition.Deserialize(fromComponentStore, LoadComponentSerialRepresentationForHash);

            if (loadComponentResult.Ok == null)
                throw new Exception("Failed to load component " + componentHashBase16 + ": " + loadComponentResult.Err);

            if (CommonConversion.StringBase16FromByteArray(Composition.GetHash(loadComponentResult.Ok)) != componentHashBase16)
                throw new Exception("Unexpected content in file " + componentHashBase16 + ": Content hash does not match.");

            return loadComponentResult.Ok;
        }

        public ProvisionalReductionRecordInFile LoadProvisionalReduction(string reducedCompositionHash)
        {
            var filePath = ImmutableList.Create(reducedCompositionHash);

            var fileContent = provisionalReductionFileStore.GetFileContent(filePath);

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
                    JsonConvert.DeserializeObject<ProvisionalReductionRecordInFile>(
                        Encoding.UTF8.GetString(fileContent.AsSpan(payloadStartIndex)));

                if (reducedCompositionHash != reductionRecordFromFile.reducedCompositionHashBase16)
                    throw new Exception("Unexpected content in file " + string.Join("/", filePath) + ", composition hash does not match.");

                return reductionRecordFromFile;
            }
            catch (Exception e)
            {
                throw new Exception("Failed to read reduction from file '" + string.Join("/", filePath) + "'.", e);
            }
        }

        public IEnumerable<string> ReductionsFilesNames() =>
            provisionalReductionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
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

        public void AppendSerializedCompositionLogRecord(byte[] record)
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

                compositionLogFileStore.AppendFileContent(compositionLogFilePath, record.Concat(Encoding.UTF8.GetBytes("\n")).ToArray());

                appendSerializedCompositionRecordLastFilePath = compositionLogFilePath;
            }
        }

        public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reductionRecord)
        {
            var fileName = reductionRecord.reducedCompositionHashBase16;

            var filePath = ImmutableList.Create(fileName);

            provisionalReductionFileStore.SetFileContent(
                filePath, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(reductionRecord, recordSerializationSettings)));
        }

        public void StoreComponent(Composition.Component component)
        {
            var (serialRepresentation, dependencies) = Composition.GetSerialRepresentationAndDependencies(component);

            var hashBase16 = CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(serialRepresentation));

            componentFileStore.SetFileContent(
                GetFilePathForComponentInComponentFileStore(hashBase16),
                serialRepresentation);

            foreach (var dependency in dependencies)
                StoreComponent(dependency);
        }
    }
}
