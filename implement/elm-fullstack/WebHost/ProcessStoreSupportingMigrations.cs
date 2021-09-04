using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Newtonsoft.Json;
using Pine;

namespace ElmFullstack.WebHost.ProcessStoreSupportingMigrations
{
    public interface IProcessStoreWriter
    {
        (byte[] recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent);

        void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction);

        void StoreComponent(Composition.Component component);
    }

    public interface IProcessStoreReader
    {
        IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse();

        ProvisionalReductionRecordInFile LoadProvisionalReduction(string reducedCompositionLogRecordHash);

        Composition.Component LoadComponent(string componentHash);

        static public (IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> projectedFiles, IFileStoreReader projectedReader)
            ProjectFileStoreReaderForAppendedCompositionLogEvent(
            IFileStoreReader originalFileStore,
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
        {
            var projectedFiles =
                new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, IReadOnlyList<byte>>(
                    comparer: EnumerableExtension.EqualityComparer<string>());

            var fileStoreWriter = new DelegatingFileStoreWriter
            {
                SetFileContentDelegate = pathAndFileContent => projectedFiles[pathAndFileContent.path] = pathAndFileContent.fileContent,
                AppendFileContentDelegate = pathAndFileContent =>
                {
                    if (!projectedFiles.TryGetValue(pathAndFileContent.path, out var fileContentBefore))
                        fileContentBefore = originalFileStore.GetFileContent(pathAndFileContent.path);

                    projectedFiles[pathAndFileContent.path] = (fileContentBefore ?? new byte[] { }).Concat(pathAndFileContent.fileContent).ToArray();
                },
                DeleteFileDelegate = _ => throw new Exception("Unexpected operation delete file."),
            };

            var processStoreWriter = new ProcessStoreWriterInFileStore(
                originalFileStore,
                getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                fileStoreWriter);

            processStoreWriter.AppendCompositionLogRecord(compositionLogEvent);

            var projectedFileStoreReader = new DelegatingFileStoreReader
            {
                GetFileContentDelegate = filePath =>
                {
                    if (projectedFiles.TryGetValue(filePath, out var projectFileContent))
                        return projectFileContent;

                    return originalFileStore.GetFileContent(filePath);
                },
                ListFilesInDirectoryDelegate = originalFileStore.ListFilesInDirectory,
            };

            return (
                projectedFiles: projectedFiles.Select(filePathAndContent => (filePathAndContent.Key, filePathAndContent.Value)),
                projectedReader: projectedFileStoreReader);
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
        public Func<CompositionLogRecordInFile.CompositionEvent, (byte[] recordHash, string recordHashBase16)> AppendCompositionLogRecordDelegate;

        public Action<Composition.Component> StoreComponentDelegate;

        public Action<ProvisionalReductionRecordInFile> StoreProvisionalReductionDelegate;

        public (byte[] recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent) =>
            AppendCompositionLogRecordDelegate(compositionEvent);

        public void StoreComponent(Composition.Component component) =>
            StoreComponentDelegate(component);

        public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction) =>
            StoreProvisionalReductionDelegate(reduction);
    }

    public class ValueInFileStructure
    {
        public string HashBase16;

        public string LiteralStringUtf8;
    }

    public class CompositionLogRecordInFile
    {
        static public string CompositionLogFirstRecordParentHashBase16 => null;

        static public string HashBase16FromCompositionRecord(byte[] compositionRecord) =>
            CommonConversion.StringBase16FromByteArray(HashFromCompositionRecord(compositionRecord));

        static public byte[] HashFromCompositionRecord(byte[] compositionRecord) =>
            Composition.GetHash(Composition.Component.Blob(compositionRecord));

        public string parentHashBase16;

        public CompositionEvent compositionEvent;

        public class CompositionEvent
        {
            [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
            public ValueInFileStructure UpdateElmAppStateForEvent;

            [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
            public ValueInFileStructure SetElmAppState;

            [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
            public ValueInFileStructure DeployAppConfigAndInitElmAppState;

            [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
            public ValueInFileStructure DeployAppConfigAndMigrateElmAppState;

            [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
            public ValueInFileStructure RevertProcessTo;

            static public CompositionEvent EventForDeployAppConfig(
                ValueInFileStructure appConfigValueInFile,
                bool initElmAppState) =>
                initElmAppState
                ?
                new CompositionEvent
                {
                    DeployAppConfigAndInitElmAppState = appConfigValueInFile,
                }
                :
                new CompositionEvent
                {
                    DeployAppConfigAndMigrateElmAppState = appConfigValueInFile,
                };
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
        static readonly protected IEnumerable<byte> compositionLogEntryDelimiter = new byte[] { 10 };

        static public JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static protected IImmutableList<string> CompositionHeadHashFilePath =>
            ImmutableList.Create("composition-log-head-hash");

        /// <summary>
        /// Use the 'literal' name to distinguish from other possible (future) representations, such as 'deflated'.
        /// </summary>
        static readonly protected IImmutableList<string> CompositionLogLiteralPath = ImmutableList.Create("composition-log", "literal");

        /*
        Distinguish literals from other kinds of representations. (derivations/recipes)
        We can also distinguish between representations in the 'literal' class: I see two extremes here: On one end is the same that we currently use for hashing. The opposite end is integrating the transitive hull of dependencies.
        */
        static protected string LiteralElementSubdirectory => "literal-element";

        static protected string DeflatedLiteralElementSubdirectory => "deflated-literal-element";

        static protected string ProvisionalReductionSubdirectory => "provisional-reduction";


        static readonly protected JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

        static public IImmutableList<string> GetFilePathForComponentInComponentFileStore(string componentHash) =>
            ImmutableList.Create(componentHash.Substring(0, 2), componentHash);

        static readonly IComparer<IImmutableList<string>> CompositionLogFileOrderPathComparer = EnumerableExtension.Comparer<IImmutableList<string>>();

        static protected IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesPaths) =>
            logFilesPaths?.OrderBy(filePath => filePath, CompositionLogFileOrderPathComparer);

        static public byte[] Serialize(CompositionLogRecordInFile record) =>
            Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(record, recordSerializationSettings));

        public ProcessStoreInFileStore()
        {
        }
    }

    public class ProcessStoreReaderInFileStore : ProcessStoreInFileStore, IProcessStoreReader
    {
        protected IFileStoreReader fileStore;

        protected IFileStoreReader LiteralElementFileStore => fileStore.ForSubdirectory(LiteralElementSubdirectory);

        protected IFileStoreReader DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

        protected IFileStoreReader ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

        protected IFileStoreReader CompositionLogLiteralFileStore => fileStore.ForSubdirectory(CompositionLogLiteralPath);

        public ProcessStoreReaderInFileStore(IFileStoreReader fileStore)
        {
            this.fileStore = fileStore;
        }

        IReadOnlyList<byte> LoadComponentSerialRepresentationForHash(IReadOnlyList<byte> componentHash) =>
            LoadComponentSerialRepresentationForHash(CommonConversion.StringBase16FromByteArray(componentHash));

        IReadOnlyList<byte> LoadComponentSerialRepresentationForHash(string componentHashBase16)
        {
            var filePath =
                GetFilePathForComponentInComponentFileStore(componentHashBase16);

            var originalFile = LiteralElementFileStore.GetFileContent(filePath);

            if (originalFile == null)
            {
                var deflatedFile = DeflatedLiteralElementFileStore.GetFileContent(filePath);

                if (deflatedFile != null)
                    return CommonConversion.Inflate(deflatedFile);
            }

            return originalFile;
        }

        public Composition.Component LoadComponent(string componentHashBase16)
        {
            var fromComponentStore = LoadComponentSerialRepresentationForHash(componentHashBase16);

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

            var fileContent = ProvisionalReductionFileStore.GetFileContent(filePath);

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
                        Encoding.UTF8.GetString((fileContent as byte[] ?? fileContent.ToArray()).AsSpan(payloadStartIndex)));

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
            ProvisionalReductionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
            .Select(Enumerable.Last);

        public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse() =>
            EnumerateSerializedCompositionLogRecordsWithFilePathReverse()
            .Select(filePathAndRecord => filePathAndRecord.record);

        public IEnumerable<(IImmutableList<string> filePath, byte[] record)> EnumerateSerializedCompositionLogRecordsWithFilePathReverse() =>
            EnumerateSerializedCompositionLogRecordsReverse_Beginning_2021_07()
            .Concat(EnumerateSerializedCompositionLogRecordsReverse_Before_2021_07()
                .Select<byte[], (IImmutableList<string> filePath, byte[] record)>(record => (null, record)));

        public IEnumerable<(IImmutableList<string>, byte[])> EnumerateSerializedCompositionLogRecordsReverse_Beginning_2021_07()
        {
            var compositionLogFilesReversed =
                CompositionLogFileOrder(CompositionLogLiteralFileStore.ListFiles())
                .Reverse()
                .ToImmutableList();

            var sequenceBeforeConsideringRevertEvent =
                compositionLogFilesReversed
                .SelectMany(filePath => SplitFileContentIntoCompositionLogRecords(
                    CompositionLogLiteralFileStore.GetFileContent(filePath)).Select(record => (filePath, record)).Reverse());

            string revertToHashBase16 = null;

            foreach (var recordFilePathAndContent in sequenceBeforeConsideringRevertEvent)
            {
                var recordAsArray = recordFilePathAndContent.record.ToArray();
                var recordHash = CompositionLogRecordInFile.HashBase16FromCompositionRecord(recordAsArray);

                if (revertToHashBase16 != null)
                {
                    if (recordHash != revertToHashBase16)
                        continue;

                    revertToHashBase16 = null;
                }

                yield return (recordFilePathAndContent.filePath, recordAsArray);

                var recordAsString = Encoding.UTF8.GetString(recordAsArray);

                var recordStruct = JsonConvert.DeserializeObject<CompositionLogRecordInFile>(recordAsString);

                if (recordStruct.compositionEvent.RevertProcessTo != null)
                {
                    revertToHashBase16 = recordStruct.compositionEvent.RevertProcessTo.HashBase16;
                }
            }
        }

        /// <summary>
        /// Drop content after the last occurrence of delimiter sequence to account for the possible partial write of the last composition record.
        /// </summary>
        static IEnumerable<IReadOnlyList<byte>> SplitFileContentIntoCompositionLogRecords(IReadOnlyList<byte> fileContent)
        {
            if (fileContent == null)
                yield break;

            var recordBegin = 0;

            for (var i = 0; i < fileContent.Count; ++i)
            {
                if (compositionLogEntryDelimiter.SequenceEqual(fileContent.Skip(i).Take(compositionLogEntryDelimiter.Count())))
                {
                    var recordBytes = fileContent.Take(i).Skip(recordBegin).ToArray();

                    yield return recordBytes;

                    i += compositionLogEntryDelimiter.Count();

                    recordBegin = i;
                }
            }
        }

        public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse_Before_2021_07()
        {
            var compositionHeadHash = fileStore.GetFileContent(CompositionHeadHashFilePath);

            if (compositionHeadHash == null)
                yield break;

            if (1000 < compositionHeadHash.Count)
                throw new Exception("Content of file for head hash is corrupted: File length is " + compositionHeadHash.Count);

            var nextHashBase16 = CommonConversion.StringBase16FromByteArray(compositionHeadHash);

            while (nextHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
            {
                var compositionRecordComponent = LoadComponent(nextHashBase16);

                if (compositionRecordComponent == null)
                    throw new Exception("Failed to load composition record component " + nextHashBase16);

                if (compositionRecordComponent.BlobContent == null)
                    throw new Exception("Unexpected content for composition record component " + nextHashBase16);

                var compositionRecordArray = compositionRecordComponent.BlobContent;

                var recordStructure = JsonConvert.DeserializeObject<CompositionLogRecordInFile>(
                    Encoding.UTF8.GetString(compositionRecordArray));

                yield return compositionRecordArray;

                if (recordStructure.compositionEvent?.RevertProcessTo != null)
                {
                    nextHashBase16 = recordStructure.compositionEvent.RevertProcessTo.HashBase16;
                }
                else
                {
                    nextHashBase16 = recordStructure.parentHashBase16?.ToLowerInvariant();
                }
            }
        }
    }

    public class ProcessStoreWriterInFileStore : ProcessStoreInFileStore, IProcessStoreWriter
    {
        static int TryDeflateSizeThreshold => 10_000;

        protected IFileStoreWriter fileStore;

        protected IFileStoreWriter LiteralElementFileStore => fileStore.ForSubdirectory(LiteralElementSubdirectory);

        protected IFileStoreWriter DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

        protected IFileStoreWriter ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

        protected IFileStoreWriter CompositionLogLiteralFileStore => fileStore.ForSubdirectory(CompositionLogLiteralPath);

        readonly Func<DateTimeOffset> getTimeForCompositionLogBatch;

        readonly object appendLock = new object();

        (string hashBase16, IImmutableList<string> filePath)? lastCompositionRecord;

        public ProcessStoreWriterInFileStore(
            IFileStoreReader fileStoreReader,
            Func<DateTimeOffset> getTimeForCompositionLogBatch,
            IFileStoreWriter fileStore)
        {
            this.getTimeForCompositionLogBatch = getTimeForCompositionLogBatch;
            this.fileStore = fileStore;

            var originalProcessStoreReader = new ProcessStoreReaderInFileStore(fileStoreReader);

            var originalStoreLastCompositionRecord =
                originalProcessStoreReader.EnumerateSerializedCompositionLogRecordsWithFilePathReverse().FirstOrDefault();

            lastCompositionRecord =
                originalStoreLastCompositionRecord.record == null
                ?
                (CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16, null)
                :
                (CompositionLogRecordInFile.HashBase16FromCompositionRecord(originalStoreLastCompositionRecord.record), originalStoreLastCompositionRecord.filePath);
        }

        public (byte[] recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent)
        {
            lock (appendLock)
            {
                var compositionLogRecordStructure = new CompositionLogRecordInFile
                {
                    parentHashBase16 = lastCompositionRecord?.hashBase16,
                    compositionEvent = compositionEvent,
                };

                var dateTime = getTimeForCompositionLogBatch();

                var dayDirectoryName = dateTime.ToString("yyyy-MM-dd");
                var fileName = dateTime.ToString("yyyy-MM-ddTHH-mm");

                var filePath =
                    CompositionLogFileOrder(
                        new[]
                        {
                            lastCompositionRecord?.filePath,
                            ImmutableList.Create(dayDirectoryName, fileName),
                        }.Where(p => p != null))
                    .Last();

                var compositionLogRecordSerialized = GetCompositionLogRecordSerialized(compositionLogRecordStructure);

                var recordHash = CompositionLogRecordInFile.HashFromCompositionRecord(compositionLogRecordSerialized);
                var recordHashBase16 = CommonConversion.StringBase16FromByteArray(recordHash);

                var compositionLogRecordSerializedWithDelimiter =
                    compositionLogRecordSerialized.Concat(compositionLogEntryDelimiter)
                    .ToArray();

                CompositionLogLiteralFileStore.AppendFileContent(filePath, compositionLogRecordSerializedWithDelimiter);

                lastCompositionRecord = (recordHashBase16, filePath);

                return (recordHash, recordHashBase16);
            }
        }

        private static byte[] GetCompositionLogRecordSerialized(CompositionLogRecordInFile compositionLogRecordStructure)
        {
            return Serialize(compositionLogRecordStructure);
        }

        public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reductionRecord)
        {
            var fileName = reductionRecord.reducedCompositionHashBase16;

            var filePath = ImmutableList.Create(fileName);

            ProvisionalReductionFileStore.SetFileContent(
                filePath, Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(reductionRecord, recordSerializationSettings)));
        }

        public void StoreComponent(Composition.Component component)
        {
            StoreComponentAndGetHash(component);
        }

        (byte[] hash, string hashBase16) StoreComponentAndGetHash(Composition.Component component)
        {
            var (serialRepresentation, dependencies) = Composition.GetSerialRepresentationAndDependencies(component);

            var hash = CommonConversion.HashSHA256(serialRepresentation);

            var hashBase16 = CommonConversion.StringBase16FromByteArray(hash);

            void storeSelf()
            {
                if (TryDeflateSizeThreshold <= serialRepresentation.Length)
                {
                    var deflated = CommonConversion.Deflate(serialRepresentation);

                    if (deflated.Length * 10 < serialRepresentation.Length * 8)
                    {
                        DeflatedLiteralElementFileStore.SetFileContent(
                            GetFilePathForComponentInComponentFileStore(hashBase16),
                            deflated);

                        return;
                    }
                }

                LiteralElementFileStore.SetFileContent(
                    GetFilePathForComponentInComponentFileStore(hashBase16),
                    serialRepresentation);
            }

            storeSelf();

            foreach (var dependency in dependencies)
                StoreComponent(dependency);

            return (hash, hashBase16);
        }
    }
}
