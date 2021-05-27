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
        (byte[] recordHash, string recordHashBase16) SetCompositionLogHeadRecord(byte[] serializedCompositionRecord);

        void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction);

        void StoreComponent(Composition.Component component);
    }

    public interface IProcessStoreReader
    {
        IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse();

        ProvisionalReductionRecordInFile LoadProvisionalReduction(string reducedCompositionLogRecordHash);

        Composition.Component LoadComponent(string componentHash);

        static public (string parentHashBase16, IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> projectedFiles, IFileStoreReader projectedReader)
            ProjectFileStoreReaderForAppendedCompositionLogEvent(
            IFileStoreReader originalFileStore,
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
        {
            var originalProcessStoreReader = new ProcessStoreReaderInFileStore(originalFileStore);

            var originalStoreLastCompositionRecord =
                originalProcessStoreReader.EnumerateSerializedCompositionLogRecordsReverse().FirstOrDefault();

            var parentHashBase16 =
                originalStoreLastCompositionRecord == null
                ?
                CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16
                :
                CompositionLogRecordInFile.HashBase16FromCompositionRecord(originalStoreLastCompositionRecord);

            var compositionLogRecordStructure = new ProcessStoreSupportingMigrations.CompositionLogRecordInFile
            {
                parentHashBase16 = parentHashBase16,
                compositionEvent = compositionLogEvent,
            };

            var compositionLogRecordSerialized = ProcessStoreInFileStore.Serialize(compositionLogRecordStructure);

            var projectedFiles = new List<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)>();

            var fileStoreWriter = new DelegatingFileStoreWriter
            {
                SetFileContentDelegate = projectedFiles.Add,
                AppendFileContentDelegate = _ => throw new Exception("Unexpected operation append to file."),
                DeleteFileDelegate = _ => throw new Exception("Unexpected operation delete file."),
            };

            var processStoreWriter = new ProcessStoreWriterInFileStore(fileStoreWriter);

            processStoreWriter.SetCompositionLogHeadRecord(compositionLogRecordSerialized);

            var projectedFileStoreReader = new DelegatingFileStoreReader
            {
                GetFileContentDelegate = filePath =>
                {
                    var projectedFilePathAndContent = projectedFiles.FirstOrDefault(c => c.filePath.SequenceEqual(filePath));

                    if (projectedFilePathAndContent.filePath?.SequenceEqual(filePath) ?? false)
                        return projectedFilePathAndContent.fileContent;

                    return originalFileStore.GetFileContent(filePath);
                },
                ListFilesInDirectoryDelegate = originalFileStore.ListFilesInDirectory,
            };

            return (parentHashBase16, projectedFiles, projectedReader: projectedFileStoreReader);
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
        public Func<byte[], (byte[] recordHash, string recordHashBase16)> SetCompositionLogHeadRecordDelegate;

        public Action<Composition.Component> StoreComponentDelegate;

        public Action<ProvisionalReductionRecordInFile> StoreProvisionalReductionDelegate;

        public (byte[] recordHash, string recordHashBase16) SetCompositionLogHeadRecord(byte[] serializedCompositionRecord) =>
            SetCompositionLogHeadRecordDelegate(serializedCompositionRecord);

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
        static public string CompositionLogFirstRecordParentHashBase16 => null;

        static public string HashBase16FromCompositionRecord(byte[] compositionRecord) =>
            CommonConversion.StringBase16FromByteArray(Composition.GetHash(Composition.Component.Blob(compositionRecord)));

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
        static public Newtonsoft.Json.JsonSerializerSettings RecordSerializationSettings => new JsonSerializerSettings
        {
            NullValueHandling = NullValueHandling.Ignore
        };

        static protected IImmutableList<string> CompositionHeadHashFilePath =>
            ImmutableList.Create("composition-log-head-hash");

        /*
        Distinguish literals from other kinds of representations. (derivations/recipes)
        We can also distinguish between representations in the 'literal' class: I see two extremes here: On one end is the same that we currently use for hashing. The opposite end is integrating the transitive hull of dependencies.
        */
        static protected string LiteralElementSubdirectory => "literal-element";

        // TODO: Remove 'ComponentSubdirectory' to complete migration.
        static protected string ComponentSubdirectory => "component";

        static protected string DeflatedLiteralElementSubdirectory => "deflated-literal-element";

        // TODO: Remove 'DeflatedComponentSubdirectory' to complete migration.
        static protected string DeflatedComponentSubdirectory => "deflated-component";

        static protected string ProvisionalReductionSubdirectory => "provisional-reduction";


        static readonly protected Newtonsoft.Json.JsonSerializerSettings recordSerializationSettings = RecordSerializationSettings;

        static public IImmutableList<string> GetFilePathForComponentInComponentFileStore(string componentHash) =>
            ImmutableList.Create(componentHash.Substring(0, 2), componentHash);

        static protected IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesNames) =>
            logFilesNames?.OrderBy(filePath => string.Join("", filePath));

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

        //  Plain Pine component.
        protected IFileStoreReader ComponentFileStore => fileStore.ForSubdirectory(ComponentSubdirectory);

        protected IFileStoreReader DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

        protected IFileStoreReader DeflatedComponentFileStore => fileStore.ForSubdirectory(DeflatedComponentSubdirectory);

        protected IFileStoreReader ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

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

            var originalFile =
                LiteralElementFileStore.GetFileContent(filePath) ??
                ComponentFileStore.GetFileContent(filePath);

            if (originalFile == null)
            {
                var deflatedFile =
                    DeflatedLiteralElementFileStore.GetFileContent(filePath) ??
                    DeflatedComponentFileStore.GetFileContent(filePath);

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

        public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse()
        {
            var compositionHeadHash = fileStore.GetFileContent(CompositionHeadHashFilePath);

            if (compositionHeadHash == null)
                yield break;

            var nextHashBase16 = CommonConversion.StringBase16FromByteArray(compositionHeadHash);

            while (nextHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16)
            {
                var compositionRecordComponent = LoadComponent(nextHashBase16);

                if (compositionRecordComponent == null)
                    throw new Exception("Failed to load composition record component " + nextHashBase16);

                if (compositionRecordComponent.BlobContent == null)
                    throw new Exception("Unexpected content for composition record component " + nextHashBase16);

                var compositionRecordArray = compositionRecordComponent.BlobContent.ToArray();

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

        //  Plain Kalmit component.
        protected IFileStoreWriter ComponentFileStore => fileStore.ForSubdirectory(ComponentSubdirectory);

        protected IFileStoreWriter DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

        protected IFileStoreWriter DeflatedComponentFileStore => fileStore.ForSubdirectory(DeflatedComponentSubdirectory);

        protected IFileStoreWriter ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

        public ProcessStoreWriterInFileStore(IFileStoreWriter fileStore)
        {
            this.fileStore = fileStore;
        }

        public (byte[] recordHash, string recordHashBase16) SetCompositionLogHeadRecord(byte[] record)
        {
            var recordHash = StoreComponentAndGetHash(Composition.Component.Blob(record));

            fileStore.SetFileContent(CompositionHeadHashFilePath, recordHash.hash);

            return recordHash;
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
