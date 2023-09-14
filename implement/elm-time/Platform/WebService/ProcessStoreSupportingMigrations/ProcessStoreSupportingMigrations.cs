using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;

public record FileStoreReaderProjectionResult(
    IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> projectedFiles,
    IFileStoreReader projectedReader);

public interface IProcessStoreWriter
{
    (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent);

    void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction);

    void StoreComponent(PineValue component);
}

public interface IProcessStoreReader
{
    IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse();

    ProvisionalReductionRecordInFile? LoadProvisionalReduction(string reducedCompositionLogRecordHash);

    PineValue? LoadComponent(string componentHash);

    public static FileStoreReaderProjectionResult
        ProjectFileStoreReaderForAppendedCompositionLogEvent(
        IFileStoreReader originalFileStore,
        CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
    {
        var projectedFiles =
            new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>(
                comparer: EnumerableExtension.EqualityComparer<IImmutableList<string>>());

        var fileStoreWriter = new DelegatingFileStoreWriter
        (
            SetFileContentDelegate: pathAndFileContent => projectedFiles[pathAndFileContent.path] = pathAndFileContent.fileContent,
            AppendFileContentDelegate: pathAndFileContent =>
            {
                if (!projectedFiles.TryGetValue(pathAndFileContent.path, out var fileContentBefore))
                    fileContentBefore = originalFileStore.GetFileContent(pathAndFileContent.path) ?? ReadOnlyMemory<byte>.Empty;

                projectedFiles[pathAndFileContent.path] =
                CommonConversion.Concat(fileContentBefore.Span, pathAndFileContent.fileContent.Span);
            },
            DeleteFileDelegate: _ => throw new Exception("Unexpected operation delete file.")
        );

        var processStoreWriter = new ProcessStoreWriterInFileStore(
            originalFileStore,
            getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
            fileStoreWriter);

        processStoreWriter.AppendCompositionLogRecord(compositionLogEvent);

        var projectedFileStoreReader = new DelegatingFileStoreReader
        (
            GetFileContentDelegate: filePath =>
            {
                if (projectedFiles.TryGetValue(filePath, out var projectFileContent))
                    return projectFileContent;

                return originalFileStore.GetFileContent(filePath);
            },
            ListFilesInDirectoryDelegate: directoryPath =>
            {
                var fromProjectedFiles =
                    projectedFiles.Keys
                    .SelectWhereNotNull(projectedFilePath =>
                        projectedFilePath.Take(directoryPath.Count).SequenceEqual(directoryPath) ?
                        projectedFilePath.RemoveRange(0, directoryPath.Count)
                        :
                        null);

                return
                    originalFileStore.ListFilesInDirectory(directoryPath).Concat(fromProjectedFiles)
                    .Distinct(EnumerableExtension.EqualityComparer<IImmutableList<string>>());
            }
        );

        return new FileStoreReaderProjectionResult(
            projectedFiles: projectedFiles.Select(filePathAndContent => (filePathAndContent.Key, filePathAndContent.Value)),
            projectedReader: projectedFileStoreReader);
    }

    public static IProcessStoreReader EmptyProcessStoreReader()
    {
        return new DelegatingProcessStoreReader
        (
            LoadComponentDelegate: _ => null,
            LoadProvisionalReductionDelegate: _ => null,
            EnumerateSerializedCompositionLogRecordsReverseDelegate: () => ImmutableList<byte[]>.Empty
        );
    }
}

internal record DelegatingProcessStoreReader(
    Func<IEnumerable<byte[]>> EnumerateSerializedCompositionLogRecordsReverseDelegate,
    Func<string, PineValue?> LoadComponentDelegate,
    Func<string, ProvisionalReductionRecordInFile?> LoadProvisionalReductionDelegate) : IProcessStoreReader
{
    public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse() =>
        EnumerateSerializedCompositionLogRecordsReverseDelegate();

    public PineValue? LoadComponent(string componentHash) =>
        LoadComponentDelegate(componentHash);

    public ProvisionalReductionRecordInFile? LoadProvisionalReduction(string reducedCompositionLogRecordHash) =>
        LoadProvisionalReductionDelegate(reducedCompositionLogRecordHash);
}

public record DelegatingProcessStoreWriter(
    Func<CompositionLogRecordInFile.CompositionEvent, (ReadOnlyMemory<byte> recordHash, string recordHashBase16)> AppendCompositionLogRecordDelegate,
    Action<PineValue> StoreComponentDelegate,
    Action<ProvisionalReductionRecordInFile> StoreProvisionalReductionDelegate) : IProcessStoreWriter
{
    public (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent) =>
        AppendCompositionLogRecordDelegate(compositionEvent);

    public void StoreComponent(PineValue component) =>
        StoreComponentDelegate(component);

    public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction) =>
        StoreProvisionalReductionDelegate(reduction);
}

public record ValueInFileStructure(
    string? HashBase16 = null,
    string? LiteralStringUtf8 = null);

public record CompositionLogRecordInFile(
    string? parentHashBase16,
    CompositionLogRecordInFile.CompositionEvent compositionEvent)
{
    public static string? CompositionLogFirstRecordParentHashBase16 => null;

    public static string HashBase16FromCompositionRecord(byte[] compositionRecord) =>
        CommonConversion.StringBase16(HashFromCompositionRecord(compositionRecord));

    public static ReadOnlyMemory<byte> HashFromCompositionRecord(byte[] compositionRecord) =>
        PineValueHashTree.ComputeHash(PineValue.Blob(compositionRecord));

    public record CompositionEvent(
        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? UpdateElmAppStateForEvent = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? ApplyFunctionOnElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? SetElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? DeployAppConfigAndInitElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? DeployAppConfigAndMigrateElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? RevertProcessTo = null)
    {
        public static CompositionEvent EventForDeployAppConfig(
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

    public record ApplyFunctionOnStateEvent(
        string functionName,
        IReadOnlyList<string> serializedArgumentsJson);
}

public record ProvisionalReductionRecordInFile(
    string reducedCompositionHashBase16,
    ValueInFileStructure? elmAppState,
    ValueInFileStructure? appConfig);

public class ProcessStoreInFileStore
{
    protected static readonly ReadOnlyMemory<byte> compositionLogEntryDelimiter = "\n"u8.ToArray();

    public static JsonSerializerOptions RecordSerializationSettings => new()
    {
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
    };

    protected static ImmutableList<string> CompositionHeadHashFilePath =>
        ["composition-log-head-hash"];

    /// <summary>
    /// Use the 'literal' name to distinguish from other possible (future) representations, such as 'deflated'.
    /// </summary>
    protected static readonly IReadOnlyList<string> CompositionLogLiteralPath = ["composition-log", "literal"];

    /*
    Distinguish literals from other kinds of representations. (derivations/recipes)
    We can also distinguish between representations in the 'literal' class: I see two extremes here: On one end is the same that we currently use for hashing. The opposite end is integrating the transitive hull of dependencies.
    */
    protected static string LiteralElementSubdirectory => "literal-element";

    protected static string DeflatedLiteralElementSubdirectory => "deflated-literal-element";

    protected static string ProvisionalReductionSubdirectory => "provisional-reduction";


    protected static readonly JsonSerializerOptions recordSerializationSettings = RecordSerializationSettings;

    public static IImmutableList<string> GetFilePathForComponentInComponentFileStore(string componentHash) =>
        ImmutableList.Create(componentHash[..2], componentHash);

    private static readonly IComparer<IImmutableList<string>> CompositionLogFileOrderPathComparer = EnumerableExtension.Comparer<IImmutableList<string>>();

    protected static IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesPaths) =>
        logFilesPaths.OrderBy(filePath => filePath, CompositionLogFileOrderPathComparer);

    public static byte[] Serialize(CompositionLogRecordInFile record) =>
        Encoding.UTF8.GetBytes(JsonSerializer.Serialize(record, recordSerializationSettings));

    public ProcessStoreInFileStore()
    {
    }
}

public class ProcessStoreReaderInFileStore(
    IFileStoreReader fileStore)
    : ProcessStoreInFileStore, IProcessStoreReader
{
    protected IFileStoreReader LiteralElementFileStore => fileStore.ForSubdirectory(LiteralElementSubdirectory);

    protected IFileStoreReader DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

    protected IFileStoreReader ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

    protected IFileStoreReader CompositionLogLiteralFileStore => fileStore.ForSubdirectory(CompositionLogLiteralPath);

    private ReadOnlyMemory<byte>? LoadComponentSerialRepresentationForHash(ReadOnlyMemory<byte> componentHash) =>
        LoadComponentSerialRepresentationForHash(CommonConversion.StringBase16(componentHash));

    private ReadOnlyMemory<byte>? LoadComponentSerialRepresentationForHash(string componentHashBase16)
    {
        var filePath =
            GetFilePathForComponentInComponentFileStore(componentHashBase16);

        var originalFile = LiteralElementFileStore.GetFileContent(filePath);

        if (originalFile == null)
        {
            var deflatedFile = DeflatedLiteralElementFileStore.GetFileContent(filePath);

            if (deflatedFile is not null)
                return CommonConversion.Inflate(deflatedFile.Value);
        }

        return originalFile;
    }

    public PineValue? LoadComponent(string componentHashBase16)
    {
        var fromComponentStore = LoadComponentSerialRepresentationForHash(componentHashBase16);

        if (fromComponentStore == null)
            return null;

        return
            PineValueHashTree.DeserializeFromHashTree(fromComponentStore.Value, LoadComponentSerialRepresentationForHash)
            .Unpack(
                fromErr: error => throw new Exception("Failed to load component " + componentHashBase16 + ": " + error),
                fromOk: loadComponentResult =>
                {
                    if (CommonConversion.StringBase16(PineValueHashTree.ComputeHash(loadComponentResult)) != componentHashBase16)
                        throw new Exception("Unexpected content in file " + componentHashBase16 + ": Content hash does not match.");

                    return loadComponentResult;
                });
    }

    public ProvisionalReductionRecordInFile? LoadProvisionalReduction(string reducedCompositionHash)
    {
        var filePath = ImmutableList.Create(reducedCompositionHash);

        var fileContent = ProvisionalReductionFileStore.GetFileContent(filePath);

        if (fileContent is null)
            return null;

        try
        {
            var payloadStartIndex =
                /*
                Previous implementation used `File.WriteAllText`:
                https://github.com/elm-time/elm-time/blob/1cd3f00bdf5a05e9bda479c534b0458b2496393c/implement/PersistentProcess/PersistentProcess.Common/ProcessStore.cs#L183
                Looking at the files from stores in production, it seems like that caused addition of BOM.
                */
                fileContent.Value.Span.StartsWith(new byte[] { 0xEF, 0xBB, 0xBF })
                ?
                3
                :
                0;

            var reductionRecordFromFile =
                JsonSerializer.Deserialize<ProvisionalReductionRecordInFile>(fileContent.Value[payloadStartIndex..].Span)!;

            if (reducedCompositionHash != reductionRecordFromFile.reducedCompositionHashBase16)
                throw new Exception("Unexpected content in file " + string.Join("/", filePath) + ", composition hash does not match.");

            return reductionRecordFromFile;
        }
        catch (Exception e)
        {
            throw new Exception(
                "Failed to read reduction from file '" + string.Join("/", filePath) + "' (" + fileContent.Value.Length + " bytes)",
                e);
        }
    }

    public IEnumerable<string> ReductionsFilesNames() =>
        ProvisionalReductionFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
        .Select(Enumerable.Last);

    public IEnumerable<byte[]> EnumerateSerializedCompositionLogRecordsReverse() =>
        EnumerateSerializedCompositionLogRecordsWithFilePathReverse()
        .Select(filePathAndRecord => filePathAndRecord.record);

    public IEnumerable<(IImmutableList<string> filePath, byte[] record)> EnumerateSerializedCompositionLogRecordsWithFilePathReverse() =>
        EnumerateSerializedCompositionLogRecordsReverse_Beginning_2021_07();

    public IEnumerable<(IImmutableList<string>, byte[])> EnumerateSerializedCompositionLogRecordsReverse_Beginning_2021_07()
    {
        var compositionLogFilesReversed =
            CompositionLogFileOrder(CompositionLogLiteralFileStore.ListFiles())
            .Reverse()
            .ToImmutableList();

        var sequenceBeforeConsideringRevertEvent =
            compositionLogFilesReversed
            .SelectMany(filePath =>
            {
                var fileContent = CompositionLogLiteralFileStore.GetFileContent(filePath);

                if (fileContent == null)
                    return ImmutableList<(IImmutableList<string>, ReadOnlyMemory<byte>)>.Empty;

                return
                    SplitFileContentIntoCompositionLogRecords(fileContent.Value).Select(record => (filePath, record)).Reverse();
            });

        string? revertToHashBase16 = null;

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

            var recordStruct = JsonSerializer.Deserialize<CompositionLogRecordInFile>(recordAsString)!;

            if (recordStruct.compositionEvent.RevertProcessTo != null)
            {
                revertToHashBase16 = recordStruct.compositionEvent.RevertProcessTo.HashBase16;
            }
        }
    }

    /// <summary>
    /// Drop content after the last occurrence of delimiter sequence to account for the possible partial write of the last composition record.
    /// </summary>
    private static IEnumerable<ReadOnlyMemory<byte>> SplitFileContentIntoCompositionLogRecords(ReadOnlyMemory<byte> fileContent)
    {
        var recordBegin = 0;

        for (var i = 0; i < fileContent.Length; ++i)
        {
            if (fileContent[i..].Span.StartsWith(compositionLogEntryDelimiter.Span))
            {
                var recordBytes = fileContent[recordBegin..i];

                yield return recordBytes;

                i += compositionLogEntryDelimiter.Length;

                recordBegin = i;
            }
        }
    }

    public IEnumerable<ReadOnlyMemory<byte>> EnumerateSerializedCompositionLogRecordsReverse_Before_2021_07()
    {
        var compositionHeadHash = fileStore.GetFileContent(CompositionHeadHashFilePath);

        if (compositionHeadHash == null)
            yield break;

        if (1000 < compositionHeadHash.Value.Length)
            throw new Exception("Content of file for head hash is corrupted: File length is " + compositionHeadHash.Value.Length);

        var nextHashBase16 = CommonConversion.StringBase16(compositionHeadHash.Value);

        while (nextHashBase16 != CompositionLogRecordInFile.CompositionLogFirstRecordParentHashBase16 && nextHashBase16 != null)
        {
            var compositionRecordComponent = LoadComponent(nextHashBase16);

            if (compositionRecordComponent == null)
                throw new Exception("Failed to load composition record component " + nextHashBase16);

            if (compositionRecordComponent is not PineValue.BlobValue compositionRecordComponentBlob)
                throw new Exception("Unexpected content for composition record component " + nextHashBase16);

            var compositionRecordBytes = compositionRecordComponentBlob.Bytes;

            yield return compositionRecordBytes;

            var recordStructure = JsonSerializer.Deserialize<CompositionLogRecordInFile>(
                Encoding.UTF8.GetString(compositionRecordBytes.Span))!;

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
    private static int TryDeflateSizeThreshold => 10_000;

    protected IFileStoreWriter fileStore;

    protected IFileStoreWriter LiteralElementFileStore => fileStore.ForSubdirectory(LiteralElementSubdirectory);

    protected IFileStoreWriter DeflatedLiteralElementFileStore => fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

    protected IFileStoreWriter ProvisionalReductionFileStore => fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

    protected IFileStoreWriter CompositionLogLiteralFileStore => fileStore.ForSubdirectory(CompositionLogLiteralPath);

    private readonly Func<DateTimeOffset> getTimeForCompositionLogBatch;

    private readonly object appendLock = new();

    private (string hashBase16, IImmutableList<string> filePath)? lastCompositionRecord;

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
            null
            :
            (CompositionLogRecordInFile.HashBase16FromCompositionRecord(originalStoreLastCompositionRecord.record), originalStoreLastCompositionRecord.filePath);
    }

    public (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(
        CompositionLogRecordInFile.CompositionEvent compositionEvent)
    {
        lock (appendLock)
        {
            var compositionLogRecordStructure = new CompositionLogRecordInFile
            (
                parentHashBase16: lastCompositionRecord?.hashBase16,
                compositionEvent: compositionEvent
            );

            var dateTime = getTimeForCompositionLogBatch();

            var dayDirectoryName = dateTime.ToString("yyyy-MM-dd");
            var fileName = dateTime.ToString("yyyy-MM-ddTHH-mm");

            var filePath =
                CompositionLogFileOrder(
                    new[]
                    {
                        lastCompositionRecord?.filePath,
                        ImmutableList.Create(dayDirectoryName, fileName),
                    }.WhereNotNull())
                .Last();

            var compositionLogRecordSerialized = GetCompositionLogRecordSerialized(compositionLogRecordStructure);

            var recordHash = CompositionLogRecordInFile.HashFromCompositionRecord(compositionLogRecordSerialized);
            var recordHashBase16 = CommonConversion.StringBase16(recordHash);

            var compositionLogRecordSerializedWithDelimiter =
                CommonConversion.Concat(
                    compositionLogRecordSerialized,
                    compositionLogEntryDelimiter.Span);

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
            filePath,
            Encoding.UTF8.GetBytes(JsonSerializer.Serialize(reductionRecord, recordSerializationSettings)));
    }

    public void StoreComponent(PineValue component)
    {
        StoreComponentAndGetHash(component);
    }

    private (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponentAndGetHash(PineValue component)
    {
        var (serialRepresentation, dependencies) = PineValueHashTree.ComputeHashTreeNodeSerialRepresentation(component);

        var hash = CommonConversion.HashSHA256(serialRepresentation);

        var hashBase16 = CommonConversion.StringBase16(hash);

        void storeSelf()
        {
            if (TryDeflateSizeThreshold <= serialRepresentation.Length)
            {
                var deflated = CommonConversion.Deflate(serialRepresentation);

                if (deflated.Length * 10 < serialRepresentation.Length * 8)
                {
                    DeflatedLiteralElementFileStore.SetFileContent(
                        GetFilePathForComponentInComponentFileStore(hashBase16),
                        deflated.ToArray());

                    return;
                }
            }

            LiteralElementFileStore.SetFileContent(
                GetFilePathForComponentInComponentFileStore(hashBase16),
                serialRepresentation.ToArray());
        }

        storeSelf();

        foreach (var dependency in dependencies)
            StoreComponent(dependency);

        return (hash, hashBase16);
    }
}
