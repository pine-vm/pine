using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.IO;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;

public record FileStoreReaderProjectionResult(
    IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> ProjectedFiles,
    IFileStoreReader ProjectedReader);

public interface IProcessStoreWriter
{
    (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(
        CompositionLogRecordInFile.CompositionEvent compositionEvent);

    void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction);

    (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponent(PineValue component);
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
            new ConcurrentDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>(
                comparer: EnumerableExtensions.EqualityComparer<IImmutableList<string>>());

        var fileStoreWriter = new DelegatingFileStoreWriter
        (
            SetFileContentDelegate:
            pathAndFileContent =>
            projectedFiles[pathAndFileContent.path] = pathAndFileContent.fileContent,

            AppendFileContentDelegate:
            pathAndFileContent =>
            {
                if (!projectedFiles.TryGetValue(pathAndFileContent.path, out var fileContentBefore))
                {
                    fileContentBefore =
                    originalFileStore.GetFileContent(pathAndFileContent.path) ??
                    ReadOnlyMemory<byte>.Empty;
                }

                projectedFiles[pathAndFileContent.path] =
                BytesConversions.Concat(fileContentBefore.Span, pathAndFileContent.fileContent.Span);
            },

            DeleteFileDelegate:
            _ => throw new Exception("Unexpected operation delete file.")
        );

        var processStoreWriter =
            new ProcessStoreWriterInFileStore(
                originalFileStore,
                getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                fileStoreWriter,
                skipWritingComponentSecondTime: true);

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
                    .Distinct(EnumerableExtensions.EqualityComparer<IImmutableList<string>>());
            }
        );

        return new FileStoreReaderProjectionResult(
            ProjectedFiles: projectedFiles.Select(filePathAndContent => (filePathAndContent.Key, filePathAndContent.Value)),
            ProjectedReader: projectedFileStoreReader);
    }

    public static IProcessStoreReader EmptyProcessStoreReader()
    {
        return new DelegatingProcessStoreReader
        (
            LoadComponentDelegate: _ => null,
            LoadProvisionalReductionDelegate: _ => null,
            EnumerateSerializedCompositionLogRecordsReverseDelegate: () => []
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
    Func<PineValue, (ReadOnlyMemory<byte> hash, string hashBase16)> StoreComponentDelegate,
    Action<ProvisionalReductionRecordInFile> StoreProvisionalReductionDelegate) : IProcessStoreWriter
{
    public (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(CompositionLogRecordInFile.CompositionEvent compositionEvent) =>
        AppendCompositionLogRecordDelegate(compositionEvent);

    public (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponent(PineValue component) =>
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

    public static string HashBase16FromCompositionRecord(ReadOnlyMemory<byte> compositionRecord) =>
        Convert.ToHexStringLower(HashFromCompositionRecord(compositionRecord).Span);

    public static ReadOnlyMemory<byte> HashFromCompositionRecord(ReadOnlyMemory<byte> compositionRecord) =>
        PineValueHashTree.ComputeHash(PineValue.Blob(compositionRecord));

    public record CompositionEvent(

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? SetElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? DeployAppConfigAndInitElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    ValueInFileStructure? DeployAppConfigAndMigrateElmAppState = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        ApplyFunctionOnLiteralAndStateEvent? ApplyFunctionOnLiteralAndState = null,

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

    public record ApplyFunctionOnLiteralAndStateEvent(
        ValueInFileStructure Function,
        ValueInFileStructure Arguments);
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

    protected static readonly IReadOnlyList<string> ValueJsonDeflatedSubdirectory =
        ["value", "json", "deflated"];

    protected static readonly IReadOnlyList<string> ValueBinaryDeflatedSubdirectory =
        ["value", "binary", "deflated"];

    protected static string ProvisionalReductionSubdirectory => "provisional-reduction";


    protected static readonly JsonSerializerOptions recordSerializationSettings = RecordSerializationSettings;

    public static IImmutableList<string> GetFilePathForComponentInComponentFileStore(string componentHash) =>
        ImmutableList.Create(componentHash[..2], componentHash);

    private static readonly IComparer<IImmutableList<string>> CompositionLogFileOrderPathComparer =
        EnumerableExtensions.Comparer<IImmutableList<string>>();

    protected static IEnumerable<IImmutableList<string>> CompositionLogFileOrder(
        IEnumerable<IImmutableList<string>> logFilesPaths) =>
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
    private readonly ConcurrentPineValueHashCache _hashCache = new();

    private readonly ConcurrentDictionary<string, ReadOnlyMemory<byte>> _componentSerialRepresentationCache = new();

    private readonly ConcurrentDictionary<string, PineValue> componentFromHashCache = new();

    protected IFileStoreReader LiteralElementFileStore =>
        fileStore.ForSubdirectory(LiteralElementSubdirectory);

    protected IFileStoreReader DeflatedLiteralElementFileStore =>
        fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

    protected IFileStoreReader ValueJsonDeflatedFileStore =>
        fileStore.ForSubdirectory(ValueJsonDeflatedSubdirectory);

    protected IFileStoreReader ValueBinaryDeflatedFileStore =>
        fileStore.ForSubdirectory(ValueBinaryDeflatedSubdirectory);

    protected IFileStoreReader ProvisionalReductionFileStore =>
        fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

    protected IFileStoreReader CompositionLogLiteralFileStore =>
        fileStore.ForSubdirectory(CompositionLogLiteralPath);

    private (string hashBase16, ReadOnlyMemory<byte>? content)
        LoadComponentSerialRepresentationForHash(ReadOnlyMemory<byte> componentHash)
    {
        var componentHashBase16 = Convert.ToHexStringLower(componentHash.Span);

        return
            (componentHashBase16,
            LoadComponentSerialRepresentationForHash(componentHashBase16));
    }

    private ReadOnlyMemory<byte>? LoadComponentSerialRepresentationForHash(string componentHashBase16)
    {
        if (_componentSerialRepresentationCache.TryGetValue(componentHashBase16, out var fromCache))
            return fromCache;

        var loaded =
            LoadComponentSerialRepresentationForHashIgnoringCache(componentHashBase16);

        if (loaded is not null)
        {
            _componentSerialRepresentationCache[componentHashBase16] = loaded.Value;
        }

        return loaded;
    }

    private ReadOnlyMemory<byte>? LoadComponentSerialRepresentationForHashIgnoringCache(string componentHashBase16)
    {
        var filePath =
            GetFilePathForComponentInComponentFileStore(componentHashBase16);

        var originalFile = LiteralElementFileStore.GetFileContent(filePath);

        if (originalFile is null)
        {
            var deflatedFile = DeflatedLiteralElementFileStore.GetFileContent(filePath);

            if (deflatedFile is not null)
                return BytesConversions.Inflate(deflatedFile.Value);
        }

        return originalFile;
    }

    public PineValue? LoadComponent(string componentHashBase16)
    {
        if (ValueBinaryDeflatedFileStore.GetFileContent(
            GetFilePathForComponentInComponentFileStore(componentHashBase16)) is { } fileContentBinaryDeflated)
        {
            var binaryBytes = BytesConversions.Inflate(fileContentBinaryDeflated);

            return ValueBinaryEncodingClassic.DecodeRoot(binaryBytes);
        }

        if (ValueJsonDeflatedFileStore.GetFileContent(
            GetFilePathForComponentInComponentFileStore(componentHashBase16)) is { } fileContentJsonDeflated)
        {
            var jsonBytes = BytesConversions.Inflate(fileContentJsonDeflated);

            return DeserializeValueFromJsonString(Encoding.UTF8.GetString(jsonBytes.Span));
        }

        return LoadComponentNormalized(componentHashBase16);
    }

    public PineValue? LoadComponentNormalized(string componentHashBase16)
    {
        var fromComponentStore = LoadComponentSerialRepresentationForHash(componentHashBase16);

        if (fromComponentStore is null)
            return null;

        return
            PineValueHashTree.DeserializeFromHashTree(
                fromComponentStore.Value,
                LoadComponentSerialRepresentationForHash,
                valueFromCache:
                componentHashBase16 =>
                {
                    componentFromHashCache.TryGetValue(componentHashBase16, out var fromCache);

                    return fromCache;
                },
                valueLoadedForHash:
                (componentHashBase16, componentValue) =>
                {
                    componentFromHashCache[componentHashBase16] = componentValue;
                })
            .Unpack(
                fromErr: error => throw new Exception("Failed to load component " + componentHashBase16 + ": " + error),
                fromOk: loadComponentResult =>
                {
                    if (Convert.ToHexStringLower(_hashCache.GetHash(loadComponentResult).Span) != componentHashBase16)
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
                https://github.com/pine-vm/pine/blob/1cd3f00bdf5a05e9bda479c534b0458b2496393c/implement/PersistentProcess/PersistentProcess.Common/ProcessStore.cs#L183
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

                if (fileContent is null)
                    return ImmutableList<(IImmutableList<string>, ReadOnlyMemory<byte>)>.Empty;

                return
                    SplitFileContentIntoCompositionLogRecords(fileContent.Value)
                    .Select(record => (filePath, record))
                    .Reverse();
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

    public static PineValue DeserializeValueFromJsonString(string serialized)
    {
        var dictionaryEntries =
            JsonSerializer.Deserialize<IReadOnlyList<PineValueCompactBuild.ListEntry>>(serialized)
            ??
            throw new Exception("Failed to deserialize dictionary entries from string: " + serialized);

        var sharedDictionary =
            PineValueCompactBuild.BuildDictionaryFromEntries(dictionaryEntries);

        var largestValue = sharedDictionary.First().Value;

        foreach (var entry in sharedDictionary)
        {
            if (entry.Value is PineValue.ListValue entryList)
            {
                if (largestValue is PineValue.ListValue largestList)
                {
                    if (largestList.NodesCount < entryList.NodesCount)
                    {
                        largestValue = entry.Value;
                    }
                }
                else
                {
                    largestValue = entry.Value;
                }
            }
        }

        return largestValue;
    }
}

public class ProcessStoreWriterInFileStore : ProcessStoreInFileStore, IProcessStoreWriter
{
    private readonly ConcurrentPineValueHashCache _hashCache = new();

    private static int TryDeflateSizeThreshold => 10_000;

    protected IFileStoreWriter _fileStore;

    protected IFileStoreWriter LiteralElementFileStore =>
        _fileStore.ForSubdirectory(LiteralElementSubdirectory);

    protected IFileStoreWriter DeflatedLiteralElementFileStore =>
        _fileStore.ForSubdirectory(DeflatedLiteralElementSubdirectory);

    protected IFileStoreWriter ValueJsonDeflatedFileStore =>
        _fileStore.ForSubdirectory(ValueJsonDeflatedSubdirectory);

    protected IFileStoreWriter ValueBinaryDeflatedFileStore =>
        _fileStore.ForSubdirectory(ValueBinaryDeflatedSubdirectory);

    protected IFileStoreWriter ProvisionalReductionFileStore =>
        _fileStore.ForSubdirectory(ProvisionalReductionSubdirectory);

    protected IFileStoreWriter CompositionLogLiteralFileStore =>
        _fileStore.ForSubdirectory(CompositionLogLiteralPath);

    private readonly Func<DateTimeOffset> getTimeForCompositionLogBatch;

    private readonly System.Threading.Lock appendLock = new();

    private (string hashBase16, IImmutableList<string> filePath)? lastCompositionRecord;

    private readonly bool skipWritingComponentSecondTime;

    private readonly ConcurrentDictionary<PineValue, (ReadOnlyMemory<byte> hash, string hashBase16)> componentsWritten = new();

    private readonly ConcurrentDictionary<string, object?> componentsWrittenHash = new();

    public ProcessStoreWriterInFileStore(
        IFileStoreReader fileStoreReader,
        Func<DateTimeOffset> getTimeForCompositionLogBatch,
        IFileStoreWriter fileStore,
        bool skipWritingComponentSecondTime)
    {
        this.getTimeForCompositionLogBatch = getTimeForCompositionLogBatch;
        this._fileStore = fileStore;
        this.skipWritingComponentSecondTime = skipWritingComponentSecondTime;

        var originalProcessStoreReader = new ProcessStoreReaderInFileStore(fileStoreReader);

        var originalStoreLastCompositionRecord =
            originalProcessStoreReader.EnumerateSerializedCompositionLogRecordsWithFilePathReverse().FirstOrDefault();

        lastCompositionRecord =
            originalStoreLastCompositionRecord.record is not { } lastStoredRecord
            ?
            null
            :
            (CompositionLogRecordInFile.HashBase16FromCompositionRecord(lastStoredRecord),
            originalStoreLastCompositionRecord.filePath);
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
            var recordHashBase16 = Convert.ToHexStringLower(recordHash.Span);

            var compositionLogRecordSerializedWithDelimiter =
                BytesConversions.Concat(
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

    public (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponent(PineValue component)
    {
        if (skipWritingComponentSecondTime)
        {
            if (componentsWritten.TryGetValue(component, out var componentHashAndBase16))
                return componentHashAndBase16;
        }

        if (component is PineValue.ListValue)
        {
            var componentHash =
                ComputeHashBytesAndRememberRecent(component);

            var componentHashBase16 = Convert.ToHexStringLower(componentHash.Span);

            if (skipWritingComponentSecondTime)
            {
                if (componentsWrittenHash.ContainsKey(componentHashBase16))
                    return (componentHash, componentHashBase16);
            }

            /*
             * 2025-04-10: Switch to new binary representation.
             * 
            var asJsonString = SerializeValueToJsonString(component);

            var asJsonBytes = Encoding.UTF8.GetBytes(asJsonString);

            var deflated = BytesConversions.Deflate(asJsonBytes);

            ValueJsonDeflatedFileStore.SetFileContent(
                GetFilePathForComponentInComponentFileStore(componentHashBase16),
                deflated.ToArray());
            */

            using var stream = new System.IO.MemoryStream();

            using var deflateStream =
                new System.IO.Compression.DeflateStream(
                    stream,
                    System.IO.Compression.CompressionLevel.Optimal,
                    leaveOpen: true);

            ValueBinaryEncodingClassic.Encode(deflateStream, component);

            deflateStream.Flush();

            var deflated = stream.ToArray();

            ValueBinaryDeflatedFileStore.SetFileContent(
                GetFilePathForComponentInComponentFileStore(componentHashBase16),
                deflated);

            componentsWrittenHash.TryAdd(componentHashBase16, null);

            return (componentHash, componentHashBase16);
        }

        return StoreComponentAndGetHashRecursive(component);
    }

    private ReadOnlyMemory<byte>? DelegateGetHashOfComponent(PineValue pineValue) =>
        _hashCache.GetHash(pineValue);

    private (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponentAndGetHashRecursive(
        PineValue component)
    {
        var (serialRepresentation, dependencies) =
            PineValueHashTree.ComputeHashTreeNodeSerialRepresentation(component, DelegateGetHashOfComponent);

        var hash = System.Security.Cryptography.SHA256.HashData(serialRepresentation.Span);

        var hashBase16 = Convert.ToHexStringLower(hash);

        void storeSelf()
        {
            if (TryDeflateSizeThreshold <= serialRepresentation.Length)
            {
                var deflated = BytesConversions.Deflate(serialRepresentation);

                if (deflated.Length * 10 < serialRepresentation.Length * 8)
                {
                    DeflatedLiteralElementFileStore.SetFileContent(
                        GetFilePathForComponentInComponentFileStore(hashBase16),
                        deflated.ToArray());

                    componentsWritten.TryAdd(
                        component,
                        (hash, hashBase16));

                    return;
                }
            }

            LiteralElementFileStore.SetFileContent(
                GetFilePathForComponentInComponentFileStore(hashBase16),
                serialRepresentation.ToArray());

            componentsWritten.TryAdd(
                component,
                (hash, hashBase16));
        }

        storeSelf();

        foreach (var dependency in dependencies)
        {
            if (skipWritingComponentSecondTime)
            {
                if (componentsWritten.ContainsKey(dependency))
                    continue;
            }

            StoreComponentAndGetHashRecursive(dependency);
        }

        return (hash, hashBase16);
    }


    readonly Queue<KeyValuePair<PineValue, ReadOnlyMemory<byte>>> componentsWrittenHashQueue = new();

    private ReadOnlyMemory<byte> ComputeHashBytesAndRememberRecent(PineValue pineValue)
    {
        var hashBytes =
            componentsWrittenHashQueue.FirstOrDefault(c => c.Key == pineValue).Value;

        if (hashBytes.IsEmpty)
        {
            hashBytes = PineValueHashTree.ComputeHash(pineValue, ComputeAndCacheHashBytesForSmallValues);
        }

        componentsWrittenHashQueue.Enqueue(
            new KeyValuePair<PineValue, ReadOnlyMemory<byte>>(pineValue, hashBytes));

        while (componentsWrittenHashQueue.Count > 100)
        {
            componentsWrittenHashQueue.Dequeue();
        }

        return hashBytes;
    }

    private ReadOnlyMemory<byte>? ComputeAndCacheHashBytesForSmallValues(PineValue pineValue)
    {
        if (pineValue is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length < 3 ||
                (blobValue.Bytes.Span[0] is 4 && blobValue.Bytes.Length < 4))
            {
                return _hashCache.GetHash(pineValue);
            }
        }

        if (pineValue is PineValue.ListValue listValue)
        {
            if (listValue.NodesCount is 0 && listValue.BlobsBytesCount < 3)
            {
                return _hashCache.GetHash(pineValue);
            }
        }

        return null;
    }

    public static string SerializeValueToJsonString(PineValue value)
    {
        var (listValues, blobValues) =
            PineValue.CollectAllComponentsFromRoots([value]);

        var sharedValuesDict =
            PineValueCompactBuild.PrebuildListEntries(
                blobValues: blobValues,
                listValues: listValues);

        var asJson =
            JsonSerializer.Serialize(sharedValuesDict.listEntries);

        return asJson;
    }
}

public class DiscardingStoreWriter : IProcessStoreWriter
{
    public (ReadOnlyMemory<byte> recordHash, string recordHashBase16) AppendCompositionLogRecord(
        CompositionLogRecordInFile.CompositionEvent compositionEvent) =>
        (ReadOnlyMemory<byte>.Empty, "");

    public (ReadOnlyMemory<byte> hash, string hashBase16) StoreComponent(PineValue component)
    {
        var hash = PineValueHashTree.ComputeHash(component);

        return (hash, Convert.ToHexStringLower(hash.Span));
    }

    public void StoreProvisionalReduction(ProvisionalReductionRecordInFile reduction)
    {
    }
}

