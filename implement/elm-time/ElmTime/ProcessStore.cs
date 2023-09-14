using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace ElmTime.ProcessStore;

public record ValueInFile(
    string? LiteralString)
{
    /*
    Future: Reduce operation expenses by sharing objects: Support reusing values here by reference.
    public string HashBase16;
    */

    /*
    Future: Switch to a simpler literal model.
    public byte[] Literal;
    */
}

public record CompositionRecord(
    byte[] ParentHash,
    string? SetStateLiteralString,
    IReadOnlyList<string>? AppendedEventsLiteralString);

public record CompositionRecordInFile(
    string ParentHashBase16,
    ValueInFile? SetState = default,
    IReadOnlyList<ValueInFile>? AppendedEvents = default)
{
    public static byte[] HashFromSerialRepresentation(byte[] serialized) => CommonConversion.HashSHA256(serialized).ToArray();
}

public record ReductionRecord(
    byte[] ReducedCompositionHash,
    string? ReducedValueLiteralString);

public interface IProcessStoreWriter
{
    void AppendSerializedCompositionRecord(byte[] serializedCompositionRecord);

    void StoreReduction(ReductionRecord reduction);
}

public interface IProcessStoreReader
{
    IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse();

    ReductionRecord? GetReduction(byte[] reducedCompositionHash);
}

public class EmptyProcessStoreReader : IProcessStoreReader
{
    public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse()
    {
        yield break;
    }

    public ReductionRecord? GetReduction(byte[] reducedCompositionHash) => null;
}

public class ProcessStoreInFileStore(IFileStore fileStore)
{
    public static JsonSerializerOptions RecordSerializationSettings => new()
    {
        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull
    };

    protected static readonly JsonSerializerOptions recordSerializationSettings = RecordSerializationSettings;

    protected record ReductionRecordInFile(
        string ReducedCompositionHashBase16,
        ValueInFile ReducedValue);

    protected IFileStoreReader compositionFileStoreReader => ((IFileStoreReader)fileStore).ForSubdirectory("composition");
    protected IFileStoreWriter compositionFileStoreWriter => ((IFileStoreWriter)fileStore).ForSubdirectory("composition");

    protected IFileStoreReader reductionFileStoreReader => ((IFileStoreReader)fileStore).ForSubdirectory("reduction");
    protected IFileStoreWriter reductionFileStoreWriter => ((IFileStoreWriter)fileStore).ForSubdirectory("reduction");

    protected static IEnumerable<IImmutableList<string>> CompositionLogFileOrder(IEnumerable<IImmutableList<string>> logFilesNames) =>
        logFilesNames.OrderBy(filePath => string.Join("", filePath));

    public IEnumerable<IImmutableList<string>> EnumerateCompositionsLogFilesPaths() =>
        CompositionLogFileOrder(
            compositionFileStoreReader.ListFilesInDirectory(ImmutableList<string>.Empty));
}

public class ProcessStoreReaderInFileStore(IFileStore fileStore)
    : ProcessStoreInFileStore(fileStore), IProcessStoreReader
{
    public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
        EnumerateCompositionsLogFilesPaths().Reverse()
        .SelectMany(compositionFilePath =>
            {
                var fileContent = compositionFileStoreReader.GetFileContent(compositionFilePath)!;

                return
                    Encoding.UTF8.GetString(fileContent.Value.Span)
                    .Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries).Reverse()
                    .Select(compositionRecord => Encoding.UTF8.GetBytes(compositionRecord));
            });

    public ReductionRecord? GetReduction(byte[] reducedCompositionHash)
    {
        var reducedCompositionHashBase16 = CommonConversion.StringBase16FromByteArray(reducedCompositionHash);

        var filePath = ImmutableList.Create(reducedCompositionHashBase16);

        var fileContent = reductionFileStoreReader.GetFileContent(filePath);

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
                JsonSerializer.Deserialize<ReductionRecordInFile>(fileContent.Value[payloadStartIndex..].Span)!;

            if (reducedCompositionHashBase16 != reductionRecordFromFile.ReducedCompositionHashBase16)
                throw new Exception("Unexpected content in file " + string.Join("/", filePath) + ", composition hash does not match.");

            return new ReductionRecord
            (
                ReducedCompositionHash: reducedCompositionHash,
                ReducedValueLiteralString: reductionRecordFromFile.ReducedValue?.LiteralString
            );
        }
        catch (Exception e)
        {
            throw new Exception(
                "Failed to read reduction from file '" + string.Join("/", filePath) + "' (" + fileContent.Value.Length + " bytes)",
                e);
        }
    }

    public IEnumerable<string> ReductionsFilesNames() =>
        reductionFileStoreReader.ListFilesInDirectory(ImmutableList<string>.Empty)
        .Select(Enumerable.Last);
}

public class ProcessStoreWriterInFileStore(
    IFileStore fileStore,
    Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath)
    : ProcessStoreInFileStore(fileStore), IProcessStoreWriter
{
    private Func<IImmutableList<string>> getCompositionLogRequestedNextFilePath = getCompositionLogRequestedNextFilePath;

    private readonly object appendSerializedCompositionRecordLock = new();

    private IImmutableList<string>? appendSerializedCompositionRecordLastFilePath = null;

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
                        [.. EnumerateCompositionsLogFilesPaths(), compositionLogRequestedFilePathInDirectory])
                    .Last();

                if (compositionLogRequestedFilePathInDirectory.Equals(lastFileNameIfAddRequestedFileName))
                    compositionLogFilePath = compositionLogRequestedFilePathInDirectory;
            }

            compositionFileStoreWriter.AppendFileContent(
                compositionLogFilePath,
                CommonConversion.Concat<byte>(record, Encoding.UTF8.GetBytes("\n")));

            appendSerializedCompositionRecordLastFilePath = compositionLogFilePath;
        }
    }

    public void StoreReduction(ReductionRecord record)
    {
        var recordInFile = new ReductionRecordInFile
        (
            ReducedCompositionHashBase16: CommonConversion.StringBase16FromByteArray(record.ReducedCompositionHash),
            ReducedValue: new ValueInFile(LiteralString: record.ReducedValueLiteralString)
        );

        var fileName = recordInFile.ReducedCompositionHashBase16;

        var filePath = ImmutableList.Create(fileName);

        reductionFileStoreWriter.SetFileContent(
            filePath, Encoding.UTF8.GetBytes(JsonSerializer.Serialize(recordInFile, recordSerializationSettings)));
    }
}
