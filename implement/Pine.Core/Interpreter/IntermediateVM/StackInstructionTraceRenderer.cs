using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Renders sequences of executed PineVM stack instructions for diagnostics and documentation.
/// </summary>
public static class StackInstructionTraceRenderer
{
    /// <summary>
    /// One configurable way to render the contents of a blob literal.
    /// </summary>
    /// <param name="Render">
    /// Function returning a text representation for the blob, or <see langword="null"/> to contribute no output.
    /// </param>
    public readonly record struct BlobRepresentation(
        Func<PineValue.BlobValue, string?> Render);

    /// <summary>
    /// Default blob rendering configuration used by this renderer.
    /// <para>
    /// The default order is Base16 first, then UTF-32 string decoding, then strict Pine integer decoding.
    /// String and integer mappings contribute no text when they do not apply.
    /// </para>
    /// </summary>
    public static readonly IReadOnlyList<BlobRepresentation> DefaultBlobRepresentations =
        BuildDefaultBlobRepresentations(
            maxBase16ByteCount: 32,
            maxUtf32StringCharCount: 32);

    /// <summary>
    /// Builds a blob representation that renders bytes as Base16 text, truncating after the specified number of bytes.
    /// </summary>
    /// <param name="maxByteCount">Maximum number of bytes to include before appending an ellipsis.</param>
    public static BlobRepresentation BuildBlobRepresentationBase16(
        int maxByteCount)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(maxByteCount);

        return
            new BlobRepresentation(
                blob => RenderBlobBase16(blob.Bytes.Span, maxByteCount));
    }

    /// <summary>
    /// Builds a blob representation that attempts to decode the blob as a UTF-32 string.
    /// </summary>
    /// <param name="maxCharCount">Maximum number of characters to include before appending an ellipsis.</param>
    /// <param name="noStringRepresentation">
    /// Optional fallback text to use when the blob is not a valid UTF-32 string. Supply whitespace or <see langword="null"/>
    /// to suppress output in that case.
    /// </param>
    public static BlobRepresentation BuildBlobRepresentationUtf32String(
        int maxCharCount,
        string? noStringRepresentation = null)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(maxCharCount);

        return
            new BlobRepresentation(
                blob =>
                {
                    if (StringEncoding.StringFromBlobValue(blob.Bytes).IsOkOrNull() is not { } asString)
                        return NormalizeNoMatchRepresentation(noStringRepresentation);

                    var limited =
                        maxCharCount < asString.Length
                        ?
                        asString[..maxCharCount] + "..."
                        :
                        asString;

                    return "UTF32 " + JsonSerializer.Serialize(limited);
                });
    }

    /// <summary>
    /// Builds a blob representation that attempts to decode the blob as a strict Pine signed integer.
    /// </summary>
    /// <param name="noIntegerRepresentation">
    /// Optional fallback text to use when the blob is not a strictly encoded Pine integer. Supply whitespace or
    /// <see langword="null"/> to suppress output in that case.
    /// </param>
    public static BlobRepresentation BuildBlobRepresentationStrictPineInteger(
        string? noIntegerRepresentation = null)
    {
        return
            new BlobRepresentation(
                blob =>
                {
                    if (IntegerEncoding.ParseSignedIntegerStrict(blob.Bytes.Span).IsOkOrNullable() is not { } asInteger)
                        return NormalizeNoMatchRepresentation(noIntegerRepresentation);

                    return "int " + asInteger;
                });
    }

    /// <summary>
    /// Builds the default ordered set of blob representations used for trace rendering.
    /// </summary>
    /// <param name="maxBase16ByteCount">Maximum number of bytes to include in the Base16 representation.</param>
    /// <param name="maxUtf32StringCharCount">Maximum number of characters to include in the UTF-32 string representation.</param>
    public static IReadOnlyList<BlobRepresentation> BuildDefaultBlobRepresentations(
        int maxBase16ByteCount,
        int maxUtf32StringCharCount) =>
        [
        BuildBlobRepresentationBase16(maxBase16ByteCount),
        BuildBlobRepresentationUtf32String(
            maxCharCount: maxUtf32StringCharCount,
            noStringRepresentation: ""),
        BuildBlobRepresentationStrictPineInteger(
            noIntegerRepresentation: "")
        ];

    /// <summary>
    /// Renders a sequence of executed stack instructions as text.
    /// </summary>
    /// <param name="trace">The executed instruction sequence to render.</param>
    /// <param name="renderInstructionIndex">
    /// Set to <see langword="true"/> to include the instruction index as a left-padded prefix on each rendered line.
    /// </param>
    /// <param name="blobRepresentations">
    /// Ordered list of blob renderers used to derive blob content text. If omitted, <see cref="DefaultBlobRepresentations"/>
    /// is used.
    /// </param>
    /// <param name="renderBlobContents">
    /// Optional callback that receives the blob and the derived representation texts and returns the final blob-content text.
    /// </param>
    public static string RenderInstructionTrace(
        IReadOnlyList<ExecutedStackInstruction> trace,
        bool renderInstructionIndex = false,
        IReadOnlyList<BlobRepresentation>? blobRepresentations = null,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents = null)
    {
        if (trace.Count is 0)
            return "";

        var indexWidth =
            renderInstructionIndex
            ?
            trace[^1].InstructionIndex.ToString().Length
            :
            0;

        return
            string.Join(
                '\n',
                trace.Select(
                    traceItem =>
                    {
                        var prefix =
                            renderInstructionIndex
                            ?
                            traceItem.InstructionIndex.ToString().PadLeft(indexWidth) + ". "
                            :
                            "";

                        return
                            prefix +
                            "depth=" + traceItem.StackFrameDepth +
                            " ip=" + traceItem.InstructionPointer +
                            " " +
                            RenderInstruction(
                                traceItem.Instruction,
                                blobRepresentations: blobRepresentations,
                                renderBlobContents: renderBlobContents);
                    }));
    }

    /// <summary>
    /// Renders a sequence of executed stack instructions using the default ordered blob representations.
    /// </summary>
    /// <param name="trace">The executed instruction sequence to render.</param>
    /// <param name="maxBase16ByteCount">Maximum number of bytes to include in Base16 blob renderings.</param>
    /// <param name="maxUtf32StringCharCount">Maximum number of characters to include in UTF-32 blob renderings.</param>
    /// <param name="renderInstructionIndex">
    /// Set to <see langword="true"/> to include the instruction index as a left-padded prefix on each rendered line.
    /// </param>
    /// <param name="renderBlobContents">
    /// Optional callback that receives the blob and the derived representation texts and returns the final blob-content text.
    /// </param>
    public static string RenderInstructionTraceWithDefaultBlobRepresentations(
        IReadOnlyList<ExecutedStackInstruction> trace,
        int maxBase16ByteCount,
        int maxUtf32StringCharCount,
        bool renderInstructionIndex = false,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents = null) =>
        RenderInstructionTrace(
            trace,
            renderInstructionIndex: renderInstructionIndex,
            blobRepresentations:
            BuildDefaultBlobRepresentations(
                maxBase16ByteCount: maxBase16ByteCount,
                maxUtf32StringCharCount: maxUtf32StringCharCount),
            renderBlobContents: renderBlobContents);

    /// <summary>
    /// Renders the instructions in a <see cref="StackFrameInstructions"/> instance as a multi-line text.
    /// Each instruction is prefixed with its zero-based index and rendered using the default blob representations.
    /// </summary>
    /// <param name="frameInstructions">The frame instructions to render.</param>
    /// <param name="blobRepresentations">
    /// Ordered list of blob renderers. If omitted, <see cref="DefaultBlobRepresentations"/> is used.
    /// </param>
    /// <param name="renderBlobContents">
    /// Optional callback for custom blob-content rendering.
    /// </param>
    public static string RenderStackFrameInstructions(
        StackFrameInstructions frameInstructions,
        IReadOnlyList<BlobRepresentation>? blobRepresentations = null,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents = null)
    {
        if (frameInstructions.Instructions.Count is 0)
            return "";

        var indexWidth =
            (frameInstructions.Instructions.Count - 1).ToString().Length;

        return
            string.Join(
                '\n',
                frameInstructions.Instructions.Select(
                    (instruction, index) =>
                    index.ToString().PadLeft(indexWidth) + ": " +
                    RenderInstruction(
                        instruction,
                        blobRepresentations: blobRepresentations,
                        renderBlobContents: renderBlobContents)));
    }

    private static string RenderInstruction(
        StackInstruction instruction,
        IReadOnlyList<BlobRepresentation>? blobRepresentations,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents)
    {
        var details =
            StackInstruction.GetDetails(
                instruction,
                literalDisplayString:
                value => RenderLiteral(
                    value,
                    blobRepresentations: blobRepresentations,
                    renderBlobContents: renderBlobContents));

        if (details.Arguments.Count is 0)
            return instruction.Kind.ToString();

        return instruction.Kind + " (" + string.Join(" , ", details.Arguments) + ")";
    }

    private static string RenderLiteral(
        PineValue value,
        IReadOnlyList<BlobRepresentation>? blobRepresentations,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents)
    {
        if (value is not PineValue.BlobValue blob)
            return StackInstruction.LiteralDisplayStringDefault(value);

        var representations =
            (blobRepresentations ?? DefaultBlobRepresentations)
            .Select(representation => representation.Render(blob))
            .Select(text => text ?? "")
            .ToArray();

        var contents =
            renderBlobContents?.Invoke(blob, representations)
            ??
            string.Join(
                " | ",
                representations.Where(text => !string.IsNullOrWhiteSpace(text)));

        return
            "Blob [" +
            CommandLineInterface.FormatIntegerForDisplay(blob.Bytes.Length) +
            "]"
            +
            (string.IsNullOrWhiteSpace(contents)
            ?
            ""
            :
            " (" + contents + ")");
    }

    private static string RenderBlobBase16(
        ReadOnlySpan<byte> bytes,
        int maxByteCount)
    {
        if (maxByteCount is 0)
            return bytes.Length is 0 ? "0x" : "0x...";

        if (bytes.Length <= maxByteCount)
            return "0x" + Convert.ToHexStringLower(bytes);

        return "0x" + Convert.ToHexStringLower(bytes[..maxByteCount]) + "...";
    }

    private static string? NormalizeNoMatchRepresentation(string? noMatchRepresentation) =>
        string.IsNullOrWhiteSpace(noMatchRepresentation)
        ?
        null
        :
        noMatchRepresentation;
}
