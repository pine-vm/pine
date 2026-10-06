using Pine.Core.Addressing;
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
    /// Context for a frame transition. Identifiers use "no-frame" at the root and "unknown-frame"
    /// when frame instructions are unavailable. Depth is the depth after the transition.
    /// </summary>
    public readonly record struct FrameTransition(
        string PreviousFrameIdentifier,
        string CurrentFrameIdentifier,
        int Depth,
        long? PreviousFrameIndex,
        long? CurrentFrameIndex,
        ExecutedStackInstruction TraceItem);

    /// <summary>
    /// Default blob rendering configuration used by this renderer.
    /// <para>
    /// The default order is Base16 first, then UTF-32 string decoding, strict Pine integer decoding,
    /// and the canonical hash for values that are neither strings nor integers.
    /// String, integer, and hash mappings contribute no text when they do not apply.
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
                blob =>
                blob.Bytes.Length is 0
                ?
                null
                :
                RenderBlobBase16(blob.Bytes.Span, maxByteCount));
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
                    if (blob.Bytes.Length is 0)
                        return null;

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
            noIntegerRepresentation: ""),
        BuildBlobRepresentationCanonicalHash()
        ];

    /// <summary>
    /// Builds a blob representation containing its canonical Pine value hash, except for blobs that
    /// are valid UTF-32 strings or strictly encoded Pine integers.
    /// </summary>
    public static BlobRepresentation BuildBlobRepresentationCanonicalHash() =>
        new(
            blob =>
            {
                if (StringEncoding.StringFromBlobValue(blob.Bytes).IsOkOrNull() is not null ||
                    IntegerEncoding.ParseSignedIntegerStrict(blob.Bytes.Span).IsOkOrNullable() is not null)
                {
                    return null;
                }

                return
                    "hash 0x" +
                    Convert.ToHexStringLower(PineValueHashTree.ComputeHash(blob).Span)[..8];
            });

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
    /// <param name="renderEnteringFrame">Optional renderer for frame entries, returning lines to insert before the instruction.</param>
    /// <param name="renderReturningFrame">Optional renderer for frame returns, returning lines to insert before the instruction.</param>
    public static string RenderInstructionTrace(
        IReadOnlyList<ExecutedStackInstruction> trace,
        bool renderInstructionIndex = false,
        IReadOnlyList<BlobRepresentation>? blobRepresentations = null,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents = null,
        Func<FrameTransition, IReadOnlyList<string>>? renderEnteringFrame = null,
        Func<FrameTransition, IReadOnlyList<string>>? renderReturningFrame = null)
    {
        if (trace.Count is 0)
            return "";

        renderEnteringFrame ??= RenderEnteringFrameDefault;
        renderReturningFrame ??= RenderReturningFrameDefault;

        var indexWidth =
            renderInstructionIndex
            ?
            trace[^1].InstructionIndex.ToString().Length
            :
            0;

        var lines = new List<string>(trace.Count);
        var frames = new List<(string? identifier, long? frameIndex)>();

        foreach (var traceItem in trace)
        {
            var identifier =
                traceItem.FrameInstructions is { } frameInstructions
                ?
                RenderStackFrameIdentifier(traceItem.FrameExpression, frameInstructions)
                :
                null;

            while (frames.Count > traceItem.StackFrameDepth ||
                (frames.Count == traceItem.StackFrameDepth &&
                frames.Count > 0 &&
                ((frames[^1].frameIndex is { } previousIndex &&
                traceItem.FrameIndex is { } currentIndex &&
                previousIndex != currentIndex) ||
                (identifier is not null &&
                frames[^1].identifier is not null &&
                frames[^1].identifier != identifier))))
            {
                var previous = frames[^1];
                frames.RemoveAt(frames.Count - 1);

                var destination =
                    frames.Count is 0 ? "no-frame" : frames[^1].identifier ?? "unknown-frame";

                lines.AddRange(
                    renderReturningFrame(
                        new FrameTransition(
                            previous.identifier ?? "unknown-frame",
                            destination,
                            frames.Count,
                            previous.frameIndex,
                            frames.Count is 0 ? null : frames[^1].frameIndex,
                            traceItem)));
            }

            while (frames.Count < traceItem.StackFrameDepth - 1)
                frames.Add((null, null));

            if (frames.Count < traceItem.StackFrameDepth)
            {
                var previous = frames.Count is 0 ? "no-frame" : frames[^1].identifier ?? "unknown-frame";
                var previousIndex = frames.Count is 0 ? null : frames[^1].frameIndex;
                frames.Add((identifier, traceItem.FrameIndex));

                lines.AddRange(
                    renderEnteringFrame(
                        new FrameTransition(
                            previous,
                            identifier ?? "unknown-frame",
                            traceItem.StackFrameDepth,
                            previousIndex,
                            traceItem.FrameIndex,
                            traceItem)));
            }
            else if (identifier is not null && frames[^1].identifier is null)
            {
                frames[^1] = (identifier, traceItem.FrameIndex);
            }

            var prefix =
                renderInstructionIndex
                ?
                traceItem.InstructionIndex.ToString().PadLeft(indexWidth) + ". "
                :
                "";

            lines.Add(
                prefix +
                "ip=" + traceItem.InstructionPointer +
                " " +
                RenderInstruction(
                    traceItem.Instruction,
                    blobRepresentations: blobRepresentations,
                    renderBlobContents: renderBlobContents));
        }

        return string.Join('\n', lines);
    }

    private static IReadOnlyList<string> RenderEnteringFrameDefault(FrameTransition transition) =>
        transition.CurrentFrameIdentifier is "unknown-frame"
        ?
        []
        :
        [
        "",
        $"entering frame ({transition.PreviousFrameIdentifier} -> {transition.CurrentFrameIdentifier}) - depth {transition.Depth}"
        ];

    private static IReadOnlyList<string> RenderReturningFrameDefault(FrameTransition transition) =>
        transition.PreviousFrameIdentifier is "unknown-frame"
        ?
        []
        :
        [
        "",
        $"returning frame ({transition.PreviousFrameIdentifier} -> {transition.CurrentFrameIdentifier}) - depth {transition.Depth}"
        ];

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
    /// <param name="renderEnteringFrame">Optional renderer for frame entries.</param>
    /// <param name="renderReturningFrame">Optional renderer for frame returns.</param>
    public static string RenderInstructionTraceWithDefaultBlobRepresentations(
        IReadOnlyList<ExecutedStackInstruction> trace,
        int maxBase16ByteCount,
        int maxUtf32StringCharCount,
        bool renderInstructionIndex = false,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents = null,
        Func<FrameTransition, IReadOnlyList<string>>? renderEnteringFrame = null,
        Func<FrameTransition, IReadOnlyList<string>>? renderReturningFrame = null) =>
        RenderInstructionTrace(
            trace,
            renderInstructionIndex: renderInstructionIndex,
            blobRepresentations:
            BuildDefaultBlobRepresentations(
                maxBase16ByteCount: maxBase16ByteCount,
                maxUtf32StringCharCount: maxUtf32StringCharCount),
            renderBlobContents: renderBlobContents,
            renderEnteringFrame: renderEnteringFrame,
            renderReturningFrame: renderReturningFrame);

    /// <summary>
    /// Renders an identifier for the source expression and environment constraint of a compiled frame.
    /// The expression hash uses the same canonical Pine value hash as instruction literals.
    /// </summary>
    public static string RenderStackFrameIdentifier(
        Expression expression,
        StackFrameInstructions frameInstructions)
    {
        var expressionValue = ExpressionEncoding.EncodeExpressionAsValue(expression);

        var expressionHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(expressionValue).Span)[..8];

        var constraint =
            frameInstructions.TrackEnvConstraint is { ParsedItems.Count: > 0 } envConstraint
            ?
            "0x" + envConstraint.HashBase16[..8]
            :
            "no-constraint";

        return "expr-0x" + expressionHash + "-" + constraint;
    }

    /// <summary>
    /// Renders the instructions in a <see cref="StackFrameInstructions"/> instance as a multi-line text.
    /// Each instruction is prefixed with its zero-based index and rendered using the default blob representations.
    /// Jump destinations include their absolute index and are preceded by their incoming jump locations.
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

        var jumpsArrivingFrom =
            BuildJumpsArrivingFrom(frameInstructions.Instructions);

        var lines = new List<string>(frameInstructions.Instructions.Count + jumpsArrivingFrom.Count);

        for (var index = 0; index < frameInstructions.Instructions.Count; ++index)
        {
            if (jumpsArrivingFrom.TryGetValue(index, out var sourceIndexes))
            {
                lines.Add(
                    "jumps_arriving_from " + sourceIndexes.Count +
                    " (" + string.Join(", ", sourceIndexes) + ")");
            }

            lines.Add(
                index.ToString().PadLeft(indexWidth) + ": " +
                RenderInstruction(
                    frameInstructions.Instructions[index],
                    blobRepresentations: blobRepresentations,
                    renderBlobContents: renderBlobContents,
                    instructionIndex: index));
        }

        return string.Join('\n', lines);
    }

    private static string RenderInstruction(
        StackInstruction instruction,
        IReadOnlyList<BlobRepresentation>? blobRepresentations,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents,
        int? instructionIndex = null)
    {
        return
            StackInstruction.RenderInstructionDisplay(
                instruction,
                literalDisplayString: value =>
                RenderLiteral(
                    value,
                    blobRepresentations: blobRepresentations,
                    renderBlobContents: renderBlobContents),
                instructionIndex: instructionIndex);
    }

    private static IReadOnlyDictionary<int, IReadOnlyList<int>> BuildJumpsArrivingFrom(
        IReadOnlyList<StackInstruction> instructions)
    {
        var arrivals = new Dictionary<int, HashSet<int>>();

        for (var sourceIndex = 0; sourceIndex < instructions.Count; ++sourceIndex)
        {
            var instruction = instructions[sourceIndex];

            var jumpOffsets =
                instruction.Kind switch
                {
                    StackInstructionKind.Jump_Const or
                    StackInstructionKind.Jump_If_Equal_Const or
                    StackInstructionKind.Length_Jump_If_Equal_Const =>
                    instruction.JumpOffset is { } jumpOffset ? [jumpOffset] : [],

                    StackInstructionKind.Switch_Jump_If_Equal_Const or
                    StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const =>
                    StackInstruction
                    .EnumerateSwitchCases(instruction)
                    .Select(switchCase => switchCase.Value),

                    _ =>
                    []
                };

            foreach (var jumpOffset in jumpOffsets)
            {
                var destinationIndex = sourceIndex + jumpOffset;

                if (!arrivals.TryGetValue(destinationIndex, out var sourceIndexes))
                {
                    sourceIndexes = [];
                    arrivals.Add(destinationIndex, sourceIndexes);
                }

                sourceIndexes.Add(sourceIndex);
            }
        }

        return
            arrivals.ToDictionary(
                entry => entry.Key,
                entry => (IReadOnlyList<int>)[.. entry.Value.Order()]);
    }

    private static string RenderLiteral(
        PineValue value,
        IReadOnlyList<BlobRepresentation>? blobRepresentations,
        Func<PineValue.BlobValue, IReadOnlyList<string>, string>? renderBlobContents)
    {
        if (value is not PineValue.BlobValue blob)
        {
            var defaultDisplay = StackInstruction.LiteralDisplayStringDefault(value);

            return
                defaultDisplay[..^1] +
                " | hash 0x" +
                Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span)[..8] +
                ")";
        }

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
