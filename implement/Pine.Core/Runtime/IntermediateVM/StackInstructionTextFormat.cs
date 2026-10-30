using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Loads the lossless textual form produced by <see cref="StackInstructionTraceRenderer"/>.
/// Values are stored separately and requested by their full lowercase SHA256.
/// </summary>
public static class StackInstructionTextFormat
{
    private const string Version = "pine-ir 1";

    /// <summary>
    /// Parses an instruction sequence (or the root sequence of a frame document).
    /// Malformed text, missing values, and invalid metadata produce a <see cref="FormatException"/>.
    /// Exceptions from <paramref name="loadValue"/> propagate to the caller.
    /// </summary>
    public static IReadOnlyList<StackInstruction> ParseInstructions(
        string text,
        Func<string, PineValue?> loadValue) =>
        new Parser(text, loadValue).Parse().Instructions;

    /// <summary>
    /// Parses a complete frame document, restoring shared and recursive linked-frame references.
    /// Resource limits are restored exactly rather than recomputed.
    /// </summary>
    public static StackFrameInstructions ParseStackFrameInstructions(
        string text,
        Func<string, PineValue?> loadValue) =>
        new Parser(text, loadValue).Parse().Frame
        ?? throw new FormatException("Expected a root frame, not a bare instruction sequence.");

    internal static StackInstructionTraceRenderer.RenderedInstructions Render(
        IReadOnlyList<StackInstruction> instructions,
        StackFrameInstructions? frame,
        StackInstructionTraceRenderer.RenderingOptions options) =>
        new Renderer(options).Render(instructions, frame);

    private sealed class Renderer(StackInstructionTraceRenderer.RenderingOptions options)
    {
        private readonly Dictionary<string, PineValue> _values = new(StringComparer.Ordinal);

        private readonly Dictionary<StackFrameInstructions, int> _frameIds = new(ReferenceEqualityComparer.Instance);

        private readonly List<StackFrameInstructions> _frames = [];

        private readonly int _hashLength =
            options.HashHexLength is < 1 or > 64
            ?
            throw new ArgumentOutOfRangeException(nameof(options), "Hash length must be between 1 and 64.")
            :
            options.ForPersistence ? 64 : options.HashHexLength;

        private string Hash(PineValue value)
        {
            var hash = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span);
            _values.TryAdd(hash, value);
            return "hash 0x" + hash[.._hashLength];
        }

        private int FrameId(StackFrameInstructions frame)
        {
            if (_frameIds.TryGetValue(frame, out var existing))
                return existing;

            var id = _frames.Count;
            _frames.Add(frame);
            _frameIds.Add(frame, id);
            return id;
        }

        private string Value(PineValue value) =>
            StackInstructionTraceRenderer.RenderLiteral(
                value,
                blobRepresentations: null,
                renderBlobContents: null,
                renderHash: Hash,
                alwaysRenderHash: options.ForPersistence,
                invariantNumbers: options.ForPersistence);

        private string ExpressionReference(Expression expression) =>
            Hash(ExpressionEncoding.EncodeExpressionAsValue(expression));

        private string Integer(BigInteger integer) =>
            integer.GetByteCount() <= 32
            ?
            integer.ToString(CultureInfo.InvariantCulture)
            :
            Value(IntegerEncoding.EncodeSignedInteger(integer));

        public StackInstructionTraceRenderer.RenderedInstructions Render(
            IReadOnlyList<StackInstruction> instructions,
            StackFrameInstructions? frame)
        {
            if (!options.ForPersistence)
            {
                return
                    new(
                        StackInstructionTraceRenderer.RenderSequence(instructions, Instruction, Value),
                        _values.ToImmutableDictionary(StringComparer.Ordinal));
            }

            var sections = new List<string> { Version };

            if (frame is null)
            {
                sections.Add(
                    "sequence\n" + StackInstructionTraceRenderer.RenderSequence(instructions, Instruction, Value));
            }
            else
            {
                FrameId(frame);
            }

            // Rendering discovers new frames. Identity, not structural equality, preserves cycles and sharing.
            for (var index = 0; index < _frames.Count; ++index)
            {
                var current = _frames[index];

                var fields =
                    new List<string>
                    {
                        "Parameters: " +
                        Value(StackInstructionMetadataEncoding.EncodePaths(current.Parameters.ParamsPaths)),
                        "LocalsCount: " + Number(current.LocalsCount),
                        "MaxStackUsage: " + Number(current.MaxStackUsage)
                    };

                if (current.TrackEnvConstraint is { } constraint)
                {
                    fields.Add(
                        "TrackEnvConstraint: " + Value(StackInstructionMetadataEncoding.EncodeConstraint(constraint)));
                }

                sections.Add(
                    "frame " + Number(index) + " (" + string.Join(", ", fields) + ")\n" +
                    StackInstructionTraceRenderer.RenderSequence(current.Instructions, Instruction, Value));
            }

            return new(string.Join("\n\n", sections), _values.ToImmutableDictionary(StringComparer.Ordinal));
        }

        private string Instruction(StackInstruction instruction, int index)
        {
            return
                StackInstruction.RenderInstructionDisplay(
                    instruction,
                    Value,
                    instructionIndex: index,
                    renderExpression: ExpressionReference,
                    displayOverride: options.ForPersistence ? Fields(instruction, index) : null,
                    observeValue: value => Hash(value));
        }

        private StackInstruction.InstructionDisplay Fields(StackInstruction instruction, int index)
        {
            if (!Enum.IsDefined(instruction.Kind))
                throw new NotImplementedException("Fields does not handle instruction kind: " + instruction.Kind);

            var fields = new List<string>();
            var details = new List<string>();

            void IntField(string name, int? number)
            {
                if (number is { } present)
                    fields.Add(name + ": " + Number(present));
            }

            if (instruction.Literal is { } literal)
                fields.Add("Literal: " + Value(literal.Evaluate()));

            if (instruction.IntegerLiteral is { } integer)
                fields.Add("IntegerLiteral: " + Integer(integer));

            if (instruction.LocalIndices is { IsDefault: false } localIndices)
            {
                fields.Add(
                    "LocalIndices: " +
                    (localIndices.Length <= 32
                    ?
                    "[ " + string.Join(", ", localIndices.Select(index => Number(index))) + " ]"
                    :
                    Value(StackInstructionMetadataEncoding.EncodePath(localIndices))));
            }

            IntField("PopCount", instruction.PopCount);
            IntField("SkipCount", instruction.SkipCount);
            IntField("TakeCount", instruction.TakeCount);
            IntField("ShiftCount", instruction.ShiftCount);

            if (instruction.JumpOffset is { } offset)
                fields.Add("JumpOffset: " + Number(offset) + " (to " + Number(unchecked(index + offset)) + ")");

            if (instruction.OptimizedInvocation is { } invocation)
            {
                fields.Add("InvocationExpression: " + ExpressionReference(invocation.Expression));
                fields.Add("InvocationExpressionEncoded: " + Value(invocation.ExpressionEncoded));

                fields.Add(
                    "InvocationInterface: " +
                    Value(
                        StackInstructionMetadataEncoding.EncodePaths(invocation.InvocationInterface.ParamsPaths)));

                if (invocation.LinkedStackFrameInstructions is { } linked)
                    fields.Add("LinkedFrame: " + Number(FrameId(linked)));
            }

            if (instruction.TemplatePlan is { } plan)
                fields.Add("TemplatePlan: " + Value(StackInstructionMetadataEncoding.EncodePlan(plan)));

            if (instruction.TemplateProducer)
                fields.Add("TemplateProducer: true");

            if (instruction.SwitchJumpTable is { } table)
            {
                var cases = table.ToArray();

                if (cases.Length <= StackInstruction.MaxInlineSwitchCases)
                {
                    fields.Add("SwitchJumpTable: " + Number(cases.Length));

                    foreach (var (value, jump) in cases.OrderBy(entry => entry.Value).ThenBy(entry => Hash(entry.Key), StringComparer.Ordinal))
                    {
                        details.Add(
                            "case " + Value(value) + ": jump (" + Number(jump) + ", " + Number(unchecked(index + jump)) +
                            ")");
                    }
                }
                else
                {
                    fields.Add("SwitchJumpTable: " + Value(StackInstructionMetadataEncoding.EncodeJumpTable(cases)));
                }
            }

            if (instruction.SliceSwitchCases is { IsDefault: false } sliceCases)
            {
                if (sliceCases.Length <= StackInstruction.MaxInlineSwitchCases)
                {
                    fields.Add("SliceSwitchCases: " + Number(sliceCases.Length));

                    foreach (var item in sliceCases)
                    {
                        details.Add(
                            "slice case " + Value(item.Literal) + ": jump (" + Number(item.JumpOffset) + ", " +
                            Number(unchecked(index + item.JumpOffset)) +
                            ")");
                    }
                }
                else
                {
                    fields.Add(
                        "SliceSwitchCases: " + Value(StackInstructionMetadataEncoding.EncodeSliceCases(sliceCases)));
                }
            }

            return new(fields, details);
        }
    }

    private static string Number(long number) => number.ToString(CultureInfo.InvariantCulture);

    private sealed class Parser
    {
        private readonly string[] _lines;

        private readonly Func<string, PineValue?> _loadValue;

        private readonly Dictionary<string, PineValue> _values = new(StringComparer.Ordinal);

        private readonly Dictionary<int, StackFrameInstructions> _frames = [];

        private readonly List<(DirectInvocation Invocation, int Frame)> _links = [];

        private int _line;

        public Parser(string text, Func<string, PineValue?> loadValue)
        {
            ArgumentNullException.ThrowIfNull(text);
            ArgumentNullException.ThrowIfNull(loadValue);
            _lines = text.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n');
            _loadValue = loadValue;
        }

        private FormatException Error(string message) => new("Line " + Number(_line + 1) + ": " + message);

        private string? Peek()
        {
            while (_line < _lines.Length && string.IsNullOrWhiteSpace(_lines[_line]))
                ++_line;

            return _line < _lines.Length ? _lines[_line].Trim() : null;
        }

        public (IReadOnlyList<StackInstruction> Instructions, StackFrameInstructions? Frame) Parse()
        {
            if (Peek() != Version)
                throw Error("Expected '" + Version + "'.");

            ++_line;
            IReadOnlyList<StackInstruction>? sequence = null;
            StackFrameInstructions? root = null;

            while (Peek() is { } header)
            {
                if (header is "sequence")
                {
                    if (sequence is not null || _frames.Count > 0)
                        throw Error("Unexpected sequence section.");

                    ++_line;
                    sequence = Instructions();
                }
                else if (header.StartsWith("frame ", StringComparison.Ordinal))
                {
                    var opening = header.IndexOf(" (", StringComparison.Ordinal);

                    if (opening < 0)
                        throw Error("Expected frame metadata.");

                    var id = Int(header[6..opening]);
                    var fields = ReadFields(header[(opening + 1)..]);

                    var parameters =
                        StaticFunctionInterface.FromPathsSorted(
                            StackInstructionMetadataEncoding.DecodePaths(Value(Required(fields, "Parameters"))));

                    var locals = Int(Required(fields, "LocalsCount"));
                    var stack = Int(Required(fields, "MaxStackUsage"));

                    var constraint =
                        fields.Remove("TrackEnvConstraint", out var encodedConstraint)
                        ?
                        StackInstructionMetadataEncoding.DecodeConstraint(Value(encodedConstraint))
                        :
                        null;

                    NoFields(fields);
                    ++_line;
                    var instructions = Instructions();

                    var frame = new StackFrameInstructions(parameters, instructions, constraint, locals, stack);

                    if (id < 0 || !_frames.TryAdd(id, frame))
                        throw Error("Invalid or duplicate frame identifier.");

                    if (root is null && sequence is null)
                    {
                        root = frame;
                        sequence = instructions;
                    }
                }
                else
                {
                    throw Error("Expected sequence or frame section.");
                }
            }

            if (sequence is null)
                throw Error("Missing root sequence or frame.");

            foreach (var (invocation, id) in _links)
            {
                if (!_frames.TryGetValue(id, out var frame))
                    throw Error("Unknown linked frame " + Number(id) + ".");

                invocation.SetLinkedStackFrameInstructions(frame);
            }

            return (sequence, root);
        }

        private IReadOnlyList<StackInstruction> Instructions()
        {
            var instructions = new List<StackInstruction>();

            while (Peek() is { } line &&
                !line.StartsWith("frame ", StringComparison.Ordinal) &&
                line is not "sequence")
            {
                if (line.StartsWith("jumps_arriving_from ", StringComparison.Ordinal))
                {
                    ++_line;
                    continue;
                }

                var colon = line.IndexOf(':');

                if (colon < 0 || Int(line[..colon]) != instructions.Count)
                    throw Error("Expected consecutive instruction index.");

                var text = line[(colon + 1)..].Trim();
                var opening = text.IndexOf(" (", StringComparison.Ordinal);
                var name = opening < 0 ? text : text[..opening];

                if (!Enum.TryParse<StackInstructionKind>(name, out var kind) || !Enum.IsDefined(kind) ||
                    name != kind.ToString())
                {
                    throw Error("Unknown instruction kind '" + name + "'.");
                }

                var fields =
                    opening < 0
                    ?
                    new Dictionary<string, string>(StringComparer.Ordinal)
                    :
                    ReadFields(text[(opening + 1)..]);

                var literal = OptionalValue(fields, "Literal");

                var integer =
                    fields.Remove("IntegerLiteral", out var integerText)
                    ?
                    integerText.StartsWith("Blob ", StringComparison.Ordinal)
                    ?
                    StackInstructionMetadataEncoding.DecodeInteger(Value(integerText))
                    :
                    BigInteger.TryParse(
                        integerText,
                        NumberStyles.AllowLeadingSign,
                        CultureInfo.InvariantCulture,
                        out var parsedInteger)
                    ?
                    parsedInteger
                    :
                    throw Error("Invalid integer literal.")
                    :
                    (BigInteger?)null;

                var locals =
                    fields.Remove("LocalIndices", out var localText)
                    ?
                    localText.StartsWith('[')
                    ?
                    ParseIndices(localText)
                    :
                    StackInstructionMetadataEncoding.DecodePath(Value(localText)).ToImmutableArray()
                    :
                    default;

                var pop = OptionalInt(fields, "PopCount");
                var skip = OptionalInt(fields, "SkipCount");
                var take = OptionalInt(fields, "TakeCount");
                var shift = OptionalInt(fields, "ShiftCount");
                var jump = OptionalInt(fields, "JumpOffset");
                DirectInvocation? invocation = null;

                if (fields.Remove("InvocationExpression", out var expressionText))
                {
                    invocation =
                        new(
                            StackInstructionMetadataEncoding.DecodeExpression(Value(expressionText)),
                            Value(Required(fields, "InvocationExpressionEncoded")),
                            StaticFunctionInterface.FromPathsSorted(
                                StackInstructionMetadataEncoding.DecodePaths(
                                    Value(Required(fields, "InvocationInterface")))));
                }

                if (fields.Remove("LinkedFrame", out var linked))
                    _links.Add((invocation ?? throw Error("LinkedFrame requires an invocation."), Int(linked)));

                var planValue = OptionalValue(fields, "TemplatePlan");

                var producer =
                    fields.Remove("TemplateProducer", out var producerText) &&
                    (producerText is "true" ? true : throw Error("Expected TemplateProducer: true."));

                var table =
                    fields.Remove("SwitchJumpTable", out var tableText)
                    ?
                    ParseJumpTable(tableText)
                    :
                    null;

                var slices =
                    fields.Remove("SliceSwitchCases", out var sliceText)
                    ?
                    ParseSliceCases(sliceText)
                    :
                    null;

                NoFields(fields);
                ++_line;

                while (Peek() is { } detail && ParseSwitchCase(detail) is { } switchCase)
                {
                    if (switchCase.IsSlice)
                    {
                        if (slices is null)
                            throw Error("Unexpected slice case.");

                        slices.Add(new(switchCase.Literal, switchCase.JumpOffset));
                    }
                    else
                    {
                        if (table is null || !table.TryAdd(switchCase.Literal, switchCase.JumpOffset))
                            throw Error("Unexpected or duplicate switch case.");
                    }

                    ++_line;
                }

                instructions.Add(
                    new StackInstruction(
                        kind,
                        Literal: literal is null ? null : PineValueInProcess.CreateFullyRepresented(literal),
                        IntegerLiteral: integer,
                        LocalIndices: locals,
                        PopCount: pop,
                        SkipCount: skip,
                        TakeCount: take,
                        ShiftCount: shift,
                        JumpOffset: jump,
                        OptimizedInvocation: invocation,
                        SwitchJumpTable: table?.ToImmutable(),
                        SliceSwitchCases: slices?.ToImmutable() ?? default)
                    {
                        TemplatePlan =
                        planValue is null ? null : StackInstructionMetadataEncoding.DecodePlan(planValue),
                        TemplateProducer = producer
                    });
            }

            return instructions;
        }

        private PineValue? OptionalValue(Dictionary<string, string> fields, string name) =>
            fields.Remove(name, out var text) ? Value(text) : null;

        private ImmutableDictionary<PineValue, int>.Builder ParseJumpTable(string text)
        {
            var table = ImmutableDictionary.CreateBuilder<PineValue, int>();

            if (text.StartsWith("List ", StringComparison.Ordinal))
            {
                foreach (var (value, offset) in StackInstructionMetadataEncoding.DecodeSwitchCases(Value(text)))
                {
                    if (!table.TryAdd(value, offset))
                        throw Error("Duplicate switch case.");
                }
            }

            return table;
        }

        private ImmutableArray<SliceSwitchCase>.Builder ParseSliceCases(string text)
        {
            var slices = ImmutableArray.CreateBuilder<SliceSwitchCase>();

            if (text.StartsWith("List ", StringComparison.Ordinal))
            {
                foreach (var (value, offset) in StackInstructionMetadataEncoding.DecodeSwitchCases(Value(text)))
                    slices.Add(new(value, offset));
            }

            return slices;
        }

        private readonly record struct ParsedSwitchCase(bool IsSlice, PineValue Literal, int JumpOffset);

        private ParsedSwitchCase? ParseSwitchCase(string text)
        {
            var (isSlice, start) =
                text.StartsWith("slice case ", StringComparison.Ordinal)
                ?
                (true, 11)
                :
                text.StartsWith("case ", StringComparison.Ordinal)
                ?
                (false, 5)
                :
                (false, -1);

            if (start < 0)
                return null;

            var contents = text[start..];
            var separator = contents.LastIndexOf(": jump (", StringComparison.Ordinal);

            if (separator <= 0 || !contents.EndsWith(')'))
                throw Error("Invalid switch case.");

            return
                new(
                    isSlice,
                    Value(contents[..separator]),
                    Int(contents[(separator + 8)..^1].Split(',')[0]));
        }

        private ImmutableArray<int> ParseIndices(string text)
        {
            if (!text.EndsWith(']'))
                throw Error("Expected local index list.");

            var contents = text[1..^1].Trim();
            return contents.Length is 0 ? [] : [.. contents.Split(',').Select(Int)];
        }

        private int? OptionalInt(Dictionary<string, string> fields, string name) =>
            fields.Remove(name, out var text) ? Int(text.Split(" (", StringSplitOptions.None)[0]) : null;

        private int Int(string text) =>
            int.TryParse(text.Trim(), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, out var number)
            ?
            number
            :
            throw Error("Invalid 32-bit integer '" + text + "'.");

        private PineValue Value(string text)
        {
            var marker = text.LastIndexOf("hash 0x", StringComparison.Ordinal);

            if (marker < 0)
                throw Error("Expected a full SHA256 value reference.");

            var start = marker + 7;
            var end = start;

            while (end < text.Length && char.IsAsciiHexDigit(text[end]))
                ++end;

            var digits = text[start..end];

            if (digits.Length != 64)
                throw Error("Expected 64 hexadecimal digits in value reference.");

            var hash = digits.ToLowerInvariant();

            if (_values.TryGetValue(hash, out var cached))
                return cached;

            var value = _loadValue(hash) ?? throw Error("Missing value for SHA256 " + hash + ".");
            var actual = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span);

            if (actual != hash)
                throw Error("Loaded value does not match SHA256 " + hash + ".");

            _values.Add(hash, value);
            return value;
        }

        private Dictionary<string, string> ReadFields(string text)
        {
            if (!text.StartsWith('(') || !text.EndsWith(')'))
                throw Error("Expected parenthesized fields.");

            var fields = new Dictionary<string, string>(StringComparer.Ordinal);
            var depth = 0;
            var quoted = false;
            var escaped = false;
            var start = 1;

            for (var index = 1; index < text.Length; ++index)
            {
                var character = text[index];

                if (quoted)
                {
                    if (escaped)
                        escaped = false;

                    else if (character is '\\')
                        escaped = true;

                    else if (character is '"')
                        quoted = false;

                    continue;
                }

                if (character is '"')
                    quoted = true;

                else if (character is '(' or '[')
                    ++depth;

                else if (character is ')' or ']')
                {
                    if (index != text.Length - 1)
                        --depth;
                }

                if (depth < 0)
                    throw Error("Unbalanced field delimiters.");

                if (index == text.Length - 1 || character is ',' && depth is 0)
                {
                    var field = text[start..index].Trim();
                    var colon = field.IndexOf(':');

                    if (colon <= 0 || !fields.TryAdd(field[..colon], field[(colon + 1)..].Trim()))
                        throw Error("Invalid or duplicate field.");

                    start = index + 1;
                }
            }

            if (depth != 0 || quoted)
                throw Error("Unbalanced field delimiters.");

            return fields;
        }

        private string Required(Dictionary<string, string> fields, string name) =>
            fields.Remove(name, out var value) ? value : throw Error("Missing field " + name + ".");

        private void NoFields(Dictionary<string, string> fields)
        {
            if (fields.Count > 0)
                throw Error("Unknown fields: " + string.Join(", ", fields.Keys));
        }
    }
}
