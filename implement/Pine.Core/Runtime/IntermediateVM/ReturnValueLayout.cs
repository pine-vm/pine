using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>Release-tunable return tagging; null disables tagging, including for list values.</summary>
internal sealed record ReturnValueTaggingPolicy(int? MinimumBlobSize = 5)
{
    internal static ReturnValueTaggingPolicy Default { get; } = new();

    internal ImmutableArray<PineValue> SelectKeys(ImmutableArray<PineValue> values)
    {
        bool Preserve(PineValue value) =>
            MinimumBlobSize is not { } minimum ||
            value is PineValue.BlobValue blob && blob.Bytes.Length < minimum;

        var reserved = values.Where(Preserve).ToHashSet();
        var nextTag = 0;

        return
            [
            .. values.Select(
                value =>
                {
                    if (Preserve(value))
                        return value;

                    PineValue key;

                    do
                        key = ReturnValueLayout.Tag(nextTag++);
                    while (!reserved.Add(key));

                    return key;
                })
            ];
    }
}

/// <summary>
/// A canonical value recipe over independent return slots. Conditional payloads are never
/// observed outside their alternative, including by persistence and reconstruction.
/// </summary>
public sealed class ReturnValueLayout : IEquatable<ReturnValueLayout>
{
    private readonly Lazy<string> _hash;

    internal abstract record Node
    {
        internal sealed record Fixed(PineValue Value) : Node;

        internal sealed record Component(int Index, ImmutableArray<PineValue> Values) : Node;

        internal sealed record List(ImmutableArray<Node> Items) : Node;

        internal sealed record Variant(int Index, ImmutableArray<PineValue> Tags, ImmutableArray<Node> Cases) : Node;
    }

    internal sealed record Guard(ImmutableArray<int> Path, PineValue Value);

    internal sealed record Slot(
        ImmutableArray<int> Path,
        ImmutableArray<PineValue> Values,
        ImmutableArray<Guard> Guards,
        ImmutableArray<PineValue> Keys = default)
    {
        internal bool IsMapped { get; } = !Keys.IsDefault && !Keys.SequenceEqual(Values);

        internal int KeyIndex(PineValueInProcess value)
        {
            var index = Keys.IndexOf(value.Evaluate());
            return index >= 0 ? index : throw new FormatException("Invalid return discriminant.");
        }
    }

    internal Node Recipe { get; }

    internal ImmutableArray<Slot> Slots { get; }

    /// <summary>The number of stack slots in this return interface.</summary>
    public int SlotCount => Slots.Length;

    /// <summary>A versioned, structural description used to identify persisted layouts.</summary>
    public PineValue Encoding { get; }

    /// <summary>Stable identity for the recipe, fixed values, and finite maps.</summary>
    public string Hash => _hash.Value;

    /// <summary>The ordinary single-value return interface.</summary>
    public static ReturnValueLayout Canonical { get; } =
        new(new Node.Component(0, []), [new Slot([], [], [])]);

    internal ReturnValueLayout(Node recipe, ImmutableArray<Slot> slots, ReturnValueTaggingPolicy? taggingPolicy = null)
    {
        Recipe = recipe;
        taggingPolicy ??= ReturnValueTaggingPolicy.Default;

        Slots =
            [
            .. slots.Select(
                slot => slot.Keys.IsDefault
                ?
                new Slot(slot.Path, slot.Values, slot.Guards, taggingPolicy.SelectKeys(slot.Values))
                :
                slot)
            ];

        Encoding =
            PineValue.List(
                [
                Tag(2),
                EncodeNode(recipe),
                PineValue.List(
                    [
                        .. Slots.Select(
                            slot => PineValue.List(
                                [
                                StackInstructionMetadataEncoding.EncodePath(slot.Path),
                                PineValue.List([.. slot.Values]),
                                PineValue.List(
                                    [
                                        .. slot.Guards.Select(
                                            guard => PineValue.List(
                                                [StackInstructionMetadataEncoding.EncodePath(guard.Path), guard.Value]))
                                    ]),
                                PineValue.List([.. slot.Keys])
                                ]))
                    ])
                ]);

        _hash = new(() => Convert.ToHexStringLower(PineValueHashTree.ComputeHash(Encoding).Span));
    }

    /// <inheritdoc/>
    public bool Equals(ReturnValueLayout? other) =>
        other is not null && (ReferenceEquals(this, other) || Encoding.Equals(other.Encoding));

    /// <inheritdoc/>
    public override bool Equals(object? obj) => obj is ReturnValueLayout other && Equals(other);

    /// <inheritdoc/>
    public override int GetHashCode() => Encoding.GetHashCode();

    internal PineControlFlowFragment Deconstruct(int sourceLocal)
    {
        PineControlFlowFragment Project(ImmutableArray<int> path)
        {
            var result = PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Local_Get(sourceLocal));

            foreach (var index in path)
                result = result.AppendOperation(StackInstruction.Skip_Head_Const(index));

            return result;
        }

        var result = PineControlFlowFragment.Empty;

        foreach (var slot in Slots)
        {
            var projection = Project(slot.Path);

            if (slot.IsMapped)
            {
                projection =
                    projection.AppendOperation(
                        StackInstruction.Select_Value_Const(
                            slot.Values
                            .Select((value, index) => KeyValuePair.Create(value, slot.Keys[index]))
                            .ToImmutableDictionary(),
                            fallbackValue: slot.Keys[0]));
            }

            for (var index = slot.Guards.Length - 1; index >= 0; index--)
            {
                var guard = slot.Guards[index];

                projection =
                    Project(guard.Path).Append(
                        new PineControlFlowNode.Conditional(
                            guard.Value,
                            PineControlFlowFragment.Empty.AppendOperation(
                                StackInstruction.Push_Literal(PineValue.EmptyList)),
                            projection));
            }

            result = result.Append(projection);
        }

        return result;
    }

    internal PineControlFlowFragment Reconstruct(int firstLocal, IReadOnlyList<int>? path = null)
    {
        static PineValue ProjectConstant(PineValue value, IReadOnlyList<int> remaining) =>
            PineValueInProcess.ValueFromPathOrNull(PineValueInProcess.Create(value), remaining.ToArray()) ??
            PineValue.EmptyList;

        PineValue? ConstantProjection(Node node, IReadOnlyList<int> remaining)
        {
            switch (node)
            {
                case Node.Fixed fixedValue:
                    return ProjectConstant(fixedValue.Value, remaining);

                case Node.List list when remaining.Count > 0:
                    return
                        remaining[0] < list.Items.Length && remaining[0] >= 0
                        ?
                        ConstantProjection(list.Items[remaining[0]], [.. remaining.Skip(1)])
                        :
                        PineValue.EmptyList;

                case Node.List list:
                    var items = list.Items.Select(item => ConstantProjection(item, [])).ToArray();
                    return items.All(item => item is not null) ? PineValue.List([.. items.Select(item => item!)]) : null;

                case Node.Component:
                case Node.Variant:
                    return null;

                default:
                    throw new NotImplementedException("ConstantProjection does not handle node: " + node.GetType().Name);
            }
        }

        PineControlFlowFragment Build(Node node, IReadOnlyList<int> remaining)
        {
            switch (node)
            {
                case Node.Fixed fixedValue:
                    return
                        PineControlFlowFragment.Empty.AppendOperation(
                            StackInstruction.Push_Literal(ProjectConstant(fixedValue.Value, remaining)));

                case Node.Component component:
                    var componentCode =
                        PineControlFlowFragment.Empty.AppendOperation(
                            StackInstruction.Local_Get(firstLocal + component.Index));

                    var componentSlot = Slots[component.Index];

                    if (!componentSlot.IsMapped)
                    {
                        foreach (var index in remaining)
                            componentCode = componentCode.AppendOperation(StackInstruction.Skip_Head_Const(index));

                        return componentCode;
                    }

                    return
                        componentCode.AppendOperation(
                            StackInstruction.Select_Value_Const(
                                component.Values
                                .Select(
                                    (value, index) => KeyValuePair.Create(componentSlot.Keys[index], ProjectConstant(value, remaining)))
                                .ToImmutableDictionary(),
                                fallbackValue: ProjectConstant(component.Values[0], remaining)));

                case Node.List list when remaining.Count > 0:
                    return
                        remaining[0] < list.Items.Length && remaining[0] >= 0
                        ?
                        Build(list.Items[remaining[0]], [.. remaining.Skip(1)])
                        :
                        PineControlFlowFragment.Empty.AppendOperation(
                            StackInstruction.Push_Literal(PineValue.EmptyList));

                case Node.List list:
                    var items = PineControlFlowFragment.Empty;

                    foreach (var item in list.Items)
                        items = items.Append(Build(item, []));

                    return items.AppendOperation(StackInstruction.Build_List(list.Items.Length));

                case Node.Variant variant:
                    var variantSlot = Slots[variant.Index];
                    var constants = variant.Cases.Select(item => ConstantProjection(item, remaining)).ToArray();

                    if (constants.All(item => item is not null))
                    {
                        if (constants.SequenceEqual(variantSlot.Keys))
                        {
                            return
                                PineControlFlowFragment.Empty.AppendOperation(
                                    StackInstruction.Local_Get(firstLocal + variant.Index));
                        }

                        return
                            PineControlFlowFragment.Empty
                            .AppendOperation(StackInstruction.Local_Get(firstLocal + variant.Index))
                            .AppendOperation(
                                StackInstruction.Select_Value_Const(
                                    constants.Select(
                                        (item, index) => KeyValuePair.Create(variantSlot.Keys[index], item!))
                                    .ToImmutableDictionary(),
                                    fallbackValue: constants[0]!));
                    }

                    var branches = variant.Cases.Select(item => Build(item, remaining)).ToImmutableArray();

                    return
                        PineControlFlowFragment.Empty
                        .AppendOperation(StackInstruction.Local_Get(firstLocal + variant.Index))
                        .Append(
                            new PineControlFlowNode.Switch(
                                PineSwitchKind.Equal,
                                [
                                .. Enumerable.Range(1, branches.Length - 1).Select(
                                    index => new PineSwitchFragmentCase(variantSlot.Keys[index], index - 1))
                                ],
                                branches[0],
                                [.. branches.Skip(1)],
                                1));

                default:
                    throw new NotImplementedException("Reconstruct does not handle node: " + node.GetType().Name);
            }
        }

        return Build(Recipe, path ?? []);
    }

    internal PineValueInProcess Materialize(ReadOnlySpan<PineValueInProcess?> slots) =>
        Build(Recipe, slots);

    private PineValueInProcess Build(Node node, ReadOnlySpan<PineValueInProcess?> slots)
    {
        switch (node)
        {
            case Node.Fixed fixedValue:
                return PineValueInProcess.Create(fixedValue.Value);

            case Node.Component component:
                var value = slots[component.Index] ?? throw new InvalidOperationException("Missing return component.");
                var slot = Slots[component.Index];
                return !slot.IsMapped ? value : PineValueInProcess.Create(component.Values[slot.KeyIndex(value)]);

            case Node.List list:
                var items = new PineValueInProcess[list.Items.Length];

                for (var index = 0; index < items.Length; index++)
                    items[index] = Build(list.Items[index], slots);

                return PineValueInProcess.CreateList(items);

            case Node.Variant variant:
                return Build(variant.Cases[Slots[variant.Index].KeyIndex(slots[variant.Index]!)], slots);

            default:
                throw new NotImplementedException("Build does not handle node: " + node.GetType().Name);
        }
    }

    internal void Deconstruct(PineValueInProcess value, Span<PineValueInProcess?> destination)
    {
        for (var index = 0; index < Slots.Length; index++)
        {
            var slot = Slots[index];

            if (slot.Guards.Any(guard => !Project(value, guard.Path).Evaluate().Equals(guard.Value)))
            {
                destination[index] = PineValueInProcess.EmptyList;
                continue;
            }

            var item = Project(value, slot.Path);

            if (slot.IsMapped)
            {
                var canonical = item.Evaluate();
                var tag = slot.Values.IndexOf(canonical);

                if (tag < 0)
                    throw new InvalidOperationException("Value does not satisfy its return interface.");

                item = PineValueInProcess.Create(slot.Keys[tag]);
            }

            destination[index] = item;
        }
    }

    internal static PineValue Tag(int index) => IntegerEncoding.EncodeSignedInteger(index);

    private static PineValueInProcess Project(PineValueInProcess value, ImmutableArray<int> path) =>
        PineValueInProcess.ValueInProcessFromPathOrNull(value, path.AsSpan()) ?? PineValueInProcess.EmptyList;

    private static PineValue EncodeNode(Node node) =>
        node switch
        {
            Node.Fixed item => PineValue.List([Tag(0), item.Value]),
            Node.Component item => PineValue.List([Tag(1), Tag(item.Index), PineValue.List([.. item.Values])]),
            Node.List item => PineValue.List([Tag(2), PineValue.List([.. item.Items.Select(EncodeNode)])]),

            Node.Variant item =>
            PineValue.List(
                [
                Tag(3),
                Tag(item.Index),
                PineValue.List([.. item.Tags]),
                PineValue.List([.. item.Cases.Select(EncodeNode)])
                ]),

            _ =>
            throw new NotImplementedException("EncodeNode does not handle node: " + node.GetType().Name)
        };

    internal static ReturnValueLayout Decode(PineValue value)
    {
        static PineValue[] Items(PineValue item, int? count = null) =>
            item is PineValue.ListValue list && (count is null || list.Items.Length == count)
            ?
            list.Items.ToArray()
            :
            throw new FormatException("Invalid return layout list.");

        static int Int(PineValue item) =>
            checked((int)IntegerEncoding.ParseSignedIntegerStrict(item).Extract(error => throw new FormatException(error)));

        var root = Items(value, 3);
        var version = Int(root[0]);

        if (version is not (1 or 2))
            throw new FormatException("Unsupported return layout version.");

        var slots =
            Items(root[2]).Select(
                item =>
                {
                    var fields = Items(item, version == 1 ? 3 : 4);
                    var values = Items(fields[1]).ToImmutableArray();

                    return
                        new Slot(
                            [.. StackInstructionMetadataEncoding.DecodePath(fields[0])],
                            values,
                            [
                                .. Items(fields[2]).Select(
                                    guard =>
                                    {
                                        var pair = Items(guard, 2);

                                        return
                                            new Guard(
                                                [.. StackInstructionMetadataEncoding.DecodePath(pair[0])],
                                                pair[1]);
                                    })
                            ],
                            version == 1 ? [.. Enumerable.Range(0, values.Length).Select(Tag)] : [.. Items(fields[3])]);
                }).ToImmutableArray();

        if (slots.Any(
            slot => slot.Path.Any(index => index < 0) || slot.Guards.Any(guard => guard.Path.Any(index => index < 0))))
        {
            throw new FormatException("Invalid return projection path.");
        }

        if (slots.Any(
            slot => slot.Keys.Length != slot.Values.Length || slot.Keys.Distinct().Count() != slot.Keys.Length))
        {
            throw new FormatException("Invalid return slot keys.");
        }

        var usedSlots = new HashSet<int>();

        int SlotIndex(PineValue encoded, ImmutableArray<PineValue> values)
        {
            var index = Int(encoded);

            if (index < 0 || index >= slots.Length || !usedSlots.Add(index) ||
                !slots[index].Values.SequenceEqual(values) || values.Distinct().Count() != values.Length)
            {
                throw new FormatException("Invalid return slot reference or finite map.");
            }

            return index;
        }

        Node DecodeNode(PineValue item, int depth)
        {
            if (depth > 64)
                throw new FormatException("Return layout exceeds nesting limit.");

            var fields = Items(item);

            if (fields.Length == 0)
                throw new FormatException("Missing return layout node kind.");

            switch (fields.Length, Int(fields[0]))

            {
                case (2, 0):
                    return new Node.Fixed(fields[1]);

                case (3, 1):
                    var values = Items(fields[2]).ToImmutableArray();
                    return new Node.Component(SlotIndex(fields[1], values), values);

                case (2, 2):
                    return new Node.List([.. Items(fields[1]).Select(child => DecodeNode(child, depth + 1))]);

                case (4, 3):
                    var tags = Items(fields[2]).ToImmutableArray();
                    var index = SlotIndex(fields[1], tags);
                    var cases = Items(fields[3]);

                    if (tags.IsEmpty || tags.Length != cases.Length)
                        throw new FormatException("Invalid return alternative count.");

                    return new Node.Variant(index, tags, [.. cases.Select(child => DecodeNode(child, depth + 1))]);

                default:
                    throw new FormatException("Invalid return layout node.");
            }
        }

        var recipe = DecodeNode(root[1], 0);

        if (usedSlots.Count != slots.Length)
            throw new FormatException("Unreferenced return slots.");

        var layout = new ReturnValueLayout(recipe, slots);
        return layout.Equals(Canonical) ? Canonical : layout;
    }
}
