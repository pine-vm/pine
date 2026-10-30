using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Value encodings for metadata referenced by the versioned stack instruction text format.
/// </summary>
internal static class StackInstructionMetadataEncoding
{
    public static PineValue EncodePath(IEnumerable<int> path) =>
        PineValue.List([.. path.Select(index => IntegerEncoding.EncodeSignedInteger(index))]);

    public static PineValue EncodePaths(IReadOnlyList<IReadOnlyList<int>> paths) =>
        PineValue.List([.. paths.Select(EncodePath)]);

    public static IReadOnlyList<int> DecodePath(PineValue value) =>
        [.. List(value).Select(Int)];

    public static IReadOnlyList<IReadOnlyList<int>> DecodePaths(PineValue value) =>
        [.. List(value).Select(DecodePath)];

    public static PineValue EncodeConstraint(PineValueClass constraint) =>
        PineValue.List(
            [
                .. constraint.ParsedItems.OrderBy(item => item.Key, IntPathComparer.Instance)
                .Select(item => PineValue.List([EncodePath(item.Key), item.Value]))
            ]);

    public static PineValueClass DecodeConstraint(PineValue value) =>
        PineValueClass.Create(
            DecodeConstraintItems(value));

    private static IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> DecodeConstraintItems(PineValue value)
    {
        var items = new SortedDictionary<IReadOnlyList<int>, PineValue>(IntPathComparer.Instance);

        foreach (var item in List(value))
        {
            var pair = List(item, 2);

            if (!items.TryAdd(DecodePath(pair[0]), pair[1]))
                throw new FormatException("Duplicate environment constraint path.");
        }

        return [.. items];
    }

    public static PineValue EncodeJumpTable(IEnumerable<KeyValuePair<PineValue, int>> table) =>
        EncodeSwitchCases(
            table.OrderBy(
                entry => Convert.ToHexStringLower(PineValueHashTree.ComputeHash(entry.Key).Span),
                StringComparer.Ordinal));

    public static PineValue EncodeSliceCases(IEnumerable<SliceSwitchCase> cases) =>
        EncodeSwitchCases(cases.Select(item => new KeyValuePair<PineValue, int>(item.Literal, item.JumpOffset)));

    public static PineValue EncodeSwitchCases(IEnumerable<KeyValuePair<PineValue, int>> cases) =>
        PineValue.List(
            [
            .. cases.Select(
                item => PineValue.List([item.Key, IntegerEncoding.EncodeSignedInteger(item.Value)]))
            ]);

    public static IReadOnlyList<KeyValuePair<PineValue, int>> DecodeSwitchCases(PineValue value) =>
        [
        .. List(value).Select(
            item =>
            {
                var pair = List(item, 2);
                return new KeyValuePair<PineValue, int>(pair[0], Int(pair[1]));
            })
        ];

    public static PineValue EncodePlan(ExpressionTemplatePlan plan)
    {
        var ids = new Dictionary<TemplateValueRecipe, int>(ReferenceEqualityComparer.Instance);
        var nodes = new List<PineValue>();

        int Include(TemplateValueRecipe root)
        {
            var pending = new Stack<(TemplateValueRecipe Source, TemplateValueRecipe? Parsed)>();
            var active = new HashSet<TemplateValueRecipe>(ReferenceEqualityComparer.Instance);
            pending.Push((root, null));

            while (pending.TryPop(out var item))
            {
                if (ids.ContainsKey(item.Source))
                    continue;

                if (item.Parsed is not { } parsed)
                {
                    if (!active.Add(item.Source))
                        throw new InvalidOperationException("Cyclic template recipe.");

                    var node = ParseRecipeNode(item.Source);
                    pending.Push((item.Source, node));

                    switch (node)
                    {
                        case TemplateValueRecipe.Constant:
                        case TemplateValueRecipe.Slot:
                        case TemplateValueRecipe.ProjectedSlot:
                            break;

                        case TemplateValueRecipe.List list:
                            for (var index = list.Items.Count - 1; index >= 0; --index)
                                pending.Push((list.Items[index], null));

                            break;

                        default:
                            throw new NotImplementedException(
                                "EncodePlan does not handle recipe variant: " + node.GetType().Name);
                    }

                    continue;
                }

                var encoded = EncodeRecipeNode(parsed, recipe => ids[recipe]);
                ids.Add(item.Source, nodes.Count);
                nodes.Add(encoded);
                active.Remove(item.Source);
            }

            return ids[root];
        }

        var intermediateIds =
            PineValue.List(
                [
                .. plan.IntermediateRecipes.Select(recipe => IntegerEncoding.EncodeSignedInteger(Include(recipe)))
                ]);

        var terminalId = IntegerEncoding.EncodeSignedInteger(Include(plan.TerminalEnvironmentRecipe));

        return
            PineValue.List(
                [
                    plan.TemplateValue,
                    plan.EncodedTerminalExpression,
                    ExpressionEncoding.EncodeExpressionAsValue(plan.TerminalExpression),
                    IntegerEncoding.EncodeSignedInteger(plan.EnvironmentCount),
                    intermediateIds,
                    terminalId,
                    IntegerEncoding.EncodeSignedInteger(plan.ElidesTerminalEval ? 1 : 0),
                    PineValue.List([.. nodes])
                ]);
    }

    public static ExpressionTemplatePlan DecodePlan(PineValue value)
    {
        var fields = List(value, 8);

        var elides =
            Int(fields[6]) switch
            {
                0 => false,
                1 => true,

                _ =>
                throw new FormatException("Invalid template elision flag.")
            };

        var recipes = new List<TemplateValueRecipe>();

        TemplateValueRecipe Recipe(PineValue id)
        {
            var index = Int(id);

            return
                index >= 0 && index < recipes.Count
                ?
                recipes[index]
                :
                throw new FormatException("Invalid template recipe reference.");
        }

        foreach (var node in List(fields[7]))
            recipes.Add(DecodeRecipeNode(node, Recipe));

        return
            new(
                fields[0],
                fields[1],
                DecodeExpression(fields[2]),
                Int(fields[3]),
                [.. List(fields[4]).Select(Recipe)],
                Recipe(fields[5]),
                elides);
    }

    private static TemplateValueRecipe ParseRecipeNode(TemplateValueRecipe recipe) =>
        recipe switch
        {
            TemplateValueRecipe.Constant constant => constant,
            TemplateValueRecipe.Slot slot => slot,
            TemplateValueRecipe.List list => list with { Items = list.Items.ToArray() },
            TemplateValueRecipe.ProjectedSlot slot => slot with { Path = slot.Path.ToArray() },

            _ =>
            throw new NotImplementedException("ParseRecipeNode does not handle recipe variant: " + recipe.GetType().Name)
        };

    private static PineValue EncodeRecipeNode(TemplateValueRecipe recipe, Func<TemplateValueRecipe, int> id) =>
        recipe switch
        {
            TemplateValueRecipe.Constant constant =>
            PineValue.List([IntegerEncoding.EncodeSignedInteger(0), constant.Value]),

            TemplateValueRecipe.Slot slot =>
            PineValue.List([IntegerEncoding.EncodeSignedInteger(1), IntegerEncoding.EncodeSignedInteger(slot.Index)]),

            TemplateValueRecipe.List list =>
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(2),
                    PineValue.List([.. list.Items.Select(item => IntegerEncoding.EncodeSignedInteger(id(item)))])
                ]),

            TemplateValueRecipe.ProjectedSlot slot =>
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(slot.Index),
                EncodePath(slot.Path)
                ]),

            _ =>
            throw new NotImplementedException("EncodeRecipeNode does not handle recipe variant: " + recipe.GetType().Name)
        };

    private static TemplateValueRecipe DecodeRecipeNode(PineValue value, Func<PineValue, TemplateValueRecipe> recipe)
    {
        var fields = List(value);

        if (fields.Count < 1)
            throw new FormatException("Missing template recipe tag.");

        return (Int(fields[0]), fields.Count) switch
        {
            (0, 2) => new TemplateValueRecipe.Constant(fields[1]),
            (1, 2) => new TemplateValueRecipe.Slot(Int(fields[1])),
            (2, 2) => new TemplateValueRecipe.List([.. List(fields[1]).Select(recipe)]),
            (3, 3) => new TemplateValueRecipe.ProjectedSlot(Int(fields[1]), DecodePath(fields[2])),

            _ =>
            throw new FormatException("Invalid template recipe.")
        };
    }

    public static Expression DecodeExpression(PineValue value) =>
        ExpressionEncoding.ParseExpressionFromValue(value)
        .Extract(error => throw new FormatException("Invalid expression: " + error));

    public static BigInteger DecodeInteger(PineValue value) =>
        value is PineValue.BlobValue blob
        ?
        IntegerEncoding.ParseSignedIntegerStrict(blob.Bytes.Span)
        .Extract(error => throw new FormatException("Invalid integer encoding: " + error))
        :
        throw new FormatException("Expected an integer blob.");

    private static int Int(PineValue value)
    {
        var integer = DecodeInteger(value);

        return
            integer < int.MinValue || integer > int.MaxValue
            ?
            throw new FormatException("Metadata integer exceeds Int32 range.")
            :
            (int)integer;
    }

    private static IReadOnlyList<PineValue> List(PineValue value, int? count = null)
    {
        if (value is not PineValue.ListValue list)
            throw new FormatException("Invalid metadata list.");

        var items = list.Items.ToArray();

        if (count is { } expected && items.Length != expected)
            throw new FormatException("Invalid metadata list.");

        return items;
    }
}
