using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.DotNet;

/// <summary>
/// Decision lowering shared by exact equality and offset-based prefix matches.
/// Matches of the same shape share common slot checks and combine alternatives that share a target.
/// </summary>
internal static class PineCSharpConstantMatch
{
    public static bool CanInline(IReadOnlyList<PineValue> literals, int limit)
    {
        if (limit == 0 || literals.Count == 0)
            return false;

        var first = literals[0];

        if (first is PineValue.BlobValue blob)
        {
            return
                blob.Bytes.Length > 0 &&
                literals.All(item => item is PineValue.BlobValue other && other.Bytes.Length == blob.Bytes.Length) &&
                Enumerable.Range(0, blob.Bytes.Length).Count(
                    index => literals.Any(
                        item =>
                        ((PineValue.BlobValue)item).Bytes.Span[index] != blob.Bytes.Span[index])) <= limit;
        }

        if (first is PineValue.ListValue list)
        {
            return
                list.Items.Length > 0 &&
                literals.All(item => item is PineValue.ListValue other && other.Items.Length == list.Items.Length) &&
                Enumerable.Range(0, list.Items.Length).Count(
                    index => literals.Any(
                        item =>
                        !((PineValue.ListValue)item).Items.Span[index].Equals(list.Items.Span[index]))) <=
                limit;
        }

        throw new NotImplementedException("CanInline does not handle: " + first.GetType().Name);
    }

    public static (bool Blob, int Length) Shape(PineValue literal) =>
        literal switch
        {
            PineValue.BlobValue blob => (true, blob.Bytes.Length),
            PineValue.ListValue list => (false, list.Items.Length),

            _ =>
            throw new NotImplementedException("Shape does not handle: " + literal.GetType().Name)
        };

    public static void Emit<TTarget>(
        StringBuilder text,
        IReadOnlyList<(PineValue Literal, TTarget Target)> cases,
        string memory,
        string offset,
        string suffix,
        Func<PineValue, string> canonicalLiteral,
        Action<TTarget> matched,
        Action? noMatch = null)
    {
        var shape = Shape(cases[0].Literal);
        string Slot(int index) => memory + ".Span[" + offset + (index == 0 ? "" : " + " + index) + "]";

        string Constant(PineValue literal, int index) =>
            shape.Blob
            ?
            ((PineValue.BlobValue)literal).Bytes.Span[index].ToString(
                System.Globalization.CultureInfo.InvariantCulture)
            :
            canonicalLiteral(((PineValue.ListValue)literal).Items.Span[index]);

        bool Same(PineValue a, PineValue b, int index) =>
            shape.Blob
            ?
            ((PineValue.BlobValue)a).Bytes.Span[index] == ((PineValue.BlobValue)b).Bytes.Span[index]
            :
            ((PineValue.ListValue)a).Items.Span[index].Equals(((PineValue.ListValue)b).Items.Span[index]);

        string Test(string slot, string constant) =>
            shape.Blob
            ?
            slot + " == " + constant
            :
            slot + ".Equals(" + constant + ")";

        var positions = Enumerable.Range(0, shape.Length).ToArray();
        var first = cases[0].Literal;
        var common = positions.Where(index => cases.All(item => Same(item.Literal, first, index))).ToArray();
        var varying = positions.Except(common).ToArray();
        var commonTests = common.Select(index => Test(Slot(index), Constant(first, index))).ToArray();
        string? commonCondition = null;

        if (commonTests.Length > 3)
        {
            commonCondition = "common_" + suffix;
            text.AppendLine("bool " + commonCondition + " = true;");

            foreach (var chunk in commonTests.Chunk(3))
                text.AppendLine(commonCondition + " = " + commonCondition + " && " + string.Join(" && ", chunk) + ";");
        }
        else if (commonTests.Length > 0)
            commonCondition = string.Join(" && ", commonTests);

        var mismatch = commonCondition is null ? [] : new[] { "!(" + commonCondition + ")" };

        if (mismatch.Length > 0)
        {
            if (noMatch is not null)
            {
                text.AppendLine("if (" + string.Join(" || ", mismatch) + ")");
                text.AppendLine("{");
                noMatch();
                text.AppendLine("}");
                text.AppendLine("else");
            }
            else
                text.AppendLine("if (" + commonCondition + ")");

            text.AppendLine("{");
        }

        foreach (var index in varying)
        {
            text.AppendLine(
                (shape.Blob ? "byte " : "PineValue ") + "item_" + suffix + "_" + index + " = " + Slot(index) + ";");
        }

        if (varying.Length is 0)
        {
            matched(cases[0].Target);
        }
        else
        {
            // Distinct literals of the same shape cannot overlap.
            foreach (var group in cases.GroupBy(item => item.Target))
            {
                var composite = varying.Length > 1 && group.Count() > 1;

                var alternatives =
                    group.Select(
                        item =>
                        {
                            var tests =
                                varying.Select(
                                    index => Test("item_" + suffix + "_" + index, Constant(item.Literal, index)));

                            var condition = string.Join(" && ", tests);
                            return composite ? "(" + condition + ")" : condition;
                        });

                // Break long OR chains up front instead of relying on repeated formatting passes.
                text.AppendLine("if (" + string.Join(" ||\n", alternatives) + ")");

                text.AppendLine("{");
                matched(group.Key);
                text.AppendLine("}");
            }

            noMatch?.Invoke();
        }

        if (mismatch.Length > 0)
            text.AppendLine("}");
    }
}
