using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.DotNet;

/// <summary>Decision lowering shared by exact equality and offset-based prefix matches.</summary>
internal static class PineCSharpConstantMatch
{
    internal sealed record Case(PineValue Literal, Action<StringBuilder> Matched);

    public static bool CanInline(IReadOnlyList<Case> cases, int limit)
    {
        if (limit == 0 || cases.Count == 0)
            return false;

        var first = cases[0].Literal;

        if (first is PineValue.BlobValue blob)
        {
            return
                blob.Bytes.Length > 0 &&
                cases.All(
                    item =>
                    item.Literal is PineValue.BlobValue other && other.Bytes.Length == blob.Bytes.Length) &&
                Enumerable.Range(0, blob.Bytes.Length).Count(
                    index => cases.Any(
                        item =>
                        ((PineValue.BlobValue)item.Literal).Bytes.Span[index] != blob.Bytes.Span[index])) <= limit;
        }

        if (first is PineValue.ListValue list)
        {
            return
                list.Items.Length > 0 &&
                cases.All(
                    item =>
                    item.Literal is PineValue.ListValue other && other.Items.Length == list.Items.Length) &&
                Enumerable.Range(0, list.Items.Length).Count(
                    index => cases.Any(
                        item =>
                        !((PineValue.ListValue)item.Literal).Items.Span[index].Equals(list.Items.Span[index]))) <=
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

    public static void Emit(
        StringBuilder text,
        IReadOnlyList<Case> cases,
        string memory,
        string offset,
        string suffix,
        Func<PineValue, string> canonicalLiteral,
        Action<StringBuilder>? noMatch = null)
    {
        var shape = Shape(cases[0].Literal);
        string Slot(int index) => memory + ".Span[" + offset + (index == 0 ? "" : " + " + index) + "]";

        string Constant(Case item, int index) =>
            shape.Blob
            ?
            ((PineValue.BlobValue)item.Literal).Bytes.Span[index].ToString(
                System.Globalization.CultureInfo.InvariantCulture)
            :
            canonicalLiteral(((PineValue.ListValue)item.Literal).Items.Span[index]);

        bool Same(Case a, Case b, int index) =>
            shape.Blob
            ?
            ((PineValue.BlobValue)a.Literal).Bytes.Span[index] == ((PineValue.BlobValue)b.Literal).Bytes.Span[index]
            :
            ((PineValue.ListValue)a.Literal).Items.Span[index].Equals(
                ((PineValue.ListValue)b.Literal).Items.Span[index]);

        string Test(string slot, string constant) =>
            shape.Blob
            ?
            slot + " == " + constant
            :
            slot + ".Equals(" + constant + ")";

        var positions = Enumerable.Range(0, shape.Length).ToArray();
        var common = positions.Where(index => cases.All(item => Same(item, cases[0], index))).ToArray();
        var varying = positions.Except(common).ToArray();
        var commonTests = common.Select(index => Test(Slot(index), Constant(cases[0], index))).ToArray();
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
                noMatch(text);
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

        foreach (var item in cases)
        {
            if (varying.Length > 0)
            {
                text.AppendLine(
                    "if (" +
                    string.Join(
                        " && ",
                        varying.Select(
                            index =>
                            Test("item_" + suffix + "_" + index, Constant(item, index)))) + ")");

                text.AppendLine("{");
            }

            item.Matched(text);

            if (varying.Length > 0)
                text.AppendLine("}");

            if (varying.Length == 0)
                break;
        }

        if (varying.Length > 0)
            noMatch?.Invoke(text);

        if (mismatch.Length > 0)
            text.AppendLine("}");
    }
}
