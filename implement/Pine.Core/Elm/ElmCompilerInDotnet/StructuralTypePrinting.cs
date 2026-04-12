using System;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Pretty-prints <see cref="StructuralType"/> instances in Elm-like syntax.
/// Choice type tags are always printed on separate lines.
/// Records and tuples switch between single-line and multiline layout depending on
/// the total printed length relative to a configurable line-length threshold.
/// The representation of the <see cref="StructuralType.Self"/> reference is configurable.
/// </summary>
public static class StructuralTypePrinting
{
    /// <summary>
    /// Configuration for pretty-printing a structural type.
    /// </summary>
    /// <param name="LineLengthThreshold">
    /// Maximum line length before records and tuples switch to multiline layout.
    /// </param>
    /// <param name="SelfRepresentation">
    /// The string used to represent the <see cref="StructuralType.Self"/> recursive reference.
    /// </param>
    public record PrintConfig(
        int LineLengthThreshold = 80,
        string SelfRepresentation = "⟳");

    /// <summary>
    /// Prints a <see cref="StructuralType"/> in Elm-like syntax using the given configuration.
    /// </summary>
    public static string Print(StructuralType type, PrintConfig config) =>
        PrintType(type, config, indent: 0, parentRequiresParens: false);

    /// <summary>
    /// Prints a <see cref="StructuralType"/> in Elm-like syntax using default configuration.
    /// </summary>
    public static string Print(StructuralType type) =>
        Print(type, new PrintConfig());

    private static string PrintType(
        StructuralType type,
        PrintConfig config,
        int indent,
        bool parentRequiresParens)
    {
        return type switch
        {
            StructuralType.IntType => "Int",
            StructuralType.FloatType => "Float",
            StructuralType.StringType => "String",
            StructuralType.CharType => "Char",
            StructuralType.BoolType => "Bool",
            StructuralType.TypeVariable tv => StructuralType.VariableIndexToName(tv.Index),
            StructuralType.ConstrainedVariable cv => StructuralType.VariableIndexToName(cv.Index),
            StructuralType.Self => config.SelfRepresentation,
            StructuralType.FunctionType func => PrintFunction(func, config, indent, parentRequiresParens),
            StructuralType.ListType list => PrintList(list, config, indent, parentRequiresParens),
            StructuralType.TupleType tuple => PrintTuple(tuple, config, indent),
            StructuralType.ClosedRecord record => PrintClosedRecord(record, config, indent),
            StructuralType.OpenRecord openRec => PrintOpenRecord(openRec, config, indent),
            StructuralType.ChoiceType choice => PrintChoice(choice, config, indent, parentRequiresParens),
            StructuralType.TypeApplication app => PrintTypeApplication(app, config, indent, parentRequiresParens),

            _ =>
            throw new NotImplementedException(
                "Unexpected StructuralType variant: " + type.GetType().Name)
        };
    }

    private static string PrintFunction(
        StructuralType.FunctionType func,
        PrintConfig config,
        int indent,
        bool parentRequiresParens)
    {
        var parts = new System.Collections.Generic.List<string>();
        StructuralType current = func;

        while (current is StructuralType.FunctionType f)
        {
            // In arrow position, only function-typed arguments need parens.
            // `List a -> b` is fine, but `(a -> b) -> c` needs parens.
            var needsParens = f.ArgumentType is StructuralType.FunctionType;

            parts.Add(
                PrintType(
                    f.ArgumentType,
                    config,
                    indent,
                    parentRequiresParens: needsParens));

            current = f.ReturnType;
        }

        parts.Add(PrintType(current, config, indent, parentRequiresParens: false));

        var result = string.Join(" -> ", parts);

        if (parentRequiresParens)
            return "(" + result + ")";

        return result;
    }

    private static string PrintList(
        StructuralType.ListType list,
        PrintConfig config,
        int indent,
        bool parentRequiresParens)
    {
        var elementStr = PrintType(list.ElementType, config, indent, parentRequiresParens: true);
        var result = "List " + elementStr;

        if (parentRequiresParens)
            return "(" + result + ")";

        return result;
    }

    private static string PrintTuple(
        StructuralType.TupleType tuple,
        PrintConfig config,
        int indent)
    {
        var elements =
            tuple.ElementTypes
            .Select(e => PrintType(e, config, indent + 1, parentRequiresParens: false))
            .ToList();

        var singleLine = "( " + string.Join(", ", elements) + " )";

        if (singleLine.Length <= config.LineLengthThreshold)
            return singleLine;

        // Multiline layout aligned with elm-format.
        // When an element is multiline, break to a new line with nested indentation
        // so the element's continuation lines align with its first line.
        var indentStr = new string(' ', indent * 4);
        var nestedIndentStr = new string(' ', (indent + 1) * 4);
        var sb = new StringBuilder();

        if (elements[0].Contains('\n'))
        {
            sb.Append("(\n");
            sb.Append(nestedIndentStr);
            sb.Append(elements[0]);
        }
        else
        {
            sb.Append("( ");
            sb.Append(elements[0]);
        }

        for (var i = 1; i < elements.Count; i++)
        {
            sb.Append('\n');
            sb.Append(indentStr);

            if (elements[i].Contains('\n'))
            {
                sb.Append(",\n");
                sb.Append(nestedIndentStr);
                sb.Append(elements[i]);
            }
            else
            {
                sb.Append(", ");
                sb.Append(elements[i]);
            }
        }

        sb.Append('\n');
        sb.Append(indentStr);
        sb.Append(')');
        return sb.ToString();
    }

    private static string PrintClosedRecord(
        StructuralType.ClosedRecord record,
        PrintConfig config,
        int indent)
    {
        if (record.Fields.Count is 0)
            return "{}";

        var sortedFields =
            record.Fields
            .OrderBy(kvp => kvp.Key, StringComparer.Ordinal)
            .ToList();

        var fieldValues =
            sortedFields
            .Select(kvp => PrintType(kvp.Value, config, indent + 1, parentRequiresParens: false))
            .ToList();

        // Try single line
        var singleLineParts =
            sortedFields
            .Select((kvp, i) => kvp.Key + " : " + fieldValues[i])
            .ToList();

        var singleLine = "{ " + string.Join(", ", singleLineParts) + " }";

        if (singleLine.Length <= config.LineLengthThreshold)
            return singleLine;

        // Multiline layout aligned with elm-format:
        // When a field value is multiline, break after the colon
        // and place the value on the next line with nested indentation.
        var indentStr = new string(' ', indent * 4);
        var nestedIndentStr = new string(' ', (indent + 1) * 4);
        var sb = new StringBuilder();

        for (var i = 0; i < sortedFields.Count; i++)
        {
            if (i > 0)
            {
                sb.Append('\n');
                sb.Append(indentStr);
                sb.Append(", ");
            }
            else
            {
                sb.Append("{ ");
            }

            sb.Append(sortedFields[i].Key);

            if (fieldValues[i].Contains('\n'))
            {
                sb.Append(" :\n");
                sb.Append(nestedIndentStr);
                sb.Append(fieldValues[i]);
            }
            else
            {
                sb.Append(" : ");
                sb.Append(fieldValues[i]);
            }
        }

        sb.Append('\n');
        sb.Append(indentStr);
        sb.Append('}');
        return sb.ToString();
    }

    private static string PrintOpenRecord(
        StructuralType.OpenRecord openRec,
        PrintConfig config,
        int indent)
    {
        var sortedFields =
            openRec.Fields
            .OrderBy(kvp => kvp.Key, StringComparer.Ordinal)
            .ToList();

        var fieldValues =
            sortedFields
            .Select(kvp => PrintType(kvp.Value, config, indent + 1, parentRequiresParens: false))
            .ToList();

        var rowVarName = StructuralType.VariableIndexToName(openRec.RowVariableIndex);

        if (sortedFields.Count is 0)
            return "{ " + rowVarName + " }";

        var singleLineParts =
            sortedFields
            .Select((kvp, i) => kvp.Key + " : " + fieldValues[i])
            .ToList();

        var singleLine = "{ " + rowVarName + " | " + string.Join(", ", singleLineParts) + " }";

        if (singleLine.Length <= config.LineLengthThreshold)
            return singleLine;

        // Multiline layout aligned with elm-format
        var indentStr = new string(' ', indent * 4);
        var nestedIndentStr = new string(' ', (indent + 1) * 4);
        var sb = new StringBuilder();
        sb.Append("{ ");
        sb.Append(rowVarName);
        sb.Append('\n');
        sb.Append(indentStr);
        sb.Append("| ");
        sb.Append(sortedFields[0].Key);

        if (fieldValues[0].Contains('\n'))
        {
            sb.Append(" :\n");
            sb.Append(nestedIndentStr);
            sb.Append(fieldValues[0]);
        }
        else
        {
            sb.Append(" : ");
            sb.Append(fieldValues[0]);
        }

        for (var i = 1; i < sortedFields.Count; i++)
        {
            sb.Append('\n');
            sb.Append(indentStr);
            sb.Append(", ");
            sb.Append(sortedFields[i].Key);

            if (fieldValues[i].Contains('\n'))
            {
                sb.Append(" :\n");
                sb.Append(nestedIndentStr);
                sb.Append(fieldValues[i]);
            }
            else
            {
                sb.Append(" : ");
                sb.Append(fieldValues[i]);
            }
        }

        sb.Append('\n');
        sb.Append(indentStr);
        sb.Append('}');
        return sb.ToString();
    }

    private static string PrintChoice(
        StructuralType.ChoiceType choice,
        PrintConfig config,
        int indent,
        bool parentRequiresParens)
    {
        var tags =
            choice.Tags
            .OrderBy(kvp => kvp.Key, StringComparer.Ordinal)
            .ToList();

        if (tags.Count is 0)
            return "<empty choice>";

        // When nested inside another type (e.g. as a tag field), print inline with parens.
        if (parentRequiresParens)
        {
            var sb = new StringBuilder();
            sb.Append('(');

            for (var i = 0; i < tags.Count; i++)
            {
                if (i > 0)
                    sb.Append(" | ");

                sb.Append(tags[i].Key);

                var tagFields = tags[i].Value;

                for (var j = 0; j < tagFields.Count; j++)
                {
                    sb.Append(' ');
                    sb.Append(PrintType(tagFields[j], config, indent, parentRequiresParens: true));
                }
            }

            sb.Append(')');
            return sb.ToString();
        }

        {
            var indentStr = new string(' ', indent * 4);

            var sb = new StringBuilder();

            for (var i = 0; i < tags.Count; i++)
            {
                if (i > 0)
                {
                    sb.Append('\n');
                    sb.Append(indentStr);
                }

                if (i is 0)
                    sb.Append("  ");

                else
                    sb.Append("| ");

                sb.Append(tags[i].Key);

                var tagFields = tags[i].Value;

                for (var j = 0; j < tagFields.Count; j++)
                {
                    sb.Append(' ');
                    sb.Append(PrintType(tagFields[j], config, indent, parentRequiresParens: true));
                }
            }

            return sb.ToString();
        }
    }

    private static string PrintTypeApplication(
        StructuralType.TypeApplication app,
        PrintConfig config,
        int indent,
        bool parentRequiresParens)
    {
        // Substitute the type arguments into the generic type so the result
        // prints identically to a type that was concrete from the start.
        // Type parameter order is inferred from first appearance in the structure.
        var typeVarIndices = CollectTypeVariableIndicesInOrder(app.GenericType);

        var substitutions = new System.Collections.Generic.Dictionary<int, StructuralType>();

        for (var i = 0; i < typeVarIndices.Count && i < app.Arguments.Count; i++)
            substitutions[typeVarIndices[i]] = app.Arguments[i];

        var concreteType = StructuralType.Substitute(app.GenericType, substitutions);

        return PrintType(concreteType, config, indent, parentRequiresParens);
    }

    /// <summary>
    /// Collects distinct type variable indices from a structural type in
    /// depth-first, left-to-right first-appearance order.
    /// </summary>
    private static System.Collections.Generic.List<int> CollectTypeVariableIndicesInOrder(
        StructuralType type)
    {
        var result = new System.Collections.Generic.List<int>();
        var seen = new System.Collections.Generic.HashSet<int>();
        CollectTypeVariableIndicesRecursive(type, result, seen);
        return result;
    }

    private static void CollectTypeVariableIndicesRecursive(
        StructuralType type,
        System.Collections.Generic.List<int> result,
        System.Collections.Generic.HashSet<int> seen)
    {
        switch (type)
        {
            case StructuralType.TypeVariable tv:
                if (seen.Add(tv.Index))
                    result.Add(tv.Index);

                break;

            case StructuralType.ConstrainedVariable cv:
                if (seen.Add(cv.Index))
                    result.Add(cv.Index);

                break;

            case StructuralType.FunctionType func:
                CollectTypeVariableIndicesRecursive(func.ArgumentType, result, seen);
                CollectTypeVariableIndicesRecursive(func.ReturnType, result, seen);
                break;

            case StructuralType.ListType list:
                CollectTypeVariableIndicesRecursive(list.ElementType, result, seen);
                break;

            case StructuralType.TupleType tuple:
                for (var i = 0; i < tuple.ElementTypes.Count; i++)
                    CollectTypeVariableIndicesRecursive(tuple.ElementTypes[i], result, seen);

                break;

            case StructuralType.ClosedRecord record:
                {
                    var keys = record.Fields.Keys.ToArray();
                    Array.Sort(keys, StringComparer.Ordinal);

                    for (var i = 0; i < keys.Length; i++)
                        CollectTypeVariableIndicesRecursive(record.Fields[keys[i]], result, seen);

                    break;
                }

            case StructuralType.OpenRecord openRec:
                {
                    if (seen.Add(openRec.RowVariableIndex))
                        result.Add(openRec.RowVariableIndex);

                    var keys = openRec.Fields.Keys.ToArray();
                    Array.Sort(keys, StringComparer.Ordinal);

                    for (var i = 0; i < keys.Length; i++)
                        CollectTypeVariableIndicesRecursive(openRec.Fields[keys[i]], result, seen);

                    break;
                }

            case StructuralType.ChoiceType choice:
                {
                    var keys = choice.Tags.Keys.ToArray();
                    Array.Sort(keys, StringComparer.Ordinal);

                    for (var i = 0; i < keys.Length; i++)
                    {
                        var fields = choice.Tags[keys[i]];

                        for (var j = 0; j < fields.Count; j++)
                            CollectTypeVariableIndicesRecursive(fields[j], result, seen);
                    }

                    break;
                }

            case StructuralType.TypeApplication app:
                CollectTypeVariableIndicesRecursive(app.GenericType, result, seen);

                for (var i = 0; i < app.Arguments.Count; i++)
                    CollectTypeVariableIndicesRecursive(app.Arguments[i], result, seen);

                break;
        }
    }
}
