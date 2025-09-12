using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;


public static class StaticExpressionDisplay
{
    /// <summary>
    /// Render an expression to a multi-line string with the given indentation string.
    /// Line endings are always <c>LF</c> to ensure consistent rendering across platforms.
    /// </summary>
    /// <param name="expression">Expression to render.</param>
    /// <param name="blobValueRenderer">Function to render blob values.</param>
    /// <param name="indentString">String used for one indentation step (e.g., two spaces).</param>
    /// <param name="indentLevel">Initial indentation level to apply to the root expression.</param>
    /// <returns>Formatted string representation.</returns>
    public static string RenderToString(
        this StaticExpression expression,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobValueRenderer,
        string indentString,
        int indentLevel = 0)
    {
        var result = new System.Text.StringBuilder();

        foreach (var (indent, text) in RenderToLines(expression, blobValueRenderer, indentLevel, containerDelimits: true))
        {
            for (var i = 0; i < indent; ++i)
            {
                result.Append(indentString);
            }

            // Always use LF as line break to ensure consistent rendering across platforms
            result.Append(text);
            result.Append('\n');
        }

        return result.ToString();
    }

    /// <summary>
    /// Render an expression to a sequence of lines, where each element carries the indentation level
    /// and the line text as produced by the renderer.
    /// </summary>
    /// <param name="expression">Expression to render.</param>
    /// <param name="blobValueRenderer">Function to render blob values.</param>
    /// <param name="indentLevel">Indentation level for the first rendered line of the <paramref name="expression"/>.</param>
    /// <param name="containerDelimits">
    /// If <c>true</c>, the renderer assumes the container (caller) handles delimiters for grouped constructs.
    /// If <c>false</c>, the renderer wraps grouped constructs in parentheses where applicable.
    /// </param>
    /// <returns>Sequence of pairs (indent, text) that together form the full rendering.</returns>
    public static IEnumerable<(int indent, string text)> RenderToLines(
        StaticExpression expression,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobValueRenderer,
        int indentLevel,
        bool containerDelimits)
    {
        switch (expression)
        {
            case StaticExpression.Literal literal:
                {
                    var (valueText, needsParens) = RenderValueAsExpression(literal.Value, blobValueRenderer);

                    if (needsParens && !containerDelimits)
                    {
                        yield return (indentLevel, "(" + valueText + ")");
                    }
                    else
                    {
                        yield return (indentLevel, valueText);
                    }

                    yield break;
                }

            case StaticExpression.List list:
                {
                    if (list.Items.Count is 0)
                    {
                        yield return (indentLevel, "[]");
                    }
                    else
                    {
                        for (var i = 0; i < list.Items.Count; ++i)
                        {
                            var item = list.Items[i];

                            var itemPrefixChar = i is 0 ? "[" : ",";

                            // Render item starting at the same indent level as the list line.
                            var itemLines =
                                RenderToLines(
                                    item,
                                    blobValueRenderer,
                                    indentLevel,
                                    containerDelimits: true)
                                .ToList();

                            for (var itemLineIndex = 0; itemLineIndex < itemLines.Count; ++itemLineIndex)
                            {
                                var (itemIndent, itemText) = itemLines[itemLineIndex];

                                if (itemLineIndex is 0)
                                {
                                    // Prefix the first line of the item with '[' or ',' at the list's indent level.
                                    yield return (indentLevel, $"{itemPrefixChar} {itemText}");
                                }
                                else
                                {
                                    // Subsequent lines retain the indentation produced by the child renderer.
                                    yield return (itemIndent, itemText);
                                }
                            }
                        }

                        yield return (indentLevel, "]");
                    }

                    yield break;
                }

            case StaticExpression.KernelApplication kernel:
                {
                    var prefix = containerDelimits ? string.Empty : "(";

                    // Function name on its own line, input indented beneath
                    yield return (indentLevel, prefix + "Pine_kernel." + kernel.Function);

                    foreach (var line in RenderToLines(
                        kernel.Input,
                        blobValueRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    if (!containerDelimits)
                    {
                        // Closing parenthesis at the same indent level as the function name
                        yield return (indentLevel, ")");
                    }

                    yield break;
                }

            case StaticExpression.ParameterReferenceExpression paramRef:
                {
                    yield return (indentLevel, paramRef.ToString());
                    yield break;
                }

            case StaticExpression.FunctionApplication fnApp:
                {
                    var argumentsLines =
                        fnApp.Arguments
                        .SelectMany(arg =>
                        RenderToLines(
                            arg,
                            blobValueRenderer,
                            indentLevel + 1,
                            containerDelimits: false))
                        .ToList();

                    if (argumentsLines.Count is 0)
                    {
                        // No arguments: render on a single line
                        yield return (indentLevel, fnApp.FunctionName);
                        yield break;
                    }

                    var prefix = containerDelimits ? string.Empty : "(";

                    // Function name on its own line
                    yield return (indentLevel, prefix + fnApp.FunctionName);

                    if (containerDelimits)
                    {
                        foreach (var line in argumentsLines)
                        {
                            yield return line;
                        }
                    }
                    else
                    {
                        var lastLine = argumentsLines.Last();

                        for (var lineIndex = 0; lineIndex < argumentsLines.Count - 1; lineIndex++)
                        {
                            var (lIndent, lText) = argumentsLines[lineIndex];
                            // Regular line, just indent as produced by the child renderer.
                            yield return (lIndent, lText);
                        }

                        yield return (lastLine.indent, lastLine.text + ")");
                    }

                    yield break;
                }

            case StaticExpression.Conditional cond:
                {
                    // First: render the head if/then
                    yield return (indentLevel, "if");

                    foreach (var line in RenderToLines(
                        cond.Condition,
                        blobValueRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    yield return (indentLevel, "then");

                    foreach (var line in RenderToLines(
                        cond.TrueBranch,
                        blobValueRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    // Render an if/else-if/else chain where nested conditionals in the false branch are folded into
                    // "else if" at the same line to improve readability.

                    // Handle zero or more else-if branches
                    var falseBranch = cond.FalseBranch;

                    while (falseBranch is StaticExpression.Conditional nested)
                    {
                        // empty line between then and else-if
                        yield return (0, string.Empty);

                        // else if
                        yield return (indentLevel, "else if");

                        foreach (var line in RenderToLines(
                            nested.Condition,
                            blobValueRenderer,
                            indentLevel + 1,
                            containerDelimits: true))
                        {
                            yield return line;
                        }

                        yield return (indentLevel, "then");

                        foreach (var line in RenderToLines(
                            nested.TrueBranch,
                            blobValueRenderer,
                            indentLevel + 1,
                            containerDelimits: true))
                        {
                            yield return line;
                        }

                        falseBranch = nested.FalseBranch;
                    }

                    // Final else
                    yield return (0, string.Empty);
                    yield return (indentLevel, "else");

                    foreach (var line in RenderToLines(
                        falseBranch,
                        blobValueRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    yield break;
                }

            default:
                throw new NotImplementedException(
                    $"Rendering of static expression type {expression.GetType()} is not implemented yet.");
        }
    }

    public static (string exprText, bool needsParens) RenderValueAsExpression(
        PineValue value,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobRenderer = null)
    {
        if (ElmValueEncoding.PineValueAsElmValue(
            value,
            additionalReusableDecodings: null,
            reportNewDecoding: null).IsOkOrNull() is { } elmValue)
        {
            var asExpression = ElmValue.RenderAsElmExpression(elmValue);

            return (asExpression.expressionString, asExpression.needsParens);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            return blobRenderer(blobValue);
        }

        if (value is PineValue.ListValue listValue)
        {
            if (listValue.Items.Length is 0)
            {
                return ("[]", false);
            }

            var itemsRenderings =
                listValue.Items.ToArray()
                .Select(item => RenderValueAsExpression(item, blobRenderer))
                .ToList();

            var combinedText =
                "[" + string.Join(", ", itemsRenderings.Select(ir => ir.exprText)) + "]";

            return (combinedText, false);
        }

        throw new NotImplementedException(
            "Rendering of Pine value type " + value.GetType() + " is not implemented yet: " + value);
    }

    public static (string exprText, bool needsParens) DefaultBlobRenderer(PineValue.BlobValue blobValue)
    {
        return ("Blob 0x" + Convert.ToHexStringLower(blobValue.Bytes.Span), true);
    }
}
