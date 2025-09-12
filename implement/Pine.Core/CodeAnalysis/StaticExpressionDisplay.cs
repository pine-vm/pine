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
    /// <param name="indentString">String used for one indentation step (e.g., two spaces).</param>
    /// <param name="indentLevel">Initial indentation level to apply to the root expression.</param>
    /// <returns>Formatted string representation.</returns>
    public static string RenderToString(
        this StaticExpression expression,
        string indentString,
        int indentLevel = 0)
    {
        var result = new System.Text.StringBuilder();

        foreach (var (indent, text) in RenderToLines(expression, indentLevel, containerDelimits: true))
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
    /// <param name="indentLevel">Indentation level for the first rendered line of the <paramref name="expression"/>.</param>
    /// <param name="containerDelimits">
    /// If <c>true</c>, the renderer assumes the container (caller) handles delimiters for grouped constructs.
    /// If <c>false</c>, the renderer wraps grouped constructs in parentheses where applicable.
    /// </param>
    /// <returns>Sequence of pairs (indent, text) that together form the full rendering.</returns>
    public static IEnumerable<(int indent, string text)> RenderToLines(
        StaticExpression expression,
        int indentLevel,
        bool containerDelimits)
    {
        switch (expression)
        {
            case StaticExpression.Literal literal:
                {
                    if (ElmValueEncoding.PineValueAsElmValue(
                        literal.Value,
                        additionalReusableDecodings: null,
                        reportNewDecoding: null).IsOkOrNull() is { } elmValue)
                    {
                        var asExpression = ElmValue.RenderAsElmExpression(elmValue);

                        yield return (indentLevel, asExpression.expressionString);
                    }
                    else
                    {
                        yield return (indentLevel, $"Literal: {literal.Value}");
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
                            var itemLines = RenderToLines(item, indentLevel, containerDelimits: true).ToList();

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

                    foreach (var line in RenderToLines(kernel.Input, indentLevel + 1, containerDelimits: true))
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
                        .SelectMany(arg => RenderToLines(arg, indentLevel + 1, containerDelimits: false))
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
                    // if
                    yield return (indentLevel, "if");

                    foreach (var line in RenderToLines(cond.Condition, indentLevel + 1, containerDelimits: true))
                    {
                        yield return line;
                    }

                    // then
                    yield return (indentLevel, "then");

                    foreach (var line in RenderToLines(cond.TrueBranch, indentLevel + 1, containerDelimits: true))
                    {
                        yield return line;
                    }

                    // empty line between then and else for readability (matches existing tests)
                    yield return (0, string.Empty);

                    // else
                    yield return (indentLevel, "else");

                    foreach (var line in RenderToLines(cond.FalseBranch, indentLevel + 1, containerDelimits: true))
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
}
