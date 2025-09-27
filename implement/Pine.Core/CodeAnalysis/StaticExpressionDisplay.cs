using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Helpers to render <see cref="StaticExpression{TFunctionIdentifier}"/> instances (and contained <see cref="PineValue"/>s)
/// into a stable, human-readable multi-line string form. The rendering aims to:
/// <list type="bullet">
/// <item><description>Be deterministic across platforms (always uses LF line endings).</description></item>
/// <item><description>Preserve enough structure to be useful in tests and diagnostics.</description></item>
/// <item><description>Avoid allocating intermediate large strings when possible by streaming lines.</description></item>
/// </list>
/// The public API offers both a convenience <c>string</c> renderer and an iterator that yields
/// line/indent pairs for callers that need custom composition.
/// </summary>
public static class StaticExpressionDisplay
{
    /// <summary>
    /// Describes how to render an application of a user-defined function.
    /// Produced by the host program (e.g. via <see cref="StaticProgram.GetFunctionApplicationRendering"/>)
    /// so the display layer can obtain canonical ordering and mapping of arguments.
    /// </summary>
    /// <param name="FunctionName">The canonical name to show for the function in the rendered output.</param>
    /// <param name="FunctionInterface">The static interface containing the ordered parameter reference paths used to reconstruct argument expressions.</param>
    public record FunctionApplicationRendering(
        string FunctionName,
        StaticFunctionInterface FunctionInterface);

    /// <summary>
    /// Render an expression to a multi-line string with the given indentation string.
    /// Line endings are always <c>LF</c> to ensure consistent rendering across platforms.
    /// </summary>
    /// <param name="expression">Expression to render.</param>
    /// <param name="blobValueRenderer">Function to render blob values (receives a <see cref="PineValue.BlobValue"/> and returns the textual form plus a flag indicating if parentheses are needed when embedded).</param>
    /// <param name="functionApplicationRenderer">Function to render function application.</param>
    /// <param name="environmentPathReferenceRenderer">Function to render environment path references.</param>
    /// <param name="indentString">String used for one indentation step (e.g., two spaces).</param>
    /// <param name="indentLevel">Initial indentation level to apply to the root expression.</param>
    /// <returns>Formatted string representation using only <c>\n</c> as line terminators.</returns>
    public static string RenderToString<TFunctionName>(
        this StaticExpression<TFunctionName> expression,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobValueRenderer,
        Func<TFunctionName, FunctionApplicationRendering> functionApplicationRenderer,
        Func<IReadOnlyList<int>, string?> environmentPathReferenceRenderer,
        string indentString,
        int indentLevel = 0)
    {
        var result = new System.Text.StringBuilder();

        foreach (var (indent, text) in
            RenderToLines(
                expression,
                blobValueRenderer,
                functionApplicationRenderer,
                environmentPathReferenceRenderer,
                indentLevel,
                containerDelimits: true))
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
    /// <param name="functionApplicationRenderer">Function to render function application.</param>
    /// <param name="environmentPathReferenceRenderer">Function to render environment path references.</param>
    /// <param name="indentLevel">Indentation level for the first rendered line of the <paramref name="expression"/>.</param>
    /// <param name="containerDelimits">
    /// If <c>true</c>, the renderer assumes the container (caller) handles delimiters for grouped constructs.
    /// If <c>false</c>, the renderer wraps grouped constructs in parentheses where applicable.
    /// </param>
    /// <returns>Sequence of pairs (indent, text) that together form the full rendering.</returns>
    public static IEnumerable<(int indent, string text)> RenderToLines<TFunctionName>(
        StaticExpression<TFunctionName> expression,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobValueRenderer,
        Func<TFunctionName, FunctionApplicationRendering> functionApplicationRenderer,
        Func<IReadOnlyList<int>, string?> environmentPathReferenceRenderer,
        int indentLevel,
        bool containerDelimits)
    {
        if (StaticExpressionExtension.TryParseAsPathToExpression(expression, StaticExpression<TFunctionName>.EnvironmentInstance) is { } path)
        {
            var pathText = environmentPathReferenceRenderer(path);

            if (pathText is not null)
            {
                yield return (indentLevel, pathText);
                yield break;
            }
        }

        switch (expression)
        {
            case StaticExpression<TFunctionName>.Literal literal:
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

            case StaticExpression<TFunctionName>.List list:
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
                                    functionApplicationRenderer,
                                    environmentPathReferenceRenderer,
                                    indentLevel,
                                    containerDelimits: true)
                                .ToList();

                            var isChildAList = item is StaticExpression<TFunctionName>.List;

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
                                    // For multi-line items in lists, indent continuation lines by two spaces so
                                    // they hang under the "[ " or ", " prefix. We already did this for nested
                                    // lists; extend to any continuation line that would otherwise align with
                                    // the list's indent (e.g., 'then'/'else' in conditionals).
                                    if (itemText.Length > 0 && (isChildAList || itemIndent == indentLevel))
                                    {
                                        yield return (itemIndent, "  " + itemText);
                                    }
                                    else
                                    {
                                        // Subsequent lines retain the indentation produced by the child renderer.
                                        yield return (itemIndent, itemText);
                                    }
                                }
                            }
                        }

                        yield return (indentLevel, "]");
                    }

                    yield break;
                }

            case StaticExpression<TFunctionName>.KernelApplication kernel:
                {
                    var prefix = containerDelimits ? string.Empty : "(";

                    // Function name on its own line, input indented beneath
                    yield return (indentLevel, prefix + "Pine_kernel." + kernel.Function);

                    foreach (var line in RenderToLines(
                        kernel.Input,
                        blobValueRenderer,
                        functionApplicationRenderer,
                        environmentPathReferenceRenderer,
                        indentLevel + 1,
                        containerDelimits: false))
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

            case StaticExpression<TFunctionName>.FunctionApplication fnApp:
                {
                    var applicationInfo = functionApplicationRenderer(fnApp.FunctionName);

                    var functionNameString = applicationInfo.FunctionName;

                    var argumentsExprs =
                        applicationInfo.FunctionInterface.ParamsPaths
                        .Select(paramPath => StaticExpressionExtension.BuildReducedPathToExpression(paramPath, fnApp.Arguments))
                        .ToList();

                    var argumentsLines =
                        argumentsExprs
                        .SelectMany(arg =>
                        RenderToLines(
                            arg,
                            blobValueRenderer,
                            functionApplicationRenderer,
                            environmentPathReferenceRenderer,
                            indentLevel + 1,
                            containerDelimits: false))
                        .ToList();

                    if (argumentsLines.Count is 0)
                    {
                        // No arguments: render on a single line
                        yield return (indentLevel, functionNameString);
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

            case StaticExpression<TFunctionName>.Conditional cond:
                {
                    // First: render the head if/then
                    yield return (indentLevel, "if");

                    foreach (var line in RenderToLines(
                        cond.Condition,
                        blobValueRenderer,
                        functionApplicationRenderer,
                        environmentPathReferenceRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    yield return (indentLevel, "then");

                    foreach (var line in RenderToLines(
                        cond.TrueBranch,
                        blobValueRenderer,
                        functionApplicationRenderer,
                        environmentPathReferenceRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    // Render an if/else-if/else chain where nested conditionals in the false branch are folded into
                    // "else if" at the same line to improve readability.

                    // Handle zero or more else-if branches
                    var falseBranch = cond.FalseBranch;

                    while (falseBranch is StaticExpression<TFunctionName>.Conditional nested)
                    {
                        // empty line between then and else-if
                        yield return (0, string.Empty);

                        // else if
                        yield return (indentLevel, "else if");

                        foreach (var line in RenderToLines(
                            nested.Condition,
                            blobValueRenderer,
                            functionApplicationRenderer,
                            environmentPathReferenceRenderer,
                            indentLevel + 1,
                            containerDelimits: true))
                        {
                            yield return line;
                        }

                        yield return (indentLevel, "then");

                        foreach (var line in RenderToLines(
                            nested.TrueBranch,
                            blobValueRenderer,
                            functionApplicationRenderer,
                            environmentPathReferenceRenderer,
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
                        functionApplicationRenderer,
                        environmentPathReferenceRenderer,
                        indentLevel + 1,
                        containerDelimits: true))
                    {
                        yield return line;
                    }

                    yield break;
                }

            case StaticExpression<TFunctionName>.CrashingParseAndEval:
                yield return (indentLevel, "<always_crash>");
                yield break;

            default:
                throw new NotImplementedException(
                    $"Rendering of static expression type {expression.GetType()} is not implemented yet.");
        }
    }

    /// <summary>
    /// Render a <see cref="PineValue"/> into an Elm-like expression string.
    /// This attempts to reuse existing Elm value encodings; if the value decodes to a known Elm value
    /// it uses <see cref="ElmValue.RenderAsElmExpression"/>. Otherwise, list values are rendered recursively
    /// and blobs are delegated to <paramref name="blobRenderer"/>.
    /// </summary>
    /// <param name="value">The value to render.</param>
    /// <param name="blobRenderer">Optional renderer for blob values.</param>
    /// <returns>A tuple where <c>exprText</c> is the textual representation and <c>needsParens</c> indicates if parentheses are required when embedding.</returns>
    /// <exception cref="NotImplementedException">Thrown if a Pine value type is not supported.</exception>
    public static (string exprText, bool needsParens) RenderValueAsExpression(
        PineValue value,
        Func<PineValue.BlobValue, (string exprText, bool needsParens)> blobRenderer)
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

    /// <summary>
    /// Default renderer for blob values producing a short hex form prefixed with <c>Blob 0x</c>.
    /// Parentheses are marked as required to avoid accidental concatenation when embedded.
    /// </summary>
    /// <param name="blobValue">The blob value instance.</param>
    /// <returns>A tuple containing the expression text and a flag indicating parentheses should be used when embedded.</returns>
    public static (string exprText, bool needsParens) DefaultBlobRenderer(PineValue.BlobValue blobValue)
    {
        if (3 < blobValue.Bytes.Length)
        {
            var asStringResult = StringEncoding.StringFromBlobValue(blobValue.Bytes);

            if (asStringResult.IsOkOrNull() is { } asString && ElmValueEncoding.StringIsValidTagName(asString))
            {
                // Render as tag name, as in Elm syntax

                return (asString, false);
            }
        }

        return ("Blob 0x" + Convert.ToHexStringLower(blobValue.Bytes.Span), true);
    }

    /// <summary>
    /// Render a whole <see cref="StaticProgram"/> by rendering each named function in lexicographical order
    /// and joining them with a blank line between definitions.
    /// </summary>
    /// <param name="staticProgram">The static program to render.</param>
    /// <returns>A textual representation of the program using LF line endings.</returns>
    public static string RenderStaticProgram(StaticProgram staticProgram)
    {
        static string? SubstituteEnvironmentPath(IReadOnlyList<int> path, string leafExpr)
        {
            if (path.Count is 0)
            {
                return null;
            }
            else
            {
                return leafExpr + string.Concat(path.Select(offset => "[" + offset.ToString() + "]"));
            }
        }

        return RenderStaticProgram(staticProgram, SubstituteEnvironmentPath);
    }

    /// <summary>
    /// Render a whole <see cref="StaticProgram"/>, allowing a custom substitution for environment path references.
    /// </summary>
    /// <param name="staticProgram">The static program to render.</param>
    /// <param name="substituteEnvironmentPath">
    /// Optional callback to customize how references into the environment are rendered when they extend beyond
    /// declared parameter paths. The callback receives the remaining tail path and the already rendered text for the
    /// matched parameter prefix (e.g., "param_0"). It should return a replacement string to use, or <see langword="null"/>
    /// to fall back to the default rendering. Signature: <c>(IReadOnlyList&lt;int&gt; pathTail, string leafExpr) =&gt; string?</c>.
    /// </param>
    /// <returns>A textual representation of the program using LF line endings.</returns>
    public static string RenderStaticProgram(
        StaticProgram staticProgram,
        Func<IReadOnlyList<int>, string, string?>? substituteEnvironmentPath)
    {
        IReadOnlyList<string> namedFunctionsTexts =
            [..staticProgram.NamedFunctions
            .OrderBy(kvp => kvp.Key.FullName)
            .Select(kvp =>
            RenderNamedFunction(
                staticProgram,
                kvp.Key,
                kvp.Value.body,
                substituteEnvironmentPath: substituteEnvironmentPath))];

        var wholeProgramText =
            string.Join(
                "\n\n",
                namedFunctionsTexts);

        return wholeProgramText;
    }

    /// <summary>
    /// Render a single named function, including its header with parameter placeholders and its body.
    /// The parameter names are inferred from the function interface returned by the program.
    /// </summary>
    /// <param name="staticProgram">The program that provides function application rendering and interfaces.</param>
    /// <param name="functionName">The name of the function to render.</param>
    /// <param name="functionBody">The static expression representing the function body.</param>
    /// <param name="substituteEnvironmentPath">
    /// Optional callback to customize how references into the environment are rendered when they extend beyond
    /// declared parameter paths. The callback receives the remaining tail path and the already rendered text for the
    /// matched parameter prefix (e.g., "param_0"). It should return a replacement string to use, or <c>null</c> to
    /// fall back to the default rendering. Signature: <c>(IReadOnlyList&lt;int&gt; pathTail, string leafExpr) =&gt; string?</c>.
    /// </param>
    /// <returns>The full function definition text.</returns>
    public static string RenderNamedFunction(
        StaticProgram staticProgram,
        DeclQualifiedName functionName,
        StaticExpression<DeclQualifiedName> functionBody,
        Func<IReadOnlyList<int>, string, string?>? substituteEnvironmentPath)
    {
        var functionInterface = staticProgram.GetFunctionApplicationRendering(functionName).FunctionInterface;

        var functionParameters = functionInterface.ParamsPaths;

        var headerText =
            (functionName + " " + string.Join(" ", functionParameters.Select(RenderParamRef))).Trim() + " =";

        string? RenderParamRefCombined(IReadOnlyList<int> path)
        {
            if (substituteEnvironmentPath is not null)
            {
                // Find longest tail handled by 'RenderParamRef'

                for (var tailStartIndex = 0; tailStartIndex < path.Count; tailStartIndex++)
                {
                    if (RenderParamRef(functionInterface)([.. path.SkipLast(tailStartIndex)]) is { } tailExpr)
                    {
                        if (substituteEnvironmentPath([.. path.TakeLast(tailStartIndex)], tailExpr) is { } substituted)
                        {
                            return substituted;
                        }
                        else
                        {
                            break;
                        }
                    }
                }
            }

            return RenderParamRef(functionInterface)(path);
        }

        return
            headerText +
            "\n" +
            RenderToString(
                functionBody,
                blobValueRenderer: DefaultBlobRenderer,
                functionApplicationRenderer: staticProgram.GetFunctionApplicationRendering,
                environmentPathReferenceRenderer: RenderParamRefCombined,
                indentString: "    ",
                indentLevel: 1);
    }

    /// <summary>
    /// Create a renderer that turns environment path references into parameter identifiers for a given function interface.
    /// Returns <c>null</c> for paths that are not parameters in the provided interface.
    /// </summary>
    /// <param name="functionInterface">The static function interface containing the parameter paths.</param>
    /// <returns>A function mapping int-paths to parameter identifier strings, or <c>null</c> when the path is not a parameter.</returns>
    public static Func<IReadOnlyList<int>, string?> RenderParamRef(StaticFunctionInterface functionInterface)
    {
        return path =>
        {
            if (functionInterface.ParamsPaths.Contains(path, IntPathEqualityComparer.Instance))
            {
                return RenderParamRef(path);
            }

            return null;
        };
    }

    private static string RenderParamRef(IReadOnlyList<int> path)
    {
        return "param_" + string.Join('_', path);
    }
}
