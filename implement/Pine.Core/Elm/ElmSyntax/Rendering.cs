using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Text;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Specifies the linebreak style to use when rendering Elm source files.
/// </summary>
public enum LinebreakStyle
{
    /// <summary>
    /// Unix-style line endings (LF, \n)
    /// </summary>
    LF,

    /// <summary>
    /// Windows-style line endings (CRLF, \r\n)
    /// </summary>
    CRLF
}

/// <summary>
/// Functionality for rendering Elm source files from the concretized syntax model.
/// This model preserves token locations, enabling more precise rendering.
/// </summary>
public class Rendering
{
    /// <summary>
    /// Detects the linebreak style used in the given text.
    /// Returns LF if no CRLF is found (default style), otherwise returns CRLF.
    /// </summary>
    public static LinebreakStyle DetectLinebreakStyle(string text)
    {
        // Check for CRLF first since it contains LF
        if (text.Contains("\r\n"))
        {
            return LinebreakStyle.CRLF;
        }

        return LinebreakStyle.LF;
    }
    /// <summary>
    /// Represents an item that can be inserted during rendering (comment or incomplete declaration).
    /// </summary>
    private readonly record struct InsertableItem(
        Location StartLocation,
        string Text)
        : IComparable<InsertableItem>
    {
        public static InsertableItem FromComment(Node<string> node) =>
            new(node.Range.Start, node.Value);

        public static InsertableItem FromIncompleteDeclaration(Node<IncompleteDeclaration> node) =>
            new(node.Range.Start, node.Value.OriginalText);

        public int CompareTo(InsertableItem other)
        {
            if (other.StartLocation.Row < StartLocation.Row)
            {
                return 1;
            }

            if (other.StartLocation.Row > StartLocation.Row)
            {
                return -1;
            }

            if (other.StartLocation.Column < StartLocation.Column)
            {
                return 1;
            }

            if (other.StartLocation.Column > StartLocation.Column)
            {
                return -1;
            }

            return 0;
        }
    }

    /// <summary>
    /// Context for tracking position while rendering with location preservation.
    /// </summary>
    private class RenderContext(LinebreakStyle linebreakStyle)
    {
        public StringBuilder Output { get; } = new();

        public int CurrentRow { get; set; } = 1;

        public int CurrentColumn { get; set; } = 1;

        /// <summary>
        /// The linebreak style to use when rendering newlines.
        /// </summary>
        public LinebreakStyle LinebreakStyle { get; } = linebreakStyle;

        /// <summary>
        /// Combined list of comments and incomplete declarations, sorted by position.
        /// </summary>
        public IReadOnlyList<InsertableItem> Insertables { get; set; } = [];

        private int _nextInsertableIndex = 0;

        /// <summary>
        /// Appends a newline using the configured linebreak style.
        /// </summary>
        private void AppendNewline()
        {
            if (LinebreakStyle is LinebreakStyle.CRLF)
            {
                Output.Append("\r\n");
            }
            else
            {
                Output.Append('\n');
            }
        }

        /// <summary>
        /// Advances to the given location by adding spaces or newlines as needed.
        /// Renders any insertables (comments or incomplete declarations) that fall between 
        /// the current position and the target position.
        /// </summary>
        public void AdvanceToLocation(Location targetLocation, int minSpaces = 1)
        {
            // Render any insertables that should appear before reaching the target location
            RenderInsertablesUpTo(targetLocation);

            // Handle row changes
            while (CurrentRow < targetLocation.Row)
            {
                AppendNewline();
                CurrentRow++;
                CurrentColumn = 1;
            }

            // Handle column changes on the same row
            if (CurrentRow == targetLocation.Row)
            {
                var spacesToAdd = targetLocation.Column - CurrentColumn;
                if (spacesToAdd > 0)
                {
                    Output.Append(' ', spacesToAdd);
                    CurrentColumn = targetLocation.Column;
                }
                else if (spacesToAdd < 0 && minSpaces > 0)
                {
                    Output.Append(' ', minSpaces);
                    CurrentColumn += minSpaces;
                }
            }
        }

        private void RenderInsertablesUpTo(Location targetLocation)
        {
            while (_nextInsertableIndex < Insertables.Count)
            {
                var item = Insertables[_nextInsertableIndex];

                var itemIsAfterCurrent = item.StartLocation.Row > CurrentRow ||
                    (item.StartLocation.Row == CurrentRow && item.StartLocation.Column >= CurrentColumn);

                var itemIsBeforeTarget = item.StartLocation.Row < targetLocation.Row ||
                    (item.StartLocation.Row == targetLocation.Row && item.StartLocation.Column < targetLocation.Column);

                if (itemIsAfterCurrent && itemIsBeforeTarget)
                {
                    RenderInsertableItem(item);
                    _nextInsertableIndex++;
                }
                else if (itemIsBeforeTarget)
                {
                    _nextInsertableIndex++;
                }
                else
                {
                    break;
                }
            }
        }

        private void RenderInsertableItem(InsertableItem item)
        {
            AdvanceToLocationSimple(item.StartLocation);

            // Append the text, converting newlines to the configured style
            AppendTextWithLinebreakConversion(item.Text);
        }

        /// <summary>
        /// Appends text to the output, converting any embedded newlines to the configured linebreak style.
        /// </summary>
        private void AppendTextWithLinebreakConversion(string text)
        {
            // First normalize all line breaks to LF, then convert to desired style
            var normalizedText = text.Replace("\r\n", "\n").Replace("\r", "\n");

            if (LinebreakStyle is LinebreakStyle.CRLF)
            {
                // Convert all LF to CRLF
                normalizedText = normalizedText.Replace("\n", "\r\n");
            }

            Output.Append(normalizedText);

            // Track position - count only actual newlines (LF), not CR
            foreach (var ch in text)
            {
                if (ch is '\n')
                {
                    CurrentRow++;
                    CurrentColumn = 1;
                }
                else if (ch is not '\r')  // Skip CR when counting position
                {
                    CurrentColumn++;
                }
            }
        }

        private void AdvanceToLocationSimple(Location targetLocation)
        {
            while (CurrentRow < targetLocation.Row)
            {
                AppendNewline();
                CurrentRow++;
                CurrentColumn = 1;
            }

            if (CurrentRow == targetLocation.Row)
            {
                var spacesToAdd = targetLocation.Column - CurrentColumn;
                if (spacesToAdd > 0)
                {
                    Output.Append(' ', spacesToAdd);
                    CurrentColumn = targetLocation.Column;
                }
            }
        }

        /// <summary>
        /// Renders all remaining insertables (comments and incomplete declarations) that haven't been rendered yet.
        /// Called at the end of rendering to output any trailing items.
        /// </summary>
        public void RenderRemainingInsertables()
        {
            while (_nextInsertableIndex < Insertables.Count)
            {
                var item = Insertables[_nextInsertableIndex];

                var itemIsAfterCurrent = item.StartLocation.Row > CurrentRow ||
                    (item.StartLocation.Row == CurrentRow && item.StartLocation.Column >= CurrentColumn);

                if (itemIsAfterCurrent)
                {
                    RenderInsertableItem(item);
                }

                _nextInsertableIndex++;
            }
        }

        public void AdvanceByMinimum(int minSpaces)
        {
            Output.Append(' ', minSpaces);
            CurrentColumn += minSpaces;
        }

        public void Append(char ch)
        {
            Output.Append(ch);

            if (ch is '\n')
            {
                CurrentRow++;
                CurrentColumn = 1;
            }
            else
            {
                CurrentColumn++;
            }
        }

        public void Append(string text)
        {
            Output.Append(text);

            foreach (var ch in text)
            {
                if (ch is '\n')
                {
                    CurrentRow++;
                    CurrentColumn = 1;
                }
                else
                {
                    CurrentColumn++;
                }
            }
        }

        /// <summary>
        /// Renders the content of a triple-quoted string, escaping special characters
        /// and using the appropriate linebreak style for literal newlines.
        /// </summary>
        public void AppendTripleQuotedStringContent(string value)
        {
            ProcessTripleQuotedStringContent(
                value,
                onChar: ch =>
                {
                    Output.Append(ch);
                    CurrentColumn++;
                },
                onEscapeSequence: escaped =>
                {
                    Output.Append(escaped);
                    CurrentColumn += escaped.Length;
                },
                onNewline: () =>
                {
                    // Use the configured linebreak style for newlines in triple-quoted strings
                    // These are literal characters inside the string, so they should be output
                    // with the same linebreak style as the rest of the file
                    AppendNewline();

                    // Track the newline for row/column counting
                    CurrentRow++;
                    CurrentColumn = 1;
                });
        }
    }

    /// <summary>
    /// Determines whether a quote at the given index should be escaped in a triple-quoted string.
    /// </summary>
    /// <remarks>
    /// A quote needs escaping in two scenarios:
    /// 1. It is part of a sequence of 3+ consecutive quotes within the content (all quotes in such sequences must be escaped)
    /// 2. It is part of a trailing sequence of quotes at the end of the content that would
    ///    combine with the closing """ delimiter to form an invalid sequence
    /// </remarks>
    public static bool ShouldEscapeQuoteInTripleQuotedString(string value, int index)
    {
        // Find the start of this quote sequence
        var sequenceStart = index;
        while (sequenceStart > 0 && value[sequenceStart - 1] is '"')
        {
            sequenceStart--;
        }

        // Count total consecutive quotes in this sequence
        var consecutiveQuotes = 0;
        for (var j = sequenceStart; j < value.Length && value[j] is '"'; j++)
        {
            consecutiveQuotes++;
        }

        // Check if this sequence extends to the end of the string
        var endsAtEndOfString = sequenceStart + consecutiveQuotes == value.Length;

        // Case 1: 3+ consecutive quotes anywhere in the content
        // ALL quotes in this sequence must be escaped
        if (consecutiveQuotes >= 3)
        {
            return true;
        }

        // Case 2: Trailing quotes that would combine with the closing """
        // If we have 1 or 2 quotes at the end, they'll combine with """ to form
        // 4 or 5 consecutive quotes, which is invalid.
        if (endsAtEndOfString && consecutiveQuotes >= 1)
        {
            return true;
        }

        return false;
    }

    /// <summary>
    /// Processes the content of a triple-quoted string, handling all escaping logic in a unified way.
    /// This method invokes the appropriate callback for each character or escape sequence, allowing
    /// the caller to either render the content or calculate its length.
    /// </summary>
    /// <param name="value">The string content to process (without the surrounding triple quotes).</param>
    /// <param name="onChar">Callback invoked for regular (unescaped) characters.</param>
    /// <param name="onEscapeSequence">Callback invoked for escape sequences (e.g., "\\t", "\\\"").</param>
    /// <param name="onNewline">Callback invoked for newline characters.</param>
    public static void ProcessTripleQuotedStringContent(
        string value,
        Action<char> onChar,
        Action<string> onEscapeSequence,
        Action onNewline)
    {
        for (var i = 0; i < value.Length; i++)
        {
            var ch = value[i];

            if (ch is '\n')
            {
                onNewline();
                continue;
            }

            if (EscapeCharForTripleQuoted(ch) is { } escaped)
            {
                onEscapeSequence(escaped);
                continue;
            }

            // Check if this quote character needs to be escaped to avoid breaking
            // the triple-quote delimiter.
            if (ch is '"' && ShouldEscapeQuoteInTripleQuotedString(value, i))
            {
                onEscapeSequence("\\\"");
                continue;
            }

            onChar(ch);
        }
    }

    /// <summary>
    /// Calculates the rendered length of a triple-quoted string's content (excluding the surrounding quotes).
    /// This accounts for escaped characters that expand to multiple output characters.
    /// </summary>
    /// <param name="value">The string content to measure (without the surrounding triple quotes).</param>
    /// <returns>
    /// A tuple containing:
    /// - TotalLength: The total character count of the rendered content
    /// - NewlineCount: The number of newline characters in the content
    /// - LastLineLength: The length of the content after the last newline (or total if no newlines)
    /// </returns>
    public static (int TotalLength, int NewlineCount, int LastLineLength) GetTripleQuotedStringContentLength(string value)
    {
        var totalLength = 0;
        var newlineCount = 0;
        var lastLineLength = 0;

        ProcessTripleQuotedStringContent(
            value,
            onChar: _ =>
            {
                totalLength++;
                lastLineLength++;
            },
            onEscapeSequence: escaped =>
            {
                totalLength += escaped.Length;
                lastLineLength += escaped.Length;
            },
            onNewline: () =>
            {
                // Newline itself counts as 1 character in the output
                totalLength++;
                newlineCount++;
                lastLineLength = 0;
            });

        return (totalLength, newlineCount, lastLineLength);
    }

    /// <summary>
    /// Renders a file to a string while preserving the original source locations.
    /// Uses LF linebreaks by default.
    /// Note: If the file has not been formatted,
    /// consider using Avh4Format.FormatToString instead which formats before rendering.
    /// </summary>
    public static string ToString(File file) =>
        ToString(file, LinebreakStyle.LF);

    /// <summary>
    /// Renders a file to a string while preserving the original source locations.
    /// Uses the specified linebreak style for all newlines.
    /// Note: If the file has not been formatted,
    /// consider using Avh4Format.FormatToString instead which formats before rendering.
    /// </summary>
    public static string ToString(File file, LinebreakStyle linebreakStyle)
    {
        // Combine comments and incomplete declarations into a unified list of insertables
        var insertables = new List<InsertableItem>();

        foreach (var comment in file.Comments)
        {
            insertables.Add(InsertableItem.FromComment(comment));
        }

        foreach (var incompleteDecl in file.IncompleteDeclarations)
        {
            insertables.Add(InsertableItem.FromIncompleteDeclaration(incompleteDecl));
        }

        // Sort by position (row, then column)
        insertables.Sort();

        var context = new RenderContext(linebreakStyle)
        {
            Insertables = insertables
        };

        // Render module definition
        RenderModule(file.ModuleDefinition, context);

        // Render imports
        foreach (var import in file.Imports)
        {
            context.AdvanceToLocation(import.Range.Start);
            RenderImport(import, context);
        }

        // Render declarations - insertables (comments and incomplete declarations) are rendered
        // automatically by AdvanceToLocation when they fall between positions
        foreach (var declaration in file.Declarations)
        {
            context.AdvanceToLocation(declaration.Range.Start);
            RenderDeclaration(declaration, context);
        }

        // Render any remaining insertables (trailing comments and incomplete declarations)
        context.RenderRemainingInsertables();

        // Ensure file ends with a trailing newline (AVH4 elm-format style)
        var result = context.Output.ToString();

        // Use the appropriate newline based on linebreak style
        var trailingNewline = linebreakStyle is LinebreakStyle.CRLF ? "\r\n" : "\n";

        if (!result.EndsWith(trailingNewline))
        {
            // Also check for mismatched linebreak at end
            if (linebreakStyle is LinebreakStyle.CRLF && result.EndsWith('\n') && !result.EndsWith("\r\n"))
            {
                // Remove trailing LF and add CRLF
                result = result[..^1] + "\r\n";
            }
            else if (!result.EndsWith('\n'))
            {
                result += trailingNewline;
            }
        }

        return result;
    }

    private static void RenderModule(
        Node<Module> moduleNode,
        RenderContext context)
    {
        context.AdvanceToLocation(moduleNode.Range.Start);

        switch (moduleNode.Value)
        {
            case Module.NormalModule normalModule:
                context.Append("module");
                context.AdvanceToLocation(normalModule.ModuleData.ModuleName.Range.Start);
                context.Append(RenderModuleName(normalModule.ModuleData.ModuleName.Value));
                context.AdvanceToLocation(normalModule.ModuleData.ExposingTokenLocation);
                context.Append("exposing");
                RenderExposing(normalModule.ModuleData.ExposingList, context);
                break;

            case Module.PortModule portModule:
                context.Append("port");
                context.AdvanceToLocation(portModule.ModuleTokenLocation);
                context.Append("module");
                context.AdvanceToLocation(portModule.ModuleData.ModuleName.Range.Start);
                context.Append(RenderModuleName(portModule.ModuleData.ModuleName.Value));
                context.AdvanceToLocation(portModule.ModuleData.ExposingTokenLocation);
                context.Append("exposing");
                RenderExposing(portModule.ModuleData.ExposingList, context);
                break;

            case Module.EffectModule effectModule:
                context.Append("effect");
                context.AdvanceToLocation(effectModule.ModuleTokenLocation);
                context.Append("module");
                context.AdvanceToLocation(effectModule.ModuleData.ModuleName.Range.Start);
                context.Append(RenderModuleName(effectModule.ModuleData.ModuleName.Value));
                context.AdvanceToLocation(effectModule.ModuleData.ExposingTokenLocation);
                context.Append("exposing");
                RenderExposing(effectModule.ModuleData.ExposingList, context);
                break;

            default:
                throw new NotImplementedException($"Module type '{moduleNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderExposing(
        Node<Exposing> exposingNode,
        RenderContext context)
    {
        // Note: We don't advance to exposingNode.Range.Start because "exposing" keyword
        // has already been rendered by the caller. The exposing node range includes that keyword
        // in its start position, so advancing there would cause issues.

        switch (exposingNode.Value)
        {
            case Exposing.All:
                context.AdvanceByMinimum(1);
                context.Append("(..)");
                break;

            case Exposing.Explicit explicitExposing:
                // Render using stored locations for precise whitespace preservation
                context.AdvanceToLocation(explicitExposing.OpenParenLocation);
                context.Append("(");
                RenderSeparatedList(explicitExposing.Nodes, RenderTopLevelExpose, context);
                context.AdvanceToLocation(explicitExposing.CloseParenLocation);
                context.Append(")");
                break;

            default:
                throw new NotImplementedException($"Exposing type '{exposingNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderTopLevelExpose(
        Node<TopLevelExpose> exposeNode,
        RenderContext context)
    {
        context.AdvanceToLocation(exposeNode.Range.Start);

        switch (exposeNode.Value)
        {
            case TopLevelExpose.InfixExpose infix:
                context.Append($"({infix.Name})");
                break;

            case TopLevelExpose.FunctionExpose func:
                context.Append(func.Name);
                break;

            case TopLevelExpose.TypeOrAliasExpose type:
                context.Append(type.Name);
                break;

            case TopLevelExpose.TypeExpose typeExpose:
                context.Append(typeExpose.ExposedType.Name);
                if (typeExpose.ExposedType.Open is not null)
                {
                    context.Append("(..)");
                }
                break;

            default:
                throw new NotImplementedException($"TopLevelExpose type '{exposeNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderImport(
        Node<Import> importNode,
        RenderContext context)
    {
        context.AdvanceToLocation(importNode.Value.ImportTokenLocation);
        context.Append("import");

        context.AdvanceToLocation(importNode.Value.ModuleName.Range.Start);
        context.Append(RenderModuleName(importNode.Value.ModuleName.Value));

        if (importNode.Value.ModuleAlias is { } alias)
        {
            context.AdvanceToLocation(alias.AsTokenLocation);
            context.Append("as");
            context.AdvanceToLocation(alias.Alias.Range.Start);
            context.Append(RenderModuleName(alias.Alias.Value));
        }

        if (importNode.Value.ExposingList is { } exposingList)
        {
            context.AdvanceToLocation(exposingList.ExposingTokenLocation);
            context.Append("exposing");
            RenderExposing(exposingList.ExposingList, context);
        }
    }

    private static void RenderDeclaration(
        Node<Declaration> declarationNode,
        RenderContext context)
    {
        switch (declarationNode.Value)
        {
            case Declaration.FunctionDeclaration funcDecl:
                RenderFunctionDeclaration(funcDecl, context);
                break;

            case Declaration.AliasDeclaration aliasDecl:
                RenderAliasDeclaration(aliasDecl, context);
                break;

            case Declaration.CustomTypeDeclaration customTypeDecl:
                RenderCustomTypeDeclaration(customTypeDecl, context);
                break;

            case Declaration.InfixDeclaration infixDecl:
                RenderInfixDeclaration(infixDecl, context);
                break;

            case Declaration.PortDeclaration portDecl:
                RenderPortDeclaration(portDecl, context);
                break;

            default:
                throw new NotImplementedException($"Declaration type '{declarationNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderFunctionDeclaration(
        Declaration.FunctionDeclaration funcDecl,
        RenderContext context)
    {
        // Render signature if present
        if (funcDecl.Function.Signature is { } signature)
        {
            context.AdvanceToLocation(signature.Range.Start);
            context.Append(signature.Value.Name.Value);
            context.AdvanceToLocation(signature.Value.ColonLocation);
            context.Append(":");
            RenderTypeAnnotation(signature.Value.TypeAnnotation, context);
        }

        // Render implementation
        var impl = funcDecl.Function.Declaration;
        context.AdvanceToLocation(impl.Range.Start);
        context.Append(impl.Value.Name.Value);

        foreach (var arg in impl.Value.Arguments)
        {
            context.AdvanceToLocation(arg.Range.Start);
            RenderPattern(arg, context);
        }

        context.AdvanceToLocation(impl.Value.EqualsTokenLocation);
        context.Append("=");

        RenderExpression(impl.Value.Expression, context);
    }

    private static void RenderAliasDeclaration(
        Declaration.AliasDeclaration aliasDecl,
        RenderContext context)
    {
        context.AdvanceToLocation(aliasDecl.TypeAlias.TypeTokenLocation);
        context.Append("type");
        context.AdvanceToLocation(aliasDecl.TypeAlias.AliasTokenLocation);
        context.Append("alias");
        context.AdvanceToLocation(aliasDecl.TypeAlias.Name.Range.Start);
        context.Append(aliasDecl.TypeAlias.Name.Value);

        foreach (var generic in aliasDecl.TypeAlias.Generics)
        {
            context.AdvanceToLocation(generic.Range.Start);
            context.Append(generic.Value);
        }

        context.AdvanceToLocation(aliasDecl.TypeAlias.EqualsTokenLocation);
        context.Append("=");

        RenderTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, context);
    }

    private static void RenderCustomTypeDeclaration(
        Declaration.CustomTypeDeclaration customTypeDecl,
        RenderContext context)
    {
        context.AdvanceToLocation(customTypeDecl.TypeDeclaration.TypeTokenLocation);
        context.Append("type");
        context.AdvanceToLocation(customTypeDecl.TypeDeclaration.Name.Range.Start);
        context.Append(customTypeDecl.TypeDeclaration.Name.Value);

        foreach (var generic in customTypeDecl.TypeDeclaration.Generics)
        {
            context.AdvanceToLocation(generic.Range.Start);
            context.Append(generic.Value);
        }

        context.AdvanceToLocation(customTypeDecl.TypeDeclaration.EqualsTokenLocation);
        context.Append("=");

        for (var i = 0; i < customTypeDecl.TypeDeclaration.Constructors.Count; i++)
        {
            var (pipeLocation, constructor) = customTypeDecl.TypeDeclaration.Constructors[i];
            if (pipeLocation is { } pipe)
            {
                context.AdvanceToLocation(pipe);
                context.Append("|");
            }

            context.AdvanceToLocation(constructor.Range.Start);
            RenderValueConstructor(constructor, context);
        }
    }

    private static void RenderValueConstructor(
        Node<ValueConstructor> constructor,
        RenderContext context)
    {
        context.Append(constructor.Value.Name.Value);

        foreach (var arg in constructor.Value.Arguments)
        {
            context.AdvanceToLocation(arg.Range.Start);
            RenderTypeAnnotation(arg, context);
        }
    }

    private static void RenderInfixDeclaration(
        Declaration.InfixDeclaration infixDecl,
        RenderContext context)
    {
        context.AdvanceToLocation(infixDecl.Infix.InfixTokenLocation);
        context.Append("infix");
        context.AdvanceToLocation(infixDecl.Infix.Direction.Range.Start);

        context.Append(infixDecl.Infix.Direction.Value switch
        {
            InfixDirection.Left => "left",
            InfixDirection.Right => "right",
            InfixDirection.Non => "non",

            _ =>
            throw new NotImplementedException(
                $"InfixDirection '{infixDecl.Infix.Direction.Value}' not supported")
        });

        context.AdvanceToLocation(infixDecl.Infix.Precedence.Range.Start);
        context.Append(infixDecl.Infix.Precedence.Value.ToString());
        context.AdvanceToLocation(infixDecl.Infix.Operator.Range.Start);
        context.Append($"({infixDecl.Infix.Operator.Value})");
        context.AdvanceToLocation(infixDecl.Infix.EqualsTokenLocation);
        context.Append("=");
        context.AdvanceToLocation(infixDecl.Infix.FunctionName.Range.Start);
        context.Append(infixDecl.Infix.FunctionName.Value);
    }

    private static void RenderPortDeclaration(
        Declaration.PortDeclaration portDecl,
        RenderContext context)
    {
        context.AdvanceToLocation(portDecl.PortTokenLocation);
        context.Append("port");
        context.AdvanceToLocation(portDecl.Signature.Name.Range.Start);
        context.Append(portDecl.Signature.Name.Value);
        context.AdvanceToLocation(portDecl.Signature.ColonLocation);
        context.Append(":");
        RenderTypeAnnotation(portDecl.Signature.TypeAnnotation, context);
    }

    private static void RenderTypeAnnotation(
        Node<TypeAnnotation> typeAnnotationNode,
        RenderContext context)
    {
        context.AdvanceToLocation(typeAnnotationNode.Range.Start);

        switch (typeAnnotationNode.Value)
        {
            case TypeAnnotation.GenericType genericType:
                context.Append(genericType.Name);
                break;

            case TypeAnnotation.Typed typed:
                var typeName = typed.TypeName.Value;

                if (typeName.ModuleName.Count > 0)
                {
                    context.Append(string.Join(".", typeName.ModuleName) + "." + typeName.Name);
                }
                else
                {
                    context.Append(typeName.Name);
                }

                foreach (var arg in typed.TypeArguments)
                {
                    context.AdvanceToLocation(arg.Range.Start);
                    RenderTypeAnnotation(arg, context);
                }
                break;

            case TypeAnnotation.Unit:
                context.Append("()");
                break;

            case TypeAnnotation.Tupled tupled:
                // Open paren location is at the node's range start
                context.Append("(");
                RenderSeparatedList(tupled.TypeAnnotations, RenderTypeAnnotation, context);
                // Close paren location is at the node's range end - 1 (since range end is after the paren)
                context.AdvanceToLocation(typeAnnotationNode.Range.End with { Column = typeAnnotationNode.Range.End.Column - 1 });
                context.Append(")");
                break;

            case TypeAnnotation.Record record:
                // Open brace location is at the node's range start
                context.Append("{");
                RenderRecordDefinition(record.RecordDefinition, context);
                // Close brace location is at the node's range end - 1 (since range end is after the brace)
                context.AdvanceToLocation(typeAnnotationNode.Range.End with { Column = typeAnnotationNode.Range.End.Column - 1 });
                context.Append("}");
                break;

            case TypeAnnotation.GenericRecord genericRecord:
                // Open brace location is at the node's range start
                context.Append("{");
                context.AdvanceToLocation(genericRecord.GenericName.Range.Start);
                context.Append(genericRecord.GenericName.Value);
                context.AdvanceToLocation(genericRecord.PipeLocation);
                context.Append("|");
                RenderRecordDefinition(genericRecord.RecordDefinition.Value, context);
                // Close brace location is at the node's range end - 1 (since range end is after the brace)
                context.AdvanceToLocation(typeAnnotationNode.Range.End with { Column = typeAnnotationNode.Range.End.Column - 1 });
                context.Append("}");
                break;

            case TypeAnnotation.FunctionTypeAnnotation funcType:
                RenderTypeAnnotation(funcType.ArgumentType, context);
                context.AdvanceToLocation(funcType.ArrowLocation);
                context.Append("->");
                RenderTypeAnnotation(funcType.ReturnType, context);
                break;

            default:
                throw new NotImplementedException($"TypeAnnotation type '{typeAnnotationNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderRecordDefinition(
        RecordDefinition recordDef,
        RenderContext context)
    {
        RenderSeparatedList(recordDef.Fields, RenderRecordField, context);
    }

    /// <summary>
    /// Renders a separated syntax list using the separator locations from the concretized model.
    /// </summary>
    private static void RenderSeparatedList<TNode>(
        SeparatedSyntaxList<TNode> list,
        Action<TNode, RenderContext> renderItem,
        RenderContext context)
    {
        switch (list)
        {
            case SeparatedSyntaxList<TNode>.Empty:
                break;

            case SeparatedSyntaxList<TNode>.NonEmpty nonEmpty:
                renderItem(nonEmpty.First, context);
                foreach (var (separatorLocation, node) in nonEmpty.Rest)
                {
                    context.AdvanceToLocation(separatorLocation);
                    context.Append(",");
                    renderItem(node, context);
                }
                break;
        }
    }

    private static void RenderRecordField(
        Node<RecordField> fieldNode,
        RenderContext context)
    {
        context.AdvanceToLocation(fieldNode.Range.Start);
        context.Append(fieldNode.Value.FieldName.Value);
        context.AdvanceToLocation(fieldNode.Value.ColonLocation);
        context.Append(":");
        RenderTypeAnnotation(fieldNode.Value.FieldType, context);
    }

    private static void RenderRecordExprField(
        RecordExprField field,
        RenderContext context)
    {
        context.AdvanceToLocation(field.FieldName.Range.Start);
        context.Append(field.FieldName.Value);

        // Check if value is on a different row - if so, don't add trailing space after =
        var valueOnDifferentRow = field.ValueExpr.Range.Start.Row > field.FieldName.Range.Start.Row;
        if (valueOnDifferentRow)
        {
            context.Append(" =");
        }
        else
        {
            // Use the stored equals sign location to preserve original whitespace
            context.AdvanceToLocation(field.EqualsLocation, minSpaces: 1);
            context.Append("=");
        }
        RenderExpression(field.ValueExpr, context);
    }

    private static void RenderPatternNode(
        Node<Pattern> patternNode,
        RenderContext context)
    {
        RenderPattern(patternNode, context);
    }

    private static void RenderRecordPatternField(
        Node<string> fieldNode,
        RenderContext context)
    {
        context.AdvanceToLocation(fieldNode.Range.Start);
        context.Append(fieldNode.Value);
    }

    private static void RenderExpression(
        Node<ExpressionSyntax> expressionNode,
        RenderContext context)
    {
        context.AdvanceToLocation(expressionNode.Range.Start);

        switch (expressionNode.Value)
        {
            case ExpressionSyntax.UnitExpr:
                context.Append("()");
                break;

            case ExpressionSyntax.Literal literal:

                if (literal.IsTripleQuoted)
                {
                    context.Append("\"\"\"");

                    context.AppendTripleQuotedStringContent(literal.Value);

                    context.Append("\"\"\"");
                }
                else
                {
                    context.Append($"{RenderStringLiteral(literal.Value)}");
                }

                break;

            case ExpressionSyntax.CharLiteral charLiteral:
                context.Append(RenderCharLiteral(charLiteral.Value));
                break;

            case ExpressionSyntax.Integer integer:
                context.Append(integer.Value.ToString());
                break;

            case ExpressionSyntax.Hex hex:
                context.Append(RenderHexPattern(hex.Value));
                break;

            case ExpressionSyntax.Floatable floatable:
                context.Append(floatable.LiteralText);
                break;

            case ExpressionSyntax.Negation negation:
                context.Append("-");
                RenderExpression(negation.Expression, context);
                break;

            case ExpressionSyntax.ListExpr listExpr:
                // The opening bracket is at the expression's start location (already advanced to)
                context.Append("[");
                RenderSeparatedList(listExpr.Elements, RenderExpression, context);
                // The closing bracket is at the end of the expression range (column - 1 since Range.End is after the bracket)
                context.AdvanceToLocation(expressionNode.Range.End with { Column = expressionNode.Range.End.Column - 1 });
                context.Append("]");
                break;

            case ExpressionSyntax.FunctionOrValue funcOrValue:
                if (funcOrValue.ModuleName.Count > 0)
                {
                    context.Append(string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name);
                }
                else
                {
                    context.Append(funcOrValue.Name);
                }
                break;

            case ExpressionSyntax.IfBlock ifBlock:
                context.AdvanceToLocation(ifBlock.IfTokenLocation);
                context.Append("if");
                RenderExpression(ifBlock.Condition, context);
                context.AdvanceToLocation(ifBlock.ThenTokenLocation);
                context.Append("then");
                RenderExpression(ifBlock.ThenBlock, context);
                context.AdvanceToLocation(ifBlock.ElseTokenLocation);
                context.Append("else");
                RenderExpression(ifBlock.ElseBlock, context);
                break;

            case ExpressionSyntax.PrefixOperator prefixOp:
                context.Append($"({prefixOp.Operator})");
                break;

            case ExpressionSyntax.ParenthesizedExpression parenExpr:
                // expressionNode.Range.Start is the open paren location (already advanced to)
                context.Append("(");
                RenderExpression(parenExpr.Expression, context);
                // Derive close paren location from the containing node's Range.End
                // (Range.End is after the ')', so adjust column by -1)
                context.AdvanceToLocation(expressionNode.Range.End with { Column = expressionNode.Range.End.Column - 1 });
                context.Append(")");
                break;

            case ExpressionSyntax.Application application:
                for (var i = 0; i < application.Arguments.Count; i++)
                {
                    RenderExpression(application.Arguments[i], context);
                }
                break;

            case ExpressionSyntax.OperatorApplication opApp:
                RenderExpression(opApp.Left, context);
                context.AdvanceToLocation(opApp.Operator.Range.Start);
                context.Append(opApp.Operator.Value);
                RenderExpression(opApp.Right, context);
                break;

            case ExpressionSyntax.TupledExpression tupledExpr:
                // Open paren location is derived from containing node's range start (already positioned by RenderExpression)
                context.Append("(");
                RenderSeparatedList(tupledExpr.Elements, RenderExpression, context);
                // Close paren location is derived from containing node's range end - 1
                var closeParenLocation = new Location(expressionNode.Range.End.Row, expressionNode.Range.End.Column - 1);
                context.AdvanceToLocation(closeParenLocation);
                context.Append(")");
                break;

            case ExpressionSyntax.LambdaExpression lambdaExpr:
                context.AdvanceToLocation(lambdaExpr.Lambda.BackslashLocation);
                context.Append("\\");
                foreach (var arg in lambdaExpr.Lambda.Arguments)
                {
                    RenderPattern(arg, context);
                }
                context.AdvanceToLocation(lambdaExpr.Lambda.ArrowLocation);
                context.Append("->");
                RenderExpression(lambdaExpr.Lambda.Expression, context);
                break;

            case ExpressionSyntax.CaseExpression caseExpr:
                context.AdvanceToLocation(caseExpr.CaseBlock.CaseTokenLocation);
                context.Append("case");
                RenderExpression(caseExpr.CaseBlock.Expression, context);
                context.AdvanceToLocation(caseExpr.CaseBlock.OfTokenLocation);
                context.Append("of");

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    RenderPattern(caseItem.Pattern, context);
                    context.AdvanceToLocation(caseItem.ArrowLocation);
                    context.Append("->");
                    RenderExpression(caseItem.Expression, context);
                }
                break;

            case ExpressionSyntax.LetExpression letExpr:
                context.AdvanceToLocation(letExpr.Value.LetTokenLocation);
                context.Append("let");

                foreach (var decl in letExpr.Value.Declarations)
                {
                    RenderLetDeclaration(decl, context);
                }

                context.AdvanceToLocation(letExpr.Value.InTokenLocation);
                context.Append("in");
                RenderExpression(letExpr.Value.Expression, context);
                break;

            case ExpressionSyntax.RecordExpr recordExpr:
                // The opening brace is at the expression's start location (already advanced to)
                context.Append("{");
                RenderSeparatedList(recordExpr.Fields, RenderRecordExprField, context);
                // The closing brace is at the end of the expression range (column - 1 since Range.End is after the brace)
                context.AdvanceToLocation(expressionNode.Range.End with { Column = expressionNode.Range.End.Column - 1 });
                context.Append("}");
                break;

            case ExpressionSyntax.RecordAccess recordAccess:
                RenderExpression(recordAccess.Record, context);
                context.Append(".");
                context.Append(recordAccess.FieldName.Value);
                break;

            case ExpressionSyntax.RecordAccessFunction accessFunc:
                context.Append(accessFunc.FunctionName);
                break;

            case ExpressionSyntax.RecordUpdateExpression recordUpdate:
                // The opening brace is at the expression's start location (already advanced to)
                context.Append("{");
                context.AdvanceToLocation(recordUpdate.RecordName.Range.Start);
                context.Append(recordUpdate.RecordName.Value);
                context.AdvanceToLocation(recordUpdate.PipeLocation);
                context.Append("|");
                RenderSeparatedList(recordUpdate.Fields, RenderRecordExprField, context);
                // The closing brace is at the end of the expression range (column - 1 since Range.End is after the brace)
                context.AdvanceToLocation(expressionNode.Range.End with { Column = expressionNode.Range.End.Column - 1 });
                context.Append("}");
                break;

            case ExpressionSyntax.GLSLExpression glsl:
                context.Append("[glsl|");
                context.Append(glsl.ShaderCode);
                context.Append("|]");
                break;

            default:
                throw new NotImplementedException($"Expression type '{expressionNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderLetDeclaration(
        Node<ExpressionSyntax.LetDeclaration> letDeclNode,
        RenderContext context)
    {
        context.AdvanceToLocation(letDeclNode.Range.Start);

        switch (letDeclNode.Value)
        {
            case ExpressionSyntax.LetDeclaration.LetFunction letFunc:
                if (letFunc.Function.Signature is { } signature)
                {
                    context.AdvanceToLocation(signature.Range.Start);
                    context.Append(signature.Value.Name.Value);
                    context.AdvanceToLocation(signature.Value.ColonLocation);
                    context.Append(":");
                    RenderTypeAnnotation(signature.Value.TypeAnnotation, context);
                }

                var impl = letFunc.Function.Declaration;
                context.AdvanceToLocation(impl.Range.Start);
                context.Append(impl.Value.Name.Value);

                foreach (var arg in impl.Value.Arguments)
                {
                    RenderPattern(arg, context);
                }

                context.AdvanceToLocation(impl.Value.EqualsTokenLocation);
                context.Append("=");
                RenderExpression(impl.Value.Expression, context);
                break;

            case ExpressionSyntax.LetDeclaration.LetDestructuring letDestructuring:
                RenderPattern(letDestructuring.Pattern, context);
                context.AdvanceToLocation(letDestructuring.EqualsTokenLocation);
                context.Append("=");
                RenderExpression(letDestructuring.Expression, context);
                break;

            default:
                throw new NotImplementedException($"LetDeclaration type '{letDeclNode.Value.GetType().Name}' not supported");
        }
    }

    private static void RenderPattern(
        Node<Pattern> patternNode,
        RenderContext context)
    {
        context.AdvanceToLocation(patternNode.Range.Start);

        switch (patternNode.Value)
        {
            case Pattern.AllPattern:
                context.Append("_");
                break;

            case Pattern.VarPattern varPattern:
                context.Append(varPattern.Name);
                break;

            case Pattern.UnitPattern:
                context.Append("()");
                break;

            case Pattern.CharPattern charPattern:
                context.Append(RenderCharLiteral(charPattern.Value));
                break;

            case Pattern.StringPattern stringPattern:
                context.Append($"{RenderStringLiteral(stringPattern.Value)}");
                break;

            case Pattern.IntPattern intPattern:
                context.Append(intPattern.Value.ToString());
                break;

            case Pattern.HexPattern hexPattern:
                context.Append(RenderHexPattern(hexPattern.Value));
                break;

            case Pattern.FloatPattern floatPattern:
                context.Append(FormatFloatForElm(floatPattern.Value));
                break;

            case Pattern.TuplePattern tuplePattern:
                // patternNode.Range.Start is the open paren location (already advanced to)
                context.Append("(");
                RenderSeparatedList(tuplePattern.Elements, RenderPatternNode, context);
                // Derive close paren location from the containing node's Range.End
                // (Range.End is after the ')', so adjust column by -1)
                context.AdvanceToLocation(patternNode.Range.End with { Column = patternNode.Range.End.Column - 1 });
                context.Append(")");
                break;

            case Pattern.RecordPattern recordPattern:
                // patternNode.Range.Start is the open brace location (already advanced to)
                context.Append("{");
                RenderSeparatedList(recordPattern.Fields, RenderRecordPatternField, context);
                // Derive close brace location from the containing node's Range.End
                // (Range.End is after the '}', so adjust column by -1)
                context.AdvanceToLocation(patternNode.Range.End with { Column = patternNode.Range.End.Column - 1 });
                context.Append("}");
                break;

            case Pattern.UnConsPattern unConsPattern:
                RenderPattern(unConsPattern.Head, context);
                context.AdvanceToLocation(unConsPattern.ConsOperatorLocation);
                context.Append("::");
                RenderPattern(unConsPattern.Tail, context);
                break;

            case Pattern.ListPattern listPattern:
                // patternNode.Range.Start is the open bracket location (already advanced to)
                context.Append("[");
                RenderSeparatedList(listPattern.Elements, RenderPatternNode, context);
                // Derive close bracket location from the containing node's Range.End
                // (Range.End is after the ']', so adjust column by -1)
                context.AdvanceToLocation(patternNode.Range.End with { Column = patternNode.Range.End.Column - 1 });
                context.Append("]");
                break;

            case Pattern.NamedPattern namedPattern:
                if (namedPattern.Name.ModuleName.Count > 0)
                {
                    context.Append(string.Join(".", namedPattern.Name.ModuleName) + "." + namedPattern.Name.Name);
                }
                else
                {
                    context.Append(namedPattern.Name.Name);
                }

                foreach (var arg in namedPattern.Arguments)
                {
                    context.AdvanceByMinimum(1); // space before each argument
                    RenderPattern(arg, context);
                }
                break;

            case Pattern.AsPattern asPattern:
                RenderPattern(asPattern.Pattern, context);
                context.AdvanceByMinimum(1); // space before "as"
                context.Append("as");
                context.AdvanceByMinimum(1); // space after "as"
                context.Append(asPattern.Name.Value);
                break;

            case Pattern.ParenthesizedPattern parenPattern:
                // patternNode.Range.Start is the open paren location (already advanced to)
                context.Append("(");
                RenderPattern(parenPattern.Pattern, context);
                // Derive close paren location from the containing node's Range.End
                // (Range.End is after the ')', so adjust column by -1)
                context.AdvanceToLocation(patternNode.Range.End with { Column = patternNode.Range.End.Column - 1 });
                context.Append(")");
                break;

            default:
                throw new NotImplementedException($"Pattern type '{patternNode.Value.GetType().Name}' not supported");
        }
    }

    private static string RenderModuleName(IReadOnlyList<string> moduleName) =>
        string.Join(".", moduleName);

    /// <summary>
    /// Converts the specified string to a simple source code string literal.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Special characters such as backslashes, quotes, and control characters are escaped to produce a valid string literal.
    /// </para>
    /// <para>
    /// Character escaping follows avh4/elm-format behavior:
    /// <see href="https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/elm-format-lib/src/ElmFormat/Render/Box.hs#L897-L928"/>
    /// </para>
    /// </remarks>
    public static string RenderStringLiteral(string value)
    {
        var sb = new StringBuilder();

        for (var i = 0; i < value.Length; i++)
        {
            var ch = value[i];

            switch (ch)
            {
                case '\\':
                    sb.Append("\\\\");
                    break;

                case '\"':
                    sb.Append("\\\"");
                    break;

                case '\n':
                    sb.Append("\\n");
                    break;

                case '\r':
                    // avh4/elm-format uses Unicode escape for carriage return
                    sb.Append("\\u{000D}");
                    break;

                case '\t':
                    sb.Append("\\t");
                    break;

                default:
                    // Check for surrogate pairs (emoji and other characters above U+FFFF)
                    if (char.IsHighSurrogate(ch) && i + 1 < value.Length && char.IsLowSurrogate(value[i + 1]))
                    {
                        var codePoint = char.ConvertToUtf32(ch, value[i + 1]);
                        i++; // Skip the low surrogate
                        sb.Append(char.ConvertFromUtf32(codePoint));
                    }
                    // Escape non-printable characters and whitespace (except regular space).
                    // This matches avh4/elm-format behavior which uses Haskell's:
                    //   - `not (Char.isPrint c)` for non-printable characters
                    //   - `Char.isSpace c` (except ' ') for whitespace that needs escaping
                    // In C#, this is equivalent to: IsControl OR (IsWhiteSpace AND not regular space)
                    else if (CharacterNeedsEscaping(ch))
                    {
                        sb.Append($"\\u{{{(int)ch:X4}}}");
                    }
                    else
                    {
                        sb.Append(ch);
                    }
                    break;
            }
        }

        return "\"" + sb.ToString() + "\"";
    }

    /// <summary>
    /// Determines whether a character needs to be escaped in Elm string/char literals.
    /// </summary>
    /// <remarks>
    /// Matches avh4/elm-format escaping behavior which escapes:
    /// - Non-printable characters (control characters)
    /// - Whitespace characters except regular space (U+0020)
    /// <para>
    /// Reference: <see href="https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/elm-format-lib/src/ElmFormat/Render/Box.hs#L897-L928"/>
    /// </para>
    /// </remarks>
    private static bool CharacterNeedsEscaping(char ch) =>
        char.IsControl(ch) || (char.IsWhiteSpace(ch) && ch != ' ');

    /// <summary>
    /// Escapes a character for use in triple-quoted strings.
    /// </summary>
    /// <remarks>
    /// Character escaping follows avh4/elm-format behavior:
    /// <see href="https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/elm-format-lib/src/ElmFormat/Render/Box.hs#L897-L928"/>
    /// </remarks>
    private static string? EscapeCharForTripleQuoted(char ch) =>
        ch switch
        {
            '\n' =>
            null,  // Literal newlines are preserved in triple-quoted strings

            '\t' =>
            "\\t", // Tabs are escaped

            '\r' =>
            "\\u{000D}", // Carriage return uses Unicode escape

            '\\' =>
            "\\\\", // Backslashes are escaped

            // Escape non-printable characters and whitespace (except regular space)
            _ when CharacterNeedsEscaping(ch) =>
            $"\\u{{{(int)ch:X4}}}",

            _ =>
            null
        };

    /// <summary>
    /// Renders a character literal from its Unicode code point.
    /// </summary>
    /// <remarks>
    /// Character escaping follows avh4/elm-format behavior:
    /// <see href="https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/elm-format-lib/src/ElmFormat/Render/Box.hs#L897-L928"/>
    /// </remarks>
    internal static string RenderCharLiteral(int value)
    {
        var c = char.ConvertFromUtf32(value);

        if (c is "'")
            return "'\\''";

        if (c is "\\")
            return "'\\\\'";

        if (c is "\n")
            return "'\\n'";

        if (c is "\r")
            return "'\\u{000D}'";  // avh4/elm-format uses Unicode escape for carriage return

        if (c is "\t")
            return "'\\t'";

        // Escape non-printable characters and whitespace (except regular space).
        // This matches avh4/elm-format behavior which uses Haskell's:
        //   - `not (Char.isPrint c)` for non-printable characters
        //   - `Char.isSpace c` (except ' ') for whitespace that needs escaping
        // In C#, this is equivalent to: IsControl OR (IsWhiteSpace AND not regular space)
        // For code points > 0xFFFF (like emoji), we check using the first character of the surrogate pair.
        if (value <= 0xFFFF && CharacterNeedsEscaping((char)value))
        {
            // Use Unicode escape sequence for control characters
            // Format with uppercase hex, padded to 4 digits minimum
            return $"'\\u{{{value:X4}}}'";
        }

        return "'" + c + "'";
    }

    /// <summary>
    /// Formats a hexadecimal pattern with padding to match avh4/elm-format behavior.
    /// Pads to lengths of 2, 4, 8, or multiples of 8 (e.g., 8, 16, 32...).
    /// </summary>
    internal static string RenderHexPattern(long value)
    {
        // Convert to hex string (uppercase)
        var hex = value.ToString("X");

        // Determine target length: 2, 4, 8, 16, 32, etc.
        // Matches avh4/elm-format padding behavior

        int targetLength;

        if (hex.Length <= 2)
            targetLength = 2;
        else if (hex.Length <= 4)
            targetLength = 4;
        else if (hex.Length <= 8)
            targetLength = 8;
        else
            // Round up to next multiple of 8
            targetLength = (hex.Length + 7) / 8 * 8;

        // Pad with leading zeros
        var paddedHex = hex.PadLeft(targetLength, '0');

        return "0x" + paddedHex;
    }

    internal static string FormatFloatForElm(double value)
    {
        // Use "R" (round-trip) format which produces the shortest representation
        // that parses back to the same value
        var str = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);

        if (str.Contains('E') || str.Contains('e'))
        {
            // Convert scientific notation to fixed-point decimal
            // The format "F17" uses fixed-point with up to 17 digits
            str = value.ToString("F17", System.Globalization.CultureInfo.InvariantCulture).TrimEnd('0');

            // Make sure we don't end with just a decimal point
            if (str.EndsWith('.'))
            {
                str += "0";
            }
        }

        // Elm requires at least one digit after the decimal point
        if (!str.Contains('.'))
        {
            str += ".0";
        }

        return str;
    }
}
