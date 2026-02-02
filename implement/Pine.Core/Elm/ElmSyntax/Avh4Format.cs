using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Format Elm modules following the style of <see href="https://github.com/avh4/elm-format"/>
/// </summary>
public class Avh4Format
{
    /// <summary>
    /// Format an Elm file using AVH4 formatting style and return the formatted source code as a string.
    /// Uses LF linebreaks by default.
    /// This is a convenience method that combines formatting and rendering.
    /// </summary>
    /// <param name="file">The File to format.</param>
    /// <returns>The formatted Elm source code as a string.</returns>
    public static string FormatToString(File file)
    {
        return FormatToString(file, LinebreakStyle.LF);
    }

    /// <summary>
    /// Format an Elm file using AVH4 formatting style and return the formatted source code as a string.
    /// Uses the specified linebreak style for all newlines.
    /// This is a convenience method that combines formatting and rendering.
    /// </summary>
    /// <param name="file">The File to format.</param>
    /// <param name="linebreakStyle">The linebreak style to use (LF or CRLF).</param>
    /// <returns>The formatted Elm source code as a string.</returns>
    public static string FormatToString(File file, LinebreakStyle linebreakStyle)
    {
        var formatted = Format(file);

        return Rendering.ToString(formatted, linebreakStyle);
    }

    /// <summary>
    /// Format an Elm file using AVH4 formatting style, returning a formatted File.
    /// </summary>
    /// <param name="file">The File to format.</param>
    /// <returns>A formatted File with updated token locations.</returns>
    public static File Format(File file)
    {
        // Build the comment query helper
        var commentQueries = new CommentQueryHelper(file.Comments);

        var visitor = new Avh4FormatVisitor(commentQueries, file.IncompleteDeclarations);
        var context = FormattingContext.Initial();

        var (formatted, formattedComments, formattedIncompleteDeclarations) =
            visitor.FormatFile(file, context);

        return
            formatted with
            {
                Comments = formattedComments,
                IncompleteDeclarations = formattedIncompleteDeclarations
            };
    }

    #region Constants and Keywords

    /// <summary>
    /// Indentation constants for Elm formatting.
    /// </summary>
    private static class Indentation
    {
        /// <summary>Standard full indent (4 spaces) for nested content.</summary>
        public const int Full = 4;
    }

    /// <summary>
    /// Named constants for Elm keywords and syntax to replace magic numbers.
    /// </summary>
    private static class Keywords
    {
        public const string Module = "module";
        public const string Port = "port";
        public const string Effect = "effect";
        public const string Infix = "infix";
        public const string Alias = "alias";
        public const string ExposingAll = "exposing (..)";
        public const string Exposing = "exposing";
        public const string Import = "import";
        public const string As = "as";
        public const string Case = "case";
        public const string Of = "of";
        public const string Let = "let";
        public const string In = "in";
        public const string If = "if";
        public const string Then = "then";
        public const string Else = "else";
        public const string Type = "type";
        public const string Unit = "()";
        public const string EmptyList = "[]";
        public const string EmptyRecord = "{}";
        public const string ListOpen = "[";
        public const string ListClose = "]";
        public const string RecordOpen = "{";
        public const string RecordClose = "}";
        public const string TupleOpen = "(";
        public const string TupleClose = ")";
        public const string OpenParen = "(";
        public const string CloseParen = ")";
        public const string OpenBrace = "{";
        public const string CloseBrace = "}";
        public const string Comma = ",";
        public const string Colon = ":";
        public new const string Equals = "=";
        public const string Arrow = "->";
        public const string Minus = "-";
        public const string Pipe = "|";
        public const string TripleQuotes = "\"\"\"";
    }

    #endregion

    #region Formatting Context

    /// <summary>
    /// Formatting context that tracks current position and indentation in spaces.
    /// The constructor is private to ensure all indent mutations go through helper methods,
    /// making indentation logic more robust and less error-prone.
    /// </summary>
    private sealed class FormattingContext
    {
        public int CurrentRow { get; }
        public int CurrentColumn { get; }
        public int IndentSpaces { get; }
        public ImmutableList<Node<string>> Comments { get; }

        private FormattingContext(
            int currentRow,
            int currentColumn,
            int indentSpaces,
            ImmutableList<Node<string>> comments)
        {
            CurrentRow = currentRow;
            CurrentColumn = currentColumn;
            IndentSpaces = indentSpaces;
            Comments = comments;
        }

        /// <summary>
        /// Creates the initial formatting context at row 1, column 1, with no indentation.
        /// </summary>
        public static FormattingContext Initial() =>
            new(currentRow: 1, currentColumn: 1, indentSpaces: 0, comments: []);

        public Location CurrentLocation() =>
            new(CurrentRow, CurrentColumn);

        /// <summary>
        /// Advances to the next row and stays at the current indent column.
        /// </summary>
        public FormattingContext NextRowToIndent(int addMoreLineBreaks = 0)
        {
            return new FormattingContext(
                currentRow: CurrentRow + 1 + addMoreLineBreaks,
                currentColumn: 1 + IndentSpaces,
                indentSpaces: IndentSpaces,
                comments: Comments);
        }

        public FormattingContext NextRowToIndentIfCurrentColumnGreaterThanIndent()
        {
            if (CurrentColumn > 1 + IndentSpaces)
            {
                return NextRowToIndent();
            }
            else
            {
                return this;
            }
        }

        /// <summary>
        /// Adds a blank line by advancing two rows and positioning at the indent column.
        /// </summary>
        public FormattingContext WithBlankLine() =>
            NextRowToIndent().NextRowToIndent();

        /// <summary>
        /// Advances the column by count characters without changing indent.
        /// Use this when advancing past tokens that don't affect nested content alignment.
        /// </summary>
        public FormattingContext Advance(int count) =>
            new(CurrentRow, CurrentColumn + count, IndentSpaces, Comments);

        /// <summary>
        /// Advances the formatting context by one space separator position.
        /// </summary>
        public FormattingContext AdvanceSpaceSeparator() =>
            Advance(1);

        /// <summary>
        /// Advances to the next column that is greater than current indent and a multiple of 4
        /// (columns 5, 9, 13, ...)
        /// </summary>
        public FormattingContext AdvanceToNextIndentLevel()
        {
            var nextColumn = GetNextMultipleOfFourColumn();

            return new FormattingContext(
                currentRow: CurrentRow,
                currentColumn: nextColumn,
                indentSpaces: IndentSpaces,
                comments: Comments);
        }

        /// <summary>
        /// Sets indent to the current column position.
        /// </summary>
        public FormattingContext SetIndentToCurrentColumn()
        {
            return new FormattingContext(
                CurrentRow,
                CurrentColumn,
                indentSpaces: CurrentColumn - 1,
                comments: Comments);
        }

        /// <summary>
        /// Returns to the indent level from an earlier context.
        /// Use this when exiting a nested block to restore the previous indentation.
        /// </summary>
        public FormattingContext ReturnToIndent(FormattingContext prevContext) =>
            new(
                currentRow: CurrentRow,
                currentColumn: CurrentColumn,
                indentSpaces: prevContext.IndentSpaces,
                comments: Comments);

        /// <summary>
        /// Resets indent to zero.
        /// Use this for top-level declarations that should start at column 1.
        /// </summary>
        public FormattingContext ResetIndent() =>
            new(CurrentRow, CurrentColumn, indentSpaces: 0, comments: Comments);

        /// <summary>
        /// Sets column to the indent position (1 + IndentSpaces).
        /// Use this when starting a new line that should be at the current indent level.
        /// </summary>
        public FormattingContext SetIndentColumn() =>
            new(CurrentRow, 1 + IndentSpaces, IndentSpaces, Comments);

        /// <summary>
        /// Creates an indented reference context. This is a common pattern used to establish
        /// a reference point for multiline content that should be indented one level deeper.
        /// Equivalent to: SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn()
        /// </summary>
        public FormattingContext CreateIndentedRef() =>
            SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

        /// <summary>
        /// Calculates the next column that is greater than current position and a multiple of 4.
        /// Returns columns like 5, 9, 13, ... (since columns are 1-based).
        /// </summary>
        private int GetNextMultipleOfFourColumn()
        {
            var currentSpaces = CurrentColumn - 1;
            var nextMultipleSpaces = ((currentSpaces / Indentation.Full) + 1) * Indentation.Full;
            return nextMultipleSpaces + 1;
        }

        /// <summary>
        /// Formats a parsed comment and adds it to this context, returning an updated context.
        /// This encapsulates the common pattern of: get location, calculate end, create formatted comment, add to list, update position.
        /// </summary>
        public FormattingContext FormatAndAddCommentAndNextRowToIndent(Node<ParsedComment> comment)
        {
            return
                FormatAndAddComment(comment)
                .NextRowToIndent();
        }

        /// <summary>
        /// Formats and adds a Node&lt;string&gt; comment (from AST documentation fields).
        /// </summary>
        public FormattingContext FormatAndAddComment(Node<string> comment)
        {
            var commentLocation = CurrentLocation();
            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
            var formattedComment = comment.WithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);

            return new FormattingContext(commentEnd.Row, commentEnd.Column, IndentSpaces, updatedComments);
        }

        /// <summary>
        /// Formats a sequence of comments and adds them to this context, returning an updated context.
        /// </summary>
        public FormattingContext FormatAndAddComments(
            IReadOnlyList<Node<ParsedComment>> commentsToFormat)
        {
            var currentContext = this;

            foreach (var comment in commentsToFormat)
            {
                currentContext = currentContext.FormatAndAddComment(comment);

                if (comment.Value.Category is CommentCategory.LineComment)
                {
                    currentContext = currentContext.NextRowToIndent();
                }
            }

            return currentContext;
        }

        /// <summary>
        /// Formats a parsed comment, adds it to the context, and positions at the comment end location (same row).
        /// Use this when content should continue immediately after the comment on the same line.
        /// </summary>
        public FormattingContext FormatAndAddComment(
            Node<ParsedComment> comment)
        {
            var commentLocation = CurrentLocation();
            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value.Text);
            var formattedComment = comment.ToStringNodeWithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);
            return new FormattingContext(commentEnd.Row, commentEnd.Column, IndentSpaces, updatedComments);
        }

        /// <summary>
        /// Formats a parsed comment, adds it to the context, and positions at the indent column on the next row.
        /// Use this when the next content should start at the current indent level on a new line.
        /// </summary>
        public FormattingContext FormatAndAddCommentThenNextRowToIndent(
            Node<ParsedComment> comment)
        {
            return
                FormatAndAddComment(comment)
                .NextRowToIndent();
        }
    }

    /// <summary>
    /// Result of a formatting operation containing the formatted node and updated context.
    /// Comments are tracked in the FormattingContext.
    /// </summary>
    private record FormattingResult<T>(
        T FormattedNode,
        FormattingContext Context)
    {
        public static FormattingResult<T> Create(
            T formattedNode,
            FormattingContext context) =>
            new(formattedNode, context);
    }

    #endregion

    #region Static Helpers

    /// <summary>
    /// Calculate the end location of a comment given its start location and value.
    /// Also returns whether the comment ends with a newline.
    /// </summary>
    private static Location CalculateCommentEndLocation(
        Location startLocation,
        string commentValue)
    {
        var (lineBreakCount, lastLineLength) = CountLineBreaksAndLastLineLength(commentValue.TrimEnd());

        var endRow = startLocation.Row + lineBreakCount;

        var endColumn =
            lineBreakCount is 0
            ?
            startLocation.Column + commentValue.Length
            :
            lastLineLength + 1; // +1 for 1-based column

        return new Location(endRow, endColumn);
    }

    private static (int lineBreakCount, int lastLineLength) CountLineBreaksAndLastLineLength(string text)
    {
        var lineBreakCount = 0;
        var lastLineStart = 0;

        for (var i = 0; i < text.Length; i++)
        {
            if (text[i] is '\n')
            {
                lineBreakCount++;
                lastLineStart = i + 1;
            }
        }

        var lastLineLength = text.Length - lastLineStart;

        return (lineBreakCount, lastLineLength);
    }

    /// <summary>
    /// Check if a range spans multiple rows.
    /// </summary>
    private static bool SpansMultipleRows(Range range) =>
        range.End.Row > range.Start.Row;

    /// <summary>
    /// Helper to check if a collection of nodes spans multiple rows.
    /// </summary>
    private static bool NodesSpanMultipleRows<T>(IReadOnlyList<Node<T>> nodes)
    {
        if (nodes.Count is 0)
            return false;

        var startRow = nodes[0].Range.Start.Row;

        for (var i = 1; i < nodes.Count; i++)
        {
            if (nodes[i].Range.Start.Row != startRow)
                return true;
        }

        return false;
    }

    /// <summary>
    /// Determines if an expression is "simple" enough that it doesn't need parentheses.
    /// Simple expressions include literals, names, unit, records, lists, and tuples
    /// (which already have their own delimiters).
    /// </summary>
    private static bool IsSimpleExpressionThatDoesNotNeedParens(ExpressionSyntax expr) =>
        expr is ExpressionSyntax.Literal
            or ExpressionSyntax.CharLiteral
            or ExpressionSyntax.Integer
            or ExpressionSyntax.Floatable
            or ExpressionSyntax.FunctionOrValue
            or ExpressionSyntax.UnitExpr
            or ExpressionSyntax.RecordExpr
            or ExpressionSyntax.ListExpr
            or ExpressionSyntax.TupledExpression
            or ExpressionSyntax.RecordAccessFunction;

    #endregion

    #region Generic Separated List Formatter

    /// <summary>
    /// Configuration for separated list formatting, containing alignment references and bracket information.
    /// </summary>
    /// <param name="AlignmentRef">Reference context for aligning separators (e.g., aligned with opening bracket).</param>
    /// <param name="ContentIndentRef">Reference context for content indentation (e.g., past the opening bracket).</param>
    /// <param name="Separator">The separator string (e.g., ", ").</param>
    /// <param name="CloseBracket">The closing bracket string (e.g., "]").</param>
    /// <param name="CloseBracketAlignRef">Optional reference for close bracket alignment. If null, uses AlignmentRef.</param>
    private record SeparatedListConfig(
        FormattingContext AlignmentRef,
        FormattingContext ContentIndentRef,
        string Separator,
        string CloseBracket,
        FormattingContext? CloseBracketAlignRef = null);

    /// <summary>
    /// Determines the comment placement style for separated lists.
    /// </summary>
    private enum SeparatedListCommentStyle
    {
        /// <summary>
        /// List-style: Comments with blank lines go at alignment ref, preserving blank lines.
        /// </summary>
        ListStyle,

        /// <summary>
        /// Tuple-style: All comments go at content indent inline with separators.
        /// </summary>
        TupleStyle
    }

    /// <summary>
    /// Generic higher-order function for formatting a multiline separated list with comment handling.
    /// This function abstracts the common pattern of formatting lists, tuples, records, etc.
    /// with proper comment placement between items and around separators.
    /// This version works with Node-wrapped items and extracts the Range automatically.
    /// </summary>
    /// <typeparam name="TItem">The inner type of items (items are Node&lt;TItem&gt;).</typeparam>
    /// <typeparam name="TResult">The type of formatted items.</typeparam>
    /// <param name="separatedList">The non-empty separated list to format.</param>
    /// <param name="formatItem">Delegate to format a single item.</param>
    /// <param name="commentQueries">Helper for querying comments from the original source.</param>
    /// <param name="config">Configuration for alignment, separator, and close bracket.</param>
    /// <param name="startContext">The formatting context after the opening bracket.</param>
    /// <param name="containerRange">The original source range of the container.</param>
    /// <param name="commentStyle">The comment placement style to use.</param>
    /// <returns>A tuple of the formatted separated list and the final context.</returns>
    private static (SeparatedSyntaxList<TResult>.NonEmpty FormattedList, FormattingContext FinalContext)
        FormatMultilineSeparatedList<TItem, TResult>(
            SeparatedSyntaxList<Node<TItem>>.NonEmpty separatedList,
            System.Func<Node<TItem>, FormattingContext, FormattingResult<TResult>> formatItem,
            CommentQueryHelper commentQueries,
            SeparatedListConfig config,
            FormattingContext startContext,
            Range containerRange,
            SeparatedListCommentStyle commentStyle)
    {
        // Delegate to the general version with Range extraction from Node
        return FormatMultilineSeparatedListGeneral(
            separatedList: separatedList,
            formatItem: formatItem,
            getItemRange: item => item.Range,
            commentQueries: commentQueries,
            config: config,
            startContext: startContext,
            containerRange: containerRange,
            commentStyle: commentStyle);
    }

    /// <summary>
    /// General version of multiline separated list formatter that works with any item type.
    /// Uses a delegate to extract the Range from items for comment placement.
    /// </summary>
    private static (SeparatedSyntaxList<TResult>.NonEmpty FormattedList, FormattingContext FinalContext)
        FormatMultilineSeparatedListGeneral<TItem, TResult>(
            SeparatedSyntaxList<TItem>.NonEmpty separatedList,
            System.Func<TItem, FormattingContext, FormattingResult<TResult>> formatItem,
            System.Func<TItem, Range> getItemRange,
            CommentQueryHelper commentQueries,
            SeparatedListConfig config,
            FormattingContext startContext,
            Range containerRange,
            SeparatedListCommentStyle commentStyle)
    {
        var firstItem = separatedList.First;
        var restItems = separatedList.Rest;
        var firstItemRange = getItemRange(firstItem);

        // Check for comments before the first element
        var commentsBeforeFirst =
            commentQueries.GetAfterLocationBeforeRow(containerRange.Start, firstItemRange.Start.Row);

        var firstElemCtx = startContext;

        if (commentsBeforeFirst.Count > 0)
        {
            foreach (var comment in commentsBeforeFirst)
            {
                firstElemCtx = firstElemCtx.ReturnToIndent(config.ContentIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
            }
        }

        // Format first element
        var firstElemResult = formatItem(firstItem, firstElemCtx);
        var elemCtx = firstElemResult.Context;

        // Check for trailing comment on the same row as the first element
        var firstElemTrailingComment = commentQueries.GetTrailing(firstItemRange);
        if (firstElemTrailingComment is not null)
        {
            // Only handle the trailing comment if formatItem didn't already process it.
            // We detect this by checking if we're still on the same row AND before the comment's column.
            // If formatItem handled it, the context would have advanced past the comment.
            var currLoc = elemCtx.CurrentLocation();
            if (currLoc.Row == firstItemRange.End.Row && currLoc.Column <= firstElemTrailingComment.Range.Start.Column)
            {
                elemCtx = elemCtx.Advance(1); // space before comment
                elemCtx = elemCtx.FormatAndAddComment(firstElemTrailingComment);
            }
        }

        // Build rest list with proper separator locations
        var formattedRestItems = new List<(Location SeparatorLocation, TResult Node)>();

        // Track previous element for comment detection
        var prevItemRange = firstItemRange;

        // Subsequent elements each on new line with separator at start, aligned with opening bracket
        for (var i = 0; i < restItems.Count; i++)
        {
            var (originalSepLoc, currItem) = restItems[i];
            var currItemRange = getItemRange(currItem);

            // Get the original comma row for this element
            var commaRowInOriginal = originalSepLoc.Row;

            // Get all comments between previous element and current element
            var allCommentsBetween = commentQueries.GetBetweenRows(prevItemRange.End.Row, currItemRange.Start.Row);

            // Split comments based on original separator location
            var commentsBeforeComma = new List<Node<ParsedComment>>();
            var commentsAfterComma = new List<Node<ParsedComment>>();

            if (commaRowInOriginal > 0)
            {
                // Split comments: those before the comma row go before, those on or after go after
                foreach (var comment in allCommentsBetween)
                {
                    if (comment.Range.Start.Row < commaRowInOriginal)
                    {
                        commentsBeforeComma.Add(comment);
                    }
                    else
                    {
                        commentsAfterComma.Add(comment);
                    }
                }
            }
            else
            {
                // No separator info - all comments go after comma
                commentsAfterComma.AddRange(allCommentsBetween);
            }

            // Helper to check if originally compact (multiple items on first row)
            bool IsOriginallyCompact()
            {
                var firstRow = firstItemRange.Start.Row;
                var count = 1; // First item
                foreach (var (_, restNode) in restItems)
                {
                    if (getItemRange(restNode).Start.Row == firstRow)
                        count++;
                }
                return count > 1;
            }

            // Render comments BEFORE the comma (trailing comments for previous element)
            var handledCommentsBeforeComma = false;
            if (commentsBeforeComma.Count > 0)
            {
                var firstComment = commentsBeforeComma[0];
                var hasBlankLineBeforeComment = firstComment.Range.Start.Row > prevItemRange.End.Row + 1;
                var wasOriginallyCompact = IsOriginallyCompact();

                // Use the comment style to determine formatting
                if (commentStyle is SeparatedListCommentStyle.TupleStyle)
                {
                    // Tuple-style: all comments go at content indent
                    foreach (var comment in commentsBeforeComma)
                    {
                        elemCtx = elemCtx.ReturnToIndent(config.ContentIndentRef).NextRowToIndent();
                        elemCtx = elemCtx.FormatAndAddComment(comment);
                    }
                }
                else if (hasBlankLineBeforeComment || wasOriginallyCompact || commentsBeforeComma.Count > 1)
                {
                    // List-style with blank line: format as
                    // blank line, comment(s) at alignment ref, then separator
                    elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).NextRowToIndent();
                    elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).NextRowToIndent();
                    foreach (var comment in commentsBeforeComma)
                    {
                        elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                    handledCommentsBeforeComma = true;
                }
                else
                {
                    // List-style single comment without blank line - place at content indent
                    foreach (var comment in commentsBeforeComma)
                    {
                        elemCtx = elemCtx.ReturnToIndent(config.ContentIndentRef).NextRowToIndent();
                        elemCtx = elemCtx.FormatAndAddComment(comment);
                    }
                }
            }

            // Move to separator row (skip if we just formatted comments at alignment)
            if (!handledCommentsBeforeComma)
            {
                elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).NextRowToIndent();
            }
            else
            {
                // After FormatAndAddComment, we're already at next row, just set indent
                elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).SetIndentColumn();
            }
            var separatorLoc = elemCtx.CurrentLocation();

            if (commentsAfterComma.Count is not 0)
            {
                // Check for blank lines and compact format
                // For "after comma" comments, check if there's a blank line between the separator and the comment
                // A comment on the same line as the separator should be placed inline with it
                var firstComment = commentsAfterComma[0];
                var isCommentOnSameLineAsSeparator = firstComment.Range.Start.Row == commaRowInOriginal;
                var hasBlankLineAfterSeparator = firstComment.Range.Start.Row > commaRowInOriginal + 1;
                var wasOriginallyCompact = IsOriginallyCompact();

                // Use the comment style to determine formatting
                // Place comment inline with separator if:
                // - TupleStyle always uses inline
                // - ListStyle: comment is on same line as separator, OR no blank line after separator and not compact
                if (commentStyle is SeparatedListCommentStyle.TupleStyle ||
                    isCommentOnSameLineAsSeparator ||
                    (!hasBlankLineAfterSeparator && !wasOriginallyCompact && commentsAfterComma.Count is 1))
                {
                    // Comments on same line as separator: ", -- comment\n  element"
                    // For tuple-style, all comments go inline; for list-style, only single comments without blank line
                    elemCtx = elemCtx.Advance(config.Separator.Length);
                    foreach (var comment in commentsAfterComma)
                    {
                        elemCtx = elemCtx.ReturnToIndent(config.ContentIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                    var elemResult = formatItem(currItem, elemCtx);
                    formattedRestItems.Add((separatorLoc, elemResult.FormattedNode));
                    elemCtx = elemResult.Context.ReturnToIndent(startContext);
                }
                else
                {
                    // Comments between elements with blank line: format as
                    // blank line, comment(s) on own lines, ", element"
                    elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).NextRowToIndent();
                    foreach (var comment in commentsAfterComma)
                    {
                        elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                    separatorLoc = elemCtx.CurrentLocation();
                    elemCtx = elemCtx.Advance(config.Separator.Length);
                    var elemResult = formatItem(currItem, elemCtx);
                    formattedRestItems.Add((separatorLoc, elemResult.FormattedNode));
                    elemCtx = elemResult.Context.ReturnToIndent(startContext);
                }
            }
            else
            {
                elemCtx = elemCtx.Advance(config.Separator.Length);
                var elemResult = formatItem(currItem, elemCtx);
                formattedRestItems.Add((separatorLoc, elemResult.FormattedNode));
                elemCtx = elemResult.Context;

                // Check for trailing comment on the same row as the element
                var trailingComment = commentQueries.GetTrailing(currItemRange);
                if (trailingComment is not null)
                {
                    // Only handle the trailing comment if formatItem didn't already process it.
                    // We detect this by checking if we're still on the same row AND before the comment's column.
                    var currLoc = elemCtx.CurrentLocation();
                    if (currLoc.Row == currItemRange.End.Row && currLoc.Column <= trailingComment.Range.Start.Column)
                    {
                        elemCtx = elemCtx.Advance(1); // space before comment
                        elemCtx = elemCtx.FormatAndAddComment(trailingComment);
                    }
                }
            }

            prevItemRange = currItemRange;
        }

        // Check for comments between last element and closing bracket
        var lastItem = restItems.Count > 0 ? restItems[restItems.Count - 1].Node : firstItem;
        var lastItemRange = getItemRange(lastItem);

        var commentsBeforeClose =
            commentQueries.GetBetweenRows(lastItemRange.End.Row, containerRange.End.Row);

        // Check for blank line before the first comment (only once, not per comment)
        var hasBlankLineBeforeFirstComment =
            commentsBeforeClose.Count > 0 &&
            commentsBeforeClose[0].Range.Start.Row > lastItemRange.End.Row + 1;

        if (commentStyle is not SeparatedListCommentStyle.TupleStyle && hasBlankLineBeforeFirstComment)
        {
            elemCtx = elemCtx.NextRowToIndent(); // blank line
        }

        foreach (var comment in commentsBeforeClose)
        {
            if (commentStyle is SeparatedListCommentStyle.TupleStyle)
            {
                // Tuple-style: comments at content indent
                elemCtx = elemCtx.ReturnToIndent(config.ContentIndentRef).NextRowToIndent();
                elemCtx = elemCtx.FormatAndAddComment(comment);
            }
            else
            {
                // List-style: comment at alignment ref
                elemCtx = elemCtx.ReturnToIndent(config.AlignmentRef).NextRowToIndent();
                elemCtx = elemCtx.FormatAndAddComment(comment);
            }
        }

        // Closing bracket on new line, aligned with opening bracket (or custom alignment if specified)
        var closeBracketAlignRef = config.CloseBracketAlignRef ?? config.AlignmentRef;
        var closeCtx = elemCtx.ReturnToIndent(closeBracketAlignRef).NextRowToIndent();
        var afterClose = closeCtx.Advance(config.CloseBracket.Length);

        var formattedList = new SeparatedSyntaxList<TResult>.NonEmpty(
            firstElemResult.FormattedNode,
            formattedRestItems);

        return (formattedList, afterClose.ReturnToIndent(startContext));
    }

    /// <summary>
    /// Configuration for single-line separated list formatting.
    /// </summary>
    /// <param name="OpenBracket">The opening bracket string (e.g., "[ " or "( ").</param>
    /// <param name="Separator">The separator string (e.g., ", ").</param>
    /// <param name="CloseBracket">The closing bracket string with space (e.g., " ]" or " )").</param>
    private record SingleLineSeparatedListConfig(
        string OpenBracket,
        string Separator,
        string CloseBracket);

    /// <summary>
    /// Generic higher-order function for formatting a separated list, handling empty, single-line, and multiline cases.
    /// This unified function abstracts the common pattern of formatting lists, tuples, etc.
    /// This version works with Node-wrapped items and extracts the Range automatically.
    /// </summary>
    /// <typeparam name="TItem">The inner type of items (items are Node&lt;TItem&gt;).</typeparam>
    /// <typeparam name="TResult">The type of formatted items.</typeparam>
    /// <param name="separatedList">The separated list to format (may be empty).</param>
    /// <param name="formatItem">Delegate to format a single item.</param>
    /// <param name="commentQueries">Helper for querying comments from the original source.</param>
    /// <param name="singleLineConfig">Configuration for single-line formatting (open bracket, separator, close bracket).</param>
    /// <param name="multilineConfig">Configuration for multiline formatting (alignment refs, separator, close bracket).</param>
    /// <param name="context">The formatting context at the start of the list (before opening bracket).</param>
    /// <param name="containerRange">The original source range of the container.</param>
    /// <param name="commentStyle">The comment placement style to use for multiline lists.</param>
    /// <returns>A tuple of the formatted separated list and the final context.</returns>
    private static (SeparatedSyntaxList<TResult> FormattedList, FormattingContext FinalContext)
        FormatSeparatedList<TItem, TResult>(
            SeparatedSyntaxList<Node<TItem>> separatedList,
            System.Func<Node<TItem>, FormattingContext, FormattingResult<TResult>> formatItem,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig singleLineConfig,
            SeparatedListConfig multilineConfig,
            FormattingContext context,
            Range containerRange,
            SeparatedListCommentStyle commentStyle)
    {
        // Delegate to the general version with Range extraction from Node
        return FormatSeparatedListGeneral(
            separatedList: separatedList,
            formatItem: formatItem,
            getItemRange: item => item.Range,
            commentQueries: commentQueries,
            singleLineConfig: singleLineConfig,
            multilineConfig: multilineConfig,
            context: context,
            containerRange: containerRange,
            commentStyle: commentStyle);
    }

    /// <summary>
    /// General version of FormatSeparatedList that works with any item type.
    /// Uses a delegate to extract the Range from items for comment placement.
    /// This unified function handles empty, single-line, and multiline cases.
    /// </summary>
    private static (SeparatedSyntaxList<TResult> FormattedList, FormattingContext FinalContext)
        FormatSeparatedListGeneral<TItem, TResult>(
            SeparatedSyntaxList<TItem> separatedList,
            System.Func<TItem, FormattingContext, FormattingResult<TResult>> formatItem,
            System.Func<TItem, Range> getItemRange,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig singleLineConfig,
            SeparatedListConfig multilineConfig,
            FormattingContext context,
            Range containerRange,
            SeparatedListCommentStyle commentStyle)
    {
        // Handle empty list case
        if (separatedList is SeparatedSyntaxList<TItem>.Empty)
        {
            // Format empty list: "[]" or "()" or "{}" depending on config
            var emptyOpen = singleLineConfig.OpenBracket.TrimEnd();
            var emptyClose = singleLineConfig.CloseBracket.TrimStart();
            var afterEmpty = context.Advance(emptyOpen.Length + emptyClose.Length);
            return (new SeparatedSyntaxList<TResult>.Empty(), afterEmpty);
        }

        if (separatedList is not SeparatedSyntaxList<TItem>.NonEmpty nonEmptyList)
        {
            throw new System.NotImplementedException(
                $"Unexpected list type '{separatedList.GetType().Name}' in {nameof(FormatSeparatedListGeneral)} " +
                $"at row {containerRange.Start.Row}, column {containerRange.Start.Column}.");
        }

        var isMultiline = SpansMultipleRows(containerRange);

        if (isMultiline)
        {
            // Multiline formatting
            var afterOpenBracket = context.Advance(singleLineConfig.OpenBracket.Length);

            return
                FormatMultilineSeparatedListGeneral(
                    separatedList: nonEmptyList,
                    formatItem: formatItem,
                    getItemRange: getItemRange,
                    commentQueries: commentQueries,
                    config: multilineConfig,
                    startContext: afterOpenBracket,
                    containerRange: containerRange,
                    commentStyle: commentStyle);
        }
        else
        {
            // Single-line formatting with inline comment handling
            return
                FormatSingleLineSeparatedListGeneral(
                    separatedList: nonEmptyList,
                    formatItem: formatItem,
                    getItemRange: getItemRange,
                    commentQueries: commentQueries,
                    config: singleLineConfig,
                    context: context,
                    containerRange: containerRange);
        }
    }

    /// <summary>
    /// Generic higher-order function for formatting a single-line separated list with inline comment handling.
    /// This function handles comments between elements and around separators based on original positions.
    /// </summary>
    private static (SeparatedSyntaxList<TResult>.NonEmpty FormattedList, FormattingContext FinalContext)
        FormatSingleLineSeparatedList<TItem, TResult>(
            SeparatedSyntaxList<Node<TItem>>.NonEmpty separatedList,
            System.Func<Node<TItem>, FormattingContext, FormattingResult<TResult>> formatItem,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig config,
            FormattingContext context,
            Range containerRange)
    {
        // Delegate to the general version with Range extraction from Node
        return FormatSingleLineSeparatedListGeneral(
            separatedList: separatedList,
            formatItem: formatItem,
            getItemRange: item => item.Range,
            commentQueries: commentQueries,
            config: config,
            context: context,
            containerRange: containerRange);
    }

    /// <summary>
    /// General version of single-line separated list formatter that works with any item type.
    /// Uses a delegate to extract the Range from items for comment placement.
    /// </summary>
    private static (SeparatedSyntaxList<TResult>.NonEmpty FormattedList, FormattingContext FinalContext)
        FormatSingleLineSeparatedListGeneral<TItem, TResult>(
            SeparatedSyntaxList<TItem>.NonEmpty separatedList,
            System.Func<TItem, FormattingContext, FormattingResult<TResult>> formatItem,
            System.Func<TItem, Range> getItemRange,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig config,
            FormattingContext context,
            Range containerRange)
    {
        var afterOpenBracket = context.Advance(config.OpenBracket.Length);
        var currentContext = afterOpenBracket;

        // Handle comments after opening bracket before first element
        var firstItemRange = getItemRange(separatedList.First);
        var commentsAfterOpen = commentQueries.GetOnRowBetweenColumns(
            containerRange.Start.Row,
            containerRange.Start.Column,
            firstItemRange.Start.Column);

        foreach (var comment in commentsAfterOpen)
        {
            currentContext = currentContext.FormatAndAddComment(comment);
            currentContext = currentContext.Advance(1); // space after comment
        }

        // Format first element
        var firstResult = formatItem(separatedList.First, currentContext);
        currentContext = firstResult.Context;

        // Format rest elements with separators and inline comments
        var restItems = new List<(Location SeparatorLocation, TResult Node)>();
        var prevItemRange = firstItemRange;

        foreach (var (originalSepLoc, item) in separatedList.Rest)
        {
            var itemRange = getItemRange(item);

            // Comments before separator (after previous element but before comma)
            var commentsBeforeSep = commentQueries.GetOnRowBetweenColumns(
                containerRange.Start.Row,
                prevItemRange.End.Column,
                originalSepLoc.Column);

            foreach (var comment in commentsBeforeSep)
            {
                currentContext = currentContext.Advance(1); // space before comment
                currentContext = currentContext.FormatAndAddComment(comment);
            }

            // Record separator location and advance past comma only (space handled after comments)
            var separatorLoc = currentContext.CurrentLocation();
            currentContext = currentContext.Advance(1); // comma character

            // Comments after separator (after comma but before element)
            var commentsAfterSep = commentQueries.GetOnRowBetweenColumns(
                containerRange.Start.Row,
                originalSepLoc.Column,
                itemRange.Start.Column);

            foreach (var comment in commentsAfterSep)
            {
                currentContext = currentContext.Advance(1); // space before comment
                currentContext = currentContext.FormatAndAddComment(comment);
            }

            currentContext = currentContext.Advance(1); // space before element

            // Format element
            var itemResult = formatItem(item, currentContext);
            restItems.Add((separatorLoc, itemResult.FormattedNode));
            currentContext = itemResult.Context;

            prevItemRange = itemRange;
        }

        // Handle comments before closing bracket after last element
        var commentsBeforeClose = commentQueries.GetOnRowBetweenColumns(
            containerRange.Start.Row,
            prevItemRange.End.Column,
            containerRange.End.Column);

        foreach (var comment in commentsBeforeClose)
        {
            currentContext = currentContext.Advance(1); // space before comment
            currentContext = currentContext.FormatAndAddComment(comment);
        }

        // Close bracket
        var afterClose = currentContext.Advance(config.CloseBracket.Length);

        var formattedList = new SeparatedSyntaxList<TResult>.NonEmpty(firstResult.FormattedNode, restItems);
        return (formattedList, afterClose);
    }

    /// <summary>
    /// Formats a separated list that is always single-line (no multiline variant).
    /// This overload handles empty lists and non-empty single-line lists with proper inline comment handling.
    /// </summary>
    /// <typeparam name="TItem">The type of items in the original list.</typeparam>
    /// <typeparam name="TResult">The type of formatted items.</typeparam>
    /// <param name="separatedList">The separated list to format (may be empty).</param>
    /// <param name="formatItem">Delegate to format a single item.</param>
    /// <param name="commentQueries">Helper for querying comments from the original source.</param>
    /// <param name="config">Configuration for single-line formatting (open bracket, separator, close bracket).</param>
    /// <param name="context">The formatting context at the start of the list (before opening bracket).</param>
    /// <param name="containerRange">The original source range of the container.</param>
    /// <returns>A tuple of the formatted separated list and the final context.</returns>
    private static (SeparatedSyntaxList<TResult> FormattedList, FormattingContext FinalContext)
        FormatSingleLineOnlySeparatedList<TItem, TResult>(
            SeparatedSyntaxList<Node<TItem>> separatedList,
            System.Func<Node<TItem>, FormattingContext, FormattingResult<TResult>> formatItem,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig config,
            FormattingContext context,
            Range containerRange)
    {
        // Delegate to the general version with Range extraction from Node
        return FormatSingleLineOnlySeparatedListGeneral(
            separatedList: separatedList,
            formatItem: formatItem,
            getItemRange: item => item.Range,
            commentQueries: commentQueries,
            config: config,
            context: context,
            containerRange: containerRange);
    }

    /// <summary>
    /// General version of FormatSingleLineOnlySeparatedList that works with any item type.
    /// Uses a delegate to extract the Range from items for comment placement.
    /// </summary>
    private static (SeparatedSyntaxList<TResult> FormattedList, FormattingContext FinalContext)
        FormatSingleLineOnlySeparatedListGeneral<TItem, TResult>(
            SeparatedSyntaxList<TItem> separatedList,
            System.Func<TItem, FormattingContext, FormattingResult<TResult>> formatItem,
            System.Func<TItem, Range> getItemRange,
            CommentQueryHelper commentQueries,
            SingleLineSeparatedListConfig config,
            FormattingContext context,
            Range containerRange)
    {
        // Handle empty list case
        if (separatedList is SeparatedSyntaxList<TItem>.Empty)
        {
            var emptyOpen = config.OpenBracket.TrimEnd();
            var emptyClose = config.CloseBracket.TrimStart();
            var afterEmpty = context.Advance(emptyOpen.Length + emptyClose.Length);
            return (new SeparatedSyntaxList<TResult>.Empty(), afterEmpty);
        }

        // We have a non-empty list - use the general single-line helper with comment handling
        var nonEmptyList = (SeparatedSyntaxList<TItem>.NonEmpty)separatedList;
        var (formattedList, finalContext) = FormatSingleLineSeparatedListGeneral(
            separatedList: nonEmptyList,
            formatItem: formatItem,
            getItemRange: getItemRange,
            commentQueries: commentQueries,
            config: config,
            context: context,
            containerRange: containerRange);
        return (formattedList, finalContext);
    }

    #endregion

    #region Comment Placement Helper

    /// <summary>
    /// Unified helper for placing comments in the formatted output.
    /// This class provides a single entry point for all comment placement operations,
    /// ensuring consistent handling across the formatter.
    /// </summary>
    private class CommentPlacementHelper
    {
        /// <summary>
        /// Places a documentation comment (Node&lt;string&gt;) at the current context position.
        /// </summary>
        public static FormattingContext PlaceDocComment(
            FormattingContext context,
            Node<string> docComment)
        {
            return
                context
                .FormatAndAddComment(docComment)
                .NextRowToIndent();
        }

        /// <summary>
        /// Core logic for placing a sequence of comments with proper spacing based on comment type.
        /// Handles the common pattern of:
        /// - Adding blank lines before section comments that follow doc comments
        /// - No extra blank lines between consecutive section comments
        /// </summary>
        /// <param name="context">The current formatting context.</param>
        /// <param name="comments">The comments to place.</param>
        /// <param name="index">Index of the current comment being placed.</param>
        /// <param name="resetIndentForTopLevel">Whether to reset indent to column 1 before each comment.</param>
        /// <returns>The updated formatting context after placing the comment.</returns>
        private static FormattingContext PlaceCommentWithSpacing(
            FormattingContext context,
            IReadOnlyList<Node<ParsedComment>> comments,
            int index,
            bool resetIndentForTopLevel)
        {
            var comment = comments[index];
            var currentContext = context;

            // If this is a section comment (non-doc) that comes after a DOC comment,
            // add 3 blank lines before it (to visually separate)
            // But if it comes after another section comment, no blank lines (they should be consecutive)
            if (index > 0 && !comment.Value.IsDocComment)
            {
                var prevComment = comments[index - 1];
                if (prevComment.Value.IsDocComment)
                {
                    // WithBlankLine() does 2 row advances, plus 1 for going to next row = 3 blank lines total
                    currentContext = currentContext.WithBlankLine().NextRowToIndent();
                }
                // else: previous was also a section comment, just continue on next line (no extra blank lines)
            }

            // Optionally reset indent for top-level comments (between declarations)
            if (resetIndentForTopLevel)
            {
                currentContext = currentContext.ResetIndent().SetIndentColumn();
            }

            return currentContext.FormatAndAddCommentAndNextRowToIndent(comment);
        }

        /// <summary>
        /// Places a sequence of comments, with proper spacing between them.
        /// </summary>
        /// <param name="context">The current formatting context.</param>
        /// <param name="comments">The comments to place.</param>
        /// <param name="addBlankLinesAfterSection">Whether to add blank lines after non-doc comments (section comments).</param>
        /// <returns>The updated formatting context.</returns>
        public static FormattingContext PlaceCommentSequence(
            FormattingContext context,
            IReadOnlyList<Node<ParsedComment>> comments,
            bool addBlankLinesAfterSection = false)
        {
            if (comments.Count is 0)
                return context;

            var currentContext = context;

            for (var i = 0; i < comments.Count; i++)
            {
                var comment = comments[i];

                // Use shared logic for comment spacing based on doc vs section comment patterns
                currentContext = PlaceCommentWithSpacing(currentContext, comments, i, resetIndentForTopLevel: false);

                // Handle section comment spacing
                if (addBlankLinesAfterSection && !comment.Value.IsDocComment)
                {
                    if (i < comments.Count - 1)
                    {
                        var nextComment = comments[i + 1];
                        // Check if there was a gap (blank line) between this comment and next in the original
                        var gapBetweenComments = nextComment.Range.Start.Row > comment.Range.End.Row + 1;
                        if (gapBetweenComments)
                        {
                            currentContext = currentContext.WithBlankLine();
                        }
                    }
                    else
                    {
                        // Last non-doc comment - add blank lines after
                        currentContext = currentContext.WithBlankLine();
                    }
                }
            }

            return currentContext;
        }

        /// <summary>
        /// Places comments that appear between declarations with proper section comment formatting.
        /// This handles the complex logic for doc comments vs section comments and appropriate spacing.
        /// </summary>
        /// <param name="context">The current formatting context (after previous declaration).</param>
        /// <param name="comments">The comments between declarations.</param>
        /// <returns>The updated formatting context positioned for the next declaration.</returns>
        public static FormattingContext PlaceCommentsBetweenDeclarations(
            FormattingContext context,
            IReadOnlyList<Node<ParsedComment>> comments)
        {
            if (comments.Count is 0)
                return context.WithBlankLine().NextRowToIndent();

            var firstComment = comments[0];
            var isFirstCommentDoc = firstComment.Value.IsDocComment;
            var currentContext = context;

            // Set initial spacing based on comment type
            if (isFirstCommentDoc)
            {
                // Doc comment: 2 blank lines before (same as between declarations without comments)
                currentContext = currentContext.WithBlankLine().NextRowToIndent();
            }
            else
            {
                // Section comment: 3 blank lines before (to visually separate sections)
                // WithBlankLine() does 2 row advances, so WithBlankLine().WithBlankLine() = 4 advances
                currentContext = currentContext.WithBlankLine().WithBlankLine();
            }

            // Place each comment using shared logic
            for (var ci = 0; ci < comments.Count; ci++)
            {
                var comment = comments[ci];

                // Use shared logic for comment spacing based on doc vs section comment patterns
                // resetIndentForTopLevel: true because between declarations, comments go at column 1
                currentContext = PlaceCommentWithSpacing(currentContext, comments, ci, resetIndentForTopLevel: true);

                // For non-doc section comments:
                // Only add 2 blank lines after the LAST section comment
                if (!comment.Value.IsDocComment)
                {
                    var isLastComment = ci == comments.Count - 1;
                    if (isLastComment)
                    {
                        currentContext = currentContext.WithBlankLine();
                    }
                }
            }

            return currentContext;
        }
    }

    #endregion

    #region Visitor Implementation

    /// <summary>
    /// Visitor implementation for AVH4 formatting on concretized syntax model.
    /// </summary>
    private class Avh4FormatVisitor(
        CommentQueryHelper commentQueries,
        IReadOnlyList<Node<IncompleteDeclaration>> originalIncompleteDeclarations)
    {
        #region File Formatting

        public (File, IReadOnlyList<Node<string>>, IReadOnlyList<Node<IncompleteDeclaration>>) FormatFile(
            File file,
            FormattingContext context)
        {
            // Check for comments before the module definition (at the beginning of the file)
            var commentsBeforeModule = commentQueries.GetBeforeRow(file.ModuleDefinition.Range.Start.Row);

            FormattingContext contextBeforeModule = context;
            if (commentsBeforeModule.Count > 0)
            {
                // Format comments that appear before the module definition
                foreach (var comment in commentsBeforeModule)
                {
                    contextBeforeModule = contextBeforeModule.FormatAndAddCommentAndNextRowToIndent(comment);
                }
                // Add two blank lines after comments before module (like WithBlankLine does)
                contextBeforeModule = contextBeforeModule.NextRowToIndent().NextRowToIndent();
            }

            // Format module definition
            var (formattedModule, contextAfterModule) =
                FormatModuleDefinition(file.ModuleDefinition, contextBeforeModule, commentQueries);

            // Check for comments associated with first import (leading comments)
            var firstImport = file.Imports.FirstOrDefault();

            var commentsAfterModuleBeforeImports =
                firstImport is not null
                ? commentQueries.GetBetweenRows(file.ModuleDefinition.Range.End.Row, firstImport.Range.Start.Row)
                : [];

            FormattingContext contextBeforeImports;

            if (commentsAfterModuleBeforeImports.Count is not 0)
            {
                var startContext = contextAfterModule.WithBlankLine();

                contextBeforeImports =
                    CommentPlacementHelper.PlaceCommentSequence(
                        startContext, commentsAfterModuleBeforeImports);

                contextBeforeImports = contextBeforeImports.NextRowToIndent();
            }
            else
            {
                contextBeforeImports = contextAfterModule.WithBlankLine();
            }

            // Format imports
            FormattingContext contextAfterImports;
            IReadOnlyList<Node<Import>> formattedImports;

            if (firstImport is not null)
            {
                (formattedImports, contextAfterImports) =
                    FormatImports(file.Imports, contextBeforeImports, commentQueries);
            }
            else
            {
                formattedImports = [];
                contextAfterImports = contextAfterModule;
            }

            // Search for comments between imports/module and first declaration using original positions
            var lastOriginalRowBeforeDecls =
                file.Imports.Any()
                ? file.Imports.Last().Range.End.Row
                : file.ModuleDefinition.Range.End.Row;

            var firstDeclaration = file.Declarations.FirstOrDefault();

            var commentsBefore =
                firstDeclaration is not null
                ? commentQueries.GetBetweenRows(lastOriginalRowBeforeDecls, firstDeclaration.Range.Start.Row)
                : [];

            FormattingContext contextBeforeDecls;

            if (commentsBefore.Count is not 0)
            {
                var firstComment = commentsBefore[0];
                var isFirstCommentDoc = firstComment.Value.IsDocComment;

                FormattingContext startContext;
                if (formattedImports.Any())
                {
                    if (isFirstCommentDoc)
                    {
                        // Doc comment after imports: 2 blank lines (WithBlankLine)
                        startContext = contextAfterImports.WithBlankLine();
                    }
                    else
                    {
                        // Non-doc comment after imports: 3 blank lines (WithBlankLine + NextRowToIndent)
                        startContext = contextAfterImports.WithBlankLine().NextRowToIndent();
                    }
                }
                else
                {
                    startContext = contextAfterModule.WithBlankLine();
                }

                contextBeforeDecls =
                    CommentPlacementHelper.PlaceCommentSequence(
                        startContext,
                        commentsBefore,
                        addBlankLinesAfterSection: true);

                // For module-level doc comments (between module and first declaration, no imports),
                // we need to add 2 blank lines after the doc comment
                var lastComment = commentsBefore[^1];
                if (!formattedImports.Any() && lastComment.Value.IsDocComment)
                {
                    // Doc comment at module level: 2 blank lines after
                    contextBeforeDecls = contextBeforeDecls.WithBlankLine();
                }
            }
            else
            {
                contextBeforeDecls = formattedImports.Any()
                    ? contextAfterImports.WithBlankLine()
                    : contextAfterModule.WithBlankLine().NextRowToIndent();
            }

            // Format declarations along with incomplete declarations interleaved by position
            var (formattedDeclarations, contextAfterDeclarations, formattedIncompleteDeclarations) =
                FormatDeclarationsWithIncompletes(file.Declarations, contextBeforeDecls);

            // Get trailing comments that appear after all declarations
            var lastDeclRow = file.Declarations.Any()
                ? file.Declarations.Max(d => d.Range.End.Row)
                : (file.Imports.Any()
                    ? file.Imports.Last().Range.End.Row
                    : file.ModuleDefinition.Range.End.Row);

            var trailingComments = commentQueries.GetAfterRow(lastDeclRow);

            // Format trailing comments
            var contextAfterTrailingComments = contextAfterDeclarations;
            if (trailingComments.Count > 0)
            {
                var firstTrailingComment = trailingComments[0];
                FormattingContext trailingContext;
                if (file.Declarations.Any() || formattedIncompleteDeclarations.Any())
                {
                    // For section comments (non-doc), use 3 blank lines before
                    // For doc comments, use 2 blank lines before
                    if (!firstTrailingComment.Value.IsDocComment)
                    {
                        trailingContext = contextAfterDeclarations.WithBlankLine().WithBlankLine();
                    }
                    else
                    {
                        trailingContext = contextAfterDeclarations.WithBlankLine().NextRowToIndent();
                    }
                }
                else if (formattedImports.Any())
                {
                    // Trailing comments after imports but no declarations
                    trailingContext = contextAfterImports.WithBlankLine();
                }
                else
                {
                    // Trailing comments after module definition only (no imports, no declarations)
                    // Just one blank line after module definition
                    trailingContext = contextAfterModule.WithBlankLine();
                }

                contextAfterTrailingComments =
                    CommentPlacementHelper.PlaceCommentSequence(trailingContext, trailingComments);
            }

            var formattedFile = new File(
                ModuleDefinition: formattedModule,
                Imports: formattedImports,
                Declarations: formattedDeclarations,
                Comments: [],
                IncompleteDeclarations: []);

            return (formattedFile, contextAfterTrailingComments.Comments, formattedIncompleteDeclarations);
        }

        /// <summary>
        /// Calculates the end location given a start location and text content.
        /// </summary>
        private static Location CalculateEndLocation(Location start, string text)
        {
            var row = start.Row;
            var column = start.Column;

            foreach (var ch in text)
            {
                if (ch is '\n')
                {
                    row++;
                    column = 1;
                }
                else
                {
                    column++;
                }
            }

            return new Location(row, column);
        }

        #endregion

        #region Module Formatting

        private static (Node<Module>, FormattingContext) FormatModuleDefinition(
            Node<Module> module,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            return module.Value switch
            {
                Module.NormalModule normalModule =>
                FormatNormalModule(normalModule, context, commentQueries),

                Module.PortModule portModule =>
                FormatPortModule(portModule, context, commentQueries),

                Module.EffectModule effectModule =>
                FormatEffectModule(effectModule, context, commentQueries),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for module type '{module.Value.GetType().Name}' is not implemented " +
                    $"in {nameof(FormatModuleDefinition)} at row {module.Range.Start.Row}, column {module.Range.Start.Column}.")
            };
        }

        /// <summary>
        /// Core formatting logic for module definitions.
        /// Handles the common pattern of: advance past prefix keywords, format module name, format exposing list.
        /// </summary>
        private static (
            Range range,
            Location moduleTokenLoc,
            Location? prefixTokenLoc,
            Node<IReadOnlyList<string>> moduleNameNode,
            Location exposingTokenLoc,
            Node<Exposing> formattedExposing,
            FormattingContext contextAfterExposing)
            FormatModuleCore(
                IReadOnlyList<string> moduleNameValue,
                Node<Exposing> exposingList,
                FormattingContext context,
                CommentQueryHelper commentQueries,
                string? prefixKeyword = null)
        {
            Location? prefixTokenLoc = null;
            FormattingContext afterPrefix = context;

            // Handle prefix keyword (e.g., "port " or "effect ")
            if (prefixKeyword is not null)
            {
                prefixTokenLoc = context.CurrentLocation();
                afterPrefix = context.Advance(prefixKeyword.Length).AdvanceSpaceSeparator();
            }

            // "module "
            var moduleTokenLoc = afterPrefix.CurrentLocation();
            var afterModuleKeyword = afterPrefix.Advance(Keywords.Module.Length).AdvanceSpaceSeparator();

            var moduleName = string.Join(".", moduleNameValue);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length).AdvanceSpaceSeparator();

            var exposingTokenLoc = afterModuleName.CurrentLocation();

            var (formattedExposing, contextAfterExposing) =
                FormatExposing(exposingList, afterModuleName, commentQueries);

            var range = MakeRange(context.CurrentLocation(), contextAfterExposing.CurrentLocation());
            var moduleNameNode = MakeNode(range, moduleNameValue);

            return (range, moduleTokenLoc, prefixTokenLoc, moduleNameNode, exposingTokenLoc, formattedExposing, contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatNormalModule(
            Module.NormalModule module,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            var (range, moduleTokenLoc, _, moduleNameNode, exposingTokenLoc, formattedExposing, contextAfterExposing) =
                FormatModuleCore(module.ModuleData.ModuleName.Value, module.ModuleData.ExposingList, context, commentQueries);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing
            );

            return (MakeNode<Module>(range, new Module.NormalModule(moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatPortModule(
            Module.PortModule module,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            var (range, moduleTokenLoc, prefixTokenLoc, moduleNameNode, exposingTokenLoc, formattedExposing, contextAfterExposing) =
                FormatModuleCore(module.ModuleData.ModuleName.Value, module.ModuleData.ExposingList, context, commentQueries, Keywords.Port);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing
            );

            return (MakeNode<Module>(range, new Module.PortModule(prefixTokenLoc!.Value, moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatEffectModule(
            Module.EffectModule module,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            var (range, moduleTokenLoc, prefixTokenLoc, moduleNameNode, exposingTokenLoc, formattedExposing, contextAfterExposing) =
                FormatModuleCore(module.ModuleData.ModuleName.Value, module.ModuleData.ExposingList, context, commentQueries, Keywords.Effect);

            var formattedModuleData = new EffectModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing,
                Command: module.ModuleData.Command,
                Subscription: module.ModuleData.Subscription
            );

            return (MakeNode<Module>(range, new Module.EffectModule(prefixTokenLoc!.Value, moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposing(
            Node<Exposing> exposing,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            return exposing.Value switch
            {
                Exposing.All =>
                FormatExposingAll(context),

                Exposing.Explicit explicitList =>
                FormatExposingExplicit(explicitList, context, commentQueries),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for exposing type '{exposing.Value.GetType().Name}' is not implemented " +
                    $"in {nameof(FormatExposing)} at row {exposing.Range.Start.Row}, column {exposing.Range.Start.Column}.")
            };
        }

        private static (Node<Exposing>, FormattingContext) FormatExposingAll(FormattingContext context)
        {
            var afterExposing = context.Advance(Keywords.ExposingAll.Length);
            var range = MakeRange(context.CurrentLocation(), afterExposing.CurrentLocation());
            return (MakeNode<Exposing>(range, new Exposing.All(range)), afterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposingExplicit(
            Exposing.Explicit explicitList,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            var isMultiLine = IsExposingListMultiLine(explicitList);

            // Helper to format a TopLevelExpose item
            static FormattingResult<Node<TopLevelExpose>> FormatTopLevelExposeItem(
                Node<TopLevelExpose> node,
                FormattingContext ctx)
            {
                var exposeName = GetTopLevelExposeName(node.Value);
                var afterExpose = ctx.Advance(exposeName.Length);
                var formattedNode = MakeNode(ctx.CurrentLocation(), afterExpose.CurrentLocation(), node.Value);
                return FormattingResult<Node<TopLevelExpose>>.Create(formattedNode, afterExpose);
            }

            var openParenLocation = context.CurrentLocation();

            if (!isMultiLine)
            {
                // Single-line format: exposing (A, B, C)
                var exposingOpen = Keywords.Exposing + " " + Keywords.OpenParen;  // "exposing ("
                var singleLineConfig = new SingleLineSeparatedListConfig(
                    OpenBracket: exposingOpen,
                    Separator: Keywords.Comma + " ",  // ", "
                    CloseBracket: Keywords.CloseParen);  // ")"

                // Handle empty case
                if (explicitList.Nodes is SeparatedSyntaxList<Node<TopLevelExpose>>.Empty)
                {
                    var afterOpen = context.Advance(exposingOpen.Length);
                    var closeContext = afterOpen.Advance(Keywords.CloseParen.Length);
                    var emptyRange = MakeRange(context.CurrentLocation(), closeContext.CurrentLocation());
                    return (MakeNode<Exposing>(emptyRange, new Exposing.Explicit(
                        OpenParenLocation: context.CurrentLocation(),
                        Nodes: new SeparatedSyntaxList<Node<TopLevelExpose>>.Empty(),
                        CloseParenLocation: closeContext.CurrentLocation() with { Column = closeContext.CurrentLocation().Column - 1 })), closeContext);
                }

                var nonEmptyList = (SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty)explicitList.Nodes;

                // Use FormatSingleLineSeparatedList for the items
                var (formattedList, afterClose) = FormatSingleLineSeparatedList(
                    separatedList: nonEmptyList,
                    formatItem: FormatTopLevelExposeItem,
                    commentQueries: commentQueries,
                    config: singleLineConfig,
                    context: context,
                    containerRange: MakeRange(explicitList.OpenParenLocation, explicitList.CloseParenLocation));

                var range = MakeRange(context.CurrentLocation(), afterClose.CurrentLocation());
                return (MakeNode<Exposing>(range, new Exposing.Explicit(
                    OpenParenLocation: openParenLocation,
                    Nodes: formattedList,
                    CloseParenLocation: afterClose.CurrentLocation() with { Column = afterClose.CurrentLocation().Column - 1 })), afterClose);
            }
            else
            {
                // Multiline mode with grouping:
                // Items from the same @docs group go on the same line
                // Format:
                // exposing
                //     ( Item1, Item2, Item3
                //     , Item4, Item5
                //     , Item6
                //     )
                var afterExposingKeyword = context.Advance(Keywords.Exposing.Length);
                // Reference context for indented content (one indent level from base)
                var indentedRef = context.CreateIndentedRef();
                // Save parent context for later indent restoration
                var parentContext = afterExposingKeyword;
                var parenLineContext = afterExposingKeyword.ReturnToIndent(indentedRef).NextRowToIndent();

                openParenLocation = parenLineContext.CurrentLocation();

                // Handle empty case
                if (explicitList.Nodes is SeparatedSyntaxList<Node<TopLevelExpose>>.Empty)
                {
                    var afterOpenParen = parenLineContext.Advance(Keywords.TupleOpen.Length).AdvanceSpaceSeparator();
                    var closeLineContext = afterOpenParen.NextRowToIndent();
                    var closeParenLocation = closeLineContext.CurrentLocation();
                    var afterCloseParen = closeLineContext.Advance(Keywords.CloseParen.Length);
                    var finalContext = afterCloseParen.ReturnToIndent(parentContext);
                    var emptyRange = MakeRange(context.CurrentLocation(), finalContext.CurrentLocation());
                    return (MakeNode<Exposing>(emptyRange, new Exposing.Explicit(
                        OpenParenLocation: openParenLocation,
                        Nodes: new SeparatedSyntaxList<Node<TopLevelExpose>>.Empty(),
                        CloseParenLocation: closeParenLocation)), finalContext);
                }

                var nonEmptyList = (SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty)explicitList.Nodes;
                var afterOpenParen2 = parenLineContext.Advance(Keywords.TupleOpen.Length).AdvanceSpaceSeparator();

                // Get module documentation to extract @docs groups
                var moduleDocComment = commentQueries.GetFirstDocCommentAfterRow(explicitList.CloseParenLocation.Row);

                // Extract @docs groups and group items
                IReadOnlyList<IReadOnlyList<Node<TopLevelExpose>>> itemGroups;
                if (moduleDocComment is not null)
                {
                    var docsGroups = ExtractDocsGroupsFromDocComment(moduleDocComment.Value.Text);
                    // Only use grouping if there are actual @docs directives
                    // Otherwise fall back to one item per line
                    if (docsGroups.Count > 0)
                    {
                        itemGroups = GroupExposedItemsByDocs([.. nonEmptyList.Nodes], docsGroups);
                    }
                    else
                    {
                        // Doc comment exists but has no @docs directives - one item per line
                        itemGroups = [.. nonEmptyList.Nodes.Select(item => new[] { item } as IReadOnlyList<Node<TopLevelExpose>>)];
                    }
                }
                else
                {
                    // No module documentation - put each item in its own group (current behavior)
                    itemGroups = [.. nonEmptyList.Nodes.Select(item => new[] { item } as IReadOnlyList<Node<TopLevelExpose>>)];
                }

                // Format items grouped by @docs
                var (formattedList, afterFields) = FormatGroupedExposingList(
                    itemGroups: itemGroups,
                    formatItem: FormatTopLevelExposeItem,
                    alignmentRef: indentedRef,
                    startContext: afterOpenParen2);

                // Close paren on new line
                var closeLineCtx = afterFields.ReturnToIndent(indentedRef).NextRowToIndent();
                var closeParenLocation2 = closeLineCtx.CurrentLocation();
                var afterCloseParen2 = closeLineCtx.Advance(Keywords.CloseParen.Length);

                var finalContext2 = afterCloseParen2.ReturnToIndent(parentContext);
                var range = MakeRange(context.CurrentLocation(), finalContext2.CurrentLocation());

                return (MakeNode<Exposing>(range, new Exposing.Explicit(
                    OpenParenLocation: openParenLocation,
                    Nodes: formattedList,
                    CloseParenLocation: closeParenLocation2)), finalContext2);
            }
        }

        /// <summary>
        /// Formats a grouped exposing list where items in the same group appear on the same line.
        /// Format:
        /// ( Item1, Item2, Item3
        /// , Item4, Item5
        /// , Item6
        /// )
        /// </summary>
        private static (SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty FormattedList, FormattingContext FinalContext)
            FormatGroupedExposingList(
                IReadOnlyList<IReadOnlyList<Node<TopLevelExpose>>> itemGroups,
                System.Func<Node<TopLevelExpose>, FormattingContext, FormattingResult<Node<TopLevelExpose>>> formatItem,
                FormattingContext alignmentRef,
                FormattingContext startContext)
        {
            var allFormattedItems = new List<Node<TopLevelExpose>>();
            var restItems = new List<(Location SeparatorLocation, Node<TopLevelExpose> Node)>();
            var currentContext = startContext;
            var isFirstItem = true;

            for (var groupIndex = 0; groupIndex < itemGroups.Count; groupIndex++)
            {
                var group = itemGroups[groupIndex];
                if (group.Count is 0)
                    continue;

                // For groups after the first, start on a new line with separator at alignment
                if (groupIndex > 0)
                {
                    currentContext = currentContext.ReturnToIndent(alignmentRef).NextRowToIndent();
                }

                for (var itemIndex = 0; itemIndex < group.Count; itemIndex++)
                {
                    var item = group[itemIndex];

                    if (isFirstItem)
                    {
                        // First item - format without leading separator
                        var result = formatItem(item, currentContext);
                        allFormattedItems.Add(result.FormattedNode);
                        currentContext = result.Context;
                        isFirstItem = false;
                    }
                    else
                    {
                        // Subsequent items - add separator
                        var separatorLoc = currentContext.CurrentLocation();
                        currentContext = currentContext.Advance(Keywords.Comma.Length).AdvanceSpaceSeparator();
                        var result = formatItem(item, currentContext);
                        restItems.Add((separatorLoc, result.FormattedNode));
                        currentContext = result.Context;
                    }
                }
            }

            // Build the result list
            if (allFormattedItems.Count is 0)
            {
                throw new System.InvalidOperationException("Expected at least one item in the exposing list");
            }

            var formattedList = new SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty(
                allFormattedItems[0],
                restItems);

            return (formattedList, currentContext);
        }

        private static bool IsExposingListMultiLine(Exposing.Explicit explicitList)
        {
            // Check if close paren is on a different row than open paren
            if (explicitList.CloseParenLocation.Row > explicitList.OpenParenLocation.Row)
                return true;

            // Also check if nodes span multiple rows
            return NodesSpanMultipleRows([.. explicitList.Nodes.Nodes]);
        }

        private static string GetTopLevelExposeName(TopLevelExpose expose)
        {
            return expose switch
            {
                TopLevelExpose.InfixExpose infix =>
                $"({infix.Name})",

                TopLevelExpose.FunctionExpose func =>
                func.Name,

                TopLevelExpose.TypeOrAliasExpose type =>
                type.Name,

                TopLevelExpose.TypeExpose typeExpose =>
                typeExpose.ExposedType.Open is not null
                ? $"{typeExpose.ExposedType.Name}(..)"
                : typeExpose.ExposedType.Name,

                _ =>
                throw new System.NotImplementedException(
                    $"Getting name for expose type '{expose.GetType().Name}' is not implemented in {nameof(GetTopLevelExposeName)}.")
            };
        }

        /// <summary>
        /// Extracts @docs groups from a documentation comment.
        /// Each @docs directive defines a group of exports that should be formatted together.
        /// Returns a list of groups, where each group is a list of names that should appear on the same line.
        /// </summary>
        private static IReadOnlyList<IReadOnlyList<string>> ExtractDocsGroupsFromDocComment(string docCommentText)
        {
            var groups = new List<IReadOnlyList<string>>();

            // Find all @docs directives
            // Format: @docs name1, name2, name3
            var lines = docCommentText.Split('\n');
            foreach (var line in lines)
            {
                var trimmed = line.Trim();
                if (trimmed.StartsWith("@docs "))
                {
                    var namesStr = trimmed.Substring("@docs ".Length);
                    var names = namesStr
                        .Split(',')
                        .Select(n => n.Trim())
                        .Where(n => !string.IsNullOrEmpty(n))
                        .Select(n =>
                        {
                            // Strip parentheses from operator names like "(+)" -> "+"
                            // Must have at least 3 chars to have content between parens: "(x)"
                            if (n.Length > 2 && n.StartsWith("(") && n.EndsWith(")"))
                            {
                                return n.Substring(1, n.Length - 2);
                            }
                            return n;
                        })
                        .ToList();

                    if (names.Count > 0)
                    {
                        groups.Add(names);
                    }
                }
            }

            return groups;
        }

        /// <summary>
        /// Groups exposing list items based on @docs directives from module documentation.
        /// Items in the same @docs group are placed in the same output group.
        /// Items not mentioned in any @docs directive are collected into a final group.
        /// </summary>
        private static IReadOnlyList<IReadOnlyList<Node<TopLevelExpose>>> GroupExposedItemsByDocs(
            IReadOnlyList<Node<TopLevelExpose>> items,
            IReadOnlyList<IReadOnlyList<string>> docsGroups)
        {
            // Build a lookup from item name to item
            var itemsByName = new Dictionary<string, Node<TopLevelExpose>>();
            var usedItems = new HashSet<Node<TopLevelExpose>>();

            foreach (var item in items)
            {
                var name = GetTopLevelExposeBaseName(item.Value);
                // Don't overwrite if duplicate (keep first occurrence)
                if (!itemsByName.ContainsKey(name))
                {
                    itemsByName[name] = item;
                }
            }

            var result = new List<IReadOnlyList<Node<TopLevelExpose>>>();

            // Create groups based on @docs directives
            foreach (var docsGroup in docsGroups)
            {
                var group = new List<Node<TopLevelExpose>>();
                foreach (var name in docsGroup)
                {
                    if (itemsByName.TryGetValue(name, out var item) && !usedItems.Contains(item))
                    {
                        group.Add(item);
                        usedItems.Add(item);
                    }
                }
                if (group.Count > 0)
                {
                    result.Add(group);
                }
            }

            // Collect remaining items (not in any @docs group) into a final group
            var remaining = items.Where(item => !usedItems.Contains(item)).ToList();
            if (remaining.Count > 0)
            {
                result.Add(remaining);
            }

            return result;
        }

        /// <summary>
        /// Gets the base name of a TopLevelExpose (without parens or (..) suffix).
        /// Used for matching against @docs directives.
        /// </summary>
        private static string GetTopLevelExposeBaseName(TopLevelExpose expose)
        {
            return expose switch
            {
                TopLevelExpose.InfixExpose infix => infix.Name,
                TopLevelExpose.FunctionExpose func => func.Name,
                TopLevelExpose.TypeOrAliasExpose type => type.Name,
                TopLevelExpose.TypeExpose typeExpose => typeExpose.ExposedType.Name,
                _ => throw new System.NotImplementedException(
                    $"Getting base name for expose type '{expose.GetType().Name}' is not implemented in {nameof(GetTopLevelExposeBaseName)}.")
            };
        }

        #endregion

        #region Import Formatting

        private static (IReadOnlyList<Node<Import>>, FormattingContext) FormatImports(
            IReadOnlyList<Node<Import>> imports,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            if (!imports.Any())
                return ([], context);

            var sortedImports =
                imports
                .OrderBy(import => string.Join(".", import.Value.ModuleName.Value), System.StringComparer.Ordinal)
                .ToList();

            var formattedImports = new List<Node<Import>>();
            var currentContext = context;

            foreach (var import in sortedImports)
            {
                var (formattedImport, nextContext) = FormatImport(import, currentContext, commentQueries);
                formattedImports.Add(formattedImport);
                // Reset indent to zero before next import since imports always start at column 1
                currentContext = nextContext.ResetIndent().NextRowToIndent();
            }

            return (formattedImports, currentContext);
        }

        private static (Node<Import>, FormattingContext) FormatImport(
            Node<Import> import,
            FormattingContext context,
            CommentQueryHelper commentQueries)
        {
            var importTokenLoc = context.CurrentLocation();
            var afterImportKeyword = context.Advance(Keywords.Import.Length).AdvanceSpaceSeparator();

            // Reference context for indented content (one indent level from start of import)
            var indentedRef = context.CreateIndentedRef();

            var moduleName = string.Join(".", import.Value.ModuleName.Value);
            var afterModuleName = afterImportKeyword.Advance(moduleName.Length);

            var currentContext = afterModuleName;

            (Location AsTokenLocation, Node<IReadOnlyList<string>> Alias)? moduleAlias = null;
            if (import.Value.ModuleAlias is { } alias)
            {
                // Advance for space before "as"
                var afterSpace = currentContext.AdvanceSpaceSeparator();
                var asTokenLoc = afterSpace.CurrentLocation();
                // Advance past "as " ("as" + space after)
                currentContext = afterSpace.Advance(Keywords.As.Length).AdvanceSpaceSeparator();
                var aliasName = string.Join(".", alias.Alias.Value);
                var aliasStartLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(aliasName.Length);
                moduleAlias = (asTokenLoc, MakeNode(aliasStartLoc, currentContext.CurrentLocation(), alias.Alias.Value));
            }

            (Location ExposingTokenLocation, Node<Exposing> ExposingList)? exposingList = null;
            if (import.Value.ExposingList is { } exposing)
            {
                // Determine if the exposing list content is multiline
                // For multiline: exposing keyword goes on new line with indentation
                // For single-line: exposing stays on same line as module name
                var isExposingListMultiline =
                    exposing.ExposingList.Value is Exposing.Explicit explicitExposing
                    && IsExposingListMultiLine(explicitExposing);

                if (isExposingListMultiline)
                {
                    // Exposing on new line with 4-space indentation from start of import
                    currentContext = currentContext.ReturnToIndent(indentedRef).NextRowToIndent();
                    var exposingTokenLoc = currentContext.CurrentLocation();
                    // FormatExposing expects to be positioned BEFORE "exposing" keyword
                    var (formattedExposing, afterExposingList) = FormatExposing(exposing.ExposingList, currentContext, commentQueries);
                    exposingList = (exposingTokenLoc, formattedExposing);
                    currentContext = afterExposingList;
                }
                else
                {
                    // Exposing on same line - format it properly
                    currentContext = currentContext.AdvanceSpaceSeparator(); // space before "exposing"
                    var exposingTokenLoc = currentContext.CurrentLocation();
                    // FormatExposing expects to be positioned BEFORE "exposing" keyword
                    var (formattedExposing, afterExposingList) = FormatExposing(exposing.ExposingList, currentContext, commentQueries);
                    exposingList = (exposingTokenLoc, formattedExposing);
                    currentContext = afterExposingList;
                }
            }

            var range = MakeRange(context.CurrentLocation(), currentContext.CurrentLocation());
            var moduleNameNode = MakeNode(range, import.Value.ModuleName.Value);

            var formattedImport = new Import(
                ImportTokenLocation: importTokenLoc,
                ModuleName: moduleNameNode,
                ModuleAlias: moduleAlias,
                ExposingList: exposingList
            );

            return (MakeNode(range, formattedImport), currentContext);
        }

        #endregion

        #region Declaration Formatting

        /// <summary>
        /// Formats both complete and incomplete declarations, interleaving them by their original positions.
        /// </summary>  
        private (IReadOnlyList<Node<Declaration>>, FormattingContext, IReadOnlyList<Node<IncompleteDeclaration>>) FormatDeclarationsWithIncompletes(
            IReadOnlyList<Node<Declaration>> declarations,
            FormattingContext context)
        {
            // Create a union of complete and incomplete declarations sorted by position
            // We use a discriminated union approach with an interface
            var allItems = new List<(int row, bool isComplete, int originalIndex)>();

            for (var i = 0; i < declarations.Count; i++)
            {
                allItems.Add((declarations[i].Range.Start.Row, isComplete: true, originalIndex: i));
            }

            for (var i = 0; i < originalIncompleteDeclarations.Count; i++)
            {
                allItems.Add((originalIncompleteDeclarations[i].Range.Start.Row, isComplete: false, originalIndex: i));
            }

            // Sort by row to interleave correctly
            allItems.Sort((a, b) => a.row.CompareTo(b.row));

            if (allItems.Count is 0)
                return ([], context, []);

            var formattedDeclarations = new List<Node<Declaration>>();
            var formattedIncompletes = new List<Node<IncompleteDeclaration>>();
            var currentContext = context;

            for (var i = 0; i < allItems.Count; i++)
            {
                var (row, isComplete, originalIndex) = allItems[i];

                if (isComplete)
                {
                    var decl = declarations[originalIndex];
                    var declContext = currentContext.ResetIndent();

                    var declResult =
                        FormatDeclaration(decl, declContext);

                    formattedDeclarations.Add(declResult.FormattedNode);
                    var nextContext = declResult.Context;

                    // Update context for next item
                    if (i < allItems.Count - 1)
                    {
                        var nextItem = allItems[i + 1];

                        if (nextItem.isComplete)
                        {
                            var nextDecl = declarations[nextItem.originalIndex];
                            var commentsBetween = commentQueries.GetBetweenRowsInclusiveEnd(
                                decl.Range.End.Row, nextDecl.Range.Start.Row);

                            if (declResult.FormattedNode.Value is Declaration.InfixDeclaration && nextDecl.Value is Declaration.InfixDeclaration)
                            {
                                currentContext = nextContext.NextRowToIndent();
                            }
                            else
                            {
                                currentContext = CommentPlacementHelper.PlaceCommentsBetweenDeclarations(nextContext, commentsBetween);
                            }
                        }
                        else
                        {
                            // Next item is incomplete - just add blank lines
                            currentContext = nextContext.WithBlankLine().NextRowToIndent();
                        }
                    }
                    else
                    {
                        currentContext = nextContext;
                    }
                }
                else
                {
                    // Handle incomplete declaration
                    var incompleteDecl = originalIncompleteDeclarations[originalIndex];
                    var originalText = incompleteDecl.Value.OriginalText;
                    var startLoc = currentContext.CurrentLocation();
                    var endLoc = CalculateEndLocation(startLoc, originalText);

                    // Preserve the original error location and message
                    formattedIncompletes.Add(MakeNode(
                        new Range(startLoc, endLoc),
                        new IncompleteDeclaration(
                            originalText,
                            incompleteDecl.Value.ErrorLocation,
                            incompleteDecl.Value.ErrorMessage)));

                    // Advance context past this incomplete declaration
                    foreach (var ch in originalText)
                    {
                        if (ch is '\n')
                        {
                            currentContext = currentContext.ResetIndent().NextRowToIndent();
                        }
                        else
                        {
                            currentContext = currentContext.Advance(1);
                        }
                    }

                    // Add blank lines before the next item
                    if (i < allItems.Count - 1)
                    {
                        currentContext = currentContext.WithBlankLine().NextRowToIndent();
                    }
                }
            }

            return (formattedDeclarations, currentContext, formattedIncompletes);
        }

        private FormattingResult<Node<Declaration>> FormatDeclaration(
            Node<Declaration> decl,
            FormattingContext context)
        {
            return decl.Value switch
            {
                Declaration.FunctionDeclaration funcDecl =>
                FormatFunctionDeclaration(funcDecl, decl.Range, context),

                Declaration.AliasDeclaration aliasDecl =>
                FormatAliasDeclaration(aliasDecl, context),

                Declaration.CustomTypeDeclaration customTypeDecl =>
                FormatCustomTypeDeclaration(customTypeDecl, context),

                Declaration.InfixDeclaration infixDecl =>
                FormatInfixDeclaration(infixDecl, context),

                Declaration.PortDeclaration portDecl =>
                FormatPortDeclaration(portDecl, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented " +
                    $"at row {decl.Range.Start.Row}, column {decl.Range.Start.Column}.")
            };
        }

        private FormattingResult<Node<Declaration>> FormatFunctionDeclaration(
            Declaration.FunctionDeclaration funcDecl,
            Range originalDeclRange,
            FormattingContext context)
        {
            var currentContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            // Format documentation comment if present using unified helper
            if (funcDecl.Function.Documentation is { } docComment)
            {
                currentContext = CommentPlacementHelper.PlaceDocComment(currentContext, docComment);

                // Check for comments between doc comment and the next content (signature or function name)
                var docCommentEndRow = docComment.Range.End.Row;
                var nextContentStartRow = funcDecl.Function.Signature is { } sig
                    ? sig.Value.Name.Range.Start.Row
                    : funcDecl.Function.Declaration.Value.Name.Range.Start.Row;

                var commentsBetweenDocAndContent = commentQueries.GetBetweenRows(docCommentEndRow, nextContentStartRow);
                if (commentsBetweenDocAndContent.Count > 0)
                {
                    // Format these comments with section comment spacing (3 blank lines before)
                    // WithBlankLine() = 2 row advances, plus NextRowToIndent() = 1 more, for total of 3 blank lines
                    currentContext = currentContext.WithBlankLine().NextRowToIndent();
                    foreach (var comment in commentsBetweenDocAndContent)
                    {
                        currentContext = currentContext.ResetIndent().SetIndentColumn();
                        currentContext = currentContext.FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                    // Add spacing after section comments (2 blank lines)
                    currentContext = currentContext.WithBlankLine();
                }
            }

            // Format signature if present
            Node<Signature>? formattedSignature = null;
            if (funcDecl.Function.Signature is { } signature)
            {
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);

                // Detect if type annotation should be on a new line after the colon
                var isTypeAnnotOnNewLine = signature.Value.TypeAnnotation.Range.Start.Row > signature.Value.Name.Range.Start.Row;

                var colonLoc = afterSigName.Advance(1).CurrentLocation(); // space before colon
                var afterColon = afterSigName.Advance(2); // " :"

                FormattingResult<Node<TypeAnnotation>> typeAnnotResult;
                if (isTypeAnnotOnNewLine)
                {
                    // Type annotation on new line with indentation - no space after colon
                    var typeContext = afterColon.ReturnToIndent(indentedRef).NextRowToIndent();

                    // Handle comments between colon and type annotation
                    var colonRow = signature.Value.ColonLocation.Row;
                    var typeAnnotStartRow = signature.Value.TypeAnnotation.Range.Start.Row;
                    var commentsBeforeType = commentQueries.GetBetweenRows(colonRow, typeAnnotStartRow);

                    typeContext =
                        typeContext
                        .FormatAndAddComments(commentsBeforeType)
                        .NextRowToIndentIfCurrentColumnGreaterThanIndent();

                    typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);
                }
                else
                {
                    // Type annotation on same line after " : "
                    var sameLineContext = afterColon.Advance(1); // space after colon
                    typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, sameLineContext);
                }

                var sigRange = MakeRange(currentContext.CurrentLocation(), typeAnnotResult.Context.CurrentLocation());
                formattedSignature = MakeNode(sigRange, new Signature(
                    Name: MakeNode(
                        currentContext.CurrentLocation(),
                        afterSigName.CurrentLocation(),
                        sigName),
                    ColonLocation: colonLoc,
                    TypeAnnotation: typeAnnotResult.FormattedNode
                ));

                // Return to base indent level for the implementation
                currentContext = typeAnnotResult.Context.ReturnToIndent(context).NextRowToIndent();
            }

            var impl = funcDecl.Function.Declaration.Value;

            // Function name
            var funcNameStartLoc = currentContext.CurrentLocation();
            var afterName = currentContext.Advance(impl.Name.Value.Length);

            // Arguments - format with updated ranges
            var formattedArguments = new List<Node<Pattern>>();
            var afterArgs = afterName;
            foreach (var arg in impl.Arguments)
            {
                afterArgs = afterArgs.Advance(1); // space before arg
                var patternResult = FormatPattern(arg, afterArgs);
                afterArgs = patternResult.Context;
                formattedArguments.Add(patternResult.FormattedNode);
            }

            // " ="
            var equalsLoc = afterArgs.Advance(1).CurrentLocation(); // space before =
            var afterEquals = afterArgs.Advance(2); // " ="

            // Move to next line and indent for the expression
            var exprContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

            // Check for comments between equals and expression in original
            var equalsRow = funcDecl.Function.Declaration.Value.EqualsTokenLocation.Row;
            var exprStartRow = impl.Expression.Range.Start.Row;
            var commentsBeforeExpr = commentQueries.GetBetweenRows(equalsRow, exprStartRow);

            // Format any comments that appear before the expression
            exprContext =
                exprContext
                .FormatAndAddComments(commentsBeforeExpr)
                .NextRowToIndentIfCurrentColumnGreaterThanIndent();

            // Format the expression with updated locations
            var exprResult = FormatExpression(impl.Expression, exprContext);

            // Build formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: MakeNode(funcNameStartLoc, afterName.CurrentLocation(), impl.Name.Value),
                Arguments: formattedArguments,
                EqualsTokenLocation: equalsLoc,
                Expression: exprResult.FormattedNode
            );

            var formattedFunc = new FunctionStruct(
                Documentation: funcDecl.Function.Documentation,
                Signature: formattedSignature,
                Declaration: MakeNode(
                    currentContext.CurrentLocation(),
                    exprResult.Context.CurrentLocation(),
                    formattedImpl)
            );

            var range = MakeRange(context.CurrentLocation(), exprResult.Context.CurrentLocation());
            return FormattingResult<Node<Declaration>>.Create(
                MakeNode<Declaration>(range, new Declaration.FunctionDeclaration(formattedFunc)),
                exprResult.Context.ReturnToIndent(context));
        }

        private FormattingResult<Node<Declaration>> FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context)
        {
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            // Format documentation comment if present using unified helper
            if (aliasDecl.TypeAlias.Documentation is { } docComment)
            {
                startContext = CommentPlacementHelper.PlaceDocComment(startContext, docComment);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(Keywords.Type.Length).AdvanceSpaceSeparator(); // "type "
            var aliasTokenLoc = afterType.CurrentLocation();
            var afterTypeAlias = afterType.Advance(Keywords.Alias.Length).AdvanceSpaceSeparator(); // "alias "

            var aliasName = aliasDecl.TypeAlias.Name.Value;
            var nameStartLoc = afterTypeAlias.CurrentLocation();
            var afterName = afterTypeAlias.Advance(aliasName.Length);
            var formattedName = MakeNode(nameStartLoc, afterName.CurrentLocation(), aliasName);

            var currentContext = afterName;
            var formattedGenerics = new List<Node<string>>();
            foreach (var generic in aliasDecl.TypeAlias.Generics)
            {
                currentContext = currentContext.AdvanceSpaceSeparator();
                var genericStartLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(generic.Value.Length);
                formattedGenerics.Add(MakeNode(genericStartLoc, currentContext.CurrentLocation(), generic.Value));
            }

            // " =" space before equals
            var afterNameSpace = currentContext.AdvanceSpaceSeparator();
            var equalsLoc = afterNameSpace.CurrentLocation();
            var afterEquals = afterNameSpace.Advance(Keywords.Equals.Length); // just "="

            // Save parent context for later indent restoration
            var parentContext = afterEquals;
            var typeContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

            // Check for comments between the equals sign and the type annotation
            var commentsBeforeType = commentQueries.GetBetweenRows(
                aliasDecl.TypeAlias.EqualsTokenLocation.Row,
                aliasDecl.TypeAlias.TypeAnnotation.Range.Start.Row);

            foreach (var comment in commentsBeforeType)
            {
                typeContext = typeContext.FormatAndAddCommentAndNextRowToIndent(comment);
            }

            // Format the type annotation with proper locations
            var typeAnnotResult = FormatTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, typeContext);

            var formattedTypeAlias = new TypeAlias(
                Documentation: aliasDecl.TypeAlias.Documentation,
                TypeTokenLocation: typeTokenLoc,
                AliasTokenLocation: aliasTokenLoc,
                Name: formattedName,
                Generics: formattedGenerics,
                EqualsTokenLocation: equalsLoc,
                TypeAnnotation: typeAnnotResult.FormattedNode
            );

            var finalContext = typeAnnotResult.Context.ReturnToIndent(parentContext);
            var range = MakeRange(startContext.CurrentLocation(), finalContext.CurrentLocation());
            return FormattingResult<Node<Declaration>>.Create(
                MakeNode<Declaration>(range, new Declaration.AliasDeclaration(formattedTypeAlias)),
                finalContext);
        }

        private FormattingResult<Node<Declaration>> FormatCustomTypeDeclaration(
            Declaration.CustomTypeDeclaration customTypeDecl,
            FormattingContext context)
        {
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            // Format documentation comment if present using unified helper
            if (customTypeDecl.TypeDeclaration.Documentation is { } docComment)
            {
                startContext = CommentPlacementHelper.PlaceDocComment(startContext, docComment);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(Keywords.Type.Length).AdvanceSpaceSeparator();

            var typeName = customTypeDecl.TypeDeclaration.Name.Value;
            var afterName = afterType.Advance(typeName.Length);

            var currentContext = afterName;
            var formattedGenerics = new List<Node<string>>();
            foreach (var generic in customTypeDecl.TypeDeclaration.Generics)
            {
                currentContext = currentContext.AdvanceSpaceSeparator();
                var genericLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(generic.Value.Length);
                formattedGenerics.Add(MakeNode(genericLoc, currentContext.CurrentLocation(), generic.Value));
            }

            // Move to next line for constructors
            var constructorIndentContext = currentContext.ReturnToIndent(indentedRef).NextRowToIndent();

            var equalsLoc = constructorIndentContext.CurrentLocation();
            var afterEquals = constructorIndentContext.Advance(Keywords.Equals.Length).AdvanceSpaceSeparator();

            // Format constructors
            var formattedConstructors = new List<(Location? PipeLocation, Node<ValueConstructor> Constructor)>();
            var constructorCtx = afterEquals;

            for (var i = 0; i < customTypeDecl.TypeDeclaration.Constructors.Count; i++)
            {
                var (pipeLocation, constructor) = customTypeDecl.TypeDeclaration.Constructors[i];

                Location? formattedPipeLoc = null;
                if (i is 0)
                {
                    // Check for comments between equals sign and first constructor
                    // This includes comments on the same row as = (after it but before constructor) and rows in between
                    var equalsRow = customTypeDecl.TypeDeclaration.EqualsTokenLocation.Row;
                    var equalsColumn = customTypeDecl.TypeDeclaration.EqualsTokenLocation.Column;
                    var constructorOnSameRowAsEquals = constructor.Range.Start.Row == equalsRow;

                    // Get comments between = and constructor on the same row (not trailing comments on args)
                    var commentsAfterEqualsOnSameLine = constructorOnSameRowAsEquals
                        ? commentQueries.GetOnRowBetweenColumns(equalsRow, equalsColumn, constructor.Range.Start.Column).ToList()
                        : [.. commentQueries.GetOnRowAfterColumn(equalsRow, equalsColumn)];
                    var commentsBetweenRows = constructor.Range.Start.Row > equalsRow
                        ? commentQueries.GetBetweenRows(equalsRow, constructor.Range.Start.Row).ToList()
                        : [];

                    // Combine comments: same line first, then between rows
                    var allCommentsBeforeFirstConstructor = commentsAfterEqualsOnSameLine
                        .Concat(commentsBetweenRows)
                        .ToList();

                    if (allCommentsBeforeFirstConstructor.Count is not 0)
                    {
                        // Comment gets extra 2 space indent (6 spaces total = 4 base indent + 2 extra)
                        var commentIndentRef = constructorIndentContext.SetIndentColumn().Advance(2).SetIndentToCurrentColumn();

                        // Handle inline comments (constructor on same line as equals)
                        if (constructorOnSameRowAsEquals && commentsAfterEqualsOnSameLine.Count > 0 && commentsBetweenRows.Count is 0)
                        {
                            // All comments and constructor on same line - format inline
                            foreach (var comment in commentsAfterEqualsOnSameLine)
                            {
                                constructorCtx = constructorCtx.FormatAndAddComment(comment);
                                constructorCtx = constructorCtx.Advance(1); // space after comment
                            }
                            // Constructor stays inline, no need to position at indent
                        }
                        else
                        {
                            // Comments span multiple lines - format on separate lines
                            // First comment on same line as = stays on that line
                            if (commentsAfterEqualsOnSameLine.Count > 0)
                            {
                                var firstComment = commentsAfterEqualsOnSameLine[0];
                                constructorCtx = constructorCtx.FormatAndAddCommentAndNextRowToIndent(firstComment);
                                constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef);

                                // Format remaining same-line comments (if any) - they go on separate rows
                                for (var ci = 1; ci < commentsAfterEqualsOnSameLine.Count; ci++)
                                {
                                    // FormatAndAddComment already positioned us on a new row, just set indent
                                    constructorCtx = constructorCtx.SetIndentColumn();
                                    constructorCtx = constructorCtx.FormatAndAddCommentAndNextRowToIndent(commentsAfterEqualsOnSameLine[ci]);
                                    constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef);
                                }
                            }

                            // Format comments from between rows
                            var isFirstBetweenComment = true;
                            foreach (var comment in commentsBetweenRows)
                            {
                                // First between comment: if we just wrote a same-line comment, we're already on a new row
                                // Otherwise we need to move to the next row
                                if (isFirstBetweenComment && commentsAfterEqualsOnSameLine.Count is 0)
                                {
                                    // No same-line comments, so we need to go to next row
                                    constructorCtx = constructorCtx.NextRowToIndent();
                                }
                                else
                                {
                                    // FormatAndAddComment already positioned us on a new row, just set indent
                                    constructorCtx = constructorCtx.SetIndentColumn();
                                }
                                isFirstBetweenComment = false;
                                constructorCtx = constructorCtx.FormatAndAddCommentAndNextRowToIndent(comment);
                                constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef);
                            }
                            // After comments, position for the first constructor at comment indent level
                            // (constructor aligns with comments at 6 spaces)
                            constructorCtx = constructorCtx.SetIndentColumn();
                        }
                    }
                }
                else
                {
                    // Check for comments between this and previous constructor
                    var prevConstructor = customTypeDecl.TypeDeclaration.Constructors[i - 1].Constructor;
                    var commentsBetweenConstructors = commentQueries.GetBetweenRanges(prevConstructor.Range, constructor.Range).ToList();

                    // Also check for inline comments on the same row as the constructor (before the constructor name)
                    // The pipe location is at pipeLocation, and comments would be between the pipe and the constructor name
                    var pipeRow = pipeLocation?.Row ?? constructor.Range.Start.Row;
                    var inlineComments = pipeLocation.HasValue
                        ? commentQueries.GetOnRowBetweenColumns(pipeRow, pipeLocation.Value.Column, constructor.Value.Name.Range.Start.Column).ToList()
                        : [];

                    if (commentsBetweenConstructors.Count is not 0)
                    {
                        // Format comments between constructors
                        // Comment gets extra 2 space indent (6 spaces total = 4 base indent + 2 extra)
                        var commentIndentRef = constructorIndentContext.SetIndentColumn().Advance(2).SetIndentToCurrentColumn();
                        var isFirstComment = true;
                        foreach (var comment in commentsBetweenConstructors)
                        {
                            if (isFirstComment)
                            {
                                // First comment needs to go to new row
                                constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef).NextRowToIndent();
                                isFirstComment = false;
                            }
                            else
                            {
                                // FormatAndAddComment already positioned us on a new row
                                constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef).SetIndentColumn();
                            }
                            constructorCtx = constructorCtx.FormatAndAddCommentAndNextRowToIndent(comment);
                        }
                        // After comments, position for the pipe and constructor
                        constructorCtx = constructorCtx.ReturnToIndent(constructorIndentContext).SetIndentColumn();
                    }
                    else
                    {
                        // Move to next line for subsequent constructors (only when no comments between constructors)
                        constructorCtx = constructorCtx.NextRowToIndent();
                    }
                    formattedPipeLoc = constructorCtx.CurrentLocation();
                    constructorCtx = constructorCtx.Advance(2); // "| "

                    // Format any inline comments after the pipe and before the constructor name
                    foreach (var inlineComment in inlineComments)
                    {
                        constructorCtx = constructorCtx.FormatAndAddComment(inlineComment);
                        constructorCtx = constructorCtx.Advance(1); // space after comment
                    }
                }

                // Constructor name
                var constructorStartLoc = constructorCtx.CurrentLocation();
                var afterConstructorName = constructorCtx.Advance(constructor.Value.Name.Value.Length);

                // Create reference context for argument indentation (2 spaces from constructor name)
                var argIndentRef = constructorCtx.Advance(2).SetIndentToCurrentColumn();

                // Constructor arguments
                var formattedArgs = new List<Node<TypeAnnotation>>();
                var argCtx = afterConstructorName;
                for (var argIndex = 0; argIndex < constructor.Value.Arguments.Count; argIndex++)
                {
                    var arg = constructor.Value.Arguments[argIndex];

                    // Determine where to look for comments based on whether this is the first argument
                    int searchStartRow;
                    int searchStartCol;
                    if (argIndex is 0)
                    {
                        // For first argument, search from constructor name row/col
                        searchStartRow = constructor.Value.Name.Range.End.Row;
                        searchStartCol = constructor.Value.Name.Range.End.Column;
                    }
                    else
                    {
                        // For subsequent arguments, search from previous argument's end
                        var prevArg = constructor.Value.Arguments[argIndex - 1];
                        searchStartRow = prevArg.Range.End.Row;
                        searchStartCol = prevArg.Range.End.Column;
                    }

                    // Check for comments between this argument and the previous (on separate rows)
                    var commentsBetween = commentQueries.GetBetweenRows(searchStartRow, arg.Range.Start.Row).ToList();

                    // Also check for inline comments on the same row (between prev arg end and current arg start)
                    var inlineComments = (searchStartRow == arg.Range.Start.Row)
                        ? commentQueries.GetOnRowBetweenColumns(searchStartRow, searchStartCol, arg.Range.Start.Column).ToList()
                        : [];

                    if (commentsBetween.Count > 0)
                    {
                        // Arguments with comments between them should be on separate lines
                        var isFirstComment = true;
                        foreach (var comment in commentsBetween)
                        {
                            if (isFirstComment)
                            {
                                argCtx = argCtx.ReturnToIndent(argIndentRef).NextRowToIndent();
                                isFirstComment = false;
                            }
                            else
                            {
                                // FormatAndAddComment already positioned us on a new row
                                argCtx = argCtx.ReturnToIndent(argIndentRef).SetIndentColumn();
                            }
                            argCtx = argCtx.FormatAndAddCommentAndNextRowToIndent(comment);
                        }

                        // FormatAndAddComment already positioned us on the next row, just set indent
                        argCtx = argCtx.ReturnToIndent(argIndentRef).SetIndentColumn();
                        var argResult = FormatTypeAnnotation(arg, argCtx);
                        formattedArgs.Add(argResult.FormattedNode);
                        argCtx = argResult.Context;
                        continue;
                    }

                    // Handle inline comments on the same row
                    if (inlineComments.Count > 0)
                    {
                        foreach (var comment in inlineComments)
                        {
                            argCtx = argCtx.Advance(1); // space before comment
                            argCtx = argCtx.FormatAndAddComment(comment);
                        }
                    }

                    // Check if argument is on a new line in the original
                    var isArgOnNewLine =
                        argIndex is 0
                        ? arg.Range.Start.Row > constructor.Value.Name.Range.End.Row
                        : arg.Range.Start.Row > constructor.Value.Arguments[argIndex - 1].Range.End.Row;

                    if (isArgOnNewLine)
                    {
                        // Argument on new line with indentation (2 spaces from constructor name)
                        argCtx = argCtx.ReturnToIndent(argIndentRef).NextRowToIndent();
                    }
                    else
                    {
                        argCtx = argCtx.Advance(1); // space
                    }

                    var fmtArgResult = FormatTypeAnnotation(arg, argCtx);
                    formattedArgs.Add(fmtArgResult.FormattedNode);
                    argCtx = fmtArgResult.Context;
                }

                // Check for trailing comment - on last argument if any, otherwise on constructor name
                if (constructor.Value.Arguments.Count > 0)
                {
                    var lastArg = constructor.Value.Arguments[constructor.Value.Arguments.Count - 1];
                    var trailingComment = commentQueries.GetTrailing(lastArg.Range);
                    if (trailingComment is not null)
                    {
                        argCtx = argCtx.Advance(1); // space before comment
                        argCtx = argCtx.FormatAndAddComment(trailingComment);
                    }
                }
                else
                {
                    // No arguments - check for trailing comment on constructor name
                    var trailingComment = commentQueries.GetTrailing(constructor.Value.Name.Range);
                    if (trailingComment is not null)
                    {
                        argCtx = argCtx.Advance(1); // space before comment
                        argCtx = argCtx.FormatAndAddComment(trailingComment);
                    }
                }

                var formattedConstructor = new ValueConstructor(
                    MakeNode(
                        constructorStartLoc,
                        afterConstructorName.CurrentLocation(),
                        constructor.Value.Name.Value),
                    formattedArgs);

                formattedConstructors.Add(
                    (formattedPipeLoc, MakeNode(constructorStartLoc, argCtx.CurrentLocation(), formattedConstructor)));

                // Restore constructor-level indent after formatting arguments
                constructorCtx = argCtx.ReturnToIndent(constructorIndentContext);
            }

            var formattedTypeStruct = new TypeStruct(
                Documentation: customTypeDecl.TypeDeclaration.Documentation,
                TypeTokenLocation: typeTokenLoc,
                Name: MakeNode(afterType.CurrentLocation(), afterName.CurrentLocation(), typeName),
                Generics: formattedGenerics,
                EqualsTokenLocation: equalsLoc,
                Constructors: formattedConstructors
            );

            var finalContext = constructorCtx.ReturnToIndent(startContext);
            var range = MakeRange(startContext.CurrentLocation(), constructorCtx.CurrentLocation());
            return FormattingResult<Node<Declaration>>.Create(
                MakeNode<Declaration>(range, new Declaration.CustomTypeDeclaration(formattedTypeStruct)),
                finalContext);
        }

        private static FormattingResult<Node<Declaration>> FormatInfixDeclaration(
            Declaration.InfixDeclaration infixDecl,
            FormattingContext context)
        {
            // Direction field width for alignment: "right" (5), "left" (4), "non" (3)
            // Pad to 6 chars to align precedence across all infix declarations
            const int DirectionFieldWidth = 6;

            // "infix "
            var infixTokenLoc = context.CurrentLocation();
            var afterInfixKeyword = context.Advance(Keywords.Infix.Length).AdvanceSpaceSeparator();

            // Direction (left/right/non) - elm-format aligns precedence by padding direction
            var directionLoc = afterInfixKeyword.CurrentLocation();

            var directionText =
                infixDecl.Infix.Direction.Value switch
                {
                    InfixDirection.Left =>
                    "left",

                    InfixDirection.Right =>
                    "right",

                    InfixDirection.Non =>
                    "non",

                    _ =>
                    throw new System.NotImplementedException(
                        $"InfixDirection '{infixDecl.Infix.Direction.Value}' not supported")
                };

            var afterDirection = afterInfixKeyword.Advance(directionText.Length);

            // Pad to align precedence
            var paddingSpaces = System.Math.Max(0, DirectionFieldWidth - directionText.Length);
            var afterDirectionPadding = afterDirection.Advance(paddingSpaces);

            // Precedence
            var precedenceLoc = afterDirectionPadding.CurrentLocation();
            var precedenceText = infixDecl.Infix.Precedence.Value.ToString();
            var afterPrecedence = afterDirectionPadding.Advance(precedenceText.Length).AdvanceSpaceSeparator();

            // Operator "(op)"
            var operatorLoc = afterPrecedence.CurrentLocation();
            var operatorText = $"({infixDecl.Infix.Operator.Value})";
            var afterOperator = afterPrecedence.Advance(operatorText.Length).AdvanceSpaceSeparator();

            // "="
            var equalsLoc = afterOperator.CurrentLocation();
            var afterEquals = afterOperator.Advance(1).AdvanceSpaceSeparator();

            // Function name
            var functionNameLoc = afterEquals.CurrentLocation();
            var afterFunctionName = afterEquals.Advance(infixDecl.Infix.FunctionName.Value.Length);

            var range = MakeRange(context.CurrentLocation(), afterFunctionName.CurrentLocation());

            var formattedDirection = MakeNode(
                directionLoc,
                afterInfixKeyword.Advance(directionText.Length).CurrentLocation(),
                infixDecl.Infix.Direction.Value);

            var formattedPrecedence = MakeNode(
                precedenceLoc,
                afterDirectionPadding.Advance(precedenceText.Length).CurrentLocation(),
                infixDecl.Infix.Precedence.Value);

            var formattedOperator = MakeNode(
                operatorLoc,
                afterPrecedence.Advance(operatorText.Length).CurrentLocation(),
                infixDecl.Infix.Operator.Value);

            var formattedFunctionName = MakeNode(
                functionNameLoc,
                afterFunctionName.CurrentLocation(),
                infixDecl.Infix.FunctionName.Value);

            var formattedInfix = new Infix(
                InfixTokenLocation: infixTokenLoc,
                Direction: formattedDirection,
                Precedence: formattedPrecedence,
                Operator: formattedOperator,
                EqualsTokenLocation: equalsLoc,
                FunctionName: formattedFunctionName
            );

            return FormattingResult<Node<Declaration>>.Create(
                MakeNode<Declaration>(range, new Declaration.InfixDeclaration(formattedInfix)),
                afterFunctionName);
        }

        private static FormattingResult<Node<Declaration>> FormatPortDeclaration(
            Declaration.PortDeclaration portDecl,
            FormattingContext context)
        {
            var portTokenLoc = context.CurrentLocation();
            var afterPort = context.Advance(Keywords.Port.Length).AdvanceSpaceSeparator();

            var sigName = portDecl.Signature.Name.Value;
            var afterSigName = afterPort.Advance(sigName.Length);
            var afterSpace = afterSigName.AdvanceSpaceSeparator();
            var colonLoc = afterSpace.CurrentLocation();
            var afterColon = afterSpace.Advance(Keywords.Colon.Length);

            // Simplified type annotation
            var typeAnnotLength =
                portDecl.Signature.TypeAnnotation.Range.End.Column - portDecl.Signature.TypeAnnotation.Range.Start.Column;

            var afterType = afterColon.AdvanceSpaceSeparator().Advance(typeAnnotLength);

            var formattedSig =
                new Signature(
                    Name: MakeNode(
                        afterPort.CurrentLocation(),
                        afterSigName.CurrentLocation(),
                        sigName),
                ColonLocation: colonLoc,
                TypeAnnotation: portDecl.Signature.TypeAnnotation
            );

            var range =
                MakeRange(context.CurrentLocation(), afterType.CurrentLocation());

            return FormattingResult<Node<Declaration>>.Create(
                MakeNode<Declaration>(range, new Declaration.PortDeclaration(portTokenLoc, formattedSig)),
                afterType);
        }

        #endregion

        #region Type Annotation Formatting

        private FormattingResult<Node<TypeAnnotation>> FormatTypeAnnotation(
            Node<TypeAnnotation> typeAnnot,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null,
            bool forceMultilineArrows = false)
        {
            var startLoc = context.CurrentLocation();
            var typeResult = FormatTypeAnnotationValue(typeAnnot.Value, typeAnnot.Range, context, arrowBaseRef, forceMultilineArrows);
            return FormattingResult<Node<TypeAnnotation>>.Create(
                MakeNode(startLoc, typeResult.Context.CurrentLocation(), typeResult.FormattedNode),
                typeResult.Context);
        }

        private FormattingResult<TypeAnnotation> FormatTypeAnnotationValue(
            TypeAnnotation typeAnnot,
            Range originalRange,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null,
            bool forceMultilineArrows = false)
        {
            // Reference context for indented content (one indent level from current position)
            var indentedRef = context.CreateIndentedRef();

            switch (typeAnnot)
            {
                case TypeAnnotation.GenericType genericType:
                    return FormattingResult<TypeAnnotation>.Create(genericType, context.Advance(genericType.Name.Length));

                case TypeAnnotation.Typed typed:
                    {
                        var typeName = typed.TypeName.Value;

                        var typeNameText =
                            typeName.ModuleName.Count > 0
                            ? string.Join(".", typeName.ModuleName) + "." + typeName.Name
                            : typeName.Name;

                        var afterTypeName = context.Advance(typeNameText.Length);

                        var formattedArgs = new List<Node<TypeAnnotation>>();
                        var currentCtx = afterTypeName;

                        // Track the original row for multiline detection
                        var typeNameOriginalRow = typed.TypeName.Range.Start.Row;

                        for (var argIndex = 0; argIndex < typed.TypeArguments.Count; argIndex++)
                        {
                            var arg = typed.TypeArguments[argIndex];

                            // Check if this argument should be on a new line by comparing to original positions
                            var prevOriginalRow = argIndex is 0
                                ? typeNameOriginalRow
                                : typed.TypeArguments[argIndex - 1].Range.End.Row;
                            var isArgMultiline = arg.Range.Start.Row > prevOriginalRow;

                            if (isArgMultiline)
                            {
                                // Argument on new line with extra indentation
                                var argContext = currentCtx.ReturnToIndent(indentedRef).NextRowToIndent();
                                var argResult = FormatTypeAnnotation(arg, argContext);
                                formattedArgs.Add(argResult.FormattedNode);
                                currentCtx = argResult.Context.ReturnToIndent(context);
                            }
                            else
                            {
                                currentCtx = currentCtx.Advance(1); // space before arg
                                var argResult = FormatTypeAnnotation(arg, currentCtx);
                                formattedArgs.Add(argResult.FormattedNode);
                                currentCtx = argResult.Context;
                            }
                        }

                        var formattedTyped = new TypeAnnotation.Typed(
                            typed.TypeName,
                            formattedArgs
                        );
                        return FormattingResult<TypeAnnotation>.Create(formattedTyped, currentCtx);
                    }

                case TypeAnnotation.Unit:
                    return FormattingResult<TypeAnnotation>.Create(typeAnnot, context.Advance(2)); // "()"

                case TypeAnnotation.Tupled tupled:
                    {
                        var tupledElements = Stil4mElmSyntax7.FromFullSyntaxModel.ToList(tupled.TypeAnnotations);
                        var isSingleElement = tupledElements.Count is 1;

                        // Single element case (grouping parens): Use no spaces "(Element)"
                        // This is special and cannot use the generic formatter
                        if (isSingleElement)
                        {
                            return FormatTupledTypeAnnotationSingleElement(
                                tupled, tupledElements[0], originalRange, context);
                        }

                        // Multi-element case (2+ elements): Use spaces "( Element1, Element2 )"
                        // This can use the generic FormatSeparatedListGeneral function
                        // Use originalRange to determine multiline layout (previously used OpenParenLocation/CloseParenLocation)
                        var containerRange = originalRange;

                        // Create reference context at opening paren column for alignment
                        var parenAlignRef = context.SetIndentToCurrentColumn();
                        // Create reference for content indented past paren
                        var elemContentRef = context.Advance(2).SetIndentToCurrentColumn();

                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "( ",
                            Separator: ", ",
                            CloseBracket: " )");

                        var multilineConfig = new SeparatedListConfig(
                            AlignmentRef: parenAlignRef,
                            ContentIndentRef: elemContentRef,
                            Separator: Keywords.Comma + " ",  // ", "
                            CloseBracket: ")");

                        var (formattedList, finalContext) = FormatSeparatedList(
                            separatedList: tupled.TypeAnnotations,
                            formatItem: (item, ctx) => FormatTypeAnnotation(item, ctx),
                            commentQueries: commentQueries,
                            singleLineConfig: singleLineConfig,
                            multilineConfig: multilineConfig,
                            context: context,
                            containerRange: containerRange,
                            commentStyle: SeparatedListCommentStyle.TupleStyle);

                        var formattedTupledAnnot = new TypeAnnotation.Tupled(
                            formattedList
                        );
                        return FormattingResult<TypeAnnotation>.Create(formattedTupledAnnot, finalContext);
                    }

                case TypeAnnotation.FunctionTypeAnnotation funcType:
                    {
                        // Check if the function type is multiline based on original layout
                        // We need to check if ANY part of the function type annotation spans multiple lines
                        // If so, ALL arrows should be formatted on new lines
                        var overallMultiline = originalRange.End.Row > originalRange.Start.Row || forceMultilineArrows;
                        var returnTypeOnNewLine = funcType.ReturnType.Range.Start.Row > funcType.ArgumentType.Range.Start.Row ||
                                                  overallMultiline;

                        // Propagate multiline flag to nested function types
                        var propagateMultiline = returnTypeOnNewLine;

                        // Check if the return type is on a separate line from the arrow
                        // This happens when -> is on its own line
                        var resultTypeOnOwnLine = funcType.ReturnType.Range.Start.Row > funcType.ArrowLocation.Row;

                        // Use passed reference or create one at current column if first encounter
                        var effectiveArrowBaseRef = arrowBaseRef ?? context.SetIndentToCurrentColumn();

                        var argTypeResult = FormatTypeAnnotation(funcType.ArgumentType, context, effectiveArrowBaseRef, propagateMultiline);

                        Location arrowLocation;
                        FormattingResult<Node<TypeAnnotation>> returnTypeResult;

                        if (returnTypeOnNewLine)
                        {
                            // Multiline: arrow on new line, at the base column
                            var newLineCtx = argTypeResult.Context.ReturnToIndent(effectiveArrowBaseRef).NextRowToIndent();
                            arrowLocation = newLineCtx.CurrentLocation();

                            if (resultTypeOnOwnLine)
                            {
                                // Arrow is on its own line, result type goes on next line with additional indentation
                                var afterArrow = newLineCtx.Advance(2); // "->" (no space after since we're going to new line)
                                // Create reference for result type at arrow base + 4
                                var resultTypeRef = effectiveArrowBaseRef.CreateIndentedRef();
                                var resultTypeCtx = afterArrow.ReturnToIndent(resultTypeRef).NextRowToIndent();
                                // Pass effectiveArrowBaseRef so nested function type arrows align properly
                                returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, resultTypeCtx, effectiveArrowBaseRef, propagateMultiline);
                            }
                            else
                            {
                                // Arrow on new line, but result type on same line as arrow
                                var arrowCtx = newLineCtx.Advance(3); // "-> "
                                returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, arrowCtx, effectiveArrowBaseRef, propagateMultiline);
                            }
                        }
                        else
                        {
                            // Single line: " -> "
                            var arrowCtx = argTypeResult.Context.Advance(1); // space before arrow
                            arrowLocation = arrowCtx.CurrentLocation();
                            var afterArrow = arrowCtx.Advance(3); // "-> "
                            returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, afterArrow, effectiveArrowBaseRef, propagateMultiline);
                        }

                        var formattedFuncType = new TypeAnnotation.FunctionTypeAnnotation(
                            argTypeResult.FormattedNode,
                            arrowLocation,
                            returnTypeResult.FormattedNode
                        );
                        // Return with IndentSpaces reset to the original context
                        return FormattingResult<TypeAnnotation>.Create(formattedFuncType, returnTypeResult.Context.ReturnToIndent(context));
                    }

                case TypeAnnotation.Record record:
                    {
                        // Format record type annotation
                        // Use originalRange to determine multiline layout
                        var containerRange = originalRange;

                        // Handle empty record as special case: {}
                        if (record.RecordDefinition.Fields is SeparatedSyntaxList<Node<RecordField>>.Empty)
                        {
                            var afterEmptyRecord = context.Advance(2); // After both "{" and "}"

                            var emptyRecordDef = new RecordDefinition(new SeparatedSyntaxList<Node<RecordField>>.Empty());
                            var emptyRecordResult = new TypeAnnotation.Record(
                                emptyRecordDef
                            );
                            return FormattingResult<TypeAnnotation>.Create(emptyRecordResult, afterEmptyRecord);
                        }

                        // Create reference context at opening brace column for alignment
                        var braceAlignRef = context.SetIndentToCurrentColumn();
                        // Create reference context at brace + 2 for field indent
                        var fieldContentRef = context.Advance(2).SetIndentToCurrentColumn();

                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "{ ",
                            Separator: ", ",
                            CloseBracket: " }");

                        var multilineConfig = new SeparatedListConfig(
                            AlignmentRef: braceAlignRef,
                            ContentIndentRef: fieldContentRef,
                            Separator: Keywords.Comma + " ",  // ", "
                            CloseBracket: "}");

                        // Capture context refs for the field formatter closure
                        var fieldIndentRef = fieldContentRef;

                        var (formattedList, finalContext) = FormatSeparatedList(
                            separatedList: record.RecordDefinition.Fields,
                            formatItem: (field, fieldCtx) => FormatRecordField(field, fieldCtx, fieldIndentRef),
                            commentQueries: commentQueries,
                            singleLineConfig: singleLineConfig,
                            multilineConfig: multilineConfig,
                            context: context,
                            containerRange: containerRange,
                            commentStyle: SeparatedListCommentStyle.ListStyle);

                        var formattedRecordDef = new RecordDefinition(formattedList);
                        var formattedRecord = new TypeAnnotation.Record(
                            formattedRecordDef
                        );
                        return FormattingResult<TypeAnnotation>.Create(formattedRecord, finalContext);
                    }

                case TypeAnnotation.GenericRecord genericRecord:
                    {
                        // Format generic record type annotation: { other | field1 : Type1, field2 : Type2 }
                        // Use originalRange to determine multiline layout
                        var containerRange = originalRange;

                        // Create reference context at opening brace column for alignment
                        var braceAlignRef = context.SetIndentToCurrentColumn();

                        // First output "{ " and then the generic name
                        var afterOpenBrace = context.Advance(2); // "{ "
                        var genericNameStartLoc = afterOpenBrace.CurrentLocation();
                        var afterGenericName = afterOpenBrace.Advance(genericRecord.GenericName.Value.Length);

                        // Handle comments between the opening brace and generic name
                        var commentsBeforeGenericName = commentQueries.GetBetweenRows(
                            containerRange.Start.Row,
                            genericRecord.GenericName.Range.Start.Row);

                        // Check for comments between generic name and pipe
                        var commentsBeforePipe = commentQueries.GetBetweenRows(
                            genericRecord.GenericName.Range.End.Row,
                            genericRecord.PipeLocation.Row);

                        // Handle multiline generic name section
                        var isMultiline = SpansMultipleRows(containerRange);
                        FormattingContext afterPipe;
                        Location pipeLoc;

                        // The indent for pipe and fields is always brace + 4
                        var pipeAndFieldAlignRef = braceAlignRef.Advance(4).SetIndentToCurrentColumn();

                        if (isMultiline || commentsBeforeGenericName.Count > 0 || commentsBeforePipe.Count > 0)
                        {
                            // Multiline handling
                            var ctx = afterOpenBrace;

                            // Handle comments before generic name
                            foreach (var comment in commentsBeforeGenericName)
                            {
                                ctx = ctx.ReturnToIndent(pipeAndFieldAlignRef).NextRowToIndent();
                                ctx = ctx.FormatAndAddComment(comment);
                            }

                            // Position at generic name
                            if (commentsBeforeGenericName.Count > 0)
                            {
                                ctx = ctx.ReturnToIndent(pipeAndFieldAlignRef).NextRowToIndent();
                            }
                            genericNameStartLoc = ctx.CurrentLocation();
                            ctx = ctx.Advance(genericRecord.GenericName.Value.Length);
                            afterGenericName = ctx;

                            // Handle comments between generic name and pipe
                            foreach (var comment in commentsBeforePipe)
                            {
                                ctx = ctx.ReturnToIndent(pipeAndFieldAlignRef).NextRowToIndent();
                                ctx = ctx.FormatAndAddComment(comment);
                            }

                            // Place pipe on new line if multiline or has comments
                            var pipeOnNewLine = genericRecord.PipeLocation.Row > genericRecord.GenericName.Range.End.Row || commentsBeforePipe.Count > 0;
                            if (pipeOnNewLine)
                            {
                                ctx = ctx.ReturnToIndent(pipeAndFieldAlignRef).NextRowToIndent();
                                pipeLoc = ctx.CurrentLocation();
                                afterPipe = ctx.Advance(1); // "|"
                            }
                            else
                            {
                                // Pipe on same line: "other |"
                                pipeLoc = afterGenericName.Advance(1).CurrentLocation();
                                afterPipe = afterGenericName.Advance(2); // " |"
                            }
                        }
                        else
                        {
                            // Single line: "{ other | ... }"
                            pipeLoc = afterGenericName.Advance(1).CurrentLocation();
                            afterPipe = afterGenericName.Advance(2); // " |"
                        }

                        // Now format the fields using FormatSeparatedList
                        // We need a custom config for generic record fields
                        var fieldContentRef = afterPipe.Advance(1).SetIndentToCurrentColumn();

                        // Build the container range for the fields portion (from after pipe to end)
                        var fieldsContainerRange = new Range(
                            genericRecord.RecordDefinition.Range.Start,
                            containerRange.End);

                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: " ",  // Just a space after pipe
                            Separator: ", ",
                            CloseBracket: " }");

                        // For multiline, use pipeAndFieldAlignRef for separator alignment,
                        // but braceAlignRef for close bracket alignment
                        var multilineConfig = new SeparatedListConfig(
                            AlignmentRef: pipeAndFieldAlignRef,
                            ContentIndentRef: fieldContentRef,
                            Separator: Keywords.Comma + " ",  // ", "
                            CloseBracket: "}",
                            CloseBracketAlignRef: braceAlignRef);

                        var fieldIndentRefForFields = fieldContentRef;

                        var (formattedList, finalContext) =
                            FormatSeparatedList(
                                separatedList: genericRecord.RecordDefinition.Value.Fields,
                                formatItem: (field, fieldCtx) => FormatRecordField(field, fieldCtx, fieldIndentRefForFields),
                                commentQueries: commentQueries,
                                singleLineConfig: singleLineConfig,
                                multilineConfig: multilineConfig,
                                context: afterPipe,
                                containerRange: fieldsContainerRange,
                                commentStyle: SeparatedListCommentStyle.ListStyle);

                        // Create formatted generic name node
                        var formattedGenericName = MakeNode(
                            genericNameStartLoc,
                            genericNameStartLoc with { Column = genericNameStartLoc.Column + genericRecord.GenericName.Value.Length },
                            genericRecord.GenericName.Value);

                        var formattedRecordDef = new RecordDefinition(formattedList);
                        var formattedRecordDefNode = new Node<RecordDefinition>(
                            genericRecord.RecordDefinition.Range,
                            formattedRecordDef);

                        var formattedGenericRecord = new TypeAnnotation.GenericRecord(
                            formattedGenericName,
                            pipeLoc,
                            formattedRecordDefNode
                        );
                        return FormattingResult<TypeAnnotation>.Create(formattedGenericRecord, finalContext);
                    }

                default:
                    throw new System.NotImplementedException(
                        $"Type annotation formatting not implemented for '{typeAnnot.GetType().Name}' " +
                        $"in {nameof(FormatTypeAnnotation)} at row {originalRange.Start.Row}, column {originalRange.Start.Column}.");
            }
        }

        /// <summary>
        /// Formats a single-element TypeAnnotation.Tupled (grouping parentheses).
        /// Single element case uses no spaces: "(Element)" instead of "( Element )"
        /// </summary>
        private FormattingResult<TypeAnnotation> FormatTupledTypeAnnotationSingleElement(
            TypeAnnotation.Tupled tupled,
            Node<TypeAnnotation> singleElement,
            Range originalRange,
            FormattingContext context)
        {
            // Detect if tuple should be multiline based on the containing node's range
            var isMultiline = SpansMultipleRows(originalRange);

            if (isMultiline)
            {
                // Multiline format for single element (grouping parens): (Element\n)
                var afterOpenParen = context.Advance(1);  // "("

                // Create reference context at opening paren column for alignment
                var parenAlignRef = context.SetIndentToCurrentColumn();

                var elemResult = FormatTypeAnnotation(singleElement, afterOpenParen);
                var elemCtx = elemResult.Context.ReturnToIndent(context);

                // Closing paren on new line, aligned with opening paren
                var closeCtx = elemCtx.ReturnToIndent(parenAlignRef).NextRowToIndent();
                var afterCloseParen = closeCtx.Advance(1); // ")"

                var separatedElems = new SeparatedSyntaxList<Node<TypeAnnotation>>.NonEmpty(
                    elemResult.FormattedNode,
                    []);

                var formattedTupledAnnot = new TypeAnnotation.Tupled(
                    separatedElems
                );
                return FormattingResult<TypeAnnotation>.Create(formattedTupledAnnot, afterCloseParen.ReturnToIndent(context));
            }
            else
            {
                // Single line format for single element (grouping parens): (Element)
                var afterOpenParen = context.Advance(1);  // "("

                var elemResult = FormatTypeAnnotation(singleElement, afterOpenParen);
                var currentCtx = elemResult.Context;

                // Check for trailing comment between the element and closing paren
                var trailingComment = commentQueries.GetOnRowBetweenColumns(
                    originalRange.End.Row,
                    singleElement.Range.End.Column,
                    originalRange.End.Column);

                foreach (var comment in trailingComment)
                {
                    currentCtx = currentCtx.Advance(1); // space before comment
                    currentCtx = currentCtx.FormatAndAddComment(comment);
                }

                var afterCloseParen = currentCtx.Advance(1); // ")"

                var separatedElems = new SeparatedSyntaxList<Node<TypeAnnotation>>.NonEmpty(
                    elemResult.FormattedNode,
                    []);

                var formattedTupledAnnot = new TypeAnnotation.Tupled(
                    separatedElems
                );
                return FormattingResult<TypeAnnotation>.Create(formattedTupledAnnot, afterCloseParen);
            }
        }

        /// <summary>
        /// Formats a record field, handling both single-line and multiline cases.
        /// For single-line fields: "fieldName : Type"
        /// For multiline fields: handles comments between field name and colon, comments between colon and type,
        /// and trailing comments on the field type.
        /// </summary>
        private FormattingResult<Node<RecordField>> FormatRecordField(
            Node<RecordField> field,
            FormattingContext context,
            FormattingContext fieldIndentRef)
        {
            var fieldStartLoc = context.CurrentLocation();
            var afterFieldName = context.Advance(field.Value.FieldName.Value.Length);

            // Check if colon is on a different line than field name
            var isColonOnNewLine = field.Value.ColonLocation.Row > field.Value.FieldName.Range.End.Row;

            // Check for comments between field name and colon
            var commentsBeforeColon = commentQueries.GetBetweenRowsInclusiveEndWithColumnFilter(
                field.Value.FieldName.Range.End.Row,
                field.Value.ColonLocation.Row,
                field.Value.ColonLocation.Column);

            // Check if field type is on a new line
            var isFieldTypeOnNewLine = field.Value.FieldType.Range.Start.Row > field.Value.ColonLocation.Row;

            // Check for comments between colon and type
            var commentsAfterColon = commentQueries.GetBetweenRows(
                field.Value.ColonLocation.Row,
                field.Value.FieldType.Range.Start.Row);

            // Check for trailing comment on the field type
            var fieldTypeTrailingComment = commentQueries.GetTrailing(field.Value.FieldType.Range);

            // Determine if this field needs multiline handling based on:
            // - Layout: colon or type on a different line than the field name
            // - Comments: any comments between field name/colon/type or trailing the type
            var needsMultilineHandling =
                isColonOnNewLine ||
                isFieldTypeOnNewLine ||
                commentsBeforeColon.Count > 0 ||
                commentsAfterColon.Count > 0 ||
                fieldTypeTrailingComment is not null;

            if (!needsMultilineHandling)
            {
                // Simple single-line format: "fieldName : Type"
                var simpleColonLoc = afterFieldName.Advance(1).CurrentLocation(); // space before colon
                var simpleAfterColon = afterFieldName.Advance(3); // " : "

                var simpleFieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, simpleAfterColon);

                var simpleFormattedFieldNameNode = MakeNode(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

                var simpleFormattedField = new RecordField(
                    simpleFormattedFieldNameNode,
                    simpleColonLoc,
                    simpleFieldTypeResult.FormattedNode
                );

                return FormattingResult<Node<RecordField>>.Create(
                    MakeNode(fieldStartLoc, simpleFieldTypeResult.Context.CurrentLocation(), simpleFormattedField),
                    simpleFieldTypeResult.Context);
            }

            // Multiline handling with comments
            // When the colon or type needs to be on a new line, or when there are comments,
            // the type is indented to the next indent level (columns 5, 9, 13, ...).
            // This is consistent with Elm's standard formatting style.
            var fieldTypeIndentRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            FormattingContext colonContext;
            Location colonLoc;
            FormattingContext afterColon;

            if (isColonOnNewLine || commentsBeforeColon.Count > 0)
            {
                // Handle comments between field name and colon
                var ctx = afterFieldName;
                foreach (var comment in commentsBeforeColon)
                {
                    ctx = ctx.ReturnToIndent(fieldIndentRef).NextRowToIndent();
                    ctx = ctx.FormatAndAddComment(comment);
                }
                // Colon on new line, indented to next indent level
                colonContext = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent();
                colonLoc = colonContext.CurrentLocation();
                afterColon = colonContext.Advance(1); // just ":"
            }
            else
            {
                // Colon on same line as field name
                colonContext = afterFieldName.Advance(1); // space before colon
                colonLoc = colonContext.CurrentLocation();
                afterColon = colonContext.Advance(2); // ": "
            }

            // Check for comments on the same line as the type, BEFORE the type
            var commentsOnSameLineAsType = commentQueries.GetOnRowBeforeColumnAfterRow(
                field.Value.FieldType.Range.Start.Row,
                field.Value.FieldType.Range.Start.Column,
                field.Value.ColonLocation.Row);

            FormattingResult<Node<TypeAnnotation>> fieldTypeResult;

            if (isFieldTypeOnNewLine || commentsAfterColon.Count > 0)
            {
                var ctx = afterColon;

                foreach (var comment in commentsAfterColon)
                {
                    ctx = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent();
                    ctx = ctx.FormatAndAddComment(comment);
                }

                // Field type on new line with extra indentation
                var fieldTypeContext = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent();

                // Handle inline comments that appear on the same line as the type
                foreach (var comment in commentsOnSameLineAsType)
                {
                    fieldTypeContext = fieldTypeContext.FormatAndAddComment(comment).Advance(1);
                }

                fieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, fieldTypeContext);
            }
            else
            {
                fieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, afterColon);
            }

            // Handle trailing comment on the field type
            if (fieldTypeTrailingComment is not null)
            {
                fieldTypeResult = FormattingResult<Node<TypeAnnotation>>.Create(
                    fieldTypeResult.FormattedNode,
                    fieldTypeResult.Context.Advance(1).FormatAndAddComment(fieldTypeTrailingComment));
            }

            var formattedFieldNameNode = MakeNode(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

            var formattedFieldMultiline = new RecordField(
                formattedFieldNameNode,
                colonLoc,
                fieldTypeResult.FormattedNode
            );

            return FormattingResult<Node<RecordField>>.Create(
                MakeNode(fieldStartLoc, fieldTypeResult.Context.CurrentLocation(), formattedFieldMultiline),
                fieldTypeResult.Context);
        }

        #endregion

        #region Expression Formatting

        private FormattingResult<Node<ExpressionSyntax>> FormatExpression(
            Node<ExpressionSyntax> expr,
            FormattingContext context)
        {
            var startLoc = context.CurrentLocation();
            var result = FormatExpressionValue(expr.Value, expr.Range, context);
            return FormattingResult<Node<ExpressionSyntax>>.Create(
                MakeNode(startLoc, result.Context.CurrentLocation(), result.FormattedNode),
                result.Context);
        }

        private FormattingResult<ExpressionSyntax> FormatExpressionValue(
            ExpressionSyntax expr,
            Range originalRange,
            FormattingContext context)
        {
            switch (expr)
            {
                case ExpressionSyntax.UnitExpr:
                    return FormattingResult<ExpressionSyntax>.Create(expr, context.Advance(2)); // "()"

                case ExpressionSyntax.Literal literal:
                    if (literal.IsTripleQuoted)
                    {
                        // For triple-quoted strings, we need to track row changes from embedded newlines
                        // and account for escaped characters that will be longer in the rendered output
                        var afterOpenQuotes = context.Advance(3); // """
                        var literalCtx = afterOpenQuotes;

                        // Use the unified method to process the string content
                        Rendering.ProcessTripleQuotedStringContent(
                            literal.Value,
                            onChar: _ =>
                            {
                                literalCtx = literalCtx.Advance(1);
                            },
                            onEscapeSequence: escaped =>
                            {
                                literalCtx = literalCtx.Advance(escaped.Length);
                            },
                            onNewline: () =>
                            {
                                literalCtx = literalCtx.ResetIndent().NextRowToIndent();
                            });

                        var afterCloseQuotes = literalCtx.Advance(3); // """
                        return FormattingResult<ExpressionSyntax>.Create(literal, afterCloseQuotes);
                    }
                    else
                    {
                        // Use the rendered representation to calculate the correct length
                        // since the value may contain escaped characters
                        var renderedLiteral = Rendering.RenderStringLiteral(literal.Value);
                        return FormattingResult<ExpressionSyntax>.Create(literal, context.Advance(renderedLiteral.Length));
                    }

                case ExpressionSyntax.CharLiteral charLit:
                    // Use the actual rendered length which varies for escaped characters
                    return FormattingResult<ExpressionSyntax>.Create(charLit, context.Advance(Rendering.RenderCharLiteral(charLit.Value).Length));

                case ExpressionSyntax.Integer intLit:
                    return FormattingResult<ExpressionSyntax>.Create(intLit, context.Advance(intLit.LiteralText.Length));

                case ExpressionSyntax.Floatable floatLit:
                    return FormattingResult<ExpressionSyntax>.Create(floatLit, context.Advance(floatLit.LiteralText.Length));

                case ExpressionSyntax.FunctionOrValue funcOrVal:
                    var funcName = funcOrVal.ModuleName.Count > 0
                        ? string.Join(".", funcOrVal.ModuleName) + "." + funcOrVal.Name
                        : funcOrVal.Name;
                    return FormattingResult<ExpressionSyntax>.Create(funcOrVal, context.Advance(funcName.Length));

                case ExpressionSyntax.Negation negation:
                    {
                        var afterNegSign = context.Advance(1); // "-"
                        var negResult = FormatExpression(negation.Expression, afterNegSign);
                        return FormattingResult<ExpressionSyntax>.Create(
                            new ExpressionSyntax.Negation(negResult.FormattedNode),
                            negResult.Context);
                    }

                case ExpressionSyntax.Application app:
                    {
                        // Check if application spans multiple lines based on the containing node's range only
                        var isMultiline = SpansMultipleRows(originalRange);

                        var formattedArgs = new List<Node<ExpressionSyntax>>();
                        var appCtx = context;

                        if (isMultiline)
                        {
                            // Multiline: each argument on its own line, all at the same column
                            // Create reference context at the target column for arguments
                            var multilineTargetRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

                            // Get the row of the function (first element) to check if first arg is on same line
                            var functionRow = app.Arguments.Count > 0 ? app.Arguments[0].Range.Start.Row : -1;

                            for (var i = 0; i < app.Arguments.Count; i++)
                            {
                                var arg = app.Arguments[i];

                                // Determine if this argument should go on a new line
                                // Rule: The first applied argument (index 1) can stay on the same line as
                                // the function if it was originally there. All other args go on new lines.
                                bool putOnNewLine;
                                if (i is 0)
                                {
                                    // Function itself: never put on new line - it's always at the start of the Application
                                    putOnNewLine = false;
                                }
                                else if (i is 1)
                                {
                                    // First applied argument: keep on same line only if:
                                    // 1. It was originally on the same line as the function, AND
                                    // 2. It doesn't span multiple lines (is not multiline itself)
                                    var argStartsOnFunctionLine = arg.Range.Start.Row == functionRow;
                                    var argIsMultiline = SpansMultipleRows(arg.Range);
                                    putOnNewLine = !argStartsOnFunctionLine || argIsMultiline;
                                }
                                else
                                {
                                    // All other arguments: always on new lines
                                    putOnNewLine = true;
                                }

                                if (putOnNewLine)
                                {
                                    // Check for comments between previous argument and this one
                                    var hadComments = false;
                                    if (i > 0)
                                    {
                                        var prevArg = app.Arguments[i - 1];
                                        var commentsBetween = commentQueries.GetBetweenRows(
                                            prevArg.Range.End.Row,
                                            arg.Range.Start.Row);

                                        for (var ci = 0; ci < commentsBetween.Count; ci++)
                                        {
                                            var comment = commentsBetween[ci];
                                            hadComments = true;

                                            // Only move to new line before the FIRST comment
                                            // For subsequent comments, FormatAndAddComment already positioned us
                                            if (ci is 0)
                                            {
                                                appCtx = appCtx.ReturnToIndent(multilineTargetRef).NextRowToIndent();
                                            }
                                            else
                                            {
                                                // Just ensure we're at the right column
                                                appCtx = appCtx.ReturnToIndent(multilineTargetRef).SetIndentColumn();
                                            }

                                            // Format the comment
                                            appCtx = appCtx.FormatAndAddCommentAndNextRowToIndent(comment);
                                        }
                                    }

                                    // Move to next line at the consistent target column, but only if we didn't
                                    // just process comments that already end on a new line
                                    if (!hadComments || appCtx.CurrentColumn != multilineTargetRef.CurrentColumn)
                                    {
                                        appCtx = appCtx.ReturnToIndent(multilineTargetRef).NextRowToIndent();
                                    }
                                    else
                                    {
                                        // Comments left us on the right row, just need to set column
                                        appCtx = appCtx.ReturnToIndent(multilineTargetRef).SetIndentColumn();
                                    }
                                }
                                else if (i > 0)
                                {
                                    // Same line: just add a space (for first applied arg staying on function line)
                                    appCtx = appCtx.Advance(1);
                                }

                                var argResult = FormatExpression(arg, appCtx);
                                formattedArgs.Add(argResult.FormattedNode);
                                appCtx = argResult.Context.ReturnToIndent(context);
                            }
                        }
                        else
                        {
                            // Single line: arguments separated by spaces
                            for (var i = 0; i < app.Arguments.Count; i++)
                            {
                                if (i > 0) appCtx = appCtx.Advance(1); // space
                                var argResult = FormatExpression(app.Arguments[i], appCtx);
                                formattedArgs.Add(argResult.FormattedNode);
                                appCtx = argResult.Context;
                            }
                        }

                        return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.Application(formattedArgs), appCtx);
                    }

                case ExpressionSyntax.RecordExpr recordExpr:
                    {
                        // Helper to get the range of a record field (from field name to value end)
                        static Range GetRecordFieldRange(RecordExprField field) =>
                            MakeRange(field.FieldName.Range.Start, field.ValueExpr.Range.End);

                        // Create reference contexts at opening brace column for alignment
                        var recordAlignRef = context.SetIndentToCurrentColumn();
                        // Create reference for content indented past brace
                        var recordContentRef = context.Advance(2).SetIndentToCurrentColumn();

                        // Helper to format a record field (handles both single-line and multiline values)
                        FormattingResult<RecordExprField> FormatRecordField(
                            RecordExprField field, FormattingContext ctx)
                        {
                            var fieldStartLoc = ctx.CurrentLocation();
                            var afterFieldName = ctx.Advance(field.FieldName.Value.Length);

                            // Check if field value is on a new line (multiline value) in the original source
                            var fieldValueOnNewLine = field.ValueExpr.Range.Start.Row > field.FieldName.Range.Start.Row;

                            FormattingResult<Node<ExpressionSyntax>> valueResult;
                            Location equalsLoc;
                            if (fieldValueOnNewLine)
                            {
                                // Value on new line with indentation to next multiple of 4 from field start
                                equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " ="
                                var afterEq = afterFieldName.Advance(2); // " ="
                                // Create reference for value indentation based on field start
                                var fieldIndentRef = ctx.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var valueContext = afterEq.ReturnToIndent(fieldIndentRef).NextRowToIndent();

                                // Check for comments between = and value expression
                                var eqRow = field.FieldName.Range.End.Row;
                                var valStartRow = field.ValueExpr.Range.Start.Row;
                                var commentsBeforeVal = commentQueries.GetBetweenRows(eqRow, valStartRow);
                                foreach (var comment in commentsBeforeVal)
                                {
                                    valueContext = valueContext.FormatAndAddCommentAndNextRowToIndent(comment);
                                    // FormatAndAddComment already positions us on the correct row
                                    // Just need to set the column
                                    valueContext = valueContext.ReturnToIndent(fieldIndentRef).SetIndentColumn();
                                }

                                valueResult = FormatExpression(field.ValueExpr, valueContext);
                            }
                            else
                            {
                                equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                                var afterEq = afterFieldName.Advance(3); // " = "
                                valueResult = FormatExpression(field.ValueExpr, afterEq);
                            }

                            var fieldNameNode = MakeNode(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);
                            var formattedField = new RecordExprField(fieldNameNode, equalsLoc, valueResult.FormattedNode);

                            return FormattingResult<RecordExprField>.Create(formattedField, valueResult.Context);
                        }

                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "{ ",
                            Separator: ", ",
                            CloseBracket: " }");

                        var multilineConfig = new SeparatedListConfig(
                            AlignmentRef: recordAlignRef,
                            ContentIndentRef: recordContentRef,
                            Separator: Keywords.Comma + " ",  // ", "
                            CloseBracket: "}");

                        var (formattedList, finalContext) = FormatSeparatedListGeneral(
                            separatedList: recordExpr.Fields,
                            formatItem: FormatRecordField,
                            getItemRange: GetRecordFieldRange,
                            commentQueries: commentQueries,
                            singleLineConfig: singleLineConfig,
                            multilineConfig: multilineConfig,
                            context: context,
                            containerRange: originalRange,
                            commentStyle: SeparatedListCommentStyle.ListStyle);  // Records use list-style for blank line preservation

                        return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.RecordExpr(
                            formattedList
                        ), finalContext);
                    }

                case ExpressionSyntax.ListExpr listExpr:
                    {
                        // Create reference context at opening bracket column for alignment
                        var listAlignRef = context.SetIndentToCurrentColumn();
                        // Create reference for content indented past bracket
                        var listContentRef = context.Advance(2).SetIndentToCurrentColumn();

                        // Use the unified generic separated list formatter
                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "[ ",
                            Separator: ", ",
                            CloseBracket: " ]");

                        var multilineConfig = new SeparatedListConfig(
                            AlignmentRef: listAlignRef,
                            ContentIndentRef: listContentRef,
                            Separator: Keywords.Comma + " ",  // ", "
                            CloseBracket: "]");

                        var (formattedList, finalContext) = FormatSeparatedList(
                            separatedList: listExpr.Elements,
                            formatItem: FormatExpression,
                            commentQueries: commentQueries,
                            singleLineConfig: singleLineConfig,
                            multilineConfig: multilineConfig,
                            context: context,
                            containerRange: originalRange,
                            commentStyle: SeparatedListCommentStyle.ListStyle);

                        return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.ListExpr(
                            formattedList
                        ), finalContext);
                    }

                case ExpressionSyntax.ParenthesizedExpression parenExpr:
                    {
                        // Trim duplicate parentheses: ((expr)) -> (expr)
                        // Following elm-format behavior, when there are multiple levels of nested
                        // parentheses around an expression, we reduce them to a single level.
                        // This is safe because the AST is an immutable tree parsed from source code,
                        // so circular references are not possible and depth is bounded by source nesting.
                        var innerExpr = parenExpr.Expression;
                        while (innerExpr.Value is ExpressionSyntax.ParenthesizedExpression nestedParen)
                        {
                            innerExpr = nestedParen.Expression;
                        }

                        // Also remove parentheses around simple expressions that don't need them:
                        // literals, names, unit, etc.
                        if (IsSimpleExpressionThatDoesNotNeedParens(innerExpr.Value))
                        {
                            // Skip the parentheses entirely and just format the inner expression
                            return FormatExpressionValue(innerExpr.Value, innerExpr.Range, context);
                        }

                        // Create reference context at opening paren for close paren alignment
                        var openParenRef = context.SetIndentToCurrentColumn();

                        var afterOpenParen = context.Advance(1);

                        // Check for comments between opening paren and inner expression
                        var openParenRow = originalRange.Start.Row;
                        var innerStartRow = innerExpr.Range.Start.Row;
                        var openParenEndCol = originalRange.Start.Column; // Column of '(' itself

                        FormattingContext contextBeforeInner;

                        if (innerStartRow > openParenRow)
                        {
                            // Inner expression is on a different line - check for line comments on the open paren line
                            // Use afterColumn = openParenEndCol since we want comments AFTER the '(' (column > openParenEndCol)
                            var commentsAfterOpenParen = commentQueries.GetOnRowAfterColumn(openParenRow, openParenEndCol);

                            if (commentsAfterOpenParen.Count > 0)
                            {
                                // Format the comment on the same line as the opening paren
                                contextBeforeInner = afterOpenParen;
                                foreach (var comment in commentsAfterOpenParen)
                                {
                                    contextBeforeInner = contextBeforeInner.FormatAndAddComment(comment);
                                }
                                // Move to next row at inner expression indent
                                var innerIndentRef = openParenRef.Advance(1).SetIndentToCurrentColumn(); // align with content after "("
                                contextBeforeInner = contextBeforeInner.ReturnToIndent(innerIndentRef).NextRowToIndent();
                            }
                            else
                            {
                                contextBeforeInner = afterOpenParen;
                            }
                        }
                        else
                        {
                            contextBeforeInner = afterOpenParen;
                        }

                        var innerResult = FormatExpression(innerExpr, contextBeforeInner);

                        // Determine if closing paren should be on new line.
                        // The only condition for making ParenthesizedExpression multiline is the content expression being multiline.
                        var formattedContentIsMultiline = innerResult.Context.CurrentRow > afterOpenParen.CurrentRow;
                        var closeParenOnNewLine = formattedContentIsMultiline;

                        // Check for trailing comment on the inner expression before the closing paren
                        // Look for comments between inner expression end and closing paren in the original source
                        var innerEndRow = innerExpr.Range.End.Row;
                        var innerEndCol = innerExpr.Range.End.Column;
                        var closeParenCol = originalRange.End.Column; // original closing paren position

                        var trailingCommentsBeforeCloseParen =
                            (innerEndRow == originalRange.End.Row)
                            ? commentQueries.GetOnRowBetweenColumns(innerEndRow, innerEndCol, closeParenCol)
                            : [];

                        var contextAfterInner = innerResult.Context;
                        foreach (var trailingComment in trailingCommentsBeforeCloseParen)
                        {
                            contextAfterInner = contextAfterInner.Advance(1); // space before comment
                            contextAfterInner = contextAfterInner.FormatAndAddComment(trailingComment);
                        }

                        FormattingContext afterCloseParen;

                        if (closeParenOnNewLine)
                        {
                            // Closing paren on new line, aligned with opening paren
                            var closeCtx = contextAfterInner.ReturnToIndent(openParenRef).NextRowToIndent();
                            afterCloseParen = closeCtx.Advance(1); // ")"
                        }
                        else
                        {
                            // Closing paren on same line as end of inner expression
                            afterCloseParen = contextAfterInner.Advance(1); // ")"
                        }

                        return FormattingResult<ExpressionSyntax>.Create(
                            new ExpressionSyntax.ParenthesizedExpression(innerResult.FormattedNode),
                            afterCloseParen);
                    }

                case ExpressionSyntax.OperatorApplication opApp:
                    {
                        var leftResult = FormatExpression(opApp.Left, context);

                        // Check if the right operand is on a new line in the original
                        var rightOnNewLine = opApp.Right.Range.Start.Row > opApp.Left.Range.End.Row;

                        // Check if right operand spans multiple rows (is multiline)
                        var rightIsMultiline = SpansMultipleRows(opApp.Right.Range);

                        // For <| operator, also treat as multiline if right operand spans multiple rows
                        // (even if it starts on the same line as <|)
                        var treatAsMultiline = rightOnNewLine ||
                            (opApp.Operator.Value is "<|" && rightIsMultiline);

                        if (treatAsMultiline)
                        {
                            // Special case for left pipe operator <|
                            // Unlike other operators, <| stays at the end of the preceding line
                            if (opApp.Operator.Value is "<|")
                            {
                                // " <|" at the end of the left operand
                                var pipeAfterSpace = leftResult.Context.Advance(1); // " "
                                var operatorStart = pipeAfterSpace.CurrentLocation();
                                var pipeAfterOp = pipeAfterSpace.Advance(opApp.Operator.Value.Length);
                                var operatorEnd = pipeAfterOp.CurrentLocation();

                                // Create operator node with its new position
                                var formattedOperator = MakeNode(operatorStart, operatorEnd, opApp.Operator.Value);

                                // Right operand on new line with extra indentation
                                // Create reference at next indent level based on current column
                                // (not based on IndentSpaces, which may not reflect the actual position)
                                var rightIndentRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var pipeRightContext = pipeAfterOp.ReturnToIndent(rightIndentRef).NextRowToIndent();

                                // Look for comments between operator and right operand
                                var pipeCommentsBeforeRight = commentQueries.GetBetweenRows(
                                    opApp.Operator.Range.End.Row, opApp.Right.Range.Start.Row);

                                // Format comments between operator and right operand
                                foreach (var comment in pipeCommentsBeforeRight)
                                {
                                    pipeRightContext = pipeRightContext.FormatAndAddCommentAndNextRowToIndent(comment);
                                }

                                var pipeRightResult = FormatExpression(opApp.Right, pipeRightContext);

                                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.OperatorApplication(
                                    formattedOperator,
                                    opApp.Direction,
                                    leftResult.FormattedNode,
                                    pipeRightResult.FormattedNode
                                ), pipeRightResult.Context.ReturnToIndent(context));
                            }

                            // Multiline: operator and right operand on new line
                            // For nested chained operators, check if we've been passed an explicit
                            // operator alignment column via IndentSpaces.
                            var inheritedColumn = context.IndentSpaces + 1;

                            // Calculate where the operator is in the original source
                            // The operator is at: right operand start column - operator length - 1 (space)
                            var originalOpColumn = opApp.Right.Range.Start.Column - opApp.Operator.Value.Length - 1;
                            var leftStartColumn = opApp.Left.Range.Start.Column;

                            // Create reference context for operator alignment
                            FormattingContext targetRef;
                            // We're in a chained operator alignment if:
                            // 1. The original operator column is LESS than the left operand column
                            //    (meaning operators are aligned to the left of operands)
                            // 2. IndentSpaces is at a 4-space boundary (was set by parent operator)
                            // 3. The inherited column makes sense (<= current column)
                            if (originalOpColumn < leftStartColumn &&
                                (context.IndentSpaces % Indentation.Full) is 0 &&
                                context.IndentSpaces >= Indentation.Full &&
                                inheritedColumn < context.CurrentColumn)
                            {
                                // We're in a chained operator - use the parent's alignment (current indent)
                                targetRef = context.SetIndentColumn().SetIndentToCurrentColumn();
                            }
                            else
                            {
                                // Calculate fresh based on our position
                                targetRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                            }

                            // Look for comments between the left operand and the operator
                            var commentsBeforeOperator = commentQueries.GetBetweenRows(
                                opApp.Left.Range.End.Row, opApp.Operator.Range.Start.Row);

                            var newLineCtx = leftResult.Context.ReturnToIndent(targetRef).NextRowToIndent();

                            // Format comments between left operand and operator
                            foreach (var comment in commentsBeforeOperator)
                            {
                                newLineCtx = newLineCtx.FormatAndAddCommentAndNextRowToIndent(comment);
                                // Align next comment or operator at target column
                                newLineCtx = newLineCtx.ReturnToIndent(targetRef).SetIndentColumn();
                            }

                            var opStart = newLineCtx.CurrentLocation();
                            var afterOp = newLineCtx.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();

                            // Create operator node with its new position
                            var formattedOp = MakeNode(opStart, opEnd, opApp.Operator.Value);

                            // Check for comments between operator and right operand on the SAME line
                            // (trailing comment after operator but before the right operand)
                            var opEndRow = opApp.Operator.Range.End.Row;
                            var rightStartRow = opApp.Right.Range.Start.Row;
                            var trailingOpComments = opEndRow == rightStartRow
                                ? commentQueries.GetOnRowBetweenColumns(
                                    opEndRow,
                                    opApp.Operator.Range.End.Column,
                                    opApp.Right.Range.Start.Column)
                                : commentQueries.GetOnRowAfterColumn(opEndRow, opApp.Operator.Range.End.Column);

                            FormattingContext rightContext;

                            if (trailingOpComments.Count > 0)
                            {
                                // Comment immediately after operator: "&&" followed by " -- comment" then newline
                                var afterOpSpace = afterOp.Advance(1); // " "
                                // Format comment - this positions at indent column on new row
                                var afterComment = afterOpSpace.FormatAndAddCommentAndNextRowToIndent(trailingOpComments[0]);
                                // Right operand position: from indent, advance by operator width + 1 space
                                rightContext = afterComment.Advance(opApp.Operator.Value.Length + 1);
                            }
                            else
                            {
                                // No trailing comment - space after operator
                                // IndentSpaces is already set to targetColumn - 1 from SetIndentToCurrentColumn
                                rightContext = afterOp.Advance(1); // " "
                            }

                            // Look for comments between operator and right operand (on separate lines)
                            var commentsBeforeRight = commentQueries.GetBetweenRows(
                                opApp.Operator.Range.End.Row, opApp.Right.Range.Start.Row);

                            // Format comments between operator and right operand
                            foreach (var comment in commentsBeforeRight)
                            {
                                rightContext = rightContext.FormatAndAddCommentAndNextRowToIndent(comment);
                            }

                            var rightResult = FormatExpression(opApp.Right, rightContext);

                            return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context.ReturnToIndent(context));
                        }
                        else
                        {
                            // Single line - check for inline comments between left operand and operator
                            // Use GetOnRowBetweenColumns to only get comments between left operand end and operator start
                            var leftEndRow = opApp.Left.Range.End.Row;
                            var leftEndCol = opApp.Left.Range.End.Column;
                            var opStartCol = opApp.Operator.Range.Start.Column;
                            var leftTrailingComments = commentQueries.GetOnRowBetweenColumns(leftEndRow, leftEndCol, opStartCol);
                            var afterLeftContext = leftResult.Context;

                            foreach (var leftTrailingComment in leftTrailingComments)
                            {
                                // Format inline comment after left operand: "0x30 {- 0 -} <= code"
                                afterLeftContext = afterLeftContext.Advance(1); // space before comment
                                afterLeftContext = afterLeftContext.FormatAndAddComment(leftTrailingComment);
                            }

                            var afterLeftSpace = afterLeftContext.Advance(1); // " "
                            var opStart = afterLeftSpace.CurrentLocation();
                            var afterOp = afterLeftSpace.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();
                            var afterOpSpace = afterOp.Advance(1); // " "

                            // Check for comments between operator and right operand (on the same line)
                            var opEndCol = opApp.Operator.Range.End.Column;
                            var rightStartCol = opApp.Right.Range.Start.Column;
                            var opRow = opApp.Operator.Range.End.Row;
                            var opToRightComments = commentQueries.GetOnRowBetweenColumns(opRow, opEndCol, rightStartCol);

                            var contextBeforeRight = afterOpSpace;
                            foreach (var opToRightComment in opToRightComments)
                            {
                                // Format inline comment after operator: ">= {- 0 -} 0x30"
                                contextBeforeRight = contextBeforeRight.FormatAndAddComment(opToRightComment);
                                contextBeforeRight = contextBeforeRight.Advance(1); // space after comment
                            }

                            var rightResult = FormatExpression(opApp.Right, contextBeforeRight);

                            // Check for trailing comment on the right operand
                            // Only look for comments between right operand end and the end of this expression
                            // to avoid picking up comments from other parts of the line
                            var rightEndRow = opApp.Right.Range.End.Row;
                            var rightEndCol = opApp.Right.Range.End.Column;
                            var exprEndCol = originalRange.End.Column;
                            var rightTrailingComments = (rightEndRow == originalRange.End.Row)
                                ? commentQueries.GetOnRowBetweenColumns(rightEndRow, rightEndCol, exprEndCol)
                                : [];
                            var finalContext = rightResult.Context;
                            foreach (var rightTrailingComment in rightTrailingComments)
                            {
                                finalContext = finalContext.Advance(1); // space before comment
                                finalContext = finalContext.FormatAndAddComment(rightTrailingComment);
                            }

                            // Create operator node with its new position
                            var formattedOp = MakeNode(opStart, opEnd, opApp.Operator.Value);

                            return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), finalContext);
                        }
                    }

                case ExpressionSyntax.PrefixOperator prefixOp:
                    return FormattingResult<ExpressionSyntax>.Create(prefixOp, context.Advance(prefixOp.Operator.Length + 2)); // "(op)"

                case ExpressionSyntax.RecordAccess recordAccess:
                    {
                        var recordResult = FormatExpression(recordAccess.Record, context);
                        var afterDot = recordResult.Context.Advance(1); // "."
                        var fieldNameLoc = afterDot.CurrentLocation();
                        var afterFieldAccess = afterDot.Advance(recordAccess.FieldName.Value.Length);
                        var fieldNameWithLoc = MakeNode(fieldNameLoc, afterFieldAccess.CurrentLocation(), recordAccess.FieldName.Value);

                        return FormattingResult<ExpressionSyntax>.Create(
                            new ExpressionSyntax.RecordAccess(recordResult.FormattedNode, fieldNameWithLoc),
                            afterFieldAccess);
                    }

                case ExpressionSyntax.RecordAccessFunction accessFunc:
                    return FormattingResult<ExpressionSyntax>.Create(accessFunc, context.Advance(accessFunc.FunctionName.Length));

                case ExpressionSyntax.IfBlock ifBlock:
                    return FormatIfBlock(ifBlock, context);

                case ExpressionSyntax.CaseExpression caseExpr:
                    return FormatCaseExpression(caseExpr, context);

                case ExpressionSyntax.LetExpression letExpr:
                    return FormatLetExpression(letExpr, context);

                case ExpressionSyntax.LambdaExpression lambdaExpr:
                    return FormatLambdaExpression(lambdaExpr, context);

                case ExpressionSyntax.TupledExpression tupledExpr:
                    return FormatTupledExpression(tupledExpr, originalRange, context);

                case ExpressionSyntax.RecordUpdateExpression recordUpdate:
                    return FormatRecordUpdateExpression(recordUpdate, originalRange, context);

                case ExpressionSyntax.GLSLExpression glslExpr:
                    return FormatGLSLExpression(glslExpr, originalRange, context);

                default:
                    throw new System.NotImplementedException(
                        $"Expression formatting not implemented for: {expr.GetType().Name} " +
                        $"at row {originalRange.Start.Row}, column {originalRange.Start.Column}");
            }
        }

        private FormattingResult<ExpressionSyntax> FormatIfBlock(
            ExpressionSyntax.IfBlock ifBlock,
            FormattingContext context,
            int? chainBaseColumn = null)
        {
            // Check if this is a chained else-if (else block is another if)
            // But only if there are no comments between else and if tokens
            var isElseIf = false;

            if (ifBlock.ElseBlock.Value is ExpressionSyntax.IfBlock innerIf)
            {
                // Check if there are any comments between else token and inner if token
                var commentsBetweenElseAndIf = commentQueries.HasBetweenRows(
                    ifBlock.ElseTokenLocation.Row, innerIf.IfTokenLocation.Row);

                // Also check for comments on the same row BETWEEN else and if
                // (not just any comments after else - we need to check only up to where "if" starts)
                var elseEndCol = ifBlock.ElseTokenLocation.Column + 4; // "else" is 4 chars
                var ifStartCol = innerIf.IfTokenLocation.Column;

                var commentsAfterElseBeforeIf =
                    (ifBlock.ElseTokenLocation.Row == innerIf.IfTokenLocation.Row) &&
                    commentQueries.GetOnRowBetweenColumns(ifBlock.ElseTokenLocation.Row, elseEndCol, ifStartCol).Count > 0;

                // Only treat as else-if if no comments between else and if
                isElseIf = !commentsBetweenElseAndIf && !commentsAfterElseBeforeIf;
            }

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            // Check if condition is multiline based on original layout
            var conditionIsMultiline =
                SpansMultipleRows(ifBlock.Condition.Range) ||
                ifBlock.ThenTokenLocation.Row > ifBlock.Condition.Range.Start.Row;

            // "if" or "if "
            var ifTokenLoc = context.CurrentLocation();

            // Use the chain base column if provided (for else-if chains), otherwise use current column
            var effectiveBaseColumn = chainBaseColumn ?? ifTokenLoc.Column;

            // Create reference context at effective base column for else alignment
            var effectiveBaseRef = chainBaseColumn.HasValue
                ? context  // If chain base provided, context is already at that position
                : context.SetIndentToCurrentColumn();

            // Create reference context for body indentation (next multiple of 4 from base)
            var bodyRef = effectiveBaseRef.CreateIndentedRef();

            FormattingResult<Node<ExpressionSyntax>> conditionResult;
            Location thenTokenLoc;
            FormattingContext afterThen;

            if (conditionIsMultiline)
            {
                // Multiline condition: "if" on its own, condition indented on next line
                var afterIf = context.Advance(Keywords.If.Length);
                var conditionContext = afterIf.ReturnToIndent(indentedRef).NextRowToIndent();

                // Check for comments between "if" and the condition
                var commentsBeforeCondition = commentQueries.GetBetweenRows(
                    ifBlock.IfTokenLocation.Row,
                    ifBlock.Condition.Range.Start.Row);

                if (commentsBeforeCondition.Count > 0)
                {
                    var conditionIndentRef = conditionContext.SetIndentToCurrentColumn();
                    foreach (var comment in commentsBeforeCondition)
                    {
                        // Format the comment, add it to the context, and position at condition indent
                        conditionContext = conditionContext.ReturnToIndent(conditionIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                }

                conditionResult = FormatExpression(ifBlock.Condition, conditionContext);

                // Check for comments between condition and "then"
                var commentsAfterCondition =
                    commentQueries.GetBetweenRows(
                        ifBlock.Condition.Range.End.Row,
                        ifBlock.ThenTokenLocation.Row);

                var afterConditionCtx = conditionResult.Context;
                if (commentsAfterCondition.Count > 0)
                {
                    // Comments between condition and "then" - indent them like condition
                    foreach (var comment in commentsAfterCondition)
                    {
                        afterConditionCtx = afterConditionCtx.ReturnToIndent(bodyRef).NextRowToIndent();
                        // Format the comment, add it to the context, and stay at the comment end position
                        afterConditionCtx = afterConditionCtx.FormatAndAddComment(comment);
                    }
                }

                // "then" on its own line at base indent
                var thenLineContext = afterConditionCtx.ReturnToIndent(context).NextRowToIndent();
                thenTokenLoc = thenLineContext.CurrentLocation();
                afterThen = thenLineContext.Advance(Keywords.Then.Length);
            }
            else
            {
                // Single-line condition: "if condition then"
                var afterIf = context.Advance(Keywords.If.Length).AdvanceSpaceSeparator();
                conditionResult = FormatExpression(ifBlock.Condition, afterIf);

                // Check for comments between condition end and "then" on the same line
                var condEndRow = ifBlock.Condition.Range.End.Row;
                var condEndCol = ifBlock.Condition.Range.End.Column;
                var thenCol = ifBlock.ThenTokenLocation.Column;
                var inlineCommentsBeforeThen = (condEndRow == ifBlock.ThenTokenLocation.Row)
                    ? commentQueries.GetOnRowBetweenColumns(condEndRow, condEndCol, thenCol)
                    : [];

                var afterCondCtx = conditionResult.Context;
                foreach (var comment in inlineCommentsBeforeThen)
                {
                    afterCondCtx = afterCondCtx.AdvanceSpaceSeparator();
                    afterCondCtx = afterCondCtx.FormatAndAddComment(comment);
                }

                // " then" (space before then)
                var afterCondSpace = afterCondCtx.AdvanceSpaceSeparator();
                thenTokenLoc = afterCondSpace.CurrentLocation();
                afterThen = afterCondSpace.Advance(Keywords.Then.Length);
            }

            // Then block on new line, indented from the base column
            // Also set IndentSpaces so nested expressions (like case/if) indent correctly
            var thenContextReference = afterThen.ReturnToIndent(bodyRef).NextRowToIndent();
            var thenContext = thenContextReference;

            // Check for comments between "then" and the then-block
            var commentsBeforeThenBlock =
                commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                    ifBlock.ThenTokenLocation.Row,
                    ifBlock.ThenTokenLocation.Column + 4,
                    ifBlock.ThenBlock.Range.Start.Row,
                    requireAfterColumn: true);

            if (commentsBeforeThenBlock.Count > 0)
            {
                var thenBodyIndentRef = thenContext.SetIndentToCurrentColumn();
                foreach (var comment in commentsBeforeThenBlock)
                {
                    // Format the comment, add it to the context, and position at then body indent
                    thenContext = thenContext.ReturnToIndent(thenBodyIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                }
            }

            var thenResult = FormatExpression(ifBlock.ThenBlock, thenContext);

            // Check for comments between then-block and else
            var commentsAfterThenBlock =
                commentQueries.GetBetweenRows(
                    ifBlock.ThenBlock.Range.End.Row,
                    ifBlock.ElseTokenLocation.Row);

            // Format comments after then-block
            var afterThenBlockContext = thenResult.Context;
            FormattingContext elseContext;

            if (commentsAfterThenBlock.Count > 0)
            {
                // Move to next line for comment(s)
                afterThenBlockContext = afterThenBlockContext.ReturnToIndent(bodyRef).NextRowToIndent();

                foreach (var comment in commentsAfterThenBlock)
                {
                    // Format the comment, add it to the context, and position at indent on the next row
                    afterThenBlockContext = afterThenBlockContext.FormatAndAddCommentThenNextRowToIndent(comment);
                }

                // After comments, we're already at the start of a new line
                // Just add one more blank line (one newline) before else
                elseContext = afterThenBlockContext.NextRowToIndent().ReturnToIndent(effectiveBaseRef).SetIndentColumn();
            }
            else
            {
                // No comments - add blank line (2 rows) before else
                elseContext = afterThenBlockContext.WithBlankLine().ReturnToIndent(effectiveBaseRef).SetIndentColumn();
            }

            // "else" or "else if"
            var elseTokenLoc = elseContext.CurrentLocation();

            if (isElseIf)
            {
                // "else if " - chain
                var afterElse =
                    elseContext.Advance(Keywords.Else.Length).AdvanceSpaceSeparator();

                var innerIfBlock =
                    (ExpressionSyntax.IfBlock)ifBlock.ElseBlock.Value;

                // Format inner if starting from "if" position
                var innerIfTokenLoc = afterElse.CurrentLocation();

                // Check if inner condition is multiline
                var innerConditionIsMultiline =
                    SpansMultipleRows(innerIfBlock.Condition.Range) ||
                    innerIfBlock.ThenTokenLocation.Row > innerIfBlock.Condition.Range.Start.Row;

                FormattingResult<Node<ExpressionSyntax>> innerConditionResult;
                Location innerThenLoc;
                FormattingContext afterInnerThen;

                if (innerConditionIsMultiline)
                {
                    // Multiline inner condition: "if" on its own line, condition indented
                    var afterInnerIf = afterElse.Advance(Keywords.If.Length);
                    var innerConditionContext = afterInnerIf.ReturnToIndent(bodyRef).NextRowToIndent();

                    // Check for comments before the inner condition
                    var commentsBeforeInnerCondition = commentQueries.GetBetweenRows(
                        innerIfBlock.IfTokenLocation.Row, innerIfBlock.Condition.Range.Start.Row);

                    if (commentsBeforeInnerCondition.Count > 0)
                    {
                        var innerCondIndentRef = innerConditionContext.SetIndentToCurrentColumn();
                        foreach (var comment in commentsBeforeInnerCondition)
                        {
                            // Format the comment, add it to the context, and position at inner condition indent
                            innerConditionContext = innerConditionContext.ReturnToIndent(innerCondIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                        }
                    }

                    innerConditionResult = FormatExpression(innerIfBlock.Condition, innerConditionContext);

                    // Check for comments between inner condition and inner "then"
                    var commentsAfterInnerCondition = commentQueries.GetBetweenRows(
                        innerIfBlock.Condition.Range.End.Row, innerIfBlock.ThenTokenLocation.Row);

                    var afterInnerConditionCtx = innerConditionResult.Context;
                    if (commentsAfterInnerCondition.Count > 0)
                    {
                        foreach (var comment in commentsAfterInnerCondition)
                        {
                            afterInnerConditionCtx = afterInnerConditionCtx.ReturnToIndent(bodyRef).NextRowToIndent();
                            // Format the comment, add it to the context, and stay at the comment end position
                            afterInnerConditionCtx = afterInnerConditionCtx.FormatAndAddComment(comment);
                        }
                    }

                    // "then" on its own line at base indent
                    var innerThenLineContext = afterInnerConditionCtx.ReturnToIndent(effectiveBaseRef).NextRowToIndent();
                    innerThenLoc = innerThenLineContext.CurrentLocation();
                    afterInnerThen = innerThenLineContext.Advance(Keywords.Then.Length);
                }
                else
                {
                    // Single-line inner condition: "if condition then"
                    var afterInnerIf = afterElse.Advance(Keywords.If.Length).AdvanceSpaceSeparator();
                    innerConditionResult = FormatExpression(innerIfBlock.Condition, afterInnerIf);

                    // Check for comments between inner condition end and inner "then" on the same line
                    var innerCondEndRow = innerIfBlock.Condition.Range.End.Row;
                    var innerCondEndCol = innerIfBlock.Condition.Range.End.Column;
                    var innerThenCol = innerIfBlock.ThenTokenLocation.Column;

                    var inlineCommentsBeforeInnerThen =
                        (innerCondEndRow == innerIfBlock.ThenTokenLocation.Row)
                        ? commentQueries.GetOnRowBetweenColumns(innerCondEndRow, innerCondEndCol, innerThenCol)
                        : [];

                    var afterInnerCondCtx = innerConditionResult.Context;
                    foreach (var comment in inlineCommentsBeforeInnerThen)
                    {
                        afterInnerCondCtx = afterInnerCondCtx.AdvanceSpaceSeparator();
                        afterInnerCondCtx = afterInnerCondCtx.FormatAndAddComment(comment);
                    }

                    // " then"
                    var afterInnerCondSpace = afterInnerCondCtx.AdvanceSpaceSeparator();
                    innerThenLoc = afterInnerCondSpace.CurrentLocation();
                    afterInnerThen = afterInnerCondSpace.Advance(Keywords.Then.Length);
                }

                // Check for comments between inner "then" and inner then-block
                // Also set IndentSpaces so nested expressions indent correctly
                var innerThenContextReference = afterInnerThen.ReturnToIndent(bodyRef).NextRowToIndent();
                var innerThenContext = innerThenContextReference;

                var commentsBeforeInnerThenBlock =
                    commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                    innerIfBlock.ThenTokenLocation.Row, innerIfBlock.ThenTokenLocation.Column + Keywords.Then.Length,
                    innerIfBlock.ThenBlock.Range.Start.Row, requireAfterColumn: true);

                if (commentsBeforeInnerThenBlock.Count > 0)
                {
                    var innerThenBodyIndentRef = innerThenContext.SetIndentToCurrentColumn();

                    foreach (var comment in commentsBeforeInnerThenBlock)
                    {
                        // Format the comment, add it to the context, and position at inner then body indent
                        innerThenContext =
                            innerThenContext
                            .ReturnToIndent(innerThenBodyIndentRef)
                            .FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                }

                // Inner then block - indented from the base column (same as outer if)
                var innerThenResult =
                    FormatExpression(innerIfBlock.ThenBlock, innerThenContext);

                // Check for comments between inner then-block and inner else
                var commentsAfterInnerThenBlock =
                    commentQueries.GetBetweenRows(innerIfBlock.ThenBlock.Range.End.Row, innerIfBlock.ElseTokenLocation.Row);

                // Format comments after inner then-block
                var afterInnerThenBlockContext = innerThenResult.Context;
                FormattingContext innerElseContext;

                if (commentsAfterInnerThenBlock.Count > 0)
                {
                    // Move to next line for comment(s)
                    afterInnerThenBlockContext = afterInnerThenBlockContext.ReturnToIndent(bodyRef).NextRowToIndent();

                    foreach (var comment in commentsAfterInnerThenBlock)
                    {
                        // Format the comment, add it to the context, and position at indent on the next row
                        afterInnerThenBlockContext = afterInnerThenBlockContext.FormatAndAddCommentThenNextRowToIndent(comment);
                    }

                    // After comments, we're already at the start of a new line
                    // Just add one more blank line (one newline) before else
                    innerElseContext = afterInnerThenBlockContext.NextRowToIndent().ReturnToIndent(effectiveBaseRef).SetIndentColumn();
                }
                else
                {
                    // No comments - add blank line (2 rows) before else
                    innerElseContext = afterInnerThenBlockContext.WithBlankLine().ReturnToIndent(effectiveBaseRef).SetIndentColumn();
                }

                var innerElseTokenLoc = innerElseContext.CurrentLocation();

                Node<ExpressionSyntax> formattedInnerElseBlock;
                FormattingContext afterInnerElseBlock;

                if (innerIfBlock.ElseBlock.Value is ExpressionSyntax.IfBlock nestedIf)
                {
                    // Continue the chain - pass the base column
                    var afterInnerElse = innerElseContext.Advance(Keywords.Else.Length).AdvanceSpaceSeparator();
                    var nestedIfResult = FormatIfBlock(nestedIf, afterInnerElse, effectiveBaseColumn);
                    formattedInnerElseBlock = MakeNode(afterInnerElse.CurrentLocation(), nestedIfResult.Context.CurrentLocation(), nestedIfResult.FormattedNode);
                    afterInnerElseBlock = nestedIfResult.Context;
                }
                else
                {
                    // Final else block - body indented from the base column
                    var afterInnerElse = innerElseContext.Advance(Keywords.Else.Length);
                    // Also set IndentSpaces so nested expressions indent correctly
                    var innerElseBodyContextReference = afterInnerElse.ReturnToIndent(bodyRef).NextRowToIndent();
                    var innerElseBodyContext = innerElseBodyContextReference;

                    // Check for comments between inner "else" and inner else-block
                    var commentsBeforeInnerElseBlock = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                        innerIfBlock.ElseTokenLocation.Row, innerIfBlock.ElseTokenLocation.Column + Keywords.Else.Length,
                        innerIfBlock.ElseBlock.Range.Start.Row, requireAfterColumn: true);

                    if (commentsBeforeInnerElseBlock.Count > 0)
                    {
                        var innerElseBodyIndentRef = innerElseBodyContext.SetIndentToCurrentColumn();
                        foreach (var comment in commentsBeforeInnerElseBlock)
                        {
                            // Format the comment, add it to the context, and position at inner else body indent
                            innerElseBodyContext = innerElseBodyContext.ReturnToIndent(innerElseBodyIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                        }
                    }

                    var innerElseResult = FormatExpression(innerIfBlock.ElseBlock, innerElseBodyContext);
                    formattedInnerElseBlock = innerElseResult.FormattedNode;
                    afterInnerElseBlock = innerElseResult.Context;
                }

                var formattedInnerIf = new ExpressionSyntax.IfBlock(
                    innerIfTokenLoc,
                    innerConditionResult.FormattedNode,
                    innerThenLoc,
                    innerThenResult.FormattedNode,
                    innerElseTokenLoc,
                    formattedInnerElseBlock
                );

                // The else block wraps the inner if block
                var elseBlockRange = MakeRange(afterElse.CurrentLocation(), afterInnerElseBlock.CurrentLocation());
                var formattedElseBlock = MakeNode<ExpressionSyntax>(elseBlockRange, formattedInnerIf);

                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.IfBlock(
                    ifTokenLoc,
                    conditionResult.FormattedNode,
                    thenTokenLoc,
                    thenResult.FormattedNode,
                    elseTokenLoc,
                    formattedElseBlock
                ), afterInnerElseBlock);
            }
            else
            {
                // "else"
                var afterElse = elseContext.Advance(Keywords.Else.Length);

                // Check if the else block is an if-block that was separated from else due to comments
                // In that case, the nested if should be at the same column as else, not indented
                var isIfBlockSeparatedByComments = ifBlock.ElseBlock.Value is ExpressionSyntax.IfBlock;

                // Else block on new line
                // If it's a separated if-block, use effectiveBaseRef (same level as else)
                // Otherwise, use bodyRef (indented by 4 from else)
                var elseBlockIndentRef = isIfBlockSeparatedByComments ? effectiveBaseRef : bodyRef;
                var elseBlockContextReference = afterElse.ReturnToIndent(elseBlockIndentRef).NextRowToIndent();
                var elseBlockContext = elseBlockContextReference;

                // Check for comments between "else" and the else-block
                var commentsBeforeElseBlock = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                    ifBlock.ElseTokenLocation.Row, ifBlock.ElseTokenLocation.Column + 4,
                    ifBlock.ElseBlock.Range.Start.Row, requireAfterColumn: true);

                if (commentsBeforeElseBlock.Count > 0)
                {
                    var elseBodyIndentRef = elseBlockContext.SetIndentToCurrentColumn();
                    foreach (var comment in commentsBeforeElseBlock)
                    {
                        // Format the comment, add it to the context, and position at else body indent
                        elseBlockContext = elseBlockContext.ReturnToIndent(elseBodyIndentRef).FormatAndAddCommentAndNextRowToIndent(comment);
                    }
                }

                var elseResult = FormatExpression(ifBlock.ElseBlock, elseBlockContext);

                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.IfBlock(
                    ifTokenLoc,
                    conditionResult.FormattedNode,
                    thenTokenLoc,
                    thenResult.FormattedNode,
                    elseTokenLoc,
                    elseResult.FormattedNode
                ), elseResult.Context.ReturnToIndent(context));
            }
        }

        private FormattingResult<ExpressionSyntax> FormatCaseExpression(
            ExpressionSyntax.CaseExpression caseExpr,
            FormattingContext context)
        {
            // Check if the case expression spans multiple lines
            // This can happen if:
            // 1. The scrutinee expression starts on a new line from the case keyword
            // 2. The "of" keyword is on a different line from the scrutinee
            var scrutineeOnNewLine = caseExpr.CaseBlock.Expression.Range.Start.Row > caseExpr.CaseBlock.CaseTokenLocation.Row;
            var ofOnDifferentLine = caseExpr.CaseBlock.OfTokenLocation.Row > caseExpr.CaseBlock.Expression.Range.End.Row;
            var isMultilineCaseHeader = scrutineeOnNewLine || ofOnDifferentLine;

            // "case" or "case "
            var caseTokenLoc = context.CurrentLocation();

            // Create reference context at case keyword for alignment
            var caseTokenRef = context.SetIndentToCurrentColumn();
            // Create reference for indented content (next multiple of 4)
            var caseIndentRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            FormattingResult<Node<ExpressionSyntax>> exprResult;
            FormattingContext afterExpr;

            if (isMultilineCaseHeader)
            {
                // "case" without trailing space
                var afterCase = context.Advance(Keywords.Case.Length);
                // Expression on new line, indented to next multiple of 4 from the case keyword position
                var exprContext = afterCase.ReturnToIndent(caseIndentRef).NextRowToIndent();
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, exprContext);
                afterExpr = exprResult.Context;
            }
            else
            {
                // "case " with trailing space
                var afterCase = context.Advance(Keywords.Case.Length);
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, afterCase.AdvanceSpaceSeparator());
                afterExpr = exprResult.Context;
            }

            // " of" (space before of) or just "of" if scrutinee was on new line
            Location ofTokenLoc;
            FormattingContext afterOf;
            if (isMultilineCaseHeader)
            {
                // "of" on new line, aligned with "case" keyword
                var ofContext = afterExpr.ReturnToIndent(caseTokenRef).NextRowToIndent();
                ofTokenLoc = ofContext.CurrentLocation();
                afterOf = ofContext.Advance(Keywords.Of.Length);
            }
            else
            {
                // " of" on same line
                var afterSpace = afterExpr.AdvanceSpaceSeparator();
                ofTokenLoc = afterSpace.CurrentLocation();
                afterOf = afterSpace.Advance(Keywords.Of.Length);
            }

            // Format cases
            var formattedCases = new List<Case>();
            // Case branches indent to next multiple of 4 from the case keyword position (same as scrutinee)
            var caseContext = afterOf.ReturnToIndent(caseIndentRef).NextRowToIndent();
            var caseBranchIndentContext = caseContext; // Save for ReturnToIndent

            for (var i = 0; i < caseExpr.CaseBlock.Cases.Count; i++)
            {
                var caseItem = caseExpr.CaseBlock.Cases[i];

                // Add empty line between cases (after first case)
                if (i > 0)
                {
                    caseContext = caseContext.NextRowToIndent();
                }

                // Check for comments before this case pattern
                var prevEnd = i is 0
                    ? caseExpr.CaseBlock.Expression.Range.End
                    : caseExpr.CaseBlock.Cases[i - 1].Expression.Range.End;
                var commentsBeforePattern = commentQueries.GetBetweenRows(
                    prevEnd.Row, caseItem.Pattern.Range.Start.Row);

                foreach (var comment in commentsBeforePattern)
                {
                    // Format the comment, add it to the context, and position at indent on the next row
                    caseContext = caseContext.FormatAndAddCommentThenNextRowToIndent(comment);
                }

                // Format and transform the pattern with comments
                var patternResult =
                    FormatPattern(caseItem.Pattern, caseContext);

                var transformedPatternNode = patternResult.FormattedNode;

                var afterPattern = patternResult.Context;

                // " ->" (space before arrow)
                var afterPatternSpace = afterPattern.Advance(1);
                var arrowLoc = afterPatternSpace.CurrentLocation();
                var afterArrow = afterPatternSpace.Advance(2);

                // Expression on new line, indented from the pattern position
                // Create reference at pattern position + 4
                var caseExprRef = caseContext.Advance(Indentation.Full).SetIndentToCurrentColumn();
                var caseExprContext = afterArrow.ReturnToIndent(caseExprRef).NextRowToIndent();

                // Check for comments before the case expression
                var commentsBeforeExpr = commentQueries.GetBetweenRanges(caseItem.Pattern.Range, caseItem.Expression.Range).ToList();

                foreach (var comment in commentsBeforeExpr)
                {
                    // Format the comment, add it to the context, and position at indent on the next row
                    caseExprContext = caseExprContext.FormatAndAddCommentThenNextRowToIndent(comment);
                }

                var caseExprResult = FormatExpression(caseItem.Expression, caseExprContext);

                formattedCases.Add(new Case(
                    transformedPatternNode,
                    arrowLoc,
                    caseExprResult.FormattedNode
                ));

                // Only add newline for blank line before next case (not after last case)
                if (i < caseExpr.CaseBlock.Cases.Count - 1)
                {
                    // Maintain the case-specific indentation for subsequent branches
                    caseContext = caseExprResult.Context.ReturnToIndent(caseBranchIndentContext).NextRowToIndent();
                }
                else
                {
                    caseContext = caseExprResult.Context
                        .ReturnToIndent(caseBranchIndentContext);
                }
            }

            var formattedCaseBlock = new CaseBlock(
                caseTokenLoc,
                exprResult.FormattedNode,
                ofTokenLoc,
                formattedCases
            );

            return FormattingResult<ExpressionSyntax>.Create(
                new ExpressionSyntax.CaseExpression(formattedCaseBlock),
                caseContext.ReturnToIndent(context));
        }

        private FormattingResult<ExpressionSyntax> FormatLetExpression(
            ExpressionSyntax.LetExpression letExpr,
            FormattingContext context)
        {
            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            // "let"
            var letTokenLoc = context.CurrentLocation();
            // Create reference at let keyword position for in/expression alignment
            var letTokenRef = context.SetIndentToCurrentColumn();
            var afterLet = context.Advance(Keywords.Let.Length);

            // Format let declarations
            var formattedDecls = new List<Node<ExpressionSyntax.LetDeclaration>>();
            var declContext = afterLet.ReturnToIndent(indentedRef).NextRowToIndent();

            for (var i = 0; i < letExpr.Value.Declarations.Count; i++)
            {
                var decl = letExpr.Value.Declarations[i];

                // Check for comments before this declaration
                int prevEndRow;
                if (i is 0)
                {
                    // For first decl, look for comments between "let" and first declaration
                    prevEndRow = letExpr.Value.LetTokenLocation.Row;
                }
                else
                {
                    prevEndRow = letExpr.Value.Declarations[i - 1].Range.End.Row;
                }

                var commentsBefore = commentQueries.GetBetweenRows(prevEndRow, decl.Range.Start.Row).ToList();

                // If there are comments between declarations (not before first), add a blank line first
                if (i > 0 && commentsBefore.Count > 0)
                {
                    declContext = declContext.NextRowToIndent();
                }

                // Format comments before this declaration
                foreach (var comment in commentsBefore)
                {
                    // Format the comment, add it to the context, and position at indent on the next row
                    declContext = declContext.FormatAndAddCommentThenNextRowToIndent(comment);
                }

                // Add empty line between declarations (after first, and only if no comments were output)
                if (i > 0 && commentsBefore.Count is 0)
                {
                    declContext = declContext.NextRowToIndent();
                }

                var declResult = FormatLetDeclaration(decl, declContext);
                formattedDecls.Add(declResult.FormattedNode);

                declContext = declResult.Context.NextRowToIndent();
            }

            // Check for comments between last declaration and "in"
            var lastDeclEndRow = letExpr.Value.Declarations.Count > 0
                ? letExpr.Value.Declarations[letExpr.Value.Declarations.Count - 1].Range.End.Row
                : letExpr.Value.LetTokenLocation.Row;
            var commentsBeforeIn = commentQueries.GetBetweenRows(
                lastDeclEndRow, letExpr.Value.InTokenLocation.Row);

            // Format comments between last declaration and "in"
            if (commentsBeforeIn.Count > 0)
            {
                // Add blank line before comments
                declContext = declContext.NextRowToIndent();

                foreach (var comment in commentsBeforeIn)
                {
                    // Format the comment, add it to the context, and position at indent on the next row
                    declContext = declContext.FormatAndAddCommentThenNextRowToIndent(comment);
                }
            }

            // "in" on own line at same column as "let"
            var inContext = declContext.ReturnToIndent(letTokenRef).SetIndentColumn();
            var inTokenLoc = inContext.CurrentLocation();
            var afterIn = inContext.Advance(Keywords.In.Length);

            // Check for comments between "in" and the expression
            var commentsBeforeExpr = commentQueries.GetBetweenRows(
                letExpr.Value.InTokenLocation.Row, letExpr.Value.Expression.Range.Start.Row);

            // Expression after "in" - reset IndentSpaces to match let's position
            var exprContext = afterIn.ReturnToIndent(letTokenRef).NextRowToIndent();

            // Format comments between "in" and expression
            foreach (var comment in commentsBeforeExpr)
            {
                // Format the comment, add it to the context, and position at indent on the next row
                exprContext = exprContext.FormatAndAddCommentThenNextRowToIndent(comment);
            }

            var exprResult = FormatExpression(letExpr.Value.Expression, exprContext);

            var formattedLet = new ExpressionSyntax.LetBlock(
                letTokenLoc,
                formattedDecls,
                inTokenLoc,
                exprResult.FormattedNode
            );

            return FormattingResult<ExpressionSyntax>.Create(
                new ExpressionSyntax.LetExpression(formattedLet),
                exprResult.Context);
        }

        private FormattingResult<Node<ExpressionSyntax.LetDeclaration>> FormatLetDeclaration(
            Node<ExpressionSyntax.LetDeclaration> letDecl,
            FormattingContext context)
        {
            // Reference context for indented content (one indent level from base)
            var indentedRef = context.CreateIndentedRef();

            switch (letDecl.Value)
            {
                case ExpressionSyntax.LetDeclaration.LetFunction letFunc:
                    {
                        var startLoc = context.CurrentLocation();
                        var funcName = letFunc.Function.Declaration.Value.Name.Value;
                        var currentCtx = context;
                        Node<Signature>? formattedSignature = null;

                        // Handle signature if present
                        if (letFunc.Function.Signature is { } signature)
                        {
                            // Format the signature: "name : type" or "name :\n    type"
                            var sigNameLoc = currentCtx.CurrentLocation();
                            var afterSigName = currentCtx.Advance(funcName.Length);

                            // " : " or " :" if type is on new line
                            var afterSigNameSpace = afterSigName.Advance(1);
                            var colonLoc = afterSigNameSpace.CurrentLocation();

                            // Check if type annotation is on a new line AND spans multiple lines
                            // (only preserve multiline format for complex types like records that span multiple lines)
                            var typeAnnotOnNewLine = signature.Value.TypeAnnotation.Range.Start.Row > signature.Value.ColonLocation.Row;
                            var typeAnnotSpansMultipleLines = SpansMultipleRows(signature.Value.TypeAnnotation.Range);
                            var preserveMultilineFormat = typeAnnotOnNewLine && typeAnnotSpansMultipleLines;

                            FormattingResult<Node<TypeAnnotation>> typeAnnotResult;

                            if (preserveMultilineFormat)
                            {
                                // Type annotation on new line with extra indentation (for complex types like records)
                                var afterColon = afterSigNameSpace.Advance(1); // just ":"
                                var typeContext = afterColon.ReturnToIndent(indentedRef).NextRowToIndent();
                                typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);
                            }
                            else
                            {
                                // Type annotation on same line after ": " (for simple types or when not on new line)
                                var afterColon = afterSigNameSpace.Advance(2); // ": "
                                typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, afterColon);
                            }

                            var formattedSigValue = new Signature(
                                MakeNode(sigNameLoc, afterSigName.CurrentLocation(), funcName),
                                colonLoc,
                                typeAnnotResult.FormattedNode
                            );

                            formattedSignature = MakeNode(sigNameLoc, typeAnnotResult.Context.CurrentLocation(), formattedSigValue);

                            // Move to next line for function implementation
                            currentCtx = typeAnnotResult.Context.ReturnToIndent(context).NextRowToIndent();
                        }

                        // Now format the function implementation
                        var implStartLoc = currentCtx.CurrentLocation();
                        var afterName = currentCtx.Advance(funcName.Length);

                        // Arguments - format with updated ranges and transform patterns
                        var formattedLetArgs = new List<Node<Pattern>>();
                        var afterArgs = afterName;

                        foreach (var arg in letFunc.Function.Declaration.Value.Arguments)
                        {
                            afterArgs = afterArgs.Advance(1); // space before arg
                            var patternResult = FormatPattern(arg, afterArgs);
                            afterArgs = patternResult.Context;
                            formattedLetArgs.Add(patternResult.FormattedNode);
                        }

                        // " =" (space before equals)
                        var afterArgsSpace = afterArgs.Advance(1);
                        var equalsLoc = afterArgsSpace.CurrentLocation();
                        var afterEquals = afterArgsSpace.Advance(1); // just "="

                        // Check for comments between equals and expression
                        var equalsRow = letFunc.Function.Declaration.Value.EqualsTokenLocation.Row;
                        var exprStartRow = letFunc.Function.Declaration.Value.Expression.Range.Start.Row;
                        var commentsBeforeExpr = commentQueries.GetBetweenRows(equalsRow, exprStartRow).ToList();

                        // Expression on new line, indented
                        var exprContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

                        // Format comments between equals and expression
                        foreach (var comment in commentsBeforeExpr)
                        {
                            // Format the comment, add it to the context, and position at indent on the next row
                            exprContext = exprContext.FormatAndAddCommentThenNextRowToIndent(comment);
                        }

                        var exprResult = FormatExpression(letFunc.Function.Declaration.Value.Expression, exprContext);

                        var formattedImpl = new FunctionImplementation(
                            MakeNode(implStartLoc, afterName.CurrentLocation(), funcName),
                            formattedLetArgs,
                            equalsLoc,
                            exprResult.FormattedNode
                        );

                        var formattedLetFunc = new ExpressionSyntax.LetDeclaration.LetFunction(
                            new FunctionStruct(
                                letFunc.Function.Documentation,
                                formattedSignature,
                                MakeNode(implStartLoc, exprResult.Context.CurrentLocation(), formattedImpl)
                            )
                        );

                        var range = MakeRange(startLoc, exprResult.Context.CurrentLocation());
                        return FormattingResult<Node<ExpressionSyntax.LetDeclaration>>.Create(
                            MakeNode<ExpressionSyntax.LetDeclaration>(range, formattedLetFunc),
                            exprResult.Context.ReturnToIndent(context));
                    }

                case ExpressionSyntax.LetDeclaration.LetDestructuring letDestructuring:
                    {
                        var patternResult = FormatPattern(letDestructuring.Pattern, context);
                        var transformedPatternNode = patternResult.FormattedNode;
                        var afterPattern = patternResult.Context;

                        // " =" (space before equals)
                        var afterPatternSpace = afterPattern.Advance(1);
                        var destructEqualsLoc = afterPatternSpace.CurrentLocation();
                        var afterDestructEquals = afterPatternSpace.Advance(1); // just "="

                        // Check for comments between equals and expression
                        var equalsRow = letDestructuring.EqualsTokenLocation.Row;
                        var exprStartRow = letDestructuring.Expression.Range.Start.Row;
                        var commentsBeforeExpr = commentQueries.GetBetweenRows(equalsRow, exprStartRow).ToList();

                        var destructExprContext = afterDestructEquals.ReturnToIndent(indentedRef).NextRowToIndent();

                        // Format comments between equals and expression
                        foreach (var comment in commentsBeforeExpr)
                        {
                            // Format the comment, add it to the context, and position at indent on the next row
                            destructExprContext = destructExprContext.FormatAndAddCommentThenNextRowToIndent(comment);
                        }

                        var exprResult = FormatExpression(letDestructuring.Expression, destructExprContext);

                        var formattedDestructuring = new ExpressionSyntax.LetDeclaration.LetDestructuring(
                            transformedPatternNode,
                            destructEqualsLoc,
                            exprResult.FormattedNode
                        );

                        var destructRange = MakeRange(context.CurrentLocation(), exprResult.Context.CurrentLocation());
                        return FormattingResult<Node<ExpressionSyntax.LetDeclaration>>.Create(
                            MakeNode<ExpressionSyntax.LetDeclaration>(destructRange, formattedDestructuring),
                            exprResult.Context.ReturnToIndent(context));
                    }

                default:
                    throw new System.NotImplementedException(
                        $"LetDeclaration formatting not implemented for: {letDecl.Value.GetType().Name} " +
                        $"at row {letDecl.Range.Start.Row}, column {letDecl.Range.Start.Column}");
            }
        }

        private FormattingResult<ExpressionSyntax> FormatLambdaExpression(
            ExpressionSyntax.LambdaExpression lambdaExpr,
            FormattingContext context)
        {
            // "\"
            var backslashLoc = context.CurrentLocation();
            var afterBackslash = context.Advance(1);

            // Arguments (no space before first argument, space between subsequent arguments)
            var afterArgs = afterBackslash;
            var formattedArgs = new List<Node<Pattern>>();

            for (var i = 0; i < lambdaExpr.Lambda.Arguments.Count; i++)
            {
                var arg = lambdaExpr.Lambda.Arguments[i];
                var patternResult = FormatPattern(arg, afterArgs);
                formattedArgs.Add(patternResult.FormattedNode);

                afterArgs = patternResult.Context;
                afterArgs = afterArgs.Advance(1); // space after each argument
            }

            // "->" (space already added after last arg)
            var arrowLoc = afterArgs.CurrentLocation();
            var afterArrow = afterArgs.Advance(2); // "->"

            // Check if body should be on a new line
            // Body goes on new line if:
            // 1. Original body was on a new line, OR
            // 2. The body expression spans multiple lines (multiline function application etc.)
            var bodyStartRow = lambdaExpr.Lambda.Expression.Range.Start.Row;
            var bodyEndRow = lambdaExpr.Lambda.Expression.Range.End.Row;
            var arrowRow = lambdaExpr.Lambda.ArrowLocation.Row;
            var bodyWasOnNewLine = bodyStartRow > arrowRow;
            var bodySpansMultipleLines = bodyEndRow > bodyStartRow;
            var bodyOnNewLine = bodyWasOnNewLine || bodySpansMultipleLines;

            FormattingResult<Node<ExpressionSyntax>> exprResult;

            if (bodyOnNewLine)
            {
                // Body on new line - use next multiple of 4 for indentation from backslash position
                // This handles cases where indent caused by pipeline and opening of lambda is > 3,
                // moving inner nodes to the next indent level
                var bodyRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                var bodyContext = afterArrow.ReturnToIndent(bodyRef).NextRowToIndent();

                // Check for comments between arrow and body expression in original
                var commentsBeforeExpr = commentQueries.GetBetweenRows(arrowRow, bodyStartRow);

                // Format any comments that appear before the expression
                foreach (var comment in commentsBeforeExpr)
                {
                    bodyContext = bodyContext.FormatAndAddCommentAndNextRowToIndent(comment);
                }

                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, bodyContext);
            }
            else
            {
                // Body on same line after space
                var afterSpace = afterArrow.Advance(1);
                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, afterSpace);
            }

            var formattedLambda = new LambdaStruct(
                backslashLoc,
                formattedArgs,
                arrowLoc,
                exprResult.FormattedNode
            );

            return FormattingResult<ExpressionSyntax>.Create(
                new ExpressionSyntax.LambdaExpression(formattedLambda),
                exprResult.Context);
        }

        private FormattingResult<ExpressionSyntax> FormatTupledExpression(
            ExpressionSyntax.TupledExpression tupledExpr,
            Range originalRange,
            FormattingContext context)
        {
            // Get the SeparatedSyntaxList directly
            if (tupledExpr.Elements is not SeparatedSyntaxList<Node<ExpressionSyntax>>.NonEmpty nonEmptyElements)
            {
                // Empty tuple - shouldn't happen for tuples but handle gracefully
                return FormattingResult<ExpressionSyntax>.Create(tupledExpr, context);
            }

            // Create reference contexts at open paren for alignment
            var elemAlignRef = context.SetIndentToCurrentColumn();
            // Create reference for content indented past paren
            var elemContentRef = context.Advance(2).SetIndentToCurrentColumn();

            // Use the unified generic separated list formatter
            var singleLineConfig = new SingleLineSeparatedListConfig(
                OpenBracket: "( ",
                Separator: ", ",
                CloseBracket: " )");

            var multilineConfig = new SeparatedListConfig(
                AlignmentRef: elemAlignRef,
                ContentIndentRef: elemContentRef,
                Separator: Keywords.Comma + " ",  // ", "
                CloseBracket: ")");

            var (formattedList, finalContext) = FormatSeparatedList(
                separatedList: tupledExpr.Elements,
                formatItem: FormatExpression,
                commentQueries: commentQueries,
                singleLineConfig: singleLineConfig,
                multilineConfig: multilineConfig,
                context: context,
                containerRange: originalRange,
                commentStyle: SeparatedListCommentStyle.TupleStyle);

            // Since tuples always have at least 2 elements, the result should be NonEmpty
            if (formattedList is SeparatedSyntaxList<Node<ExpressionSyntax>>.NonEmpty nonEmptyResult)
            {
                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.TupledExpression(
                    nonEmptyResult
                ), finalContext);
            }

            // Fallback - shouldn't happen for valid tuples
            return FormattingResult<ExpressionSyntax>.Create(tupledExpr, finalContext);
        }

        private FormattingResult<ExpressionSyntax> FormatRecordUpdateExpression(
            ExpressionSyntax.RecordUpdateExpression recordUpdate,
            Range originalRange,
            FormattingContext context)
        {
            var fields = Stil4mElmSyntax7.FromFullSyntaxModel.ToList(recordUpdate.Fields);

            // Detect if record update should be multiline based on the containing node's range only
            var isMultiline = SpansMultipleRows(originalRange);

            // "{ "
            var afterOpenBrace = context.Advance(2);

            // Record name
            var recordNameLoc = afterOpenBrace.CurrentLocation();
            var afterRecordName = afterOpenBrace.Advance(recordUpdate.RecordName.Value.Length);

            if (isMultiline)
            {
                // Multiline format:
                // { r
                //     | field = value
                //     , field2 = value2
                // }

                // Record name is on same line as opening brace
                // Create reference contexts for alignment
                var openBraceRef = context.SetIndentToCurrentColumn();
                var pipeRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                var pipeContext = afterRecordName.ReturnToIndent(pipeRef).NextRowToIndent();
                var pipeLoc = pipeContext.CurrentLocation();
                var afterPipe = pipeContext.Advance(2); // "| "

                // Create reference for value indentation (pipe + 4)
                var valueIndentRef = pipeRef.CreateIndentedRef();

                RecordExprField? firstField = null;
                var restFields = new List<(Location SeparatorLocation, RecordExprField Node)>();
                var fieldCtx = afterPipe;

                for (var i = 0; i < fields.Count; i++)
                {
                    var field = fields[i];
                    Location? separatorLoc = null;

                    if (i > 0)
                    {
                        // Move to new line, align comma with pipe
                        fieldCtx = fieldCtx.ReturnToIndent(pipeRef).NextRowToIndent();
                        separatorLoc = fieldCtx.CurrentLocation();
                        fieldCtx = fieldCtx.Advance(2); // ", "
                    }

                    var fieldStartLoc = fieldCtx.CurrentLocation();
                    var afterFieldName = fieldCtx.Advance(field.FieldName.Value.Length);
                    var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "

                    // Check if the value expression is on a new line in the original
                    var valueOnNewLine = field.ValueExpr.Range.Start.Row > field.EqualsLocation.Row;
                    FormattingContext valueContext;

                    if (valueOnNewLine)
                    {
                        // " =" then newline with extra indentation
                        var afterEq = afterFieldName.Advance(2); // " ="
                        valueContext = afterEq.ReturnToIndent(valueIndentRef).NextRowToIndent();
                    }
                    else
                    {
                        // " = " on same line
                        valueContext = afterFieldName.Advance(3); // " = "
                    }

                    var fieldResult = FormatExpression(field.ValueExpr, valueContext);

                    var fieldNameNode = MakeNode(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                    var formattedField = new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode);

                    if (!separatorLoc.HasValue)
                    {
                        firstField = formattedField;
                    }
                    else
                    {
                        restFields.Add((separatorLoc.Value, formattedField));
                    }

                    fieldCtx = fieldResult.Context.ReturnToIndent(context);
                }

                // Closing brace on new line, aligned with opening brace (not pipe!)
                var closeCtx = fieldCtx.ReturnToIndent(openBraceRef).NextRowToIndent();
                var afterCloseBrace = closeCtx.Advance(1); // "}"

                SeparatedSyntaxList<RecordExprField> separatedFields;
                if (firstField is not null)
                {
                    separatedFields = new SeparatedSyntaxList<RecordExprField>.NonEmpty(firstField, restFields);
                }
                else
                {
                    separatedFields = new SeparatedSyntaxList<RecordExprField>.Empty();
                }

                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.RecordUpdateExpression(
                    MakeNode(recordNameLoc, afterRecordName.CurrentLocation(), recordUpdate.RecordName.Value),
                    pipeLoc,
                    separatedFields
                ), afterCloseBrace.ReturnToIndent(context));
            }
            else
            {
                // Single line format: { r | field = value, field2 = value2 }
                // Helper to format a record update field for single-line
                FormattingResult<RecordExprField> FormatRecordUpdateField(
                    RecordExprField field, FormattingContext ctx)
                {
                    var fieldStartLoc = ctx.CurrentLocation();
                    var afterFieldName = ctx.Advance(field.FieldName.Value.Length);
                    var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                    var afterEq = afterFieldName.Advance(3); // " = "
                    var valueResult = FormatExpression(field.ValueExpr, afterEq);

                    var fieldNameNode = MakeNode(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);
                    var formattedField = new RecordExprField(fieldNameNode, equalsLoc, valueResult.FormattedNode);

                    return FormattingResult<RecordExprField>.Create(formattedField, valueResult.Context);
                }

                // Helper to get the range of a record field
                static Range GetRecordFieldRange(RecordExprField field) =>
                    MakeRange(field.FieldName.Range.Start, field.ValueExpr.Range.End);

                // " | "
                var pipeLoc = afterRecordName.Advance(1).CurrentLocation();
                var afterPipe = afterRecordName.Advance(3);

                // Handle empty fields case
                if (recordUpdate.Fields is SeparatedSyntaxList<RecordExprField>.Empty)
                {
                    // " }"
                    var afterCloseBrace = afterPipe.Advance(2);
                    return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.RecordUpdateExpression(
                        MakeNode(recordNameLoc, afterRecordName.CurrentLocation(), recordUpdate.RecordName.Value),
                        pipeLoc,
                        recordUpdate.Fields
                    ), afterCloseBrace);
                }

                // Use the general single-line formatter
                var singleLineConfig = new SingleLineSeparatedListConfig(
                    OpenBracket: "",  // No open bracket - we start after "|"
                    Separator: ", ",
                    CloseBracket: " }");

                var nonEmptyFields = (SeparatedSyntaxList<RecordExprField>.NonEmpty)recordUpdate.Fields;
                var (formattedFields, afterClose) = FormatSingleLineSeparatedListGeneral(
                    separatedList: nonEmptyFields,
                    formatItem: FormatRecordUpdateField,
                    getItemRange: GetRecordFieldRange,
                    commentQueries: commentQueries,
                    config: singleLineConfig,
                    context: afterPipe,
                    containerRange: originalRange);

                return FormattingResult<ExpressionSyntax>.Create(new ExpressionSyntax.RecordUpdateExpression(
                    MakeNode(recordNameLoc, afterRecordName.CurrentLocation(), recordUpdate.RecordName.Value),
                    pipeLoc,
                    formattedFields
                ), afterClose);
            }
        }

        private static FormattingResult<ExpressionSyntax> FormatGLSLExpression(
            ExpressionSyntax.GLSLExpression glslExpr,
            Range originalRange,
            FormattingContext context)
        {
            // GLSL expressions are formatted as: [glsl| shader_code |]
            // The shader code is preserved exactly as is (including internal whitespace and newlines)
            var afterOpenBracket = context.Advance(6); // "[glsl|"

            // The shader code spans multiple lines - calculate end position
            var shaderCode = glslExpr.ShaderCode;
            var currentCtx = afterOpenBracket;

            // Track position through the shader code
            foreach (var ch in shaderCode)
            {
                if (ch is '\n')
                {
                    currentCtx = currentCtx.ResetIndent().NextRowToIndent();
                }
                else
                {
                    currentCtx = currentCtx.Advance(1);
                }
            }

            // "|]"
            var afterCloseBracket = currentCtx.Advance(2);

            return FormattingResult<ExpressionSyntax>.Create(glslExpr, afterCloseBracket.ReturnToIndent(context));
        }

        #endregion

        #region Pattern Formatting

        /// <summary>
        /// Directly transforms a pattern into a node with correct positions.
        /// This method builds the pattern node tree while tracking positions in the context,
        /// avoiding any intermediate string representation.
        /// </summary>
        private FormattingResult<Node<Pattern>> FormatPattern(
            Node<Pattern> patternNode,
            FormattingContext context)
        {
            var pattern = patternNode.Value;
            var originalRange = patternNode.Range;
            var startLoc = context.CurrentLocation();

            switch (pattern)
            {
                case Pattern.AllPattern:
                    return FormatSimplePattern(pattern, 1, startLoc, context);

                case Pattern.VarPattern varPattern:
                    return FormatSimplePattern(pattern, varPattern.Name.Length, startLoc, context);

                case Pattern.UnitPattern:
                    return FormatSimplePattern(pattern, 2, startLoc, context);

                case Pattern.CharPattern charPattern:
                    return FormatSimplePattern(pattern, Rendering.RenderCharLiteral(charPattern.Value).Length, startLoc, context);

                case Pattern.StringPattern stringPattern:
                    return FormatSimplePattern(pattern, Rendering.RenderStringLiteral(stringPattern.Value).Length, startLoc, context);

                case Pattern.IntPattern intPattern:
                    return FormatSimplePattern(pattern, intPattern.Value.ToString().Length, startLoc, context);

                case Pattern.HexPattern hexPattern:
                    return FormatSimplePattern(pattern, Rendering.RenderHexPattern(hexPattern.Value).Length, startLoc, context);

                case Pattern.FloatPattern floatPattern:
                    return FormatSimplePattern(pattern, Rendering.FormatFloatForElm(floatPattern.Value).Length, startLoc, context);

                case Pattern.RecordPattern recordPattern:
                    {
                        // Format: { field1, field2, ... }
                        // Record patterns are always single-line
                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "{ ",
                            Separator: ", ",
                            CloseBracket: " }");

                        // Helper to format a string field node
                        static FormattingResult<Node<string>> FormatStringField(
                            Node<string> field, FormattingContext ctx)
                        {
                            var fieldStartLoc = ctx.CurrentLocation();
                            var fieldEndLoc = new Location(fieldStartLoc.Row, fieldStartLoc.Column + field.Value.Length);
                            var formattedField = MakeNode(fieldStartLoc, fieldEndLoc, field.Value);
                            return FormattingResult<Node<string>>.Create(
                                formattedField, ctx.Advance(field.Value.Length));
                        }

                        var (formattedList, afterClose) =
                            FormatSingleLineOnlySeparatedList(
                                separatedList: recordPattern.Fields,
                                formatItem: FormatStringField,
                                commentQueries: commentQueries,
                                config: singleLineConfig,
                                context: context,
                                containerRange: originalRange);

                        var endLoc = afterClose.CurrentLocation();
                        Pattern newPattern = new Pattern.RecordPattern(formattedList);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, afterClose);
                    }

                case Pattern.NamedPattern namedPattern when namedPattern.Arguments.Count is 0:
                    {
                        // Simple named pattern without arguments
                        var qualifiedName = FormatQualifiedName(namedPattern.Name);
                        var len = qualifiedName.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNode(startLoc, endLoc, pattern);
                        return FormattingResult<Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.NamedPattern namedPattern:
                    {
                        // Named pattern with arguments: Name arg1 arg2 ...
                        var qualifiedName = FormatQualifiedName(namedPattern.Name);
                        var currentContext = context.Advance(qualifiedName.Length);

                        var newArguments = new List<Node<Pattern>>();

                        foreach (var arg in namedPattern.Arguments)
                        {
                            // Space before each argument
                            currentContext = currentContext.Advance(1);

                            var argResult = FormatPattern(arg, currentContext);
                            newArguments.Add(argResult.FormattedNode);
                            currentContext = argResult.Context;
                        }

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.NamedPattern(namedPattern.Name, newArguments);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.ParenthesizedPattern parenPattern:
                    {
                        // Format: (inner_pattern)
                        var currentContext = context.Advance(1); // "("

                        // Handle comments between opening paren and inner pattern
                        var commentsAfterParen = commentQueries.GetOnRowBetweenColumns(
                            originalRange.Start.Row,
                            originalRange.Start.Column,
                            parenPattern.Pattern.Range.Start.Column);

                        foreach (var comment in commentsAfterParen)
                        {
                            currentContext = currentContext.FormatAndAddComment(comment);
                            currentContext = currentContext.Advance(1); // space after comment
                        }

                        var innerResult = FormatPattern(parenPattern.Pattern, currentContext);
                        currentContext = innerResult.Context;

                        // Handle comments between inner pattern and closing paren
                        var commentsBeforeCloseParen = commentQueries.GetOnRowBetweenColumns(
                            originalRange.Start.Row,
                            parenPattern.Pattern.Range.End.Column,
                            originalRange.End.Column);

                        foreach (var comment in commentsBeforeCloseParen)
                        {
                            currentContext = currentContext.Advance(1); // space before comment
                            currentContext = currentContext.FormatAndAddComment(comment);
                        }

                        currentContext = currentContext.Advance(1); // ")"

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.ParenthesizedPattern(innerResult.FormattedNode);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.TuplePattern tuplePattern:
                    {
                        // Tuple patterns are always single-line with inline comment handling
                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "( ",
                            Separator: ", ",
                            CloseBracket: " )");

                        var (formattedList, afterClose) = FormatSingleLineOnlySeparatedList(
                            separatedList: tuplePattern.Elements,
                            formatItem: FormatPattern,
                            commentQueries: commentQueries,
                            config: singleLineConfig,
                            context: context,
                            containerRange: originalRange);

                        var endLoc = afterClose.CurrentLocation();
                        // TuplePattern requires NonEmpty, but we handle gracefully
                        if (formattedList is SeparatedSyntaxList<Node<Pattern>>.NonEmpty nonEmptyFormattedList)
                        {
                            Pattern newPattern = new Pattern.TuplePattern(nonEmptyFormattedList);
                            var node = MakeNode(startLoc, endLoc, newPattern);
                            return FormattingResult<Node<Pattern>>.Create(node, afterClose);
                        }
                        else
                        {
                            // Empty tuple pattern - shouldn't happen but handle gracefully
                            var emptyEndLoc = new Location(startLoc.Row, startLoc.Column + 2);
                            Pattern emptyPattern = new Pattern.TuplePattern(
                                new SeparatedSyntaxList<Node<Pattern>>.Empty());
                            var emptyNode = MakeNode(startLoc, emptyEndLoc, emptyPattern);
                            return FormattingResult<Node<Pattern>>.Create(
                                emptyNode, context.Advance(2));
                        }
                    }

                case Pattern.ListPattern listPattern:
                    {
                        // List patterns are always single-line with inline comment handling
                        var singleLineConfig = new SingleLineSeparatedListConfig(
                            OpenBracket: "[ ",
                            Separator: ", ",
                            CloseBracket: " ]");

                        var (formattedList, afterClose) = FormatSingleLineOnlySeparatedList(
                            separatedList: listPattern.Elements,
                            formatItem: FormatPattern,
                            commentQueries: commentQueries,
                            config: singleLineConfig,
                            context: context,
                            containerRange: originalRange);

                        var endLoc = afterClose.CurrentLocation();
                        Pattern newPattern = new Pattern.ListPattern(formattedList);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, afterClose);
                    }

                case Pattern.UnConsPattern unConsPattern:
                    {
                        // Format: head :: tail
                        var headResult = FormatPattern(unConsPattern.Head, context);
                        var currentContext = headResult.Context;

                        // Handle comments between head and :: operator
                        var commentsBeforeOp = commentQueries.GetOnRowBetweenColumns(
                            originalRange.Start.Row,
                            unConsPattern.Head.Range.End.Column,
                            unConsPattern.ConsOperatorLocation.Column);

                        foreach (var comment in commentsBeforeOp)
                        {
                            currentContext = currentContext.Advance(1); // space before comment
                            currentContext = currentContext.FormatAndAddComment(comment);
                        }

                        currentContext = currentContext.Advance(1); // space before ::
                        var consOpLoc = currentContext.CurrentLocation();
                        currentContext = currentContext.Advance(2); // "::" (2 chars)

                        // Handle comments between :: and tail pattern
                        var commentsAfterOp = commentQueries.GetOnRowBetweenColumns(
                            originalRange.Start.Row,
                            unConsPattern.ConsOperatorLocation.Column + 2, // After ::
                            unConsPattern.Tail.Range.Start.Column);

                        if (commentsAfterOp.Count > 0)
                        {
                            foreach (var comment in commentsAfterOp)
                            {
                                currentContext = currentContext.Advance(1); // space before comment
                                currentContext = currentContext.FormatAndAddComment(comment);
                            }
                            currentContext = currentContext.Advance(1); // space after last comment
                        }
                        else
                        {
                            currentContext = currentContext.Advance(1); // space after ::
                        }

                        var tailResult = FormatPattern(unConsPattern.Tail, currentContext);
                        currentContext = tailResult.Context;

                        // Handle comments after tail (but within the overall pattern range)
                        var commentsAfterTail = commentQueries.GetOnRowBetweenColumns(
                            originalRange.Start.Row,
                            unConsPattern.Tail.Range.End.Column,
                            originalRange.End.Column);

                        foreach (var comment in commentsAfterTail)
                        {
                            currentContext = currentContext.Advance(1); // space before comment
                            currentContext = currentContext.FormatAndAddComment(comment);
                        }

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.UnConsPattern(
                            headResult.FormattedNode, consOpLoc, tailResult.FormattedNode);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.AsPattern asPattern:
                    {
                        // Format: pattern as name
                        var innerResult = FormatPattern(asPattern.Pattern, context);
                        var currentContext = innerResult.Context;

                        currentContext = currentContext.Advance(1); // space before "as"
                        var asTokenLoc = currentContext.CurrentLocation();
                        currentContext = currentContext.Advance(3); // "as "

                        // name
                        var nameLoc = currentContext.CurrentLocation();
                        var nameLen = asPattern.Name.Value.Length;
                        var nameEndLoc = new Location(nameLoc.Row, nameLoc.Column + nameLen);
                        currentContext = currentContext.Advance(nameLen);

                        var newNameNode = MakeNode(nameLoc, nameEndLoc, asPattern.Name.Value);

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.AsPattern(
                            innerResult.FormattedNode, asTokenLoc, newNameNode);
                        var node = MakeNode(startLoc, endLoc, newPattern);
                        return FormattingResult<Node<Pattern>>.Create(node, currentContext);
                    }

                default:
                    throw new System.NotImplementedException(
                        $"Pattern type '{pattern.GetType().Name}' not implemented in {nameof(FormatPattern)}");
            }
        }

        /// <summary>
        /// Helper for formatting simple patterns (literals and names) that have a fixed length.
        /// Reduces repetition across AllPattern, VarPattern, UnitPattern, CharPattern, StringPattern,
        /// IntPattern, HexPattern, and FloatPattern cases.
        /// </summary>
        private static FormattingResult<Node<Pattern>> FormatSimplePattern(
            Pattern pattern,
            int length,
            Location startLoc,
            FormattingContext context)
        {
            var endLoc = new Location(startLoc.Row, startLoc.Column + length);
            var node = MakeNode(startLoc, endLoc, pattern);
            return FormattingResult<Node<Pattern>>.Create(
                node, context.Advance(length));
        }

        private static string EscapeCharForPattern(char ch) =>
            ch switch
            {
                '\n' => "\\n",
                '\r' => "\\u{000D}",  // Elm uses Unicode escape for carriage return
                '\t' => "\\t",
                '\\' => "\\\\",
                '\'' => "\\'",

                _ when ch < 32 =>
                $"\\u{{{(int)ch:X4}}}",

                _ =>
                ch.ToString()
            };

        private static string FormatQualifiedName(QualifiedNameRef nameRef)
        {
            if (nameRef.ModuleName.Count > 0)
                return string.Join(".", nameRef.ModuleName) + "." + nameRef.Name;

            return nameRef.Name;
        }

        #endregion
    }

    #endregion

    /// <summary>
    /// Creates a new node that contains the specified value and is associated with the given source range.
    /// </summary>
    private static Node<T> MakeNode<T>(
        Location start,
        Location end,
        T value) =>
        new(MakeRange(start, end), value);

    /// <summary>
    /// Creates a new node that contains the specified value and is associated with the given source range.
    /// </summary>
    private static Node<T> MakeNode<T>(
        Range range,
        T value) =>
        new(range, value);

    /// <summary>
    /// Creates a Range from start and end locations.
    /// </summary>
    private static Range MakeRange(Location start, Location end) =>
        new(start, end);
}
