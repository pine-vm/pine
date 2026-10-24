using Pine.Core.Elm.Syntax.Concrete;
using System.Collections.Generic;

namespace Pine.Core.Elm.Syntax;

/// <summary>
/// Extension methods for creating and converting Node&lt;ParsedComment&gt; instances.
/// </summary>
public static class CommentNodeExtensions
{
    /// <summary>
    /// Creates a Node&lt;ParsedComment&gt; by parsing the comment text from the given node.
    /// </summary>
    public static Node<ParsedComment> ToCommentNode(this Node<string> node) =>
        new(node.Range, ParsedComment.FromText(node.Value));

    /// <summary>
    /// Creates a Node&lt;string&gt; with the comment text at the specified range.
    /// Used when formatting comments to place them at new locations.
    /// </summary>
    public static Node<string> ToStringNodeWithRange(this Node<ParsedComment> comment, Location start, Location end) =>
        new(new Range(start, end), comment.Value.Text);
}

/// <summary>
/// Encapsulates all comment query operations with pre-sorted and pre-indexed data.
/// This class handles filtering and sorting of comments for the formatter.
/// All query methods return comments ordered by row then column.
/// Comments are parsed once on construction, avoiding repeated string inspection.
/// </summary>
public class CommentQueryHelper
{
    private readonly IReadOnlyList<Node<ParsedComment>> _allComments;

    private readonly Dictionary<int, List<Node<ParsedComment>>> _byStartRow = [];

    /// <summary>
    /// Creates a new comment query helper from a list of comments.
    /// Comments are parsed once during construction.
    /// </summary>
    /// <param name="comments">All comments in the file.</param>
    public CommentQueryHelper(IReadOnlyList<Node<string>> comments)
    {
        var entries = new (Node<ParsedComment> Comment, int Index)[comments.Count];

        for (var i = 0; i < entries.Length; i++)
            entries[i] = (comments[i].ToCommentNode(), i);

        System.Array.Sort(
            entries,
            static (left, right) =>
            {
                var rowComparison = left.Comment.Range.Start.Row.CompareTo(right.Comment.Range.Start.Row);

                if (rowComparison is not 0)
                    return rowComparison;

                var columnComparison = left.Comment.Range.Start.Column.CompareTo(right.Comment.Range.Start.Column);

                return columnComparison is 0 ? left.Index.CompareTo(right.Index) : columnComparison;
            });

        var sortedComments = new Node<ParsedComment>[entries.Length];

        for (var i = 0; i < entries.Length; i++)
        {
            var comment = entries[i].Comment;
            sortedComments[i] = comment;

            if (!_byStartRow.TryGetValue(comment.Range.Start.Row, out var rowComments))
            {
                rowComments = [];
                _byStartRow.Add(comment.Range.Start.Row, rowComments);
            }

            rowComments.Add(comment);
        }

        _allComments = sortedComments;
    }

    /// <summary>
    /// Get comments between two row numbers.
    /// Both bounds are exclusive: comments must start after afterRow and before beforeRow.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetBetweenRows(int afterRow, int beforeRow) =>
        Filter(_allComments, c => c.Range.Start.Row > afterRow && c.Range.Start.Row < beforeRow);

    /// <summary>
    /// Get comments that appear before a given row.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetBeforeRow(int beforeRow) =>
        Filter(_allComments, c => c.Range.Start.Row < beforeRow);

    /// <summary>
    /// Get comments that appear after a given row.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetAfterRow(int afterRow) =>
        Filter(_allComments, c => c.Range.Start.Row > afterRow);

    /// <summary>
    /// Get comments that start after the end of one range and before the start of another range.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetBetweenRanges(Range after, Range before) =>
        GetBetweenRows(after.End.Row, before.Start.Row);

    /// <summary>
    /// Get a trailing comment on the same row as the element ends, after the element ends.
    /// </summary>
    public Node<ParsedComment>? GetTrailing(Range elementRange) =>
        _byStartRow.TryGetValue(elementRange.End.Row, out var rowComments)
        ?
        Find(rowComments, c => c.Range.Start.Column > elementRange.End.Column)
        :
        null;

    /// <summary>
    /// Get comments between two row numbers with inclusive end bound.
    /// Start is exclusive, end is inclusive.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetBetweenRowsInclusiveEnd(int afterRow, int beforeRowInclusive) =>
        Filter(_allComments, c => c.Range.Start.Row > afterRow && c.Range.Start.Row <= beforeRowInclusive);

    /// <summary>
    /// Get comments after a location (row/column) but before a target row.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetAfterLocationBeforeRow(Location afterLoc, int beforeRow) =>
        Filter(
            _allComments,
            c =>
            c.Range.Start.Row >= afterLoc.Row &&
            c.Range.Start.Row < beforeRow &&
            (c.Range.Start.Row > afterLoc.Row || c.Range.Start.Column > afterLoc.Column));

    /// <summary>
    /// Get comments between two rows with inclusive end, filtering by column on end row.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetBetweenRowsInclusiveEndWithColumnFilter(
        int afterRow, int beforeRowInclusive, int beforeColumn) =>
        Filter(
            _allComments,
            c =>
            c.Range.Start.Row > afterRow &&
            c.Range.Start.Row <= beforeRowInclusive &&
            (c.Range.Start.Row < beforeRowInclusive || c.Range.Start.Column < beforeColumn));

    /// <summary>
    /// Get comments on a specific row that are before a specific column, but after another row.
    /// Returns comments ordered by column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetOnRowBeforeColumnAfterRow(int row, int beforeColumn, int afterRow) =>
        _byStartRow.TryGetValue(row, out var rowComments)
        ?
        Filter(rowComments, c => c.Range.Start.Column < beforeColumn && c.Range.Start.Row > afterRow)
        :
        [];

    /// <summary>
    /// Get comments after one row/column but before another row, with optional column check.
    /// Returns comments ordered by row then column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetAfterRowBeforeRowWithColumnCheck(
        int afterRow, int afterColumn, int beforeRow, bool requireAfterColumn = false) =>
        Filter(
            _allComments,
            c =>
            c.Range.Start.Row >= afterRow &&
            c.Range.Start.Row < beforeRow &&
            (!requireAfterColumn || c.Range.Start.Row > afterRow || c.Range.Start.Column > afterColumn));

    /// <summary>
    /// Check if there are any comments between two rows (exclusive bounds).
    /// </summary>
    public bool HasBetweenRows(int afterRow, int beforeRow) =>
        Find(_allComments, c => c.Range.Start.Row > afterRow && c.Range.Start.Row < beforeRow) is not null;

    /// <summary>
    /// Check if any comment starts within the given range.
    /// </summary>
    public bool HasWithinRange(Range range) =>
        Find(
            _allComments,
            comment =>
            (comment.Range.Start.Row > range.Start.Row ||
            (comment.Range.Start.Row == range.Start.Row &&
            comment.Range.Start.Column > range.Start.Column)) &&
            (comment.Range.Start.Row < range.End.Row ||
            (comment.Range.Start.Row == range.End.Row &&
            comment.Range.Start.Column < range.End.Column))) is not null;

    /// <summary>
    /// Get all comments on a specific row that start after a given column.
    /// Returns comments ordered by column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetOnRowAfterColumn(int row, int afterColumn) =>
        _byStartRow.TryGetValue(row, out var rowComments)
        ?
        Filter(rowComments, c => c.Range.Start.Column > afterColumn)
        :
        [];

    /// <summary>
    /// Get comments on a specific row that start after one column and end before another column.
    /// Used for finding inline comments between two column positions.
    /// Returns comments ordered by column.
    /// </summary>
    public IReadOnlyList<Node<ParsedComment>> GetOnRowBetweenColumns(int row, int afterColumn, int beforeColumn) =>
        _byStartRow.TryGetValue(row, out var rowComments)
        ?
        Filter(
            rowComments,
            c => c.Range.Start.Column > afterColumn &&
                c.Range.End.Column <= beforeColumn)
        :
        [];

    /// <summary>
    /// Get the first doc comment that starts after a given row.
    /// Used to find the module documentation comment after the module definition.
    /// </summary>
    public Node<ParsedComment>? GetFirstDocCommentAfterRow(int afterRow) =>
        Find(_allComments, c => c.Range.Start.Row > afterRow && c.Value.IsDocComment);

    private static IReadOnlyList<Node<ParsedComment>> Filter(
        IReadOnlyList<Node<ParsedComment>> comments,
        System.Func<Node<ParsedComment>, bool> predicate)
    {
        var result = new List<Node<ParsedComment>>();

        for (var i = 0; i < comments.Count; i++)
        {
            if (predicate(comments[i]))
                result.Add(comments[i]);
        }

        return result;
    }

    private static Node<ParsedComment>? Find(
        IReadOnlyList<Node<ParsedComment>> comments,
        System.Func<Node<ParsedComment>, bool> predicate)
    {
        for (var i = 0; i < comments.Count; i++)
        {
            if (predicate(comments[i]))
                return comments[i];
        }

        return null;
    }
}
