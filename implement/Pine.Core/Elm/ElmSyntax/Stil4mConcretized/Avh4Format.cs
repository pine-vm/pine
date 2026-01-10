using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;
using Range = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range;

namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

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
        public const string Module = "module ";
        public const string PortModule = "port module ";
        public const string EffectModule = "effect module ";
        public const string Port = "port ";
        public const string Effect = "effect ";
        public const string Alias = "alias ";
        public const string ExposingAll = "exposing (..)";
        public const string Exposing = "exposing";
        public const string ExposingOpen = "exposing (";
        public const string SpaceExposingSpace = " exposing ";
        public const string Import = "import ";
        public const string As = " as ";
        public const string AsSpace = "as ";
        public const string Case = "case ";
        public const string Of = " of";
        public const string Let = "let";
        public const string In = "in";
        public const string If = "if";
        public const string IfSpace = "if ";
        public const string SpaceThen = " then";
        public const string Then = "then";
        public const string Else = "else";
        public const string ElseIf = "else if ";
        public const string Type = "type ";
        public const string Unit = "()";
        public const string EmptyList = "[]";
        public const string ListOpen = "[ ";
        public const string ListClose = " ]";
        public const string RecordOpen = "{ ";
        public const string RecordClose = " }";
        public const string TupleOpen = "( ";
        public const string TupleClose = " )";
        public const string OpenParen = "(";
        public const string CloseParen = ")";
        public const string Comma = ", ";
        public const string Colon = ": ";
        public const string ColonSpace = " : ";
        public const string SpaceColon = " :";
        public new const string Equals = " = ";
        public const string EqualsSign = "=";
        public const string SpaceEquals = " =";
        public const string Arrow = "-> ";
        public const string Minus = "-";
        public const string Pipe = " | ";
        public const string PipeSpace = "| ";
        public const string EqualsSpace = "= ";
        public const string Space = " ";
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
        public ImmutableList<Stil4mElmSyntax7.Node<string>> Comments { get; }

        private FormattingContext(
            int currentRow,
            int currentColumn,
            int indentSpaces,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
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
        public FormattingContext NextRowToIndent() =>
            new(CurrentRow + 1, 1 + IndentSpaces, IndentSpaces, Comments);

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
        /// Calculate the end location of a comment given its start location and value.
        /// Also returns whether the comment ends with a newline.
        /// </summary>
        private static (Location EndLocation, bool EndsWithNewline) CalculateCommentEndLocation(
            Location startLocation,
            string commentValue)
        {
            var lines = commentValue.Split('\n');
            var endsWithNewline = commentValue.EndsWith('\n');

            if (lines.Length is 1)
            {
                return (new Location(startLocation.Row, startLocation.Column + commentValue.Length), endsWithNewline);
            }
            else
            {
                var endRow = startLocation.Row + lines.Length - 1;
                var lastLineLength = lines[lines.Length - 1].Length;
                return (new Location(endRow, lastLineLength + 1), endsWithNewline);
            }
        }

        /// <summary>
        /// Creates a new FormattingContext positioned after a comment ends.
        /// If the comment ends with a newline, we're already at the start of a new line.
        /// </summary>
        private FormattingContext PositionedAfterComment(
            Location commentEnd,
            bool commentEndsWithNewline,
            ImmutableList<Stil4mElmSyntax7.Node<string>> updatedComments)
        {
            if (commentEndsWithNewline)
            {
                // Comment ends with newline - we're already at the start of a new line (commentEnd.Row)
                return new FormattingContext(commentEnd.Row, 1 + IndentSpaces, IndentSpaces, updatedComments);
            }
            else
            {
                // Comment doesn't end with newline - move to the next row
                return new FormattingContext(commentEnd.Row + 1, 1 + IndentSpaces, IndentSpaces, updatedComments);
            }
        }

        /// <summary>
        /// Formats a parsed comment and adds it to this context, returning an updated context.
        /// This encapsulates the common pattern of: get location, calculate end, create formatted comment, add to list, update position.
        /// </summary>
        public FormattingContext FormatAndAddComment(Stil4mElmSyntax7.Node<ParsedComment> comment)
        {
            var commentLocation = CurrentLocation();
            var (commentEnd, endsWithNewline) = CalculateCommentEndLocation(commentLocation, comment.Value.Text);
            var formattedComment = comment.ToStringNodeWithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);
            return PositionedAfterComment(commentEnd, endsWithNewline, updatedComments);
        }

        /// <summary>
        /// Formats and adds a Node&lt;string&gt; comment (from AST documentation fields).
        /// </summary>
        public FormattingContext FormatAndAddComment(Stil4mElmSyntax7.Node<string> comment)
        {
            var commentLocation = CurrentLocation();
            var (commentEnd, endsWithNewline) = CalculateCommentEndLocation(commentLocation, comment.Value);
            var formattedComment = comment.WithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);
            return PositionedAfterComment(commentEnd, endsWithNewline, updatedComments);
        }

        /// <summary>
        /// Formats a sequence of comments and adds them to this context, returning an updated context.
        /// </summary>
        public FormattingContext FormatAndAddComments(
            IReadOnlyList<Stil4mElmSyntax7.Node<ParsedComment>> commentsToFormat)
        {
            var currentContext = this;

            foreach (var comment in commentsToFormat)
            {
                currentContext = currentContext.FormatAndAddComment(comment);
            }

            return currentContext;
        }

        /// <summary>
        /// Formats a parsed comment, adds it to the context, and positions at the comment end location (same row).
        /// Use this when content should continue immediately after the comment on the same line.
        /// </summary>
        public FormattingContext FormatAndAddCommentThenPositionAtEnd(
            Stil4mElmSyntax7.Node<ParsedComment> comment)
        {
            var commentLocation = CurrentLocation();
            var (commentEnd, _) = CalculateCommentEndLocation(commentLocation, comment.Value.Text);
            var formattedComment = comment.ToStringNodeWithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);
            return new FormattingContext(commentEnd.Row, commentEnd.Column, IndentSpaces, updatedComments);
        }

        /// <summary>
        /// Formats a parsed comment, adds it to the context, and positions at the indent column on the next row.
        /// Use this when the next content should start at the current indent level on a new line.
        /// </summary>
        public FormattingContext FormatAndAddCommentThenNextRowToIndent(
            Stil4mElmSyntax7.Node<ParsedComment> comment)
        {
            var commentLocation = CurrentLocation();
            var (commentEnd, _) = CalculateCommentEndLocation(commentLocation, comment.Value.Text);
            var formattedComment = comment.ToStringNodeWithRange(commentLocation, commentEnd);
            var updatedComments = Comments.Add(formattedComment);
            return new FormattingContext(commentEnd.Row + 1, 1 + IndentSpaces, IndentSpaces, updatedComments);
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
    private static (Location EndLocation, bool EndsWithNewline) CalculateCommentEndLocationEx(
        Location startLocation,
        string commentValue)
    {
        var lines = commentValue.Split('\n');
        var endsWithNewline = commentValue.EndsWith('\n');

        if (lines.Length is 1)
        {
            return (new Location(startLocation.Row, startLocation.Column + commentValue.Length), endsWithNewline);
        }
        else
        {
            var endRow = startLocation.Row + lines.Length - 1;
            var lastLineLength = lines[lines.Length - 1].Length;
            return (new Location(endRow, lastLineLength + 1), endsWithNewline);
        }
    }

    /// <summary>
    /// Calculate the end location of a comment given its start location and value.
    /// </summary>
    private static Location CalculateCommentEndLocation(Location startLocation, string commentValue) =>
        CalculateCommentEndLocationEx(startLocation, commentValue).EndLocation;

    /// <summary>
    /// Check if a range spans multiple rows.
    /// </summary>
    private static bool SpansMultipleRows(Range range) =>
        range.End.Row > range.Start.Row;

    /// <summary>
    /// Helper to check if a collection of nodes spans multiple rows.
    /// </summary>
    private static bool NodesSpanMultipleRows<T>(IReadOnlyList<Stil4mElmSyntax7.Node<T>> nodes)
    {
        if (nodes.Count < 2)
            return false;

        var firstRow = nodes[0].Range.Start.Row;
        return nodes.Skip(1).Any(node => node.Range.Start.Row != firstRow);
    }

    /// <summary>
    /// Determines if an expression is "simple" enough that it doesn't need parentheses.
    /// Simple expressions include literals, names, unit, records, lists, and tuples
    /// (which already have their own delimiters).
    /// </summary>
    private static bool IsSimpleExpressionThatDoesNotNeedParens(Expression expr) =>
        expr is Expression.Literal
            or Expression.CharLiteral
            or Expression.Integer
            or Expression.Hex
            or Expression.Floatable
            or Expression.FunctionOrValue
            or Expression.UnitExpr
            or Expression.RecordExpr
            or Expression.ListExpr
            or Expression.TupledExpression
            or Expression.RecordAccessFunction;

    #endregion

    #region Visitor Implementation

    /// <summary>
    /// Visitor implementation for AVH4 formatting on concretized syntax model.
    /// </summary>
    private class Avh4FormatVisitor(
        CommentQueryHelper commentQueries,
        IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>> originalIncompleteDeclarations)
    {
        #region File Formatting

        public (File, IReadOnlyList<Stil4mElmSyntax7.Node<string>>, IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>>) FormatFile(
            File file,
            FormattingContext context)
        {
            // Format module definition
            var (formattedModule, contextAfterModule) =
                FormatModuleDefinition(file.ModuleDefinition, context);

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
                contextBeforeImports = FormatCommentsAtContext(
                    commentsAfterModuleBeforeImports, startContext);
                contextBeforeImports = contextBeforeImports.NextRowToIndent();
            }
            else
            {
                contextBeforeImports = contextAfterModule.WithBlankLine();
            }

            // Format imports
            FormattingContext contextAfterImports;
            IReadOnlyList<Stil4mElmSyntax7.Node<Import>> formattedImports;

            if (firstImport is not null)
            {
                (formattedImports, contextAfterImports) =
                    FormatImports(file.Imports, contextBeforeImports);
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
                var startContext = formattedImports.Any()
                    ?
                    contextAfterImports.WithBlankLine().NextRowToIndent()
                    :
                    contextAfterModule.WithBlankLine();

                contextBeforeDecls = FormatCommentsAtContext(
                    commentsBefore, startContext, addBlankLinesAfterNonDocComments: true);

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
                FormattingContext trailingContext;
                if (file.Declarations.Any() || formattedIncompleteDeclarations.Any())
                {
                    trailingContext = contextAfterDeclarations.WithBlankLine().NextRowToIndent();
                }
                else
                {
                    trailingContext = contextBeforeDecls;
                }
                contextAfterTrailingComments = FormatCommentsAtContext(
                    trailingComments, trailingContext);
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

        private static FormattingContext FormatCommentsAtContext(
            IReadOnlyList<Stil4mElmSyntax7.Node<ParsedComment>> comments,
            FormattingContext startContext,
            bool addBlankLinesAfterNonDocComments = false)
        {
            var currentContext = startContext;

            for (var i = 0; i < comments.Count; i++)
            {
                var comment = comments[i];
                currentContext = currentContext.FormatAndAddComment(comment);

                // Only add blank lines after non-doc comments if there's a gap before the next comment
                // or if this is the last comment. This preserves consecutive comments without blank lines.
                if (addBlankLinesAfterNonDocComments && !comment.Value.IsDocComment)
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

        #endregion

        #region Module Formatting

        private static (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatModuleDefinition(
            Stil4mElmSyntax7.Node<Module> module,
            FormattingContext context)
        {
            return module.Value switch
            {
                Module.NormalModule normalModule =>
                FormatNormalModule(normalModule, context),

                Module.PortModule portModule =>
                FormatPortModule(portModule, context),

                Module.EffectModule effectModule =>
                FormatEffectModule(effectModule, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for module type '{module.Value.GetType().Name}' is not implemented.")
            };
        }

        private static (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatNormalModule(
            Module.NormalModule module,
            FormattingContext context)
        {
            // "module "
            var moduleTokenLoc = context.CurrentLocation();
            var afterModuleKeyword = context.Advance(Keywords.Module.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + Keywords.Space.Length);

            var exposingTokenLoc = afterModuleName.CurrentLocation();
            var (formattedExposing, contextAfterExposing) = FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = MakeRange(context.CurrentLocation(), contextAfterExposing.CurrentLocation());
            var moduleNameNode = new Stil4mElmSyntax7.Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing
            );

            return (new Stil4mElmSyntax7.Node<Module>(range, new Module.NormalModule(moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatPortModule(
            Module.PortModule module,
            FormattingContext context)
        {
            var portTokenLoc = context.CurrentLocation();
            var afterPort = context.Advance(Keywords.Port.Length);
            var moduleTokenLoc = afterPort.CurrentLocation();
            var afterModuleKeyword = afterPort.Advance(Keywords.Module.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + Keywords.Space.Length);

            var exposingTokenLoc = afterModuleName.CurrentLocation();
            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = MakeRange(context.CurrentLocation(), contextAfterExposing.CurrentLocation());
            var moduleNameNode = new Stil4mElmSyntax7.Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing
            );

            return (new Stil4mElmSyntax7.Node<Module>(range, new Module.PortModule(portTokenLoc, moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatEffectModule(
            Module.EffectModule module,
            FormattingContext context)
        {
            var effectTokenLoc = context.CurrentLocation();
            var afterEffect = context.Advance(Keywords.Effect.Length);
            var moduleTokenLoc = afterEffect.CurrentLocation();
            var afterModuleKeyword = afterEffect.Advance(Keywords.Module.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + Keywords.Space.Length);

            var exposingTokenLoc = afterModuleName.CurrentLocation();
            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = MakeRange(context.CurrentLocation(), contextAfterExposing.CurrentLocation());
            var moduleNameNode = new Stil4mElmSyntax7.Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new EffectModuleData(
                ModuleName: moduleNameNode,
                ExposingTokenLocation: exposingTokenLoc,
                ExposingList: formattedExposing,
                Command: module.ModuleData.Command,
                Subscription: module.ModuleData.Subscription
            );

            return (new Stil4mElmSyntax7.Node<Module>(range, new Module.EffectModule(effectTokenLoc, moduleTokenLoc, formattedModuleData)), contextAfterExposing);
        }

        private static (Stil4mElmSyntax7.Node<Exposing>, FormattingContext) FormatExposing(
            Stil4mElmSyntax7.Node<Exposing> exposing,
            FormattingContext context)
        {
            return exposing.Value switch
            {
                Exposing.All =>
                FormatExposingAll(context),

                Exposing.Explicit explicitList =>
                FormatExposingExplicit(explicitList, context, IsExposingListMultiLine(explicitList)),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for exposing type '{exposing.Value.GetType().Name}' is not implemented.")
            };
        }

        private static (Stil4mElmSyntax7.Node<Exposing>, FormattingContext) FormatExposingAll(FormattingContext context)
        {
            var afterExposing = context.Advance(Keywords.ExposingAll.Length);
            var range = MakeRange(context.CurrentLocation(), afterExposing.CurrentLocation());
            return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.All(range)), afterExposing);
        }

        private static (Stil4mElmSyntax7.Node<Exposing>, FormattingContext) FormatExposingExplicit(
            Exposing.Explicit explicitList,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                var afterOpenParen = context.Advance(Keywords.ExposingOpen.Length);
                var currentContext = afterOpenParen;

                if (explicitList.Nodes.Count is 0)
                {
                    var closeContext = currentContext.Advance(Keywords.CloseParen.Length);
                    var emptyRange = MakeRange(context.CurrentLocation(), closeContext.CurrentLocation());
                    return (new Stil4mElmSyntax7.Node<Exposing>(emptyRange, new Exposing.Explicit(
                        OpenParenLocation: context.CurrentLocation(),
                        Nodes: new SeparatedSyntaxList<Stil4mElmSyntax7.Node<TopLevelExpose>>.Empty(),
                        CloseParenLocation: closeContext.CurrentLocation() with { Column = closeContext.CurrentLocation().Column - 1 })), closeContext);
                }

                // Parse first node
                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], currentContext);
                currentContext = afterFirst;

                var restNodes = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<TopLevelExpose> Node)>();

                for (var i = 1; i < explicitList.Nodes.Count; i++)
                {
                    var node = explicitList.Nodes[i];
                    var commaLocation = currentContext.CurrentLocation();
                    currentContext = currentContext.Advance(Keywords.Comma.Length);
                    var (formattedNode, nextContext) = FormatTopLevelExpose(node, currentContext);
                    restNodes.Add((commaLocation, formattedNode));
                    currentContext = nextContext;
                }

                currentContext = currentContext.Advance(Keywords.CloseParen.Length);
                var range = MakeRange(context.CurrentLocation(), currentContext.CurrentLocation());
                var nodesList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<TopLevelExpose>>.NonEmpty(firstNode, restNodes);
                return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.Explicit(
                    OpenParenLocation: context.CurrentLocation(),
                    Nodes: nodesList,
                    CloseParenLocation: currentContext.CurrentLocation() with { Column = currentContext.CurrentLocation().Column - 1 })), currentContext);
            }
            else
            {
                // Check if the exposing list is well-formatted (each new line starts with comma)
                // If not, we should break everything onto separate lines
                var isWellFormatted = IsExposingListWellFormatted(explicitList);

                var afterExposingKeyword = context.Advance(Keywords.Exposing.Length);
                // Reference context for indented content (one indent level from base)
                // Go to indent column first, then advance to next level, then set indent
                var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                // Save parent context for later indent restoration
                var parentContext = afterExposingKeyword;
                var parenLineContext = afterExposingKeyword.ReturnToIndent(indentedRef).NextRowToIndent();

                var openParenLocation = parenLineContext.CurrentLocation();
                var afterOpenParen = parenLineContext.Advance(Keywords.TupleOpen.Length);

                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], afterOpenParen);

                var itemContext = afterFirst;
                var previousRow = explicitList.Nodes[0].Range.Start.Row;

                var restNodes = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<TopLevelExpose> Node)>();

                for (var i = 1; i < explicitList.Nodes.Count; i++)
                {
                    var node = explicitList.Nodes[i];
                    var nodeRow = node.Range.Start.Row;

                    // Check if this item is on a different row than the previous item in the ORIGINAL source
                    // For well-formatted lists, preserve groupings. For malformed lists, break everything.
                    if (!isWellFormatted || nodeRow != previousRow)
                    {
                        // Item is on a new line - format with comma at start of new line
                        itemContext = itemContext.NextRowToIndent();
                        var commaLocation = itemContext.CurrentLocation();
                        var afterComma = itemContext.Advance(Keywords.Comma.Length);
                        var (formattedNode, nextContext) = FormatTopLevelExpose(node, afterComma);
                        restNodes.Add((commaLocation, formattedNode));
                        itemContext = nextContext;
                    }
                    else
                    {
                        // Item is on the same line - add comma and item on same line
                        var commaLocation = itemContext.CurrentLocation();
                        var afterComma = itemContext.Advance(Keywords.Comma.Length);
                        var (formattedNode, nextContext) = FormatTopLevelExpose(node, afterComma);
                        restNodes.Add((commaLocation, formattedNode));
                        itemContext = nextContext;
                    }

                    previousRow = nodeRow;
                }

                // Closing paren on its own line
                itemContext = itemContext.NextRowToIndent();
                var closeParenLocation = itemContext.CurrentLocation();
                var afterCloseParen = itemContext.Advance(Keywords.CloseParen.Length);
                var finalContext = afterCloseParen.ReturnToIndent(parentContext);
                var range = MakeRange(context.CurrentLocation(), finalContext.CurrentLocation());
                var nodesList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<TopLevelExpose>>.NonEmpty(firstNode, restNodes);
                return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.Explicit(
                    OpenParenLocation: openParenLocation,
                    Nodes: nodesList,
                    CloseParenLocation: closeParenLocation)), finalContext);
            }
        }

        /// <summary>
        /// Checks if a multiline exposing list is well-formatted.
        /// In elm-format's canonical format, all items on lines after the first
        /// should start at the same column as the first item.
        /// </summary>
        private static bool IsExposingListWellFormatted(Exposing.Explicit explicitList)
        {
            if (explicitList.Nodes.Count < 2)
                return true;

            var firstRow = explicitList.Nodes[0].Range.Start.Row;
            var firstColumn = explicitList.Nodes[0].Range.Start.Column;

            // Group items by row
            var itemsByRow = new Dictionary<int, List<int>>();
            foreach (var node in explicitList.Nodes.Nodes)
            {
                var row = node.Range.Start.Row;
                if (!itemsByRow.ContainsKey(row))
                    itemsByRow[row] = [];
                itemsByRow[row].Add(node.Range.Start.Column);
            }

            // In canonical format, the first item on each row (after the first row)
            // should start at the same column as the first item on the first row
            // (accounting for the leading comma which is at column-2)
            foreach (var kvp in itemsByRow)
            {
                if (kvp.Key == firstRow)
                    continue;

                var minColumnOnRow = kvp.Value.Min();
                // The first item on non-first rows should start at firstColumn
                // (the comma is 2 chars before: ", item")
                if (minColumnOnRow != firstColumn)
                    return false;
            }

            return true;
        }

        private static bool IsExposingListMultiLine(Exposing.Explicit explicitList)
        {
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
                    $"Getting name for expose type '{expose.GetType().Name}' is not implemented.")
            };
        }

        private static (Stil4mElmSyntax7.Node<TopLevelExpose>, FormattingContext) FormatTopLevelExpose(
            Stil4mElmSyntax7.Node<TopLevelExpose> node,
            FormattingContext context)
        {
            var exposeName = GetTopLevelExposeName(node.Value);
            var afterExpose = context.Advance(exposeName.Length);

            return (MakeNodeWithRange(context.CurrentLocation(), afterExpose.CurrentLocation(), node.Value), afterExpose);
        }

        #endregion

        #region Import Formatting

        private static (IReadOnlyList<Stil4mElmSyntax7.Node<Import>>, FormattingContext) FormatImports(
            IReadOnlyList<Stil4mElmSyntax7.Node<Import>> imports,
            FormattingContext context)
        {
            if (!imports.Any())
                return ([], context);

            var sortedImports =
                imports
                .OrderBy(import => string.Join(".", import.Value.ModuleName.Value), System.StringComparer.Ordinal)
                .ToList();

            var formattedImports = new List<Stil4mElmSyntax7.Node<Import>>();
            var currentContext = context;

            foreach (var import in sortedImports)
            {
                var (formattedImport, nextContext) = FormatImport(import, currentContext);
                formattedImports.Add(formattedImport);
                // Reset indent to zero before next import since imports always start at column 1
                currentContext = nextContext.ResetIndent().NextRowToIndent();
            }

            return (formattedImports, currentContext);
        }

        private static (Stil4mElmSyntax7.Node<Import>, FormattingContext) FormatImport(
            Stil4mElmSyntax7.Node<Import> import,
            FormattingContext context)
        {
            var importTokenLoc = context.CurrentLocation();
            var afterImportKeyword = context.Advance(Keywords.Import.Length);

            // Reference context for indented content (one indent level from start of import)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            var moduleName = string.Join(".", import.Value.ModuleName.Value);
            var afterModuleName = afterImportKeyword.Advance(moduleName.Length);

            var currentContext = afterModuleName;

            (Location AsTokenLocation, Stil4mElmSyntax7.Node<IReadOnlyList<string>> Alias)? moduleAlias = null;
            if (import.Value.ModuleAlias is { } alias)
            {
                // Advance for space before "as"
                var afterSpace = currentContext.Advance(Keywords.Space.Length);
                var asTokenLoc = afterSpace.CurrentLocation();
                // Advance past "as " (3 chars: "as" + space after)
                currentContext = afterSpace.Advance(Keywords.AsSpace.Length);
                var aliasName = string.Join(".", alias.Alias.Value);
                var aliasStartLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(aliasName.Length);
                moduleAlias = (asTokenLoc, MakeNodeWithRange(aliasStartLoc, currentContext.CurrentLocation(), alias.Alias.Value));
            }

            (Location ExposingTokenLocation, Stil4mElmSyntax7.Node<Exposing> ExposingList)? exposingList = null;
            if (import.Value.ExposingList is { } exposing)
            {
                // Check if exposing is on a new line in the original
                var exposingOnNewLine = exposing.ExposingTokenLocation.Row > import.Value.ModuleName.Range.End.Row;

                if (exposingOnNewLine)
                {
                    // Exposing on new line with 4-space indentation from start of import
                    currentContext = currentContext.ReturnToIndent(indentedRef).NextRowToIndent();
                    var exposingTokenLoc = currentContext.CurrentLocation();
                    var afterExposing = currentContext.Advance(Keywords.Exposing.Length);
                    var (formattedExposing, afterExposingList) = FormatExposing(exposing.ExposingList, afterExposing);
                    exposingList = (exposingTokenLoc, formattedExposing);
                    currentContext = afterExposingList;
                }
                else
                {
                    // Exposing on same line
                    var exposingTokenLoc = currentContext.Advance(Keywords.Space.Length).CurrentLocation();
                    currentContext = currentContext.Advance(Keywords.SpaceExposingSpace.Length);
                    var exposingTextLength = exposing.ExposingList.Range.End.Column - exposing.ExposingList.Range.Start.Column;
                    currentContext = currentContext.Advance(exposingTextLength);
                    exposingList = (exposingTokenLoc, exposing.ExposingList);
                }
            }

            var range = MakeRange(context.CurrentLocation(), currentContext.CurrentLocation());
            var moduleNameNode = new Stil4mElmSyntax7.Node<IReadOnlyList<string>>(range, import.Value.ModuleName.Value);

            var formattedImport = new Import(
                ImportTokenLocation: importTokenLoc,
                ModuleName: moduleNameNode,
                ModuleAlias: moduleAlias,
                ExposingList: exposingList
            );

            return (new Stil4mElmSyntax7.Node<Import>(range, formattedImport), currentContext);
        }

        #endregion

        #region Declaration Formatting

        /// <summary>
        /// Formats both complete and incomplete declarations, interleaving them by their original positions.
        /// </summary>  
        private (IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>>, FormattingContext, IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>>) FormatDeclarationsWithIncompletes(
            IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>> declarations,
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

            var formattedDeclarations = new List<Stil4mElmSyntax7.Node<Declaration>>();
            var formattedIncompletes = new List<Stil4mElmSyntax7.Node<IncompleteDeclaration>>();
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
                            else if (commentsBetween.Count is not 0)
                            {
                                var firstComment = commentsBetween[0];
                                var isFirstCommentDoc = firstComment.Value.IsDocComment;

                                if (isFirstCommentDoc)
                                {
                                    currentContext = nextContext.WithBlankLine().NextRowToIndent();
                                }
                                else
                                {
                                    currentContext = nextContext.WithBlankLine().WithBlankLine().ResetIndent();
                                }

                                for (var ci = 0; ci < commentsBetween.Count; ci++)
                                {
                                    var comment = commentsBetween[ci];
                                    // Ensure we're at column 1 for top-level comments
                                    currentContext = currentContext.ResetIndent().SetIndentColumn();
                                    currentContext = currentContext.FormatAndAddComment(comment);

                                    // For non-doc section comments:
                                    // - FormatAndAddComment already advances to the next row after the comment
                                    // - Only add 2 blank lines after the LAST section comment (before next declaration)
                                    // - Consecutive section comments stay together (no blank lines between)
                                    if (!comment.Value.IsDocComment)
                                    {
                                        var isLastComment = ci == commentsBetween.Count - 1;
                                        if (isLastComment)
                                        {
                                            // 2 blank lines after the last section comment (before next declaration)
                                            currentContext = currentContext.WithBlankLine();
                                        }
                                        // For non-last comments, don't add any extra spacing
                                        // FormatAndAddComment already put us at the start of the next line
                                    }
                                }
                            }
                            else
                            {
                                currentContext = nextContext.WithBlankLine().NextRowToIndent();
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
                    formattedIncompletes.Add(new Stil4mElmSyntax7.Node<IncompleteDeclaration>(
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

        private FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatDeclaration(
            Stil4mElmSyntax7.Node<Declaration> decl,
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
                    FormatInfixDeclaration(infixDecl, decl.Range, context),

                Declaration.PortDeclaration portDecl =>
                    FormatPortDeclaration(portDecl, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented " +
                    $"at row {decl.Range.Start.Row}, column {decl.Range.Start.Column}.")
            };
        }

        private FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatFunctionDeclaration(
            Declaration.FunctionDeclaration funcDecl,
            Range originalDeclRange,
            FormattingContext context)
        {
            var currentContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            // Format documentation comment if present
            if (funcDecl.Function.Documentation is { } docComment)
            {
                currentContext = currentContext.FormatAndAddComment(docComment);
            }

            // Format signature if present
            Stil4mElmSyntax7.Node<Signature>? formattedSignature = null;
            if (funcDecl.Function.Signature is { } signature)
            {
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);

                // Detect if type annotation should be on a new line after the colon
                var isTypeAnnotOnNewLine = signature.Value.TypeAnnotation.Range.Start.Row > signature.Value.Name.Range.Start.Row;

                var colonLoc = afterSigName.Advance(1).CurrentLocation(); // space before colon
                var afterColon = afterSigName.Advance(2); // " :"

                FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>> typeAnnotResult;
                if (isTypeAnnotOnNewLine)
                {
                    // Type annotation on new line with indentation - no space after colon
                    var typeContext = afterColon.ReturnToIndent(indentedRef).NextRowToIndent();
                    typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);
                }
                else
                {
                    // Type annotation on same line after " : "
                    var sameLineContext = afterColon.Advance(1); // space after colon
                    typeAnnotResult = FormatTypeAnnotation(signature.Value.TypeAnnotation, sameLineContext);
                }

                var sigRange = MakeRange(currentContext.CurrentLocation(), typeAnnotResult.Context.CurrentLocation());
                formattedSignature = new Stil4mElmSyntax7.Node<Signature>(sigRange, new Signature(
                    Name: new Stil4mElmSyntax7.Node<string>(
                        MakeRange(currentContext.CurrentLocation(), afterSigName.CurrentLocation()),
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
            var formattedArguments = new List<Stil4mElmSyntax7.Node<Pattern>>();
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
            exprContext = exprContext.FormatAndAddComments(commentsBeforeExpr);

            // Format the expression with updated locations
            var exprResult = FormatExpression(impl.Expression, exprContext);

            // Build formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: MakeNodeWithRange(funcNameStartLoc, afterName.CurrentLocation(), impl.Name.Value),
                Arguments: formattedArguments,
                EqualsTokenLocation: equalsLoc,
                Expression: exprResult.FormattedNode
            );

            var formattedFunc = new FunctionStruct(
                Documentation: funcDecl.Function.Documentation,
                Signature: formattedSignature,
                Declaration: new Stil4mElmSyntax7.Node<FunctionImplementation>(
                    MakeRange(currentContext.CurrentLocation(), exprResult.Context.CurrentLocation()),
                    formattedImpl)
            );

            var range = MakeRange(context.CurrentLocation(), exprResult.Context.CurrentLocation());
            return FormattingResult<Stil4mElmSyntax7.Node<Declaration>>.Create(
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.FunctionDeclaration(formattedFunc)),
                exprResult.Context.ReturnToIndent(context));
        }

        private FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context)
        {
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            if (aliasDecl.TypeAlias.Documentation is { } docComment)
            {
                startContext = startContext.FormatAndAddComment(docComment);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(Keywords.Type.Length); // "type " - includes trailing space
            var aliasTokenLoc = afterType.CurrentLocation();
            var afterTypeAlias = afterType.Advance(Keywords.Alias.Length); // "alias " - includes trailing space

            var aliasName = aliasDecl.TypeAlias.Name.Value;
            var nameStartLoc = afterTypeAlias.CurrentLocation();
            var afterName = afterTypeAlias.Advance(aliasName.Length);
            var formattedName = MakeNodeWithRange(nameStartLoc, afterName.CurrentLocation(), aliasName);

            var currentContext = afterName;
            var formattedGenerics = new List<Stil4mElmSyntax7.Node<string>>();
            foreach (var generic in aliasDecl.TypeAlias.Generics)
            {
                currentContext = currentContext.Advance(Keywords.Space.Length);
                var genericStartLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(generic.Value.Length);
                formattedGenerics.Add(MakeNodeWithRange(genericStartLoc, currentContext.CurrentLocation(), generic.Value));
            }

            // " =" space before equals
            var afterNameSpace = currentContext.Advance(Keywords.Space.Length);
            var equalsLoc = afterNameSpace.CurrentLocation();
            var afterEquals = afterNameSpace.Advance(Keywords.EqualsSign.Length); // just "="

            // Save parent context for later indent restoration
            var parentContext = afterEquals;
            var typeContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

            // Check for comments between the equals sign and the type annotation
            var commentsBeforeType = commentQueries.GetBetweenRows(
                aliasDecl.TypeAlias.EqualsTokenLocation.Row,
                aliasDecl.TypeAlias.TypeAnnotation.Range.Start.Row);

            foreach (var comment in commentsBeforeType)
            {
                typeContext = typeContext.FormatAndAddComment(comment);
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
            return FormattingResult<Stil4mElmSyntax7.Node<Declaration>>.Create(
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.AliasDeclaration(formattedTypeAlias)),
                finalContext);
        }

        private FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatCustomTypeDeclaration(
            Declaration.CustomTypeDeclaration customTypeDecl,
            FormattingContext context)
        {
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            if (customTypeDecl.TypeDeclaration.Documentation is { } docComment)
            {
                startContext = startContext.FormatAndAddComment(docComment);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(Keywords.Type.Length);

            var typeName = customTypeDecl.TypeDeclaration.Name.Value;
            var afterName = afterType.Advance(typeName.Length);

            var currentContext = afterName;
            var formattedGenerics = new List<Stil4mElmSyntax7.Node<string>>();
            foreach (var generic in customTypeDecl.TypeDeclaration.Generics)
            {
                currentContext = currentContext.Advance(Keywords.Space.Length);
                var genericLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(generic.Value.Length);
                formattedGenerics.Add(MakeNodeWithRange(genericLoc, currentContext.CurrentLocation(), generic.Value));
            }

            // Move to next line for constructors
            var constructorIndentContext = currentContext.ReturnToIndent(indentedRef).NextRowToIndent();

            var equalsLoc = constructorIndentContext.CurrentLocation();
            var afterEquals = constructorIndentContext.Advance(Keywords.EqualsSpace.Length);

            // Format constructors
            var formattedConstructors = new List<(Location? PipeLocation, Stil4mElmSyntax7.Node<ValueConstructor> Constructor)>();
            var constructorCtx = afterEquals;

            for (var i = 0; i < customTypeDecl.TypeDeclaration.Constructors.Count; i++)
            {
                var (pipeLocation, constructor) = customTypeDecl.TypeDeclaration.Constructors[i];

                Location? formattedPipeLoc = null;
                if (i > 0)
                {
                    // Check for comments between this and previous constructor
                    var prevConstructor = customTypeDecl.TypeDeclaration.Constructors[i - 1].Constructor;
                    var commentsBetweenConstructors = commentQueries.GetBetweenRanges(prevConstructor.Range, constructor.Range).ToList();

                    if (commentsBetweenConstructors.Count is not 0)
                    {
                        // Format comments between constructors
                        // Comment gets extra 2 space indent (6 spaces total = 4 base indent + 2 extra)
                        var commentIndentRef = constructorIndentContext.SetIndentColumn().Advance(2).SetIndentToCurrentColumn();
                        foreach (var comment in commentsBetweenConstructors)
                        {
                            constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef).NextRowToIndent();
                            constructorCtx = constructorCtx.FormatAndAddComment(comment);
                            // Don't add extra blank lines after constructor comment
                            constructorCtx = constructorCtx.ReturnToIndent(constructorIndentContext);
                        }
                        // FormatAndAddComment already positioned us on the next row, just set indent
                        constructorCtx = constructorCtx.SetIndentColumn();
                    }
                    else
                    {
                        // Move to next line for subsequent constructors (only when no comments)
                        constructorCtx = constructorCtx.NextRowToIndent();
                    }
                    formattedPipeLoc = constructorCtx.CurrentLocation();
                    constructorCtx = constructorCtx.Advance(2); // "| "
                }

                // Constructor name
                var constructorStartLoc = constructorCtx.CurrentLocation();
                var afterConstructorName = constructorCtx.Advance(constructor.Value.Name.Value.Length);

                // Create reference context for argument indentation (2 spaces from constructor name)
                var argIndentRef = constructorCtx.Advance(2).SetIndentToCurrentColumn();

                // Constructor arguments
                var formattedArgs = new List<Stil4mElmSyntax7.Node<TypeAnnotation>>();
                var argCtx = afterConstructorName;
                for (var argIndex = 0; argIndex < constructor.Value.Arguments.Count; argIndex++)
                {
                    var arg = constructor.Value.Arguments[argIndex];

                    // Determine where to look for comments based on whether this is the first argument
                    int searchStartRow;
                    if (argIndex is 0)
                    {
                        // For first argument, search from constructor name row
                        searchStartRow = constructor.Value.Name.Range.End.Row;
                    }
                    else
                    {
                        // For subsequent arguments, search from previous argument's end
                        searchStartRow = constructor.Value.Arguments[argIndex - 1].Range.End.Row;
                    }

                    var commentsBetween = commentQueries.GetBetweenRows(searchStartRow, arg.Range.Start.Row).ToList();

                    if (commentsBetween.Count > 0)
                    {
                        // Arguments with comments between them should be on separate lines
                        foreach (var comment in commentsBetween)
                        {
                            argCtx = argCtx.ReturnToIndent(argIndentRef).NextRowToIndent();
                            argCtx = argCtx.FormatAndAddComment(comment);
                        }

                        // FormatAndAddComment already positioned us on the next row, just set indent
                        argCtx = argCtx.ReturnToIndent(argIndentRef).SetIndentColumn();
                        var argResult = FormatTypeAnnotation(arg, argCtx);
                        formattedArgs.Add(argResult.FormattedNode);
                        argCtx = argResult.Context;
                        continue;
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

                var formattedConstructor = new ValueConstructor(
                    MakeNodeWithRange(
                        constructorStartLoc,
                        afterConstructorName.CurrentLocation(),
                        constructor.Value.Name.Value),
                    formattedArgs);

                formattedConstructors.Add(
                    (formattedPipeLoc, MakeNodeWithRange(constructorStartLoc, argCtx.CurrentLocation(), formattedConstructor)));

                // Restore constructor-level indent after formatting arguments
                constructorCtx = argCtx.ReturnToIndent(constructorIndentContext);
            }

            var formattedTypeStruct = new TypeStruct(
                Documentation: customTypeDecl.TypeDeclaration.Documentation,
                TypeTokenLocation: typeTokenLoc,
                Name: MakeNodeWithRange(afterType.CurrentLocation(), afterName.CurrentLocation(), typeName),
                Generics: formattedGenerics,
                EqualsTokenLocation: equalsLoc,
                Constructors: formattedConstructors
            );

            var finalContext = constructorCtx.ReturnToIndent(startContext);
            var range = MakeRange(startContext.CurrentLocation(), constructorCtx.CurrentLocation());
            return FormattingResult<Stil4mElmSyntax7.Node<Declaration>>.Create(
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.CustomTypeDeclaration(formattedTypeStruct)),
                finalContext);
        }

        private static FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatInfixDeclaration(
            Declaration.InfixDeclaration infixDecl,
            Range originalRange,
            FormattingContext context)
        {
            var infixTokenLoc = context.CurrentLocation();

            // Simplified: just preserve the original length
            var lineLength = originalRange.End.Column - originalRange.Start.Column;
            var afterInfix = context.Advance(lineLength);

            var range = MakeRange(context.CurrentLocation(), afterInfix.CurrentLocation());
            var formattedInfix = new Infix(
                InfixTokenLocation: infixTokenLoc,
                Direction: infixDecl.Infix.Direction,
                Precedence: infixDecl.Infix.Precedence,
                Operator: infixDecl.Infix.Operator,
                EqualsTokenLocation: infixDecl.Infix.EqualsTokenLocation,
                FunctionName: infixDecl.Infix.FunctionName
            );

            return FormattingResult<Stil4mElmSyntax7.Node<Declaration>>.Create(
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.InfixDeclaration(formattedInfix)),
                afterInfix);
        }

        private static FormattingResult<Stil4mElmSyntax7.Node<Declaration>> FormatPortDeclaration(
            Declaration.PortDeclaration portDecl,
            FormattingContext context)
        {
            var portTokenLoc = context.CurrentLocation();
            var afterPort = context.Advance(5); // "port "

            var sigName = portDecl.Signature.Name.Value;
            var afterSigName = afterPort.Advance(sigName.Length);
            var colonLoc = afterSigName.CurrentLocation();
            var afterColon = afterSigName.Advance(3); // " : "

            // Simplified type annotation
            var typeAnnotLength =
                portDecl.Signature.TypeAnnotation.Range.End.Column - portDecl.Signature.TypeAnnotation.Range.Start.Column;

            var afterType = afterColon.Advance(typeAnnotLength);

            var formattedSig =
                new Signature(
                    Name: MakeNodeWithRange(
                        afterPort.CurrentLocation(),
                        afterSigName.CurrentLocation(),
                        sigName),
                ColonLocation: colonLoc,
                TypeAnnotation: portDecl.Signature.TypeAnnotation
            );

            var range =
                MakeRange(context.CurrentLocation(), afterType.CurrentLocation());

            return FormattingResult<Stil4mElmSyntax7.Node<Declaration>>.Create(
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.PortDeclaration(portTokenLoc, formattedSig)),
                afterType);
        }

        #endregion

        #region Type Annotation Formatting

        private FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>> FormatTypeAnnotation(
            Stil4mElmSyntax7.Node<TypeAnnotation> typeAnnot,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null)
        {
            var startLoc = context.CurrentLocation();
            var typeResult = FormatTypeAnnotationValue(typeAnnot.Value, context, arrowBaseRef);
            return FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>>.Create(
                MakeNodeWithRange(startLoc, typeResult.Context.CurrentLocation(), typeResult.FormattedNode),
                typeResult.Context);
        }

        private FormattingResult<TypeAnnotation> FormatTypeAnnotationValue(
            TypeAnnotation typeAnnot,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null)
        {
            // Reference context for indented content (one indent level from current position)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            switch (typeAnnot)
            {
                case TypeAnnotation.GenericType genericType:
                    return FormattingResult<TypeAnnotation>.Create(genericType, context.Advance(genericType.Name.Length));

                case TypeAnnotation.Typed typed:
                    {
                        var typeName = typed.TypeName.Value;
                        var typeNameText = typeName.ModuleName.Count > 0
                            ? string.Join(".", typeName.ModuleName) + "." + typeName.Name
                            : typeName.Name;
                        var afterTypeName = context.Advance(typeNameText.Length);

                        var formattedArgs = new List<Stil4mElmSyntax7.Node<TypeAnnotation>>();
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
                        var tupledElements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupled.TypeAnnotations);
                        var openParenLoc = context.CurrentLocation();

                        // Detect if tuple should be multiline based on the containing node's start and end locations
                        var isMultilineTuple = tupled.CloseParenLocation.Row > tupled.OpenParenLocation.Row;

                        if (isMultilineTuple)
                        {
                            // Multiline format:
                            // ( Element1
                            // , Element2
                            // )
                            var afterOpenParen = context.Advance(2); // "( "

                            Stil4mElmSyntax7.Node<TypeAnnotation>? firstElem = null;
                            var restElems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<TypeAnnotation> Node)>();
                            var tupledCtx = afterOpenParen;

                            // Create reference context at opening paren column for alignment
                            var parenAlignRef = context.SetIndentToCurrentColumn();

                            for (var i = 0; i < tupledElements.Count; i++)
                            {
                                var elem = tupledElements[i];
                                Location? separatorLoc = null;

                                if (i > 0)
                                {
                                    // Move to new line, align comma with opening paren
                                    tupledCtx = tupledCtx.ReturnToIndent(parenAlignRef).NextRowToIndent();
                                    separatorLoc = tupledCtx.CurrentLocation();
                                    tupledCtx = tupledCtx.Advance(2); // ", "
                                }

                                var elemResult = FormatTypeAnnotation(elem, tupledCtx);

                                if (!separatorLoc.HasValue)
                                {
                                    firstElem = elemResult.FormattedNode;
                                }
                                else
                                {
                                    restElems.Add((separatorLoc.Value, elemResult.FormattedNode));
                                }

                                tupledCtx = elemResult.Context.ReturnToIndent(context);
                            }

                            // Closing paren on new line, aligned with opening paren
                            var closeCtx = tupledCtx.ReturnToIndent(parenAlignRef).NextRowToIndent();
                            var closeParenLoc = closeCtx.CurrentLocation();
                            var afterCloseParen = closeCtx.Advance(1); // ")"

                            SeparatedSyntaxList<Stil4mElmSyntax7.Node<TypeAnnotation>> separatedElems;
                            if (firstElem is not null)
                            {
                                separatedElems = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<TypeAnnotation>>.NonEmpty(firstElem, restElems);
                            }
                            else
                            {
                                separatedElems = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<TypeAnnotation>>.Empty();
                            }

                            var formattedTupledAnnot = new TypeAnnotation.Tupled(
                                openParenLoc,
                                separatedElems,
                                closeParenLoc
                            );
                            return FormattingResult<TypeAnnotation>.Create(formattedTupledAnnot, afterCloseParen.ReturnToIndent(context));
                        }
                        else
                        {
                            // Single line format
                            // For single-element (grouping parens): (Element)
                            // For tuples (2+ elements): ( Element1, Element2 )
                            var isSingleElement = tupledElements.Count is 1;

                            var afterOpenParen = isSingleElement
                                ? context.Advance(1)  // "("
                                : context.Advance(2); // "( "

                            var formattedTupled = new List<Stil4mElmSyntax7.Node<TypeAnnotation>>();
                            var tupledCtx = afterOpenParen;

                            for (var i = 0; i < tupledElements.Count; i++)
                            {
                                if (i > 0)
                                {
                                    tupledCtx = tupledCtx.Advance(2); // ", "
                                }

                                var elemResult =
                                    FormatTypeAnnotation(tupledElements[i], tupledCtx);

                                formattedTupled.Add(elemResult.FormattedNode);
                                tupledCtx = elemResult.Context;
                            }

                            Location closeParenLoc;
                            FormattingContext afterCloseParen;

                            if (isSingleElement)
                            {
                                closeParenLoc = tupledCtx.CurrentLocation();
                                afterCloseParen = tupledCtx.Advance(1); // ")"
                            }
                            else
                            {
                                closeParenLoc = tupledCtx.Advance(1).CurrentLocation(); // " )"
                                afterCloseParen = tupledCtx.Advance(2);
                            }

                            var formattedTupledAnnot = new TypeAnnotation.Tupled(
                                openParenLoc,
                                ToSeparatedSyntaxList(formattedTupled),
                                closeParenLoc
                            );
                            return FormattingResult<TypeAnnotation>.Create(formattedTupledAnnot, afterCloseParen);
                        }
                    }

                case TypeAnnotation.FunctionTypeAnnotation funcType:
                    {
                        // Check if the function type is multiline based on original layout
                        var returnTypeOnNewLine = funcType.ReturnType.Range.Start.Row > funcType.ArgumentType.Range.Start.Row;

                        // Check if the return type is on a separate line from the arrow
                        // This happens when -> is on its own line
                        var resultTypeOnOwnLine = funcType.ReturnType.Range.Start.Row > funcType.ArrowLocation.Row;

                        // Use passed reference or create one at current column if first encounter
                        var effectiveArrowBaseRef = arrowBaseRef ?? context.SetIndentToCurrentColumn();

                        var argTypeResult = FormatTypeAnnotation(funcType.ArgumentType, context, effectiveArrowBaseRef);

                        Location arrowLocation;
                        FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>> returnTypeResult;

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
                                var resultTypeRef = effectiveArrowBaseRef.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var resultTypeCtx = afterArrow.ReturnToIndent(resultTypeRef).NextRowToIndent();
                                // Pass effectiveArrowBaseRef so nested function type arrows align properly
                                returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, resultTypeCtx, effectiveArrowBaseRef);
                            }
                            else
                            {
                                // Arrow on new line, but result type on same line as arrow
                                var arrowCtx = newLineCtx.Advance(3); // "-> "
                                returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, arrowCtx, effectiveArrowBaseRef);
                            }
                        }
                        else
                        {
                            // Single line: " -> "
                            var arrowCtx = argTypeResult.Context.Advance(1); // space before arrow
                            arrowLocation = arrowCtx.CurrentLocation();
                            var afterArrow = arrowCtx.Advance(3); // "-> "
                            returnTypeResult = FormatTypeAnnotation(funcType.ReturnType, afterArrow, effectiveArrowBaseRef);
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
                        var recordFields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields);

                        // Detect if record should be multiline based on the containing node's start and end locations
                        var isMultilineRecord = record.CloseBraceLocation.Row > record.OpenBraceLocation.Row;

                        var recordOpenBraceLoc = context.CurrentLocation();

                        if (isMultilineRecord)
                        {
                            // Multiline format:
                            // { fieldA : Type1
                            // , fieldB : Type2
                            // }
                            var afterRecordOpenBrace = context.Advance(2); // "{ "

                            Stil4mElmSyntax7.Node<RecordField>? firstField = null;
                            var restFields = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<RecordField> Node)>();
                            var recordFieldCtx = afterRecordOpenBrace;

                            // Create reference context at opening brace column for alignment
                            var braceAlignRef = context.SetIndentToCurrentColumn();
                            // Create reference context at brace + 2 for field indent
                            var fieldIndentRef = context.Advance(2).SetIndentToCurrentColumn();
                            // Create reference context at brace + 4 for field type indent
                            var fieldTypeIndentRef = context.Advance(4).SetIndentToCurrentColumn();

                            // Handle comments right after the opening brace but before the first field
                            if (recordFields.Count > 0)
                            {
                                var firstFieldOriginal = recordFields[0];
                                var commentsAfterBrace = commentQueries.GetAfterLocationBeforeRow(
                                    record.OpenBraceLocation, firstFieldOriginal.Range.Start.Row);

                                foreach (var comment in commentsAfterBrace)
                                {
                                    // If comment is on same row as opening brace, put it right after "{ "
                                    if (comment.Range.Start.Row == record.OpenBraceLocation.Row)
                                    {
                                        // Format the comment, add it to the context, and position at field indent
                                        recordFieldCtx = recordFieldCtx.ReturnToIndent(fieldIndentRef).FormatAndAddComment(comment);
                                    }
                                    else
                                    {
                                        // Comment on its own line before first field
                                        recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                        // Format the comment, add it to the context, and stay at the comment end position
                                        recordFieldCtx = recordFieldCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                                    }
                                }
                            }

                            for (var i = 0; i < recordFields.Count; i++)
                            {
                                var field = recordFields[i];
                                Location? separatorLoc = null;

                                if (i > 0)
                                {
                                    // Get the original separator location from the SeparatedSyntaxList
                                    Location? originalSeparatorLoc = null;
                                    if (record.RecordDefinition.Fields is SeparatedSyntaxList<Stil4mElmSyntax7.Node<RecordField>>.NonEmpty nonEmpty)
                                    {
                                        if (i - 1 < nonEmpty.Rest.Count)
                                        {
                                            originalSeparatorLoc = nonEmpty.Rest[i - 1].SeparatorLocation;
                                        }
                                    }

                                    // Check for comments between this field and the previous one
                                    var prevField = recordFields[i - 1];
                                    // Use the field type's end position instead of the whole field node's range
                                    // because the field node's range may incorrectly extend to the next field's separator
                                    var prevFieldEnd = prevField.Value.FieldType.Range.End;

                                    // Get all comments between fields
                                    var allCommentsBetween = commentQueries.GetBetweenRows(prevFieldEnd.Row, field.Range.Start.Row).ToList();

                                    // Separate comments into those before the comma row and those on/after the comma row
                                    var separatorRow = originalSeparatorLoc?.Row ?? (prevFieldEnd.Row + 1);
                                    var separatorColumn = originalSeparatorLoc?.Column ?? 1;
                                    var commentsBeforeSeparator = allCommentsBetween
                                        .Where(c => c.Range.Start.Row < separatorRow)
                                        .ToList();
                                    var commentsOnSeparatorRow = allCommentsBetween
                                        .Where(c => c.Range.Start.Row == separatorRow &&
                                                    originalSeparatorLoc is not null &&
                                                    c.Range.Start.Column > separatorColumn)
                                        .ToList();
                                    var commentsAfterSeparatorRow = allCommentsBetween
                                        .Where(c => c.Range.Start.Row > separatorRow)
                                        .ToList();

                                    // Process comments before the separator (comma)
                                    if (commentsBeforeSeparator.Count > 0)
                                    {
                                        // Check if there's a blank line before the first comment
                                        var hasBlankLineBeforeComment = commentsBeforeSeparator[0].Range.Start.Row > prevFieldEnd.Row + 1;

                                        if (hasBlankLineBeforeComment)
                                        {
                                            // Blank line before comments
                                            recordFieldCtx = recordFieldCtx.NextRowToIndent();
                                        }

                                        // Add each comment on its own line
                                        foreach (var comment in commentsBeforeSeparator)
                                        {
                                            recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                            // Format the comment, add it to the context, and stay at the comment end position
                                            recordFieldCtx = recordFieldCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                                        }
                                    }

                                    // Move to new line, align comma with opening brace
                                    recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                    separatorLoc = recordFieldCtx.CurrentLocation();
                                    recordFieldCtx = recordFieldCtx.Advance(2); // ", "

                                    // Process comments on the same row as comma (after comma)
                                    if (commentsOnSeparatorRow.Count > 0)
                                    {
                                        foreach (var comment in commentsOnSeparatorRow)
                                        {
                                            // Format the comment, add it to the context, and position at field indent
                                            recordFieldCtx = recordFieldCtx.ReturnToIndent(fieldIndentRef).FormatAndAddComment(comment);
                                        }
                                    }

                                    // Process comments after separator row but before field
                                    foreach (var comment in commentsAfterSeparatorRow)
                                    {
                                        recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                        // Format the comment, add it to the context, and stay at the comment end position
                                        recordFieldCtx = recordFieldCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                                    }
                                }

                                var fieldStartLoc = recordFieldCtx.CurrentLocation();
                                var afterFieldName = recordFieldCtx.Advance(field.Value.FieldName.Value.Length);

                                // Check if colon is on a different line than field name
                                var isColonOnNewLine = field.Value.ColonLocation.Row > field.Value.FieldName.Range.End.Row;

                                // Check for comments between field name and colon
                                var commentsBeforeColon = commentQueries.GetBetweenRowsInclusiveEndWithColumnFilter(
                                    field.Value.FieldName.Range.End.Row,
                                    field.Value.ColonLocation.Row,
                                    field.Value.ColonLocation.Column);

                                FormattingContext colonContext;
                                Location colonLoc;
                                FormattingContext afterColon;

                                if (isColonOnNewLine || commentsBeforeColon.Count > 0)
                                {
                                    // Handle comments between field name and colon
                                    var ctx = afterFieldName;
                                    foreach (var comment in commentsBeforeColon)
                                    {
                                        ctx = ctx.ReturnToIndent(fieldIndentRef).NextRowToIndent(); // indent after brace
                                        // Format the comment, add it to the context, and stay at the comment end position
                                        ctx = ctx.FormatAndAddCommentThenPositionAtEnd(comment);
                                    }
                                    // Colon on new line, indented
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

                                // Check if field type should be on a new line
                                var isFieldTypeMultiline = field.Value.FieldType.Range.Start.Row > field.Value.ColonLocation.Row;

                                // Check for comments between colon and type (on separate lines)
                                var commentsAfterColon = commentQueries.GetBetweenRows(
                                    field.Value.ColonLocation.Row,
                                    field.Value.FieldType.Range.Start.Row);

                                // Check for comments on the same line as the type, BEFORE the type
                                var commentsOnSameLineAsType = commentQueries.GetOnRowBeforeColumnAfterRow(
                                    field.Value.FieldType.Range.Start.Row,
                                    field.Value.FieldType.Range.Start.Column,
                                    field.Value.ColonLocation.Row);

                                FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>> fieldTypeResult;

                                if (isFieldTypeMultiline || commentsAfterColon.Count > 0)
                                {
                                    var ctx = afterColon;

                                    foreach (var comment in commentsAfterColon)
                                    {
                                        ctx = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent(); // indent for type
                                        // Format the comment, add it to the context, and stay at the comment end position
                                        ctx = ctx.FormatAndAddCommentThenPositionAtEnd(comment);
                                    }

                                    // Field type on new line with extra indentation
                                    var fieldTypeContext = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent(); // Set proper indent

                                    // Handle inline comments that appear on the same line as the type
                                    foreach (var comment in commentsOnSameLineAsType)
                                    {
                                        // Format the comment, add it to the context, and advance position for space after comment
                                        fieldTypeContext = fieldTypeContext.FormatAndAddCommentThenPositionAtEnd(comment).Advance(1);
                                    }

                                    fieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, fieldTypeContext);
                                }
                                else
                                {
                                    fieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, afterColon);
                                }

                                recordFieldCtx = fieldTypeResult.Context;

                                var formattedFieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

                                var formattedField = new RecordField(
                                    formattedFieldNameNode,
                                    colonLoc,
                                    fieldTypeResult.FormattedNode
                                );

                                var formattedFieldNode = MakeNodeWithRange(fieldStartLoc, fieldTypeResult.Context.CurrentLocation(), formattedField);

                                if (!separatorLoc.HasValue)
                                {
                                    firstField = formattedFieldNode;
                                }
                                else
                                {
                                    restFields.Add((separatorLoc.Value, formattedFieldNode));
                                }

                                recordFieldCtx = fieldTypeResult.Context.ReturnToIndent(context);
                            }

                            // Check for comments before the closing brace
                            var lastField = recordFields.Count > 0 ? recordFields[recordFields.Count - 1] : null;
                            var commentsBeforeCloseBrace = lastField is not null
                                ? commentQueries.GetBetweenRows(lastField.Value.FieldType.Range.End.Row, record.CloseBraceLocation.Row).ToList()
                                : [];

                            if (commentsBeforeCloseBrace.Count > 0)
                            {
                                // Check if there's a blank line before the first comment
                                var hasBlankLine = lastField is not null &&
                                    commentsBeforeCloseBrace[0].Range.Start.Row > lastField.Value.FieldType.Range.End.Row + 1;

                                if (hasBlankLine)
                                {
                                    recordFieldCtx = recordFieldCtx.NextRowToIndent(); // blank line
                                }

                                foreach (var comment in commentsBeforeCloseBrace)
                                {
                                    recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                    // Format the comment, add it to the context, and stay at the comment end position
                                    recordFieldCtx = recordFieldCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                                }
                            }

                            // Closing brace on new line, aligned with opening brace
                            var closeCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                            var recordCloseBraceLoc = closeCtx.CurrentLocation();
                            var afterRecordCloseBrace = closeCtx.Advance(1); // "}"

                            SeparatedSyntaxList<Stil4mElmSyntax7.Node<RecordField>> separatedFields;
                            if (firstField is not null)
                            {
                                separatedFields = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<RecordField>>.NonEmpty(firstField, restFields);
                            }
                            else
                            {
                                separatedFields = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<RecordField>>.Empty();
                            }

                            var formattedRecordDef = new RecordDefinition(separatedFields);
                            var formattedRecord = new TypeAnnotation.Record(
                                recordOpenBraceLoc,
                                formattedRecordDef,
                                recordCloseBraceLoc
                            );
                            return FormattingResult<TypeAnnotation>.Create(formattedRecord, afterRecordCloseBrace.ReturnToIndent(context));
                        }
                        else
                        {
                            // Handle empty record as special case: {}
                            if (recordFields.Count is 0)
                            {
                                var recordCloseBraceLocEmpty = context.Advance(1).CurrentLocation(); // "}" is at position after "{"
                                var afterEmptyRecord = context.Advance(2); // After both "{" and "}"

                                var emptyRecordDef = new RecordDefinition(new SeparatedSyntaxList<Stil4mElmSyntax7.Node<RecordField>>.Empty());
                                var emptyRecordResult = new TypeAnnotation.Record(
                                    recordOpenBraceLoc,
                                    emptyRecordDef,
                                    recordCloseBraceLocEmpty
                                );
                                return FormattingResult<TypeAnnotation>.Create(emptyRecordResult, afterEmptyRecord);
                            }

                            // Single line format: { field1 : Type1, field2 : Type2 }
                            var afterRecordOpenBrace = context.Advance(2); // "{ "

                            var formattedRecordFields = new List<Stil4mElmSyntax7.Node<RecordField>>();
                            var recordFieldCtx = afterRecordOpenBrace;

                            for (var i = 0; i < recordFields.Count; i++)
                            {
                                if (i > 0)
                                {
                                    recordFieldCtx = recordFieldCtx.Advance(2); // ", "
                                }
                                var field = recordFields[i];
                                var fieldStartLoc = recordFieldCtx.CurrentLocation();
                                var afterFieldName = recordFieldCtx.Advance(field.Value.FieldName.Value.Length);
                                var colonLoc = afterFieldName.Advance(1).CurrentLocation(); // space before colon
                                var afterColon = afterFieldName.Advance(3); // " : "

                                var fieldTypeResult = FormatTypeAnnotation(field.Value.FieldType, afterColon);

                                var formattedFieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

                                var formattedField = new RecordField(
                                    formattedFieldNameNode,
                                    colonLoc,
                                    fieldTypeResult.FormattedNode
                                );

                                formattedRecordFields.Add(MakeNodeWithRange(fieldStartLoc, fieldTypeResult.Context.CurrentLocation(), formattedField));

                                recordFieldCtx = fieldTypeResult.Context;
                            }

                            var recordCloseBraceLoc = recordFieldCtx.Advance(1).CurrentLocation(); // " }"
                            var afterRecordCloseBrace = recordFieldCtx.Advance(2);

                            var formattedRecordDef = new RecordDefinition(ToSeparatedSyntaxList(formattedRecordFields));
                            var formattedRecord = new TypeAnnotation.Record(
                                recordOpenBraceLoc,
                                formattedRecordDef,
                                recordCloseBraceLoc
                            );
                            return FormattingResult<TypeAnnotation>.Create(formattedRecord, afterRecordCloseBrace);
                        }
                    }

                case TypeAnnotation.GenericRecord genericRecord:
                    {
                        var genericRecordText = RenderTypeAnnotationText(typeAnnot);
                        return FormattingResult<TypeAnnotation>.Create(genericRecord, context.Advance(genericRecordText.Length));
                    }

                default:
                    throw new System.NotImplementedException(
                        $"Type annotation formatting not implemented for: {typeAnnot.GetType().Name}");
            }
        }

        private static SeparatedSyntaxList<T> ToSeparatedSyntaxList<T>(IReadOnlyList<T> items)
        {
            if (items.Count is 0)
                return new SeparatedSyntaxList<T>.Empty();

            var rest = new List<(Location SeparatorLocation, T Node)>();
            for (var i = 1; i < items.Count; i++)
            {
                rest.Add((new Location(1, 1), items[i]));
            }
            return new SeparatedSyntaxList<T>.NonEmpty(items[0], rest);
        }

        private static string RenderTypeAnnotationText(TypeAnnotation typeAnnot)
        {
            return typeAnnot switch
            {
                TypeAnnotation.GenericType genericType => genericType.Name,
                TypeAnnotation.Typed typed =>
                    (typed.TypeName.Value.ModuleName.Count > 0
                        ? string.Join(".", typed.TypeName.Value.ModuleName) + "." + typed.TypeName.Value.Name
                        : typed.TypeName.Value.Name) +
                    (typed.TypeArguments.Count > 0
                        ? " " + string.Join(" ", typed.TypeArguments.Select(a => RenderTypeAnnotationText(a.Value)))
                        : ""),
                TypeAnnotation.Unit => "()",
                TypeAnnotation.Tupled tupled =>
                    "( " + string.Join(", ", Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupled.TypeAnnotations).Select(t => RenderTypeAnnotationText(t.Value))) + " )",
                TypeAnnotation.FunctionTypeAnnotation funcType =>
                    RenderTypeAnnotationText(funcType.ArgumentType.Value) + " -> " + RenderTypeAnnotationText(funcType.ReturnType.Value),
                TypeAnnotation.Record record =>
                    Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields).Count is 0
                        ? "{}"
                        : "{ " + string.Join(", ", Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields).Select(f => f.Value.FieldName.Value + " : " + RenderTypeAnnotationText(f.Value.FieldType.Value))) + " }",
                TypeAnnotation.GenericRecord genericRecord =>
                    "{ " + genericRecord.GenericName.Value + " | " +
                    string.Join(", ", Stil4mElmSyntax7.FromStil4mConcretized.ToList(genericRecord.RecordDefinition.Value.Fields).Select(f => f.Value.FieldName.Value + " : " + RenderTypeAnnotationText(f.Value.FieldType.Value))) + " }",

                _ =>
                throw new System.NotImplementedException(
                    $"Type annotation text rendering not implemented for: {typeAnnot.GetType().Name}")
            };
        }

        #endregion

        #region Expression Formatting

        private FormattingResult<Stil4mElmSyntax7.Node<Expression>> FormatExpression(
            Stil4mElmSyntax7.Node<Expression> expr,
            FormattingContext context)
        {
            var startLoc = context.CurrentLocation();
            var result = FormatExpressionValue(expr.Value, expr.Range, context);
            return FormattingResult<Stil4mElmSyntax7.Node<Expression>>.Create(
                MakeNodeWithRange(startLoc, result.Context.CurrentLocation(), result.FormattedNode),
                result.Context);
        }

        private FormattingResult<Expression> FormatExpressionValue(
            Expression expr,
            Range originalRange,
            FormattingContext context)
        {
            switch (expr)
            {
                case Expression.UnitExpr:
                    return FormattingResult<Expression>.Create(expr, context.Advance(2)); // "()"

                case Expression.Literal literal:
                    if (literal.IsTripleQuoted)
                    {
                        // For triple-quoted strings, we need to track row changes from embedded newlines
                        var afterOpenQuotes = context.Advance(3); // """
                        var literalCtx = afterOpenQuotes;

                        // Process the string content to track position through newlines
                        foreach (var ch in literal.Value)
                        {
                            if (ch is '\n')
                            {
                                literalCtx = literalCtx.ResetIndent().NextRowToIndent();
                            }
                            else
                            {
                                literalCtx = literalCtx.Advance(1);
                            }
                        }

                        var afterCloseQuotes = literalCtx.Advance(3); // """
                        return FormattingResult<Expression>.Create(literal, afterCloseQuotes);
                    }
                    else
                    {
                        // Use the rendered representation to calculate the correct length
                        // since the value may contain escaped characters
                        var renderedLiteral = Rendering.RenderStringLiteral(literal.Value);
                        return FormattingResult<Expression>.Create(literal, context.Advance(renderedLiteral.Length));
                    }

                case Expression.CharLiteral charLit:
                    // Use the actual rendered length which varies for escaped characters
                    return FormattingResult<Expression>.Create(charLit, context.Advance(Rendering.RenderCharLiteral(charLit.Value).Length));

                case Expression.Integer intLit:
                    return FormattingResult<Expression>.Create(intLit, context.Advance(intLit.Value.ToString().Length));

                case Expression.Hex hexLit:
                    // Use the same hex rendering logic as the Renderer
                    return FormattingResult<Expression>.Create(hexLit, context.Advance(Rendering.RenderHexPattern(hexLit.Value).Length));

                case Expression.Floatable floatLit:
                    return FormattingResult<Expression>.Create(floatLit, context.Advance(floatLit.LiteralText.Length));

                case Expression.FunctionOrValue funcOrVal:
                    var funcName = funcOrVal.ModuleName.Count > 0
                        ? string.Join(".", funcOrVal.ModuleName) + "." + funcOrVal.Name
                        : funcOrVal.Name;
                    return FormattingResult<Expression>.Create(funcOrVal, context.Advance(funcName.Length));

                case Expression.Negation negation:
                    {
                        var afterNegSign = context.Advance(1); // "-"
                        var negResult = FormatExpression(negation.Expression, afterNegSign);
                        return FormattingResult<Expression>.Create(
                            new Expression.Negation(negResult.FormattedNode),
                            negResult.Context);
                    }

                case Expression.Application app:
                    {
                        // Check if application spans multiple lines based on the containing node's range only
                        var isMultiline = SpansMultipleRows(originalRange);

                        var formattedArgs = new List<Stil4mElmSyntax7.Node<Expression>>();
                        var appCtx = context;
                        var currentComments = context.Comments;

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
                                    // First applied argument: keep on same line if it was on same line as function
                                    putOnNewLine = arg.Range.Start.Row > functionRow;
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
                                            appCtx = appCtx.FormatAndAddComment(comment);
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
                                currentComments = argResult.Context.Comments;
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
                                currentComments = argResult.Context.Comments;
                            }
                        }

                        return FormattingResult<Expression>.Create(new Expression.Application(formattedArgs), appCtx);
                    }

                case Expression.RecordExpr recordExpr:
                    {
                        var afterOpenBrace = context.Advance(1); // "{"
                        var recordFields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(recordExpr.Fields);

                        // Get original separator locations if available
                        IReadOnlyList<Location>? originalSeparatorLocations = null;
                        if (recordExpr.Fields is SeparatedSyntaxList<RecordExprField>.NonEmpty nonEmpty)
                        {
                            originalSeparatorLocations = [.. nonEmpty.Rest.Select(r => r.SeparatorLocation)];
                        }

                        if (recordFields.Count is 0)
                        {
                            return FormattingResult<Expression>.Create(
                                new Expression.RecordExpr(recordExpr.Fields),
                                afterOpenBrace.Advance(1)); // "}"
                        }

                        // Check if record should be multiline based on the containing node's range only
                        var isMultilineRecord = SpansMultipleRows(originalRange);

                        if (isMultilineRecord)
                        {
                            // Multiline record: { field1 = val1
                            //                   , field2 = val2
                            //                   }
                            var afterOpenSpace = afterOpenBrace.Advance(1); // " "
                            var recordCtx = afterOpenSpace;
                            var currentComments = context.Comments;

                            // Create reference context at opening brace column for field alignment
                            // (context is at {, so SetIndentToCurrentColumn captures the brace column)
                            var fieldAlignRef = context.SetIndentToCurrentColumn();

                            // First field on same line as opening brace
                            var firstField = recordFields[0];
                            var firstFieldStartLoc = recordCtx.CurrentLocation();
                            var afterFirstFieldName = recordCtx.Advance(firstField.FieldName.Value.Length);

                            // Check if first field value is on a new line (multiline value)
                            var firstFieldValueOnNewLine = firstField.ValueExpr.Range.Start.Row > firstField.FieldName.Range.Start.Row;

                            FormattingResult<Stil4mElmSyntax7.Node<Expression>> firstFieldResult;
                            Location firstEqualsLoc;
                            if (firstFieldValueOnNewLine)
                            {
                                // Value on new line with indentation to next multiple of 4 from field start
                                firstEqualsLoc = afterFirstFieldName.Advance(1).CurrentLocation(); // " ="
                                var afterFirstEq = afterFirstFieldName.Advance(2); // " ="
                                // Create reference at the indented position
                                var targetRef = recordCtx.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var valueContext = afterFirstEq.ReturnToIndent(targetRef).NextRowToIndent();

                                // Check for comments between = and value expression
                                var equalsRow = firstField.FieldName.Range.End.Row;
                                var valueStartRow = firstField.ValueExpr.Range.Start.Row;
                                var commentsBeforeValue = commentQueries.GetBetweenRows(equalsRow, valueStartRow);
                                foreach (var comment in commentsBeforeValue)
                                {
                                    valueContext = valueContext.FormatAndAddComment(comment);
                                    // FormatAndAddComment already positions us on the correct row
                                    // (it advances to next row if comment didn't end with newline)
                                    // Just need to set the column
                                    valueContext = valueContext.ReturnToIndent(targetRef).SetIndentColumn();
                                }

                                firstFieldResult = FormatExpression(firstField.ValueExpr, valueContext);
                            }
                            else
                            {
                                firstEqualsLoc = afterFirstFieldName.Advance(1).CurrentLocation(); // " = "
                                var afterFirstEq = afterFirstFieldName.Advance(3); // " = "
                                firstFieldResult = FormatExpression(firstField.ValueExpr, afterFirstEq);
                            }

                            var firstFieldNameNode = MakeNodeWithRange(firstFieldStartLoc, afterFirstFieldName.CurrentLocation(), firstField.FieldName.Value);

                            recordCtx = firstFieldResult.Context.ReturnToIndent(context);

                            // Subsequent fields each on new line with ", " at start
                            var restFields = new List<(Location SeparatorLocation, RecordExprField Node)>();

                            for (var i = 1; i < recordFields.Count; i++)
                            {
                                var prevField = recordFields[i - 1];
                                var field = recordFields[i];

                                // Get the original separator row for this field if available
                                var originalSeparatorRow =
                                    originalSeparatorLocations is not null && i - 1 < originalSeparatorLocations.Count
                                    ?
                                    originalSeparatorLocations[i - 1].Row
                                    :
                                    prevField.ValueExpr.Range.End.Row + 1; // Default to next row after prev field

                                // Check for comments BEFORE the comma (between prev field value end and separator row)
                                var commentsBeforeComma = commentQueries.GetBetweenRows(
                                    prevField.ValueExpr.Range.End.Row,
                                    originalSeparatorRow);

                                var hadCommentsBetweenFields = false;
                                var lastCommentEndRow = prevField.ValueExpr.Range.End.Row;
                                foreach (var comment in commentsBeforeComma)
                                {
                                    hadCommentsBetweenFields = true;
                                    // Check if there should be a blank line before this comment
                                    if (comment.Range.Start.Row > lastCommentEndRow + 1)
                                    {
                                        // There was a blank line before this comment, preserve it
                                        recordCtx = recordCtx.WithBlankLine().ReturnToIndent(fieldAlignRef).SetIndentColumn();
                                    }
                                    else
                                    {
                                        recordCtx = recordCtx.ReturnToIndent(fieldAlignRef).NextRowToIndent();
                                    }

                                    recordCtx = recordCtx.FormatAndAddComment(comment);
                                    lastCommentEndRow = comment.Range.End.Row;
                                }

                                // Move to new row for this field's separator
                                if (!hadCommentsBetweenFields || recordCtx.CurrentColumn != fieldAlignRef.CurrentColumn)
                                {
                                    recordCtx = recordCtx.ReturnToIndent(fieldAlignRef).NextRowToIndent();
                                }
                                else
                                {
                                    recordCtx = recordCtx.ReturnToIndent(fieldAlignRef).SetIndentColumn();
                                }

                                var separatorLoc = recordCtx.CurrentLocation(); // Comma goes here
                                recordCtx = recordCtx.Advance(2); // ", "

                                // Create reference for after comma position
                                var afterCommaRef = recordCtx.SetIndentToCurrentColumn();

                                // Check for comments AFTER the comma (starting at original separator row or later)
                                var commentsAfterComma = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                                    originalSeparatorRow, 0, field.FieldName.Range.Start.Row, requireAfterColumn: false);

                                foreach (var comment in commentsAfterComma)
                                {
                                    recordCtx = recordCtx.FormatAndAddComment(comment);
                                    // After comment, reposition for field name at afterCommaRef's indent
                                    recordCtx = recordCtx.ReturnToIndent(afterCommaRef).SetIndentColumn();
                                }

                                var fieldStartLoc = recordCtx.CurrentLocation();
                                var afterFieldName = recordCtx.Advance(field.FieldName.Value.Length);

                                // Check if field value is on a new line (multiline value)
                                var fieldValueOnNewLine = field.ValueExpr.Range.Start.Row > field.FieldName.Range.Start.Row;

                                FormattingResult<Stil4mElmSyntax7.Node<Expression>> fieldResult;
                                Location equalsLoc;
                                if (fieldValueOnNewLine)
                                {
                                    // Value on new line with indentation to next multiple of 4 from field start
                                    equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " ="
                                    var afterEq = afterFieldName.Advance(2); // " ="
                                    // Create reference for value indentation based on field start
                                    var fieldIndentRef = recordCtx.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                    var valueContext = afterEq.ReturnToIndent(fieldIndentRef).NextRowToIndent();

                                    // Check for comments between = and value expression
                                    var eqRow = field.FieldName.Range.End.Row;
                                    var valStartRow = field.ValueExpr.Range.Start.Row;
                                    var commentsBeforeVal = commentQueries.GetBetweenRows(eqRow, valStartRow);
                                    foreach (var comment in commentsBeforeVal)
                                    {
                                        valueContext = valueContext.FormatAndAddComment(comment);
                                        // FormatAndAddComment already positions us on the correct row
                                        // Just need to set the column
                                        valueContext = valueContext.ReturnToIndent(fieldIndentRef).SetIndentColumn();
                                    }

                                    fieldResult = FormatExpression(field.ValueExpr, valueContext);
                                }
                                else
                                {
                                    equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                                    var afterEq = afterFieldName.Advance(3); // " = "
                                    fieldResult = FormatExpression(field.ValueExpr, afterEq);
                                }

                                var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                                restFields.Add((separatorLoc, new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode)));

                                recordCtx = fieldResult.Context.ReturnToIndent(context);
                            }

                            // Closing brace on new line, aligned with opening brace
                            var closeCtx = recordCtx.ReturnToIndent(fieldAlignRef).NextRowToIndent();
                            var recordCloseBraceLoc = closeCtx.CurrentLocation();
                            var afterRecordClose = closeCtx.Advance(1); // "}"

                            // Build the SeparatedSyntaxList with proper separator locations
                            var firstRecordField = new RecordExprField(firstFieldNameNode, firstEqualsLoc, firstFieldResult.FormattedNode);
                            var fieldsAsSeparatedList = new SeparatedSyntaxList<RecordExprField>.NonEmpty(
                                firstRecordField,
                                restFields);

                            return FormattingResult<Expression>.Create(new Expression.RecordExpr(
                                fieldsAsSeparatedList
                            ), afterRecordClose.ReturnToIndent(context));
                        }
                        else
                        {
                            // Single line record: { field1 = val1, field2 = val2 }
                            var afterOpenSpace = afterOpenBrace.Advance(1); // " "
                            var formattedRecordFields = new List<RecordExprField>();
                            var recordCtx = afterOpenSpace;
                            var currentComments = context.Comments;

                            for (var i = 0; i < recordFields.Count; i++)
                            {
                                if (i > 0) recordCtx = recordCtx.Advance(2); // ", "
                                var field = recordFields[i];
                                var fieldStartLoc = recordCtx.CurrentLocation();
                                var afterFieldName = recordCtx.Advance(field.FieldName.Value.Length);
                                var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                                var afterEq = afterFieldName.Advance(3); // " = "
                                var fieldResult = FormatExpression(field.ValueExpr, afterEq);
                                currentComments = fieldResult.Context.Comments;

                                var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                                formattedRecordFields.Add(new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode));

                                recordCtx = fieldResult.Context;
                            }

                            var afterFieldsSpace = recordCtx.Advance(1); // " "
                            var afterRecordClose = afterFieldsSpace.Advance(1); // "}"

                            return FormattingResult<Expression>.Create(new Expression.RecordExpr(
                                ToSeparatedSyntaxList(formattedRecordFields)
                            ), afterRecordClose);
                        }
                    }

                case Expression.ListExpr listExpr:
                    {
                        var listOpenLoc = context.CurrentLocation();
                        var elements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(listExpr.Elements);

                        if (elements.Count is 0)
                        {
                            return FormattingResult<Expression>.Create(
                                new Expression.ListExpr(listExpr.Elements),
                                context.Advance(2)); // "[]"
                        }

                        // Check if list should be multiline based on the container range only.
                        // The AVH4 formatter should preserve the multiline structure if:
                        // - The container range spans multiple rows (start row != end row)
                        var isMultilineList = SpansMultipleRows(originalRange);

                        if (isMultilineList)
                        {
                            // Multiline list: first element on same line as "[", rest on their own lines with "," prefix
                            // Format: [ first_elem
                            //         , second_elem
                            //         ]
                            var afterListOpen = context.Advance(2); // "[ "
                            // Create reference context at opening bracket column for alignment
                            var listAlignRef = context.SetIndentToCurrentColumn();
                            // Create reference for content indented past bracket
                            var listContentRef = context.Advance(2).SetIndentToCurrentColumn();

                            // Check for comments before the first element
                            // Use originalRange.Start for the opening bracket location
                            var commentsBeforeFirst = commentQueries.GetAfterLocationBeforeRow(
                                originalRange.Start, elements[0].Range.Start.Row);

                            var firstElemCtx = afterListOpen;
                            if (commentsBeforeFirst.Count > 0)
                            {
                                // Comment(s) after "[ " but before first element
                                foreach (var comment in commentsBeforeFirst)
                                {
                                    // Format the comment, add it to the context, and position at list content indent
                                    firstElemCtx = firstElemCtx.ReturnToIndent(listContentRef).FormatAndAddComment(comment);
                                }
                            }

                            // First element 
                            var firstElemResult = FormatExpression(elements[0], firstElemCtx);
                            var elemCtx = firstElemResult.Context;

                            // Check for trailing comment on the same row as the first element
                            var firstElemTrailingComment = commentQueries.GetTrailing(elements[0].Range);

                            if (firstElemTrailingComment is not null)
                            {
                                // Add space and trailing comment
                                elemCtx = elemCtx.Advance(1); // space before comment
                                // Format the comment, add it to the context, and stay at the comment end position
                                elemCtx = elemCtx.FormatAndAddCommentThenPositionAtEnd(firstElemTrailingComment);
                            }

                            // Build rest list with proper separator locations
                            var restItems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Expression> Node)>();

                            // Subsequent elements each on new line with ", " at start, aligned with opening bracket
                            for (var i = 1; i < elements.Count; i++)
                            {
                                elemCtx = elemCtx.ReturnToIndent(listAlignRef).NextRowToIndent();
                                var separatorLoc = elemCtx.CurrentLocation(); // comma goes here

                                // Check for comments between this and previous element
                                var prevElem = elements[i - 1];
                                var currElem = elements[i];
                                var commentsBetween = commentQueries.GetBetweenRanges(prevElem.Range, currElem.Range).ToList();

                                if (commentsBetween.Count is not 0)
                                {
                                    // Determine comment placement based on original format:
                                    // If there's a blank line between previous element and comment,
                                    // OR if elements were originally compact (multiple elements on same row),
                                    // format as blank line, comment on own line, ", element"
                                    // Otherwise format as ", -- comment\n  element"
                                    var firstComment = commentsBetween[0];

                                    // Check if there's a blank line before the comment (between previous element and first comment)
                                    var hasBlankLineBeforeComment = firstComment.Range.Start.Row > prevElem.Range.End.Row + 1;

                                    // Check if the list was originally in compact format (multiple elements on first row)
                                    var firstRowElementCount = elements.Count(e => e.Range.Start.Row == elements[0].Range.Start.Row);
                                    var wasOriginallyCompact = firstRowElementCount > 1;

                                    if (!hasBlankLineBeforeComment && !wasOriginallyCompact && commentsBetween.Count is 1)
                                    {
                                        // Comment on same line as comma: ", -- comment\n  element"
                                        elemCtx = elemCtx.Advance(2); // ", "
                                        // Format the comment, add it to the context, and position at list content indent
                                        elemCtx = elemCtx.ReturnToIndent(listContentRef).FormatAndAddComment(firstComment);
                                        var elemResult = FormatExpression(currElem, elemCtx);
                                        restItems.Add((separatorLoc, elemResult.FormattedNode));
                                        elemCtx = elemResult.Context.ReturnToIndent(context);
                                    }
                                    else
                                    {
                                        // Comments between elements with blank line: format as
                                        // blank line, comment(s) on own lines, ", element"

                                        // Add blank line before comments
                                        elemCtx = elemCtx.ReturnToIndent(listAlignRef).NextRowToIndent();

                                        foreach (var comment in commentsBetween)
                                        {
                                            // Format the comment, add it to the context, and position at list align indent
                                            elemCtx = elemCtx.ReturnToIndent(listAlignRef).FormatAndAddComment(comment);
                                        }

                                        // Now add the separator and element on this line
                                        separatorLoc = elemCtx.CurrentLocation();
                                        elemCtx = elemCtx.Advance(2); // ", "
                                        var elemResult = FormatExpression(currElem, elemCtx);
                                        restItems.Add((separatorLoc, elemResult.FormattedNode));
                                        elemCtx = elemResult.Context.ReturnToIndent(context);
                                    }
                                }
                                else
                                {
                                    elemCtx = elemCtx.Advance(2); // ", "
                                    var elemResult = FormatExpression(currElem, elemCtx);
                                    restItems.Add((separatorLoc, elemResult.FormattedNode));
                                    elemCtx = elemResult.Context;

                                    // Check for trailing comment on the same row as the element
                                    var trailingComment = commentQueries.GetTrailing(currElem.Range);

                                    if (trailingComment is not null)
                                    {
                                        // Add space and trailing comment
                                        elemCtx = elemCtx.Advance(1); // space before comment
                                        // Format the comment, add it to the context, and stay at the comment end position
                                        elemCtx = elemCtx.FormatAndAddCommentThenPositionAtEnd(trailingComment);
                                    }
                                }
                            }

                            // Check for comments between last element and closing bracket
                            var lastElem = elements[elements.Count - 1];
                            // Look for comments after the last element and before the closing bracket
                            var commentsBeforeClose = commentQueries.GetBetweenRows(
                                lastElem.Range.End.Row, originalRange.End.Row);

                            foreach (var comment in commentsBeforeClose)
                            {
                                // Add blank line before comment if there was one in the original
                                var hasBlankLineBeforeComment = comment.Range.Start.Row > lastElem.Range.End.Row + 1;
                                if (hasBlankLineBeforeComment)
                                {
                                    elemCtx = elemCtx.NextRowToIndent(); // blank line
                                }

                                // Comment on its own line
                                elemCtx = elemCtx.ReturnToIndent(listAlignRef).NextRowToIndent();
                                // Format the comment, add it to the context, and stay at the comment end position
                                elemCtx = elemCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                            }

                            // Closing bracket on new line, aligned with opening bracket
                            var closeCtx = elemCtx.ReturnToIndent(listAlignRef).NextRowToIndent();
                            var multilineListCloseLoc = closeCtx.CurrentLocation();
                            var afterMultilineListClose = closeCtx.Advance(1); // "]"

                            var multilineSeparatedList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty(
                                firstElemResult.FormattedNode, restItems);

                            return FormattingResult<Expression>.Create(new Expression.ListExpr(
                                multilineSeparatedList
                            ), afterMultilineListClose.ReturnToIndent(context));
                        }
                        else
                        {
                            // Single line list
                            var afterListOpen = context.Advance(2); // "[ "
                            var formattedElements = new List<Stil4mElmSyntax7.Node<Expression>>();
                            var listCtx = afterListOpen;
                            var currentComments = context.Comments;

                            for (var i = 0; i < elements.Count; i++)
                            {
                                if (i > 0) listCtx = listCtx.Advance(2); // ", "
                                var elemResult = FormatExpression(elements[i], listCtx);
                                currentComments = elemResult.Context.Comments;
                                formattedElements.Add(elemResult.FormattedNode);
                                listCtx = elemResult.Context;
                            }

                            var afterListSpace = listCtx.Advance(1); // " "
                            var afterListClose = afterListSpace.Advance(1); // "]"

                            return FormattingResult<Expression>.Create(new Expression.ListExpr(
                                ToSeparatedSyntaxList(formattedElements)
                            ), afterListClose);
                        }
                    }

                case Expression.ParenthesizedExpression parenExpr:
                    {
                        // Trim duplicate parentheses: ((expr)) -> (expr)
                        // Following elm-format behavior, when there are multiple levels of nested
                        // parentheses around an expression, we reduce them to a single level.
                        // This is safe because the AST is an immutable tree parsed from source code,
                        // so circular references are not possible and depth is bounded by source nesting.
                        var innerExpr = parenExpr.Expression;
                        while (innerExpr.Value is Expression.ParenthesizedExpression nestedParen)
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
                        var innerResult = FormatExpression(innerExpr, afterOpenParen);

                        // Determine if closing paren should be on new line.
                        // The only condition for making ParenthesizedExpression multiline is the content expression being multiline.
                        var formattedContentIsMultiline = innerResult.Context.CurrentRow > afterOpenParen.CurrentRow;
                        var closeParenOnNewLine = formattedContentIsMultiline;

                        FormattingContext afterCloseParen;

                        if (closeParenOnNewLine)
                        {
                            // Closing paren on new line, aligned with opening paren
                            var closeCtx = innerResult.Context.ReturnToIndent(openParenRef).NextRowToIndent();
                            afterCloseParen = closeCtx.Advance(1); // ")"
                        }
                        else
                        {
                            // Closing paren on same line as end of inner expression
                            afterCloseParen = innerResult.Context.Advance(1); // ")"
                        }

                        return FormattingResult<Expression>.Create(
                            new Expression.ParenthesizedExpression(innerResult.FormattedNode),
                            afterCloseParen);
                    }

                case Expression.OperatorApplication opApp:
                    {
                        var leftResult = FormatExpression(opApp.Left, context);
                        var currentComments = leftResult.Context.Comments;

                        // Check if the right operand is on a new line in the original
                        var rightOnNewLine = opApp.Right.Range.Start.Row > opApp.Left.Range.End.Row;

                        if (rightOnNewLine)
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
                                var formattedOperator = MakeNodeWithRange(operatorStart, operatorEnd, opApp.Operator.Value);

                                // Right operand on new line with extra indentation
                                // Create reference at current indent + 4
                                var rightIndentRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var pipeRightContext = pipeAfterOp.ReturnToIndent(rightIndentRef).NextRowToIndent();
                                var pipeRightResult = FormatExpression(opApp.Right, pipeRightContext);

                                return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
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
                                newLineCtx = newLineCtx.FormatAndAddComment(comment);
                                // Align next comment or operator at target column
                                newLineCtx = newLineCtx.ReturnToIndent(targetRef).SetIndentColumn();
                            }

                            var opStart = newLineCtx.CurrentLocation();
                            var afterOp = newLineCtx.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();

                            // Create operator node with its new position
                            var formattedOp = MakeNodeWithRange(opStart, opEnd, opApp.Operator.Value);

                            // Check for comments on the same line as the operator (trailing comment after operator)
                            var trailingOpComment = commentQueries.GetTrailing(opApp.Operator.Range);
                            FormattingContext rightContext;

                            if (trailingOpComment is not null)
                            {
                                // Comment immediately after operator: "&&" followed by " -- comment" then newline
                                var afterOpSpace = afterOp.Advance(1); // " "
                                // Format comment - this positions at indent column on new row
                                var afterComment = afterOpSpace.FormatAndAddComment(trailingOpComment);
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
                                rightContext = rightContext.FormatAndAddComment(comment);
                            }

                            var rightResult = FormatExpression(opApp.Right, rightContext);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context.ReturnToIndent(context));
                        }
                        else
                        {
                            // Single line
                            var afterLeftSpace = leftResult.Context.Advance(1); // " "
                            var opStart = afterLeftSpace.CurrentLocation();
                            var afterOp = afterLeftSpace.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();
                            var afterOpSpace = afterOp.Advance(1); // " "
                            var rightResult = FormatExpression(opApp.Right, afterOpSpace);

                            // Create operator node with its new position
                            var formattedOp = MakeNodeWithRange(opStart, opEnd, opApp.Operator.Value);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context);
                        }
                    }

                case Expression.PrefixOperator prefixOp:
                    return FormattingResult<Expression>.Create(prefixOp, context.Advance(prefixOp.Operator.Length + 2)); // "(op)"

                case Expression.RecordAccess recordAccess:
                    {
                        var recordResult = FormatExpression(recordAccess.Record, context);
                        var afterDot = recordResult.Context.Advance(1); // "."
                        var fieldNameLoc = afterDot.CurrentLocation();
                        var afterFieldAccess = afterDot.Advance(recordAccess.FieldName.Value.Length);
                        var fieldNameWithLoc = MakeNodeWithRange(fieldNameLoc, afterFieldAccess.CurrentLocation(), recordAccess.FieldName.Value);

                        return FormattingResult<Expression>.Create(
                            new Expression.RecordAccess(recordResult.FormattedNode, fieldNameWithLoc),
                            afterFieldAccess);
                    }

                case Expression.RecordAccessFunction accessFunc:
                    return FormattingResult<Expression>.Create(accessFunc, context.Advance(accessFunc.FunctionName.Length));

                case Expression.IfBlock ifBlock:
                    return FormatIfBlock(ifBlock, context);

                case Expression.CaseExpression caseExpr:
                    return FormatCaseExpression(caseExpr, context);

                case Expression.LetExpression letExpr:
                    return FormatLetExpression(letExpr, context);

                case Expression.LambdaExpression lambdaExpr:
                    return FormatLambdaExpression(lambdaExpr, context);

                case Expression.TupledExpression tupledExpr:
                    return FormatTupledExpression(tupledExpr, originalRange, context);

                case Expression.RecordUpdateExpression recordUpdate:
                    return FormatRecordUpdateExpression(recordUpdate, originalRange, context);

                default:
                    throw new System.NotImplementedException(
                        $"Expression formatting not implemented for: {expr.GetType().Name} " +
                        $"at row {originalRange.Start.Row}, column {originalRange.Start.Column}");
            }
        }

        private FormattingResult<Expression> FormatIfBlock(
            Expression.IfBlock ifBlock,
            FormattingContext context,
            int? chainBaseColumn = null)
        {
            // Check if this is a chained else-if (else block is another if)
            // But only if there are no comments between else and if tokens
            var isElseIf = false;
            if (ifBlock.ElseBlock.Value is Expression.IfBlock innerIf)
            {
                // Check if there are any comments between else token and inner if token
                var commentsBetweenElseAndIf = commentQueries.HasBetweenRows(
                    ifBlock.ElseTokenLocation.Row, innerIf.IfTokenLocation.Row);

                // Also check for comments on the same row as else but after it
                var commentsAfterElseOnSameLine = commentQueries.HasOnRowAfterColumn(
                    ifBlock.ElseTokenLocation.Row, ifBlock.ElseTokenLocation.Column + 4); // "else" is 4 chars

                // Only treat as else-if if no comments between else and if
                isElseIf = !commentsBetweenElseAndIf && !commentsAfterElseOnSameLine;
            }
            var currentComments = context.Comments;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            // Check if condition is multiline based on original layout
            var conditionIsMultiline = SpansMultipleRows(ifBlock.Condition.Range) ||
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
            var bodyRef = effectiveBaseRef.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            FormattingResult<Stil4mElmSyntax7.Node<Expression>> conditionResult;
            Location thenTokenLoc;
            FormattingContext afterThen;

            if (conditionIsMultiline)
            {
                // Multiline condition: "if" on its own, condition indented on next line
                var afterIf = context.Advance(2); // "if" (no trailing space)
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
                        conditionContext = conditionContext.ReturnToIndent(conditionIndentRef).FormatAndAddComment(comment);
                    }
                }

                conditionResult = FormatExpression(ifBlock.Condition, conditionContext);
                currentComments = conditionResult.Context.Comments;

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
                        afterConditionCtx = afterConditionCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                    }
                }

                // "then" on its own line at base indent
                var thenLineContext = afterConditionCtx.ReturnToIndent(context).NextRowToIndent();
                thenTokenLoc = thenLineContext.CurrentLocation();
                afterThen = thenLineContext.Advance(4); // "then"
            }
            else
            {
                // Single-line condition: "if condition then"
                var afterIf = context.Advance(3); // "if "
                conditionResult = FormatExpression(ifBlock.Condition, afterIf);
                currentComments = conditionResult.Context.Comments;

                // " then" (space before then)
                var afterCondSpace = conditionResult.Context.Advance(1);
                thenTokenLoc = afterCondSpace.CurrentLocation();
                afterThen = afterCondSpace.Advance(4); // "then"
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
                    thenContext = thenContext.ReturnToIndent(thenBodyIndentRef).FormatAndAddComment(comment);
                }
            }

            var thenResult = FormatExpression(ifBlock.ThenBlock, thenContext);
            currentComments = thenResult.Context.Comments;

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
                var afterElse = elseContext.Advance(5); // "else "
                var innerIfBlock = (Expression.IfBlock)ifBlock.ElseBlock.Value;

                // Format inner if starting from "if" position
                var innerIfTokenLoc = afterElse.CurrentLocation();

                // Check if inner condition is multiline
                var innerConditionIsMultiline = SpansMultipleRows(innerIfBlock.Condition.Range) ||
                                                innerIfBlock.ThenTokenLocation.Row > innerIfBlock.Condition.Range.Start.Row;

                FormattingResult<Stil4mElmSyntax7.Node<Expression>> innerConditionResult;
                Location innerThenLoc;
                FormattingContext afterInnerThen;

                if (innerConditionIsMultiline)
                {
                    // Multiline inner condition: "if" on its own line, condition indented
                    var afterInnerIf = afterElse.Advance(2); // "if" (no trailing space)
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
                            innerConditionContext = innerConditionContext.ReturnToIndent(innerCondIndentRef).FormatAndAddComment(comment);
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
                            afterInnerConditionCtx = afterInnerConditionCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                        }
                    }

                    // "then" on its own line at base indent
                    var innerThenLineContext = afterInnerConditionCtx.ReturnToIndent(effectiveBaseRef).NextRowToIndent();
                    innerThenLoc = innerThenLineContext.CurrentLocation();
                    afterInnerThen = innerThenLineContext.Advance(4); // "then"
                }
                else
                {
                    // Single-line inner condition: "if condition then"
                    var afterInnerIf = afterElse.Advance(3); // "if "
                    innerConditionResult = FormatExpression(innerIfBlock.Condition, afterInnerIf);

                    // " then"
                    var afterInnerCondSpace = innerConditionResult.Context.Advance(1);
                    innerThenLoc = afterInnerCondSpace.CurrentLocation();
                    afterInnerThen = afterInnerCondSpace.Advance(4);
                }

                // Check for comments between inner "then" and inner then-block
                // Also set IndentSpaces so nested expressions indent correctly
                var innerThenContextReference = afterInnerThen.ReturnToIndent(bodyRef).NextRowToIndent();
                var innerThenContext = innerThenContextReference;
                var commentsBeforeInnerThenBlock = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                    innerIfBlock.ThenTokenLocation.Row, innerIfBlock.ThenTokenLocation.Column + 4,
                    innerIfBlock.ThenBlock.Range.Start.Row, requireAfterColumn: true);

                if (commentsBeforeInnerThenBlock.Count > 0)
                {
                    var innerThenBodyIndentRef = innerThenContext.SetIndentToCurrentColumn();
                    foreach (var comment in commentsBeforeInnerThenBlock)
                    {
                        // Format the comment, add it to the context, and position at inner then body indent
                        innerThenContext = innerThenContext.ReturnToIndent(innerThenBodyIndentRef).FormatAndAddComment(comment);
                    }
                }

                // Inner then block - indented from the base column (same as outer if)
                var innerThenResult = FormatExpression(innerIfBlock.ThenBlock, innerThenContext);
                currentComments = innerThenResult.Context.Comments;

                // Check for comments between inner then-block and inner else
                var commentsAfterInnerThenBlock = commentQueries.GetBetweenRows(
                    innerIfBlock.ThenBlock.Range.End.Row, innerIfBlock.ElseTokenLocation.Row);

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

                Stil4mElmSyntax7.Node<Expression> formattedInnerElseBlock;
                FormattingContext afterInnerElseBlock;

                if (innerIfBlock.ElseBlock.Value is Expression.IfBlock nestedIf)
                {
                    // Continue the chain - pass the base column
                    var afterInnerElse = innerElseContext.Advance(5); // "else "
                    var nestedIfResult = FormatIfBlock(nestedIf, afterInnerElse, effectiveBaseColumn);
                    currentComments = nestedIfResult.Context.Comments;
                    formattedInnerElseBlock = MakeNodeWithRange(afterInnerElse.CurrentLocation(), nestedIfResult.Context.CurrentLocation(), nestedIfResult.FormattedNode);
                    afterInnerElseBlock = nestedIfResult.Context;
                }
                else
                {
                    // Final else block - body indented from the base column
                    var afterInnerElse = innerElseContext.Advance(4); // "else"
                    // Also set IndentSpaces so nested expressions indent correctly
                    var innerElseBodyContextReference = afterInnerElse.ReturnToIndent(bodyRef).NextRowToIndent();
                    var innerElseBodyContext = innerElseBodyContextReference;

                    // Check for comments between inner "else" and inner else-block
                    var commentsBeforeInnerElseBlock = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                        innerIfBlock.ElseTokenLocation.Row, innerIfBlock.ElseTokenLocation.Column + 4,
                        innerIfBlock.ElseBlock.Range.Start.Row, requireAfterColumn: true);

                    if (commentsBeforeInnerElseBlock.Count > 0)
                    {
                        var innerElseBodyIndentRef = innerElseBodyContext.SetIndentToCurrentColumn();
                        foreach (var comment in commentsBeforeInnerElseBlock)
                        {
                            // Format the comment, add it to the context, and position at inner else body indent
                            innerElseBodyContext = innerElseBodyContext.ReturnToIndent(innerElseBodyIndentRef).FormatAndAddComment(comment);
                        }
                    }

                    var innerElseResult = FormatExpression(innerIfBlock.ElseBlock, innerElseBodyContext);
                    currentComments = innerElseResult.Context.Comments;
                    formattedInnerElseBlock = innerElseResult.FormattedNode;
                    afterInnerElseBlock = innerElseResult.Context;
                }

                var formattedInnerIf = new Expression.IfBlock(
                    innerIfTokenLoc,
                    innerConditionResult.FormattedNode,
                    innerThenLoc,
                    innerThenResult.FormattedNode,
                    innerElseTokenLoc,
                    formattedInnerElseBlock
                );

                // The else block wraps the inner if block
                var elseBlockRange = MakeRange(afterElse.CurrentLocation(), afterInnerElseBlock.CurrentLocation());
                var formattedElseBlock = new Stil4mElmSyntax7.Node<Expression>(elseBlockRange, formattedInnerIf);

                return FormattingResult<Expression>.Create(new Expression.IfBlock(
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
                var afterElse = elseContext.Advance(4);

                // Check if the else block is an if-block that was separated from else due to comments
                // In that case, the nested if should be at the same column as else, not indented
                var isIfBlockSeparatedByComments = ifBlock.ElseBlock.Value is Expression.IfBlock;

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
                        elseBlockContext = elseBlockContext.ReturnToIndent(elseBodyIndentRef).FormatAndAddComment(comment);
                    }
                }

                var elseResult = FormatExpression(ifBlock.ElseBlock, elseBlockContext);

                return FormattingResult<Expression>.Create(new Expression.IfBlock(
                    ifTokenLoc,
                    conditionResult.FormattedNode,
                    thenTokenLoc,
                    thenResult.FormattedNode,
                    elseTokenLoc,
                    elseResult.FormattedNode
                ), elseResult.Context.ReturnToIndent(context));
            }
        }

        private FormattingResult<Expression> FormatCaseExpression(
            Expression.CaseExpression caseExpr,
            FormattingContext context)
        {
            // Check if the scrutinee expression is on a new line in the original
            var scrutineeOnNewLine = caseExpr.CaseBlock.Expression.Range.Start.Row > caseExpr.CaseBlock.CaseTokenLocation.Row;

            // "case" or "case "
            var caseTokenLoc = context.CurrentLocation();

            // Create reference context at case keyword for alignment
            var caseTokenRef = context.SetIndentToCurrentColumn();
            // Create reference for indented content (next multiple of 4)
            var caseIndentRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            FormattingResult<Stil4mElmSyntax7.Node<Expression>> exprResult;
            FormattingContext afterExpr;

            if (scrutineeOnNewLine)
            {
                // "case" without trailing space
                var afterCase = context.Advance(4);
                // Expression on new line, indented to next multiple of 4 from the case keyword position
                var exprContext = afterCase.ReturnToIndent(caseIndentRef).NextRowToIndent();
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, exprContext);
                afterExpr = exprResult.Context;
            }
            else
            {
                // "case " with trailing space
                var afterCase = context.Advance(5);
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, afterCase);
                afterExpr = exprResult.Context;
            }

            // " of" (space before of) or just "of" if scrutinee was on new line
            Location ofTokenLoc;
            FormattingContext afterOf;
            if (scrutineeOnNewLine)
            {
                // "of" on new line, aligned with "case" keyword
                var ofContext = afterExpr.ReturnToIndent(caseTokenRef).NextRowToIndent();
                ofTokenLoc = ofContext.CurrentLocation();
                afterOf = ofContext.Advance(2);
            }
            else
            {
                // " of" on same line
                var afterSpace = afterExpr.Advance(1);
                ofTokenLoc = afterSpace.CurrentLocation();
                afterOf = afterSpace.Advance(2);
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

            return FormattingResult<Expression>.Create(
                new Expression.CaseExpression(formattedCaseBlock),
                caseContext.ReturnToIndent(context));
        }

        private FormattingResult<Expression> FormatLetExpression(
            Expression.LetExpression letExpr,
            FormattingContext context)
        {
            var currentComments = context.Comments;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            // "let"
            var letTokenLoc = context.CurrentLocation();
            // Create reference at let keyword position for in/expression alignment
            var letTokenRef = context.SetIndentToCurrentColumn();
            var afterLet = context.Advance(3);

            // Format let declarations
            var formattedDecls = new List<Stil4mElmSyntax7.Node<Expression.LetDeclaration>>();
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
                currentComments = declResult.Context.Comments;
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
            var afterIn = inContext.Advance(2);

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

            var formattedLet = new Expression.LetBlock(
                letTokenLoc,
                formattedDecls,
                inTokenLoc,
                exprResult.FormattedNode
            );

            return FormattingResult<Expression>.Create(
                new Expression.LetExpression(formattedLet),
                exprResult.Context);
        }

        private FormattingResult<Stil4mElmSyntax7.Node<Expression.LetDeclaration>> FormatLetDeclaration(
            Stil4mElmSyntax7.Node<Expression.LetDeclaration> letDecl,
            FormattingContext context)
        {
            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            switch (letDecl.Value)
            {
                case Expression.LetDeclaration.LetFunction letFunc:
                    {
                        var startLoc = context.CurrentLocation();
                        var funcName = letFunc.Function.Declaration.Value.Name.Value;
                        var currentCtx = context;
                        Stil4mElmSyntax7.Node<Signature>? formattedSignature = null;

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

                            FormattingResult<Stil4mElmSyntax7.Node<TypeAnnotation>> typeAnnotResult;

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
                                MakeNodeWithRange(sigNameLoc, afterSigName.CurrentLocation(), funcName),
                                colonLoc,
                                typeAnnotResult.FormattedNode
                            );

                            formattedSignature = MakeNodeWithRange(sigNameLoc, typeAnnotResult.Context.CurrentLocation(), formattedSigValue);

                            // Move to next line for function implementation
                            currentCtx = typeAnnotResult.Context.ReturnToIndent(context).NextRowToIndent();
                        }

                        // Now format the function implementation
                        var implStartLoc = currentCtx.CurrentLocation();
                        var afterName = currentCtx.Advance(funcName.Length);

                        // Arguments - format with updated ranges and transform patterns
                        var formattedLetArgs = new List<Stil4mElmSyntax7.Node<Pattern>>();
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
                            MakeNodeWithRange(implStartLoc, afterName.CurrentLocation(), funcName),
                            formattedLetArgs,
                            equalsLoc,
                            exprResult.FormattedNode
                        );

                        var formattedLetFunc = new Expression.LetDeclaration.LetFunction(
                            new FunctionStruct(
                                letFunc.Function.Documentation,
                                formattedSignature,
                                MakeNodeWithRange(implStartLoc, exprResult.Context.CurrentLocation(), formattedImpl)
                            )
                        );

                        var range = MakeRange(startLoc, exprResult.Context.CurrentLocation());
                        return FormattingResult<Stil4mElmSyntax7.Node<Expression.LetDeclaration>>.Create(
                            new Stil4mElmSyntax7.Node<Expression.LetDeclaration>(range, formattedLetFunc),
                            exprResult.Context.ReturnToIndent(context));
                    }

                case Expression.LetDeclaration.LetDestructuring letDestructuring:
                    {
                        var patternResult = FormatPattern(letDestructuring.Pattern, context);
                        var transformedPatternNode = patternResult.FormattedNode;
                        var afterPattern = patternResult.Context;

                        // " =" (space before equals)
                        var afterPatternSpace = afterPattern.Advance(1);
                        var destructEqualsLoc = afterPatternSpace.CurrentLocation();
                        var afterDestructEquals = afterPatternSpace.Advance(1); // just "="

                        var destructExprContext = afterDestructEquals.ReturnToIndent(indentedRef).NextRowToIndent();
                        var exprResult = FormatExpression(letDestructuring.Expression, destructExprContext);

                        var formattedDestructuring = new Expression.LetDeclaration.LetDestructuring(
                            transformedPatternNode,
                            destructEqualsLoc,
                            exprResult.FormattedNode
                        );

                        var destructRange = MakeRange(context.CurrentLocation(), exprResult.Context.CurrentLocation());
                        return FormattingResult<Stil4mElmSyntax7.Node<Expression.LetDeclaration>>.Create(
                            new Stil4mElmSyntax7.Node<Expression.LetDeclaration>(destructRange, formattedDestructuring),
                            exprResult.Context.ReturnToIndent(context));
                    }

                default:
                    throw new System.NotImplementedException(
                        $"LetDeclaration formatting not implemented for: {letDecl.Value.GetType().Name} " +
                        $"at row {letDecl.Range.Start.Row}, column {letDecl.Range.Start.Column}");
            }
        }

        private FormattingResult<Expression> FormatLambdaExpression(
            Expression.LambdaExpression lambdaExpr,
            FormattingContext context)
        {
            // "\"
            var backslashLoc = context.CurrentLocation();
            var afterBackslash = context.Advance(1);

            // Arguments (no space before first argument, space between subsequent arguments)
            var afterArgs = afterBackslash;
            var formattedArgs = new List<Stil4mElmSyntax7.Node<Pattern>>();

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
            // Compare original body position to original arrow position
            var bodyOnNewLine = lambdaExpr.Lambda.Expression.Range.Start.Row > lambdaExpr.Lambda.ArrowLocation.Row;

            FormattingResult<Stil4mElmSyntax7.Node<Expression>> exprResult;

            if (bodyOnNewLine)
            {
                // Body on new line - use next multiple of 4 for indentation from backslash position
                // This handles cases where indent caused by pipeline and opening of lambda is > 3,
                // moving inner nodes to the next indent level
                var bodyRef = context.AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                var bodyContext = afterArrow.ReturnToIndent(bodyRef).NextRowToIndent();

                // Check for comments between arrow and body expression in original
                var arrowRow = lambdaExpr.Lambda.ArrowLocation.Row;
                var exprStartRow = lambdaExpr.Lambda.Expression.Range.Start.Row;
                var commentsBeforeExpr = commentQueries.GetBetweenRows(arrowRow, exprStartRow);

                // Format any comments that appear before the expression
                foreach (var comment in commentsBeforeExpr)
                {
                    bodyContext = bodyContext.FormatAndAddComment(comment);
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

            return FormattingResult<Expression>.Create(
                new Expression.LambdaExpression(formattedLambda),
                exprResult.Context);
        }

        private FormattingResult<Expression> FormatTupledExpression(
            Expression.TupledExpression tupledExpr,
            Range originalRange,
            FormattingContext context)
        {
            var elements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupledExpr.Elements);

            // Check if tuple should be multiline based on the containing node's start and end locations
            var isMultiline = originalRange.End.Row > originalRange.Start.Row;

            // Get original separator (comma) locations if available
            var originalSeparatorLocations = new List<Location>();
            if (tupledExpr.Elements is SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty nonEmpty)
            {
                foreach (var (sepLoc, _) in nonEmpty.Rest)
                {
                    originalSeparatorLocations.Add(sepLoc);
                }
            }

            if (isMultiline)
            {
                // Multiline tuple: ( first_elem
                //                  , second_elem
                //                  )
                var afterOpenParen = context.Advance(2); // "( "

                // Create reference contexts at open paren for alignment
                var elemAlignRef = context.SetIndentToCurrentColumn();
                // Create reference for content indented past paren
                var elemContentRef = context.Advance(2).SetIndentToCurrentColumn();

                // Check for comments before first element
                var commentsBeforeFirst = commentQueries.GetAfterRowBeforeRowWithColumnCheck(
                    originalRange.Start.Row, 0, elements[0].Range.Start.Row, requireAfterColumn: false);

                var firstElemStartCtx = afterOpenParen;
                if (commentsBeforeFirst.Count > 0)
                {
                    // Format: ( -- comment\n      first_element
                    foreach (var comment in commentsBeforeFirst)
                    {
                        // Format the comment, add it to the context, and position at element content indent
                        firstElemStartCtx = firstElemStartCtx.ReturnToIndent(elemContentRef).FormatAndAddComment(comment);
                    }
                }

                // First element on same line as opening paren (or after comments)
                var firstElemResult = FormatExpression(elements[0], firstElemStartCtx);
                var tupledCtx = firstElemResult.Context;

                // Build rest with proper separator locations
                var restItems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Expression> Node)>();

                for (var i = 1; i < elements.Count; i++)
                {
                    var prevElem = elements[i - 1];
                    var currElem = elements[i];

                    // Get all comments between previous element and current element
                    var allCommentsBetween = commentQueries.GetBetweenRows(
                        prevElem.Range.End.Row, currElem.Range.Start.Row);

                    // Get the original comma row for this element (index i-1 in originalSeparatorLocations)
                    var commaRowInOriginal =
                        i - 1 < originalSeparatorLocations.Count
                        ? originalSeparatorLocations[i - 1].Row
                        : -1;

                    // Split comments: those before the comma row go before the comma,
                    // those on or after the comma row go after the comma
                    var commentsBeforeComma = new List<Stil4mElmSyntax7.Node<ParsedComment>>();
                    var commentsAfterComma = new List<Stil4mElmSyntax7.Node<ParsedComment>>();

                    foreach (var comment in allCommentsBetween)
                    {
                        if (commaRowInOriginal > 0 && comment.Range.Start.Row < commaRowInOriginal)
                        {
                            // Comment is on a row before the comma row
                            commentsBeforeComma.Add(comment);
                        }
                        else
                        {
                            // Comment is on or after the comma row
                            commentsAfterComma.Add(comment);
                        }
                    }

                    // Render comments BEFORE the comma (trailing comments for previous element)
                    foreach (var comment in commentsBeforeComma)
                    {
                        tupledCtx = tupledCtx.ReturnToIndent(elemContentRef).NextRowToIndent();
                        // Format the comment, add it to the context, and stay at the comment end position
                        tupledCtx = tupledCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                    }

                    tupledCtx = tupledCtx.ReturnToIndent(elemAlignRef).NextRowToIndent();
                    var separatorLoc = tupledCtx.CurrentLocation(); // comma goes here

                    if (commentsAfterComma.Count > 0)
                    {
                        // Format: , -- comment\n      element
                        tupledCtx = tupledCtx.Advance(2); // ", "

                        foreach (var comment in commentsAfterComma)
                        {
                            // Format the comment, add it to the context, and position at element content indent
                            tupledCtx = tupledCtx.ReturnToIndent(elemContentRef).FormatAndAddComment(comment);
                        }
                    }
                    else
                    {
                        tupledCtx = tupledCtx.Advance(2); // ", "
                    }

                    var elemResult = FormatExpression(currElem, tupledCtx);
                    restItems.Add((separatorLoc, elemResult.FormattedNode));
                    tupledCtx = elemResult.Context;
                }

                // Check for comments before closing paren (after last element)
                var lastElem = elements[elements.Count - 1];
                var commentsBeforeClose = commentQueries.GetBetweenRows(
                    lastElem.Range.End.Row, originalRange.End.Row);

                foreach (var comment in commentsBeforeClose)
                {
                    tupledCtx = tupledCtx.ReturnToIndent(elemContentRef).NextRowToIndent();
                    // Format the comment, add it to the context, and stay at the comment end position
                    tupledCtx = tupledCtx.FormatAndAddCommentThenPositionAtEnd(comment);
                }

                // Closing paren on new line, aligned with opening paren
                var closeCtx = tupledCtx.ReturnToIndent(elemAlignRef).NextRowToIndent();
                var afterCloseParen = closeCtx.Advance(1); // ")"

                var multilineSeparatedList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty(
                    firstElemResult.FormattedNode, restItems);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    multilineSeparatedList
                ), afterCloseParen.ReturnToIndent(context));
            }
            else
            {
                // Single line tuple: ( a, b )
                var afterOpenParen = context.Advance(2); // "( "

                var formattedElements = new List<Stil4mElmSyntax7.Node<Expression>>();
                var tupledCtx = afterOpenParen;

                for (var i = 0; i < elements.Count; i++)
                {
                    if (i > 0) tupledCtx = tupledCtx.Advance(2); // ", "
                    var elemResult = FormatExpression(elements[i], tupledCtx);
                    formattedElements.Add(elemResult.FormattedNode);
                    tupledCtx = elemResult.Context;
                }

                // " )"
                var afterSpace = tupledCtx.Advance(1);
                var afterCloseParen = afterSpace.Advance(1);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    ToSeparatedSyntaxList(formattedElements)
                ), afterCloseParen);
            }
        }

        private FormattingResult<Expression> FormatRecordUpdateExpression(
            Expression.RecordUpdateExpression recordUpdate,
            Range originalRange,
            FormattingContext context)
        {
            var fields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(recordUpdate.Fields);
            var currentComments = context.Comments;

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
                var valueIndentRef = pipeRef.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

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
                    currentComments = fieldResult.Context.Comments;

                    var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

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

                // Closing brace on new line, aligned with opening brace
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

                return FormattingResult<Expression>.Create(new Expression.RecordUpdateExpression(
                    MakeNodeWithRange(recordNameLoc, afterRecordName.CurrentLocation(), recordUpdate.RecordName.Value),
                    pipeLoc,
                    separatedFields
                ), afterCloseBrace.ReturnToIndent(context));
            }
            else
            {
                // Single line format: { r | field = value, field2 = value2 }

                // " | "
                var pipeLoc = afterRecordName.Advance(1).CurrentLocation();
                var afterPipe = afterRecordName.Advance(3);

                // Fields
                var formattedFields = new List<RecordExprField>();
                var fieldCtx = afterPipe;

                for (var i = 0; i < fields.Count; i++)
                {
                    if (i > 0) fieldCtx = fieldCtx.Advance(2); // ", "
                    var field = fields[i];
                    var fieldStartLoc = fieldCtx.CurrentLocation();
                    var afterFieldName = fieldCtx.Advance(field.FieldName.Value.Length);
                    var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                    var afterEq = afterFieldName.Advance(3); // " = "
                    var fieldResult = FormatExpression(field.ValueExpr, afterEq);
                    currentComments = fieldResult.Context.Comments;

                    var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                    formattedFields.Add(new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode));

                    fieldCtx = fieldResult.Context;
                }

                // " }"
                var afterFieldsSpace = fieldCtx.Advance(1);
                var afterCloseBrace = afterFieldsSpace.Advance(1);

                return FormattingResult<Expression>.Create(new Expression.RecordUpdateExpression(
                    MakeNodeWithRange(recordNameLoc, afterRecordName.CurrentLocation(), recordUpdate.RecordName.Value),
                    pipeLoc,
                    ToSeparatedSyntaxList(formattedFields)
                ), afterCloseBrace);
            }
        }

        #endregion

        #region Pattern Formatting

        /// <summary>
        /// Directly transforms a pattern into a node with correct positions.
        /// This method builds the pattern node tree while tracking positions in the context,
        /// avoiding any intermediate string representation.
        /// </summary>
        private FormattingResult<Stil4mElmSyntax7.Node<Pattern>> FormatPattern(
            Stil4mElmSyntax7.Node<Pattern> patternNode,
            FormattingContext context)
        {
            var pattern = patternNode.Value;
            var originalRange = patternNode.Range;
            var startLoc = context.CurrentLocation();

            switch (pattern)
            {
                case Pattern.AllPattern:
                    {
                        var endLoc = new Location(startLoc.Row, startLoc.Column + 1);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(1));
                    }

                case Pattern.VarPattern varPattern:
                    {
                        var len = varPattern.Name.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.UnitPattern:
                    {
                        var endLoc = new Location(startLoc.Row, startLoc.Column + 2);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(2));
                    }

                case Pattern.CharPattern charPattern:
                    {
                        var text = $"'{EscapeCharForPattern((char)charPattern.Value)}'";
                        var len = text.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.StringPattern stringPattern:
                    {
                        var text = $"\"{stringPattern.Value}\"";
                        var len = text.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.IntPattern intPattern:
                    {
                        var text = intPattern.Value.ToString();
                        var len = text.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.HexPattern hexPattern:
                    {
                        var text = Rendering.RenderHexPattern(hexPattern.Value);
                        var len = text.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.FloatPattern floatPattern:
                    {
                        var text = Rendering.FormatFloatForElm(floatPattern.Value);
                        var len = text.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.RecordPattern recordPattern:
                    {
                        // Format: { field1, field2, ... }
                        var currentContext = context.Advance(2); // "{ "

                        SeparatedSyntaxList<Stil4mElmSyntax7.Node<string>> newFields;
                        if (recordPattern.Fields.Count is 0)
                        {
                            newFields = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<string>>.Empty();
                        }
                        else
                        {
                            // Process first field
                            var firstFieldName = recordPattern.Fields[0].Value;
                            var firstFieldStartLoc = currentContext.CurrentLocation();
                            var firstFieldEndLoc = new Location(firstFieldStartLoc.Row, firstFieldStartLoc.Column + firstFieldName.Length);
                            var firstField = MakeNodeWithRange(firstFieldStartLoc, firstFieldEndLoc, firstFieldName);
                            currentContext = currentContext.Advance(firstFieldName.Length);

                            // Process remaining fields
                            var restFields = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<string> Node)>();
                            for (var i = 1; i < recordPattern.Fields.Count; i++)
                            {
                                // Record comma location and advance
                                var commaLoc = currentContext.CurrentLocation();
                                currentContext = currentContext.Advance(2); // ", "

                                // Get field
                                var fieldStartLoc = currentContext.CurrentLocation();
                                var fieldName = recordPattern.Fields[i].Value;
                                var fieldEndLoc = new Location(fieldStartLoc.Row, fieldStartLoc.Column + fieldName.Length);
                                var fieldNode = MakeNodeWithRange(fieldStartLoc, fieldEndLoc, fieldName);
                                restFields.Add((commaLoc, fieldNode));
                                currentContext = currentContext.Advance(fieldName.Length);
                            }

                            newFields = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<string>>.NonEmpty(
                                firstField, restFields);
                        }

                        currentContext = currentContext.Advance(1); // " "
                        currentContext = currentContext.Advance(1); // "}"

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.RecordPattern(newFields);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.NamedPattern namedPattern when namedPattern.Arguments.Count is 0:
                    {
                        // Simple named pattern without arguments
                        var qualifiedName = FormatQualifiedName(namedPattern.Name);
                        var len = qualifiedName.Length;
                        var endLoc = new Location(startLoc.Row, startLoc.Column + len);
                        var node = MakeNodeWithRange(startLoc, endLoc, pattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                            node, context.Advance(len));
                    }

                case Pattern.NamedPattern namedPattern:
                    {
                        // Named pattern with arguments: Name arg1 arg2 ...
                        var qualifiedName = FormatQualifiedName(namedPattern.Name);
                        var currentContext = context.Advance(qualifiedName.Length);

                        var newArguments = new List<Stil4mElmSyntax7.Node<Pattern>>();

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
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
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
                            currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
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
                            currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                        }

                        currentContext = currentContext.Advance(1); // ")"

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.ParenthesizedPattern(innerResult.FormattedNode);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.TuplePattern tuplePattern:
                    {
                        // Format: ( elem1, elem2, ... )
                        var currentContext = context.Advance(2); // "( "

                        Stil4mElmSyntax7.Node<Pattern>? firstElement = null;
                        var restElements = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Pattern> Node)>();

                        // Get the SeparatedSyntaxList as NonEmpty to access separator locations
                        var elementsNonEmpty = tuplePattern.Elements as SeparatedSyntaxList<Stil4mElmSyntax7.Node<Pattern>>.NonEmpty;

                        for (var i = 0; i < tuplePattern.Elements.Count; i++)
                        {
                            var elem = tuplePattern.Elements[i];

                            Location? commaLoc = null;

                            if (i > 0)
                            {
                                // Get the previous element to find comments between it and the comma
                                var prevElem = tuplePattern.Elements[i - 1];

                                // Get the original comma location from the SeparatedSyntaxList
                                // Rest[i-1] gives (commaLocation, currentElement)
                                var originalCommaColumn = elementsNonEmpty!.Rest[i - 1].SeparatorLocation.Column;

                                // Comments that should appear AFTER previous element but BEFORE the comma
                                var commentsBeforeComma = commentQueries.GetOnRowBetweenColumns(
                                    originalRange.Start.Row,
                                    prevElem.Range.End.Column,
                                    originalCommaColumn);

                                foreach (var comment in commentsBeforeComma)
                                {
                                    currentContext = currentContext.Advance(1); // space before comment
                                    currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                                }

                                // Record comma location
                                commaLoc = currentContext.CurrentLocation();
                                currentContext = currentContext.Advance(1); // ","

                                // Comments that should appear AFTER the comma but BEFORE current element
                                var commentsAfterComma = commentQueries.GetOnRowBetweenColumns(
                                    originalRange.Start.Row,
                                    originalCommaColumn,
                                    elem.Range.Start.Column);

                                foreach (var comment in commentsAfterComma)
                                {
                                    currentContext = currentContext.Advance(1); // space before comment
                                    currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                                }

                                currentContext = currentContext.Advance(1); // space before next element
                            }
                            else
                            {
                                // First element - handle comments after opening paren
                                var commentsAfterParen = commentQueries.GetOnRowBetweenColumns(
                                    originalRange.Start.Row,
                                    originalRange.Start.Column,
                                    elem.Range.Start.Column);

                                foreach (var comment in commentsAfterParen)
                                {
                                    currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                                    currentContext = currentContext.Advance(1); // space after comment
                                }
                            }

                            var elemResult = FormatPattern(elem, currentContext);
                            currentContext = elemResult.Context;

                            if (!commaLoc.HasValue)
                            {
                                firstElement = elemResult.FormattedNode;
                            }
                            else
                            {
                                restElements.Add((commaLoc.Value, elemResult.FormattedNode));
                            }

                            // After the last element, handle comments before closing paren
                            if (i == tuplePattern.Elements.Count - 1)
                            {
                                var commentsBeforeCloseParen = commentQueries.GetOnRowBetweenColumns(
                                    originalRange.Start.Row,
                                    elem.Range.End.Column,
                                    originalRange.End.Column);

                                foreach (var comment in commentsBeforeCloseParen)
                                {
                                    currentContext = currentContext.Advance(1); // space before comment
                                    currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                                }
                            }
                        }

                        currentContext = currentContext.Advance(1); // " " before close paren
                        currentContext = currentContext.Advance(1); // ")"

                        var endLoc = currentContext.CurrentLocation();
                        var newElements = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Pattern>>.NonEmpty(
                            firstElement!, restElements);
                        Pattern newPattern = new Pattern.TuplePattern(newElements);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
                    }

                case Pattern.ListPattern listPattern:
                    {
                        if (listPattern.Elements.Count is 0)
                        {
                            var emptyEndLoc = new Location(startLoc.Row, startLoc.Column + 2);
                            Pattern emptyPattern = new Pattern.ListPattern(
                                new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Pattern>>.Empty());
                            var emptyNode = MakeNodeWithRange(startLoc, emptyEndLoc, emptyPattern);
                            return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(
                                emptyNode, context.Advance(2));
                        }

                        // Format: [ elem1, elem2, ... ]
                        var currentContext = context.Advance(2); // "[ "

                        Stil4mElmSyntax7.Node<Pattern>? firstElement = null;
                        var restElements = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Pattern> Node)>();

                        for (var i = 0; i < listPattern.Elements.Count; i++)
                        {
                            var elem = listPattern.Elements[i];

                            Location? commaLoc = null;

                            if (i > 0)
                            {
                                commaLoc = currentContext.CurrentLocation();
                                currentContext = currentContext.Advance(2); // ", "
                            }

                            var elemResult = FormatPattern(elem, currentContext);
                            currentContext = elemResult.Context;

                            if (!commaLoc.HasValue)
                            {
                                firstElement = elemResult.FormattedNode;
                            }
                            else
                            {
                                restElements.Add((commaLoc.Value, elemResult.FormattedNode));
                            }
                        }

                        currentContext = currentContext.Advance(1); // " " before close bracket
                        currentContext = currentContext.Advance(1); // "]"

                        var endLoc = currentContext.CurrentLocation();
                        var newElements = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Pattern>>.NonEmpty(
                            firstElement!, restElements);
                        Pattern newPattern = new Pattern.ListPattern(newElements);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
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
                            currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
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
                                currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
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
                            currentContext = currentContext.FormatAndAddCommentThenPositionAtEnd(comment);
                        }

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.UnConsPattern(
                            headResult.FormattedNode, consOpLoc, tailResult.FormattedNode);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
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

                        var newNameNode = MakeNodeWithRange(nameLoc, nameEndLoc, asPattern.Name.Value);

                        var endLoc = currentContext.CurrentLocation();
                        Pattern newPattern = new Pattern.AsPattern(
                            innerResult.FormattedNode, asTokenLoc, newNameNode);
                        var node = MakeNodeWithRange(startLoc, endLoc, newPattern);
                        return FormattingResult<Stil4mElmSyntax7.Node<Pattern>>.Create(node, currentContext);
                    }

                default:
                    throw new System.NotImplementedException(
                        $"Pattern type '{pattern.GetType().Name}' not implemented in {nameof(FormatPattern)}");
            }
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
    private static Stil4mElmSyntax7.Node<T> MakeNodeWithRange<T>(
        Location start,
        Location end,
        T value) =>
        new(MakeRange(start, end), value);

    /// <summary>
    /// Creates a Range from start and end locations.
    /// </summary>
    private static Range MakeRange(Location start, Location end) =>
        new(start, end);
}
