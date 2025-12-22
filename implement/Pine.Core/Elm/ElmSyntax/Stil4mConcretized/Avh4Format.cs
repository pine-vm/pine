using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;
using Range = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range;

namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Format Elm modules following the style of https://github.com/avh4/elm-format
/// </summary>
public class Avh4Format
{
    /// <summary>
    /// Format an Elm file using AVH4 formatting style and return the formatted source code as a string.
    /// This is a convenience method that combines formatting and rendering.
    /// </summary>
    /// <param name="file">The File to format.</param>
    /// <returns>The formatted Elm source code as a string.</returns>
    public static string FormatToString(File file)
    {
        var formatted = Format(file);
        return Rendering.ToStringWithoutFormatting(formatted);
    }

    /// <summary>
    /// Format an Elm file using AVH4 formatting style, returning a formatted File.
    /// </summary>
    /// <param name="file">The File to format.</param>
    /// <returns>A formatted File with updated token locations.</returns>
    public static File Format(File file)
    {
        var visitor = new Avh4FormatVisitor(file.Comments, file.IncompleteDeclarations);
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

        private FormattingContext(int currentRow, int currentColumn, int indentSpaces)
        {
            CurrentRow = currentRow;
            CurrentColumn = currentColumn;
            IndentSpaces = indentSpaces;
        }

        /// <summary>
        /// Creates the initial formatting context at row 1, column 1, with no indentation.
        /// </summary>
        public static FormattingContext Initial() =>
            new(currentRow: 1, currentColumn: 1, indentSpaces: 0);

        /// <summary>
        /// Creates a formatting context at a specific position, preserving the indent from another context.
        /// Use this when positioning after multiline content (like comments) while maintaining indent.
        /// </summary>
        public static FormattingContext AtPosition(int row, int column, FormattingContext inheritIndentFrom) =>
            new(row, column, inheritIndentFrom.IndentSpaces);

        public Location CurrentLocation() =>
            new(CurrentRow, CurrentColumn);

        private FormattingContext NextRow() =>
            new(CurrentRow + 1, currentColumn: 1, IndentSpaces);

        /// <summary>
        /// Advances to the next row and stays at the current indent column.
        /// </summary>
        public FormattingContext NextRowToIndent() =>
            new(CurrentRow + 1, 1 + IndentSpaces, IndentSpaces);

        /// <summary>
        /// Adds a blank line by advancing two rows.
        /// Equivalent to NextRow().NextRow().
        /// </summary>
        public FormattingContext WithBlankLine() =>
            NextRow().NextRow();

        /// <summary>
        /// Sets the column position without changing indent.
        /// Use this for positioning within a line.
        /// </summary>
        private FormattingContext SetColumn(int column) =>
            new(CurrentRow, column, IndentSpaces);

        /// <summary>
        /// Advances the column by count characters without changing indent.
        /// Use this when advancing past tokens that don't affect nested content alignment.
        /// </summary>
        public FormattingContext Advance(int count) =>
            new(CurrentRow, CurrentColumn + count, IndentSpaces);

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
                indentSpaces: IndentSpaces);
        }

        /// <summary>
        /// Sets indent to the current column position.
        /// </summary>
        public FormattingContext SetIndentToCurrentColumn()
        {
            return new FormattingContext(
                CurrentRow,
                CurrentColumn,
                indentSpaces: CurrentColumn - 1);
        }

        /// <summary>
        /// Returns to the indent level from an earlier context.
        /// Use this when exiting a nested block to restore the previous indentation.
        /// </summary>
        public FormattingContext ReturnToIndent(FormattingContext prevContext) =>
            new(CurrentRow, CurrentColumn, prevContext.IndentSpaces);

        /// <summary>
        /// Resets indent to zero.
        /// Use this for top-level declarations that should start at column 1.
        /// </summary>
        public FormattingContext ResetIndent() =>
            new(CurrentRow, CurrentColumn, indentSpaces: 0);

        /// <summary>
        /// Sets column to the indent position (1 + IndentSpaces).
        /// Use this when starting a new line that should be at the current indent level.
        /// </summary>
        public FormattingContext SetIndentColumn() =>
            new(CurrentRow, 1 + IndentSpaces, IndentSpaces);

        /// <summary>
        /// Calculates the next column that is greater than current position and a multiple of 4.
        /// Returns columns like 5, 9, 13, ... (since columns are 1-based).
        /// </summary>
        public int GetNextMultipleOfFourColumn()
        {
            var currentSpaces = CurrentColumn - 1;
            var nextMultipleSpaces = ((currentSpaces / Indentation.Full) + 1) * Indentation.Full;
            return nextMultipleSpaces + 1;
        }
    }

    /// <summary>
    /// Result of a formatting operation containing the formatted node, updated context,
    /// and accumulated comments.
    /// This type is intended to replace tuple returns in formatting methods to eliminate mutation.
    /// </summary>
    private record FormattingResult<T>(
        T FormattedNode,
        FormattingContext Context,
        ImmutableList<Stil4mElmSyntax7.Node<string>> Comments)
    {
        public static FormattingResult<T> Create(
            T formattedNode,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments) =>
            new(formattedNode, context, comments);

        public static FormattingResult<T> Create(
            T formattedNode,
            FormattingContext context) =>
            new(formattedNode, context, []);
    }

    #endregion

    #region Static Helpers

    /// <summary>
    /// Calculate the end location of a comment given its start location and value.
    /// Also returns whether the comment ends with a newline.
    /// </summary>
    private static (Location EndLocation, bool EndsWithNewline) CalculateCommentEndLocationEx(Location startLocation, string commentValue)
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
    /// Creates a new FormattingContext positioned after a comment ends.
    /// If the comment ends with a newline, we're already at the start of a new line.
    /// </summary>
    private static FormattingContext CreateContextAfterComment(
        FormattingContext currentContext,
        Location commentEnd,
        bool commentEndsWithNewline)
    {
        if (commentEndsWithNewline)
        {
            // Comment ends with newline - we're already at the start of a new line (commentEnd.Row)
            return FormattingContext.AtPosition(
                commentEnd.Row,
                1,
                currentContext).SetIndentColumn();
        }
        else
        {
            // Comment doesn't end with newline - move to the next row
            return FormattingContext.AtPosition(
                commentEnd.Row + 1,
                1,
                currentContext).SetIndentColumn();
        }
    }

    /// <summary>
    /// Formats a comment and adds it to the comments list, returning the updated context and comments.
    /// This encapsulates the common pattern of: get location, calculate end, create formatted comment, add to list, update context.
    /// </summary>
    private static (FormattingContext Context, ImmutableList<Stil4mElmSyntax7.Node<string>> Comments) FormatAndAddComment(
        Stil4mElmSyntax7.Node<string> comment,
        FormattingContext context,
        ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
    {
        var commentLocation = context.CurrentLocation();
        var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
        var formattedComment = comment.WithRange(commentLocation, commentEnd);
        var updatedComments = comments.Add(formattedComment);
        var updatedContext = CreateContextAfterComment(context, commentEnd, endsWithNewline);
        return (updatedContext, updatedComments);
    }

    /// <summary>
    /// Check if a comment is a doc comment (starts with {-|).
    /// </summary>
    private static bool IsDocComment(string commentValue)
    {
        return commentValue.TrimStart().StartsWith("{-|");
    }

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

    #endregion

    #region Visitor Implementation

    /// <summary>
    /// Visitor implementation for AVH4 formatting on concretized syntax model.
    /// </summary>
    private class Avh4FormatVisitor(
        IReadOnlyList<Stil4mElmSyntax7.Node<string>> originalComments,
        IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>> originalIncompleteDeclarations)
    {
        #region File Formatting

        public (File, IReadOnlyList<Stil4mElmSyntax7.Node<string>>, IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>>) FormatFile(
            File file,
            FormattingContext context)
        {
            var formattedComments = ImmutableList<Stil4mElmSyntax7.Node<string>>.Empty;

            // Format module definition
            var (formattedModule, contextAfterModule, commentsAfterModule) =
                FormatModuleDefinition(file.ModuleDefinition, context, formattedComments);

            // Check for comments between module definition and imports
            var moduleRow = file.ModuleDefinition.Range.End.Row;
            var firstImport = file.Imports.FirstOrDefault();

            var commentsAfterModuleBeforeImports =
                firstImport is not null
                ? GetCommentsBetweenRows(moduleRow, firstImport.Range.Start.Row)
                : [];

            FormattingContext contextBeforeImports;
            ImmutableList<Stil4mElmSyntax7.Node<string>> currentComments;

            if (commentsAfterModuleBeforeImports.Count is not 0)
            {
                var startContext = contextAfterModule.WithBlankLine();
                (currentComments, contextBeforeImports) = FormatCommentsAtContext(
                    commentsAfterModuleBeforeImports, startContext, commentsAfterModule);
                contextBeforeImports = contextBeforeImports.NextRowToIndent();
            }
            else
            {
                contextBeforeImports = contextAfterModule.WithBlankLine();
                currentComments = commentsAfterModule;
            }

            // Format imports
            FormattingContext contextAfterImports;
            IReadOnlyList<Stil4mElmSyntax7.Node<Import>> formattedImports;
            ImmutableList<Stil4mElmSyntax7.Node<string>> commentsAfterImports;

            if (firstImport is not null)
            {
                (formattedImports, contextAfterImports, commentsAfterImports) =
                    FormatImports(file.Imports, contextBeforeImports, currentComments);
            }
            else
            {
                formattedImports = [];
                contextAfterImports = contextAfterModule;
                commentsAfterImports = currentComments;
            }

            // Check for comments before first declaration
            var commentsBeforeDecls = commentsAfterImports;

            // Use ORIGINAL file positions when searching for comments (originalComments has original positions)
            var lastOriginalRowBeforeDecls =
                file.Imports.Any()
                ? file.Imports.Last().Range.End.Row
                : file.ModuleDefinition.Range.End.Row;

            var firstDeclaration = file.Declarations.FirstOrDefault();

            var commentsBefore =
                firstDeclaration is not null
                ? GetCommentsBetweenRows(lastOriginalRowBeforeDecls, firstDeclaration.Range.Start.Row)
                : [];

            FormattingContext contextBeforeDecls;

            if (commentsBefore.Count is not 0)
            {
                var firstComment = commentsBefore[0];

                var startContext = formattedImports.Any()
                    ?
                    contextAfterImports.WithBlankLine().NextRowToIndent()
                    :
                    contextAfterModule.WithBlankLine();

                (commentsBeforeDecls, contextBeforeDecls) = FormatCommentsAtContext(
                    commentsBefore, startContext, commentsAfterImports, addBlankLinesAfterNonDocComments: true);

                // For module-level doc comments (between module and first declaration, no imports),
                // we need to add 2 blank lines after the doc comment
                var lastComment = commentsBefore[^1];
                if (!formattedImports.Any() && IsDocComment(lastComment.Value))
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
            var (formattedDeclarations, contextAfterDeclarations, commentsAfterDecls, formattedIncompleteDeclarations) =
                FormatDeclarationsWithIncompletes(file.Declarations, contextBeforeDecls, commentsBeforeDecls);

            // Get trailing comments that appear after all declarations
            var lastDeclRow = file.Declarations.Any()
                ? file.Declarations.Max(d => d.Range.End.Row)
                : (file.Imports.Any()
                    ? file.Imports.Last().Range.End.Row
                    : file.ModuleDefinition.Range.End.Row);

            var trailingComments = GetCommentsAfterRow(lastDeclRow);

            // Format trailing comments
            var finalComments = commentsAfterDecls;
            FormattingContext contextAfterTrailingComments = contextAfterDeclarations;
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
                (finalComments, contextAfterTrailingComments) = FormatCommentsAtContext(
                    trailingComments, trailingContext, commentsAfterDecls);
            }

            var formattedFile = new File(
                ModuleDefinition: formattedModule,
                Imports: formattedImports,
                Declarations: formattedDeclarations,
                Comments: [],
                IncompleteDeclarations: []);

            return (formattedFile, finalComments, formattedIncompleteDeclarations);
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

        private static (ImmutableList<Stil4mElmSyntax7.Node<string>> FormattedComments, FormattingContext NextContext) FormatCommentsAtContext(
            IReadOnlyList<Stil4mElmSyntax7.Node<string>> comments,
            FormattingContext startContext,
            ImmutableList<Stil4mElmSyntax7.Node<string>> existingComments,
            bool addBlankLinesAfterNonDocComments = false)
        {
            var currentContext = startContext;
            var currentComments = existingComments;

            foreach (var comment in comments)
            {
                (currentContext, currentComments) = FormatAndAddComment(comment, currentContext, currentComments);

                // Only add blank lines after real comments (not incomplete declarations stored as comments)
                // Incomplete declarations already contain their original whitespace
                if (addBlankLinesAfterNonDocComments && !IsDocComment(comment.Value))
                {
                    currentContext = currentContext.WithBlankLine();
                }
            }

            return (currentComments, currentContext);
        }

        /// <summary>
        /// Get comments between two row numbers.
        /// Start is exclusive (comments must start after afterRow).
        /// End is inclusive (comments may end at or before beforeRow) to handle incomplete declarations
        /// that span up to the start of the next element.
        /// </summary>
        private IReadOnlyList<Stil4mElmSyntax7.Node<string>> GetCommentsBetweenRows(int afterRow, int beforeRow) =>
            [.. originalComments
                .Where(c => c.Range.Start.Row > afterRow && c.Range.Start.Row < beforeRow)
                .OrderBy(c => c.Range.Start.Row)];

        /// <summary>
        /// Get comments (including incomplete declarations) that appear after a given row.
        /// </summary>
        private IReadOnlyList<Stil4mElmSyntax7.Node<string>> GetCommentsAfterRow(int afterRow) =>
            [.. originalComments
                .Where(c => c.Range.Start.Row > afterRow)
                .OrderBy(c => c.Range.Start.Row)];

        /// <summary>
        /// Get comments that start after the end of one range and before the start of another range.
        /// </summary>
        private IReadOnlyList<Stil4mElmSyntax7.Node<string>> GetCommentsBetweenRanges(Range after, Range before) =>
            [.. originalComments
                .Where(c => c.Range.Start.Row > after.End.Row && c.Range.Start.Row < before.Start.Row)
                .OrderBy(c => c.Range.Start.Row)];

        /// <summary>
        /// Get a trailing comment on the same row as the element, after the element ends.
        /// </summary>
        private Stil4mElmSyntax7.Node<string>? GetTrailingComment(Range elementRange) =>
            originalComments
                .FirstOrDefault(c => c.Range.Start.Row == elementRange.Start.Row &&
                                     c.Range.Start.Column > elementRange.End.Column);

        #endregion

        #region Module Formatting

        private static (Stil4mElmSyntax7.Node<Module>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatModuleDefinition(
            Stil4mElmSyntax7.Node<Module> module,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            var result = module.Value switch
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

            return (result.Item1, result.Item2, formattedComments);
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
                var formattedNodes = new List<Stil4mElmSyntax7.Node<TopLevelExpose>>();

                for (var i = 0; i < explicitList.Nodes.Count; i++)
                {
                    var node = explicitList.Nodes[i];
                    var (formattedNode, nextContext) = FormatTopLevelExpose(node, currentContext);
                    formattedNodes.Add(formattedNode);

                    currentContext = i < explicitList.Nodes.Count - 1
                        ? nextContext.Advance(Keywords.Comma.Length)
                        : nextContext.Advance(Keywords.CloseParen.Length);
                }

                var range = MakeRange(context.CurrentLocation(), currentContext.CurrentLocation());
                return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), currentContext);
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

                var afterOpenParen = parenLineContext.Advance(Keywords.TupleOpen.Length);
                var formattedNodes = new List<Stil4mElmSyntax7.Node<TopLevelExpose>>();

                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], afterOpenParen);
                formattedNodes.Add(firstNode);

                var itemContext = afterFirst;
                var previousRow = explicitList.Nodes[0].Range.Start.Row;

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
                        var afterComma = itemContext.Advance(Keywords.Comma.Length);
                        var (formattedNode, nextContext) = FormatTopLevelExpose(node, afterComma);
                        formattedNodes.Add(formattedNode);
                        itemContext = nextContext;
                    }
                    else
                    {
                        // Item is on the same line - add comma and item on same line
                        var afterComma = itemContext.Advance(Keywords.Comma.Length);
                        var (formattedNode, nextContext) = FormatTopLevelExpose(node, afterComma);
                        formattedNodes.Add(formattedNode);
                        itemContext = nextContext;
                    }

                    previousRow = nodeRow;
                }

                // Closing paren on its own line
                itemContext = itemContext.NextRowToIndent();
                var afterCloseParen = itemContext.Advance(Keywords.CloseParen.Length);
                var finalContext = afterCloseParen.ReturnToIndent(parentContext);
                var range = MakeRange(context.CurrentLocation(), finalContext.CurrentLocation());
                return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), finalContext);
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
            foreach (var node in explicitList.Nodes)
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
            return NodesSpanMultipleRows(explicitList.Nodes);
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

        private static (IReadOnlyList<Stil4mElmSyntax7.Node<Import>>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatImports(
            IReadOnlyList<Stil4mElmSyntax7.Node<Import>> imports,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            if (!imports.Any())
                return ([], context, formattedComments);

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
                currentContext = nextContext.NextRowToIndent();
            }

            return (formattedImports, currentContext, formattedComments);
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
        private (IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>, IReadOnlyList<Stil4mElmSyntax7.Node<IncompleteDeclaration>>) FormatDeclarationsWithIncompletes(
            IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>> declarations,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
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
                return ([], context, formattedComments, []);

            var formattedDeclarations = new List<Stil4mElmSyntax7.Node<Declaration>>();
            var formattedIncompletes = new List<Stil4mElmSyntax7.Node<IncompleteDeclaration>>();
            var currentContext = context;
            var currentComments = formattedComments;

            for (var i = 0; i < allItems.Count; i++)
            {
                var (row, isComplete, originalIndex) = allItems[i];

                if (isComplete)
                {
                    var decl = declarations[originalIndex];
                    var declContext = currentContext.ResetIndent();

                    var (formattedDecl, nextContext, updatedComments) =
                        FormatDeclaration(decl, declContext, currentComments);

                    currentComments = updatedComments;
                    formattedDeclarations.Add(formattedDecl);

                    // Update context for next item
                    if (i < allItems.Count - 1)
                    {
                        var nextItem = allItems[i + 1];

                        if (nextItem.isComplete)
                        {
                            var nextDecl = declarations[nextItem.originalIndex];
                            var commentsBetween = originalComments
                                .Where(c => c.Range.Start.Row > decl.Range.End.Row && c.Range.Start.Row <= nextDecl.Range.Start.Row)
                                .OrderBy(c => c.Range.Start.Row)
                                .ToList();

                            if (formattedDecl.Value is Declaration.InfixDeclaration && nextDecl.Value is Declaration.InfixDeclaration)
                            {
                                currentContext = nextContext.NextRowToIndent();
                            }
                            else if (commentsBetween.Count is not 0)
                            {
                                var firstComment = commentsBetween[0];
                                var isFirstCommentDoc = IsDocComment(firstComment.Value);

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
                                    (currentContext, currentComments) = FormatAndAddComment(comment, currentContext, currentComments);

                                    // For non-doc section comments:
                                    // - FormatAndAddComment already advances to the next row after the comment
                                    // - Only add 2 blank lines after the LAST section comment (before next declaration)
                                    // - Consecutive section comments stay together (no blank lines between)
                                    if (!IsDocComment(comment.Value))
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

            return (formattedDeclarations, currentContext, currentComments, formattedIncompletes);
        }

        private (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatDeclaration(
            Stil4mElmSyntax7.Node<Declaration> decl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            return decl.Value switch
            {
                Declaration.FunctionDeclaration funcDecl =>
                    FormatFunctionDeclaration(funcDecl, decl.Range, context, formattedComments),

                Declaration.AliasDeclaration aliasDecl =>
                    FormatAliasDeclaration(aliasDecl, context, formattedComments),

                Declaration.CustomTypeDeclaration customTypeDecl =>
                    FormatCustomTypeDeclaration(customTypeDecl, context, formattedComments),

                Declaration.InfixDeclaration infixDecl =>
                    FormatInfixDeclaration(infixDecl, decl.Range, context, formattedComments),

                Declaration.PortDeclaration portDecl =>
                    FormatPortDeclaration(portDecl, context, formattedComments),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented " +
                    $"at row {decl.Range.Start.Row}, column {decl.Range.Start.Column}.")
            };
        }

        private (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatFunctionDeclaration(
            Declaration.FunctionDeclaration funcDecl,
            Range originalDeclRange,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            var currentContext = context;
            var currentComments = formattedComments;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            // Format documentation comment if present
            if (funcDecl.Function.Documentation is { } docComment)
            {
                (currentContext, currentComments) = FormatAndAddComment(docComment, currentContext, currentComments);
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

                Stil4mElmSyntax7.Node<TypeAnnotation> formattedTypeAnnot;
                FormattingContext afterType;
                ImmutableList<Stil4mElmSyntax7.Node<string>> typeAnnotComments;

                if (isTypeAnnotOnNewLine)
                {
                    // Type annotation on new line with indentation - no space after colon
                    var typeContext = afterColon.ReturnToIndent(indentedRef).NextRowToIndent();
                    (formattedTypeAnnot, afterType, typeAnnotComments) = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);
                }
                else
                {
                    // Type annotation on same line after " : "
                    var sameLineContext = afterColon.Advance(1); // space after colon
                    (formattedTypeAnnot, afterType, typeAnnotComments) = FormatTypeAnnotation(signature.Value.TypeAnnotation, sameLineContext);
                }

                currentComments = currentComments.AddRange(typeAnnotComments);

                var sigRange = MakeRange(currentContext.CurrentLocation(), afterType.CurrentLocation());
                formattedSignature = new Stil4mElmSyntax7.Node<Signature>(sigRange, new Signature(
                    Name: new Stil4mElmSyntax7.Node<string>(
                        MakeRange(currentContext.CurrentLocation(), afterSigName.CurrentLocation()),
                        sigName),
                    ColonLocation: colonLoc,
                    TypeAnnotation: formattedTypeAnnot
                ));

                // Return to base indent level for the implementation
                currentContext = afterType.ReturnToIndent(context).NextRowToIndent();
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
                var argStartLoc = afterArgs.CurrentLocation();
                var patternText = FormatPatternText(arg.Value);
                afterArgs = afterArgs.Advance(patternText.Length);
                var argEndLoc = afterArgs.CurrentLocation();
                // Transform the pattern to have updated internal locations for correct rendering
                var transformedPattern = TransformPatternWithLocation(arg.Value, argStartLoc);
                formattedArguments.Add(MakeNodeWithRange(argStartLoc, argEndLoc, transformedPattern));
            }

            // " ="
            var equalsLoc = afterArgs.Advance(1).CurrentLocation(); // space before =
            var afterEquals = afterArgs.Advance(2); // " ="

            // Move to next line and indent for the expression
            var exprContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

            // Check for comments between equals and expression in original
            var equalsRow = funcDecl.Function.Declaration.Value.EqualsTokenLocation.Row;
            var exprStartRow = impl.Expression.Range.Start.Row;
            var commentsBeforeExpr = GetCommentsBetweenRows(equalsRow, exprStartRow);

            // Format any comments that appear before the expression
            foreach (var comment in commentsBeforeExpr)
            {
                var commentLocation = exprContext.CurrentLocation();
                var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                currentComments = currentComments.Add(formattedComment);
                exprContext = CreateContextAfterComment(exprContext, commentEnd, endsWithNewline);
                // Maintain the same indent level after the comment (column is 1-based)
                exprContext = exprContext.SetIndentColumn();
            }

            // Format the expression with updated locations
            var exprResult = FormatExpression(impl.Expression, exprContext, currentComments);
            currentComments = exprResult.Comments;

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
            return (
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.FunctionDeclaration(formattedFunc)),
                exprResult.Context.ReturnToIndent(context),
                currentComments);
        }

        private (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            var currentComments = formattedComments;
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            if (aliasDecl.TypeAlias.Documentation is { } docComment)
            {
                (startContext, currentComments) = FormatAndAddComment(docComment, startContext, currentComments);
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
            var commentsBeforeType = originalComments
                .Where(c => c.Range.Start.Row > aliasDecl.TypeAlias.EqualsTokenLocation.Row &&
                            c.Range.Start.Row < aliasDecl.TypeAlias.TypeAnnotation.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            foreach (var comment in commentsBeforeType)
            {
                // Position comment at indent column
                var commentLocation = typeContext.CurrentLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                currentComments = currentComments.Add(formattedComment);
                typeContext = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, typeContext).NextRowToIndent();
            }

            // Format the type annotation with proper locations
            var (formattedTypeAnnotation, afterTypeAnnot, typeAnnotComments) =
                FormatTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, typeContext);

            // Add any comments collected during type annotation formatting
            currentComments = currentComments.AddRange(typeAnnotComments);

            var formattedTypeAlias = new TypeAlias(
                Documentation: aliasDecl.TypeAlias.Documentation,
                TypeTokenLocation: typeTokenLoc,
                AliasTokenLocation: aliasTokenLoc,
                Name: formattedName,
                Generics: formattedGenerics,
                EqualsTokenLocation: equalsLoc,
                TypeAnnotation: formattedTypeAnnotation
            );

            var finalContext = afterTypeAnnot.ReturnToIndent(parentContext);
            var range = MakeRange(startContext.CurrentLocation(), finalContext.CurrentLocation());
            return (
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.AliasDeclaration(formattedTypeAlias)),
                finalContext,
                currentComments);
        }

        private (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatCustomTypeDeclaration(
            Declaration.CustomTypeDeclaration customTypeDecl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            var currentComments = formattedComments;
            var startContext = context;

            // Reference context for indented content (one indent level from base)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            if (customTypeDecl.TypeDeclaration.Documentation is { } docComment)
            {
                (startContext, currentComments) = FormatAndAddComment(docComment, startContext, currentComments);
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
                    var commentsBetweenConstructors = GetCommentsBetweenRanges(prevConstructor.Range, constructor.Range).ToList();

                    if (commentsBetweenConstructors.Count is not 0)
                    {
                        // Format comments between constructors
                        // Comment gets extra 2 space indent (6 spaces total = 4 base indent + 2 extra)
                        var commentIndentRef = constructorIndentContext.SetIndentColumn().Advance(2).SetIndentToCurrentColumn();
                        foreach (var comment in commentsBetweenConstructors)
                        {
                            constructorCtx = constructorCtx.ReturnToIndent(commentIndentRef).NextRowToIndent();
                            var commentLocation = constructorCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            // Don't add extra blank lines after constructor comment
                            constructorCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, constructorIndentContext);
                        }
                    }

                    // Move to next line for subsequent constructors
                    constructorCtx = constructorCtx.NextRowToIndent();
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

                    var commentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > searchStartRow &&
                                    c.Range.Start.Row < arg.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (commentsBetween.Count > 0)
                    {
                        // Arguments with comments between them should be on separate lines
                        foreach (var comment in commentsBetween)
                        {
                            argCtx = argCtx.ReturnToIndent(argIndentRef).NextRowToIndent();
                            var commentLocation = argCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            argCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, argCtx);
                        }

                        // Next argument on new line
                        argCtx = argCtx.ReturnToIndent(argIndentRef).NextRowToIndent();
                        var (formattedArg, afterArg, argComments) = FormatTypeAnnotation(arg, argCtx);
                        formattedArgs.Add(formattedArg);
                        argCtx = afterArg;
                        currentComments = currentComments.AddRange(argComments);
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

                    var (fmtArg, afterFmtArg, fmtArgComments) = FormatTypeAnnotation(arg, argCtx);
                    formattedArgs.Add(fmtArg);
                    argCtx = afterFmtArg;
                    currentComments = currentComments.AddRange(fmtArgComments);
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
            return (
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.CustomTypeDeclaration(formattedTypeStruct)),
                finalContext,
                currentComments);
        }

        private static (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatInfixDeclaration(
            Declaration.InfixDeclaration infixDecl,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
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

            return (
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.InfixDeclaration(formattedInfix)),
                afterInfix,
                formattedComments);
        }

        private static (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatPortDeclaration(
            Declaration.PortDeclaration portDecl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
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

            return (
                new Stil4mElmSyntax7.Node<Declaration>(range, new Declaration.PortDeclaration(portTokenLoc, formattedSig)),
                afterType,
                formattedComments);
        }

        #endregion

        #region Type Annotation Formatting

        private (Stil4mElmSyntax7.Node<TypeAnnotation>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatTypeAnnotation(
            Stil4mElmSyntax7.Node<TypeAnnotation> typeAnnot,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null)
        {
            var startLoc = context.CurrentLocation();
            var (formattedValue, afterType, comments) = FormatTypeAnnotationValue(typeAnnot.Value, context, arrowBaseRef);
            return (MakeNodeWithRange(startLoc, afterType.CurrentLocation(), formattedValue), afterType, comments);
        }

        private (TypeAnnotation, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatTypeAnnotationValue(
            TypeAnnotation typeAnnot,
            FormattingContext context,
            FormattingContext? arrowBaseRef = null)
        {
            var emptyComments = ImmutableList<Stil4mElmSyntax7.Node<string>>.Empty;

            // Reference context for indented content (one indent level from current position)
            var indentedRef = context.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();

            switch (typeAnnot)
            {
                case TypeAnnotation.GenericType genericType:
                    return (genericType, context.Advance(genericType.Name.Length), emptyComments);

                case TypeAnnotation.Typed typed:
                    {
                        var typeName = typed.TypeName.Value;
                        var typeNameText = typeName.ModuleName.Count > 0
                            ? string.Join(".", typeName.ModuleName) + "." + typeName.Name
                            : typeName.Name;
                        var afterTypeName = context.Advance(typeNameText.Length);

                        var formattedArgs = new List<Stil4mElmSyntax7.Node<TypeAnnotation>>();
                        var currentCtx = afterTypeName;
                        var collectedComments = emptyComments;

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
                                var (formattedArg, afterArg, argComments) = FormatTypeAnnotation(arg, argContext);
                                formattedArgs.Add(formattedArg);
                                currentCtx = afterArg.ReturnToIndent(context);
                                collectedComments = collectedComments.AddRange(argComments);
                            }
                            else
                            {
                                currentCtx = currentCtx.Advance(1); // space before arg
                                var (formattedArg, afterArg, argComments) = FormatTypeAnnotation(arg, currentCtx);
                                formattedArgs.Add(formattedArg);
                                currentCtx = afterArg;
                                collectedComments = collectedComments.AddRange(argComments);
                            }
                        }

                        var formattedTyped = new TypeAnnotation.Typed(
                            typed.TypeName,
                            formattedArgs
                        );
                        return (formattedTyped, currentCtx, collectedComments);
                    }

                case TypeAnnotation.Unit:
                    return (typeAnnot, context.Advance(2), emptyComments); // "()"

                case TypeAnnotation.Tupled tupled:
                    {
                        var tupledElements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupled.TypeAnnotations);
                        var openParenLoc = context.CurrentLocation();
                        var collectedComments = emptyComments;

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

                                var (formattedElem, afterElem, elemComments) = FormatTypeAnnotation(elem, tupledCtx);
                                collectedComments = collectedComments.AddRange(elemComments);

                                if (i is 0)
                                {
                                    firstElem = formattedElem;
                                }
                                else
                                {
                                    restElems.Add((separatorLoc!, formattedElem));
                                }

                                tupledCtx = afterElem.ReturnToIndent(context);
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
                            return (formattedTupledAnnot, afterCloseParen.ReturnToIndent(context), collectedComments);
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

                                var (formattedElem, afterElem, elemComments) =
                                    FormatTypeAnnotation(tupledElements[i], tupledCtx);

                                formattedTupled.Add(formattedElem);
                                tupledCtx = afterElem;
                                collectedComments = collectedComments.AddRange(elemComments);
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
                            return (formattedTupledAnnot, afterCloseParen, collectedComments);
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

                        var (formattedArgType, afterArgType, argComments) = FormatTypeAnnotation(funcType.ArgumentType, context, effectiveArrowBaseRef);

                        Location arrowLocation;
                        Stil4mElmSyntax7.Node<TypeAnnotation> formattedReturnType;
                        FormattingContext afterReturnType;
                        ImmutableList<Stil4mElmSyntax7.Node<string>> returnComments;

                        if (returnTypeOnNewLine)
                        {
                            // Multiline: arrow on new line, at the base column
                            var newLineCtx = afterArgType.ReturnToIndent(effectiveArrowBaseRef).NextRowToIndent();
                            arrowLocation = newLineCtx.CurrentLocation();

                            if (resultTypeOnOwnLine)
                            {
                                // Arrow is on its own line, result type goes on next line with additional indentation
                                var afterArrow = newLineCtx.Advance(2); // "->" (no space after since we're going to new line)
                                // Create reference for result type at arrow base + 4
                                var resultTypeRef = effectiveArrowBaseRef.SetIndentColumn().AdvanceToNextIndentLevel().SetIndentToCurrentColumn();
                                var resultTypeCtx = afterArrow.ReturnToIndent(resultTypeRef).NextRowToIndent();
                                // Pass effectiveArrowBaseRef so nested function type arrows align properly
                                (formattedReturnType, afterReturnType, returnComments) = FormatTypeAnnotation(funcType.ReturnType, resultTypeCtx, effectiveArrowBaseRef);
                            }
                            else
                            {
                                // Arrow on new line, but result type on same line as arrow
                                var arrowCtx = newLineCtx.Advance(3); // "-> "
                                (formattedReturnType, afterReturnType, returnComments) = FormatTypeAnnotation(funcType.ReturnType, arrowCtx, effectiveArrowBaseRef);
                            }
                        }
                        else
                        {
                            // Single line: " -> "
                            var arrowCtx = afterArgType.Advance(1); // space before arrow
                            arrowLocation = arrowCtx.CurrentLocation();
                            var afterArrow = arrowCtx.Advance(3); // "-> "
                            (formattedReturnType, afterReturnType, returnComments) = FormatTypeAnnotation(funcType.ReturnType, afterArrow, effectiveArrowBaseRef);
                        }

                        var formattedFuncType = new TypeAnnotation.FunctionTypeAnnotation(
                            formattedArgType,
                            arrowLocation,
                            formattedReturnType
                        );
                        // Return with IndentSpaces reset to the original context
                        return (formattedFuncType, afterReturnType.ReturnToIndent(context), argComments.AddRange(returnComments));
                    }

                case TypeAnnotation.Record record:
                    {
                        // Format record type annotation
                        var recordFields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields);
                        var collectedComments = emptyComments;

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
                                var commentsAfterBrace = originalComments
                                    .Where(c => c.Range.Start.Row >= record.OpenBraceLocation.Row &&
                                                c.Range.Start.Row < firstFieldOriginal.Range.Start.Row &&
                                                (c.Range.Start.Row > record.OpenBraceLocation.Row ||
                                                 c.Range.Start.Column > record.OpenBraceLocation.Column))
                                    .OrderBy(c => c.Range.Start.Row)
                                    .ThenBy(c => c.Range.Start.Column)
                                    .ToList();

                                foreach (var comment in commentsAfterBrace)
                                {
                                    // If comment is on same row as opening brace, put it right after "{ "
                                    if (comment.Range.Start.Row == record.OpenBraceLocation.Row)
                                    {
                                        var commentLocation = recordFieldCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        // Move to next row for the field, indented
                                        recordFieldCtx = FormattingContext.AtPosition(
                                            commentEnd.Row + 1,
                                            fieldIndentRef.CurrentColumn, // indent past brace
                                            fieldIndentRef);
                                    }
                                    else
                                    {
                                        // Comment on its own line before first field
                                        recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                        var commentLocation = recordFieldCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        recordFieldCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, braceAlignRef);
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
                                    var allCommentsBetween = originalComments
                                        .Where(c => c.Range.Start.Row > prevFieldEnd.Row &&
                                                    c.Range.Start.Row < field.Range.Start.Row)
                                        .OrderBy(c => c.Range.Start.Row)
                                        .ThenBy(c => c.Range.Start.Column)
                                        .ToList();

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
                                            var commentLocation = recordFieldCtx.CurrentLocation();
                                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                            collectedComments = collectedComments.Add(formattedComment);
                                            recordFieldCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, braceAlignRef);
                                        }
                                    }

                                    // Move to new line, align comma with opening brace
                                    recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                    separatorLoc = recordFieldCtx.CurrentLocation();
                                    recordFieldCtx = recordFieldCtx.Advance(2); // ", "

                                    // Process comments on the same row as comma (after comma)
                                    foreach (var comment in commentsOnSeparatorRow)
                                    {
                                        var commentLocation = recordFieldCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        // Move to next row for the field name, indented past comma
                                        recordFieldCtx = FormattingContext.AtPosition(
                                            commentEnd.Row + 1,
                                            fieldIndentRef.CurrentColumn, // indent past comma
                                            fieldIndentRef);
                                    }

                                    // Process comments after separator row but before field
                                    foreach (var comment in commentsAfterSeparatorRow)
                                    {
                                        recordFieldCtx = recordFieldCtx.ReturnToIndent(braceAlignRef).NextRowToIndent();
                                        var commentLocation = recordFieldCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        recordFieldCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, braceAlignRef);
                                    }
                                }

                                var fieldStartLoc = recordFieldCtx.CurrentLocation();
                                var afterFieldName = recordFieldCtx.Advance(field.Value.FieldName.Value.Length);

                                // Check if colon is on a different line than field name
                                var isColonOnNewLine = field.Value.ColonLocation.Row > field.Value.FieldName.Range.End.Row;

                                // Check for comments between field name and colon
                                var commentsBeforeColon = originalComments
                                    .Where(c => c.Range.Start.Row > field.Value.FieldName.Range.End.Row &&
                                                c.Range.Start.Row <= field.Value.ColonLocation.Row &&
                                                (c.Range.Start.Row < field.Value.ColonLocation.Row ||
                                                 c.Range.Start.Column < field.Value.ColonLocation.Column))
                                    .OrderBy(c => c.Range.Start.Row)
                                    .ToList();

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
                                        var commentLocation = ctx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        ctx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, fieldIndentRef);
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
                                var commentsAfterColon = originalComments
                                    .Where(c => c.Range.Start.Row > field.Value.ColonLocation.Row &&
                                                c.Range.Start.Row < field.Value.FieldType.Range.Start.Row)
                                    .OrderBy(c => c.Range.Start.Row)
                                    .ToList();

                                // Check for comments on the same line as the type, BEFORE the type
                                var commentsOnSameLineAsType = originalComments
                                    .Where(c => c.Range.Start.Row == field.Value.FieldType.Range.Start.Row &&
                                                c.Range.Start.Column < field.Value.FieldType.Range.Start.Column &&
                                                c.Range.Start.Row > field.Value.ColonLocation.Row)
                                    .OrderBy(c => c.Range.Start.Column)
                                    .ToList();

                                Stil4mElmSyntax7.Node<TypeAnnotation> formattedFieldType;
                                FormattingContext afterFieldType;
                                ImmutableList<Stil4mElmSyntax7.Node<string>> fieldTypeComments;

                                if (isFieldTypeMultiline || commentsAfterColon.Count > 0)
                                {
                                    var ctx = afterColon;
                                    foreach (var comment in commentsAfterColon)
                                    {
                                        ctx = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent(); // indent for type
                                        var commentLocation = ctx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        ctx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, fieldTypeIndentRef);
                                    }
                                    // Field type on new line with extra indentation
                                    var fieldTypeContext = ctx.ReturnToIndent(fieldTypeIndentRef).NextRowToIndent(); // Set proper indent

                                    // Handle inline comments that appear on the same line as the type
                                    foreach (var comment in commentsOnSameLineAsType)
                                    {
                                        var commentLocation = fieldTypeContext.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        collectedComments = collectedComments.Add(formattedComment);
                                        fieldTypeContext = FormattingContext.AtPosition(
                                            commentEnd.Row,
                                            commentEnd.Column + 1, // space after comment
                                            fieldTypeContext);
                                    }

                                    (formattedFieldType, afterFieldType, fieldTypeComments) = FormatTypeAnnotation(field.Value.FieldType, fieldTypeContext);
                                }
                                else
                                {
                                    (formattedFieldType, afterFieldType, fieldTypeComments) = FormatTypeAnnotation(field.Value.FieldType, afterColon);
                                }
                                collectedComments = collectedComments.AddRange(fieldTypeComments);

                                var formattedFieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

                                var formattedField = new RecordField(
                                    formattedFieldNameNode,
                                    colonLoc,
                                    formattedFieldType
                                );

                                var formattedFieldNode = MakeNodeWithRange(fieldStartLoc, afterFieldType.CurrentLocation(), formattedField);

                                if (i is 0)
                                {
                                    firstField = formattedFieldNode;
                                }
                                else
                                {
                                    restFields.Add((separatorLoc!, formattedFieldNode));
                                }

                                recordFieldCtx = afterFieldType.ReturnToIndent(context);
                            }

                            // Check for comments before the closing brace
                            var lastField = recordFields.Count > 0 ? recordFields[recordFields.Count - 1] : null;
                            var commentsBeforeCloseBrace = originalComments
                                .Where(c => lastField is not null &&
                                            c.Range.Start.Row > lastField.Value.FieldType.Range.End.Row &&
                                            c.Range.Start.Row < record.CloseBraceLocation.Row)
                                .OrderBy(c => c.Range.Start.Row)
                                .ToList();

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
                                    var commentLocation = recordFieldCtx.CurrentLocation();
                                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                    collectedComments = collectedComments.Add(formattedComment);
                                    recordFieldCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, braceAlignRef);
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
                            return (formattedRecord, afterRecordCloseBrace.ReturnToIndent(context), collectedComments);
                        }
                        else
                        {
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

                                var (formattedFieldType, afterFieldType, fieldTypeComments) = FormatTypeAnnotation(field.Value.FieldType, afterColon);
                                collectedComments = collectedComments.AddRange(fieldTypeComments);

                                var formattedFieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.Value.FieldName.Value);

                                var formattedField = new RecordField(
                                    formattedFieldNameNode,
                                    colonLoc,
                                    formattedFieldType
                                );

                                formattedRecordFields.Add(MakeNodeWithRange(fieldStartLoc, afterFieldType.CurrentLocation(), formattedField));

                                recordFieldCtx = afterFieldType;
                            }

                            var recordCloseBraceLoc = recordFieldCtx.Advance(1).CurrentLocation(); // " }"
                            var afterRecordCloseBrace = recordFieldCtx.Advance(2);

                            var formattedRecordDef = new RecordDefinition(ToSeparatedSyntaxList(formattedRecordFields));
                            var formattedRecord = new TypeAnnotation.Record(
                                recordOpenBraceLoc,
                                formattedRecordDef,
                                recordCloseBraceLoc
                            );
                            return (formattedRecord, afterRecordCloseBrace, collectedComments);
                        }
                    }

                case TypeAnnotation.GenericRecord genericRecord:
                    {
                        var genericRecordText = RenderTypeAnnotationText(typeAnnot);
                        return (genericRecord, context.Advance(genericRecordText.Length), emptyComments);
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
                    "{ " + string.Join(", ", Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields).Select(f => f.Value.FieldName.Value + " : " + RenderTypeAnnotationText(f.Value.FieldType.Value))) + " }",
                TypeAnnotation.GenericRecord genericRecord =>
                    "{ " + genericRecord.GenericName.Value + " | " +
                    string.Join(", ", Stil4mElmSyntax7.FromStil4mConcretized.ToList(genericRecord.RecordDefinition.Value.Fields).Select(f => f.Value.FieldName.Value + " : " + RenderTypeAnnotationText(f.Value.FieldType.Value))) + " }",
                _ => throw new System.NotImplementedException($"Type annotation text rendering not implemented for: {typeAnnot.GetType().Name}")
            };
        }

        #endregion

        #region Expression Formatting

        private FormattingResult<Stil4mElmSyntax7.Node<Expression>> FormatExpression(
            Stil4mElmSyntax7.Node<Expression> expr,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var startLoc = context.CurrentLocation();
            var result = FormatExpressionValue(expr.Value, expr.Range, context, comments);
            return new FormattingResult<Stil4mElmSyntax7.Node<Expression>>(
                MakeNodeWithRange(startLoc, result.Context.CurrentLocation(), result.FormattedNode),
                result.Context,
                result.Comments);
        }

        private FormattingResult<Expression> FormatExpressionValue(
            Expression expr,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            switch (expr)
            {
                case Expression.UnitExpr:
                    return FormattingResult<Expression>.Create(expr, context.Advance(2), comments); // "()"

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
                        return FormattingResult<Expression>.Create(literal, afterCloseQuotes, comments);
                    }
                    else
                    {
                        // Use the rendered representation to calculate the correct length
                        // since the value may contain escaped characters
                        var renderedLiteral = Rendering.RenderStringLiteral(literal.Value, isTripleQuoted: false);
                        return FormattingResult<Expression>.Create(literal, context.Advance(renderedLiteral.Length), comments);
                    }

                case Expression.CharLiteral charLit:
                    // Use the actual rendered length which varies for escaped characters
                    return FormattingResult<Expression>.Create(charLit, context.Advance(Rendering.RenderCharLiteral(charLit.Value).Length), comments);

                case Expression.Integer intLit:
                    return FormattingResult<Expression>.Create(intLit, context.Advance(intLit.Value.ToString().Length), comments);

                case Expression.Hex hexLit:
                    // Use the same hex rendering logic as the Renderer
                    return FormattingResult<Expression>.Create(hexLit, context.Advance(Rendering.RenderHexPattern(hexLit.Value).Length), comments);

                case Expression.Floatable floatLit:
                    return FormattingResult<Expression>.Create(floatLit, context.Advance(floatLit.Value.ToString().Length), comments);

                case Expression.FunctionOrValue funcOrVal:
                    var funcName = funcOrVal.ModuleName.Count > 0
                        ? string.Join(".", funcOrVal.ModuleName) + "." + funcOrVal.Name
                        : funcOrVal.Name;
                    return FormattingResult<Expression>.Create(funcOrVal, context.Advance(funcName.Length), comments);

                case Expression.Negation negation:
                    {
                        var afterNegSign = context.Advance(1); // "-"
                        var negResult = FormatExpression(negation.Expression, afterNegSign, comments);
                        return new FormattingResult<Expression>(
                            new Expression.Negation(negResult.FormattedNode),
                            negResult.Context,
                            negResult.Comments);
                    }

                case Expression.Application app:
                    {
                        // Check if application spans multiple lines based on the containing node's range only
                        var isMultiline = SpansMultipleRows(originalRange);

                        var formattedArgs = new List<Stil4mElmSyntax7.Node<Expression>>();
                        var appCtx = context;
                        var currentComments = comments;

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
                                        var commentsBetween = GetCommentsBetweenRows(
                                            prevArg.Range.End.Row,
                                            arg.Range.Start.Row);

                                        for (var ci = 0; ci < commentsBetween.Count; ci++)
                                        {
                                            var comment = commentsBetween[ci];
                                            hadComments = true;

                                            // Only move to new line before the FIRST comment
                                            // For subsequent comments, CreateContextAfterComment already positioned us
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
                                            var commentLocation = appCtx.CurrentLocation();
                                            var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                            currentComments = currentComments.Add(formattedComment);
                                            appCtx = CreateContextAfterComment(appCtx, commentEnd, endsWithNewline);
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

                                var argResult = FormatExpression(arg, appCtx, currentComments);
                                formattedArgs.Add(argResult.FormattedNode);
                                appCtx = argResult.Context.ReturnToIndent(context);
                                currentComments = argResult.Comments;
                            }
                        }
                        else
                        {
                            // Single line: arguments separated by spaces
                            for (var i = 0; i < app.Arguments.Count; i++)
                            {
                                if (i > 0) appCtx = appCtx.Advance(1); // space
                                var argResult = FormatExpression(app.Arguments[i], appCtx, currentComments);
                                formattedArgs.Add(argResult.FormattedNode);
                                appCtx = argResult.Context;
                                currentComments = argResult.Comments;
                            }
                        }

                        return FormattingResult<Expression>.Create(new Expression.Application(formattedArgs), appCtx, currentComments);
                    }

                case Expression.RecordExpr recordExpr:
                    {
                        var openBraceLoc = context.CurrentLocation();
                        var afterOpenBrace = context.Advance(1); // "{"
                        var recordFields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(recordExpr.Fields);

                        // Get original separator locations if available
                        IReadOnlyList<Location>? originalSeparatorLocations = null;
                        if (recordExpr.Fields is SeparatedSyntaxList<RecordExprField>.NonEmpty nonEmpty)
                        {
                            originalSeparatorLocations = nonEmpty.Rest.Select(r => r.SeparatorLocation).ToList();
                        }

                        if (recordFields.Count is 0)
                        {
                            return FormattingResult<Expression>.Create(
                                new Expression.RecordExpr(recordExpr.Fields),
                                afterOpenBrace.Advance(1), comments); // "}"
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
                            var currentComments = comments;

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
                                var commentsBeforeValue = GetCommentsBetweenRows(equalsRow, valueStartRow);
                                foreach (var comment in commentsBeforeValue)
                                {
                                    var commentLocation = valueContext.CurrentLocation();
                                    var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                    currentComments = currentComments.Add(formattedComment);
                                    valueContext = CreateContextAfterComment(valueContext, commentEnd, endsWithNewline);
                                    // CreateContextAfterComment already positions us on the correct row
                                    // (it advances to next row if comment didn't end with newline)
                                    // Just need to set the column
                                    valueContext = valueContext.ReturnToIndent(targetRef).SetIndentColumn();
                                }

                                firstFieldResult = FormatExpression(firstField.ValueExpr, valueContext, currentComments);
                            }
                            else
                            {
                                firstEqualsLoc = afterFirstFieldName.Advance(1).CurrentLocation(); // " = "
                                var afterFirstEq = afterFirstFieldName.Advance(3); // " = "
                                firstFieldResult = FormatExpression(firstField.ValueExpr, afterFirstEq, currentComments);
                            }
                            currentComments = firstFieldResult.Comments;

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
                                var commentsBeforeComma = GetCommentsBetweenRows(
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

                                    var commentLocation = recordCtx.CurrentLocation();
                                    var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                    currentComments = currentComments.Add(formattedComment);
                                    recordCtx = CreateContextAfterComment(recordCtx, commentEnd, endsWithNewline);
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
                                var commentsAfterComma = originalComments
                                    .Where(c => c.Range.Start.Row >= originalSeparatorRow &&
                                               c.Range.Start.Row < field.FieldName.Range.Start.Row)
                                    .OrderBy(c => c.Range.Start.Row)
                                    .ToList();

                                foreach (var comment in commentsAfterComma)
                                {
                                    var commentLocation = recordCtx.CurrentLocation();
                                    var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                    currentComments = currentComments.Add(formattedComment);
                                    recordCtx = CreateContextAfterComment(recordCtx, commentEnd, endsWithNewline);
                                    // After comment, reposition for field name (CreateContextAfterComment leaves us at column 1)
                                    recordCtx = recordCtx.ReturnToIndent(afterCommaRef).SetIndentColumn(); // After ", "
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
                                    var commentsBeforeVal = GetCommentsBetweenRows(eqRow, valStartRow);
                                    foreach (var comment in commentsBeforeVal)
                                    {
                                        var commentLocation = valueContext.CurrentLocation();
                                        var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                        currentComments = currentComments.Add(formattedComment);
                                        valueContext = CreateContextAfterComment(valueContext, commentEnd, endsWithNewline);
                                        // CreateContextAfterComment already positions us on the correct row
                                        // Just need to set the column
                                        valueContext = valueContext.ReturnToIndent(fieldIndentRef).SetIndentColumn();
                                    }

                                    fieldResult = FormatExpression(field.ValueExpr, valueContext, currentComments);
                                }
                                else
                                {
                                    equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                                    var afterEq = afterFieldName.Advance(3); // " = "
                                    fieldResult = FormatExpression(field.ValueExpr, afterEq, currentComments);
                                }
                                currentComments = fieldResult.Comments;

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
                            ), afterRecordClose.ReturnToIndent(context), currentComments);
                        }
                        else
                        {
                            // Single line record: { field1 = val1, field2 = val2 }
                            var afterOpenSpace = afterOpenBrace.Advance(1); // " "
                            var formattedRecordFields = new List<RecordExprField>();
                            var recordCtx = afterOpenSpace;
                            var currentComments = comments;

                            for (var i = 0; i < recordFields.Count; i++)
                            {
                                if (i > 0) recordCtx = recordCtx.Advance(2); // ", "
                                var field = recordFields[i];
                                var fieldStartLoc = recordCtx.CurrentLocation();
                                var afterFieldName = recordCtx.Advance(field.FieldName.Value.Length);
                                var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                                var afterEq = afterFieldName.Advance(3); // " = "
                                var fieldResult = FormatExpression(field.ValueExpr, afterEq, currentComments);
                                currentComments = fieldResult.Comments;

                                var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                                formattedRecordFields.Add(new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode));

                                recordCtx = fieldResult.Context;
                            }

                            var afterFieldsSpace = recordCtx.Advance(1); // " "
                            var afterRecordClose = afterFieldsSpace.Advance(1); // "}"

                            return FormattingResult<Expression>.Create(new Expression.RecordExpr(
                                ToSeparatedSyntaxList(formattedRecordFields)
                            ), afterRecordClose, currentComments);
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
                                context.Advance(2), comments); // "[]"
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
                            var currentComments = comments;

                            // Create reference context at opening bracket column for alignment
                            var listAlignRef = context.SetIndentToCurrentColumn();
                            // Create reference for content indented past bracket
                            var listContentRef = context.Advance(2).SetIndentToCurrentColumn();

                            // Check for comments before the first element
                            // Use originalRange.Start for the opening bracket location
                            var commentsBeforeFirst = originalComments
                                .Where(c => c.Range.Start.Row >= originalRange.Start.Row &&
                                            c.Range.Start.Row < elements[0].Range.Start.Row &&
                                            c.Range.Start.Column > originalRange.Start.Column)
                                .OrderBy(c => c.Range.Start.Row)
                                .ToList();

                            var firstElemCtx = afterListOpen;
                            if (commentsBeforeFirst.Count > 0)
                            {
                                // Comment(s) after "[ " but before first element
                                foreach (var comment in commentsBeforeFirst)
                                {
                                    var commentLocation = firstElemCtx.CurrentLocation();
                                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                    currentComments = currentComments.Add(formattedComment);
                                    // Next line for element, indented
                                    firstElemCtx = FormattingContext.AtPosition(
                                        commentEnd.Row + 1,
                                        listContentRef.CurrentColumn, // indent past bracket
                                        listContentRef);
                                }
                            }

                            // First element 
                            var firstElemResult = FormatExpression(elements[0], firstElemCtx, currentComments);
                            currentComments = firstElemResult.Comments;
                            var elemCtx = firstElemResult.Context;

                            // Check for trailing comment on the same row as the first element
                            var firstElemTrailingComment = GetTrailingComment(elements[0].Range);

                            if (firstElemTrailingComment is not null)
                            {
                                // Add space and trailing comment
                                elemCtx = elemCtx.Advance(1); // space before comment
                                var trailingCommentLocation = elemCtx.CurrentLocation();
                                var trailingCommentEnd = CalculateCommentEndLocation(trailingCommentLocation, firstElemTrailingComment.Value);
                                var formattedTrailingComment = firstElemTrailingComment.WithRange(trailingCommentLocation, trailingCommentEnd);
                                currentComments = currentComments.Add(formattedTrailingComment);
                                elemCtx = FormattingContext.AtPosition(trailingCommentEnd.Row, trailingCommentEnd.Column, context);
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
                                var commentsBetween = GetCommentsBetweenRanges(prevElem.Range, currElem.Range).ToList();

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
                                        var commentLocation = elemCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, firstComment.Value);
                                        var formattedComment = firstComment.WithRange(commentLocation, commentEnd);
                                        currentComments = currentComments.Add(formattedComment);

                                        // Element on next line, indented by 2 more spaces
                                        elemCtx = FormattingContext.AtPosition(
                                            commentEnd.Row + 1,
                                            listContentRef.CurrentColumn, // indent past comma
                                            listContentRef);
                                        var elemResult = FormatExpression(currElem, elemCtx, currentComments);
                                        currentComments = elemResult.Comments;
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
                                            var commentLocation = elemCtx.CurrentLocation();
                                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                            currentComments = currentComments.Add(formattedComment);
                                            // Next line for next comment or element
                                            elemCtx = FormattingContext.AtPosition(commentEnd.Row + 1, listAlignRef.CurrentColumn, listAlignRef);
                                        }

                                        // Now add the separator and element on this line
                                        separatorLoc = elemCtx.CurrentLocation();
                                        elemCtx = elemCtx.Advance(2); // ", "
                                        var elemResult = FormatExpression(currElem, elemCtx, currentComments);
                                        currentComments = elemResult.Comments;
                                        restItems.Add((separatorLoc, elemResult.FormattedNode));
                                        elemCtx = elemResult.Context.ReturnToIndent(context);
                                    }
                                }
                                else
                                {
                                    elemCtx = elemCtx.Advance(2); // ", "
                                    var elemResult = FormatExpression(currElem, elemCtx, currentComments);
                                    currentComments = elemResult.Comments;
                                    restItems.Add((separatorLoc, elemResult.FormattedNode));
                                    elemCtx = elemResult.Context;

                                    // Check for trailing comment on the same row as the element
                                    var trailingComment = GetTrailingComment(currElem.Range);

                                    if (trailingComment is not null)
                                    {
                                        // Add space and trailing comment
                                        elemCtx = elemCtx.Advance(1); // space before comment
                                        var trailingCommentLocation = elemCtx.CurrentLocation();
                                        var trailingCommentEnd = CalculateCommentEndLocation(trailingCommentLocation, trailingComment.Value);
                                        var formattedTrailingComment = trailingComment.WithRange(trailingCommentLocation, trailingCommentEnd);
                                        currentComments = currentComments.Add(formattedTrailingComment);
                                        elemCtx = FormattingContext.AtPosition(trailingCommentEnd.Row, trailingCommentEnd.Column, context);
                                    }
                                }
                            }

                            // Check for comments between last element and closing bracket
                            var lastElem = elements[elements.Count - 1];
                            // Look for comments after the last element and before the closing bracket
                            var commentsBeforeClose = originalComments
                                .Where(c => c.Range.Start.Row > lastElem.Range.End.Row &&
                                           c.Range.Start.Row < originalRange.End.Row)
                                .OrderBy(c => c.Range.Start.Row)
                                .ToList();

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
                                var commentLocation = elemCtx.CurrentLocation();
                                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                currentComments = currentComments.Add(formattedComment);
                                elemCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, listAlignRef);
                            }

                            // Closing bracket on new line, aligned with opening bracket
                            var closeCtx = elemCtx.ReturnToIndent(listAlignRef).NextRowToIndent();
                            var multilineListCloseLoc = closeCtx.CurrentLocation();
                            var afterMultilineListClose = closeCtx.Advance(1); // "]"

                            var multilineSeparatedList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty(
                                firstElemResult.FormattedNode, restItems);

                            return FormattingResult<Expression>.Create(new Expression.ListExpr(
                                multilineSeparatedList
                            ), afterMultilineListClose.ReturnToIndent(context), currentComments);
                        }
                        else
                        {
                            // Single line list
                            var afterListOpen = context.Advance(2); // "[ "
                            var formattedElements = new List<Stil4mElmSyntax7.Node<Expression>>();
                            var listCtx = afterListOpen;
                            var currentComments = comments;

                            for (var i = 0; i < elements.Count; i++)
                            {
                                if (i > 0) listCtx = listCtx.Advance(2); // ", "
                                var elemResult = FormatExpression(elements[i], listCtx, currentComments);
                                currentComments = elemResult.Comments;
                                formattedElements.Add(elemResult.FormattedNode);
                                listCtx = elemResult.Context;
                            }

                            var afterListSpace = listCtx.Advance(1); // " "
                            var afterListClose = afterListSpace.Advance(1); // "]"

                            return FormattingResult<Expression>.Create(new Expression.ListExpr(
                                ToSeparatedSyntaxList(formattedElements)
                            ), afterListClose, currentComments);
                        }
                    }

                case Expression.ParenthesizedExpression parenExpr:
                    {
                        var openParenLoc = context.CurrentLocation();

                        // Create reference context at opening paren for close paren alignment
                        var openParenRef = context.SetIndentToCurrentColumn();

                        // Check if the original content was multiline AND contains a let expression
                        // Let expressions need IndentSpaces adjusted so `in` aligns with `let`
                        var originalIsMultiline = parenExpr.CloseParenLocation.Row > parenExpr.OpenParenLocation.Row;
                        var containsLetExpr = parenExpr.Expression.Value is Expression.LetExpression;

                        // For multiline let expressions, set indent at `(` so nested `in` aligns with `(`
                        // Set indent BEFORE advancing past `(`, then advance
                        var afterOpenParen = originalIsMultiline && containsLetExpr
                            ? context.SetIndentToCurrentColumn().Advance(1)
                            : context.Advance(1);
                        var innerResult = FormatExpression(parenExpr.Expression, afterOpenParen, comments);

                        // Determine if closing paren should be on new line.
                        // The closing paren goes on a new line if:
                        // 1. It was on a different row than the end of the inner expression in the original source, OR
                        // 2. The inner expression's Range.End.Row is artificially inflated (larger than CloseParenLocation.Row)
                        //    AND the formatted content spans multiple rows
                        //    (this handles cases where SnapshotTestFormat expands single-line parens to multiline)
                        bool closeParenOnNewLine;
                        if (parenExpr.CloseParenLocation.Row > parenExpr.Expression.Range.End.Row)
                        {
                            // Normal case: close paren is on a different row than expression end
                            closeParenOnNewLine = true;
                        }
                        else if (parenExpr.Expression.Range.End.Row > parenExpr.CloseParenLocation.Row
                            && innerResult.Context.CurrentRow > afterOpenParen.CurrentRow)
                        {
                            // Artificially inflated range AND formatted content is multiline
                            // Put close paren on new line
                            closeParenOnNewLine = true;
                        }
                        else
                        {
                            // Close paren is on same row as expression end (or very close) - stay on same line
                            closeParenOnNewLine = false;
                        }

                        Location closeParenLoc;
                        FormattingContext afterCloseParen;

                        if (closeParenOnNewLine)
                        {
                            // Closing paren on new line, aligned with opening paren
                            var closeCtx = innerResult.Context.ReturnToIndent(openParenRef).NextRowToIndent();
                            closeParenLoc = closeCtx.CurrentLocation();
                            afterCloseParen = closeCtx.Advance(1); // ")"
                        }
                        else
                        {
                            // Closing paren on same line as end of inner expression
                            closeParenLoc = innerResult.Context.CurrentLocation();
                            afterCloseParen = innerResult.Context.Advance(1); // ")"
                        }

                        return new FormattingResult<Expression>(
                            new Expression.ParenthesizedExpression(openParenLoc, innerResult.FormattedNode, closeParenLoc),
                            afterCloseParen, innerResult.Comments);
                    }

                case Expression.OperatorApplication opApp:
                    {
                        var leftResult = FormatExpression(opApp.Left, context, comments);
                        var currentComments = leftResult.Comments;

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
                                var pipeRightResult = FormatExpression(opApp.Right, pipeRightContext, currentComments);

                                return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                    formattedOperator,
                                    opApp.Direction,
                                    leftResult.FormattedNode,
                                    pipeRightResult.FormattedNode
                                ), pipeRightResult.Context.ReturnToIndent(context), pipeRightResult.Comments);
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
                            var commentsBeforeOperator = GetCommentsBetweenRows(
                                opApp.Left.Range.End.Row, opApp.Operator.Range.Start.Row);

                            var newLineCtx = leftResult.Context.ReturnToIndent(targetRef).NextRowToIndent();

                            // Format comments between left operand and operator
                            foreach (var comment in commentsBeforeOperator)
                            {
                                var commentLocation = newLineCtx.CurrentLocation();
                                var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                currentComments = currentComments.Add(formattedComment);
                                newLineCtx = CreateContextAfterComment(newLineCtx, commentEnd, endsWithNewline);
                                // Align next comment or operator at target column
                                newLineCtx = newLineCtx.ReturnToIndent(targetRef).SetIndentColumn();
                            }

                            var opStart = newLineCtx.CurrentLocation();
                            var afterOp = newLineCtx.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();

                            // Create operator node with its new position
                            var formattedOp = MakeNodeWithRange(opStart, opEnd, opApp.Operator.Value);

                            // Check for comments on the same line as the operator (trailing comment after operator)
                            var trailingOpComment = GetTrailingComment(opApp.Operator.Range);
                            FormattingContext rightContext;

                            if (trailingOpComment is not null)
                            {
                                // Comment immediately after operator: "&&" followed by " -- comment" then newline
                                var afterOpSpace = afterOp.Advance(1); // " "
                                var commentLocation = afterOpSpace.CurrentLocation();
                                var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, trailingOpComment.Value);
                                var formattedComment = trailingOpComment.WithRange(commentLocation, commentEnd);
                                currentComments = currentComments.Add(formattedComment);

                                // After the comment, right operand goes on a new line at column after operator + space
                                var afterCommentRow = commentEnd.Row + (endsWithNewline ? 0 : 1);
                                // Align right operand to the column after operator + 1 space
                                var rightIndentCol = opEnd.Column + 1;
                                // Use newLineCtx which has the correct indent (targetColumn - 1)
                                rightContext = FormattingContext.AtPosition(afterCommentRow, rightIndentCol, newLineCtx);
                            }
                            else
                            {
                                // No trailing comment - space after operator
                                // IndentSpaces is already set to targetColumn - 1 from SetIndentToCurrentColumn
                                rightContext = afterOp.Advance(1); // " "
                            }

                            // Look for comments between operator and right operand (on separate lines)
                            var commentsBeforeRight = GetCommentsBetweenRows(
                                opApp.Operator.Range.End.Row, opApp.Right.Range.Start.Row);

                            // Format comments between operator and right operand
                            foreach (var comment in commentsBeforeRight)
                            {
                                var commentLocation = rightContext.CurrentLocation();
                                var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                currentComments = currentComments.Add(formattedComment);
                                rightContext = CreateContextAfterComment(rightContext, commentEnd, endsWithNewline);
                                // Maintain alignment for next comment or right operand
                                rightContext = rightContext.SetIndentColumn();
                            }

                            var rightResult = FormatExpression(opApp.Right, rightContext, currentComments);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context.ReturnToIndent(context), rightResult.Comments);
                        }
                        else
                        {
                            // Single line
                            var afterLeftSpace = leftResult.Context.Advance(1); // " "
                            var opStart = afterLeftSpace.CurrentLocation();
                            var afterOp = afterLeftSpace.Advance(opApp.Operator.Value.Length);
                            var opEnd = afterOp.CurrentLocation();
                            var afterOpSpace = afterOp.Advance(1); // " "
                            var rightResult = FormatExpression(opApp.Right, afterOpSpace, currentComments);

                            // Create operator node with its new position
                            var formattedOp = MakeNodeWithRange(opStart, opEnd, opApp.Operator.Value);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                formattedOp,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context, rightResult.Comments);
                        }
                    }

                case Expression.PrefixOperator prefixOp:
                    return FormattingResult<Expression>.Create(prefixOp, context.Advance(prefixOp.Operator.Length + 2), comments); // "(op)"

                case Expression.RecordAccess recordAccess:
                    {
                        var recordResult = FormatExpression(recordAccess.Record, context, comments);
                        var afterDot = recordResult.Context.Advance(1); // "."
                        var fieldNameLoc = afterDot.CurrentLocation();
                        var afterFieldAccess = afterDot.Advance(recordAccess.FieldName.Value.Length);
                        var fieldNameWithLoc = MakeNodeWithRange(fieldNameLoc, afterFieldAccess.CurrentLocation(), recordAccess.FieldName.Value);

                        return FormattingResult<Expression>.Create(
                            new Expression.RecordAccess(recordResult.FormattedNode, fieldNameWithLoc),
                            afterFieldAccess, recordResult.Comments);
                    }

                case Expression.RecordAccessFunction accessFunc:
                    return FormattingResult<Expression>.Create(accessFunc, context.Advance(accessFunc.FunctionName.Length), comments);

                case Expression.IfBlock ifBlock:
                    return FormatIfBlock(ifBlock, context, comments);

                case Expression.CaseExpression caseExpr:
                    return FormatCaseExpression(caseExpr, context, comments);

                case Expression.LetExpression letExpr:
                    return FormatLetExpression(letExpr, context, comments);

                case Expression.LambdaExpression lambdaExpr:
                    return FormatLambdaExpression(lambdaExpr, context, comments);

                case Expression.TupledExpression tupledExpr:
                    return FormatTupledExpression(tupledExpr, originalRange, context, comments);

                case Expression.RecordUpdateExpression recordUpdate:
                    return FormatRecordUpdateExpression(recordUpdate, originalRange, context, comments);

                default:
                    throw new System.NotImplementedException(
                        $"Expression formatting not implemented for: {expr.GetType().Name} " +
                        $"at row {originalRange.Start.Row}, column {originalRange.Start.Column}");
            }
        }

        private FormattingResult<Expression> FormatIfBlock(
            Expression.IfBlock ifBlock,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments,
            int? chainBaseColumn = null)
        {
            // Check if this is a chained else-if (else block is another if)
            var isElseIf = ifBlock.ElseBlock.Value is Expression.IfBlock;
            var currentComments = comments;

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
                var commentsBeforeCondition = originalComments
                    .Where(c => c.Range.Start.Row > ifBlock.IfTokenLocation.Row &&
                                c.Range.Start.Row < ifBlock.Condition.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                if (commentsBeforeCondition.Count > 0)
                {
                    var conditionColumn = conditionContext.CurrentColumn;
                    foreach (var comment in commentsBeforeCondition)
                    {
                        var commentLocation = conditionContext.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        // Next line for next comment or condition
                        conditionContext = FormattingContext.AtPosition(commentEnd.Row + 1, conditionColumn, context);
                    }
                }

                conditionResult = FormatExpression(ifBlock.Condition, conditionContext, currentComments);
                currentComments = conditionResult.Comments;

                // Check for comments between condition and "then"
                var commentsAfterCondition = originalComments
                    .Where(c => c.Range.Start.Row > ifBlock.Condition.Range.End.Row &&
                                c.Range.Start.Row < ifBlock.ThenTokenLocation.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                var afterConditionCtx = conditionResult.Context;
                if (commentsAfterCondition.Count > 0)
                {
                    // Comments between condition and "then" - indent them like condition
                    foreach (var comment in commentsAfterCondition)
                    {
                        afterConditionCtx = afterConditionCtx.ReturnToIndent(bodyRef).NextRowToIndent();
                        var commentLocation = afterConditionCtx.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        afterConditionCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, bodyRef);
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
                conditionResult = FormatExpression(ifBlock.Condition, afterIf, currentComments);
                currentComments = conditionResult.Comments;

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
            var commentsBeforeThenBlock = originalComments
                .Where(c => c.Range.Start.Row >= ifBlock.ThenTokenLocation.Row &&
                            c.Range.Start.Row < ifBlock.ThenBlock.Range.Start.Row &&
                            (c.Range.Start.Row > ifBlock.ThenTokenLocation.Row ||
                             c.Range.Start.Column > ifBlock.ThenTokenLocation.Column + 4)) // past "then"
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            if (commentsBeforeThenBlock.Count > 0)
            {
                var thenBodyColumn = thenContext.CurrentColumn;
                foreach (var comment in commentsBeforeThenBlock)
                {
                    var commentLocation = thenContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    // Next line for next comment or then-block
                    // Use thenContextReference which has the correct indent (bodyColumn - 1)
                    thenContext = FormattingContext.AtPosition(commentEnd.Row + 1, thenBodyColumn, thenContextReference);
                }
            }

            var thenResult = FormatExpression(ifBlock.ThenBlock, thenContext, currentComments);
            currentComments = thenResult.Comments;

            // Empty line before else - align with the base column
            var elseContext = thenResult.Context.WithBlankLine().ReturnToIndent(effectiveBaseRef).SetIndentColumn();

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
                    var commentsBeforeInnerCondition = originalComments
                        .Where(c => c.Range.Start.Row > innerIfBlock.IfTokenLocation.Row &&
                                    c.Range.Start.Row < innerIfBlock.Condition.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (commentsBeforeInnerCondition.Count > 0)
                    {
                        var innerCondColumn = innerConditionContext.CurrentColumn;
                        foreach (var comment in commentsBeforeInnerCondition)
                        {
                            var commentLocation = innerConditionContext.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            innerConditionContext = FormattingContext.AtPosition(commentEnd.Row + 1, innerCondColumn, context);
                        }
                    }

                    innerConditionResult = FormatExpression(innerIfBlock.Condition, innerConditionContext, currentComments);
                    currentComments = innerConditionResult.Comments;

                    // Check for comments between inner condition and inner "then"
                    var commentsAfterInnerCondition = originalComments
                        .Where(c => c.Range.Start.Row > innerIfBlock.Condition.Range.End.Row &&
                                    c.Range.Start.Row < innerIfBlock.ThenTokenLocation.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    var afterInnerConditionCtx = innerConditionResult.Context;
                    if (commentsAfterInnerCondition.Count > 0)
                    {
                        foreach (var comment in commentsAfterInnerCondition)
                        {
                            afterInnerConditionCtx = afterInnerConditionCtx.ReturnToIndent(bodyRef).NextRowToIndent();
                            var commentLocation = afterInnerConditionCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            afterInnerConditionCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, bodyRef);
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
                    innerConditionResult = FormatExpression(innerIfBlock.Condition, afterInnerIf, currentComments);
                    currentComments = innerConditionResult.Comments;

                    // " then"
                    var afterInnerCondSpace = innerConditionResult.Context.Advance(1);
                    innerThenLoc = afterInnerCondSpace.CurrentLocation();
                    afterInnerThen = afterInnerCondSpace.Advance(4);
                }

                // Check for comments between inner "then" and inner then-block
                // Also set IndentSpaces so nested expressions indent correctly
                var innerThenContextReference = afterInnerThen.ReturnToIndent(bodyRef).NextRowToIndent();
                var innerThenContext = innerThenContextReference;
                var commentsBeforeInnerThenBlock = originalComments
                    .Where(c => c.Range.Start.Row >= innerIfBlock.ThenTokenLocation.Row &&
                                c.Range.Start.Row < innerIfBlock.ThenBlock.Range.Start.Row &&
                                (c.Range.Start.Row > innerIfBlock.ThenTokenLocation.Row ||
                                 c.Range.Start.Column > innerIfBlock.ThenTokenLocation.Column + 4))
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                if (commentsBeforeInnerThenBlock.Count > 0)
                {
                    var innerThenBodyColumn = innerThenContext.CurrentColumn;
                    foreach (var comment in commentsBeforeInnerThenBlock)
                    {
                        var commentLocation = innerThenContext.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        // Use innerThenContextReference which has the correct indent (bodyColumn - 1)
                        innerThenContext = FormattingContext.AtPosition(commentEnd.Row + 1, innerThenBodyColumn, innerThenContextReference);
                    }
                }

                // Inner then block - indented from the base column (same as outer if)
                var innerThenResult = FormatExpression(innerIfBlock.ThenBlock, innerThenContext, currentComments);
                currentComments = innerThenResult.Comments;

                // Inner else - recurse for the rest of the chain - align with base column
                var innerElseContext = innerThenResult.Context.WithBlankLine().ReturnToIndent(effectiveBaseRef).SetIndentColumn();
                var innerElseTokenLoc = innerElseContext.CurrentLocation();

                Stil4mElmSyntax7.Node<Expression> formattedInnerElseBlock;
                FormattingContext afterInnerElseBlock;

                if (innerIfBlock.ElseBlock.Value is Expression.IfBlock nestedIf)
                {
                    // Continue the chain - pass the base column
                    var afterInnerElse = innerElseContext.Advance(5); // "else "
                    var nestedIfResult = FormatIfBlock(nestedIf, afterInnerElse, currentComments, effectiveBaseColumn);
                    currentComments = nestedIfResult.Comments;
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
                    var commentsBeforeInnerElseBlock = originalComments
                        .Where(c => c.Range.Start.Row >= innerIfBlock.ElseTokenLocation.Row &&
                                    c.Range.Start.Row < innerIfBlock.ElseBlock.Range.Start.Row &&
                                    (c.Range.Start.Row > innerIfBlock.ElseTokenLocation.Row ||
                                     c.Range.Start.Column > innerIfBlock.ElseTokenLocation.Column + 4))
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (commentsBeforeInnerElseBlock.Count > 0)
                    {
                        var innerElseBodyColumn = innerElseBodyContext.CurrentColumn;
                        foreach (var comment in commentsBeforeInnerElseBlock)
                        {
                            var commentLocation = innerElseBodyContext.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            // Use innerElseBodyContextReference which has the correct indent (bodyColumn - 1)
                            innerElseBodyContext = FormattingContext.AtPosition(commentEnd.Row + 1, innerElseBodyColumn, innerElseBodyContextReference);
                        }
                    }

                    var innerElseResult = FormatExpression(innerIfBlock.ElseBlock, innerElseBodyContext, currentComments);
                    currentComments = innerElseResult.Comments;
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
                ), afterInnerElseBlock, currentComments);
            }
            else
            {
                // "else"
                var afterElse = elseContext.Advance(4);

                // Else block on new line, indented from the base column
                // Also set IndentSpaces so nested expressions (like case/if) indent correctly
                var elseBlockContextReference = afterElse.ReturnToIndent(bodyRef).NextRowToIndent();
                var elseBlockContext = elseBlockContextReference;

                // Check for comments between "else" and the else-block
                var commentsBeforeElseBlock = originalComments
                    .Where(c => c.Range.Start.Row >= ifBlock.ElseTokenLocation.Row &&
                                c.Range.Start.Row < ifBlock.ElseBlock.Range.Start.Row &&
                                (c.Range.Start.Row > ifBlock.ElseTokenLocation.Row ||
                                 c.Range.Start.Column > ifBlock.ElseTokenLocation.Column + 4)) // past "else"
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                if (commentsBeforeElseBlock.Count > 0)
                {
                    var elseBodyColumn = elseBlockContext.CurrentColumn;
                    foreach (var comment in commentsBeforeElseBlock)
                    {
                        var commentLocation = elseBlockContext.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        // Next line for next comment or else-block
                        // Use elseBlockContextReference which has the correct indent (bodyColumn - 1)
                        elseBlockContext = FormattingContext.AtPosition(commentEnd.Row + 1, elseBodyColumn, elseBlockContextReference);
                    }
                }

                var elseResult = FormatExpression(ifBlock.ElseBlock, elseBlockContext, currentComments);

                return FormattingResult<Expression>.Create(new Expression.IfBlock(
                    ifTokenLoc,
                    conditionResult.FormattedNode,
                    thenTokenLoc,
                    thenResult.FormattedNode,
                    elseTokenLoc,
                    elseResult.FormattedNode
                ), elseResult.Context.ReturnToIndent(context), elseResult.Comments);
            }
        }

        private FormattingResult<Expression> FormatCaseExpression(
            Expression.CaseExpression caseExpr,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var currentComments = comments;

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
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, exprContext, currentComments);
                afterExpr = exprResult.Context;
            }
            else
            {
                // "case " with trailing space
                var afterCase = context.Advance(5);
                exprResult = FormatExpression(caseExpr.CaseBlock.Expression, afterCase, currentComments);
                afterExpr = exprResult.Context;
            }
            currentComments = exprResult.Comments;

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
                var commentsBeforePattern = originalComments
                    .Where(c => c.Range.Start.Row > prevEnd.Row &&
                                c.Range.Start.Row < caseItem.Pattern.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforePattern)
                {
                    var commentLocation = caseContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    caseContext = FormattingContext.AtPosition(commentEnd.Row + 1, 1, caseContext).SetIndentColumn();
                }

                // Pattern
                var patternText = FormatPatternText(caseItem.Pattern.Value);
                var patternStartLoc = caseContext.CurrentLocation();
                var afterPattern = caseContext.Advance(patternText.Length);
                var patternRange = MakeRange(patternStartLoc, afterPattern.CurrentLocation());

                // " ->" (space before arrow)
                var afterPatternSpace = afterPattern.Advance(1);
                var arrowLoc = afterPatternSpace.CurrentLocation();
                var afterArrow = afterPatternSpace.Advance(2);

                // Expression on new line, indented from the pattern position
                // Create reference at pattern position + 4
                var caseExprRef = caseContext.Advance(Indentation.Full).SetIndentToCurrentColumn();
                var caseExprContext = afterArrow.ReturnToIndent(caseExprRef).NextRowToIndent();

                // Check for comments before the case expression
                var commentsBeforeExpr = GetCommentsBetweenRanges(caseItem.Pattern.Range, caseItem.Expression.Range).ToList();

                foreach (var comment in commentsBeforeExpr)
                {
                    var commentLocation = caseExprContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    caseExprContext = FormattingContext.AtPosition(
                        commentEnd.Row + 1,
                        1,
                        caseExprContext).SetIndentColumn();
                }

                var caseExprResult = FormatExpression(caseItem.Expression, caseExprContext, currentComments);
                currentComments = caseExprResult.Comments;

                // Transform the pattern to have updated internal locations
                var transformedPattern = TransformPatternWithLocation(caseItem.Pattern.Value, patternStartLoc);

                formattedCases.Add(new Case(
                    MakeNodeWithRange(patternStartLoc, afterPattern.CurrentLocation(), transformedPattern),
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
                caseContext.ReturnToIndent(context),
                currentComments);
        }

        private FormattingResult<Expression> FormatLetExpression(
            Expression.LetExpression letExpr,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var currentComments = comments;

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

                var commentsBefore = originalComments
                    .Where(c => c.Range.Start.Row > prevEndRow && c.Range.Start.Row < decl.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                // If there are comments between declarations (not before first), add a blank line first
                if (i > 0 && commentsBefore.Count > 0)
                {
                    declContext = declContext.NextRowToIndent();
                }

                // Format comments before this declaration
                foreach (var comment in commentsBefore)
                {
                    var commentLocation = declContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    declContext = FormattingContext.AtPosition(commentEnd.Row + 1, 1, declContext).SetIndentColumn();
                }

                // Add empty line between declarations (after first, and only if no comments were output)
                if (i > 0 && commentsBefore.Count is 0)
                {
                    declContext = declContext.NextRowToIndent();
                }

                var declResult = FormatLetDeclaration(decl, declContext, currentComments);
                currentComments = declResult.Comments;
                formattedDecls.Add(declResult.FormattedNode);

                declContext = declResult.Context.NextRowToIndent();
            }

            // Check for comments between last declaration and "in"
            var lastDeclEndRow = letExpr.Value.Declarations.Count > 0
                ? letExpr.Value.Declarations[letExpr.Value.Declarations.Count - 1].Range.End.Row
                : letExpr.Value.LetTokenLocation.Row;
            var commentsBeforeIn = originalComments
                .Where(c => c.Range.Start.Row > lastDeclEndRow && c.Range.Start.Row < letExpr.Value.InTokenLocation.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // Format comments between last declaration and "in"
            if (commentsBeforeIn.Count > 0)
            {
                // Add blank line before comments
                declContext = declContext.NextRowToIndent();

                foreach (var comment in commentsBeforeIn)
                {
                    var commentLocation = declContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    declContext = FormattingContext.AtPosition(commentEnd.Row + 1, 1, declContext).SetIndentColumn();
                }
            }

            // "in" on own line at same column as "let"
            var inContext = declContext.ReturnToIndent(letTokenRef).SetIndentColumn();
            var inTokenLoc = inContext.CurrentLocation();
            var afterIn = inContext.Advance(2);

            // Check for comments between "in" and the expression
            var commentsBeforeExpr = originalComments
                .Where(c => c.Range.Start.Row > letExpr.Value.InTokenLocation.Row &&
                           c.Range.Start.Row < letExpr.Value.Expression.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // Expression after "in" - reset IndentSpaces to match let's position
            var exprContext = afterIn.ReturnToIndent(letTokenRef).NextRowToIndent();

            // Format comments between "in" and expression
            foreach (var comment in commentsBeforeExpr)
            {
                var commentLocation = exprContext.CurrentLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                currentComments = currentComments.Add(formattedComment);
                exprContext = FormattingContext.AtPosition(commentEnd.Row + 1, 1, exprContext).SetIndentColumn();
            }

            var exprResult = FormatExpression(letExpr.Value.Expression, exprContext, currentComments);

            var formattedLet = new Expression.LetBlock(
                letTokenLoc,
                formattedDecls,
                inTokenLoc,
                exprResult.FormattedNode
            );

            return FormattingResult<Expression>.Create(
                new Expression.LetExpression(formattedLet),
                exprResult.Context,
                exprResult.Comments);
        }

        private FormattingResult<Stil4mElmSyntax7.Node<Expression.LetDeclaration>> FormatLetDeclaration(
            Stil4mElmSyntax7.Node<Expression.LetDeclaration> letDecl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
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

                            Stil4mElmSyntax7.Node<TypeAnnotation> formattedTypeAnnot;
                            FormattingContext afterTypeAnnot;
                            ImmutableList<Stil4mElmSyntax7.Node<string>> typeAnnotComments;

                            if (preserveMultilineFormat)
                            {
                                // Type annotation on new line with extra indentation (for complex types like records)
                                var afterColon = afterSigNameSpace.Advance(1); // just ":"
                                var typeContext = afterColon.ReturnToIndent(indentedRef).NextRowToIndent();
                                (formattedTypeAnnot, afterTypeAnnot, typeAnnotComments) = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);
                            }
                            else
                            {
                                // Type annotation on same line after ": " (for simple types or when not on new line)
                                var afterColon = afterSigNameSpace.Advance(2); // ": "
                                (formattedTypeAnnot, afterTypeAnnot, typeAnnotComments) = FormatTypeAnnotation(signature.Value.TypeAnnotation, afterColon);
                            }

                            comments = comments.AddRange(typeAnnotComments);

                            var formattedSigValue = new Signature(
                                MakeNodeWithRange(sigNameLoc, afterSigName.CurrentLocation(), funcName),
                                colonLoc,
                                formattedTypeAnnot
                            );

                            formattedSignature = MakeNodeWithRange(sigNameLoc, afterTypeAnnot.CurrentLocation(), formattedSigValue);

                            // Move to next line for function implementation
                            currentCtx = afterTypeAnnot.ReturnToIndent(context).NextRowToIndent();
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
                            var argStartLoc = afterArgs.CurrentLocation();
                            var patternText = FormatPatternText(arg.Value);
                            afterArgs = afterArgs.Advance(patternText.Length);
                            var argEndLoc = afterArgs.CurrentLocation();
                            // Transform the pattern to have updated internal locations for correct rendering
                            var transformedPattern = TransformPatternWithLocation(arg.Value, argStartLoc);
                            formattedLetArgs.Add(MakeNodeWithRange(argStartLoc, argEndLoc, transformedPattern));
                        }

                        // " =" (space before equals)
                        var afterArgsSpace = afterArgs.Advance(1);
                        var equalsLoc = afterArgsSpace.CurrentLocation();
                        var afterEquals = afterArgsSpace.Advance(1); // just "="

                        // Check for comments between equals and expression
                        var equalsRow = letFunc.Function.Declaration.Value.EqualsTokenLocation.Row;
                        var exprStartRow = letFunc.Function.Declaration.Value.Expression.Range.Start.Row;
                        var commentsBeforeExpr = originalComments
                            .Where(c => c.Range.Start.Row > equalsRow && c.Range.Start.Row < exprStartRow)
                            .OrderBy(c => c.Range.Start.Row)
                            .ToList();

                        // Expression on new line, indented
                        var exprContext = afterEquals.ReturnToIndent(indentedRef).NextRowToIndent();

                        // Format comments between equals and expression
                        foreach (var comment in commentsBeforeExpr)
                        {
                            var commentLocation = exprContext.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            comments = comments.Add(formattedComment);
                            exprContext = FormattingContext.AtPosition(commentEnd.Row + 1, 1, exprContext).SetIndentColumn();
                        }

                        var exprResult = FormatExpression(letFunc.Function.Declaration.Value.Expression, exprContext, comments);

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
                            exprResult.Context.ReturnToIndent(context),
                            exprResult.Comments);
                    }

                case Expression.LetDeclaration.LetDestructuring letDestructuring:
                    {
                        var patternText = FormatPatternText(letDestructuring.Pattern.Value);
                        var patternLoc = context.CurrentLocation();
                        var afterPattern = context.Advance(patternText.Length);

                        // Transform the pattern to have updated internal locations for correct rendering
                        var transformedPattern = TransformPatternWithLocation(letDestructuring.Pattern.Value, patternLoc);
                        var formattedPatternNode = MakeNodeWithRange(patternLoc, afterPattern.CurrentLocation(), transformedPattern);

                        // " =" (space before equals)
                        var afterPatternSpace = afterPattern.Advance(1);
                        var destructEqualsLoc = afterPatternSpace.CurrentLocation();
                        var afterDestructEquals = afterPatternSpace.Advance(1); // just "="

                        var destructExprContext = afterDestructEquals.ReturnToIndent(indentedRef).NextRowToIndent();
                        var exprResult = FormatExpression(letDestructuring.Expression, destructExprContext, comments);

                        var formattedDestructuring = new Expression.LetDeclaration.LetDestructuring(
                            formattedPatternNode,
                            destructEqualsLoc,
                            exprResult.FormattedNode
                        );

                        var destructRange = MakeRange(patternLoc, exprResult.Context.CurrentLocation());
                        return FormattingResult<Stil4mElmSyntax7.Node<Expression.LetDeclaration>>.Create(
                            new Stil4mElmSyntax7.Node<Expression.LetDeclaration>(destructRange, formattedDestructuring),
                            exprResult.Context.ReturnToIndent(context),
                            exprResult.Comments);
                    }

                default:
                    throw new System.NotImplementedException(
                        $"LetDeclaration formatting not implemented for: {letDecl.Value.GetType().Name} " +
                        $"at row {letDecl.Range.Start.Row}, column {letDecl.Range.Start.Column}");
            }
        }

        private FormattingResult<Expression> FormatLambdaExpression(
            Expression.LambdaExpression lambdaExpr,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
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
                var argText = FormatPatternText(arg.Value);

                // Create new node with updated location
                var argLocation = afterArgs.CurrentLocation();
                formattedArgs.Add(MakeNodeWithRange(
                    argLocation,
                    new Location(argLocation.Row, argLocation.Column + argText.Length),
                    arg.Value));

                afterArgs = afterArgs.Advance(argText.Length);
                afterArgs = afterArgs.Advance(1); // space after each argument
            }

            // "->" (space already added after last arg)
            var arrowLoc = afterArgs.CurrentLocation();
            var afterArrow = afterArgs.Advance(2); // "->"

            // Check if body should be on a new line
            // Compare original body position to original arrow position
            var bodyOnNewLine = lambdaExpr.Lambda.Expression.Range.Start.Row > lambdaExpr.Lambda.ArrowLocation.Row;

            var currentComments = comments;
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
                var commentsBeforeExpr = GetCommentsBetweenRows(arrowRow, exprStartRow);

                // Format any comments that appear before the expression
                foreach (var comment in commentsBeforeExpr)
                {
                    var commentLocation = bodyContext.CurrentLocation();
                    var (commentEnd, endsWithNewline) = CalculateCommentEndLocationEx(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    bodyContext = CreateContextAfterComment(bodyContext, commentEnd, endsWithNewline);
                    // Maintain the same indent level after the comment (column is 1-based)
                    bodyContext = bodyContext.SetIndentColumn();
                }

                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, bodyContext, currentComments);
            }
            else
            {
                // Body on same line after space
                var afterSpace = afterArrow.Advance(1);
                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, afterSpace, currentComments);
            }

            var formattedLambda = new LambdaStruct(
                backslashLoc,
                formattedArgs,
                arrowLoc,
                exprResult.FormattedNode
            );

            return FormattingResult<Expression>.Create(
                new Expression.LambdaExpression(formattedLambda),
                exprResult.Context,
                exprResult.Comments);
        }

        private FormattingResult<Expression> FormatTupledExpression(
            Expression.TupledExpression tupledExpr,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var elements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupledExpr.Elements);
            var currentComments = comments;

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
                var commentsBeforeFirst = originalComments
                    .Where(c => c.Range.Start.Row >= originalRange.Start.Row &&
                               c.Range.Start.Row < elements[0].Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                var firstElemStartCtx = afterOpenParen;
                if (commentsBeforeFirst.Count > 0)
                {
                    // Format: ( -- comment\n      first_element
                    var firstComment = commentsBeforeFirst[0];
                    var commentLocation = firstElemStartCtx.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, firstComment.Value);
                    var formattedComment = firstComment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);

                    // First element on next line, indented past the opening paren
                    firstElemStartCtx = FormattingContext.AtPosition(
                        commentEnd.Row + 1,
                        elemContentRef.CurrentColumn, // indent past opening paren
                        elemContentRef);

                    // Handle any additional comments before first element
                    for (var ci = 1; ci < commentsBeforeFirst.Count; ci++)
                    {
                        var additionalComment = commentsBeforeFirst[ci];
                        var addCommentLoc = firstElemStartCtx.CurrentLocation();
                        var addCommentEnd = CalculateCommentEndLocation(addCommentLoc, additionalComment.Value);
                        var formattedAddComment = additionalComment.WithRange(addCommentLoc, addCommentEnd);
                        currentComments = currentComments.Add(formattedAddComment);
                        firstElemStartCtx = FormattingContext.AtPosition(addCommentEnd.Row + 1, elemContentRef.CurrentColumn, elemContentRef);
                    }
                }

                // First element on same line as opening paren (or after comments)
                var firstElemResult = FormatExpression(elements[0], firstElemStartCtx, currentComments);
                currentComments = firstElemResult.Comments;
                var tupledCtx = firstElemResult.Context;

                // Build rest with proper separator locations
                var restItems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Expression> Node)>();

                for (var i = 1; i < elements.Count; i++)
                {
                    var prevElem = elements[i - 1];
                    var currElem = elements[i];

                    // Get all comments between previous element and current element
                    var allCommentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > prevElem.Range.End.Row &&
                                   c.Range.Start.Row < currElem.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    // Get the original comma row for this element (index i-1 in originalSeparatorLocations)
                    var commaRowInOriginal = i - 1 < originalSeparatorLocations.Count
                        ? originalSeparatorLocations[i - 1].Row
                        : -1;

                    // Split comments: those before the comma row go before the comma,
                    // those on or after the comma row go after the comma
                    var commentsBeforeComma = new List<Stil4mElmSyntax7.Node<string>>();
                    var commentsAfterComma = new List<Stil4mElmSyntax7.Node<string>>();

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
                        var commentLoc = tupledCtx.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLoc, comment.Value);
                        var formattedComment = comment.WithRange(commentLoc, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        tupledCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, elemContentRef);
                    }

                    tupledCtx = tupledCtx.ReturnToIndent(elemAlignRef).NextRowToIndent();
                    var separatorLoc = tupledCtx.CurrentLocation(); // comma goes here

                    if (commentsAfterComma.Count > 0)
                    {
                        // Format: , -- comment\n      element
                        tupledCtx = tupledCtx.Advance(2); // ", "

                        var firstAfterComment = commentsAfterComma[0];
                        var afterCommentLoc = tupledCtx.CurrentLocation();
                        var afterCommentEnd = CalculateCommentEndLocation(afterCommentLoc, firstAfterComment.Value);
                        var formattedAfterComment = firstAfterComment.WithRange(afterCommentLoc, afterCommentEnd);
                        currentComments = currentComments.Add(formattedAfterComment);

                        // Element on next line, indented past the comma
                        tupledCtx = FormattingContext.AtPosition(
                            afterCommentEnd.Row + 1,
                            elemContentRef.CurrentColumn, // indent past comma
                            elemContentRef);

                        // Handle additional comments after comma
                        for (var ci = 1; ci < commentsAfterComma.Count; ci++)
                        {
                            var additionalComment = commentsAfterComma[ci];
                            var addCommentLoc = tupledCtx.CurrentLocation();
                            var addCommentEnd = CalculateCommentEndLocation(addCommentLoc, additionalComment.Value);
                            var formattedAddComment = additionalComment.WithRange(addCommentLoc, addCommentEnd);
                            currentComments = currentComments.Add(formattedAddComment);
                            tupledCtx = FormattingContext.AtPosition(addCommentEnd.Row + 1, elemContentRef.CurrentColumn, elemContentRef);
                        }
                    }
                    else
                    {
                        tupledCtx = tupledCtx.Advance(2); // ", "
                    }

                    var elemResult = FormatExpression(currElem, tupledCtx, currentComments);
                    currentComments = elemResult.Comments;
                    restItems.Add((separatorLoc, elemResult.FormattedNode));
                    tupledCtx = elemResult.Context;
                }

                // Check for comments before closing paren (after last element)
                var lastElem = elements[elements.Count - 1];
                var commentsBeforeClose = originalComments
                    .Where(c => c.Range.Start.Row > lastElem.Range.End.Row &&
                               c.Range.Start.Row < originalRange.End.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforeClose)
                {
                    tupledCtx = tupledCtx.ReturnToIndent(elemContentRef).NextRowToIndent();
                    var commentLoc = tupledCtx.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLoc, comment.Value);
                    var formattedComment = comment.WithRange(commentLoc, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    tupledCtx = FormattingContext.AtPosition(commentEnd.Row, commentEnd.Column, elemContentRef);
                }

                // Closing paren on new line, aligned with opening paren
                var closeCtx = tupledCtx.ReturnToIndent(elemAlignRef).NextRowToIndent();
                var afterCloseParen = closeCtx.Advance(1); // ")"

                var multilineSeparatedList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty(
                    firstElemResult.FormattedNode, restItems);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    multilineSeparatedList
                ), afterCloseParen.ReturnToIndent(context), currentComments);
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
                    var elemResult = FormatExpression(elements[i], tupledCtx, currentComments);
                    currentComments = elemResult.Comments;
                    formattedElements.Add(elemResult.FormattedNode);
                    tupledCtx = elemResult.Context;
                }

                // " )"
                var afterSpace = tupledCtx.Advance(1);
                var afterCloseParen = afterSpace.Advance(1);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    ToSeparatedSyntaxList(formattedElements)
                ), afterCloseParen, currentComments);
            }
        }

        private FormattingResult<Expression> FormatRecordUpdateExpression(
            Expression.RecordUpdateExpression recordUpdate,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var fields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(recordUpdate.Fields);
            var currentComments = comments;

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

                    var fieldResult = FormatExpression(field.ValueExpr, valueContext, currentComments);
                    currentComments = fieldResult.Comments;

                    var fieldNameNode = MakeNodeWithRange(fieldStartLoc, afterFieldName.CurrentLocation(), field.FieldName.Value);

                    var formattedField = new RecordExprField(fieldNameNode, equalsLoc, fieldResult.FormattedNode);

                    if (i is 0)
                    {
                        firstField = formattedField;
                    }
                    else
                    {
                        restFields.Add((separatorLoc!, formattedField));
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
                ), afterCloseBrace.ReturnToIndent(context), currentComments);
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
                    var fieldResult = FormatExpression(field.ValueExpr, afterEq, currentComments);
                    currentComments = fieldResult.Comments;

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
                ), afterCloseBrace, currentComments);
            }
        }

        #endregion

        #region Pattern Formatting

        /// <summary>
        /// Transforms a pattern to have updated internal locations based on the formatted position.
        /// This is necessary because patterns contain internal token locations (like OpenBracketLocation)
        /// that the renderer uses, and these need to match the formatted output positions.
        /// </summary>
        private static Pattern TransformPatternWithLocation(Pattern pattern, Location startLoc)
        {
            var patternText = FormatPatternText(pattern);
            var currentCol = startLoc.Column;

            return pattern switch
            {
                Pattern.AllPattern or
                Pattern.VarPattern or
                Pattern.UnitPattern or
                Pattern.CharPattern or
                Pattern.StringPattern or
                Pattern.IntPattern or
                Pattern.HexPattern or
                Pattern.FloatPattern =>
                    // These patterns don't have internal locations that need updating
                    pattern,

                Pattern.ListPattern listPattern =>
                    new Pattern.ListPattern(
                        new Location(startLoc.Row, currentCol),  // OpenBracketLocation at "["
                        listPattern.Elements.Count is 0
                            ? []
                            : TransformPatternListElements(listPattern.Elements, startLoc.Row, currentCol + 2), // After "[ "
                        new Location(startLoc.Row, currentCol + patternText.Length - 1)  // CloseBracketLocation at "]"
                    ),

                Pattern.TuplePattern tuplePattern =>
                    new Pattern.TuplePattern(
                        new Location(startLoc.Row, currentCol),  // OpenParenLocation
                        TransformPatternListElements(tuplePattern.Elements, startLoc.Row, currentCol + 2), // After "( "
                        new Location(startLoc.Row, currentCol + patternText.Length - 1)  // CloseParenLocation
                    ),

                Pattern.RecordPattern recordPattern =>
                    new Pattern.RecordPattern(
                        new Location(startLoc.Row, currentCol),  // OpenBraceLocation
                        TransformStringNodeList(recordPattern.Fields, startLoc.Row, currentCol + 2), // After "{ "
                        new Location(startLoc.Row, currentCol + patternText.Length - 1)  // CloseBraceLocation
                    ),

                Pattern.UnConsPattern unConsPattern =>
                    TransformUnConsPattern(unConsPattern, startLoc, patternText, currentCol),

                Pattern.ParenthesizedPattern parenPattern =>
                    TransformParenthesizedPattern(parenPattern, startLoc, patternText, currentCol),

                Pattern.NamedPattern namedPattern =>
                    TransformNamedPattern(namedPattern, startLoc, currentCol),

                Pattern.AsPattern asPattern =>
                    TransformAsPattern(asPattern, startLoc, patternText, currentCol),

                _ => pattern  // Fallback: return original pattern
            };
        }

        private static Pattern.UnConsPattern TransformUnConsPattern(
            Pattern.UnConsPattern unConsPattern, Location startLoc, string patternText, int currentCol)
        {
            var headText = FormatPatternText(unConsPattern.Head.Value);
            var headLoc = new Location(startLoc.Row, currentCol);
            var headEndCol = currentCol + headText.Length;
            var consOpCol = headEndCol + 1; // After " "
            var tailCol = consOpCol + 3; // After ":: "

            return new Pattern.UnConsPattern(
                MakeNodeWithRange(headLoc, new Location(startLoc.Row, headEndCol),
                    TransformPatternWithLocation(unConsPattern.Head.Value, headLoc)),
                new Location(startLoc.Row, consOpCol),  // ConsOperatorLocation
                MakeNodeWithRange(new Location(startLoc.Row, tailCol),
                    new Location(startLoc.Row, currentCol + patternText.Length),
                    TransformPatternWithLocation(unConsPattern.Tail.Value, new Location(startLoc.Row, tailCol)))
            );
        }

        private static Pattern.ParenthesizedPattern TransformParenthesizedPattern(
            Pattern.ParenthesizedPattern parenPattern, Location startLoc, string patternText, int currentCol)
        {
            var innerLoc = new Location(startLoc.Row, currentCol + 1); // After "("
            return new Pattern.ParenthesizedPattern(
                new Location(startLoc.Row, currentCol),  // OpenParenLocation
                MakeNodeWithRange(innerLoc, new Location(startLoc.Row, currentCol + patternText.Length - 1),
                    TransformPatternWithLocation(parenPattern.Pattern.Value, innerLoc)),
                new Location(startLoc.Row, currentCol + patternText.Length - 1)  // CloseParenLocation
            );
        }

        private static Pattern.NamedPattern TransformNamedPattern(
            Pattern.NamedPattern namedPattern, Location startLoc, int currentCol)
        {
            var nameText = FormatQualifiedName(namedPattern.Name);
            var afterNameCol = currentCol + nameText.Length;
            var transformedArgs = new List<Stil4mElmSyntax7.Node<Pattern>>();
            var argCol = afterNameCol + 1; // After space

            foreach (var arg in namedPattern.Arguments)
            {
                var argText = FormatPatternText(arg.Value);
                var argLoc = new Location(startLoc.Row, argCol);
                transformedArgs.Add(MakeNodeWithRange(
                    argLoc,
                    new Location(startLoc.Row, argCol + argText.Length),
                    TransformPatternWithLocation(arg.Value, argLoc)));
                argCol += argText.Length + 1; // After arg and space
            }

            return new Pattern.NamedPattern(namedPattern.Name, transformedArgs);
        }

        private static Pattern.AsPattern TransformAsPattern(
            Pattern.AsPattern asPattern, Location startLoc, string patternText, int currentCol)
        {
            var innerPatternText = FormatPatternText(asPattern.Pattern.Value);
            var innerLoc = new Location(startLoc.Row, currentCol);
            var asKeywordCol = currentCol + innerPatternText.Length + 1; // After pattern and space
            var nameCol = asKeywordCol + 3; // After "as "

            return new Pattern.AsPattern(
                MakeNodeWithRange(innerLoc, new Location(startLoc.Row, currentCol + innerPatternText.Length),
                    TransformPatternWithLocation(asPattern.Pattern.Value, innerLoc)),
                new Location(startLoc.Row, asKeywordCol),  // AsTokenLocation
                MakeNodeWithRange(new Location(startLoc.Row, nameCol),
                    new Location(startLoc.Row, currentCol + patternText.Length),
                    asPattern.Name.Value)
            );
        }

        private static IReadOnlyList<Stil4mElmSyntax7.Node<Pattern>> TransformPatternListElements(
            IReadOnlyList<Stil4mElmSyntax7.Node<Pattern>> elements, int row, int startCol)
        {
            var result = new List<Stil4mElmSyntax7.Node<Pattern>>();
            var col = startCol;

            for (var i = 0; i < elements.Count; i++)
            {
                var elem = elements[i];
                var elemText = FormatPatternText(elem.Value);
                var elemLoc = new Location(row, col);

                result.Add(MakeNodeWithRange(
                    elemLoc,
                    new Location(row, col + elemText.Length),
                    TransformPatternWithLocation(elem.Value, elemLoc)));

                col += elemText.Length + 2; // After elem and ", "
            }

            return result;
        }

        private static IReadOnlyList<Stil4mElmSyntax7.Node<string>> TransformStringNodeList(
            IReadOnlyList<Stil4mElmSyntax7.Node<string>> nodes, int row, int startCol)
        {
            var result = new List<Stil4mElmSyntax7.Node<string>>();
            var col = startCol;

            for (var i = 0; i < nodes.Count; i++)
            {
                var node = nodes[i];
                var nodeText = node.Value;

                result.Add(MakeNodeWithRange(
                    new Location(row, col),
                    new Location(row, col + nodeText.Length),
                    nodeText));

                col += nodeText.Length + 2; // After node and ", "
            }

            return result;
        }

        private static string FormatPatternText(Pattern pattern)
        {
            return pattern switch
            {
                Pattern.AllPattern =>
                "_",

                Pattern.VarPattern varPattern =>
                varPattern.Name,

                Pattern.UnitPattern =>
                "()",

                Pattern.CharPattern charPattern =>
                $"'{EscapeCharForPattern((char)charPattern.Value)}'",

                Pattern.StringPattern stringPattern =>
                $"\"{stringPattern.Value}\"",

                Pattern.IntPattern intPattern =>
                intPattern.Value.ToString(),

                Pattern.HexPattern hexPattern =>
                Rendering.RenderHexPattern(hexPattern.Value),

                Pattern.FloatPattern floatPattern =>
                Rendering.FormatFloatForElm(floatPattern.Value),

                Pattern.TuplePattern tuplePattern =>
                $"( {string.Join(", ", tuplePattern.Elements.Select(e => FormatPatternText(e.Value)))} )",

                Pattern.RecordPattern recordPattern =>
                $"{{ {string.Join(", ", recordPattern.Fields.Select(f => f.Value))} }}",

                Pattern.ListPattern listPattern =>
                listPattern.Elements.Count is 0
                ?
                "[]"
                :
                $"[ {string.Join(", ", listPattern.Elements.Select(e => FormatPatternText(e.Value)))} ]",

                Pattern.NamedPattern namedPattern =>
                namedPattern.Arguments.Count > 0
                ?
                $"{FormatQualifiedName(namedPattern.Name)} {string.Join(" ", namedPattern.Arguments.Select(a => FormatPatternText(a.Value)))}"
                :
                FormatQualifiedName(namedPattern.Name),

                Pattern.AsPattern asPattern =>
                $"{FormatPatternText(asPattern.Pattern.Value)} as {asPattern.Name.Value}",

                Pattern.ParenthesizedPattern parenPattern =>
                $"({FormatPatternText(parenPattern.Pattern.Value)})",

                Pattern.UnConsPattern unConsPattern =>
                $"{FormatPatternText(unConsPattern.Head.Value)} :: {FormatPatternText(unConsPattern.Tail.Value)}",

                _ =>
                throw new System.NotImplementedException(
                    $"Pattern type '{pattern.GetType().Name}' not implemented")
            };
        }

        private static string EscapeCharForPattern(char ch) =>
            ch switch
            {
                '\n' => "\\n",
                '\r' => "\\u{000D}",  // Elm uses Unicode escape for carriage return
                '\t' => "\\t",
                '\\' => "\\\\",
                '\'' => "\\'",
                _ when ch < 32 => $"\\u{{{(int)ch:X4}}}",
                _ => ch.ToString()
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
