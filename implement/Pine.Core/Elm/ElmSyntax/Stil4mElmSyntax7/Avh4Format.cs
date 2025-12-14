using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Format Elm syntax trees following the style of https://github.com/avh4/elm-format
/// Uses FormattingContext to track position and preserve locations for rendering.
/// </summary>
public class Avh4Format
{
    public static File Format(File file)
    {
        var visitor = new Avh4FormatVisitor(file.Comments);
        var context = new FormattingContext(CurrentRow: 1, CurrentColumn: 1, IndentLevel: 0);
        var locationMapping = ImmutableDictionary<Location, Location>.Empty;

        var (formatted, updatedLocationMapping, formattedComments) = visitor.FormatFile(file, context, locationMapping);

        return formatted with { Comments = formattedComments };
    }

    #region Constants and Keywords

    /// <summary>
    /// Named constants for Elm keywords and syntax to replace magic numbers.
    /// This makes the code self-documenting and prevents off-by-one errors in length calculations.
    /// </summary>
    private static class Keywords
    {
        public const string Module = "module ";
        public const string PortModule = "port module ";
        public const string EffectModule = "effect module ";
        public const string ExposingAll = "exposing (..)";
        public const string Exposing = "exposing";
        public const string ExposingOpen = "exposing (";
        public const string Import = "import ";
        public const string As = " as ";
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
        public const string Equals = " = ";
        public const string Arrow = "-> ";
        public const string Minus = "-";
        public const string Pipe = " | ";
    }

    #endregion

    #region Static Helper Methods
    /// <summary>
    /// Records that an original range maps to a new formatted range.
    /// Creates mappings for all locations within the range.
    /// Returns an updated location mapping.
    /// </summary>
    private static ImmutableDictionary<Location, Location> RecordMapping(
        Range originalRange,
        Range formattedRange,
        ImmutableDictionary<Location, Location> locationMapping)
    {
        var newMapping = locationMapping;

        // Always map the start location
        if (!newMapping.ContainsKey(originalRange.Start))
        {
            newMapping = newMapping.Add(originalRange.Start, formattedRange.Start);
        }

        // Map the end location if different
        if (originalRange.End != originalRange.Start && !newMapping.ContainsKey(originalRange.End))
        {
            newMapping = newMapping.Add(originalRange.End, formattedRange.End);
        }

        return newMapping;
    }

    /// <summary>
    /// Calculate the end location of a comment given its start location and value.
    /// Handles both single-line and multi-line comments.
    /// </summary>
    private static Location CalculateCommentEndLocation(Location startLocation, string commentValue)
    {
        var lines = commentValue.Split('\n');

        if (lines.Length is 1)
        {
            // Single-line comment: end is on the same row
            return new Location(startLocation.Row, startLocation.Column + commentValue.Length);
        }
        else
        {
            // Multi-line comment: end is on a different row
            var endRow = startLocation.Row + lines.Length - 1;
            var lastLineLength = lines[lines.Length - 1].Length;
            return new Location(endRow, lastLineLength + 1);
        }
    }

    /// <summary>
    /// Creates a new FormattingContext positioned after a comment ends.
    /// Handles both single-line and multi-line comments by advancing to the next row.
    /// </summary>
    private static FormattingContext CreateContextAfterComment(
        FormattingContext currentContext,
        Location commentEnd)
    {
        return new FormattingContext(
            commentEnd.Row + 1,
            1,
            currentContext.IndentLevel,
            currentContext.AddHalfIndent).SetIndentColumn();
    }

    /// <summary>
    /// Creates a context after a comment where the next element stays at the same indent level
    /// (doesn't add extra row advance for blank lines).
    /// Stays at the comment end position (same row).
    /// </summary>
    private static FormattingContext CreateContextAfterCommentSameIndent(
        FormattingContext currentContext,
        Location commentEnd)
    {
        // Stay at the comment end position
        return new FormattingContext(
            commentEnd.Row,
            commentEnd.Column,
            currentContext.IndentLevel,
            currentContext.AddHalfIndent);
    }

    /// <summary>
    /// Check if a comment is a doc comment (starts with {-|).
    /// Doc comments are connected directly to the declaration they document with no blank lines.
    /// </summary>
    private static bool IsDocComment(string commentValue)
    {
        return commentValue.TrimStart().StartsWith("{-|");
    }

    /// <summary>
    /// Result of formatting a comment, containing the formatted comment node and the updated context.
    /// </summary>
    private record FormattedCommentResult(
        Node<string> FormattedComment,
        FormattingContext NextContext);

    /// <summary>
    /// Result of formatting an element with full state tracking.
    /// Contains the formatted node, updated context, location mapping, and formatted comments.
    /// This record type replaces complex 4-element tuples for better readability.
    /// </summary>
    private record FormattingResult<T>(
        T FormattedNode,
        FormattingContext Context,
        ImmutableDictionary<Location, Location> LocationMapping,
        ImmutableList<Node<string>> Comments);

    /// <summary>
    /// Formats a comment at the given context position.
    /// Calculates the comment's end location, creates the formatted comment with proper range,
    /// and returns both the formatted comment and the context positioned after the comment.
    /// </summary>
    private static FormattedCommentResult FormatCommentAtContext(
        Node<string> comment,
        FormattingContext context)
    {
        var commentLocation = context.ToLocation();
        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
        var formattedComment = comment.WithRange(commentLocation, commentEnd);
        var nextContext = CreateContextAfterComment(context, commentEnd);

        return new FormattedCommentResult(formattedComment, nextContext);
    }

    #endregion

    #region Formatting Context

    /// <summary>
    /// Formatting context that tracks current position and indentation level.
    /// </summary>
    private record FormattingContext(
        int CurrentRow,
        int CurrentColumn,
        int IndentLevel,
        bool AddHalfIndent = false)
    {
        public Location ToLocation() =>
            new(CurrentRow, CurrentColumn);

        public FormattingContext NextRow() =>
            this with
            {
                CurrentRow = CurrentRow + 1,
                CurrentColumn = 1
            };

        public FormattingContext SetColumn(int column) =>
            this with
            {
                CurrentColumn = column
            };

        public FormattingContext Advance(int count) =>

            this with
            {
                CurrentColumn = CurrentColumn + count
            };

        public FormattingContext Indent() =>
            this with
            {
                IndentLevel = IndentLevel + 1,
                AddHalfIndent = false
            };

        public FormattingContext Dedent()
        {
            if (IndentLevel <= 0)
            {
                throw new System.InvalidOperationException(
                    $"Cannot dedent below zero. Current IndentLevel: {IndentLevel}");
            }

            return this with
            {
                IndentLevel = IndentLevel - 1
            };
        }

        public FormattingContext SetIndentColumn() =>
            this with
            {
                CurrentColumn = 1 + (IndentLevel * 4) + (AddHalfIndent ? 4 : 0)
            };

        public FormattingContext WithHalfIndent(bool addHalf) =>
            this with
            {
                AddHalfIndent = addHalf
            };
    }

    #endregion

    #region Visitor Implementation

    /// <summary>
    /// Visitor implementation for AVH4 formatting.
    /// </summary>
    private class Avh4FormatVisitor(IReadOnlyList<Node<string>> originalComments) : ExpressionVisitorBase<FormattingContext, (Node<Expression>, FormattingContext)>
    {

        /// <summary>
        /// Accumulates formatted comments during expression visiting.
        /// This allows comments to be collected even through code paths that don't thread
        /// comments through return values (like VisitExpressionNodeLessMapping).
        /// </summary>
        private readonly List<Node<string>> _accumulatedComments = [];

        #region File Formatting

        public (File, ImmutableDictionary<Location, Location>, IReadOnlyList<Node<string>>) FormatFile(
            File file,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper)
        {
            // Clear accumulated comments from any previous formatting
            _accumulatedComments.Clear();

            var formattedComments = ImmutableList<Node<string>>.Empty;

            // Format module definition
            var (formattedModule, contextAfterModule, mapperAfterModule, commentsAfterModule) =
                FormatModuleDefinition(file.ModuleDefinition, context, locationMapper, formattedComments);

            // Check for comments between module definition and imports (module-level doc comments)
            var moduleRow = file.ModuleDefinition.Range.End.Row;
            var firstImport = file.Imports.FirstOrDefault();

            var commentsAfterModuleBeforeImports = firstImport is not null
                ? GetCommentsBetweenRows(moduleRow, firstImport.Range.Start.Row)
                : [];

            FormattingContext contextBeforeImports;
            ImmutableList<Node<string>> currentComments;

            if (commentsAfterModuleBeforeImports.Count != 0)
            {
                // Format module-level doc comments (between module and imports)
                var startContext = contextAfterModule.NextRow().NextRow();
                (currentComments, contextBeforeImports) = FormatCommentsAtContext(
                    commentsAfterModuleBeforeImports, startContext, commentsAfterModule);
                // Add blank line before imports
                contextBeforeImports = contextBeforeImports.NextRow();
            }
            else
            {
                // No comments between module and imports - just add blank line
                contextBeforeImports = contextAfterModule.NextRow().NextRow();
                currentComments = commentsAfterModule;
            }

            // Format imports
            FormattingContext contextAfterImports;
            IReadOnlyList<Node<Import>> formattedImports;
            ImmutableDictionary<Location, Location> mapperAfterImports;
            ImmutableList<Node<string>> commentsAfterImports;

            if (firstImport is not null)
            {
                (formattedImports, contextAfterImports, mapperAfterImports, commentsAfterImports) =
                    FormatImports(file.Imports, contextBeforeImports, mapperAfterModule, currentComments);
            }
            else
            {
                formattedImports = [];
                contextAfterImports = contextAfterModule;
                mapperAfterImports = mapperAfterModule;
                commentsAfterImports = currentComments;
            }

            // Check for comments before first declaration (after imports/module)
            ImmutableList<Node<string>> commentsBeforeDecls = commentsAfterImports;
            var lastRowBeforeDecls = formattedImports.Any()
                ? formattedImports.Last().Range.End.Row
                : formattedModule.Range.End.Row;

            // Determine if there are comments before the first declaration
            var firstDeclaration = file.Declarations.FirstOrDefault();
            var commentsBefore = firstDeclaration is not null
                ? GetCommentsBetweenRows(lastRowBeforeDecls, firstDeclaration.Range.Start.Row)
                : [];

            FormattingContext contextBeforeDecls;

            if (commentsBefore.Count != 0)
            {
                // When there are comments before the first declaration:
                // - With imports: 2 blank lines after last import before the comment (same as before declaration)
                // - Without imports: 1 blank line after module before the comment
                // - 2 blank lines after non-doc comments before the declaration
                var startContext = formattedImports.Any()
                    ? contextAfterImports.NextRow().NextRow().NextRow()  // 2 blank lines after imports
                    : contextAfterModule.NextRow().NextRow();   // 1 blank line after module if no imports

                (commentsBeforeDecls, contextBeforeDecls) = FormatCommentsAtContext(
                    commentsBefore, startContext, commentsAfterImports, addBlankLinesAfterNonDocComments: true);
            }
            else
            {
                // No comments: standard 2 blank lines before declarations
                contextBeforeDecls = formattedImports.Any()
                    ? contextAfterImports.NextRow().NextRow()  // 1 more blank line after imports (already have 1 from last import)
                    : contextAfterModule.NextRow().NextRow().NextRow();   // 2 blank lines after module if no imports
            }

            // Format declarations
            var (formattedDeclarations, _, mapperAfterDecls, commentsAfterDecls) =
                FormatDeclarations(file.Declarations, contextBeforeDecls, mapperAfterImports, commentsBeforeDecls);

            // Merge comments threaded through return values with accumulated comments from expression visiting
            var allFormattedComments = commentsAfterDecls.AddRange(_accumulatedComments);

            var formattedFile = new File(
                ModuleDefinition: formattedModule,
                Imports: formattedImports,
                Declarations: formattedDeclarations,
                Comments: []); // Comments will be set by Format method

            return (formattedFile, mapperAfterDecls, allFormattedComments);
        }

        /// <summary>
        /// Formats a list of comments at the given context position.
        /// Returns the formatted comments and the updated context after all comments.
        /// </summary>
        private (ImmutableList<Node<string>> FormattedComments, FormattingContext NextContext) FormatCommentsAtContext(
            IReadOnlyList<Node<string>> comments,
            FormattingContext startContext,
            ImmutableList<Node<string>> existingComments,
            bool addBlankLinesAfterNonDocComments = false)
        {
            var currentContext = startContext;
            var currentComments = existingComments;

            foreach (var comment in comments)
            {
                var result = FormatCommentAtContext(comment, currentContext);
                currentComments = currentComments.Add(result.FormattedComment);
                currentContext = result.NextContext;

                // Optionally add blank lines after non-doc comments
                if (addBlankLinesAfterNonDocComments && !IsDocComment(comment.Value))
                {
                    currentContext = currentContext.NextRow().NextRow();
                }
            }

            return (currentComments, currentContext);
        }

        /// <summary>
        /// Gets comments that fall between two row boundaries.
        /// </summary>
        private IReadOnlyList<Node<string>> GetCommentsBetweenRows(int afterRow, int beforeRow) =>
            originalComments
                .Where(c => c.Range.Start.Row > afterRow && c.Range.End.Row < beforeRow)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

        #endregion

        #region Module Formatting

        private static (Node<Module>, FormattingContext, ImmutableDictionary<Location, Location>, ImmutableList<Node<string>>) FormatModuleDefinition(
            Node<Module> module,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
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

            // Record the mapping from original module range to formatted range
            var updatedMapper = RecordMapping(module.Range, result.Item1.Range, locationMapper);
            return (result.Item1, result.Item2, updatedMapper, formattedComments);
        }

        private static (Node<Module>, FormattingContext) FormatNormalModule(
            Module.NormalModule module,
            FormattingContext context)
        {
            // "module "
            var afterModuleKeyword = context.Advance(Keywords.Module.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1); // +1 for space

            var (formattedExposing, contextAfterExposing) = FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing
            );

            return (new Node<Module>(range, new Module.NormalModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatPortModule(
            Module.PortModule module,
            FormattingContext context)
        {
            // "port module "
            var afterKeyword = context.Advance(Keywords.PortModule.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);

            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing
            );

            return (new Node<Module>(range, new Module.PortModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatEffectModule(
            Module.EffectModule module,
            FormattingContext context)
        {
            // "effect module "
            var afterKeyword = context.Advance(Keywords.EffectModule.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);

            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new EffectModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing,
                Command: module.ModuleData.Command,
                Subscription: module.ModuleData.Subscription
            );

            return (new Node<Module>(range, new Module.EffectModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposing(
            Node<Exposing> exposing,
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

        private static (Node<Exposing>, FormattingContext) FormatExposingAll(FormattingContext context)
        {
            // "exposing (..)"
            var afterExposing = context.Advance(Keywords.ExposingAll.Length);
            var range = new Range(context.ToLocation(), afterExposing.ToLocation());
            return (new Node<Exposing>(range, new Exposing.All(range)), afterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposingExplicit(
            Exposing.Explicit explicitList,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "exposing (item1, item2, item3)"
                var afterOpenParen = context.Advance(Keywords.ExposingOpen.Length);

                var currentContext = afterOpenParen;
                var formattedNodes = new List<Node<TopLevelExpose>>();

                for (var i = 0; i < explicitList.Nodes.Count; i++)
                {
                    var node = explicitList.Nodes[i];
                    var (formattedNode, nextContext) = FormatTopLevelExpose(node, currentContext);
                    formattedNodes.Add(formattedNode);

                    currentContext = i < explicitList.Nodes.Count - 1
                        ? nextContext.Advance(Keywords.Comma.Length)
                        : nextContext.Advance(Keywords.CloseParen.Length);
                }

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), currentContext);
            }
            else
            {
                // Multi-line: first item on same line as paren, rest on new lines
                var afterExposingKeyword = context.Advance(Keywords.Exposing.Length);

                // Move to next line and indent for opening paren
                var parenLineContext = afterExposingKeyword.NextRow().Indent().SetIndentColumn();

                // "( "
                var afterOpenParen = parenLineContext.Advance(Keywords.TupleOpen.Length);

                var formattedNodes = new List<Node<TopLevelExpose>>();

                // First item on same line as opening paren
                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], afterOpenParen);
                formattedNodes.Add(firstNode);

                // Subsequent items on new lines with ", " prefix
                var itemContext = afterFirst.NextRow().SetIndentColumn();

                for (var i = 1; i < explicitList.Nodes.Count; i++)
                {
                    // ", "
                    var afterComma = itemContext.Advance(2);

                    var (formattedNode, nextContext) = FormatTopLevelExpose(explicitList.Nodes[i], afterComma);
                    formattedNodes.Add(formattedNode);

                    // Move to next line
                    itemContext = nextContext.NextRow().SetIndentColumn();
                }

                // ")"
                var afterCloseParen = itemContext.Advance(1);

                var finalContext = afterCloseParen.Dedent();
                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                return (new Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), finalContext);
            }
        }

        /// <summary>
        /// Helper method to check if a collection of nodes spans multiple rows.
        /// Returns false for collections with fewer than 2 items.
        /// </summary>
        private static bool NodesSpanMultipleRows<T>(IReadOnlyList<Node<T>> nodes)
        {
            if (nodes.Count < 2)
                return false;

            var firstRow = nodes[0].Range.Start.Row;
            return nodes.Skip(1).Any(node => node.Range.Start.Row != firstRow);
        }

        /// <summary>
        /// Check if a range spans multiple rows.
        /// </summary>
        private static bool SpansMultipleRows(Range range) =>
            range.End.Row > range.Start.Row;

        /// <summary>
        /// Check if two ranges are on different rows (second range starts after first ends).
        /// </summary>
        private static bool RangesOnDifferentRows(Range first, Range second) =>
            first.End.Row < second.Start.Row;

        /// <summary>
        /// Check if an exposing list should be formatted as multiline.
        /// </summary>
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
                ?
                $"{typeExpose.ExposedType.Name}(..)"
                :
                typeExpose.ExposedType.Name,

                _ =>
                throw new System.NotImplementedException(
                    $"Getting name for expose type '{expose.GetType().Name}' is not implemented.")
            };
        }

        private static (Node<TopLevelExpose>, FormattingContext) FormatTopLevelExpose(
            Node<TopLevelExpose> node,
            FormattingContext context)
        {
            var exposeName = GetTopLevelExposeName(node.Value);
            var afterExpose = context.Advance(exposeName.Length);
            var range = new Range(context.ToLocation(), afterExpose.ToLocation());

            return (new Node<TopLevelExpose>(range, node.Value), afterExpose);
        }

        #endregion

        #region Import Formatting

        private static (IReadOnlyList<Node<Import>>, FormattingContext, ImmutableDictionary<Location, Location>, ImmutableList<Node<string>>) FormatImports(
            IReadOnlyList<Node<Import>> imports,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            if (!imports.Any())
                return ([], context, locationMapper, formattedComments);

            // Sort imports by module name in ascending order (alphabetical)
            var sortedImports =
                imports
                .OrderBy(import => string.Join(".", import.Value.ModuleName.Value))
                .ToList();

            var formattedImports = new List<Node<Import>>();
            var currentContext = context;
            var currentMapper = locationMapper;

            foreach (var import in sortedImports)
            {
                var (formattedImport, nextContext) =
                    FormatImport(import, currentContext);

                // Record the mapping
                currentMapper = RecordMapping(import.Range, formattedImport.Range, currentMapper);

                formattedImports.Add(formattedImport);

                // Single newline between imports (no blank lines)
                currentContext = nextContext.NextRow();
            }

            return (formattedImports, currentContext, currentMapper, formattedComments);
        }

        private static (Node<Import>, FormattingContext) FormatImport(Node<Import> import, FormattingContext context)
        {
            // "import "
            var afterImportKeyword = context.Advance(Keywords.Import.Length);

            // Module name
            var moduleName = string.Join(".", import.Value.ModuleName.Value);
            var afterModuleName = afterImportKeyword.Advance(moduleName.Length);

            var currentContext = afterModuleName;

            // Handle module alias if present
            if (import.Value.ModuleAlias is { } alias)
            {
                // " as "
                currentContext = currentContext.Advance(4);
                // Alias name
                var aliasName = string.Join(".", alias.Value);
                currentContext = currentContext.Advance(aliasName.Length);
            }

            // Handle exposing list if present
            if (import.Value.ExposingList is { } exposingList)
            {
                // " exposing "
                currentContext = currentContext.Advance(10);

                // Calculate the length of the exposing list text
                // For now, we'll calculate based on the original range
                var exposingTextLength = exposingList.Range.End.Column - exposingList.Range.Start.Column;
                currentContext = currentContext.Advance(exposingTextLength);
            }

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, import.Value.ModuleName.Value);

            var formattedImport = new Import(
                ModuleName: moduleNameNode,
                ModuleAlias: import.Value.ModuleAlias,  // Preserved as-is for now
                ExposingList: import.Value.ExposingList  // Preserved as-is for now
            );

            return (new Node<Import>(range, formattedImport), currentContext);
        }

        #endregion

        #region Declaration Formatting

        private (IReadOnlyList<Node<Declaration>>, FormattingContext, ImmutableDictionary<Location, Location>, ImmutableList<Node<string>>) FormatDeclarations(
            IReadOnlyList<Node<Declaration>> declarations,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            if (!declarations.Any())
                return ([], context, locationMapper, formattedComments);

            var formattedDeclarations = new List<Node<Declaration>>();
            var currentContext = context;
            var currentMapper = locationMapper;
            var currentComments = formattedComments;

            for (var i = 0; i < declarations.Count; i++)
            {
                var decl = declarations[i];

                // Ensure we start each declaration at indent level 0
                var declContext = currentContext with { IndentLevel = 0 };
                var (formattedDecl, nextContext, updatedMapper, updatedComments) =
                    FormatDeclaration(decl, declContext, currentMapper, currentComments);

                // Record the mapping
                currentMapper = RecordMapping(decl.Range, formattedDecl.Range, updatedMapper);
                currentComments = updatedComments;

                formattedDeclarations.Add(formattedDecl);

                // Spacing after declaration (except for the last one):
                // - Consecutive infix declarations: 1 newline (no blank lines)
                // - Comments before next declaration: handle with appropriate spacing
                //   - Regular comments: 2 blank lines after current decl, comment, 2 blank lines before next decl
                //   - Doc comments: 2 blank lines after current decl, doc comment connects directly to next decl
                // - All other cases: 3 newlines (2 blank lines)
                if (i < declarations.Count - 1)
                {
                    var nextDecl = declarations[i + 1];

                    // Check if there are any comments between this declaration and the next
                    // For doc comments, they may be within the next declaration's range but still need to be formatted
                    var commentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > decl.Range.End.Row && c.Range.Start.Row <= nextDecl.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (formattedDecl.Value is Declaration.InfixDeclaration && nextDecl.Value is Declaration.InfixDeclaration)
                    {
                        currentContext = nextContext.NextRow();
                    }
                    else if (commentsBetween.Count != 0)
                    {
                        // Add 2 blank lines before first comment
                        currentContext = nextContext.NextRow().NextRow().NextRow();

                        // Format each comment
                        foreach (var comment in commentsBetween)
                        {
                            // Format the comment at current position
                            var commentLocation = currentContext.ToLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);

                            currentComments = currentComments.Add(formattedComment);

                            // Move to next row after comment
                            currentContext = CreateContextAfterComment(currentContext, commentEnd);

                            // If this is a doc comment, connect directly to next declaration (no blank lines)
                            // Otherwise, add 2 blank lines
                            if (!IsDocComment(comment.Value))
                            {
                                currentContext = currentContext.NextRow().NextRow();
                            }
                        }
                    }
                    else
                    {
                        currentContext = nextContext.NextRow().NextRow().NextRow();
                    }
                }
                else
                {
                    currentContext = nextContext;
                }
            }

            return (formattedDeclarations, currentContext, currentMapper, currentComments);
        }

        private FormattingResult<Node<Declaration>> FormatDeclaration(
            Node<Declaration> decl,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            return decl.Value switch
            {
                Declaration.FunctionDeclaration funcDecl =>
                    FormatFunctionDeclaration(funcDecl, decl.Range, context, locationMapper, formattedComments),

                Declaration.AliasDeclaration aliasDecl =>
                    FormatAliasDeclaration(aliasDecl, context, locationMapper, formattedComments),

                Declaration.CustomTypeDeclaration customTypeDecl =>
                    FormatCustomTypeDeclaration(customTypeDecl, context, locationMapper, formattedComments),

                Declaration.InfixDeclaration infixDecl =>
                    FormatInfixDeclarationWithMapping(infixDecl, decl.Range, context, locationMapper, formattedComments),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented.")
            };
        }

        private FormattingResult<Node<Declaration>> FormatFunctionDeclaration(
            Declaration.FunctionDeclaration funcDecl,
            Range originalDeclRange,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            var currentContext = context;
            var currentComments = formattedComments;

            // Format documentation comment if present
            if (funcDecl.Function.Documentation is { } docComment)
            {
                // Format the doc comment at current position
                var docCommentLocation = currentContext.ToLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                var formattedDocComment = docComment.WithRange(docCommentLocation, docCommentEnd);

                currentComments = currentComments.Add(formattedDocComment);

                // Move to next row after doc comment (connects directly to declaration)
                currentContext = CreateContextAfterComment(currentContext, docCommentEnd);
            }

            // Format signature if present using explicit null handling
            var (formattedSignature, contextAfterSignature) = FormatOptionalSignature(funcDecl.Function.Signature, currentContext);
            currentContext = contextAfterSignature;

            var impl = funcDecl.Function.Declaration.Value;

            // Function name
            var afterName = currentContext.Advance(impl.Name.Value.Length);

            // Arguments
            var afterArgs = impl.Arguments.Aggregate(afterName, (ctx, arg) =>
            {
                var patternText = FormatPatternText(arg.Value);
                return ctx.Advance(1 + patternText.Length); // space + pattern
            });

            // " ="
            var afterEquals = afterArgs.Advance(2);

            // Pull in comments that appear within this function implementation
            // (between the function name and the end of the expression)
            var commentsInImpl = originalComments
                .Where(c => c.Range.Start.Row >= funcDecl.Function.Declaration.Range.Start.Row &&
                           c.Range.End.Row <= funcDecl.Function.Declaration.Range.End.Row)
                .ToList();

            // Move to next line and indent for the expression
            var exprContext = afterEquals.NextRow().Indent().SetIndentColumn();
            var updatedComments = currentComments;

            // Add comments that appear before the expression
            foreach (var comment in commentsInImpl)
            {
                // Only add comments that are before the expression
                if (comment.Range.Start.Row < impl.Expression.Range.Start.Row)
                {
                    // Create formatted comment at current position
                    var commentLocation = exprContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    updatedComments = updatedComments.Add(formattedComment);

                    // Advance to next line after the comment ends (handles multi-line comments)
                    exprContext = CreateContextAfterComment(exprContext, commentEnd);
                }
            }

            // Format the expression and record mapping
            var (formattedExpr, contextAfterExpr, updatedMapper, finalComments) = VisitExpressionNode(impl.Expression, exprContext, locationMapper, updatedComments);

            // Build formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: new Node<string>(impl.Name.Range, impl.Name.Value),
                Arguments: impl.Arguments,
                Expression: formattedExpr
            );

            var formattedFunc = new FunctionStruct(
                Documentation: funcDecl.Function.Documentation,
                Signature: formattedSignature,
                Declaration: new Node<FunctionImplementation>(
                    new Range(currentContext.ToLocation(), contextAfterExpr.ToLocation()),
                    formattedImpl)
            );

            var range = new Range(context.ToLocation(), contextAfterExpr.ToLocation());
            return new FormattingResult<Node<Declaration>>(
                new Node<Declaration>(range, new Declaration.FunctionDeclaration(formattedFunc)),
                contextAfterExpr,
                updatedMapper,
                finalComments);
        }

        private FormattingResult<Node<Declaration>> FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            var currentComments = formattedComments;
            var startContext = context;

            // Format documentation comment if present
            if (aliasDecl.TypeAlias.Documentation is { } docComment)
            {
                // Format the doc comment at current position
                var docCommentLocation = startContext.ToLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                var formattedDocComment = docComment.WithRange(docCommentLocation, docCommentEnd);

                currentComments = currentComments.Add(formattedDocComment);

                // Move to next row after doc comment (connects directly to declaration)
                startContext = CreateContextAfterComment(startContext, docCommentEnd);
            }

            // "type alias "
            var afterTypeAlias = startContext.Advance(11);

            // Name
            var aliasName = aliasDecl.TypeAlias.Name.Value;
            var afterName = afterTypeAlias.Advance(aliasName.Length);

            // Generics (if any)
            var currentContext = afterName;
            foreach (var generic in aliasDecl.TypeAlias.Generics)
            {
                currentContext = currentContext.Advance(1); // space
                currentContext = currentContext.Advance(generic.Value.Length);
            }

            // " ="
            var afterEquals = currentContext.Advance(2);

            // Move to next line and indent for the type annotation
            var typeContext = afterEquals.NextRow().Indent().SetIndentColumn();

            // Format the type annotation (for now, keep it simple for single-line records)
            var (formattedTypeAnnotation, afterType) = FormatTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, typeContext);

            var formattedTypeAlias = new TypeAlias(
                Documentation: aliasDecl.TypeAlias.Documentation,
                Name: aliasDecl.TypeAlias.Name,
                Generics: aliasDecl.TypeAlias.Generics,
                TypeAnnotation: formattedTypeAnnotation
            );

            var finalContext = afterType.Dedent();
            var range = new Range(startContext.ToLocation(), finalContext.ToLocation());
            return new FormattingResult<Node<Declaration>>(
                new Node<Declaration>(range, new Declaration.AliasDeclaration(formattedTypeAlias)),
                finalContext,
                locationMapper,
                currentComments);
        }

        #endregion

        #region Type Annotation Formatting

        private (Node<TypeAnnotation>, FormattingContext) FormatTypeAnnotation(Node<TypeAnnotation> typeAnnot, FormattingContext context)
        {
            return typeAnnot.Value switch
            {
                TypeAnnotation.GenericType generic =>
                    FormatGenericTypeAnnotation(generic, context),

                TypeAnnotation.Unit =>
                    FormatUnitTypeAnnotation(context),

                TypeAnnotation.Typed typed =>
                    FormatTypedAnnotation(typed, context),

                TypeAnnotation.FunctionTypeAnnotation funcType =>
                    FormatFunctionTypeAnnotation(funcType, context),

                TypeAnnotation.Tupled tupled =>
                    FormatTupledTypeAnnotation(tupled, context),

                TypeAnnotation.GenericRecord genericRecord =>
                    FormatGenericRecordTypeAnnotation(genericRecord, context),

                TypeAnnotation.Record recordDef =>
                    FormatRecordTypeAnnotation(recordDef, typeAnnot.Range, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for type annotation '{typeAnnot.Value.GetType().Name}' is not implemented.")
            };
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatGenericTypeAnnotation(TypeAnnotation.GenericType generic, FormattingContext context)
        {
            var afterName = context.Advance(generic.Name.Length);
            var range = new Range(context.ToLocation(), afterName.ToLocation());
            return (new Node<TypeAnnotation>(range, generic), afterName);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatUnitTypeAnnotation(FormattingContext context)
        {
            var afterUnit = context.Advance(Keywords.Unit.Length);
            var range = new Range(context.ToLocation(), afterUnit.ToLocation());
            return (new Node<TypeAnnotation>(range, new TypeAnnotation.Unit()), afterUnit);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatTypedAnnotation(TypeAnnotation.Typed typed, FormattingContext context)
        {
            var moduleName = typed.TypeName.Value.ModuleName.Count > 0
                ? string.Join(".", typed.TypeName.Value.ModuleName) + "."
                : "";
            var typeName = moduleName + typed.TypeName.Value.Name;

            var currentContext = context.Advance(typeName.Length);
            var formattedArgs = new List<Node<TypeAnnotation>>();

            foreach (var arg in typed.TypeArguments)
            {
                currentContext = currentContext.Advance(1); // space before argument

                // Parenthesize complex type arguments
                var needsParens = arg.Value is TypeAnnotation.FunctionTypeAnnotation ||
                                  (arg.Value is TypeAnnotation.Typed t && t.TypeArguments.Count > 0);

                if (needsParens)
                {
                    currentContext = currentContext.Advance(1); // "("
                }

                var argNode = new Node<TypeAnnotation>(arg.Range, arg.Value);
                var (formattedArg, afterArg) = FormatTypeAnnotation(argNode, currentContext);
                formattedArgs.Add(formattedArg);
                currentContext = afterArg;

                if (needsParens)
                {
                    currentContext = currentContext.Advance(1); // ")"
                }
            }

            var formattedTyped = new TypeAnnotation.Typed(
                TypeName: typed.TypeName,
                TypeArguments: formattedArgs);

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<TypeAnnotation>(range, formattedTyped), currentContext);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatFunctionTypeAnnotation(TypeAnnotation.FunctionTypeAnnotation funcType, FormattingContext context)
        {
            // Check if the function type annotation was originally multiline (arrow on new line)
            var isMultiLine = RangesOnDifferentRows(funcType.ArgumentType.Range, funcType.ReturnType.Range);

            var currentContext = context;

            // Check if argument needs parentheses
            // Only function type arguments need parens (for precedence with ->)
            // Typed with args (like Array a) does NOT need parens because type application binds tighter than ->
            var argNeedsParens = funcType.ArgumentType.Value is TypeAnnotation.FunctionTypeAnnotation;

            Node<TypeAnnotation> formattedArg;
            if (argNeedsParens)
            {
                // Start paren
                currentContext = currentContext.Advance(1); // "("

                // Format the inner argument type
                var (innerArg, afterInner) = FormatTypeAnnotation(funcType.ArgumentType, currentContext);
                currentContext = afterInner;

                // End paren
                currentContext = currentContext.Advance(1); // ")"

                // Wrap in single-element Tupled to tell renderer to output parens
                var parenRange = new Range(context.ToLocation(), currentContext.ToLocation());
                formattedArg = new Node<TypeAnnotation>(parenRange, new TypeAnnotation.Tupled([innerArg]));
            }
            else
            {
                // Format the argument type directly
                var (arg, afterArg) = FormatTypeAnnotation(funcType.ArgumentType, currentContext);
                formattedArg = arg;
                currentContext = afterArg;
            }

            if (isMultiLine)
            {
                // Multiline function type: arrow on new line
                // Move to next line, set indent column, then "-> "
                currentContext = currentContext.NextRow().SetIndentColumn();
                currentContext = currentContext.Advance(3); // "-> "
            }
            else
            {
                // Single-line function type: " -> "
                currentContext = currentContext.Advance(4);
            }

            // Format the return type
            var (formattedReturn, afterReturn) = FormatTypeAnnotation(funcType.ReturnType, currentContext);
            currentContext = afterReturn;

            var formattedFunc = new TypeAnnotation.FunctionTypeAnnotation(
                ArgumentType: formattedArg,
                ReturnType: formattedReturn);

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<TypeAnnotation>(range, formattedFunc), currentContext);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatTupledTypeAnnotation(TypeAnnotation.Tupled tupled, FormattingContext context)
        {
            // Single-element tuple is just parentheses around a type (no spaces)
            if (tupled.TypeAnnotations.Count is 1)
            {
                // "("
                var currentContext = context.Advance(1);

                var (formattedType, afterType) = FormatTypeAnnotation(tupled.TypeAnnotations[0], currentContext);

                // ")"
                currentContext = afterType.Advance(1);

                var formattedTupled = new TypeAnnotation.Tupled([formattedType]);
                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<TypeAnnotation>(range, formattedTupled), currentContext);
            }

            // Multi-element tuple: "( element1, element2, ... )"
            // "( "
            var multiContext = context.Advance(2);
            var formattedTypes = new List<Node<TypeAnnotation>>();

            for (var i = 0; i < tupled.TypeAnnotations.Count; i++)
            {
                if (i > 0)
                {
                    // ", "
                    multiContext = multiContext.Advance(2);
                }

                var (formattedType, afterType) = FormatTypeAnnotation(tupled.TypeAnnotations[i], multiContext);
                formattedTypes.Add(formattedType);
                multiContext = afterType;
            }

            // " )"
            multiContext = multiContext.Advance(2);

            var formattedMultiTupled = new TypeAnnotation.Tupled(formattedTypes);
            var multiRange = new Range(context.ToLocation(), multiContext.ToLocation());
            return (new Node<TypeAnnotation>(multiRange, formattedMultiTupled), multiContext);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatGenericRecordTypeAnnotation(TypeAnnotation.GenericRecord genericRecord, FormattingContext context)
        {
            // "{ "
            var currentContext = context.Advance(2);

            // Generic name
            var genericName = genericRecord.GenericName.Value;
            var afterGenericName = currentContext.Advance(genericName.Length);
            var genericNameRange = new Range(currentContext.ToLocation(), afterGenericName.ToLocation());
            currentContext = afterGenericName;

            // " | "
            currentContext = currentContext.Advance(3);

            // Format fields
            var formattedFields = new List<Node<RecordField>>();
            for (var i = 0; i < genericRecord.RecordDefinition.Value.Fields.Count; i++)
            {
                if (i > 0)
                {
                    // ", "
                    currentContext = currentContext.Advance(2);
                }

                var field = genericRecord.RecordDefinition.Value.Fields[i];
                var fieldStartContext = currentContext;

                // field name
                var fieldName = field.Value.FieldName.Value;
                currentContext = currentContext.Advance(fieldName.Length);
                var fieldNameRange = new Range(fieldStartContext.ToLocation(), currentContext.ToLocation());

                // " : "
                currentContext = currentContext.Advance(3);

                // field type
                var (formattedFieldType, afterFieldType) = FormatTypeAnnotation(field.Value.FieldType, currentContext);
                currentContext = afterFieldType;

                var newField = new RecordField(
                    FieldName: new Node<string>(fieldNameRange, fieldName),
                    FieldType: formattedFieldType);
                var fieldRange = new Range(fieldStartContext.ToLocation(), currentContext.ToLocation());
                formattedFields.Add(new Node<RecordField>(fieldRange, newField));
            }

            // " }"
            currentContext = currentContext.Advance(2);

            var formattedRecordDef = new RecordDefinition(formattedFields);
            var formattedGenericRecord = new TypeAnnotation.GenericRecord(
                GenericName: new Node<string>(genericNameRange, genericName),
                RecordDefinition: new Node<RecordDefinition>(
                    new Range(context.Advance(2).ToLocation(), currentContext.Advance(-2).ToLocation()),
                    formattedRecordDef));

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<TypeAnnotation>(range, formattedGenericRecord), currentContext);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotation(TypeAnnotation.Record recordType, Range originalRange, FormattingContext context)
        {
            // Check if the record should be formatted as multiline
            var isMultiline = SpansMultipleRows(originalRange);

            if (isMultiline)
            {
                return FormatRecordTypeAnnotationMultiline(recordType, context);
            }
            else
            {
                return FormatRecordTypeAnnotationSingleLine(recordType, context);
            }
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotationSingleLine(TypeAnnotation.Record recordType, FormattingContext context)
        {
            // "{ "
            var afterOpenBrace = context.Advance(2);

            var currentContext = afterOpenBrace;
            var formattedFields = new List<Node<RecordField>>();

            for (var i = 0; i < recordType.RecordDefinition.Fields.Count; i++)
            {
                var field = recordType.RecordDefinition.Fields[i];
                var fieldStartContext = currentContext;

                // field name
                var fieldName = field.Value.FieldName.Value;
                currentContext = currentContext.Advance(fieldName.Length);

                // " : "
                currentContext = currentContext.Advance(3);

                // Format the field type using FormatTypeAnnotation
                var (formattedFieldType, afterFieldType) = FormatTypeAnnotation(field.Value.FieldType, currentContext);
                currentContext = afterFieldType;

                // Create new field node with proper range
                var fieldNameRange = new Range(fieldStartContext.ToLocation(), fieldStartContext.Advance(fieldName.Length).ToLocation());

                var newField = new RecordField(
                    FieldName: new Node<string>(fieldNameRange, fieldName),
                    FieldType: formattedFieldType
                );

                var fieldRange = new Range(fieldStartContext.ToLocation(), currentContext.ToLocation());
                formattedFields.Add(new Node<RecordField>(fieldRange, newField));

                // comma and space between fields (except after last)
                if (i < recordType.RecordDefinition.Fields.Count - 1)
                {
                    currentContext = currentContext.Advance(2); // ", "
                }
            }

            // " }"
            var afterCloseBrace = currentContext.Advance(2);

            var formattedRecordDef = new RecordDefinition(formattedFields);
            var range = new Range(context.ToLocation(), afterCloseBrace.ToLocation());
            return (new Node<TypeAnnotation>(range, new TypeAnnotation.Record(formattedRecordDef)), afterCloseBrace);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotationMultiline(TypeAnnotation.Record recordType, FormattingContext context)
        {
            // Track which comments we've already processed to avoid duplicates
            var alreadyFormattedCommentTexts = new HashSet<string>();

            // "{ "
            var afterOpenBrace = context.Advance(2);

            var formattedFields = new List<Node<RecordField>>();

            // First field on same line as opening brace
            var field0 = recordType.RecordDefinition.Fields[0];
            var fieldName0 = field0.Value.FieldName.Value;
            var afterName0 = afterOpenBrace.Advance(fieldName0.Length);
            var afterColon0 = afterName0.Advance(3); // " : "

            // Format the field type using FormatTypeAnnotation
            var (formattedFieldType0, afterType0) = FormatTypeAnnotation(field0.Value.FieldType, afterColon0);

            var field0NameRange = new Range(afterOpenBrace.ToLocation(), afterName0.ToLocation());
            var newField0 = new RecordField(
                FieldName: new Node<string>(field0NameRange, fieldName0),
                FieldType: formattedFieldType0
            );
            var field0Range = new Range(afterOpenBrace.ToLocation(), afterType0.ToLocation());
            formattedFields.Add(new Node<RecordField>(field0Range, newField0));

            // Subsequent fields on new lines with ", " prefix
            var fieldContext = afterType0.NextRow().SetIndentColumn();
            var prevField = field0;

            for (var i = 1; i < recordType.RecordDefinition.Fields.Count; i++)
            {
                var field = recordType.RecordDefinition.Fields[i];

                // Check for comments between previous field and this field
                // Use field type end row for previous field since field ranges can extend further
                var prevFieldContentEndRow = prevField.Value.FieldType.Range.End.Row;
                var commentsBetweenFields = originalComments
                    .Where(c => c.Range.Start.Row > prevFieldContentEndRow &&
                               c.Range.End.Row < field.Range.Start.Row &&
                               !alreadyFormattedCommentTexts.Contains(c.Value))
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                // Format any comments before this field
                foreach (var comment in commentsBetweenFields)
                {
                    // Add blank line before comment
                    fieldContext = fieldContext.NextRow().SetIndentColumn();

                    // Format the comment
                    var commentLocation = fieldContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);
                    alreadyFormattedCommentTexts.Add(comment.Value);

                    // Move to next row after comment
                    fieldContext = CreateContextAfterComment(fieldContext, commentEnd);
                    fieldContext = fieldContext.SetIndentColumn();
                }

                // ", "
                var afterComma = fieldContext.Advance(2);

                // field name
                var fieldName = field.Value.FieldName.Value;
                var afterName = afterComma.Advance(fieldName.Length);

                // " : "
                var afterColon = afterName.Advance(3);

                // Format the field type using FormatTypeAnnotation
                var (formattedFieldType, afterType) = FormatTypeAnnotation(field.Value.FieldType, afterColon);

                // Create new field node
                var fieldNameRange = new Range(afterComma.ToLocation(), afterName.ToLocation());
                var newField = new RecordField(
                    FieldName: new Node<string>(fieldNameRange, fieldName),
                    FieldType: formattedFieldType
                );
                var fieldRange = new Range(fieldContext.ToLocation(), afterType.ToLocation());
                formattedFields.Add(new Node<RecordField>(fieldRange, newField));

                // Move to next line
                fieldContext = afterType.NextRow().SetIndentColumn();
                prevField = field;
            }

            // "}"
            var afterCloseBrace = fieldContext.Advance(1);

            var formattedRecordDef = new RecordDefinition(formattedFields);
            var range = new Range(context.ToLocation(), afterCloseBrace.ToLocation());
            return (new Node<TypeAnnotation>(range, new TypeAnnotation.Record(formattedRecordDef)), afterCloseBrace);
        }

        private FormattingResult<Node<Declaration>> FormatCustomTypeDeclaration(
            Declaration.CustomTypeDeclaration customTypeDecl,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            var typeDecl = customTypeDecl.TypeDeclaration;
            var currentContext = context;
            var updatedComments = formattedComments;

            // If there's a documentation comment, advance context past it
            // (the doc comment will be rendered directly by Rendering.RenderCustomType)
            if (typeDecl.Documentation is { } docComment)
            {
                var docCommentLocation = currentContext.ToLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                // Move to next row after doc comment (connects directly to declaration)
                currentContext = CreateContextAfterComment(currentContext, docCommentEnd);
            }

            // Save context position before "type" keyword
            var typeKeywordContext = currentContext;

            // "type "
            currentContext = currentContext.Advance(Keywords.Type.Length);

            // type name
            var typeName = typeDecl.Name.Value;
            var typeNameStartContext = currentContext;
            currentContext = currentContext.Advance(typeName.Length);

            // generics
            foreach (var generic in typeDecl.Generics)
            {
                currentContext = currentContext.Advance(1); // space
                currentContext = currentContext.Advance(generic.Value.Length);
            }

            // Move to next row and indent for "="
            currentContext = currentContext.NextRow().Indent().SetIndentColumn();
            currentContext = currentContext.Advance(2); // "= "

            // Format constructors
            var formattedConstructors = new List<Node<ValueConstructor>>();

            for (var i = 0; i < typeDecl.Constructors.Count; i++)
            {
                var constructor = typeDecl.Constructors[i];
                var constructorStartContext = currentContext;

                if (i > 0)
                {
                    // Check for comments between previous and current constructor
                    var prevConstructor = typeDecl.Constructors[i - 1];
                    var commentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > prevConstructor.Range.End.Row &&
                                   c.Range.Start.Row < constructor.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (commentsBetween.Count != 0)
                    {
                        foreach (var comment in commentsBetween)
                        {
                            // Move to new line for comment
                            currentContext = currentContext.NextRow().SetIndentColumn();

                            // Comments between choice type tags are indented 2 spaces after the indent column
                            // This aligns them 2 spaces to the right of the "|" character
                            var commentContext = currentContext.Advance(2);

                            // Create formatted comment at current position
                            var commentLocation = commentContext.ToLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);

                            updatedComments = updatedComments.Add(formattedComment);

                            // Update context to after the comment (handles multi-line comments)
                            currentContext = new FormattingContext(
                                commentEnd.Row,
                                commentEnd.Column,
                                currentContext.IndentLevel,
                                currentContext.AddHalfIndent);
                        }
                    }

                    // Move to new line and add "| " prefix for subsequent constructors
                    currentContext = currentContext.NextRow();

                    currentContext = currentContext.SetIndentColumn();
                    currentContext = currentContext.Advance(2);
                }

                // constructor name
                var constructorName = constructor.Value.Name.Value;
                var nameStart = currentContext;
                currentContext = currentContext.Advance(constructorName.Length);
                var constructorNameRange = new Range(nameStart.ToLocation(), currentContext.ToLocation());

                // constructor arguments
                var formattedArgs = new List<Node<TypeAnnotation>>();
                Node<TypeAnnotation>? prevArg = null;

                // Arguments with comments should be indented 4 spaces beyond the current indent
                // For custom types, the context is already at the indent for "= " or "| "
                // We need to add 4 more spaces for the argument lines
                var argumentIndent = currentContext.IndentLevel + 1;

                for (var argIndex = 0; argIndex < constructor.Value.Arguments.Count; argIndex++)
                {
                    var arg = constructor.Value.Arguments[argIndex];

                    // Check for comments between previous argument and this one
                    if (prevArg is not null)
                    {
                        var commentsBetweenArgs = originalComments
                            .Where(c => c.Range.Start.Row > prevArg.Range.End.Row &&
                                       c.Range.End.Row < arg.Range.Start.Row)
                            .OrderBy(c => c.Range.Start.Row)
                            .ToList();

                        if (commentsBetweenArgs.Count != 0)
                        {
                            foreach (var comment in commentsBetweenArgs)
                            {
                                // Move to new line for comment, indented 4 spaces past current indent
                                var commentIndentContext = new FormattingContext(
                                    currentContext.CurrentRow + 1,
                                    1,
                                    argumentIndent,
                                    false).SetIndentColumn();

                                // Create formatted comment at current position
                                var commentLocation = commentIndentContext.ToLocation();
                                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                var formattedComment = comment.WithRange(commentLocation, commentEnd);

                                updatedComments = updatedComments.Add(formattedComment);

                                // Update current context to after the comment
                                currentContext = CreateContextAfterComment(commentIndentContext, commentEnd);
                            }

                            // Set the context for the argument
                            currentContext = new FormattingContext(
                                currentContext.CurrentRow,
                                1,
                                argumentIndent,
                                false).SetIndentColumn();
                        }
                        else
                        {
                            // No comments, just add a space before the argument
                            currentContext = currentContext.Advance(1);
                        }
                    }
                    else
                    {
                        // First argument - check if it has comments before it (after constructor name)
                        var commentsBeforeFirstArg = originalComments
                            .Where(c => c.Range.Start.Row > constructor.Value.Name.Range.End.Row &&
                                       c.Range.End.Row < arg.Range.Start.Row)
                            .OrderBy(c => c.Range.Start.Row)
                            .ToList();

                        if (commentsBeforeFirstArg.Count != 0)
                        {
                            foreach (var comment in commentsBeforeFirstArg)
                            {
                                // Move to new line for comment, indented 4 spaces past current indent
                                var commentIndentContext = new FormattingContext(
                                    currentContext.CurrentRow + 1,
                                    1,
                                    argumentIndent,
                                    false).SetIndentColumn();

                                // Create formatted comment at current position
                                var commentLocation = commentIndentContext.ToLocation();
                                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                var formattedComment = comment.WithRange(commentLocation, commentEnd);

                                updatedComments = updatedComments.Add(formattedComment);

                                // Update current context to after the comment
                                currentContext = CreateContextAfterComment(commentIndentContext, commentEnd);
                            }

                            // Set the context for the first argument
                            currentContext = new FormattingContext(
                                currentContext.CurrentRow,
                                1,
                                argumentIndent,
                                false).SetIndentColumn();
                        }
                        else
                        {
                            // No comments before first arg, just add a space
                            currentContext = currentContext.Advance(1);
                        }
                    }

                    var (formattedArg, contextAfterArg) =
                        FormatTypeAnnotation(arg, currentContext);

                    formattedArgs.Add(formattedArg);
                    prevArg = arg;
                    currentContext = contextAfterArg;
                }

                // After processing all arguments, restore the original indent level
                // This is important for subsequent constructors
                currentContext = new FormattingContext(
                    currentContext.CurrentRow,
                    currentContext.CurrentColumn,
                    context.IndentLevel + 1, // Same indent as "= " or "| " prefix
                    false);

                // Create formatted constructor
                var formattedConstructor = new ValueConstructor(
                    Name: new Node<string>(constructorNameRange, constructorName),
                    Arguments: formattedArgs
                );

                var constructorRange = new Range(constructorStartContext.ToLocation(), currentContext.ToLocation());
                formattedConstructors.Add(new Node<ValueConstructor>(constructorRange, formattedConstructor));
            }

            // Create formatted type declaration
            var typeNameRange = new Range(typeNameStartContext.ToLocation(), typeNameStartContext.Advance(typeName.Length).ToLocation());
            var formattedTypeDecl = new TypeStruct(
                Documentation: typeDecl.Documentation,
                Name: new Node<string>(typeNameRange, typeName),
                Generics: typeDecl.Generics,
                Constructors: formattedConstructors
            );

            var declRange = new Range(context.ToLocation(), currentContext.ToLocation());
            var formattedDecl = new Declaration.CustomTypeDeclaration(formattedTypeDecl);
            return new FormattingResult<Node<Declaration>>(
                new Node<Declaration>(declRange, formattedDecl),
                currentContext,
                locationMapper,
                updatedComments);
        }

        private (Node<Declaration>, FormattingContext) FormatInfixDeclaration(
            Declaration.InfixDeclaration infixDecl,
            Range originalRange,
            FormattingContext context)
        {
            // Infix declarations are formatted on a single line as:
            // "infix <direction> <precedence> (<operator>) = <functionName>"
            // Example: "infix right 0 (<|) = apL"

            var infix = infixDecl.Infix;

            // Calculate the full text length to advance context
            var direction = infix.Direction.Value switch
            {
                InfixDirection.Left => "left ",
                InfixDirection.Right => "right",
                InfixDirection.Non => "non  ",
                _ => throw new System.NotImplementedException($"Unknown infix direction: {infix.Direction.Value}")
            };

            var textLength = 6 + direction.Length + 1 + infix.Precedence.Value.ToString().Length + 1 +
                            1 + infix.Operator.Value.Length + 1 + 1 + 1 + 1 + infix.FunctionName.Value.Length;
            // "infix " + direction + " " + precedence + " " + "(" + operator + ")" + " " + "=" + " " + functionName

            var currentContext = context.Advance(textLength);

            var declRange = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<Declaration>(declRange, infixDecl), currentContext);
        }

        private FormattingResult<Node<Declaration>>
            FormatInfixDeclarationWithMapping(
                Declaration.InfixDeclaration infixDecl,
                Range range,
                FormattingContext context,
                ImmutableDictionary<Location, Location> locationMapper,
                ImmutableList<Node<string>> formattedComments)
        {
            var (formatted, nextContext) = FormatInfixDeclaration(infixDecl, range, context);
            return new FormattingResult<Node<Declaration>>(formatted, nextContext, locationMapper, formattedComments);
        }

        /// <summary>
        /// Check if a function signature should be formatted as multiline.
        /// Uses the TypeAnnotation's range to detect if it spans multiple rows.
        /// </summary>
        private static bool IsSignatureMultiLine(Signature signature)
        {
            return signature.TypeAnnotation.Range.Start.Row != signature.TypeAnnotation.Range.End.Row;
        }

        /// <summary>
        /// Formats an optional signature, returning null if the signature is null.
        /// This provides explicit null handling for optional signature values.
        /// </summary>
        private (Node<Signature>?, FormattingContext) FormatOptionalSignature(
            Node<Signature>? signature,
            FormattingContext context)
        {
            if (signature is null)
                return (null, context);

            var isMultiLine = IsSignatureMultiLine(signature.Value);
            var currentContext = context;

            if (isMultiLine)
            {
                // Multi-line signature: format the type annotation properly
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);
                // " :"
                var afterColon = afterSigName.Advance(2);
                // Move to next line and indent for the type annotation
                var typeContext = afterColon.NextRow().Indent().SetIndentColumn();

                // Format the type annotation using FormatTypeAnnotation
                var (formattedTypeAnnotation, afterType) = FormatTypeAnnotation(signature.Value.TypeAnnotation, typeContext);

                // Create new Signature with formatted TypeAnnotation
                var formattedSig = new Signature(
                    Name: signature.Value.Name,
                    TypeAnnotation: formattedTypeAnnotation);

                var sigRange = new Range(currentContext.ToLocation(), afterType.ToLocation());
                var formattedSignature = new Node<Signature>(sigRange, formattedSig);

                // Move to next line for the implementation
                return (formattedSignature, afterType.Dedent().NextRow());
            }
            else
            {
                // Single-line signature
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);
                // " : "
                var afterColon = afterSigName.Advance(3);

                var (formattedTypeAnnotation, afterType) = FormatTypeAnnotation(signature.Value.TypeAnnotation, afterColon);

                // Create new Signature with updated TypeAnnotation range
                var formattedSig = new Signature(
                    Name: signature.Value.Name,
                    TypeAnnotation: formattedTypeAnnotation);

                var sigRange = new Range(currentContext.ToLocation(), afterType.ToLocation());
                var formattedSignature = new Node<Signature>(sigRange, formattedSig);

                // Move to next line for the implementation
                return (formattedSignature, afterType.NextRow());
            }
        }

        #endregion

        #region Pattern Formatting

        private static string FormatPatternText(Pattern pattern)
        {
            return pattern switch
            {
                Pattern.VarPattern varPattern =>
                varPattern.Name,

                Pattern.AllPattern =>
                "_",

                Pattern.UnitPattern =>
                "()",

                Pattern.CharPattern charPattern =>
                $"'{(char)charPattern.Value}'",

                Pattern.StringPattern stringPattern =>
                $"\"{stringPattern.Value}\"",

                Pattern.IntPattern intPattern =>
                intPattern.Value.ToString(),

                Pattern.HexPattern hexPattern =>
                Rendering.RenderHexPattern(hexPattern.Value),

                Pattern.FloatPattern floatPattern =>
                floatPattern.Value.ToString(),

                Pattern.TuplePattern tuplePattern =>
                FormatTuplePattern(tuplePattern),

                Pattern.ListPattern listPattern =>
                FormatListPattern(listPattern),

                Pattern.RecordPattern recordPattern =>
                FormatRecordPattern(recordPattern),

                Pattern.UnConsPattern unConsPattern =>
                FormatUnConsPattern(unConsPattern),

                Pattern.AsPattern asPattern =>
                FormatAsPattern(asPattern),

                Pattern.ParenthesizedPattern parenPattern =>
                $"({FormatPatternText(parenPattern.Pattern.Value)})",

                Pattern.NamedPattern namedPattern =>
                FormatNamedPattern(namedPattern),

                _ =>
                /*
                 * https://github.com/stil4m/elm-syntax/commit/6a78fc2f04b79d72141ae8bc6068a3cb042219c6
                 * Do not support parsing FloatPattern: https://github.com/stil4m/elm-syntax/pull/200
                 * */
                throw new System.NotImplementedException(
                    $"Formatting for pattern type '{pattern.GetType().Name}' is not implemented.")
            };
        }

        private static string FormatNamedPattern(Pattern.NamedPattern namedPattern)
        {
            // Get the constructor name (could be qualified like "Maybe.Just" or simple like "Just")
            var nameParts = new List<string>();
            if (namedPattern.Name.ModuleName is not null && namedPattern.Name.ModuleName.Count > 0)
            {
                nameParts.AddRange(namedPattern.Name.ModuleName);
            }
            nameParts.Add(namedPattern.Name.Name);
            var constructorName = string.Join(".", nameParts);

            // Format arguments if any
            if (namedPattern.Arguments.Count is 0)
            {
                return constructorName;
            }

            var argTexts = namedPattern.Arguments.Select(arg => FormatPatternText(arg.Value));
            return constructorName + " " + string.Join(" ", argTexts);
        }

        private static string FormatTuplePattern(Pattern.TuplePattern tuplePattern)
        {
            var patterns = tuplePattern.Elements.Select(p => FormatPatternText(p.Value));
            return $"( {string.Join(", ", patterns)} )";
        }

        private static string FormatListPattern(Pattern.ListPattern listPattern)
        {
            if (listPattern.Elements.Count is 0)
            {
                return "[]";
            }

            var patterns = listPattern.Elements.Select(p => FormatPatternText(p.Value));
            return $"[ {string.Join(", ", patterns)} ]";
        }

        private static string FormatRecordPattern(Pattern.RecordPattern recordPattern)
        {
            if (recordPattern.Fields.Count is 0)
            {
                return "{}";
            }

            var fields = recordPattern.Fields.Select(f => f.Value);
            return $"{{ {string.Join(", ", fields)} }}";
        }

        private static string FormatUnConsPattern(Pattern.UnConsPattern unConsPattern)
        {
            var head = FormatPatternText(unConsPattern.Head.Value);
            var tail = FormatPatternText(unConsPattern.Tail.Value);
            return $"{head} :: {tail}";
        }

        private static string FormatAsPattern(Pattern.AsPattern asPattern)
        {
            var pattern = FormatPatternText(asPattern.Pattern.Value);
            return $"{pattern} as {asPattern.Name.Value}";
        }

        #endregion

        #region Expression Visitor Methods

        public override (Node<Expression>, FormattingContext) VisitInteger(Expression.Integer expr, FormattingContext context)
        {
            var text = expr.Value.ToString();
            var afterExpr = context.Advance(text.Length);
            var range = new Range(context.ToLocation(), afterExpr.ToLocation());
            return (new Node<Expression>(range, expr), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitLiteral(Expression.Literal expr, FormattingContext context)
        {
            var text =
                Rendering.RenderStringLiteral(
                    expr.Value,
                    isTripleQuoted: expr.IsTripleQuoted);

            // For multiline strings, we need to correctly track row and column positions
            var lines = text.Split('\n');
            FormattingContext afterExpr;
            if (lines.Length > 1)
            {
                // Multiline string: advance through each line
                var endRow = context.CurrentRow + lines.Length - 1;
                var endColumn = lines[^1].Length + 1; // +1 because columns are 1-indexed
                afterExpr = new FormattingContext(
                    endRow,
                    endColumn,
                    context.IndentLevel,
                    context.AddHalfIndent);
            }
            else
            {
                // Single-line string: just advance the column
                afterExpr = context.Advance(text.Length);
            }

            var range = new Range(context.ToLocation(), afterExpr.ToLocation());

            return (new Node<Expression>(range, expr), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitFunctionOrValue(Expression.FunctionOrValue expr, FormattingContext context)
        {
            var currentContext = context;

            // Format module name if present (e.g., "OtherModule" in "OtherModule.otherDecl")
            if (expr.ModuleName.Count > 0)
            {
                foreach (var modulePart in expr.ModuleName)
                {
                    currentContext = currentContext.Advance(modulePart.Length + 1); // module name + dot
                }
            }

            // Format function/value name
            currentContext = currentContext.Advance(expr.Name.Length);

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<Expression>(range, expr), currentContext);
        }

        public override (Node<Expression>, FormattingContext) VisitApplication(Expression.Application expr, FormattingContext context)
        {
            return FormatApplication(expr, context, IsApplicationMultiLine(expr));
        }

        /// <summary>
        /// Check if a function application should be formatted as multiline.
        /// </summary>
        private static bool IsApplicationMultiLine(Expression.Application expr)
        {
            return NodesSpanMultipleRows(expr.Arguments);
        }

        private (Node<Expression>, FormattingContext) FormatApplication(
            Expression.Application expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: format all args with spaces between them
                var currentContext = context;
                var formattedArgs = new List<Node<Expression>>();

                for (var i = 0; i < expr.Arguments.Count; i++)
                {
                    var arg = expr.Arguments[i];
                    var (formattedArg, nextContext) = Visit(arg.Value, currentContext);
                    formattedArgs.Add(formattedArg);

                    // Add space after this arg only if there are more args to come
                    currentContext = i < expr.Arguments.Count - 1
                        ? nextContext.Advance(1)
                        : nextContext;
                }

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(range, new Expression.Application(formattedArgs)), currentContext);
            }
            else
            {
                // Multi-line: first arg on current line, rest indented on new lines
                var formattedArgs = new List<Node<Expression>>();

                // First argument (function) on current line
                var (firstArg, afterFirst) = Visit(expr.Arguments[0].Value, context);
                formattedArgs.Add(firstArg);

                // Subsequent arguments each on their own line, indented
                var argContext = afterFirst.NextRow().Indent().SetIndentColumn();

                for (var i = 1; i < expr.Arguments.Count; i++)
                {
                    var arg = expr.Arguments[i];
                    // Use VisitExpressionNodeLessMapping to preserve Node's Range for proper multiline and comment detection
                    var (formattedArg, nextContext) = VisitExpressionNodeLessMapping(arg, argContext);
                    formattedArgs.Add(formattedArg);

                    // Only move to next row if there are more arguments
                    argContext = i < expr.Arguments.Count - 1
                        ? nextContext.NextRow().SetIndentColumn()
                        : nextContext;
                }

                // Context after all arguments (last line)
                var finalContext = argContext.Dedent();

                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                return (new Node<Expression>(range, new Expression.Application(formattedArgs)), finalContext);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitOperatorApplication(Expression.OperatorApplication expr, FormattingContext context)
        {
            // Check if this operator application should be multiline
            // An operator application is multiline if the operands are on different rows
            var isMultiline = RangesOnDifferentRows(expr.Left.Range, expr.Right.Range);

            if (isMultiline)
            {
                // Multiline: left on one line, operator and right on next line
                var (leftFormatted, afterLeft) = VisitExpressionNodeLessMapping(expr.Left, context);

                // Determine the offset for the operator line
                // When we're at a "comma-adjusted" position (e.g., after ", " in a list/tuple), use offset 2
                // When at the canonical indent position, use offset 4
                var canonicalColumn = 1 + (context.IndentLevel * 4) + (context.AddHalfIndent ? 4 : 0);
                var offset = (context.AddHalfIndent && context.CurrentColumn != canonicalColumn) ? 2 : 4;

                var opLineContext = afterLeft.NextRow().SetColumn(context.CurrentColumn + offset);
                var afterOp = opLineContext.Advance(expr.Operator.Length);
                var afterOpSpace = afterOp.Advance(1); // space after operator

                var (rightFormatted, afterRight) = VisitExpressionNodeLessMapping(expr.Right, afterOpSpace);

                var formattedExpr = new Expression.OperatorApplication(
                    Operator: expr.Operator,
                    Direction: expr.Direction,
                    Left: leftFormatted,
                    Right: rightFormatted
                );

                var range = new Range(context.ToLocation(), afterRight.ToLocation());
                return (new Node<Expression>(range, formattedExpr), afterRight);
            }
            else
            {
                // Single-line: left op right with spaces
                var (leftFormatted, afterLeft) = VisitExpressionNodeLessMapping(expr.Left, context);
                var afterLeftSpace = afterLeft.Advance(1); // space before operator
                var afterOp = afterLeftSpace.Advance(expr.Operator.Length);
                var afterOpSpace = afterOp.Advance(1); // space after operator
                var (rightFormatted, afterRight) = VisitExpressionNodeLessMapping(expr.Right, afterOpSpace);

                var formattedExpr = new Expression.OperatorApplication(
                    Operator: expr.Operator,
                    Direction: expr.Direction,
                    Left: leftFormatted,
                    Right: rightFormatted
                );

                var range = new Range(context.ToLocation(), afterRight.ToLocation());
                return (new Node<Expression>(range, formattedExpr), afterRight);
            }
        }

        /// <summary>
        /// Helper method to visit an expression Node with access to its original Range.
        /// This is crucial for detecting multiline status at each nesting level independently,
        /// particularly for single-element lists where the closing bracket may be on a new line.
        /// </summary>
        /// <summary>
        /// Visit an expression node and record location mappings.
        /// </summary>
        private (Node<Expression>, FormattingContext, ImmutableDictionary<Location, Location>, ImmutableList<Node<string>>) VisitExpressionNode(
            Node<Expression> node,
            FormattingContext context,
            ImmutableDictionary<Location, Location> locationMapper,
            ImmutableList<Node<string>> formattedComments)
        {
            // Special handling for list expressions to support comments between list items
            if (node.Value is Expression.ListExpr listExpr && listExpr.Elements.Count > 0)
            {
                var (formattedNode, newContext, updatedComments) = VisitListExprWithComments(
                    listExpr, node.Range, context, formattedComments);

                // Record the mapping from original to formatted range
                var updatedMapper = RecordMapping(node.Range, formattedNode.Range, locationMapper);

                return (formattedNode, newContext, updatedMapper, updatedComments);
            }

            // Special handling for if-blocks to support comments before the condition
            if (node.Value is Expression.IfBlock ifBlock)
            {
                var (formattedNode, newContext, updatedComments) = VisitIfBlockWithComments(
                    ifBlock, node.Range, context, formattedComments);

                // Record the mapping from original to formatted range
                var updatedMapper = RecordMapping(node.Range, formattedNode.Range, locationMapper);

                return (formattedNode, newContext, updatedMapper, updatedComments);
            }

            var (formattedNodeRegular, newContextRegular) = VisitExpressionNodeLessMapping(node, context);

            // Record the mapping from original to formatted range
            var updatedMapperRegular = RecordMapping(node.Range, formattedNodeRegular.Range, locationMapper);

            return (formattedNodeRegular, newContextRegular, updatedMapperRegular, formattedComments);
        }

        private (Node<Expression>, FormattingContext) VisitExpressionNodeLessMapping(
            Node<Expression> node,
            FormattingContext context)
        {
            // For non-empty ListExpr, use VisitListExprWithComments to handle comments inside lists
            // Empty lists don't have comments, so they can use the simpler path
            if (node.Value is Expression.ListExpr listExpr && listExpr.Elements.Count > 0)
            {
                var (formattedNode, newContext, formattedComments) = VisitListExprWithComments(
                    listExpr, node.Range, context, ImmutableList<Node<string>>.Empty);

                // Add any formatted comments to the accumulated comments
                foreach (var comment in formattedComments)
                {
                    _accumulatedComments.Add(comment);
                }

                return (formattedNode, newContext);
            }

            // For empty ListExpr, use the Node's Range to detect multiline at this level
            if (node.Value is Expression.ListExpr emptyListExpr)
            {
                return VisitListExprWithRange(emptyListExpr, node.Range, context);
            }

            // For RecordExpr, use the Node's Range to detect multiline at this level
            if (node.Value is Expression.RecordExpr recordExpr)
            {
                return VisitRecordExprWithRange(recordExpr, node.Range, context);
            }

            // For TupledExpression, use the Node's Range to detect multiline at this level
            if (node.Value is Expression.TupledExpression tupledExpr)
            {
                return VisitTupledExpressionWithRange(tupledExpr, node.Range, context);
            }

            // For other expressions, use the regular Visit
            return Visit(node.Value, context);
        }

        /// <summary>
        /// Format a list expression with access to its original Range for accurate multiline detection.
        /// The original Range is essential for single-element lists to detect if the closing bracket
        /// is on a different row than the element (indicating multiline format).
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitListExprWithRange(
            Expression.ListExpr expr,
            Range originalRange,
            FormattingContext context)
        {
            if (expr.Elements.Count is 0)
            {
                // Empty list: "[]"
                var afterList = context.Advance(Keywords.EmptyList.Length);
                var emptyListRange = new Range(context.ToLocation(), afterList.ToLocation());
                return (new Node<Expression>(emptyListRange, expr), afterList);
            }

            // Detect if this specific list is multiline by checking:
            // 1. For multiple elements: if they span different rows
            // 2. For single element: if the parent list Range (including closing bracket) spans multiple rows
            //    OR if the element itself spans multiple rows (indicating multiline formatting)
            bool isMultiLine;
            if (expr.Elements.Count >= 2)
            {
                // Multiple elements: use helper to check if they span different rows
                isMultiLine = NodesSpanMultipleRows(expr.Elements);
            }
            else
            {
                // Single element: check if the parent list Range spans multiple rows
                // OR if the element itself spans multiple rows
                // This detects cases like: [ [ 1, 2, 3 ]\n] where inner list is single-line
                // but outer closing bracket is on a new line, or [ { a = 1\n    }\n] where the
                // element Range has been marked as multiline by a preprocessor
                var element = expr.Elements[0];
                var elementEndRow = element.Range.End.Row;
                var elementStartRow = element.Range.Start.Row;
                var listEndRow = originalRange.End.Row;
                isMultiLine = listEndRow > elementEndRow || elementEndRow > elementStartRow;
            }

            return FormatList(expr, context, isMultiLine);
        }

        private (Node<Expression>, FormattingContext, ImmutableList<Node<string>>) VisitListExprWithComments(
            Expression.ListExpr expr,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Node<string>> formattedComments)
        {
            if (expr.Elements.Count is 0)
            {
                // Empty list: "[]"
                var afterList = context.Advance(Keywords.EmptyList.Length);
                var emptyListRange = new Range(context.ToLocation(), afterList.ToLocation());
                return (new Node<Expression>(emptyListRange, expr), afterList, formattedComments);
            }

            // Detect if this specific list is multiline
            bool isMultiLine;
            if (expr.Elements.Count >= 2)
            {
                isMultiLine = NodesSpanMultipleRows(expr.Elements);
            }
            else
            {
                var element = expr.Elements[0];
                var elementEndRow = element.Range.End.Row;
                var elementStartRow = element.Range.Start.Row;
                var listEndRow = originalRange.End.Row;
                isMultiLine = listEndRow > elementEndRow || elementEndRow > elementStartRow;
            }

            return FormatListWithComments(expr, context, isMultiLine, formattedComments);
        }

        public override (Node<Expression>, FormattingContext) VisitListExpr(Expression.ListExpr expr, FormattingContext context)
        {
            // This method is called by the base visitor pattern when we don't have direct access
            // to the original Node's Range (only the Expression value). This occurs when called
            // from external code or the base Visit() dispatcher.
            // We synthesize a Range based on element positions for multiline detection.

            if (expr.Elements.Count is 0)
            {
                // For empty lists, use a minimal placeholder Range (doesn't affect multiline detection)
                var emptyRange = new Range(new Location(1, 1), new Location(1, 3));  // "[]" is 2 chars
                return VisitListExprWithRange(expr, emptyRange, context);
            }

            // Synthesize a Range that encompasses all elements
            // This provides fallback multiline detection when the original Range isn't available
            var startLoc = expr.Elements[0].Range.Start;
            var endLoc = expr.Elements[expr.Elements.Count - 1].Range.End;
            var synthesizedRange = new Range(startLoc, endLoc);

            return VisitListExprWithRange(expr, synthesizedRange, context);
        }

        private (Node<Expression>, FormattingContext) FormatList(
            Expression.ListExpr expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "[ elem1, elem2, elem3 ]"
                var afterOpen = context.Advance(Keywords.ListOpen.Length);
                var currentContext = afterOpen;
                var formattedElements = new List<Node<Expression>>();

                for (var i = 0; i < expr.Elements.Count; i++)
                {
                    var (formattedElem, nextContext) = VisitExpressionNodeLessMapping(expr.Elements[i], currentContext);
                    formattedElements.Add(formattedElem);
                    currentContext = i < expr.Elements.Count - 1
                        ? nextContext.Advance(Keywords.Comma.Length)
                        : nextContext.Advance(Keywords.ListClose.Length);
                }

                var listRange = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(listRange, new Expression.ListExpr(formattedElements)), currentContext);
            }
            else
            {
                // Multi-line: first element on same line as "[", rest on new lines with commas
                var openingBracketColumn = context.CurrentColumn;
                var afterOpen = context.Advance(Keywords.ListOpen.Length);
                var formattedElements = new List<Node<Expression>>();

                // First element on same line as "["
                var (firstElem, afterFirst) = VisitExpressionNodeLessMapping(expr.Elements[0], afterOpen);
                formattedElements.Add(firstElem);

                // Subsequent elements on new lines with ", " prefix
                var nextRow = afterFirst.CurrentRow + 1;

                for (var i = 1; i < expr.Elements.Count; i++)
                {
                    // Create context at opening bracket column for comma
                    // When creating new contexts for children, convert parent's AddHalfIndent to a full IndentLevel
                    var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    var commaContext = new FormattingContext(
                        nextRow,
                        openingBracketColumn,
                        baseIndentLevel);

                    // ", " at the start of the line (at opening bracket column)
                    var afterComma = commaContext.Advance(2);

                    // Add half indent for subsequent list elements to support nested multiline expressions
                    var elemContext = afterComma.WithHalfIndent(true);

                    var (formattedElem, nextContext) = VisitExpressionNodeLessMapping(expr.Elements[i], elemContext);
                    formattedElements.Add(formattedElem);

                    // Move to next line
                    nextRow = nextContext.CurrentRow + 1;
                }

                // Closing bracket "]" should be at opening bracket column
                var closingBracketContext = new FormattingContext(
                    nextRow,
                    openingBracketColumn,
                    context.IndentLevel);
                var afterClose = closingBracketContext.Advance(1);

                var listRange = new Range(context.ToLocation(), afterClose.ToLocation());
                return (new Node<Expression>(listRange, new Expression.ListExpr(formattedElements)), afterClose);
            }
        }

        private (Node<Expression>, FormattingContext, ImmutableList<Node<string>>) FormatListWithComments(
            Expression.ListExpr expr,
            FormattingContext context,
            bool isMultiLine,
            ImmutableList<Node<string>> formattedComments)
        {
            if (!isMultiLine)
            {
                // Single-line lists don't have comments between items
                var (formatted, newContext) = FormatList(expr, context, false);
                return (formatted, newContext, formattedComments);
            }
            else
            {
                // Multi-line: first element on same line as "[", rest on new lines with commas
                var openingBracketColumn = context.CurrentColumn;
                var afterOpen = context.Advance(2); // "[ "
                var formattedElements = new List<Node<Expression>>();
                var updatedComments = formattedComments;

                // Check for comments before the first element (on the same line as "[" or between "[" and first element)
                var firstElement = expr.Elements[0];
                var commentsBeforeFirst = originalComments
                    .Where(c => c.Range.Start.Row >= context.CurrentRow &&
                               c.Range.End.Row < firstElement.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                FormattingContext firstElemContext;
                if (commentsBeforeFirst.Count != 0)
                {
                    // Has comments before first element - put them on same line as "[" and first element on next line
                    firstElemContext = afterOpen;

                    foreach (var comment in commentsBeforeFirst)
                    {
                        // Format comment at current position
                        var commentLocation = firstElemContext.ToLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);

                        // Add to _accumulatedComments so it gets rendered
                        _accumulatedComments.Add(formattedComment);

                        // Move to next line after the comment
                        firstElemContext = CreateContextAfterComment(firstElemContext, commentEnd);
                    }

                    // Position the first element on a new line with proper indentation
                    // Align with where elements after comma would be (openingBracketColumn + 2)
                    var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    firstElemContext = new FormattingContext(
                        firstElemContext.CurrentRow,
                        openingBracketColumn + 2, // 2 spaces past the bracket to align with "[ "
                        baseIndentLevel);
                }
                else
                {
                    firstElemContext = afterOpen;
                }

                // First element
                // Check if first element is a list that might contain comments
                Node<Expression> firstElem;
                FormattingContext afterFirst;
                if (expr.Elements[0].Value is Expression.ListExpr firstListExpr && firstListExpr.Elements.Count > 0)
                {
                    ImmutableList<Node<string>> firstComments;
                    (firstElem, afterFirst, firstComments) = VisitListExprWithComments(
                        firstListExpr, expr.Elements[0].Range, firstElemContext, updatedComments);
                    updatedComments = firstComments;
                }
                else
                {
                    (firstElem, afterFirst) = VisitExpressionNodeLessMapping(expr.Elements[0], firstElemContext);
                }
                formattedElements.Add(firstElem);

                // Check for end-of-line comment after the first element
                var endOfLineCommentAfterFirst = originalComments
                    .Where(c => c.Range.Start.Row == firstElement.Range.End.Row &&
                               c.Range.Start.Column > firstElement.Range.End.Column)
                    .FirstOrDefault();

                if (endOfLineCommentAfterFirst is not null)
                {
                    // Place comment on same line as first element, after a space
                    var commentLocation = new Location(afterFirst.CurrentRow, afterFirst.CurrentColumn + 1);
                    var commentEnd = CalculateCommentEndLocation(commentLocation, endOfLineCommentAfterFirst.Value);
                    var formattedComment = endOfLineCommentAfterFirst.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);
                }

                // Subsequent elements on new lines with ", " prefix
                var nextRow = afterFirst.CurrentRow + 1;

                for (var i = 1; i < expr.Elements.Count; i++)
                {
                    var prevElement = expr.Elements[i - 1];
                    var currentElement = expr.Elements[i];

                    // Check for end-of-line comments after the previous element (same row)
                    var endOfLineComment = originalComments
                        .Where(c => c.Range.Start.Row == prevElement.Range.End.Row &&
                                   c.Range.Start.Column > prevElement.Range.End.Column)
                        .FirstOrDefault();

                    // Track if we already handled an end-of-line comment (first element case)
                    var hasEndOfLineComment = endOfLineComment is not null && i > 1;

                    // Check for comments on separate rows between previous and current element
                    var commentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > prevElement.Range.End.Row &&
                                   c.Range.Start.Row < currentElement.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    // Add blank line and comments if any exist
                    if (commentsBetween.Count != 0)
                    {
                        // Add blank line before comment
                        nextRow++;

                        foreach (var comment in commentsBetween)
                        {
                            // Create context at opening bracket column for comment
                            var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                            var commentContext = new FormattingContext(
                                nextRow,
                                openingBracketColumn,
                                baseIndentLevel);

                            // Create formatted comment at current position
                            var commentLocation = commentContext.ToLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);

                            updatedComments = updatedComments.Add(formattedComment);

                            // Move to next line after the comment (handles multi-line comments)
                            nextRow = commentEnd.Row + 1;
                        }
                    }

                    // Create context at opening bracket column for comma
                    var baseIndentLevelForComma = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    var commaContext = new FormattingContext(
                        nextRow,
                        openingBracketColumn,
                        baseIndentLevelForComma);

                    // ", " at the start of the line (at opening bracket column)
                    var afterComma = commaContext.Advance(2);

                    // Add half indent for subsequent list elements to support nested multiline expressions
                    var elemContext = afterComma.WithHalfIndent(true);

                    // Check if element is a list expression that might contain comments
                    Node<Expression> formattedElem;
                    FormattingContext nextContext;
                    if (currentElement.Value is Expression.ListExpr nestedListExpr && nestedListExpr.Elements.Count > 0)
                    {
                        // Format nested list with comment support
                        ImmutableList<Node<string>> nestedComments;
                        (formattedElem, nextContext, nestedComments) = VisitListExprWithComments(
                            nestedListExpr, currentElement.Range, elemContext, updatedComments);
                        updatedComments = nestedComments;
                    }
                    else
                    {
                        (formattedElem, nextContext) = VisitExpressionNodeLessMapping(currentElement, elemContext);
                    }

                    formattedElements.Add(formattedElem);

                    // Check for end-of-line comment after this element (on same row, after element ends)
                    var endOfLineCommentAfterElem = originalComments
                        .Where(c => c.Range.Start.Row == currentElement.Range.End.Row &&
                                   c.Range.Start.Column > currentElement.Range.End.Column)
                        .FirstOrDefault();

                    if (endOfLineCommentAfterElem is not null)
                    {
                        // Place comment on same line as element, after a space
                        var commentLocation = new Location(nextContext.CurrentRow, nextContext.CurrentColumn + 1);
                        var commentEnd = CalculateCommentEndLocation(commentLocation, endOfLineCommentAfterElem.Value);
                        var formattedComment = endOfLineCommentAfterElem.WithRange(commentLocation, commentEnd);

                        updatedComments = updatedComments.Add(formattedComment);
                    }

                    // Move to next line
                    nextRow = nextContext.CurrentRow + 1;
                }

                // Closing bracket "]" should be at opening bracket column
                var closingBracketContext = new FormattingContext(
                    nextRow,
                    openingBracketColumn,
                    context.IndentLevel);
                var afterClose = closingBracketContext.Advance(1);

                var listRange = new Range(context.ToLocation(), afterClose.ToLocation());
                return (new Node<Expression>(listRange, new Expression.ListExpr(formattedElements)), afterClose, updatedComments);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitIfBlock(Expression.IfBlock expr, FormattingContext context)
        {
            // Check if the condition expression spans multiple lines in the original source
            // Also check for comments between the if keyword and condition
            var isConditionMultiLine = expr.Condition.Range.Start.Row != expr.Condition.Range.End.Row;

            // Track which comments have already been formatted to avoid duplicates
            var alreadyFormattedCommentTexts = new HashSet<string>(
                _accumulatedComments.Select(c => c.Value));

            // Find comments that appear before the condition expression (within the if block)
            var commentsBeforeCondition = originalComments
                .Where(c => c.Range.End.Row < expr.Condition.Range.Start.Row &&
                           c.Range.Start.Row >= expr.Condition.Range.Start.Row - 5 &&
                           !alreadyFormattedCommentTexts.Contains(c.Value))
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // Find comments between condition and then keyword
            var commentsBetweenConditionAndThen = originalComments
                .Where(c => c.Range.Start.Row > expr.Condition.Range.End.Row &&
                           c.Range.Start.Row <= expr.Condition.Range.End.Row + 1 &&
                           c.Range.End.Row < expr.ThenBlock.Range.Start.Row &&
                           !alreadyFormattedCommentTexts.Contains(c.Value))
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // If there are comments before the condition that would push condition to a new line, treat as multiline
            isConditionMultiLine = isConditionMultiLine || commentsBeforeCondition.Count != 0 || commentsBetweenConditionAndThen.Count != 0;

            Node<Expression> condFormatted;
            FormattingContext afterCond;
            FormattingContext afterThen;

            if (isConditionMultiLine)
            {
                // Multi-line condition format:
                // if
                //     -- comment
                //     condition
                // then
                //     ...

                // "if"
                var afterIf = context.Advance(2);

                // Move to next line and indent for condition (or comment before condition)
                var condContext = afterIf.NextRow().Indent().SetIndentColumn();

                // Format any comments that appear before the condition
                foreach (var comment in commentsBeforeCondition)
                {
                    // Create formatted comment at current position
                    var commentLocation = condContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);

                    // Advance to next line after the comment ends
                    condContext = CreateContextAfterComment(condContext, commentEnd);
                }

                (condFormatted, afterCond) = VisitExpressionNodeLessMapping(expr.Condition, condContext);

                // Format any comments between condition and then
                foreach (var comment in commentsBetweenConditionAndThen)
                {
                    // Move to next line at same indent level as condition
                    afterCond = afterCond.NextRow().SetIndentColumn();

                    var commentLocation = afterCond.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);
                    afterCond = CreateContextAfterCommentSameIndent(afterCond, commentEnd);
                }

                // "then" on a new line at the same indentation as "if"
                var thenLineContext = afterCond.Dedent().NextRow().SetIndentColumn();
                afterThen = thenLineContext.Advance(4); // "then"
            }
            else
            {
                // Single-line condition format:
                // if condition then
                //     ...

                // "if " (2 chars for "if" + 1 space)
                var afterIf = context.Advance(Keywords.IfSpace.Length);
                (condFormatted, afterCond) = Visit(expr.Condition.Value, afterIf);

                // " then" (1 space + 4 chars for "then")
                afterThen = afterCond.Advance(Keywords.SpaceThen.Length);
            }

            // Find and format comments that appear after 'then' keyword but before the then-block expression
            var thenKeywordRow = expr.Condition.Range.End.Row + 1 + commentsBetweenConditionAndThen.Count;
            var commentsAfterThenKeyword = originalComments
                .Where(c => c.Range.Start.Row > thenKeywordRow &&
                           c.Range.End.Row < expr.ThenBlock.Range.Start.Row &&
                           !alreadyFormattedCommentTexts.Contains(c.Value))
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            var thenContext = afterThen.NextRow().Indent().SetIndentColumn();

            // Format any comments that appear after 'then' keyword but before the then expression
            foreach (var comment in commentsAfterThenKeyword)
            {
                var commentLocation = thenContext.ToLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);

                _accumulatedComments.Add(formattedComment);
                thenContext = CreateContextAfterComment(thenContext, commentEnd);
            }

            var (thenFormatted, afterThenExpr) = Visit(expr.ThenBlock.Value, thenContext);

            // Check if else block is an if-expression (for "else if" formatting)
            var isElseIf = expr.ElseBlock.Value is Expression.IfBlock;

            // For else-if chains, check if there are comments or multiline conditions in nested if
            var hasCommentsBetweenElseAndIf = false;
            var nestedIfHasMultilineCondition = false;
            if (isElseIf)
            {
                hasCommentsBetweenElseAndIf = originalComments
                    .Any(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                             c.Range.End.Row < expr.ElseBlock.Range.Start.Row &&
                             !alreadyFormattedCommentTexts.Contains(c.Value));

                // Check if the nested if has a multiline condition or comments before its condition
                var nestedIfBlock = (Expression.IfBlock)expr.ElseBlock.Value;
                var nestedConditionIsMultiLine = nestedIfBlock.Condition.Range.Start.Row != nestedIfBlock.Condition.Range.End.Row;
                var nestedHasCommentsBeforeCondition = originalComments
                    .Any(c => c.Range.End.Row < nestedIfBlock.Condition.Range.Start.Row &&
                             c.Range.Start.Row >= nestedIfBlock.Condition.Range.Start.Row - 5 &&
                             c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                             !alreadyFormattedCommentTexts.Contains(c.Value));
                nestedIfHasMultilineCondition = nestedConditionIsMultiLine || nestedHasCommentsBeforeCondition;
            }

            // Blank line before else - position at the same column where 'if' started
            var elseLineContext = afterThenExpr.Dedent().NextRow().NextRow().SetColumn(context.CurrentColumn);

            // "else"
            var afterElseKeyword = elseLineContext.Advance(4);

            FormattingContext elseContext;
            if (isElseIf && !hasCommentsBetweenElseAndIf && !nestedIfHasMultilineCondition)
            {
                // "else if" - keep on same line with space
                elseContext = afterElseKeyword.Advance(1);
            }
            else if (isElseIf && hasCommentsBetweenElseAndIf)
            {
                // else-if with comments between else and if
                elseContext = afterElseKeyword.NextRow().SetIndentColumn();

                // Format comments at the else/if level
                var commentsBeforeNestedIf = originalComments
                    .Where(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                               c.Range.End.Row < expr.ElseBlock.Range.Start.Row &&
                               !alreadyFormattedCommentTexts.Contains(c.Value))
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforeNestedIf)
                {
                    var commentLocation = elseContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);
                    elseContext = CreateContextAfterCommentSameIndent(elseContext, commentEnd);
                }

                // Move to next row for the nested "if"
                if (commentsBeforeNestedIf.Count != 0)
                {
                    elseContext = elseContext.NextRow().SetIndentColumn();
                }
            }
            else if (isElseIf && nestedIfHasMultilineCondition)
            {
                // else-if where the nested if has multiline condition (comments before condition)
                elseContext = afterElseKeyword.Advance(1);
            }
            else if (isElseIf)
            {
                // else-if without any special cases - format on same line
                elseContext = afterElseKeyword.Advance(1);
            }
            else
            {
                // Regular else - move to next line and indent
                elseContext = afterElseKeyword.NextRow().Indent().SetIndentColumn();

                // Find and format comments before the else expression (for non-else-if cases)
                var commentsBeforeElse = originalComments
                    .Where(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                               c.Range.End.Row < expr.ElseBlock.Range.Start.Row &&
                               !alreadyFormattedCommentTexts.Contains(c.Value))
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforeElse)
                {
                    var commentLocation = elseContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    _accumulatedComments.Add(formattedComment);
                    elseContext = CreateContextAfterComment(elseContext, commentEnd);
                }
            }

            var (elseFormatted, afterElseExpr) = Visit(expr.ElseBlock.Value, elseContext);

            var formattedIf = new Expression.IfBlock(
                Condition: condFormatted,
                ThenBlock: thenFormatted,
                ElseBlock: elseFormatted
            );

            var finalContext = isElseIf ? afterElseExpr : afterElseExpr.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Expression>(range, formattedIf), finalContext);
        }

        private (Node<Expression>, FormattingContext, ImmutableList<Node<string>>) VisitIfBlockWithComments(
            Expression.IfBlock expr,
            Range originalRange,
            FormattingContext context,
            ImmutableList<Node<string>> formattedComments)
        {
            var updatedComments = formattedComments;

            // Check if the condition expression spans multiple lines in the original source
            // Also check for comments between the if keyword and condition
            var isConditionMultiLine = expr.Condition.Range.Start.Row != expr.Condition.Range.End.Row;

            // Track which original comments have already been formatted (by their text content)
            // This prevents double-formatting when parent and nested calls both try to handle the same comment
            var alreadyFormattedCommentTexts = new HashSet<string>(
                formattedComments.Select(c => c.Value));

            // Find comments that appear before the condition expression (within the if block)
            // Exclude comments that have already been formatted by the parent
            var commentsBeforeCondition = originalComments
                .Where(c => c.Range.End.Row < expr.Condition.Range.Start.Row &&
                           c.Range.Start.Row >= expr.Condition.Range.Start.Row - 5 &&
                           !alreadyFormattedCommentTexts.Contains(c.Value))
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // Find comments between condition and then keyword (NOT after then keyword)
            // In multiline format, 'then' appears on the row after condition ends
            // So comments on rows between condition.End.Row and condition.End.Row + 1 (inclusive)
            // would be between condition and 'then'
            var commentsBetweenConditionAndThen = originalComments
                .Where(c => c.Range.Start.Row > expr.Condition.Range.End.Row &&
                           c.Range.Start.Row <= expr.Condition.Range.End.Row + 1 &&
                           c.Range.End.Row < expr.ThenBlock.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            // If there are comments before condition or between condition and then, treat as multiline
            isConditionMultiLine = isConditionMultiLine || commentsBeforeCondition.Count != 0 || commentsBetweenConditionAndThen.Count != 0;

            Node<Expression> condFormatted;
            FormattingContext afterCond;
            FormattingContext afterThen;

            if (isConditionMultiLine)
            {
                // Multi-line condition format:
                // if
                //     -- comment
                //     condition
                // then
                //     ...

                // "if"
                var afterIf = context.Advance(2);

                // Move to next line and indent for condition (or comment before condition)
                var condContext = afterIf.NextRow().Indent().SetIndentColumn();

                // Format any comments that appear before the condition
                foreach (var comment in commentsBeforeCondition)
                {
                    // Create formatted comment at current position
                    var commentLocation = condContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    updatedComments = updatedComments.Add(formattedComment);

                    // Advance to next line after the comment ends
                    condContext = CreateContextAfterComment(condContext, commentEnd);
                }

                (condFormatted, afterCond) = VisitExpressionNodeLessMapping(expr.Condition, condContext);

                // Format any comments that appear between condition and then (at same indent as condition)
                foreach (var comment in commentsBetweenConditionAndThen)
                {
                    // Move to next line at same indent level as condition
                    afterCond = afterCond.NextRow().SetIndentColumn();

                    var commentLocation = afterCond.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    updatedComments = updatedComments.Add(formattedComment);
                    afterCond = CreateContextAfterCommentSameIndent(afterCond, commentEnd);
                }

                // "then" on a new line at the same indentation as "if"
                var thenLineContext = afterCond.Dedent().NextRow().SetIndentColumn();
                afterThen = thenLineContext.Advance(Keywords.Then.Length);
            }
            else
            {
                // Single-line condition format:
                // if condition then
                //     ...

                // "if " (2 chars for "if" + 1 space)
                var afterIf = context.Advance(Keywords.IfSpace.Length);
                (condFormatted, afterCond) = Visit(expr.Condition.Value, afterIf);

                // " then" (1 space + 4 chars for "then")
                afterThen = afterCond.Advance(Keywords.SpaceThen.Length);
            }

            // Find and format comments that appear after 'then' keyword but before the then-block expression
            // We need to format these so they get proper positions in the formatted output
            var thenKeywordRow = expr.Condition.Range.End.Row + 1 + commentsBetweenConditionAndThen.Count;
            var commentsAfterThenKeyword = originalComments
                .Where(c => c.Range.Start.Row > thenKeywordRow &&
                           c.Range.End.Row < expr.ThenBlock.Range.Start.Row)
                .OrderBy(c => c.Range.Start.Row)
                .ToList();

            var thenContext = afterThen.NextRow().Indent().SetIndentColumn();

            // Format any comments that appear after 'then' keyword but before the then expression
            foreach (var comment in commentsAfterThenKeyword)
            {
                var commentLocation = thenContext.ToLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);

                updatedComments = updatedComments.Add(formattedComment);
                thenContext = CreateContextAfterComment(thenContext, commentEnd);
            }

            var (thenFormatted, afterThenExpr) = Visit(expr.ThenBlock.Value, thenContext);

            // Check if else block is an if-expression (for "else if" formatting)
            var isElseIf = expr.ElseBlock.Value is Expression.IfBlock;

            // Blank line before else
            var elseLineContext = afterThenExpr.Dedent().NextRow().NextRow().SetIndentColumn();

            // "else"
            var afterElseKeyword = elseLineContext.Advance(4);

            // For else-if chains, check if there are comments between "else" and the nested "if"
            // (not between "if" and its condition - those are handled by the nested VisitIfBlockWithComments)
            // Comments between "else" and "if" have row < nestedIfBlock's range start row
            var hasCommentsBetweenElseAndIf = false;
            var nestedIfHasMultilineCondition = false;
            if (isElseIf)
            {
                hasCommentsBetweenElseAndIf = originalComments
                    .Any(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                             c.Range.End.Row < expr.ElseBlock.Range.Start.Row);

                // Check if the nested if has a multiline condition or comments before its condition
                var nestedIfBlock = (Expression.IfBlock)expr.ElseBlock.Value;
                var nestedConditionIsMultiLine = nestedIfBlock.Condition.Range.Start.Row != nestedIfBlock.Condition.Range.End.Row;
                var nestedHasCommentsBeforeCondition = originalComments
                    .Any(c => c.Range.End.Row < nestedIfBlock.Condition.Range.Start.Row &&
                             c.Range.Start.Row >= nestedIfBlock.Condition.Range.Start.Row - 5 &&
                             c.Range.Start.Row > expr.ThenBlock.Range.End.Row);
                nestedIfHasMultilineCondition = nestedConditionIsMultiLine || nestedHasCommentsBeforeCondition;
            }

            FormattingContext elseContext;
            if (isElseIf && !hasCommentsBetweenElseAndIf && !nestedIfHasMultilineCondition)
            {
                // "else if" - keep on same line with space
                elseContext = afterElseKeyword.Advance(1);
            }
            else if (isElseIf && hasCommentsBetweenElseAndIf)
            {
                // else-if with comments between else and if
                // Format: else\n-- comment\nif\n    condition
                // Position comments and if at else's indent level
                elseContext = afterElseKeyword.NextRow().SetIndentColumn();

                // Format comments at the else/if level
                var commentsBeforeNestedIf = originalComments
                    .Where(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                               c.Range.End.Row < expr.ElseBlock.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforeNestedIf)
                {
                    var commentLocation = elseContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    updatedComments = updatedComments.Add(formattedComment);
                    elseContext = CreateContextAfterCommentSameIndent(elseContext, commentEnd);
                }

                // Move to next row for the nested "if"
                if (commentsBeforeNestedIf.Count != 0)
                {
                    elseContext = elseContext.NextRow().SetIndentColumn();
                }
            }
            else if (isElseIf && nestedIfHasMultilineCondition)
            {
                // else-if where the nested if has multiline condition (comments before condition)
                // Format as: else if\n    -- comment\n    condition
                // Position context after "else " so nested call outputs "if" there
                elseContext = afterElseKeyword.Advance(1);
            }
            else if (isElseIf)
            {
                // else-if without any special cases - format on same line
                // This shouldn't happen given the conditions above, but as a fallback
                elseContext = afterElseKeyword.Advance(1);
            }
            else
            {
                // Regular else - move to next line and indent
                elseContext = afterElseKeyword.NextRow().Indent().SetIndentColumn();
            }

            // Find comments that appear before the else-block expression (for non-else-if cases)
            if (!isElseIf)
            {
                var commentsBeforeElse = originalComments
                    .Where(c => c.Range.Start.Row > expr.ThenBlock.Range.End.Row &&
                               c.Range.End.Row < expr.ElseBlock.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                // Format any comments that appear before the else expression
                foreach (var comment in commentsBeforeElse)
                {
                    var commentLocation = elseContext.ToLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);

                    updatedComments = updatedComments.Add(formattedComment);
                    elseContext = CreateContextAfterComment(elseContext, commentEnd);
                }
            }

            Node<Expression> elseFormatted;
            FormattingContext afterElseExpr;

            // For else-if chains, recursively handle the nested if-block with comments
            if (isElseIf)
            {
                var (nestedIfFormatted, nestedAfterElse, nestedComments) = VisitIfBlockWithComments(
                    (Expression.IfBlock)expr.ElseBlock.Value, expr.ElseBlock.Range, elseContext, updatedComments);
                elseFormatted = nestedIfFormatted;
                afterElseExpr = nestedAfterElse;
                updatedComments = nestedComments;
            }
            else
            {
                (elseFormatted, afterElseExpr) = Visit(expr.ElseBlock.Value, elseContext);
            }

            var formattedIf = new Expression.IfBlock(
                Condition: condFormatted,
                ThenBlock: thenFormatted,
                ElseBlock: elseFormatted
            );

            var finalContext = isElseIf ? afterElseExpr : afterElseExpr.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Expression>(range, formattedIf), finalContext, updatedComments);
        }

        public override (Node<Expression>, FormattingContext) VisitCaseExpression(Expression.CaseExpression expr, FormattingContext context)
        {
            // "case "
            var afterCase = context.Advance(Keywords.Case.Length);
            var (scrutinee, afterScrutinee) = Visit(expr.CaseBlock.Expression.Value, afterCase);

            // " of"
            var afterOf = afterScrutinee.Advance(Keywords.Of.Length);

            // Format cases
            var caseContext = afterOf.NextRow().Indent().SetIndentColumn();
            var formattedCases = new List<Case>();

            for (var i = 0; i < expr.CaseBlock.Cases.Count; i++)
            {
                var caseItem = expr.CaseBlock.Cases[i];

                // Add blank line before cases after the first
                if (i > 0)
                {
                    // The blank line comes before the pattern
                    // Move to new row and set column to case indent level
                    caseContext = caseContext.NextRow().SetIndentColumn();
                }

                // Pattern - create new range for the formatted pattern
                var patternStartContext = caseContext;
                var patternText = FormatPatternText(caseItem.Pattern.Value);
                var afterPattern = caseContext.Advance(patternText.Length);

                // " ->"
                var afterArrow = afterPattern.Advance(3);
                var exprContext = afterArrow.NextRow().Indent().SetIndentColumn();

                var (formattedExpr, afterExpr) = Visit(caseItem.Expression.Value, exprContext);

                // Create pattern node with proper range
                var patternRange = new Range(patternStartContext.ToLocation(), afterPattern.ToLocation());
                var formattedPattern = new Node<Pattern>(patternRange, caseItem.Pattern.Value);

                formattedCases.Add(new Case(
                    Pattern: formattedPattern,
                    Expression: formattedExpr
                ));

                // Move to next line after expression and reset to case indent level
                // Only add NextRow if this is not the last case
                caseContext = afterExpr.Dedent();
                if (i < expr.CaseBlock.Cases.Count - 1)
                {
                    caseContext = caseContext.NextRow().SetIndentColumn();
                }
            }

            var formattedCaseBlock = new CaseBlock(
                Expression: scrutinee,
                Cases: formattedCases
            );

            var finalContext = caseContext.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Expression>(range, new Expression.CaseExpression(formattedCaseBlock)), finalContext);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordUpdateExpression(Expression.RecordUpdateExpression expr, FormattingContext context)
        {
            return FormatRecordUpdate(expr, context, IsRecordUpdateMultiLine(expr));
        }

        /// <summary>
        /// Check if a record update expression should be formatted as multiline.
        /// A record update is multiline if any field is on a different row than the record name.
        /// </summary>
        private static bool IsRecordUpdateMultiLine(Expression.RecordUpdateExpression expr)
        {
            if (expr.Fields.Count is 0)
                return false;

            var recordNameRow = expr.RecordName.Range.Start.Row;
            return expr.Fields.Any(field => field.Value.fieldName.Range.Start.Row != recordNameRow);
        }

        private (Node<Expression>, FormattingContext) FormatRecordUpdate(
            Expression.RecordUpdateExpression expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "{ recordName | field1 = value1, field2 = value2 }"
                var afterOpenBrace = context.Advance(2); // "{ "

                // Record name
                var recordName = expr.RecordName.Value;
                var afterRecordName = afterOpenBrace.Advance(recordName.Length);

                // " | "
                var afterPipe = afterRecordName.Advance(3);

                var currentContext = afterPipe;
                var formattedFields = new List<Node<(Node<string>, Node<Expression>)>>();

                for (var i = 0; i < expr.Fields.Count; i++)
                {
                    var (fieldName, valueExpr) = expr.Fields[i].Value;

                    // Field name
                    var afterFieldName = currentContext.Advance(fieldName.Value.Length);

                    // " = "
                    var afterEquals = afterFieldName.Advance(3);

                    // Format value expression
                    var (formattedValue, afterValue) = Visit(valueExpr.Value, afterEquals);

                    // Create formatted field name node
                    var fieldNameRange = new Range(currentContext.ToLocation(), afterFieldName.ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, fieldName.Value);

                    // Create formatted field node
                    var fieldRange = new Range(currentContext.ToLocation(), afterValue.ToLocation());
                    formattedFields.Add(new Node<(Node<string>, Node<Expression>)>(fieldRange, (formattedFieldName, formattedValue)));

                    currentContext = i < expr.Fields.Count - 1
                        ? afterValue.Advance(2) // ", "
                        : afterValue.Advance(2); // " }"
                }

                // Create record name node
                var recordNameRange = new Range(afterOpenBrace.ToLocation(), afterRecordName.ToLocation());
                var formattedRecordName = new Node<string>(recordNameRange, recordName);

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                var formattedExpr = new Expression.RecordUpdateExpression(formattedRecordName, formattedFields);
                return (new Node<Expression>(range, formattedExpr), currentContext);
            }
            else
            {
                // Multi-line: record name on same line, pipe and fields indented on new lines
                var afterOpenBrace = context.Advance(2); // "{ "

                // Record name on same line as opening brace
                var recordName = expr.RecordName.Value;
                var afterRecordName = afterOpenBrace.Advance(recordName.Length);

                // Move to next line and indent for pipe
                var pipeLineContext = afterRecordName.NextRow().Indent().SetIndentColumn();

                // "| "
                var afterPipe = pipeLineContext.Advance(2);

                var formattedFields = new List<Node<(Node<string>, Node<Expression>)>>();

                // First field on same line as pipe
                var (firstFieldName, firstValueExpr) = expr.Fields[0].Value;
                var afterFirstFieldName = afterPipe.Advance(firstFieldName.Value.Length);
                var afterFirstEquals = afterFirstFieldName.Advance(3); // " = "
                var (firstFormattedValue, afterFirstValue) = Visit(firstValueExpr.Value, afterFirstEquals);

                var firstFieldNameRange = new Range(afterPipe.ToLocation(), afterFirstFieldName.ToLocation());
                var firstFormattedFieldName = new Node<string>(firstFieldNameRange, firstFieldName.Value);
                var firstFieldRange = new Range(afterPipe.ToLocation(), afterFirstValue.ToLocation());

                formattedFields.Add(
                    new Node<(Node<string>, Node<Expression>)>(firstFieldRange, (firstFormattedFieldName, firstFormattedValue)));

                // Subsequent fields on new lines with ", " prefix
                var fieldContext = afterFirstValue.NextRow().SetIndentColumn();

                for (var i = 1; i < expr.Fields.Count; i++)
                {
                    // ", "
                    var afterComma = fieldContext.Advance(2);

                    var (fieldName, valueExpr) = expr.Fields[i].Value;
                    var afterFieldName = afterComma.Advance(fieldName.Value.Length);
                    var afterEquals = afterFieldName.Advance(3); // " = "
                    var (formattedValue, afterValue) = Visit(valueExpr.Value, afterEquals);

                    var fieldNameRange = new Range(afterComma.ToLocation(), afterFieldName.ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, fieldName.Value);
                    var fieldRange = new Range(afterComma.ToLocation(), afterValue.ToLocation());
                    formattedFields.Add(new Node<(Node<string>, Node<Expression>)>(fieldRange, (formattedFieldName, formattedValue)));

                    // Move to next line
                    fieldContext = afterValue.NextRow().SetIndentColumn();
                }

                // Dedent before placing closing brace
                var closingBraceContext = fieldContext.Dedent().SetIndentColumn();

                // "}"
                var afterCloseBrace = closingBraceContext.Advance(1);

                var finalContext = afterCloseBrace.Dedent();

                // Create record name node
                var recordNameRange = new Range(afterOpenBrace.ToLocation(), afterRecordName.ToLocation());
                var formattedRecordName = new Node<string>(recordNameRange, recordName);

                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                var formattedExpr = new Expression.RecordUpdateExpression(formattedRecordName, formattedFields);
                return (new Node<Expression>(range, formattedExpr), finalContext);
            }
        }

        /// <summary>
        /// Format a let expression with proper spacing between declarations.
        /// Ensures exactly one blank line between let declarations.
        /// </summary>
        public override (Node<Expression>, FormattingContext) VisitLetExpression(Expression.LetExpression expr, FormattingContext context)
        {
            // "let"
            var afterLet = context.Advance(Keywords.Let.Length);
            var afterLetNewline = afterLet.NextRow();

            // Indent for let declarations
            var declContext = afterLetNewline.Indent().SetIndentColumn();

            var formattedDeclarations = new List<Node<Expression.LetDeclaration>>();
            var currentContext = declContext;

            for (var i = 0; i < expr.Value.Declarations.Count; i++)
            {
                var decl = expr.Value.Declarations[i];
                var (formattedDecl, afterDecl) = FormatLetDeclaration(decl.Value, currentContext);

                // Create node for the declaration
                var declRange = new Range(currentContext.ToLocation(), afterDecl.ToLocation());
                formattedDeclarations.Add(new Node<Expression.LetDeclaration>(declRange, formattedDecl));

                // Move to next line and add blank line between declarations
                if (i < expr.Value.Declarations.Count - 1)
                {
                    // One blank line between declarations (2 newlines total)
                    currentContext = afterDecl.NextRow().NextRow().SetIndentColumn();
                }
                else
                {
                    // After last declaration, just move to next line for "in"
                    currentContext = afterDecl.NextRow();
                }
            }

            // Dedent for "in"
            var inContext = currentContext.Dedent().SetIndentColumn();

            // "in"
            var afterIn = inContext.Advance(Keywords.In.Length);
            var afterInNewline = afterIn.NextRow();

            // Format the expression after "in"
            var exprContext = afterInNewline.SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNodeLessMapping(expr.Value.Expression, exprContext);

            // Create the formatted let block
            var formattedLetBlock = new Expression.LetBlock(formattedDeclarations, formattedExpr);

            var range = new Range(context.ToLocation(), afterExpr.ToLocation());
            return (new Node<Expression>(range, new Expression.LetExpression(formattedLetBlock)), afterExpr);
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetDeclaration(Expression.LetDeclaration decl, FormattingContext context)
        {
            return decl switch
            {
                Expression.LetDeclaration.LetFunction letFunc =>
                    FormatLetFunction(letFunc, context),

                Expression.LetDeclaration.LetDestructuring letDestr =>
                    FormatLetDestructuring(letDestr, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for let declaration type '{decl.GetType().Name}' is not implemented.")
            };
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetFunction(
            Expression.LetDeclaration.LetFunction letFunc,
            FormattingContext context)
        {
            // Format like a regular function declaration but without the full signature handling
            var func = letFunc.Function;

            var currentContext = context;
            Node<Signature>? formattedSignature = null;

            // Format type signature if present
            if (func.Signature is { } signature)
            {
                // Function name for signature
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);

                // " : "
                var afterColon = afterSigName.Advance(3);

                var (formattedTypeAnnotation, contextAfterTypeAnnotation) =
                    FormatTypeAnnotation(signature.Value.TypeAnnotation, afterColon);

                // Create new Signature with updated TypeAnnotation range
                var formattedSig = new Signature(
                    Name: signature.Value.Name,
                    TypeAnnotation: formattedTypeAnnotation);

                var sigRange = new Range(context.ToLocation(), contextAfterTypeAnnotation.ToLocation());
                formattedSignature = new Node<Signature>(sigRange, formattedSig);

                // Move to next line for the function declaration
                currentContext = contextAfterTypeAnnotation.NextRow().SetIndentColumn();
            }

            // Remember where the declaration starts (for the Range)
            var declStartContext = currentContext;

            // Function name
            var funcName = func.Declaration.Value.Name.Value;
            var afterName = currentContext.Advance(funcName.Length);

            // Format arguments - estimate total length from original range
            var argContext = afterName;
            if (func.Declaration.Value.Arguments.Count > 0)
            {
                // Estimate argument section length from first to last argument's range
                var firstArg = func.Declaration.Value.Arguments[0];
                var lastArg = func.Declaration.Value.Arguments[func.Declaration.Value.Arguments.Count - 1];
                var argSectionLength = lastArg.Range.End.Column - firstArg.Range.Start.Column + 1;
                argContext = argContext.Advance(argSectionLength);
            }

            // " ="
            var afterEquals = argContext.Advance(2);
            var afterEqualsNewline = afterEquals.NextRow();

            // Format the expression (indented)
            var exprContext = afterEqualsNewline.Indent().SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNodeLessMapping(func.Declaration.Value.Expression, exprContext);

            // Create formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: new Node<string>(func.Declaration.Value.Name.Range, funcName),
                Arguments: func.Declaration.Value.Arguments,
                Expression: formattedExpr);

            var formattedDeclRange = new Range(declStartContext.ToLocation(), afterExpr.ToLocation());
            var formattedDeclNode = new Node<FunctionImplementation>(formattedDeclRange, formattedImpl);

            var formattedFunc = new FunctionStruct(
                func.Documentation,
                formattedSignature,
                formattedDeclNode);

            var finalContext = afterExpr.Dedent();

            return (new Expression.LetDeclaration.LetFunction(formattedFunc), finalContext);
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetDestructuring(Expression.LetDeclaration.LetDestructuring letDestr, FormattingContext context)
        {
            // For now, preserve the pattern and format the expression
            // TODO: Implement proper pattern formatting

            // Estimate pattern length (simplified)
            var patternText = "pattern"; // Placeholder
            var afterPattern = context.Advance(patternText.Length);

            // " ="
            var afterEquals = afterPattern.Advance(2);
            var afterEqualsNewline = afterEquals.NextRow();

            // Format the expression (indented)
            var exprContext = afterEqualsNewline.Indent().SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNodeLessMapping(letDestr.Expression, exprContext);

            var finalContext = afterExpr.Dedent();

            return (new Expression.LetDeclaration.LetDestructuring(letDestr.Pattern, formattedExpr), finalContext);
        }

        public override (Node<Expression>, FormattingContext) VisitUnitExpr(
            Expression.UnitExpr expr,
            FormattingContext context)
        {
            // Unit expression is "()"
            var startLocation = context.ToLocation();
            var afterUnit = context.Advance(2); // "()"

            var exprRange = new Range(
                Start: startLocation,
                End: afterUnit.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterUnit);
        }

        public override (Node<Expression>, FormattingContext) VisitCharLiteral(
            Expression.CharLiteral expr,
            FormattingContext context)
        {
            // Char literals are like 't' or '\n' - we need to render them properly
            var startLocation = context.ToLocation();
            var charText = Rendering.RenderCharLiteral(expr.Value);
            var afterChar = context.Advance(charText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterChar.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterChar);
        }

        public override (Node<Expression>, FormattingContext) VisitHex(
            Expression.Hex expr,
            FormattingContext context)
        {
            // Hex literals like 0x81 or 0x00012345
            var startLocation = context.ToLocation();
            var hexText = Rendering.RenderHexPattern(expr.Value);
            var afterHex = context.Advance(hexText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterHex.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterHex);
        }

        public override (Node<Expression>, FormattingContext) VisitFloatable(
            Expression.Floatable expr,
            FormattingContext context)
        {
            // Float literals like 1.0 or 3.14
            // Use FormatFloatForElm to avoid scientific notation (e.g., 1E-16)
            var startLocation = context.ToLocation();
            var floatText = Rendering.FormatFloatForElm(expr.Value);
            var afterFloat = context.Advance(floatText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterFloat.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterFloat);
        }

        public override (Node<Expression>, FormattingContext) VisitTupledExpression(
            Expression.TupledExpression expr,
            FormattingContext context)
        {
            // When called without a Range, synthesize one based on element positions
            if (expr.Elements.Count is 0)
            {
                // Empty tuple (shouldn't happen in practice)
                var emptyRange = new Range(new Location(1, 1), new Location(1, 3));
                return VisitTupledExpressionWithRange(expr, emptyRange, context);
            }

            // Synthesize a Range that encompasses all elements
            var startLoc = expr.Elements[0].Range.Start;
            var endLoc = expr.Elements[expr.Elements.Count - 1].Range.End;
            var synthesizedRange = new Range(startLoc, endLoc);

            return VisitTupledExpressionWithRange(expr, synthesizedRange, context);
        }

        /// <summary>
        /// Format a tuple expression with access to its original Range for accurate multiline detection.
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitTupledExpressionWithRange(
            Expression.TupledExpression expr,
            Range originalRange,
            FormattingContext context)
        {
            if (expr.Elements.Count is 0)
            {
                // Empty tuple: "()"
                var afterTuple = context.Advance(2);
                var emptyTupleRange = new Range(context.ToLocation(), afterTuple.ToLocation());
                return (new Node<Expression>(emptyTupleRange, expr), afterTuple);
            }

            // Detect if tuple is multiline by checking if elements span different rows
            // or if the closing paren is on a different row
            bool isMultiLine;
            if (expr.Elements.Count >= 2)
            {
                isMultiLine = NodesSpanMultipleRows(expr.Elements);
            }
            else
            {
                // Single element: check if parent tuple Range spans multiple rows
                var element = expr.Elements[0];
                var elementEndRow = element.Range.End.Row;
                var elementStartRow = element.Range.Start.Row;
                var tupleEndRow = originalRange.End.Row;
                isMultiLine = tupleEndRow > elementEndRow || elementEndRow > elementStartRow;
            }

            return FormatTuple(expr, context, isMultiLine);
        }

        private (Node<Expression>, FormattingContext) FormatTuple(
            Expression.TupledExpression expr,
            FormattingContext context,
            bool isMultiLine)
        {
            var startLocation = context.ToLocation();
            var openingParenColumn = context.CurrentColumn;

            // "("
            var afterOpen = context.Advance(1);

            var formattedElements = new List<Node<Expression>>();

            if (!isMultiLine)
            {
                // Single-line tuple: ( elem1, elem2, elem3 )
                var currentContext = afterOpen.Advance(1); // space after "("

                for (var i = 0; i < expr.Elements.Count; i++)
                {
                    var (formattedElem, afterElem) = VisitExpressionNodeLessMapping(expr.Elements[i], currentContext);
                    formattedElements.Add(formattedElem);

                    if (i < expr.Elements.Count - 1)
                    {
                        currentContext = afterElem.Advance(2); // ", "
                    }
                    else
                    {
                        currentContext = afterElem.Advance(2); // " )"
                    }
                }

                var exprRange = new Range(startLocation, currentContext.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.TupledExpression(formattedElements)), currentContext);
            }
            else
            {
                // Multi-line tuple:
                // ( elem1
                // , elem2
                // , elem3
                // )
                var currentContext = afterOpen.Advance(1); // space after "("

                // First element on same line as opening paren
                var (firstElem, afterFirst) = VisitExpressionNodeLessMapping(expr.Elements[0], currentContext);
                formattedElements.Add(firstElem);

                // Subsequent elements on new lines with ", " prefix at opening paren column
                var elemContext = afterFirst;

                for (var i = 1; i < expr.Elements.Count; i++)
                {
                    // Move to new line, position comma at opening paren column
                    elemContext = elemContext.NextRow();
                    elemContext = elemContext with { CurrentColumn = openingParenColumn };
                    elemContext = elemContext.Advance(2); // ", "

                    // Add half indent for subsequent tuple elements to support nested multiline expressions
                    elemContext = elemContext.WithHalfIndent(true);

                    var (formattedElem, afterElem) = VisitExpressionNodeLessMapping(expr.Elements[i], elemContext);
                    formattedElements.Add(formattedElem);
                    elemContext = afterElem;
                }

                // Closing paren on new line at opening paren column
                var closingContext = elemContext.NextRow();
                closingContext = closingContext with { CurrentColumn = openingParenColumn };
                closingContext = closingContext.Advance(1); // ")"

                var exprRange = new Range(startLocation, closingContext.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.TupledExpression(formattedElements)), closingContext);
            }
        }

        /// <summary>
        /// Format a record expression with access to its original Range for accurate multiline detection.
        /// The original Range is essential for single-field records to detect if the closing brace
        /// is on a different row than the field (indicating multiline format).
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitRecordExprWithRange(
            Expression.RecordExpr expr,
            Range originalRange,
            FormattingContext context)
        {
            if (expr.Fields.Count is 0)
            {
                // Empty record: "{}"
                var afterRecord = context.Advance(2);
                var emptyRecordRange = new Range(context.ToLocation(), afterRecord.ToLocation());
                return (new Node<Expression>(emptyRecordRange, expr), afterRecord);
            }

            // Detect if this specific record is multiline by checking:
            // 1. For multiple fields: if they span different rows
            // 2. For single field: if the parent record Range (including closing brace) spans multiple rows
            //    OR if the field itself spans multiple rows (indicating multiline formatting)
            bool isMultiLine;
            if (expr.Fields.Count >= 2)
            {
                // Multiple fields: use helper to check if they span different rows
                isMultiLine = NodesSpanMultipleRows(expr.Fields);
            }
            else
            {
                // Single field: check if the parent record Range spans multiple rows
                // OR if the field itself spans multiple rows
                // This detects cases like: { a = []\n    } where the field is single-line
                // but the closing brace is on a new line, or { a = 13\n    } where the
                // field Range has been marked as multiline by a preprocessor
                var field = expr.Fields[0];
                var fieldEndRow = field.Range.End.Row;
                var fieldStartRow = field.Range.Start.Row;
                var recordEndRow = originalRange.End.Row;
                isMultiLine = recordEndRow > fieldEndRow || fieldEndRow > fieldStartRow;
            }

            return FormatRecord(expr, context, isMultiLine);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordExpr(
            Expression.RecordExpr expr,
            FormattingContext context)
        {
            // This method is called by the base visitor pattern when we don't have direct access
            // to the original Node's Range (only the Expression value). This occurs when called
            // from external code or the base Visit() dispatcher.
            // We synthesize a Range based on field positions for multiline detection.

            if (expr.Fields.Count is 0)
            {
                // For empty records, use a minimal placeholder Range (doesn't affect multiline detection)
                var emptyRange = new Range(new Location(1, 1), new Location(1, 3));  // "{}" is 2 chars
                return VisitRecordExprWithRange(expr, emptyRange, context);
            }

            // Synthesize a Range that encompasses all fields
            // This provides fallback multiline detection when the original Range isn't available
            var startLoc = expr.Fields[0].Range.Start;
            var endLoc = expr.Fields[expr.Fields.Count - 1].Range.End;
            var synthesizedRange = new Range(startLoc, endLoc);

            return VisitRecordExprWithRange(expr, synthesizedRange, context);
        }

        private (Node<Expression>, FormattingContext) FormatRecord(
            Expression.RecordExpr expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "{ field = value, field2 = value2 }"
                var afterOpen = context.Advance(2); // "{ "
                var currentContext = afterOpen;
                var formattedFields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

                for (var i = 0; i < expr.Fields.Count; i++)
                {
                    var field = expr.Fields[i].Value;

                    // field name
                    var fieldNameLength = field.fieldName.Value.Length;
                    var fieldNameRange = new Range(currentContext.ToLocation(), currentContext.Advance(fieldNameLength).ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, field.fieldName.Value);
                    var afterFieldName = currentContext.Advance(fieldNameLength);

                    // " = "
                    var afterEquals = afterFieldName.Advance(3);

                    // value
                    var (formattedValue, afterValue) = VisitExpressionNodeLessMapping(field.valueExpr, afterEquals);

                    var fieldRange = new Range(currentContext.ToLocation(), afterValue.ToLocation());
                    var formattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                        fieldRange,
                        (formattedFieldName, formattedValue));
                    formattedFields.Add(formattedField);

                    currentContext = i < expr.Fields.Count - 1
                        ? afterValue.Advance(2) // ", "
                        : afterValue.Advance(2); // " }"
                }

                var exprRange = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.RecordExpr(formattedFields)), currentContext);
            }
            else
            {
                // Multi-line: fields each on their own line
                var openingBraceColumn = context.CurrentColumn;
                var afterOpen = context.Advance(2); // "{ "
                var formattedFields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

                // First field on same line as "{"
                var firstField = expr.Fields[0].Value;
                var fieldNameLength = firstField.fieldName.Value.Length;
                var fieldNameRange = new Range(afterOpen.ToLocation(), afterOpen.Advance(fieldNameLength).ToLocation());
                var formattedFieldName = new Node<string>(fieldNameRange, firstField.fieldName.Value);
                var afterFieldName = afterOpen.Advance(fieldNameLength);
                var afterEquals = afterFieldName.Advance(3); // " = "

                // Check if the first field value is multiline
                var isFirstValueMultiline = firstField.valueExpr.Range.End.Row > firstField.valueExpr.Range.Start.Row;

                FormattingContext firstValueContext;
                if (isFirstValueMultiline)
                {
                    // Put multiline value on a new line, indented one level deeper
                    // Use indent-based calculation for proper alignment across nesting levels
                    var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    firstValueContext = new FormattingContext(
                        afterEquals.CurrentRow + 1,
                        1,
                        baseIndentLevel + 1).SetIndentColumn();
                }
                else
                {
                    // Single-line value stays on the same line
                    firstValueContext = afterEquals;
                }

                var (formattedValue, afterFirstValue) = VisitExpressionNodeLessMapping(firstField.valueExpr, firstValueContext);

                // Dedent if we indented for multiline value
                if (isFirstValueMultiline)
                {
                    afterFirstValue = afterFirstValue.Dedent();
                }

                var firstFieldRange = new Range(afterOpen.ToLocation(), afterFirstValue.ToLocation());
                var firstFormattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    firstFieldRange,
                    (formattedFieldName, formattedValue));
                formattedFields.Add(firstFormattedField);

                // Subsequent fields on new lines with ", " prefix
                var nextRow = afterFirstValue.CurrentRow + 1;

                for (var i = 1; i < expr.Fields.Count; i++)
                {
                    var field = expr.Fields[i].Value;

                    // Create context at opening brace column for comma
                    var commaContext = new FormattingContext(
                        nextRow,
                        openingBraceColumn,
                        context.IndentLevel);

                    // ", " at the start of the line
                    var afterComma = commaContext.Advance(2);

                    // field name
                    fieldNameLength = field.fieldName.Value.Length;
                    fieldNameRange = new Range(afterComma.ToLocation(), afterComma.Advance(fieldNameLength).ToLocation());
                    formattedFieldName = new Node<string>(fieldNameRange, field.fieldName.Value);
                    afterFieldName = afterComma.Advance(fieldNameLength);

                    // " = "
                    afterEquals = afterFieldName.Advance(3);

                    // Check if the field value is multiline (spans multiple rows in the original source)
                    var isValueMultiline = field.valueExpr.Range.End.Row > field.valueExpr.Range.Start.Row;

                    FormattingContext valueContext;
                    if (isValueMultiline)
                    {
                        // Put multiline value on a new line, indented one level deeper
                        // Use indent-based calculation for proper alignment across nesting levels
                        var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                        valueContext = new FormattingContext(
                            afterEquals.CurrentRow + 1,
                            1,
                            baseIndentLevel + 1).SetIndentColumn();
                    }
                    else
                    {
                        // Single-line value stays on the same line
                        valueContext = afterEquals;
                    }

                    // value
                    var (formattedFieldValue, afterValue) = VisitExpressionNodeLessMapping(field.valueExpr, valueContext);

                    // Dedent if we indented for multiline value
                    if (isValueMultiline)
                    {
                        afterValue = afterValue.Dedent();
                    }

                    var fieldRange = new Range(afterComma.ToLocation(), afterValue.ToLocation());
                    var formattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                        fieldRange,
                        (formattedFieldName, formattedFieldValue));
                    formattedFields.Add(formattedField);

                    // Move to next line
                    nextRow = afterValue.CurrentRow + 1;
                }

                // Closing brace "}" should be at opening brace column
                var closingBraceContext = new FormattingContext(
                    nextRow,
                    openingBraceColumn,
                    context.IndentLevel);
                var afterClose = closingBraceContext.Advance(1);

                var exprRange = new Range(context.ToLocation(), afterClose.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.RecordExpr(formattedFields)), afterClose);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitRecordAccess(
            Expression.RecordAccess expr,
            FormattingContext context)
        {
            // Record access like record.field
            var startLocation = context.ToLocation();

            // Format the record expression
            var (formattedRecord, afterRecord) = VisitExpressionNodeLessMapping(expr.Record, context);

            // "."
            var afterDot = afterRecord.Advance(1);

            // field name
            var fieldLength = expr.FieldName.Value.Length;
            var afterField = afterDot.Advance(fieldLength);

            var exprRange = new Range(startLocation, afterField.ToLocation());
            var formattedFieldName = new Node<string>(
                new Range(afterDot.ToLocation(), afterField.ToLocation()),
                expr.FieldName.Value);
            return (new Node<Expression>(exprRange, new Expression.RecordAccess(formattedRecord, formattedFieldName)), afterField);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordAccessFunction(
            Expression.RecordAccessFunction expr,
            FormattingContext context)
        {
            // Record access function like .field
            var startLocation = context.ToLocation();

            // ".field"
            var accessLength = 1 + expr.FunctionName.Length; // "." + field name
            var afterAccess = context.Advance(accessLength);

            var exprRange = new Range(startLocation, afterAccess.ToLocation());
            return (new Node<Expression>(exprRange, expr), afterAccess);
        }

        public override (Node<Expression>, FormattingContext) VisitLambdaExpression(
            Expression.LambdaExpression expr,
            FormattingContext context)
        {
            // Lambda like \x -> x + 1
            var startLocation = context.ToLocation();

            // "\"
            var currentContext = context.Advance(1);

            // Format patterns
            var formattedPatterns = new List<Node<Pattern>>();
            for (var i = 0; i < expr.Lambda.Arguments.Count; i++)
            {
                var pattern = expr.Lambda.Arguments[i];
                var patternText = FormatPatternText(pattern.Value);
                var afterPattern = currentContext.Advance(patternText.Length);
                var patternRange = new Range(currentContext.ToLocation(), afterPattern.ToLocation());
                formattedPatterns.Add(new Node<Pattern>(patternRange, pattern.Value));

                // Space after pattern (except before arrow)
                if (i < expr.Lambda.Arguments.Count - 1)
                {
                    currentContext = afterPattern.Advance(1);
                }
                else
                {
                    currentContext = afterPattern;
                }
            }

            // Check if expression body is on a different row than the arrow would be
            // This happens with parenthesized lambdas with multiline bodies
            var originalExprStartRow = expr.Lambda.Expression.Range.Start.Row;
            var arrowContext = currentContext.Advance(4); // After " -> "

            // If the original expression started on a different row, add newline after arrow
            FormattingContext exprStartContext;
            var isMultiLine = originalExprStartRow > currentContext.CurrentRow;
            if (isMultiLine)
            {
                // Multiline: move to next row and indent
                exprStartContext = currentContext.Advance(4).NextRow().Indent().SetIndentColumn();
            }
            else
            {
                // Single line: just advance past arrow
                exprStartContext = arrowContext;
            }

            // Expression
            var (formattedExpr, afterExpr) = VisitExpressionNodeLessMapping(expr.Lambda.Expression, exprStartContext);

            // If we indented for multiline, we need to dedent after
            if (isMultiLine)
            {
                afterExpr = afterExpr.Dedent();
            }

            var exprRange = new Range(startLocation, afterExpr.ToLocation());
            var formattedLambda = new LambdaStruct(formattedPatterns, formattedExpr);
            return (new Node<Expression>(exprRange, new Expression.LambdaExpression(formattedLambda)), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitPrefixOperator(
            Expression.PrefixOperator expr,
            FormattingContext context)
        {
            // Prefix operator like (+) or (-)
            var startLocation = context.ToLocation();

            // "(op)"
            var opLength = 2 + expr.Operator.Length; // "(" + operator + ")"
            var afterOp = context.Advance(opLength);

            var exprRange = new Range(startLocation, afterOp.ToLocation());
            return (new Node<Expression>(exprRange, expr), afterOp);
        }

        public override (Node<Expression>, FormattingContext) VisitNegation(
            Expression.Negation expr,
            FormattingContext context)
        {
            // Negation is represented as "-" followed by the expression
            var startLocation = context.ToLocation();

            // Move past the "-" operator (1 character)
            var afterMinus = context.Advance(1);

            // Format the inner expression
            var (formattedInner, afterInner) = VisitExpressionNodeLessMapping(expr.Expression, afterMinus);

            // Create a new Negation with the formatted inner expression
            var formattedExpr = new Expression.Negation(formattedInner);

            // Create range from start (before "-") to end
            var exprRange = new Range(
                Start: startLocation,
                End: formattedInner.Range.End);

            return (new Node<Expression>(exprRange, formattedExpr), afterInner);
        }

        public override (Node<Expression>, FormattingContext) VisitParenthesizedExpression(
            Expression.ParenthesizedExpression expr,
            FormattingContext context)
        {
            // Check if the parenthesized expression is multiline based on the original Range
            var isMultiLine = expr.Expression.Range.Start.Row != expr.Expression.Range.End.Row;

            if (isMultiLine)
            {
                // Multiline format: parentheses on their own positions, content indented
                // Example:
                // (\b ->
                //     a + b
                // )
                var startLocation = context.ToLocation();

                // Opening paren on current line
                var afterOpenParen = context.Advance(1);

                // Format inner expression on same line as opening paren
                var (formattedInner, afterInner) = VisitExpressionNodeLessMapping(expr.Expression, afterOpenParen);

                // Closing paren needs to be on a new line, at the same indentation as the opening paren
                var closingParenContext = afterInner.NextRow().SetColumn(context.CurrentColumn);
                var afterClosingParen = closingParenContext.Advance(1);

                var formattedExpr = new Expression.ParenthesizedExpression(formattedInner);
                var exprRange = new Range(
                    Start: startLocation,
                    End: afterClosingParen.ToLocation());

                return (new Node<Expression>(exprRange, formattedExpr), afterClosingParen);
            }
            else
            {
                // Single-line format: (expr)
                var startLocation = context.ToLocation();

                // Move past the opening paren (1 character)
                var innerContext = context.Advance(1);

                // Format the inner expression
                var (formattedInner, afterInner) = VisitExpressionNodeLessMapping(expr.Expression, innerContext);

                // The closing paren is one column after the inner expression
                var endContext = afterInner.Advance(1);

                // Create a new ParenthesizedExpression with the formatted inner expression
                var formattedExpr = new Expression.ParenthesizedExpression(formattedInner);

                // Create range from start (before opening paren) to end (after closing paren)
                var exprRange = new Range(
                    Start: startLocation,
                    End: endContext.ToLocation());

                return (new Node<Expression>(exprRange, formattedExpr), endContext);
            }
        }

        // For unsupported expressions, preserve as-is
        protected override (Node<Expression>, FormattingContext) VisitUnknown(Expression expr, FormattingContext context)
        {
            // Ensure variants are encoded explicitly: Fail if an override is missing.

            throw new System.NotImplementedException(
                $"Formatting for expression type '{expr.GetType().Name}' is not implemented.");
        }

        #endregion
    }

    #endregion
}
