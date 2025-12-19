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
        var visitor = new Avh4FormatVisitor(file.Comments);
        var context = new FormattingContext(CurrentRow: 1, CurrentColumn: 1, IndentSpaces: 0);

        var (formatted, formattedComments) = visitor.FormatFile(file, context);

        return formatted with { Comments = formattedComments };
    }

    #region Constants and Keywords

    /// <summary>
    /// Indentation constants for Elm formatting.
    /// </summary>
    private static class Indentation
    {
        /// <summary>Standard full indent (4 spaces) for nested content.</summary>
        public const int Full = 4;

        /// <summary>Half indent (2 spaces) for list/tuple elements.</summary>
        public const int Half = 2;
    }

    /// <summary>
    /// Named constants for Elm keywords and syntax to replace magic numbers.
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
        public new const string Equals = " = ";
        public const string Arrow = "-> ";
        public const string Minus = "-";
        public const string Pipe = " | ";
    }

    #endregion

    #region Formatting Context

    /// <summary>
    /// Formatting context that tracks current position and indentation in spaces.
    /// </summary>
    private record FormattingContext(
        int CurrentRow,
        int CurrentColumn,
        int IndentSpaces,
        bool InElementContext = false)
    {
        public Location CurrentLocation() =>
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
                IndentSpaces = IndentSpaces + Indentation.Full,
                InElementContext = false
            };

        public FormattingContext IndentHalf() =>
            this with
            {
                IndentSpaces = IndentSpaces + Indentation.Half
            };

        public FormattingContext ReturnToIndent(FormattingContext prevContext) =>
            this with
            {
                IndentSpaces = prevContext.IndentSpaces,
                InElementContext = prevContext.InElementContext,
            };

        public FormattingContext SetIndentColumn() =>
            this with
            {
                CurrentColumn = 1 + IndentSpaces
            };

        public FormattingContext WithInElementContext() =>
            this with
            {
                InElementContext = true
            };

        public int GetNextMultipleOfFourColumn()
        {
            var currentSpaces = CurrentColumn - 1;
            var nextMultipleSpaces = ((currentSpaces / Indentation.Full) + 1) * Indentation.Full;
            return nextMultipleSpaces + 1;
        }
    }

    /// <summary>
    /// Result of a formatting operation containing the formatted node, updated context,
    /// location mappings from original to formatted positions, and accumulated comments.
    /// This type is intended to replace tuple returns in formatting methods to eliminate mutation.
    /// </summary>
    private record FormattingResult<T>(
        T FormattedNode,
        FormattingContext Context,
        ImmutableDictionary<Location, Location> LocationMapping,
        ImmutableList<Stil4mElmSyntax7.Node<string>> Comments)
    {
        public static FormattingResult<T> Create(
            T formattedNode,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments) =>
            new(formattedNode, context, [], comments);

        public static FormattingResult<T> Create(
            T formattedNode,
            FormattingContext context) =>
            new(formattedNode, context, [],
                []);

        public FormattingResult<T> WithComment(Stil4mElmSyntax7.Node<string> comment) =>
            this with { Comments = Comments.Add(comment) };

        public FormattingResult<T> WithComments(IEnumerable<Stil4mElmSyntax7.Node<string>> newComments) =>
            this with { Comments = Comments.AddRange(newComments) };

        public FormattingResult<T> WithLocationMapping(Location original, Location formatted) =>
            this with { LocationMapping = LocationMapping.Add(original, formatted) };

        public FormattingResult<TNew> Map<TNew>(System.Func<T, TNew> mapper) =>
            new(mapper(FormattedNode), Context, LocationMapping, Comments);

        public FormattingResult<T> MergeFrom<TOther>(FormattingResult<TOther> other) =>
            this with
            {
                Context = other.Context,
                LocationMapping = LocationMapping.AddRange(other.LocationMapping),
                Comments = Comments.AddRange(other.Comments)
            };
    }

    #endregion

    #region Static Helpers

    /// <summary>
    /// Calculate the end location of a comment given its start location and value.
    /// </summary>
    private static Location CalculateCommentEndLocation(Location startLocation, string commentValue)
    {
        var lines = commentValue.Split('\n');

        if (lines.Length is 1)
        {
            return new Location(startLocation.Row, startLocation.Column + commentValue.Length);
        }
        else
        {
            var endRow = startLocation.Row + lines.Length - 1;
            var lastLineLength = lines[lines.Length - 1].Length;
            return new Location(endRow, lastLineLength + 1);
        }
    }

    /// <summary>
    /// Creates a new FormattingContext positioned after a comment ends.
    /// </summary>
    private static FormattingContext CreateContextAfterComment(
        FormattingContext currentContext,
        Location commentEnd)
    {
        return new FormattingContext(
            commentEnd.Row + 1,
            1,
            currentContext.IndentSpaces).SetIndentColumn();
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
    /// Check if two ranges are on different rows.
    /// </summary>
    private static bool RangesOnDifferentRows(Range first, Range second) =>
        first.End.Row < second.Start.Row;

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
    private class Avh4FormatVisitor(IReadOnlyList<Stil4mElmSyntax7.Node<string>> originalComments)
    {
        #region File Formatting

        public (File, IReadOnlyList<Stil4mElmSyntax7.Node<string>>) FormatFile(
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
                var startContext = contextAfterModule.NextRow().NextRow();
                (currentComments, contextBeforeImports) = FormatCommentsAtContext(
                    commentsAfterModuleBeforeImports, startContext, commentsAfterModule);
                contextBeforeImports = contextBeforeImports.NextRow();
            }
            else
            {
                contextBeforeImports = contextAfterModule.NextRow().NextRow();
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

            var lastRowBeforeDecls =
                formattedImports.Any()
                ? formattedImports.Last().Range.End.Row
                : formattedModule.Range.End.Row;

            var firstDeclaration = file.Declarations.FirstOrDefault();

            var commentsBefore =
                firstDeclaration is not null
                ? GetCommentsBetweenRows(lastRowBeforeDecls, firstDeclaration.Range.Start.Row)
                : [];

            FormattingContext contextBeforeDecls;

            if (commentsBefore.Count is not 0)
            {
                var startContext = formattedImports.Any()
                    ? contextAfterImports.NextRow().NextRow().NextRow()
                    : contextAfterModule.NextRow().NextRow();

                (commentsBeforeDecls, contextBeforeDecls) = FormatCommentsAtContext(
                    commentsBefore, startContext, commentsAfterImports, addBlankLinesAfterNonDocComments: true);
            }
            else
            {
                contextBeforeDecls = formattedImports.Any()
                    ? contextAfterImports.NextRow().NextRow()
                    : contextAfterModule.NextRow().NextRow().NextRow();
            }

            // Format declarations
            var (formattedDeclarations, _, commentsAfterDecls) =
                FormatDeclarations(file.Declarations, contextBeforeDecls, commentsBeforeDecls);

            var formattedFile = new File(
                ModuleDefinition: formattedModule,
                Imports: formattedImports,
                Declarations: formattedDeclarations,
                Comments: []);

            return (formattedFile, commentsAfterDecls);
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
                var commentLocation = currentContext.CurrentLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                currentComments = currentComments.Add(formattedComment);
                currentContext = CreateContextAfterComment(currentContext, commentEnd);

                if (addBlankLinesAfterNonDocComments && !IsDocComment(comment.Value))
                {
                    currentContext = currentContext.NextRow().NextRow();
                }
            }

            return (currentComments, currentContext);
        }

        private IReadOnlyList<Stil4mElmSyntax7.Node<string>> GetCommentsBetweenRows(int afterRow, int beforeRow) =>
            [.. originalComments
                .Where(c => c.Range.Start.Row > afterRow && c.Range.End.Row < beforeRow)
                .OrderBy(c => c.Range.Start.Row)];

        #endregion

        #region Module Formatting

        private (Stil4mElmSyntax7.Node<Module>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatModuleDefinition(
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

        private (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatNormalModule(
            Module.NormalModule module,
            FormattingContext context)
        {
            // "module "
            var moduleTokenLoc = context.CurrentLocation();
            var afterModuleKeyword = context.Advance(Keywords.Module.Length);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1);

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

        private (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatPortModule(
            Module.PortModule module,
            FormattingContext context)
        {
            var portTokenLoc = context.CurrentLocation();
            var afterPort = context.Advance(5); // "port "
            var moduleTokenLoc = afterPort.CurrentLocation();
            var afterModuleKeyword = afterPort.Advance(7); // "module "

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1);

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

        private (Stil4mElmSyntax7.Node<Module>, FormattingContext) FormatEffectModule(
            Module.EffectModule module,
            FormattingContext context)
        {
            var effectTokenLoc = context.CurrentLocation();
            var afterEffect = context.Advance(7); // "effect "
            var moduleTokenLoc = afterEffect.CurrentLocation();
            var afterModuleKeyword = afterEffect.Advance(7); // "module "

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1);

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

        private (Stil4mElmSyntax7.Node<Exposing>, FormattingContext) FormatExposing(
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
                var afterExposingKeyword = context.Advance(Keywords.Exposing.Length);
                var parentContext = afterExposingKeyword.NextRow();
                var parenLineContext = parentContext.Indent().SetIndentColumn();

                var afterOpenParen = parenLineContext.Advance(Keywords.TupleOpen.Length);
                var formattedNodes = new List<Stil4mElmSyntax7.Node<TopLevelExpose>>();

                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], afterOpenParen);
                formattedNodes.Add(firstNode);

                var itemContext = afterFirst.NextRow().SetIndentColumn();

                for (var i = 1; i < explicitList.Nodes.Count; i++)
                {
                    var afterComma = itemContext.Advance(2);
                    var (formattedNode, nextContext) = FormatTopLevelExpose(explicitList.Nodes[i], afterComma);
                    formattedNodes.Add(formattedNode);
                    itemContext = nextContext.NextRow().SetIndentColumn();
                }

                var afterCloseParen = itemContext.Advance(1);
                var finalContext = afterCloseParen.ReturnToIndent(parentContext);
                var range = MakeRange(context.CurrentLocation(), finalContext.CurrentLocation());
                return (new Stil4mElmSyntax7.Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), finalContext);
            }
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
            var range = MakeRange(context.CurrentLocation(), afterExpose.CurrentLocation());

            return (new Stil4mElmSyntax7.Node<TopLevelExpose>(range, node.Value), afterExpose);
        }

        #endregion

        #region Import Formatting

        private (IReadOnlyList<Stil4mElmSyntax7.Node<Import>>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatImports(
            IReadOnlyList<Stil4mElmSyntax7.Node<Import>> imports,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            if (!imports.Any())
                return ([], context, formattedComments);

            var sortedImports =
                imports
                .OrderBy(import => string.Join(".", import.Value.ModuleName.Value))
                .ToList();

            var formattedImports = new List<Stil4mElmSyntax7.Node<Import>>();
            var currentContext = context;

            foreach (var import in sortedImports)
            {
                var (formattedImport, nextContext) = FormatImport(import, currentContext);
                formattedImports.Add(formattedImport);
                currentContext = nextContext.NextRow();
            }

            return (formattedImports, currentContext, formattedComments);
        }

        private static (Stil4mElmSyntax7.Node<Import>, FormattingContext) FormatImport(
            Stil4mElmSyntax7.Node<Import> import,
            FormattingContext context)
        {
            var importTokenLoc = context.CurrentLocation();
            var afterImportKeyword = context.Advance(Keywords.Import.Length);

            var moduleName = string.Join(".", import.Value.ModuleName.Value);
            var afterModuleName = afterImportKeyword.Advance(moduleName.Length);

            var currentContext = afterModuleName;

            (Location AsTokenLocation, Stil4mElmSyntax7.Node<IReadOnlyList<string>> Alias)? moduleAlias = null;
            if (import.Value.ModuleAlias is { } alias)
            {
                // Advance 1 for space before "as"
                var afterSpace = currentContext.Advance(1);
                var asTokenLoc = afterSpace.CurrentLocation();
                // Advance past "as " (3 chars: "as" + space after)
                currentContext = afterSpace.Advance(3);
                var aliasName = string.Join(".", alias.Alias.Value);
                var aliasRange = MakeRange(currentContext.CurrentLocation(), currentContext.Advance(aliasName.Length).CurrentLocation());
                currentContext = currentContext.Advance(aliasName.Length);
                moduleAlias = (asTokenLoc, new Stil4mElmSyntax7.Node<IReadOnlyList<string>>(aliasRange, alias.Alias.Value));
            }

            (Location ExposingTokenLocation, Stil4mElmSyntax7.Node<Exposing> ExposingList)? exposingList = null;
            if (import.Value.ExposingList is { } exposing)
            {
                var exposingTokenLoc = currentContext.Advance(1).CurrentLocation(); // space before exposing
                currentContext = currentContext.Advance(10); // " exposing "
                var exposingTextLength = exposing.ExposingList.Range.End.Column - exposing.ExposingList.Range.Start.Column;
                currentContext = currentContext.Advance(exposingTextLength);
                exposingList = (exposingTokenLoc, exposing.ExposingList);
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

        private (IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatDeclarations(
            IReadOnlyList<Stil4mElmSyntax7.Node<Declaration>> declarations,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            if (!declarations.Any())
                return ([], context, formattedComments);

            var formattedDeclarations = new List<Stil4mElmSyntax7.Node<Declaration>>();
            var currentContext = context;
            var currentComments = formattedComments;

            for (var i = 0; i < declarations.Count; i++)
            {
                var decl = declarations[i];
                var declContext = currentContext with { IndentSpaces = 0 };

                var (formattedDecl, nextContext, updatedComments) =
                    FormatDeclaration(decl, declContext, currentComments);

                currentComments = updatedComments;
                formattedDeclarations.Add(formattedDecl);

                if (i < declarations.Count - 1)
                {
                    var nextDecl = declarations[i + 1];
                    var commentsBetween = originalComments
                        .Where(c => c.Range.Start.Row > decl.Range.End.Row && c.Range.Start.Row <= nextDecl.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (formattedDecl.Value is Declaration.InfixDeclaration && nextDecl.Value is Declaration.InfixDeclaration)
                    {
                        currentContext = nextContext.NextRow();
                    }
                    else if (commentsBetween.Count is not 0)
                    {
                        currentContext = nextContext.NextRow().NextRow().NextRow();
                        foreach (var comment in commentsBetween)
                        {
                            var commentLocation = currentContext.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            currentContext = CreateContextAfterComment(currentContext, commentEnd);
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

            return (formattedDeclarations, currentContext, currentComments);
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
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented.")
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

            // Format documentation comment if present
            if (funcDecl.Function.Documentation is { } docComment)
            {
                var docCommentLocation = currentContext.CurrentLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                var formattedDocComment = docComment.WithRange(docCommentLocation, docCommentEnd);
                currentComments = currentComments.Add(formattedDocComment);
                currentContext = CreateContextAfterComment(currentContext, docCommentEnd);
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
                    var typeContext = afterColon.NextRow().Indent().SetIndentColumn();
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
                currentContext = afterType.ReturnToIndent(context).NextRow();
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
                formattedArguments.Add(MakeNodeWithRange(argStartLoc, argEndLoc, arg.Value));
            }

            // " ="
            var equalsLoc = afterArgs.Advance(1).CurrentLocation(); // space before =
            var afterEquals = afterArgs.Advance(2); // " ="

            // Move to next line and indent for the expression
            var exprContext = afterEquals.NextRow().Indent().SetIndentColumn();

            // Check for comments between equals and expression in original
            var equalsRow = funcDecl.Function.Declaration.Value.EqualsTokenLocation.Row;
            var exprStartRow = impl.Expression.Range.Start.Row;
            var commentsBeforeExpr = GetCommentsBetweenRows(equalsRow, exprStartRow);

            // Format any comments that appear before the expression
            foreach (var comment in commentsBeforeExpr)
            {
                var commentLocation = exprContext.CurrentLocation();
                var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                var formattedComment = comment.WithRange(commentLocation, commentEnd);
                currentComments = currentComments.Add(formattedComment);
                exprContext = CreateContextAfterComment(exprContext, commentEnd);
                // Maintain the same indent level after the comment (column is 1-based)
                exprContext = exprContext with { CurrentColumn = exprContext.IndentSpaces + 1 };
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
                exprResult.Context,
                currentComments);
        }

        private (Stil4mElmSyntax7.Node<Declaration>, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> formattedComments)
        {
            var currentComments = formattedComments;
            var startContext = context;

            if (aliasDecl.TypeAlias.Documentation is { } docComment)
            {
                var docCommentLocation = startContext.CurrentLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                var formattedDocComment = docComment.WithRange(docCommentLocation, docCommentEnd);
                currentComments = currentComments.Add(formattedDocComment);
                startContext = CreateContextAfterComment(startContext, docCommentEnd);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(5); // "type "
            var aliasTokenLoc = afterType.CurrentLocation();
            var afterTypeAlias = afterType.Advance(6); // "alias "

            var aliasName = aliasDecl.TypeAlias.Name.Value;
            var afterName = afterTypeAlias.Advance(aliasName.Length);

            var currentContext = afterName;
            foreach (var generic in aliasDecl.TypeAlias.Generics)
            {
                currentContext = currentContext.Advance(1);
                currentContext = currentContext.Advance(generic.Value.Length);
            }

            // " =" space before equals
            var afterNameSpace = currentContext.Advance(1);
            var equalsLoc = afterNameSpace.CurrentLocation();
            var afterEquals = afterNameSpace.Advance(1); // just "="

            var parentContext = afterEquals.NextRow();
            var typeContext = parentContext.Indent().SetIndentColumn();

            // Format the type annotation with proper locations
            var (formattedTypeAnnotation, afterTypeAnnot, typeAnnotComments) = FormatTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, typeContext);

            // Add any comments collected during type annotation formatting
            currentComments = currentComments.AddRange(typeAnnotComments);

            var formattedTypeAlias = new TypeAlias(
                Documentation: aliasDecl.TypeAlias.Documentation,
                TypeTokenLocation: typeTokenLoc,
                AliasTokenLocation: aliasTokenLoc,
                Name: aliasDecl.TypeAlias.Name,
                Generics: aliasDecl.TypeAlias.Generics,
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

            if (customTypeDecl.TypeDeclaration.Documentation is { } docComment)
            {
                var docCommentLocation = startContext.CurrentLocation();
                var docCommentEnd = CalculateCommentEndLocation(docCommentLocation, docComment.Value);
                var formattedDocComment = docComment.WithRange(docCommentLocation, docCommentEnd);
                currentComments = currentComments.Add(formattedDocComment);
                startContext = CreateContextAfterComment(startContext, docCommentEnd);
            }

            var typeTokenLoc = startContext.CurrentLocation();
            var afterType = startContext.Advance(5); // "type "

            var typeName = customTypeDecl.TypeDeclaration.Name.Value;
            var afterName = afterType.Advance(typeName.Length);

            var currentContext = afterName;
            var formattedGenerics = new List<Stil4mElmSyntax7.Node<string>>();
            foreach (var generic in customTypeDecl.TypeDeclaration.Generics)
            {
                currentContext = currentContext.Advance(1); // space
                var genericLoc = currentContext.CurrentLocation();
                currentContext = currentContext.Advance(generic.Value.Length);
                formattedGenerics.Add(MakeNodeWithRange(genericLoc, currentContext.CurrentLocation(), generic.Value));
            }

            // Move to next line for constructors
            var constructorIndentContext = currentContext.NextRow().Indent().SetIndentColumn();

            var equalsLoc = constructorIndentContext.CurrentLocation();
            var afterEquals = constructorIndentContext.Advance(2); // "= "

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
                    var commentsBetweenConstructors = originalComments
                        .Where(c => c.Range.Start.Row > prevConstructor.Range.End.Row &&
                                    c.Range.Start.Row < constructor.Range.Start.Row)
                        .OrderBy(c => c.Range.Start.Row)
                        .ToList();

                    if (commentsBetweenConstructors.Count is not 0)
                    {
                        // Format comments between constructors
                        foreach (var comment in commentsBetweenConstructors)
                        {
                            constructorCtx = constructorCtx.NextRow();
                            // Comment gets extra 2 space indent (6 spaces total = 4 base indent + 2 extra)
                            var commentCol = constructorIndentContext.IndentSpaces + 2 + 1; // +1 for 1-based column
                            constructorCtx = constructorCtx.SetColumn(commentCol);
                            var commentLocation = constructorCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            // Don't add extra blank lines after constructor comment
                            constructorCtx = new FormattingContext(
                                commentEnd.Row,
                                commentEnd.Column,
                                constructorIndentContext.IndentSpaces);
                        }
                    }

                    // Move to next line for subsequent constructors
                    constructorCtx = constructorCtx.NextRow().SetIndentColumn();
                    formattedPipeLoc = constructorCtx.CurrentLocation();
                    constructorCtx = constructorCtx.Advance(2); // "| "
                }

                // Constructor name
                var constructorStartLoc = constructorCtx.CurrentLocation();
                var afterConstructorName = constructorCtx.Advance(constructor.Value.Name.Value.Length);

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
                        // Indent by 2 from constructor name (not 4)
                        var indentColumn = constructorStartLoc.Column + 2;

                        foreach (var comment in commentsBetween)
                        {
                            argCtx = argCtx.NextRow().SetColumn(indentColumn);
                            var commentLocation = argCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            argCtx = new FormattingContext(commentEnd.Row, commentEnd.Column, argCtx.IndentSpaces);
                        }

                        // Next argument on new line
                        argCtx = argCtx.NextRow().SetColumn(indentColumn);
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
                        var indentColumn = constructorStartLoc.Column + 2;
                        argCtx = argCtx.NextRow().SetColumn(indentColumn);
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

                constructorCtx = argCtx;
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
            int? arrowBaseColumn = null)
        {
            var startLoc = context.CurrentLocation();
            var (formattedValue, afterType, comments) = FormatTypeAnnotationValue(typeAnnot.Value, context, arrowBaseColumn);
            var range = MakeRange(startLoc, afterType.CurrentLocation());
            return (new Stil4mElmSyntax7.Node<TypeAnnotation>(range, formattedValue), afterType, comments);
        }

        private (TypeAnnotation, FormattingContext, ImmutableList<Stil4mElmSyntax7.Node<string>>) FormatTypeAnnotationValue(
            TypeAnnotation typeAnnot,
            FormattingContext context,
            int? arrowBaseColumn = null)
        {
            var emptyComments = ImmutableList<Stil4mElmSyntax7.Node<string>>.Empty;

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
                                var argContext = currentCtx.NextRow().Indent().SetIndentColumn();
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

                        // Detect if tuple should be multiline based on original layout
                        var isMultilineTuple = false;
                        if (tupledElements.Count >= 2)
                        {
                            var firstRow = tupledElements[0].Range.Start.Row;
                            isMultilineTuple = tupledElements.Any(e => e.Range.Start.Row != firstRow);
                        }

                        // Also check if closing paren is on a different row
                        if (!isMultilineTuple && tupledElements.Count > 0)
                        {
                            isMultilineTuple = tupled.CloseParenLocation.Row > tupledElements[0].Range.Start.Row;
                        }

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

                            // Use opening paren column for alignment
                            var parenAlignColumn = openParenLoc.Column;

                            for (var i = 0; i < tupledElements.Count; i++)
                            {
                                var elem = tupledElements[i];
                                Location? separatorLoc = null;

                                if (i > 0)
                                {
                                    // Move to new line, align comma with opening paren
                                    tupledCtx = tupledCtx.NextRow().SetColumn(parenAlignColumn);
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
                            var closeCtx = tupledCtx.NextRow().SetColumn(parenAlignColumn);
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
                            var isSingleElement = tupledElements.Count == 1;

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
                                var (formattedElem, afterElem, elemComments) = FormatTypeAnnotation(tupledElements[i], tupledCtx);
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

                        // Determine the base column for arrows - use passed value or current column if first encounter
                        var effectiveArrowBaseColumn = arrowBaseColumn ?? context.CurrentColumn;

                        var (formattedArgType, afterArgType, argComments) = FormatTypeAnnotation(funcType.ArgumentType, context, effectiveArrowBaseColumn);

                        Location arrowLocation;
                        Stil4mElmSyntax7.Node<TypeAnnotation> formattedReturnType;
                        FormattingContext afterReturnType;
                        ImmutableList<Stil4mElmSyntax7.Node<string>> returnComments;

                        if (returnTypeOnNewLine)
                        {
                            // Multiline: arrow on new line, at the base column
                            var newLineCtx = afterArgType.NextRow().SetColumn(effectiveArrowBaseColumn);
                            arrowLocation = newLineCtx.CurrentLocation();
                            var arrowCtx = newLineCtx.Advance(3); // "-> "
                            (formattedReturnType, afterReturnType, returnComments) = FormatTypeAnnotation(funcType.ReturnType, arrowCtx, effectiveArrowBaseColumn);
                        }
                        else
                        {
                            // Single line: " -> "
                            var arrowCtx = afterArgType.Advance(1); // space before arrow
                            arrowLocation = arrowCtx.CurrentLocation();
                            var afterArrow = arrowCtx.Advance(3); // "-> "
                            (formattedReturnType, afterReturnType, returnComments) = FormatTypeAnnotation(funcType.ReturnType, afterArrow, effectiveArrowBaseColumn);
                        }

                        var formattedFuncType = new TypeAnnotation.FunctionTypeAnnotation(
                            formattedArgType,
                            arrowLocation,
                            formattedReturnType
                        );
                        return (formattedFuncType, afterReturnType, argComments.AddRange(returnComments));
                    }

                case TypeAnnotation.Record record:
                    {
                        // Format record type annotation
                        var recordFields = Stil4mElmSyntax7.FromStil4mConcretized.ToList(record.RecordDefinition.Fields);
                        var collectedComments = emptyComments;

                        // Detect if record should be multiline based on original layout
                        var isMultilineRecord = false;
                        if (recordFields.Count >= 2)
                        {
                            var firstRow = recordFields[0].Range.Start.Row;
                            isMultilineRecord = recordFields.Any(f => f.Range.Start.Row != firstRow);
                        }

                        // Also check if closing brace is on a different row (indicates multiline)
                        if (!isMultilineRecord && recordFields.Count > 0)
                        {
                            isMultilineRecord = record.CloseBraceLocation.Row > recordFields[0].Range.Start.Row;
                        }

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

                            // Use opening brace column for alignment
                            var braceAlignColumn = recordOpenBraceLoc.Column;

                            for (var i = 0; i < recordFields.Count; i++)
                            {
                                var field = recordFields[i];
                                Location? separatorLoc = null;

                                if (i > 0)
                                {
                                    // Check for comments between this field and the previous one
                                    var prevField = recordFields[i - 1];
                                    // Use the field type's end position instead of the whole field node's range
                                    // because the field node's range may incorrectly extend to the next field's separator
                                    var prevFieldEnd = prevField.Value.FieldType.Range.End;
                                    var commentsBetween = originalComments
                                        .Where(c => c.Range.Start.Row > prevFieldEnd.Row &&
                                                    c.Range.Start.Row < field.Range.Start.Row)
                                        .OrderBy(c => c.Range.Start.Row)
                                        .ToList();

                                    if (commentsBetween.Count > 0)
                                    {
                                        // Check if there's a blank line before the comment
                                        var hasBlankLineBeforeComment = commentsBetween[0].Range.Start.Row > prevFieldEnd.Row + 1;

                                        if (hasBlankLineBeforeComment)
                                        {
                                            // Blank line, then comment, then ", field"
                                            recordFieldCtx = recordFieldCtx.NextRow(); // blank line
                                        }

                                        // Add each comment on its own line
                                        foreach (var comment in commentsBetween)
                                        {
                                            recordFieldCtx = recordFieldCtx.NextRow().SetColumn(braceAlignColumn);
                                            var commentLocation = recordFieldCtx.CurrentLocation();
                                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                            // Add comment with updated position to the collected comments list
                                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                            collectedComments = collectedComments.Add(formattedComment);
                                            recordFieldCtx = new FormattingContext(
                                                commentEnd.Row,
                                                commentEnd.Column,
                                                recordFieldCtx.IndentSpaces);
                                        }
                                    }

                                    // Move to new line, align comma with opening brace
                                    recordFieldCtx = recordFieldCtx.NextRow().SetColumn(braceAlignColumn);
                                    separatorLoc = recordFieldCtx.CurrentLocation();
                                    recordFieldCtx = recordFieldCtx.Advance(2); // ", "
                                }

                                var fieldStartLoc = recordFieldCtx.CurrentLocation();
                                var afterFieldName = recordFieldCtx.Advance(field.Value.FieldName.Value.Length);
                                var colonLoc = afterFieldName.Advance(1).CurrentLocation(); // space before colon
                                var afterColon = afterFieldName.Advance(3); // " : "

                                // Check if field type should be on a new line
                                var isFieldTypeMultiline = field.Value.FieldType.Range.Start.Row > field.Value.FieldName.Range.Start.Row;

                                Stil4mElmSyntax7.Node<TypeAnnotation> formattedFieldType;
                                FormattingContext afterFieldType;
                                ImmutableList<Stil4mElmSyntax7.Node<string>> fieldTypeComments;

                                if (isFieldTypeMultiline)
                                {
                                    // Field type on new line with extra indentation
                                    var fieldTypeContext = afterColon.NextRow().Indent().SetIndentColumn();
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

                            // Closing brace on new line, aligned with opening brace
                            var closeCtx = recordFieldCtx.NextRow().SetColumn(braceAlignColumn);
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
            if (items.Count == 0)
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
                    "{ " + genericRecord.GenericName.Value + " | ... }",
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
            var range = MakeRange(startLoc, result.Context.CurrentLocation());
            return new FormattingResult<Stil4mElmSyntax7.Node<Expression>>(
                new Stil4mElmSyntax7.Node<Expression>(range, result.FormattedNode),
                result.Context,
                result.LocationMapping,
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
                            if (ch == '\n')
                            {
                                literalCtx = literalCtx.NextRow().SetColumn(1);
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
                        var litLen = literal.Value.Length + 2; // " + value + "
                        return FormattingResult<Expression>.Create(literal, context.Advance(litLen), comments);
                    }

                case Expression.CharLiteral charLit:
                    return FormattingResult<Expression>.Create(charLit, context.Advance(3), comments); // 'x'

                case Expression.Integer intLit:
                    return FormattingResult<Expression>.Create(intLit, context.Advance(intLit.Value.ToString().Length), comments);

                case Expression.Hex hexLit:
                    return FormattingResult<Expression>.Create(hexLit, context.Advance(2 + hexLit.Value.ToString("X").Length), comments);

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
                            negResult.LocationMapping,
                            negResult.Comments);
                    }

                case Expression.Application app:
                    {
                        // Check if application spans multiple lines in original
                        // This includes: multiple arguments on different rows, OR any single argument spanning multiple rows
                        var isMultiline = app.Arguments.Count >= 1 && (
                            (app.Arguments.Count >= 2 && app.Arguments.Any(arg => arg.Range.Start.Row != app.Arguments[0].Range.Start.Row)) ||
                            app.Arguments.Any(arg => arg.Range.End.Row > arg.Range.Start.Row));

                        var formattedArgs = new List<Stil4mElmSyntax7.Node<Expression>>();
                        var appCtx = context;
                        var currentComments = comments;

                        if (isMultiline)
                        {
                            // Multiline: each argument on its own line, all at the same column
                            // Calculate target column based on the starting position of the application
                            var multilineTargetColumn = context.GetNextMultipleOfFourColumn();

                            for (var i = 0; i < app.Arguments.Count; i++)
                            {
                                var arg = app.Arguments[i];
                                if (i > 0 || arg.Range.Start.Row > context.CurrentRow)
                                {
                                    // Move to next line at the consistent target column
                                    // Also set IndentSpaces so nested expressions (like lambda bodies) indent correctly
                                    appCtx = appCtx.NextRow().SetColumn(multilineTargetColumn)
                                        with
                                    { IndentSpaces = multilineTargetColumn - 1 };
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

                        if (recordFields.Count == 0)
                        {
                            return FormattingResult<Expression>.Create(
                                new Expression.RecordExpr(recordExpr.Fields),
                                afterOpenBrace.Advance(1), comments); // "}"
                        }

                        // Check if record should be multiline based on field row positions OR original range spanning multiple rows
                        var isMultilineRecord =
                            (recordFields.Count >= 2 && recordFields.Any(f => f.FieldName.Range.Start.Row != recordFields[0].FieldName.Range.Start.Row)) ||
                            originalRange.End.Row > originalRange.Start.Row;

                        if (isMultilineRecord)
                        {
                            // Multiline record: { field1 = val1
                            //                   , field2 = val2
                            //                   }
                            var afterOpenSpace = afterOpenBrace.Advance(1); // " "
                            var recordCtx = afterOpenSpace;
                            var currentComments = comments;

                            // Align subsequent fields with opening brace
                            var fieldAlignColumn = openBraceLoc.Column;

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
                                var targetColumn = new FormattingContext(
                                    afterFirstEq.CurrentRow,
                                    firstFieldStartLoc.Column,
                                    context.IndentSpaces).GetNextMultipleOfFourColumn();
                                var valueContext = afterFirstEq.NextRow().SetColumn(targetColumn);
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
                                recordCtx = recordCtx.NextRow().SetColumn(fieldAlignColumn);
                                var separatorLoc = recordCtx.CurrentLocation(); // Comma goes here
                                recordCtx = recordCtx.Advance(2); // ", "
                                var field = recordFields[i];
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
                                    var targetColumn = new FormattingContext(
                                        afterEq.CurrentRow,
                                        fieldStartLoc.Column,
                                        context.IndentSpaces).GetNextMultipleOfFourColumn();
                                    var valueContext = afterEq.NextRow().SetColumn(targetColumn);
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
                            var closeCtx = recordCtx.NextRow().SetColumn(fieldAlignColumn);
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

                        if (elements.Count == 0)
                        {
                            return FormattingResult<Expression>.Create(
                                new Expression.ListExpr(listExpr.Elements),
                                context.Advance(2), comments); // "[]"
                        }

                        // Check if list should be multiline based on the container range only.
                        // The AVH4 formatter should preserve the multiline structure if:
                        // - The container range spans multiple rows (start row != end row)
                        var isMultilineList = originalRange.End.Row > originalRange.Start.Row;

                        if (isMultilineList)
                        {
                            // Multiline list: first element on same line as "[", rest on their own lines with "," prefix
                            // Format: [ first_elem
                            //         , second_elem
                            //         ]
                            var afterListOpen = context.Advance(2); // "[ "
                            var currentComments = comments;

                            // Track the opening bracket column for alignment of subsequent elements
                            var listAlignColumn = listOpenLoc.Column;

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
                                    firstElemCtx = new FormattingContext(
                                        commentEnd.Row + 1,
                                        listAlignColumn + 2, // indent past bracket
                                        context.IndentSpaces);
                                }
                            }

                            // First element 
                            var firstElemResult = FormatExpression(elements[0], firstElemCtx, currentComments);
                            currentComments = firstElemResult.Comments;
                            var elemCtx = firstElemResult.Context;

                            // Check for trailing comment on the same row as the first element
                            var firstElemTrailingComment = originalComments
                                .FirstOrDefault(c => c.Range.Start.Row == elements[0].Range.Start.Row &&
                                                     c.Range.Start.Column > elements[0].Range.End.Column);

                            if (firstElemTrailingComment is not null)
                            {
                                // Add space and trailing comment
                                elemCtx = elemCtx.Advance(1); // space before comment
                                var trailingCommentLocation = elemCtx.CurrentLocation();
                                var trailingCommentEnd = CalculateCommentEndLocation(trailingCommentLocation, firstElemTrailingComment.Value);
                                var formattedTrailingComment = firstElemTrailingComment.WithRange(trailingCommentLocation, trailingCommentEnd);
                                currentComments = currentComments.Add(formattedTrailingComment);
                                elemCtx = new FormattingContext(
                                    trailingCommentEnd.Row,
                                    trailingCommentEnd.Column,
                                    context.IndentSpaces);
                            }

                            // Build rest list with proper separator locations
                            var restItems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Expression> Node)>();

                            // Subsequent elements each on new line with ", " at start, aligned with opening bracket
                            for (var i = 1; i < elements.Count; i++)
                            {
                                elemCtx = elemCtx.NextRow().SetColumn(listAlignColumn);
                                var separatorLoc = elemCtx.CurrentLocation(); // comma goes here

                                // Check for comments between this and previous element
                                var prevElem = elements[i - 1];
                                var currElem = elements[i];
                                var commentsBetween = originalComments
                                    .Where(c => c.Range.Start.Row > prevElem.Range.End.Row &&
                                                c.Range.Start.Row < currElem.Range.Start.Row)
                                    .OrderBy(c => c.Range.Start.Row)
                                    .ToList();

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

                                    if (!hasBlankLineBeforeComment && !wasOriginallyCompact && commentsBetween.Count == 1)
                                    {
                                        // Comment on same line as comma: ", -- comment\n  element"
                                        elemCtx = elemCtx.Advance(2); // ", "
                                        var commentLocation = elemCtx.CurrentLocation();
                                        var commentEnd = CalculateCommentEndLocation(commentLocation, firstComment.Value);
                                        var formattedComment = firstComment.WithRange(commentLocation, commentEnd);
                                        currentComments = currentComments.Add(formattedComment);

                                        // Element on next line, indented by 2 more spaces
                                        elemCtx = new FormattingContext(
                                            commentEnd.Row + 1,
                                            listAlignColumn + 2, // indent past comma
                                            context.IndentSpaces);
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
                                        elemCtx = elemCtx.NextRow().SetColumn(listAlignColumn);

                                        foreach (var comment in commentsBetween)
                                        {
                                            var commentLocation = elemCtx.CurrentLocation();
                                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                                            currentComments = currentComments.Add(formattedComment);
                                            // Next line for next comment or element
                                            elemCtx = new FormattingContext(
                                                commentEnd.Row + 1,
                                                listAlignColumn,
                                                context.IndentSpaces);
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
                                    var trailingComment = originalComments
                                        .FirstOrDefault(c => c.Range.Start.Row == currElem.Range.Start.Row &&
                                                             c.Range.Start.Column > currElem.Range.End.Column);

                                    if (trailingComment is not null)
                                    {
                                        // Add space and trailing comment
                                        elemCtx = elemCtx.Advance(1); // space before comment
                                        var trailingCommentLocation = elemCtx.CurrentLocation();
                                        var trailingCommentEnd = CalculateCommentEndLocation(trailingCommentLocation, trailingComment.Value);
                                        var formattedTrailingComment = trailingComment.WithRange(trailingCommentLocation, trailingCommentEnd);
                                        currentComments = currentComments.Add(formattedTrailingComment);
                                        elemCtx = new FormattingContext(
                                            trailingCommentEnd.Row,
                                            trailingCommentEnd.Column,
                                            context.IndentSpaces);
                                    }
                                }
                            }

                            // Closing bracket on new line, aligned with opening bracket
                            var closeCtx = elemCtx.NextRow().SetColumn(listAlignColumn);
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
                        var afterOpenParen = context.Advance(1); // "("
                        var innerResult = FormatExpression(parenExpr.Expression, afterOpenParen, comments);

                        // Check if inner content spans multiple rows
                        var innerIsMultiline = innerResult.Context.CurrentRow > afterOpenParen.CurrentRow;

                        Location closeParenLoc;
                        FormattingContext afterCloseParen;

                        if (innerIsMultiline)
                        {
                            // Multiline content: closing paren on new line, aligned with opening paren
                            var closeCtx = innerResult.Context.NextRow().SetColumn(openParenLoc.Column);
                            closeParenLoc = closeCtx.CurrentLocation();
                            afterCloseParen = closeCtx.Advance(1); // ")"
                        }
                        else
                        {
                            // Single line content: closing paren on same line
                            closeParenLoc = innerResult.Context.CurrentLocation();
                            afterCloseParen = innerResult.Context.Advance(1); // ")"
                        }

                        return new FormattingResult<Expression>(
                            new Expression.ParenthesizedExpression(openParenLoc, innerResult.FormattedNode, closeParenLoc),
                            afterCloseParen, [], innerResult.Comments);
                    }

                case Expression.OperatorApplication opApp:
                    {
                        var leftResult = FormatExpression(opApp.Left, context, comments);

                        // Check if the right operand is on a new line in the original
                        var rightOnNewLine = opApp.Right.Range.Start.Row > opApp.Left.Range.End.Row;

                        if (rightOnNewLine)
                        {
                            // Multiline: operator and right operand on new line, indented to next multiple of 4
                            var targetColumn = new FormattingContext(
                                leftResult.Context.CurrentRow,
                                context.CurrentColumn,
                                context.IndentSpaces).GetNextMultipleOfFourColumn();
                            var newLineCtx = leftResult.Context.NextRow().SetColumn(targetColumn);
                            var afterOp = newLineCtx.Advance(opApp.Operator.Length);
                            var afterOpSpace = afterOp.Advance(1); // " "
                            var rightResult = FormatExpression(opApp.Right, afterOpSpace, leftResult.Comments);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                opApp.Operator,
                                opApp.Direction,
                                leftResult.FormattedNode,
                                rightResult.FormattedNode
                            ), rightResult.Context.ReturnToIndent(context), rightResult.Comments);
                        }
                        else
                        {
                            // Single line
                            var afterLeftSpace = leftResult.Context.Advance(1); // " "
                            var afterOp = afterLeftSpace.Advance(opApp.Operator.Length);
                            var afterOpSpace = afterOp.Advance(1); // " "
                            var rightResult = FormatExpression(opApp.Right, afterOpSpace, leftResult.Comments);

                            return FormattingResult<Expression>.Create(new Expression.OperatorApplication(
                                opApp.Operator,
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
                    return FormatTupledExpression(tupledExpr, context, comments);

                case Expression.RecordUpdateExpression recordUpdate:
                    return FormatRecordUpdateExpression(recordUpdate, originalRange, context, comments);

                default:
                    throw new System.NotImplementedException(
                        $"Expression formatting not implemented for: {expr.GetType().Name}");
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

            // Check if condition is multiline based on original layout
            var conditionIsMultiline = ifBlock.Condition.Range.End.Row > ifBlock.Condition.Range.Start.Row ||
                                       ifBlock.ThenTokenLocation.Row > ifBlock.Condition.Range.Start.Row;

            // "if" or "if "
            var ifTokenLoc = context.CurrentLocation();

            // Use the chain base column if provided (for else-if chains), otherwise use current column
            var effectiveBaseColumn = chainBaseColumn ?? ifTokenLoc.Column;

            // Calculate body column as next multiple of 4 from the base column
            var bodyColumn = new FormattingContext(1, effectiveBaseColumn, context.IndentSpaces).GetNextMultipleOfFourColumn();

            FormattingResult<Stil4mElmSyntax7.Node<Expression>> conditionResult;
            Location thenTokenLoc;
            FormattingContext afterThen;

            if (conditionIsMultiline)
            {
                // Multiline condition: "if" on its own, condition indented on next line
                var afterIf = context.Advance(2); // "if" (no trailing space)
                var conditionContext = afterIf.NextRow().Indent().SetIndentColumn();

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
                        conditionContext = new FormattingContext(
                            commentEnd.Row + 1,
                            conditionColumn,
                            context.IndentSpaces);
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
                    var commentColumn = bodyColumn;
                    foreach (var comment in commentsAfterCondition)
                    {
                        afterConditionCtx = afterConditionCtx.NextRow().SetColumn(commentColumn);
                        var commentLocation = afterConditionCtx.CurrentLocation();
                        var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                        var formattedComment = comment.WithRange(commentLocation, commentEnd);
                        currentComments = currentComments.Add(formattedComment);
                        afterConditionCtx = new FormattingContext(
                            commentEnd.Row,
                            commentEnd.Column,
                            context.IndentSpaces);
                    }
                }

                // "then" on its own line at base indent
                var thenLineContext = afterConditionCtx.NextRow().ReturnToIndent(context).SetIndentColumn();
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
            var thenContext = afterThen.NextRow().SetColumn(bodyColumn)
                with
            { IndentSpaces = bodyColumn - 1 };

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
                    // Use bodyColumn - 1 for IndentSpaces to maintain correct nesting
                    thenContext = new FormattingContext(
                        commentEnd.Row + 1,
                        thenBodyColumn,
                        bodyColumn - 1);
                }
            }

            var thenResult = FormatExpression(ifBlock.ThenBlock, thenContext, currentComments);
            currentComments = thenResult.Comments;

            // Empty line before else - align with the base column
            // Reset IndentSpaces to the original context value for the else continuation
            var elseContext = thenResult.Context.NextRow().NextRow().SetColumn(effectiveBaseColumn)
                with
            { IndentSpaces = context.IndentSpaces };

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
                var innerConditionIsMultiline = innerIfBlock.Condition.Range.End.Row > innerIfBlock.Condition.Range.Start.Row ||
                                                innerIfBlock.ThenTokenLocation.Row > innerIfBlock.Condition.Range.Start.Row;

                FormattingResult<Stil4mElmSyntax7.Node<Expression>> innerConditionResult;
                Location innerThenLoc;
                FormattingContext afterInnerThen;

                if (innerConditionIsMultiline)
                {
                    // Multiline inner condition: "if" on its own line, condition indented
                    var afterInnerIf = afterElse.Advance(2); // "if" (no trailing space)
                    var innerConditionContext = afterInnerIf.NextRow().SetColumn(bodyColumn);

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
                            innerConditionContext = new FormattingContext(
                                commentEnd.Row + 1,
                                innerCondColumn,
                                context.IndentSpaces);
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
                        var commentColumn = bodyColumn;
                        foreach (var comment in commentsAfterInnerCondition)
                        {
                            afterInnerConditionCtx = afterInnerConditionCtx.NextRow().SetColumn(commentColumn);
                            var commentLocation = afterInnerConditionCtx.CurrentLocation();
                            var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                            var formattedComment = comment.WithRange(commentLocation, commentEnd);
                            currentComments = currentComments.Add(formattedComment);
                            afterInnerConditionCtx = new FormattingContext(
                                commentEnd.Row,
                                commentEnd.Column,
                                context.IndentSpaces);
                        }
                    }

                    // "then" on its own line at base indent
                    var innerThenLineContext = afterInnerConditionCtx.NextRow().SetColumn(effectiveBaseColumn);
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
                var innerThenContext = afterInnerThen.NextRow().SetColumn(bodyColumn)
                    with
                { IndentSpaces = bodyColumn - 1 };
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
                        // Use bodyColumn - 1 for IndentSpaces to maintain correct nesting
                        innerThenContext = new FormattingContext(
                            commentEnd.Row + 1,
                            innerThenBodyColumn,
                            bodyColumn - 1);
                    }
                }

                // Inner then block - indented from the base column (same as outer if)
                var innerThenResult = FormatExpression(innerIfBlock.ThenBlock, innerThenContext, currentComments);
                currentComments = innerThenResult.Comments;

                // Inner else - recurse for the rest of the chain - align with base column
                // Reset IndentSpaces to the original context value for the else/chain continuation
                var innerElseContext = innerThenResult.Context.NextRow().NextRow().SetColumn(effectiveBaseColumn)
                    with
                { IndentSpaces = context.IndentSpaces };
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
                    var innerElseBodyContext = afterInnerElse.NextRow().SetColumn(bodyColumn)
                        with
                    { IndentSpaces = bodyColumn - 1 };

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
                            // Use bodyColumn - 1 for IndentSpaces to maintain correct nesting
                            innerElseBodyContext = new FormattingContext(
                                commentEnd.Row + 1,
                                innerElseBodyColumn,
                                bodyColumn - 1);
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
                var elseBlockContext = afterElse.NextRow().SetColumn(bodyColumn)
                    with
                { IndentSpaces = bodyColumn - 1 };

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
                        // Use bodyColumn - 1 for IndentSpaces to maintain correct nesting
                        elseBlockContext = new FormattingContext(
                            commentEnd.Row + 1,
                            elseBodyColumn,
                            bodyColumn - 1);
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

            // "case "
            var caseTokenLoc = context.CurrentLocation();
            var afterCase = context.Advance(5);

            // Expression
            var exprResult = FormatExpression(caseExpr.CaseBlock.Expression, afterCase, currentComments);
            currentComments = exprResult.Comments;

            // " of" (space before of)
            var afterSpace = exprResult.Context.Advance(1);
            var ofTokenLoc = afterSpace.CurrentLocation();
            var afterOf = afterSpace.Advance(2);

            // Format cases
            var formattedCases = new List<Case>();
            var caseContext = afterOf.NextRow().Indent().SetIndentColumn();

            for (var i = 0; i < caseExpr.CaseBlock.Cases.Count; i++)
            {
                var caseItem = caseExpr.CaseBlock.Cases[i];

                // Add empty line between cases (after first case)
                if (i > 0)
                {
                    caseContext = caseContext.NextRow().SetIndentColumn();
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
                    caseContext = new FormattingContext(
                        commentEnd.Row + 1,
                        1,
                        caseContext.IndentSpaces).SetIndentColumn();
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

                // Expression on new line, indented
                var caseExprContext = afterArrow.NextRow().Indent().SetIndentColumn();

                // Check for comments before the case expression
                var commentsBeforeExpr = originalComments
                    .Where(c => c.Range.Start.Row > caseItem.Pattern.Range.End.Row &&
                                c.Range.Start.Row < caseItem.Expression.Range.Start.Row)
                    .OrderBy(c => c.Range.Start.Row)
                    .ToList();

                foreach (var comment in commentsBeforeExpr)
                {
                    var commentLocation = caseExprContext.CurrentLocation();
                    var commentEnd = CalculateCommentEndLocation(commentLocation, comment.Value);
                    var formattedComment = comment.WithRange(commentLocation, commentEnd);
                    currentComments = currentComments.Add(formattedComment);
                    caseExprContext = new FormattingContext(
                        commentEnd.Row + 1,
                        1,
                        caseExprContext.IndentSpaces).SetIndentColumn();
                }

                var caseExprResult = FormatExpression(caseItem.Expression, caseExprContext, currentComments);
                currentComments = caseExprResult.Comments;

                formattedCases.Add(new Case(
                    MakeNodeWithRange(patternStartLoc, afterPattern.CurrentLocation(), caseItem.Pattern.Value),
                    arrowLoc,
                    caseExprResult.FormattedNode
                ));

                // Only add newline for blank line before next case (not after last case)
                if (i < caseExpr.CaseBlock.Cases.Count - 1)
                {
                    caseContext = caseExprResult.Context.NextRow().ReturnToIndent(afterOf.Indent());
                }
                else
                {
                    caseContext = caseExprResult.Context.ReturnToIndent(afterOf.Indent());
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

            // "let"
            var letTokenLoc = context.CurrentLocation();
            var afterLet = context.Advance(3);

            // Format let declarations
            var formattedDecls = new List<Stil4mElmSyntax7.Node<Expression.LetDeclaration>>();
            var declContext = afterLet.NextRow().Indent().SetIndentColumn();

            for (var i = 0; i < letExpr.Value.Declarations.Count; i++)
            {
                var decl = letExpr.Value.Declarations[i];

                // Add empty line between declarations (after first)
                if (i > 0)
                {
                    declContext = declContext.NextRow().SetIndentColumn();
                }

                var declResult = FormatLetDeclaration(decl, declContext, currentComments);
                currentComments = declResult.Comments;
                formattedDecls.Add(declResult.FormattedNode);

                declContext = declResult.Context.NextRow();
            }

            // "in" on own line at original indent
            var inContext = declContext.ReturnToIndent(context).SetIndentColumn();
            var inTokenLoc = inContext.CurrentLocation();
            var afterIn = inContext.Advance(2);

            // Expression after "in"
            var exprContext = afterIn.NextRow().SetIndentColumn();
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
                            // Format the signature: "name : type"
                            var sigNameLoc = currentCtx.CurrentLocation();
                            var afterSigName = currentCtx.Advance(funcName.Length);

                            // " : "
                            var afterSigNameSpace = afterSigName.Advance(1);
                            var colonLoc = afterSigNameSpace.CurrentLocation();
                            var afterColon = afterSigNameSpace.Advance(2); // ": "

                            // Format type annotation on same line (single-line type annotations in let blocks)
                            var (formattedTypeAnnot, afterTypeAnnot, typeAnnotComments) = FormatTypeAnnotation(signature.Value.TypeAnnotation, afterColon);
                            comments = comments.AddRange(typeAnnotComments);

                            var formattedSigValue = new Signature(
                                MakeNodeWithRange(sigNameLoc, afterSigName.CurrentLocation(), funcName),
                                colonLoc,
                                formattedTypeAnnot
                            );

                            formattedSignature = MakeNodeWithRange(sigNameLoc, afterTypeAnnot.CurrentLocation(), formattedSigValue);

                            // Move to next line for function implementation
                            currentCtx = afterTypeAnnot.NextRow().ReturnToIndent(context).SetIndentColumn();
                        }

                        // Now format the function implementation
                        var implStartLoc = currentCtx.CurrentLocation();
                        var afterName = currentCtx.Advance(funcName.Length);

                        // Arguments
                        var argTexts = letFunc.Function.Declaration.Value.Arguments.Select(a => FormatPatternText(a.Value)).ToList();
                        var afterArgs = afterName;
                        foreach (var argText in argTexts)
                        {
                            afterArgs = afterArgs.Advance(1 + argText.Length);
                        }

                        // " =" (space before equals)
                        var afterArgsSpace = afterArgs.Advance(1);
                        var equalsLoc = afterArgsSpace.CurrentLocation();
                        var afterEquals = afterArgsSpace.Advance(1); // just "="

                        // Expression on new line, indented
                        var exprContext = afterEquals.NextRow().Indent().SetIndentColumn();
                        var exprResult = FormatExpression(letFunc.Function.Declaration.Value.Expression, exprContext, comments);

                        var formattedImpl = new FunctionImplementation(
                            MakeNodeWithRange(implStartLoc, afterName.CurrentLocation(), funcName),
                            letFunc.Function.Declaration.Value.Arguments,
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

                        // " =" (space before equals)
                        var afterPatternSpace = afterPattern.Advance(1);
                        var destructEqualsLoc = afterPatternSpace.CurrentLocation();
                        var afterDestructEquals = afterPatternSpace.Advance(1); // just "="

                        var destructExprContext = afterDestructEquals.NextRow().Indent().SetIndentColumn();
                        var exprResult = FormatExpression(letDestructuring.Expression, destructExprContext, comments);

                        var formattedDestructuring = new Expression.LetDeclaration.LetDestructuring(
                            letDestructuring.Pattern,
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
                        $"LetDeclaration formatting not implemented for: {letDecl.Value.GetType().Name}");
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
            // The SnapshotTestFormat signals this by placing body on a different row
            var bodyOnNewLine = lambdaExpr.Lambda.Expression.Range.Start.Row > afterArrow.CurrentRow;

            FormattingResult<Stil4mElmSyntax7.Node<Expression>> exprResult;

            if (bodyOnNewLine)
            {
                // Body on new line with extra indentation
                var bodyContext = afterArrow.NextRow().Indent().SetIndentColumn();
                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, bodyContext, comments);
            }
            else
            {
                // Body on same line after space
                var afterSpace = afterArrow.Advance(1);
                exprResult = FormatExpression(lambdaExpr.Lambda.Expression, afterSpace, comments);
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
            FormattingContext context,
            ImmutableList<Stil4mElmSyntax7.Node<string>> comments)
        {
            var elements = Stil4mElmSyntax7.FromStil4mConcretized.ToList(tupledExpr.Elements);
            var currentComments = comments;

            // Check if tuple should be multiline based on element row positions
            var isMultiline = elements.Count >= 2 &&
                elements.Any(elem => elem.Range.Start.Row != elements[0].Range.Start.Row);

            var openParenLoc = context.CurrentLocation();

            if (isMultiline)
            {
                // Multiline tuple: ( first_elem
                //                  , second_elem
                //                  )
                var afterOpenParen = context.Advance(2); // "( "

                // First element on same line as opening paren
                var firstElemResult = FormatExpression(elements[0], afterOpenParen, currentComments);
                currentComments = firstElemResult.Comments;
                var tupledCtx = firstElemResult.Context;

                // Build rest with proper separator locations
                var restItems = new List<(Location SeparatorLocation, Stil4mElmSyntax7.Node<Expression> Node)>();

                // Subsequent elements aligned with the opening paren
                var elemAlignColumn = openParenLoc.Column;

                for (var i = 1; i < elements.Count; i++)
                {
                    tupledCtx = tupledCtx.NextRow().SetColumn(elemAlignColumn);
                    var separatorLoc = tupledCtx.CurrentLocation(); // comma goes here
                    tupledCtx = tupledCtx.Advance(2); // ", "
                    var elemResult = FormatExpression(elements[i], tupledCtx, currentComments);
                    currentComments = elemResult.Comments;
                    restItems.Add((separatorLoc, elemResult.FormattedNode));
                    tupledCtx = elemResult.Context;
                }

                // Closing paren on new line, aligned with opening paren
                var closeCtx = tupledCtx.NextRow().SetColumn(elemAlignColumn);
                var closeParenLoc = closeCtx.CurrentLocation();
                var afterCloseParen = closeCtx.Advance(1); // ")"

                var multilineSeparatedList = new SeparatedSyntaxList<Stil4mElmSyntax7.Node<Expression>>.NonEmpty(
                    firstElemResult.FormattedNode, restItems);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    openParenLoc,
                    multilineSeparatedList,
                    closeParenLoc
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
                var closeParenLoc = afterSpace.CurrentLocation();
                var afterCloseParen = afterSpace.Advance(1);

                return FormattingResult<Expression>.Create(new Expression.TupledExpression(
                    openParenLoc,
                    ToSeparatedSyntaxList(formattedElements),
                    closeParenLoc
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

            // Detect if record update should be multiline based on original layout
            var isMultiline = fields.Count >= 1 &&
                (recordUpdate.PipeLocation.Row > recordUpdate.RecordName.Range.Start.Row ||
                 fields.Any(f => f.FieldName.Range.Start.Row != fields[0].FieldName.Range.Start.Row));

            // Also check if closing brace is on a different row (use originalRange.End for closing brace position)
            if (!isMultiline && fields.Count > 0)
            {
                isMultiline = originalRange.End.Row > fields[0].FieldName.Range.Start.Row;
            }

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
                // Pipe is on next line
                var pipeContext = afterRecordName.NextRow().Indent().SetIndentColumn();
                var pipeLoc = pipeContext.CurrentLocation();
                var afterPipe = pipeContext.Advance(2); // "| "

                // Use pipe location column for alignment
                var pipeAlignColumn = pipeLoc.Column;

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
                        fieldCtx = fieldCtx.NextRow().SetColumn(pipeAlignColumn);
                        separatorLoc = fieldCtx.CurrentLocation();
                        fieldCtx = fieldCtx.Advance(2); // ", "
                    }

                    var fieldStartLoc = fieldCtx.CurrentLocation();
                    var afterFieldName = fieldCtx.Advance(field.FieldName.Value.Length);
                    var equalsLoc = afterFieldName.Advance(1).CurrentLocation(); // " = "
                    var afterEq = afterFieldName.Advance(3); // " = "
                    var fieldResult = FormatExpression(field.ValueExpr, afterEq, currentComments);
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
                var closeCtx = fieldCtx.NextRow().SetColumn(context.CurrentLocation().Column);
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
