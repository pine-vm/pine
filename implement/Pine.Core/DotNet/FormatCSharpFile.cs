using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

/// <summary>
/// New C# formatter using a multi-pass convergence loop architecture.
/// Each pass formats the syntax tree, converts to text, re-parses, and compares
/// with the previous text. The loop stops when text is identical between passes
/// (convergence) or after a maximum number of iterations.
/// <para>
/// All methods are static and pure — no mutable instance state.
/// </para>
/// <para>
/// For the formatting specifications, see 'csharp-coding-guidelines.md'
/// </para>
/// </summary>
public static class FormatCSharpFile
{
    private const int MaxPasses = 10;

    private const int IndentSize = 4;

    private const int MaximumLineLength = 120;

    /// <summary>Cached line-feed trivia token.</summary>
    private static readonly SyntaxTrivia s_lineFeed = SyntaxFactory.LineFeed;

    /// <summary>Cached single-space trivia token.</summary>
    private static readonly SyntaxTrivia s_space = SyntaxFactory.Space;

    // ──────────────────────────────────────────────────────────
    // Public entry point
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a syntax tree using multi-pass convergence until stable.</summary>
    public static SyntaxTree FormatSyntaxTree(SyntaxTree syntaxTree)
    {
        var previousText = syntaxTree.GetRoot().ToFullString();

        for (var pass = 0; pass < MaxPasses; pass++)
        {
            var formatted = FormatSinglePass(syntaxTree);
            var formattedText = formatted.GetRoot().ToFullString();

            if (formattedText == previousText)
                return formatted;

            previousText = formattedText;
            syntaxTree = formatted;
        }

        return syntaxTree;
    }

    /// <summary>Runs one formatting pass over the entire syntax tree.</summary>
    private static SyntaxTree FormatSinglePass(SyntaxTree syntaxTree)
    {
        var root = syntaxTree.GetRoot();
        var formattedRoot = FormatNode(root, 0);
        return syntaxTree.WithRootAndOptions(formattedRoot, syntaxTree.Options);
    }

    // ──────────────────────────────────────────────────────────
    // Helpers
    // ──────────────────────────────────────────────────────────

    /// <summary>Creates whitespace trivia for the given indentation level.</summary>
    private static SyntaxTrivia Indent(int level) =>
        level <= 0 ? SyntaxFactory.Whitespace("") : SyntaxFactory.Whitespace(new string(' ', level * IndentSize));

    /// <summary>Returns true if the trivia is an end-of-line token.</summary>
    private static bool IsLineBreak(SyntaxTrivia t) =>
        t.IsKind(SyntaxKind.EndOfLineTrivia);

    /// <summary>Returns true if the trivia is a whitespace token.</summary>
    private static bool IsWhitespace(SyntaxTrivia t) =>
        t.IsKind(SyntaxKind.WhitespaceTrivia);

    /// <summary>
    /// Checks if the node's own text (excluding leading trivia, but including trailing)
    /// spans multiple lines. This avoids counting preceding comments as part of the node.
    /// </summary>
    private static bool SpansMultipleLines(SyntaxNode node)
    {
        var loc = node.GetLocation();

        if (loc != Location.None)
        {
            var span = loc.GetLineSpan();
            return span.EndLinePosition.Line > span.StartLinePosition.Line;
        }
        // Fallback for nodes not part of a SyntaxTree (e.g. modified via .With...())
        var text = node.ToFullString();

        var nl = text.IndexOf('\n');
        return nl >= 0 && nl < text.Length - 1;
    }

    /// <summary>Checks if the given text spans multiple lines.</summary>
    private static bool SpansMultipleLines(string text)
    {
        var nl = text.IndexOf('\n');
        return nl >= 0 && nl < text.Length - 1;
    }

    /// <summary>Returns the column length of the line where the node starts, or 0 if not available.</summary>
    private static int LineEndColumn(SyntaxNode node)
    {
        var loc = node.GetLocation();

        if (loc == Location.None)
            return 0;

        var lineSpan = loc.GetLineSpan();
        var src = node.SyntaxTree.GetText();
        return src.Lines[lineSpan.StartLinePosition.Line].ToString().TrimEnd().Length;
    }

    /// <summary>Returns the start line number of the given token.</summary>
    private static int LineOf(SyntaxToken t) =>
        t.GetLocation().GetLineSpan().StartLinePosition.Line;

    /// <summary>Returns the start line number of the given node.</summary>
    private static int LineOf(SyntaxNode n) =>
        n.GetLocation().GetLineSpan().StartLinePosition.Line;

    /// <summary>Returns the end line number of the given node.</summary>
    private static int EndLineOf(SyntaxNode n) =>
        n.GetLocation().GetLineSpan().EndLinePosition.Line;

    /// <summary>
    /// True when the original node has a single-line block body like <c>{ }</c> or <c>{ return; }</c>.
    /// </summary>
    private static bool IsSingleLineBlock(BlockSyntax block)
    {
        var openLine = LineOf(block.OpenBraceToken);
        var closeLine = LineOf(block.CloseBraceToken);
        return openLine == closeLine;
    }

    /// <summary>
    /// Count consecutive EndOfLineTrivia in a trivia list.
    /// </summary>
    private static int CountLineBreaks(SyntaxTriviaList trivia) =>
        trivia.Count(t => IsLineBreak(t));

    /// <summary>
    /// Rebuild leading trivia: preserve all comments/structured trivia, 
    /// replace whitespace with correct indent, ensure correct number of leading line breaks.
    /// </summary>
    private static SyntaxTriviaList RebuildLeading(SyntaxTriviaList orig, int indent)
    {
        var r = new List<SyntaxTrivia>();
        var atLineStart = true;

        foreach (var t in orig)
        {
            if (IsLineBreak(t))
            {
                r.Add(s_lineFeed);
                atLineStart = true;
            }
            else if (IsWhitespace(t))
            {
                // skip — we re-add indent as needed
            }
            else if (t.HasStructure)
            {
                // Structured trivia (doc comments, preprocessor directives)
                // They contain their own internal newlines.
                // We need indent before them.
                if (atLineStart)
                    r.Add(Indent(indent));

                r.Add(t);
                // Structured trivia like doc comments end with a newline internally,
                // so the next token starts at beginning of line
                atLineStart = true;
            }
            else
            {
                // non-whitespace: single-line comment, etc.
                if (atLineStart)
                    r.Add(Indent(indent));

                r.Add(t);
                atLineStart = false;
            }
        }

        // Final indent for the token itself
        if (atLineStart)
            r.Add(Indent(indent));

        return [.. r];
    }

    /// <summary>
    /// Build leading trivia with at least <paramref name="minBreaks"/> line breaks,
    /// preserving any comments from the original trivia.
    /// </summary>
    private static SyntaxTriviaList EnsureLeadingBreaks(SyntaxTriviaList orig, int minBreaks, int indent)
    {
        // Collect all non-whitespace trivia (comments, preprocessor, etc.)
        var preserved = new List<SyntaxTrivia>();

        foreach (var t in orig)
        {
            if (!IsLineBreak(t) && !IsWhitespace(t))
                preserved.Add(t);
        }

        var r = new List<SyntaxTrivia>();

        for (var i = 0; i < minBreaks; i++)
            r.Add(s_lineFeed);

        foreach (var t in preserved)
        {
            r.Add(Indent(indent));
            r.Add(t);
            // Doc comments include their own trailing newline; others need one
            if (!t.HasStructure)
                r.Add(s_lineFeed);
        }

        r.Add(Indent(indent));
        return [.. r];
    }

    /// <summary>
    /// Removes whitespace and line-break trivia from a trivia list,
    /// but preserves line breaks that follow single-line comments (they are required).
    /// </summary>
    private static SyntaxTriviaList StripWhitespace(SyntaxTriviaList trivia)
    {
        var r = new List<SyntaxTrivia>();
        var lastWasComment = false;

        foreach (var t in trivia)
        {
            if (IsLineBreak(t))
            {
                if (lastWasComment)
                    r.Add(t);

                lastWasComment = false;
            }
            else if (IsWhitespace(t))
            {
                lastWasComment = false;
            }
            else
            {
                r.Add(t);
                lastWasComment = t.IsKind(SyntaxKind.SingleLineCommentTrivia);
            }
        }

        return [.. r];
    }


    /// <summary>
    /// Rebuilds separator tokens (commas), preserving any trailing comments from the originals.
    /// </summary>
    private static List<SyntaxToken> RebuildSeparators(
        IReadOnlyList<SyntaxToken> origSeps,
        int count)
    {
        var seps = new List<SyntaxToken>();

        for (var i = 0; i < count; i++)
        {
            var trailing =
                i < origSeps.Count
                ?
                StripWhitespace(origSeps[i].TrailingTrivia)
                :
                default;

            seps.Add(
                SyntaxFactory.Token(SyntaxKind.CommaToken)
                .WithLeadingTrivia().WithTrailingTrivia(trailing));
        }

        return seps;
    }


    // ──────────────────────────────────────────────────────────
    // Main dispatch
    // ──────────────────────────────────────────────────────────

    /// <summary>Dispatches a syntax node to the appropriate formatting method.</summary>
    private static SyntaxNode FormatNode(SyntaxNode node, int indent)
    {
        return
            node switch
            {
                CompilationUnitSyntax n => FormatCompilationUnit(n, indent),

                // Declarations
                FileScopedNamespaceDeclarationSyntax n => FormatFileScopedNamespace(n, indent),
                NamespaceDeclarationSyntax n => FormatNamespaceDeclaration(n, indent),
                ClassDeclarationSyntax n => FormatTypeDeclaration(n, indent),
                StructDeclarationSyntax n => FormatTypeDeclaration(n, indent),
                RecordDeclarationSyntax n => FormatTypeDeclaration(n, indent),
                InterfaceDeclarationSyntax n => FormatTypeDeclaration(n, indent),
                EnumDeclarationSyntax n => FormatEnumDeclaration(n, indent),
                DelegateDeclarationSyntax => node,
                MethodDeclarationSyntax n => FormatMethodDeclaration(n, indent),
                ConstructorDeclarationSyntax n => FormatConstructorDeclaration(n, indent),
                DestructorDeclarationSyntax => node,
                OperatorDeclarationSyntax => node,
                ConversionOperatorDeclarationSyntax => node,
                PropertyDeclarationSyntax n => FormatPropertyDeclaration(n, indent),
                IndexerDeclarationSyntax => node,
                EventDeclarationSyntax => node,
                EventFieldDeclarationSyntax => node,
                FieldDeclarationSyntax n => FormatFieldDeclaration(n, indent),
                LocalFunctionStatementSyntax n => FormatLocalFunctionStatement(n, indent),

                // Statements
                BlockSyntax n => FormatBlock(n, indent),
                LocalDeclarationStatementSyntax n => FormatLocalDeclarationStatement(n, indent),
                ExpressionStatementSyntax n => FormatExpressionStatement(n, indent),
                ReturnStatementSyntax n => FormatReturnStatement(n, indent),
                IfStatementSyntax n => FormatIfStatement(n, indent),
                ElseClauseSyntax n => FormatElseClause(n, indent),
                WhileStatementSyntax n => FormatWhileStatement(n, indent),
                ForStatementSyntax n => FormatForStatement(n, indent),
                ForEachStatementSyntax n => FormatForEachStatement(n, indent),
                ForEachVariableStatementSyntax n => FormatForEachVariableStatement(n, indent),
                DoStatementSyntax n => FormatDoStatement(n, indent),
                SwitchStatementSyntax n => FormatSwitchStatement(n, indent),
                TryStatementSyntax n => FormatTryStatement(n, indent),
                ThrowStatementSyntax n => FormatThrowStatement(n, indent),
                UsingStatementSyntax n => FormatUsingStatement(n, indent),
                LockStatementSyntax n => FormatLockStatement(n, indent),
                BreakStatementSyntax => node,
                ContinueStatementSyntax => node,
                YieldStatementSyntax => node,
                EmptyStatementSyntax => node,
                LabeledStatementSyntax => node,
                GotoStatementSyntax => node,
                CheckedStatementSyntax => node,
                UnsafeStatementSyntax => node,
                FixedStatementSyntax => node,
                GlobalStatementSyntax n => FormatGlobalStatement(n, indent),

                // Expressions
                InvocationExpressionSyntax n => FormatInvocationExpression(n, indent),
                ObjectCreationExpressionSyntax n => FormatObjectCreationExpression(n, indent),
                ImplicitObjectCreationExpressionSyntax n => FormatImplicitObjectCreationExpression(n, indent),
                MemberAccessExpressionSyntax n => FormatMemberAccessExpression(n, indent),
                ConditionalExpressionSyntax n => FormatConditionalExpression(n, indent),
                SwitchExpressionSyntax n => FormatSwitchExpression(n, indent),
                CollectionExpressionSyntax n => FormatCollectionExpression(n, indent),
                InitializerExpressionSyntax n => FormatInitializerExpression(n, indent),
                AssignmentExpressionSyntax n => FormatAssignmentExpression(n, indent),
                ParenthesizedLambdaExpressionSyntax n => FormatParenthesizedLambdaExpression(n, indent),
                SimpleLambdaExpressionSyntax n => FormatSimpleLambdaExpression(n, indent),
                BinaryExpressionSyntax n => FormatBinaryExpression(n, indent),

                ThrowExpressionSyntax n => FormatThrowExpression(n, indent),
                ParenthesizedExpressionSyntax n => n.WithExpression((ExpressionSyntax)FormatNode(n.Expression, indent)),

                ConditionalAccessExpressionSyntax n =>
                n.WithExpression((ExpressionSyntax)FormatNode(n.Expression, indent)),

                CastExpressionSyntax n => n.WithExpression((ExpressionSyntax)FormatNode(n.Expression, indent)),
                AwaitExpressionSyntax n => n.WithExpression((ExpressionSyntax)FormatNode(n.Expression, indent)),

                // Argument/Parameter lists
                ArgumentListSyntax n => FormatArgumentList(n, indent),
                BracketedArgumentListSyntax => node,
                ParameterListSyntax n => FormatParameterList(n, indent),
                BracketedParameterListSyntax => node,
                TypeParameterListSyntax => node,

                // Arrow expression and equals value
                ArrowExpressionClauseSyntax n => FormatArrowExpressionClause(n, indent),
                EqualsValueClauseSyntax n => FormatEqualsValueClause(n, indent),

                // Switch parts
                SwitchExpressionArmSyntax n => FormatSwitchExpressionArm(n, indent),
                SwitchSectionSyntax n => FormatSwitchSection(n, indent),

                // Patterns
                BinaryPatternSyntax n => FormatBinaryPattern(n, indent),

                // Catch/finally
                CatchClauseSyntax n => FormatCatchClause(n, indent),
                FinallyClauseSyntax n => FormatFinallyClause(n, indent),

                // Using directives
                UsingDirectiveSyntax => node,

                // Leaf types — return as-is
                PredefinedTypeSyntax or IdentifierNameSyntax or GenericNameSyntax or
                QualifiedNameSyntax or AliasQualifiedNameSyntax or NullableTypeSyntax or
                ArrayTypeSyntax or TupleTypeSyntax or PointerTypeSyntax or RefTypeSyntax or
                OmittedTypeArgumentSyntax or FunctionPointerTypeSyntax or ScopedTypeSyntax or
                LiteralExpressionSyntax or ThisExpressionSyntax or BaseExpressionSyntax or
                DefaultExpressionSyntax or TypeOfExpressionSyntax or SizeOfExpressionSyntax or
                InterpolatedStringExpressionSyntax or ElementAccessExpressionSyntax or
                ImplicitArrayCreationExpressionSyntax or ArrayCreationExpressionSyntax or
                StackAllocArrayCreationExpressionSyntax or ImplicitStackAllocArrayCreationExpressionSyntax or
                AnonymousObjectCreationExpressionSyntax or AnonymousMethodExpressionSyntax or
                TupleExpressionSyntax or RefExpressionSyntax or DeclarationExpressionSyntax or
                IsPatternExpressionSyntax or CheckedExpressionSyntax or MakeRefExpressionSyntax or
                RefTypeExpressionSyntax or RefValueExpressionSyntax or PostfixUnaryExpressionSyntax or
                PrefixUnaryExpressionSyntax or RangeExpressionSyntax or WithExpressionSyntax or
                QueryExpressionSyntax or OmittedArraySizeExpressionSyntax or
                BaseObjectCreationExpressionSyntax or
                ConstantPatternSyntax or DeclarationPatternSyntax or DiscardPatternSyntax or
                RecursivePatternSyntax or VarPatternSyntax or TypePatternSyntax or
                RelationalPatternSyntax or UnaryPatternSyntax or
                ParenthesizedPatternSyntax or ListPatternSyntax or SlicePatternSyntax or
                WhenClauseSyntax or CatchDeclarationSyntax or CatchFilterClauseSyntax or
                ArgumentSyntax or ParameterSyntax or TypeParameterSyntax or TypeArgumentListSyntax or
                AttributeListSyntax or AttributeArgumentListSyntax or BaseListSyntax or
                SimpleBaseTypeSyntax or PrimaryConstructorBaseTypeSyntax or
                TypeParameterConstraintClauseSyntax or
                CaseSwitchLabelSyntax or CasePatternSwitchLabelSyntax or DefaultSwitchLabelSyntax or
                VariableDeclarationSyntax or VariableDeclaratorSyntax or
                AccessorListSyntax or AccessorDeclarationSyntax or
                ExpressionElementSyntax or SpreadElementSyntax or
                InterpolationSyntax or IncompleteMemberSyntax or ExternAliasDirectiveSyntax or
                ConstructorInitializerSyntax or ImplicitElementAccessSyntax =>
                node,

                // Require an explicit branch for each variant
                _ =>
                throw new NotImplementedException(
                    $"Formatting not implemented for node type: {node.GetType().Name}")
            };
    }

    // ──────────────────────────────────────────────────────────
    // Compilation Unit
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a compilation unit, including usings and top-level members.</summary>
    public static CompilationUnitSyntax FormatCompilationUnit(CompilationUnitSyntax node, int indent)
    {
        var usings = FormatUsingDirectives(node.Usings);
        var members = FormatMemberList(node.Members, indent);

        // Ensure blank line between usings and first member
        // The last using has empty trailing trivia, so we need 2 linebreaks:
        // one for the newline after the using, and one for the blank line.
        if (usings.Count > 0 && members.Count > 0)
        {
            var first = members[0];
            var leading = first.GetLeadingTrivia();

            members =
                members.Replace(
                    first,
                    first.WithLeadingTrivia(
                        EnsureLeadingBreaks(leading, 2, indent)));
        }

        // Ensure file ends with exactly one newline
        if (members.Count > 0)
        {
            var last = members.Last();
            members = members.Replace(last, last.WithTrailingTrivia(s_lineFeed));
        }
        else if (usings.Count > 0)
        {
            var last = usings.Last();
            usings = usings.Replace(last, last.WithTrailingTrivia(s_lineFeed));
        }

        return node.WithUsings(usings).WithMembers(members);
    }

    // ──────────────────────────────────────────────────────────
    // Using Directives
    // ──────────────────────────────────────────────────────────

    /// <summary>Sorts and groups using directives by kind (regular, alias, static).</summary>
    private static SyntaxList<UsingDirectiveSyntax> FormatUsingDirectives(SyntaxList<UsingDirectiveSyntax> usings)
    {
        if (usings.Count is 0)
            return usings;

        var regular = new List<UsingDirectiveSyntax>();
        var statics = new List<UsingDirectiveSyntax>();
        var aliases = new List<UsingDirectiveSyntax>();

        foreach (var u in usings)
        {
            if (u.Alias is not null)
                aliases.Add(u);

            else if (u.StaticKeyword != default)
                statics.Add(u);

            else
                regular.Add(u);
        }

        regular = [.. regular.OrderBy(u => u.Name?.ToString() ?? "", StringComparer.OrdinalIgnoreCase)];
        statics = [.. statics.OrderBy(u => u.Name?.ToString() ?? "", StringComparer.OrdinalIgnoreCase)];

        var result = new List<UsingDirectiveSyntax>();

        void AddGroup(List<UsingDirectiveSyntax> group)
        {
            if (group.Count is 0)
                return;

            for (var i = 0; i < group.Count; i++)
            {
                var u = group[i];

                // Preserve any comments from the original leading trivia
                var origComments = StripWhitespace(u.GetLeadingTrivia());

                if (result.Count is 0 && i is 0)
                {
                    // First using overall: preserve the file-level leading trivia
                    u = u.WithLeadingTrivia(usings[0].GetLeadingTrivia());
                }
                else if (i is 0)
                {
                    // First of a subsequent group: blank line before group
                    var leading = new List<SyntaxTrivia> { s_lineFeed, s_lineFeed };

                    foreach (var c in origComments)
                    {
                        leading.Add(c);

                        if (!c.HasStructure)
                            leading.Add(s_lineFeed);
                    }

                    u = u.WithLeadingTrivia(new SyntaxTriviaList(leading));
                }
                else
                {
                    // Subsequent usings within a group
                    var leading = new List<SyntaxTrivia> { s_lineFeed };

                    foreach (var c in origComments)
                    {
                        leading.Add(c);

                        if (!c.HasStructure)
                            leading.Add(s_lineFeed);
                    }

                    u = u.WithLeadingTrivia(new SyntaxTriviaList(leading));
                }

                u = u.WithTrailingTrivia();
                result.Add(u);
            }
        }

        AddGroup(regular);
        AddGroup(aliases);
        AddGroup(statics);

        return SyntaxFactory.List(result);
    }

    // ──────────────────────────────────────────────────────────
    // Member list formatting
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a list of member declarations with correct spacing and indentation.</summary>
    private static SyntaxList<MemberDeclarationSyntax> FormatMemberList(
        SyntaxList<MemberDeclarationSyntax> members,
        int indent)
    {
        if (members.Count is 0)
            return members;

        var result = new List<MemberDeclarationSyntax>();

        for (var i = 0; i < members.Count; i++)
        {
            var orig = members[i];
            var fmt = (MemberDeclarationSyntax)FormatNode(orig, indent);

            if (i > 0)
            {
                var prev = members[i - 1];
                var needsOneBlank = NeedsOneBlankLine(prev, orig);
                // The previous member already has a trailing \n.
                // So for 1 blank line we need 1 more \n in the leading trivia.
                var minLeadingBreaks = needsOneBlank ? 1 : 0;

                var leading = fmt.GetLeadingTrivia();
                // Count existing leading line breaks (not including prev member's trailing)
                var existingBreaks = CountLineBreaks(leading);

                if (existingBreaks < minLeadingBreaks)
                    fmt = fmt.WithLeadingTrivia(EnsureLeadingBreaks(leading, minLeadingBreaks, indent));

                else
                    fmt = fmt.WithLeadingTrivia(RebuildLeading(leading, indent));
            }
            else
            {
                fmt = fmt.WithLeadingTrivia(RebuildLeading(fmt.GetLeadingTrivia(), indent));
            }

            var trailingComments = StripWhitespace(fmt.GetTrailingTrivia());
            var trailingList = new List<SyntaxTrivia>();

            foreach (var c in trailingComments)
            {
                trailingList.Add(s_space);
                trailingList.Add(c);
            }

            trailingList.Add(s_lineFeed);
            fmt = fmt.WithTrailingTrivia(new SyntaxTriviaList(trailingList));
            result.Add(fmt);
        }

        return SyntaxFactory.List(result);
    }

    /// <summary>Determines if one blank line is needed between two adjacent members.</summary>
    private static bool NeedsOneBlankLine(MemberDeclarationSyntax prev, MemberDeclarationSyntax current)
    {
        if (current is BaseTypeDeclarationSyntax or MethodDeclarationSyntax or
            ConstructorDeclarationSyntax or PropertyDeclarationSyntax or
            EventDeclarationSyntax or IndexerDeclarationSyntax or
            OperatorDeclarationSyntax or ConversionOperatorDeclarationSyntax or
            DelegateDeclarationSyntax or EnumDeclarationSyntax or DestructorDeclarationSyntax or
            FieldDeclarationSyntax)
            return true;

        if (prev is BaseTypeDeclarationSyntax or MethodDeclarationSyntax or
            ConstructorDeclarationSyntax or PropertyDeclarationSyntax or
            EventDeclarationSyntax or IndexerDeclarationSyntax or
            OperatorDeclarationSyntax or ConversionOperatorDeclarationSyntax or
            DelegateDeclarationSyntax or EnumDeclarationSyntax or DestructorDeclarationSyntax or
            FieldDeclarationSyntax)
            return true;
        // Global statements: check if either spans multiple lines
        if (prev is GlobalStatementSyntax && SpansMultipleLines(prev))
            return true;

        if (current is GlobalStatementSyntax && SpansMultipleLines(current))
            return true;

        return false;
    }

    // ──────────────────────────────────────────────────────────
    // Namespace
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a file-scoped namespace declaration and its members.</summary>
    private static FileScopedNamespaceDeclarationSyntax FormatFileScopedNamespace(FileScopedNamespaceDeclarationSyntax node, int indent)
    {
        var members = FormatMemberList(node.Members, indent);
        var usings = FormatUsingDirectives(node.Usings);

        // Ensure blank line between usings and first member (matches CompilationUnit behavior)
        if (usings.Count > 0 && members.Count > 0)
        {
            var first = members[0];
            var leading = first.GetLeadingTrivia();

            members =
                members.Replace(
                    first,
                    first.WithLeadingTrivia(
                        EnsureLeadingBreaks(leading, 2, indent)));
        }
        else if (members.Count > 0)
        {
            var first = members[0];
            var leading = first.GetLeadingTrivia();

            members =
                members.Replace(
                    first,
                    first.WithLeadingTrivia(
                        EnsureLeadingBreaks(leading, 1, indent)));
        }

        return node.WithUsings(usings).WithMembers(members);
    }

    /// <summary>Formats a braced namespace declaration and its members.</summary>
    private static NamespaceDeclarationSyntax FormatNamespaceDeclaration(NamespaceDeclarationSyntax node, int indent)
    {
        var members = FormatMemberList(node.Members, indent + 1);
        var usings = FormatUsingDirectives(node.Usings);
        var open = FormatOpenBrace(node.OpenBraceToken, indent);

        var close =
            node.CloseBraceToken
            .WithLeadingTrivia(Indent(indent)).WithTrailingTrivia();

        return
            node.WithUsings(usings).WithMembers(members)
            .WithOpenBraceToken(open).WithCloseBraceToken(close);
    }

    // ──────────────────────────────────────────────────────────
    // Type Declaration
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a class, struct, record, or interface declaration.</summary>
    private static TypeDeclarationSyntax FormatTypeDeclaration(TypeDeclarationSyntax node, int indent)
    {
        var members = FormatMemberList(node.Members, indent + 1);
        var open = FormatOpenBrace(node.OpenBraceToken, indent);
        // Preserve comments in close brace leading trivia at block content indent level
        var closeLeading = RebuildLeading(node.CloseBraceToken.LeadingTrivia, indent + 1);

        var closeList = closeLeading.ToList();

        if (closeList.Count > 0 && IsWhitespace(closeList[^1]))
            closeList[^1] = Indent(indent);

        var close = node.CloseBraceToken.WithLeadingTrivia(new SyntaxTriviaList(closeList)).WithTrailingTrivia();

        return
            node
            .WithKeyword(node.Keyword.WithTrailingTrivia(s_space))
            .WithIdentifier(node.Identifier.WithLeadingTrivia())
            .WithMembers(members).WithOpenBraceToken(open).WithCloseBraceToken(close);
    }

    /// <summary>Formats an enum declaration with correct brace placement.</summary>
    private static EnumDeclarationSyntax FormatEnumDeclaration(EnumDeclarationSyntax node, int indent)
    {
        var open = FormatOpenBrace(node.OpenBraceToken, indent);
        var close = node.CloseBraceToken.WithLeadingTrivia(Indent(indent)).WithTrailingTrivia();
        return node.WithOpenBraceToken(open).WithCloseBraceToken(close);
    }

    /// <summary>Formats an opening brace token with correct leading newline and indent.</summary>
    private static SyntaxToken FormatOpenBrace(SyntaxToken brace, int indent)
    {
        var prev = brace.GetPreviousToken();
        var prevHasNewline = prev.TrailingTrivia.Any(t => IsLineBreak(t));

        // Preserve any comments from the brace's original leading trivia
        var hasComments = brace.LeadingTrivia.Any(t =>
            !IsWhitespace(t) && !IsLineBreak(t));

        if (hasComments)
        {
            var leading = RebuildLeading(brace.LeadingTrivia, indent);
            // Ensure at least one newline before the indent
            if (leading.Count is 0 || !IsLineBreak(leading[0]))
            {
                if (!prevHasNewline)
                    leading = new SyntaxTriviaList(s_lineFeed).AddRange(leading);
            }

            return brace.WithLeadingTrivia(leading).WithTrailingTrivia(s_lineFeed);
        }

        return
            prevHasNewline
            ?
            brace.WithLeadingTrivia(Indent(indent)).WithTrailingTrivia(s_lineFeed)
            :
            brace.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia(s_lineFeed);
    }

    /// <summary>
    /// Ensures the block's open brace has a leading newline.
    /// FormatOpenBrace relies on GetPreviousToken() which sees the original tree,
    /// but control-flow formatters (if/while/for/foreach) later strip the close-paren
    /// trailing trivia. This helper patches the brace's leading trivia when needed.
    /// </summary>
    private static BlockSyntax EnsureBraceNewline(BlockSyntax block, int indent)
    {
        var brace = block.OpenBraceToken;

        if (brace.LeadingTrivia.Any(t => IsLineBreak(t)))
            return block;

        return
            block.WithOpenBraceToken(
                brace.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia(s_lineFeed));
    }

    // ──────────────────────────────────────────────────────────
    // Method/Constructor/Property/Field
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a method declaration, including body, expression body, and parameters.</summary>
    private static MethodDeclarationSyntax FormatMethodDeclaration(MethodDeclarationSyntax node, int indent)
    {
        var r = node;

        if (r.Body is not null)
            r = r.WithBody(FormatBlock(r.Body, indent));

        if (r.ExpressionBody is not null)
            r = r.WithExpressionBody(FormatArrowExpressionClause(r.ExpressionBody, indent));

        r = r.WithParameterList((ParameterListSyntax)FormatParameterList(r.ParameterList, indent));
        return r;
    }

    /// <summary>Formats a constructor declaration, including body and parameters.</summary>
    private static ConstructorDeclarationSyntax FormatConstructorDeclaration(ConstructorDeclarationSyntax node, int indent)
    {
        var r = node;

        if (r.Body is not null)
            r = r.WithBody(FormatBlock(r.Body, indent));

        r = r.WithParameterList((ParameterListSyntax)FormatParameterList(r.ParameterList, indent));
        return r;
    }

    /// <summary>Formats a property declaration, including expression body and initializer.</summary>
    private static PropertyDeclarationSyntax FormatPropertyDeclaration(PropertyDeclarationSyntax node, int indent)
    {
        var r = node;

        if (r.ExpressionBody is not null)
            r = r.WithExpressionBody(FormatArrowExpressionClause(r.ExpressionBody, indent));

        if (r.Initializer is not null)
            r = r.WithInitializer(FormatEqualsValueClause(r.Initializer, indent));

        return r;
    }

    /// <summary>Formats a field declaration, including its initializer if present.</summary>
    private static FieldDeclarationSyntax FormatFieldDeclaration(FieldDeclarationSyntax node, int indent)
    {
        var decl = node.Declaration;

        if (decl.Variables.Count > 0 && decl.Variables[0].Initializer is { } init)
        {
            var fmtInit = FormatEqualsValueClause(init, indent);

            var newVar =
                decl.Variables[0]
                .WithIdentifier(decl.Variables[0].Identifier.WithTrailingTrivia(s_space))
                .WithInitializer(fmtInit);

            decl = decl.WithVariables(decl.Variables.Replace(decl.Variables[0], newVar));
        }
        // Ensure space after type
        decl = decl.WithType(decl.Type.WithTrailingTrivia(s_space));

        return node.WithDeclaration(decl);
    }

    /// <summary>Formats a local function statement, including body and parameters.</summary>
    private static LocalFunctionStatementSyntax FormatLocalFunctionStatement(LocalFunctionStatementSyntax node, int indent)
    {
        var r = node;

        if (r.Body is not null)
            r = r.WithBody(FormatBlock(r.Body, indent));

        if (r.ExpressionBody is not null)
            r = r.WithExpressionBody(FormatArrowExpressionClause(r.ExpressionBody, indent));

        r = r.WithParameterList((ParameterListSyntax)FormatParameterList(r.ParameterList, indent));
        return r;
    }

    // ──────────────────────────────────────────────────────────
    // Block and statement list
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a block with correct brace placement and statement indentation.</summary>
    private static BlockSyntax FormatBlock(BlockSyntax node, int indent)
    {
        // Preserve completely empty single-line blocks like { } 
        if (IsSingleLineBlock(node) && node.Statements.Count == 0)
        {
            return node;
        }

        var stmts = FormatStatementList(node.Statements, indent + 1);
        var open = FormatOpenBrace(node.OpenBraceToken, indent);
        // Preserve comments in close brace leading trivia at block content indent level
        var closeLeading = RebuildLeading(node.CloseBraceToken.LeadingTrivia, indent + 1);
        // If the last trivia item is whitespace (indent), replace with block-level indent
        var closeList = closeLeading.ToList();

        if (closeList.Count > 0 && IsWhitespace(closeList[^1]))
        {
            closeList[^1] = Indent(indent);
        }

        var close = node.CloseBraceToken.WithLeadingTrivia(new SyntaxTriviaList(closeList)).WithTrailingTrivia();
        return node.WithOpenBraceToken(open).WithCloseBraceToken(close).WithStatements(stmts);
    }

    /// <summary>Formats a list of statements with correct indentation and blank-line separation.</summary>
    private static SyntaxList<StatementSyntax> FormatStatementList(SyntaxList<StatementSyntax> stmts, int indent)
    {
        if (stmts.Count is 0)
            return stmts;

        var result = new List<StatementSyntax>();

        for (var i = 0; i < stmts.Count; i++)
        {
            var orig = stmts[i];
            var fmt = (StatementSyntax)FormatNode(orig, indent);

            // Preserve trailing comments
            var trailingComments = StripWhitespace(fmt.GetTrailingTrivia());

            var trailingList = new List<SyntaxTrivia>();

            foreach (var c in trailingComments)
            {
                trailingList.Add(s_space);
                trailingList.Add(c);
            }

            trailingList.Add(s_lineFeed);
            fmt = fmt.WithTrailingTrivia(new SyntaxTriviaList(trailingList));

            // Handle leading trivia: rebuild with correct indent and blank line
            if (i > 0)
            {
                var needsBlank =
                    NeedBlankBetweenStatements(stmts[i - 1], orig)
                    // Also check formatted versions: formatting may break a single-line
                    // statement to multi-line (e.g. due to line-length), which requires
                    // blank line separation even though the original was single-line.
                    // Use Trim() to exclude leading/trailing trivia newlines.
                    || SpansMultipleLines(result[i - 1].ToFullString().Trim())
                    || SpansMultipleLines(fmt.ToFullString().Trim());

                if (needsBlank)
                {
                    // Add blank line in leading trivia (prev stmt already has trailing \n)
                    var leading = fmt.GetLeadingTrivia();

                    var existingBreaks = CountLineBreaks(leading);

                    if (existingBreaks < 1)
                        fmt = fmt.WithLeadingTrivia(EnsureLeadingBreaks(leading, 1, indent));

                    else
                        fmt = fmt.WithLeadingTrivia(RebuildLeading(leading, indent));
                }
                else
                {
                    fmt = fmt.WithLeadingTrivia(RebuildLeading(fmt.GetLeadingTrivia(), indent));
                }
            }
            else
            {
                fmt = fmt.WithLeadingTrivia(RebuildLeading(fmt.GetLeadingTrivia(), indent));
            }

            result.Add(fmt);
        }

        return SyntaxFactory.List(result);
    }

    /// <summary>Determines if a blank line is needed between two adjacent statements.</summary>
    private static bool NeedBlankBetweenStatements(StatementSyntax current, StatementSyntax next)
    {
        // Multi-line statements need blank line separation
        if (SpansMultipleLines(current) || SpansMultipleLines(next))
            return true;
        // Preserve existing blank lines
        var nextBreaks = CountLineBreaks(next.GetLeadingTrivia());

        if (nextBreaks >= 2)
            return true;

        return false;
    }

    // ──────────────────────────────────────────────────────────
    // Control flow
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats an if statement, including condition, body, and else clause.</summary>
    private static IfStatementSyntax FormatIfStatement(IfStatementSyntax node, int indent)
    {
        var cond = (ExpressionSyntax)FormatNode(node.Condition, indent);
        StatementSyntax body;

        if (node.Statement is BlockSyntax block)
            body = EnsureBraceNewline(FormatBlock(block, indent), indent);

        else
        {
            body = (StatementSyntax)FormatNode(node.Statement, indent + 1);

            // If the formatted body spans multiple lines, wrap it in braces.
            // Use ToString() to check the statement text without leading trivia (comments).
            var bodyText = body.ToString().Trim();

            if (SpansMultipleLines(bodyText))
            {
                body = body.WithLeadingTrivia().WithTrailingTrivia();
                var wrappedBlock = SyntaxFactory.Block(SyntaxFactory.SingletonList(body));
                body = EnsureBraceNewline(FormatBlock(wrappedBlock, indent), indent);
            }
            else
            {
                body = body.WithLeadingTrivia(s_lineFeed, Indent(indent + 1));
            }
        }

        var r =
            node
            .WithIfKeyword(node.IfKeyword.WithTrailingTrivia(s_space))
            .WithCondition(cond)
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(
                node.CloseParenToken.WithLeadingTrivia()
                .WithTrailingTrivia(StripWhitespace(node.CloseParenToken.TrailingTrivia)))
            .WithStatement(body);

        if (node.Else is not null)
            r = r.WithElse(FormatElseClause(node.Else, indent));

        return r;
    }

    /// <summary>Formats an else clause, including else-if chains.</summary>
    private static ElseClauseSyntax FormatElseClause(ElseClauseSyntax node, int indent)
    {
        StatementSyntax body;

        if (node.Statement is IfStatementSyntax elseIf)
        {
            body = FormatIfStatement(elseIf, indent);
            body = body.WithLeadingTrivia(s_space);
        }
        else if (node.Statement is BlockSyntax block)
            body = EnsureBraceNewline(FormatBlock(block, indent), indent);

        else
        {
            body = (StatementSyntax)FormatNode(node.Statement, indent + 1);

            // If the formatted body spans multiple lines, wrap it in braces.
            // Use ToString() to check the statement text without leading trivia (comments).
            var bodyText = body.ToString().Trim();

            if (SpansMultipleLines(bodyText))
            {
                body = body.WithLeadingTrivia().WithTrailingTrivia();
                var wrappedBlock = SyntaxFactory.Block(SyntaxFactory.SingletonList(body));
                body = EnsureBraceNewline(FormatBlock(wrappedBlock, indent), indent);
            }
            else
            {
                body =
                    body.WithLeadingTrivia(
                        EnsureLeadingBreaks(node.Statement.GetLeadingTrivia(), 1, indent + 1));
            }
        }

        var kw =
            node.ElseKeyword
            .WithLeadingTrivia(EnsureLeadingBreaks(node.ElseKeyword.LeadingTrivia, 1, indent))
            .WithTrailingTrivia();

        return node.WithElseKeyword(kw).WithStatement(body);
    }

    /// <summary>Formats a while statement with correct keyword and brace placement.</summary>
    private static WhileStatementSyntax FormatWhileStatement(WhileStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            (StatementSyntax)EnsureBraceNewline(FormatBlock(block, indent), indent)
            :
            ((StatementSyntax)FormatNode(node.Statement, indent + 1)).WithLeadingTrivia(s_lineFeed, Indent(indent + 1));

        var cond = (ExpressionSyntax)FormatNode(node.Condition, indent);

        return
            node
            .WithWhileKeyword(node.WhileKeyword.WithTrailingTrivia(s_space))
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCondition(cond.WithLeadingTrivia(StripWhitespace(node.Condition.GetLeadingTrivia())))
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithStatement(body);
    }

    /// <summary>Formats a for statement with correct keyword and brace placement.</summary>
    private static ForStatementSyntax FormatForStatement(ForStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            (StatementSyntax)EnsureBraceNewline(FormatBlock(block, indent), indent)
            :
            ((StatementSyntax)FormatNode(node.Statement, indent + 1)).WithLeadingTrivia(s_lineFeed, Indent(indent + 1));

        return
            node
            .WithForKeyword(node.ForKeyword.WithTrailingTrivia(s_space))
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithStatement(body);
    }

    /// <summary>Formats a foreach statement with correct keyword and brace placement.</summary>
    private static ForEachStatementSyntax FormatForEachStatement(ForEachStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            (StatementSyntax)EnsureBraceNewline(FormatBlock(block, indent), indent)
            :
            ((StatementSyntax)FormatNode(node.Statement, indent + 1)).WithLeadingTrivia(s_lineFeed, Indent(indent + 1));

        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);

        return
            node
            .WithForEachKeyword(node.ForEachKeyword.WithTrailingTrivia(s_space))
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithType(node.Type.WithLeadingTrivia())
            .WithIdentifier(node.Identifier.WithTrailingTrivia(s_space))
            .WithInKeyword(node.InKeyword.WithLeadingTrivia().WithTrailingTrivia(s_space))
            .WithExpression(fmtExpr.WithLeadingTrivia(StripWhitespace(node.Expression.GetLeadingTrivia())))
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithStatement(body);
    }

    /// <summary>Formats a foreach variable statement (deconstruction pattern).</summary>
    private static ForEachVariableStatementSyntax FormatForEachVariableStatement(ForEachVariableStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            (StatementSyntax)EnsureBraceNewline(FormatBlock(block, indent), indent)
            :
            ((StatementSyntax)FormatNode(node.Statement, indent + 1)).WithLeadingTrivia(s_lineFeed, Indent(indent + 1));

        return
            node
            .WithForEachKeyword(node.ForEachKeyword.WithTrailingTrivia(s_space))
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithStatement(body);
    }

    /// <summary>Formats a do-while statement with correct keyword placement.</summary>
    private static DoStatementSyntax FormatDoStatement(DoStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            FormatBlock(block, indent)
            :
            (StatementSyntax)FormatNode(node.Statement, indent + 1);

        return
            node
            .WithDoKeyword(node.DoKeyword.WithTrailingTrivia())
            .WithStatement(body)
            .WithWhileKeyword(
                node.WhileKeyword
                    .WithLeadingTrivia(s_lineFeed, Indent(indent))
                    .WithTrailingTrivia(s_space));
    }

    /// <summary>Formats a using statement with correct keyword and body placement.</summary>
    private static UsingStatementSyntax FormatUsingStatement(UsingStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            FormatBlock(block, indent)
            :
            ((StatementSyntax)FormatNode(node.Statement, indent + 1)).WithLeadingTrivia(s_lineFeed, Indent(indent + 1));

        return node.WithUsingKeyword(node.UsingKeyword.WithTrailingTrivia(s_space)).WithStatement(body);
    }

    /// <summary>Formats a lock statement with correct keyword and body placement.</summary>
    private static LockStatementSyntax FormatLockStatement(LockStatementSyntax node, int indent)
    {
        var body =
            node.Statement is BlockSyntax block
            ?
            FormatBlock(block, indent)
            :
            (StatementSyntax)FormatNode(node.Statement, indent + 1);

        return node.WithLockKeyword(node.LockKeyword.WithTrailingTrivia(s_space)).WithStatement(body);
    }

    // ──────────────────────────────────────────────────────────
    // Switch statement
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a switch statement with sections, braces, and indentation.</summary>
    private static SwitchStatementSyntax FormatSwitchStatement(SwitchStatementSyntax node, int indent)
    {
        var secIndent = indent + 1;
        var sections = new List<SwitchSectionSyntax>();

        for (var i = 0; i < node.Sections.Count; i++)
        {
            var sec = FormatSwitchSection(node.Sections[i], secIndent);

            if (i > 0)
            {
                var leading = sec.GetLeadingTrivia();
                // Prev section's last stmt has trailing \n, so 1 more = blank line
                if (CountLineBreaks(leading) < 1)
                    sec = sec.WithLeadingTrivia(EnsureLeadingBreaks(leading, 1, secIndent));

                else
                    sec = sec.WithLeadingTrivia(RebuildLeading(leading, secIndent));
            }
            else
                sec = sec.WithLeadingTrivia(RebuildLeading(sec.GetLeadingTrivia(), secIndent));

            sections.Add(sec);
        }

        var open = node.OpenBraceToken.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia(s_lineFeed);
        // Preserve comments in close brace leading trivia
        var closeLeading = RebuildLeading(node.CloseBraceToken.LeadingTrivia, secIndent);

        var closeList = closeLeading.ToList();

        if (closeList.Count > 0 && IsWhitespace(closeList[^1]))
            closeList[^1] = Indent(indent);

        var close = node.CloseBraceToken.WithLeadingTrivia(new SyntaxTriviaList(closeList)).WithTrailingTrivia();

        return
            node
            .WithSwitchKeyword(node.SwitchKeyword.WithTrailingTrivia(s_space))
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithOpenBraceToken(open).WithCloseBraceToken(close)
            .WithSections(SyntaxFactory.List(sections));
    }

    /// <summary>Formats a switch section with its labels and statements.</summary>
    private static SwitchSectionSyntax FormatSwitchSection(SwitchSectionSyntax node, int indent)
    {
        var stmtIndent = indent + 1;
        var labels = new List<SwitchLabelSyntax>();

        for (var li = 0; li < node.Labels.Count; li++)
        {
            var label = node.Labels[li];

            // For labels after the first, ensure they start on a new line
            if (li > 0)
            {
                var origLeading = label.GetLeadingTrivia();
                label = label.WithLeadingTrivia(EnsureLeadingBreaks(origLeading, 1, indent));
            }

            if (label is CasePatternSwitchLabelSyntax cp)
            {
                var fmtCp =
                    cp.WithKeyword(cp.Keyword.WithTrailingTrivia(s_space))
                    .WithColonToken(
                        cp.ColonToken.WithLeadingTrivia()
                        .WithTrailingTrivia(StripWhitespace(cp.ColonToken.TrailingTrivia)));
                // Normalize when clause whitespace
                if (fmtCp.WhenClause is { } wc)
                {
                    var fmtCond = (ExpressionSyntax)FormatNode(wc.Condition, indent);

                    fmtCp =
                        fmtCp.WithPattern(fmtCp.Pattern.WithTrailingTrivia(s_space))
                        .WithWhenClause(
                            wc
                            .WithWhenKeyword(wc.WhenKeyword.WithLeadingTrivia().WithTrailingTrivia(s_space))
                            .WithCondition(fmtCond.WithLeadingTrivia().WithTrailingTrivia()));
                }

                labels.Add(fmtCp);
            }
            else if (label is CaseSwitchLabelSyntax cs)
            {
                labels.Add(
                    cs.WithKeyword(cs.Keyword.WithTrailingTrivia(s_space))
                    .WithColonToken(
                        cs.ColonToken.WithLeadingTrivia()
                        .WithTrailingTrivia(StripWhitespace(cs.ColonToken.TrailingTrivia))));
            }
            else if (label is DefaultSwitchLabelSyntax ds)
            {
                labels.Add(
                    ds.WithKeyword(ds.Keyword.WithTrailingTrivia())
                    .WithColonToken(
                        ds.ColonToken.WithLeadingTrivia()
                        .WithTrailingTrivia(StripWhitespace(ds.ColonToken.TrailingTrivia))));
            }
            else
                labels.Add(label);
        }

        var stmts = FormatStatementList(node.Statements, stmtIndent);
        // Ensure first statement has a line break at the start (label colon has no trailing \n)
        if (stmts.Count > 0)
        {
            var first = stmts[0];
            var leading = first.GetLeadingTrivia();
            // Check if leading trivia starts with a line break
            if (leading.Count is 0 || !IsLineBreak(leading[0]))
            {
                var newLeading = new SyntaxTriviaList(s_lineFeed).AddRange(leading);
                stmts = stmts.Replace(first, first.WithLeadingTrivia(newLeading));
            }
        }

        return node.WithLabels(SyntaxFactory.List(labels)).WithStatements(stmts);
    }

    // ──────────────────────────────────────────────────────────
    // Try/Catch/Finally
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a try statement, including catch and finally clauses.</summary>
    private static TryStatementSyntax FormatTryStatement(TryStatementSyntax node, int indent)
    {
        var block = FormatBlock(node.Block, indent);
        var catches = node.Catches.Select(c => FormatCatchClause(c, indent)).ToList();
        var r = node.WithBlock(block).WithCatches(SyntaxFactory.List(catches));

        if (node.Finally is not null)
            r = r.WithFinally(FormatFinallyClause(node.Finally, indent));

        return r;
    }

    /// <summary>Formats a catch clause with keyword placement and block formatting.</summary>
    private static CatchClauseSyntax FormatCatchClause(CatchClauseSyntax node, int indent)
    {
        var block = FormatBlock(node.Block, indent);
        var kw = node.CatchKeyword.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia();
        var r = node.WithCatchKeyword(kw).WithBlock(block);

        if (node.Declaration is not null)
        {
            r =
                r.WithDeclaration(
                    node.Declaration.WithOpenParenToken(
                        node.Declaration.OpenParenToken.WithLeadingTrivia(s_space)));
        }

        return r;
    }

    /// <summary>Formats a finally clause with keyword placement and block formatting.</summary>
    private static FinallyClauseSyntax FormatFinallyClause(FinallyClauseSyntax node, int indent)
    {
        var block = FormatBlock(node.Block, indent);
        var kw = node.FinallyKeyword.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia();
        return node.WithFinallyKeyword(kw).WithBlock(block);
    }

    // ──────────────────────────────────────────────────────────
    // Statement formatting
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a local variable declaration statement with its initializer.</summary>
    private static LocalDeclarationStatementSyntax FormatLocalDeclarationStatement(LocalDeclarationStatementSyntax node, int indent)
    {
        if (node.Declaration.Variables.Count > 0 && node.Declaration.Variables[0].Initializer is { } init)
        {
            var fmtInit = FormatEqualsValueClause(init, indent);
            var newVar = node.Declaration.Variables[0].WithInitializer(fmtInit);

            return
                node.WithDeclaration(
                    node.Declaration.WithVariables(
                        node.Declaration.Variables.Replace(node.Declaration.Variables[0], newVar)));
        }

        return node;
    }

    /// <summary>Formats an expression statement by formatting its inner expression.</summary>
    private static ExpressionStatementSyntax FormatExpressionStatement(ExpressionStatementSyntax node, int indent)
    {
        return node.WithExpression((ExpressionSyntax)FormatNode(node.Expression, indent));
    }

    /// <summary>Formats a return statement, handling multi-line expression placement.</summary>
    private static ReturnStatementSyntax FormatReturnStatement(ReturnStatementSyntax node, int indent)
    {
        if (node.Expression is null)
            return node;

        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent + 1);
        // Strip leading whitespace from expression
        fmtExpr = fmtExpr.WithLeadingTrivia(StripWhitespace(fmtExpr.GetLeadingTrivia()));

        var exprText = fmtExpr.ToFullString().Trim();

        // Check if expression should be on new line:
        // 1) Expression text spans multiple lines
        // 2) Original had a line break between return keyword and expression
        var originalOnNewLine = LineOf(node.Expression) > LineOf(node.ReturnKeyword);

        var isMultiLine = SpansMultipleLines(exprText);

        if (isMultiLine || originalOnNewLine)
        {
            fmtExpr =
                fmtExpr.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Expression.GetLeadingTrivia(), 1, indent + 1));

            return node.WithReturnKeyword(node.ReturnKeyword.WithTrailingTrivia()).WithExpression(fmtExpr);
        }

        return
            node.WithReturnKeyword(node.ReturnKeyword.WithTrailingTrivia(s_space))
            .WithExpression(fmtExpr.WithLeadingTrivia());
    }

    /// <summary>Formats a throw statement with correct keyword spacing.</summary>
    private static ThrowStatementSyntax FormatThrowStatement(ThrowStatementSyntax node, int indent)
    {
        if (node.Expression is null)
            return node;

        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);
        return node.WithThrowKeyword(node.ThrowKeyword.WithTrailingTrivia(s_space)).WithExpression(fmtExpr);
    }

    /// <summary>Formats a throw expression with correct keyword spacing.</summary>
    private static ThrowExpressionSyntax FormatThrowExpression(ThrowExpressionSyntax node, int indent)
    {
        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);
        return node.WithThrowKeyword(node.ThrowKeyword.WithTrailingTrivia(s_space)).WithExpression(fmtExpr);
    }

    /// <summary>Formats a global (top-level) statement by formatting its inner statement.</summary>
    private static GlobalStatementSyntax FormatGlobalStatement(GlobalStatementSyntax node, int indent)
    {
        return node.WithStatement((StatementSyntax)FormatNode(node.Statement, indent));
    }

    // ──────────────────────────────────────────────────────────
    // Argument Lists
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats an argument list, choosing single-line or multi-line layout.</summary>
    private static ArgumentListSyntax FormatArgumentList(ArgumentListSyntax node, int indent)
    {
        if (node.Arguments.Count is 0)
            return node;

        if (ShouldArgListBeMultiLine(node))
            return FormatArgListMultiLine(node, indent);

        return FormatArgListSingleLine(node);
    }

    /// <summary>Determines if an argument list should use multi-line layout.</summary>
    private static bool ShouldArgListBeMultiLine(ArgumentListSyntax node)
    {
        if (node.Arguments.Count is 0)
            return false;

        // Line length check — only break if the arg list itself is wide enough
        // to be the cause of the overshoot. Short arg lists should let the parent
        // context (equals clause, return statement, etc.) handle the breaking.
        var lineLen = LineEndColumn(node);

        if (lineLen > MaximumLineLength)
        {
            if (node.Arguments.Count is 1)
            {
                // For single-arg lists, only break if the arg contains complex content
                var argExpr = node.Arguments[0].Expression;

                if (argExpr is CollectionExpressionSyntax or InitializerExpressionSyntax or
                    InvocationExpressionSyntax or ObjectCreationExpressionSyntax)
                    return true;
            }
            else
            {
                // Estimate single-line arg width
                var singleLineWidth = 2; // parens

                for (var i = 0; i < node.Arguments.Count; i++)
                {
                    if (i > 0)
                        singleLineWidth += 2; // ", "

                    singleLineWidth += node.Arguments[i].ToString().Trim().Length;
                }
                // Only break if args themselves are wide enough to justify breaking.
                // Using 2/3 of MaximumLineLength as threshold leaves ~40 chars for the
                // method name + indent, preventing cascading breaks where the parent
                // context (equals clause, return statement, etc.) should break instead.
                if (singleLineWidth > MaximumLineLength * 2 / 3)
                    return true;
            }
        }

        var openLine = LineOf(node.OpenParenToken);
        var closeLine = LineOf(node.CloseParenToken);

        if (openLine != closeLine)
        {
            // Lambda with block body — always multi-line
            foreach (var arg in node.Arguments)
            {
                if (arg.Expression is ParenthesizedLambdaExpressionSyntax { Block: not null })
                    return true;

                if (arg.Expression is SimpleLambdaExpressionSyntax { Block: not null })
                    return true;
            }

            // Any arg multi-line
            foreach (var arg in node.Arguments)
                if (SpansMultipleLines(arg))
                    return true;

            // First arg on different line
            if (LineOf(node.Arguments[0]) != openLine)
                return true;
            // Any arg on different line than first
            for (var i = 1; i < node.Arguments.Count; i++)
                if (LineOf(node.Arguments[i]) != LineOf(node.Arguments[0]))
                    return true;
        }

        return false;
    }

    /// <summary>Formats an argument list on a single line with normalized spacing.</summary>
    private static ArgumentListSyntax FormatArgListSingleLine(ArgumentListSyntax node)
    {
        var args = new List<SyntaxNode>();

        for (var i = 0; i < node.Arguments.Count; i++)
        {
            var a = node.Arguments[i];
            a = i is 0 ? a.WithLeadingTrivia() : a.WithLeadingTrivia(s_space);
            a = a.WithTrailingTrivia(StripWhitespace(a.GetTrailingTrivia()));
            args.Add(a);
        }

        var seps =
            RebuildSeparators(
                [.. node.Arguments.GetSeparators()],
                node.Arguments.Count - 1);

        return
            node
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia())
            .WithArguments(SyntaxFactory.SeparatedList(args.Cast<ArgumentSyntax>(), seps));
    }

    /// <summary>Formats an argument list across multiple lines, one argument per line.</summary>
    private static ArgumentListSyntax FormatArgListMultiLine(ArgumentListSyntax node, int indent)
    {
        var argIndent = indent + 1;
        var args = new List<SyntaxNode>();

        for (var i = 0; i < node.Arguments.Count; i++)
        {
            var a = node.Arguments[i];
            var origLeading = a.GetLeadingTrivia();
            var fmtExpr = (ExpressionSyntax)FormatNode(a.Expression, argIndent);
            a = a.WithExpression(fmtExpr);
            a = a.WithLeadingTrivia(EnsureLeadingBreaks(origLeading, 1, argIndent));
            a = a.WithTrailingTrivia(StripWhitespace(a.GetTrailingTrivia()));

            // For named arguments, if the first line would exceed max length,
            // break after the colon (outer-first principle within the argument).
            if (a is ArgumentSyntax argSyntax && argSyntax.NameColon is { } nameColon)
            {
                var argText = a.ToFullString().Trim();
                var firstLine = argText.Split('\n')[0];
                var lineLen = argIndent * IndentSize + firstLine.TrimEnd().Length;

                if (lineLen > MaximumLineLength)
                {
                    var nc = nameColon.WithTrailingTrivia();
                    var expr = argSyntax.Expression.WithLeadingTrivia(s_lineFeed, Indent(argIndent));
                    a = argSyntax.WithNameColon(nc).WithExpression(expr);
                    a = a.WithLeadingTrivia(s_lineFeed, Indent(argIndent));
                }
            }

            args.Add(a);
        }

        var seps =
            RebuildSeparators(
                [.. node.Arguments.GetSeparators()],
                node.Arguments.Count - 1);

        return
            node
            .WithOpenParenToken(node.OpenParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia().WithTrailingTrivia())
            .WithArguments(SyntaxFactory.SeparatedList(args.Cast<ArgumentSyntax>(), seps));
    }

    // ──────────────────────────────────────────────────────────
    // Parameter Lists
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a parameter list, choosing single-line or multi-line layout.</summary>
    private static ParameterListSyntax FormatParameterList(ParameterListSyntax node, int indent)
    {
        if (node.Parameters.Count is 0)
            return node;

        if (ShouldParamListBeMultiLine(node))
            return FormatParamListMultiLine(node, indent);

        return FormatParamListSingleLine(node);
    }

    /// <summary>Determines if a parameter list should use multi-line layout.</summary>
    private static bool ShouldParamListBeMultiLine(ParameterListSyntax node)
    {
        if (node.Parameters.Count is 0)
            return false;

        var openLine = LineOf(node.OpenParenToken);
        var closeLine = LineOf(node.CloseParenToken);

        if (openLine != closeLine)
        {
            if (LineOf(node.Parameters[0]) != openLine)
                return true;

            for (var i = 1; i < node.Parameters.Count; i++)
                if (LineOf(node.Parameters[i]) != LineOf(node.Parameters[0]))
                    return true;
        }

        return false;
    }

    /// <summary>Formats a parameter list on a single line with normalized spacing.</summary>
    private static ParameterListSyntax FormatParamListSingleLine(ParameterListSyntax node)
    {
        var ps = new List<SyntaxNode>();

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var p = node.Parameters[i];
            p = i is 0 ? p.WithLeadingTrivia() : p.WithLeadingTrivia(s_space);

            if (p.Type is not null)
                p = p.WithType(p.Type.WithTrailingTrivia(s_space));

            p = p.WithTrailingTrivia();
            ps.Add(p);
        }

        var seps =
            Enumerable.Range(0, node.Parameters.Count - 1)
            .Select(_ => SyntaxFactory.Token(SyntaxKind.CommaToken).WithLeadingTrivia().WithTrailingTrivia()).ToList();

        return
            node
            .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia())
            .WithParameters(SyntaxFactory.SeparatedList(ps.Cast<ParameterSyntax>(), seps));
    }

    /// <summary>Formats a parameter list across multiple lines, one parameter per line.</summary>
    private static ParameterListSyntax FormatParamListMultiLine(ParameterListSyntax node, int indent)
    {
        var pIndent = indent + 1;
        var ps = new List<SyntaxNode>();

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var p = node.Parameters[i];
            p = p.WithLeadingTrivia(s_lineFeed, Indent(pIndent));

            if (p.Type is not null)
                p = p.WithType(p.Type.WithTrailingTrivia(s_space));

            p = p.WithTrailingTrivia();
            ps.Add(p);
        }

        var seps =
            Enumerable.Range(0, node.Parameters.Count - 1)
            .Select(_ => SyntaxFactory.Token(SyntaxKind.CommaToken).WithLeadingTrivia().WithTrailingTrivia()).ToList();

        return
            node
            .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia())
            .WithParameters(SyntaxFactory.SeparatedList(ps.Cast<ParameterSyntax>(), seps));
    }

    // ──────────────────────────────────────────────────────────
    // Collection Expression
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a collection expression, choosing single-line or multi-line layout.</summary>
    private static CollectionExpressionSyntax FormatCollectionExpression(CollectionExpressionSyntax node, int indent)
    {
        if (node.Elements.Count is 0)
            return node;

        // Check line length — if single line exceeds max, switch to multi-line
        var shouldBeMultiLine = LineEndColumn(node) > MaximumLineLength;

        if (!shouldBeMultiLine)
        {
            var openLine = LineOf(node.OpenBracketToken);
            var closeLine = LineOf(node.CloseBracketToken);

            if (openLine != closeLine)
            {
                if (LineOf(node.Elements[0]) != openLine)
                    shouldBeMultiLine = true;

                for (var i = 1; i < node.Elements.Count && !shouldBeMultiLine; i++)
                    if (LineOf(node.Elements[i]) != LineOf(node.Elements[0]))
                        shouldBeMultiLine = true;

                foreach (var e in node.Elements)
                    if (SpansMultipleLines(e))
                        shouldBeMultiLine = true;
            }
        }

        if (shouldBeMultiLine)
            return FormatCollectionMultiLine(node, indent);

        return FormatCollectionSingleLine(node);
    }

    /// <summary>Formats a collection expression on a single line.</summary>
    private static CollectionExpressionSyntax FormatCollectionSingleLine(CollectionExpressionSyntax node)
    {
        var elems = new List<SyntaxNode>();

        for (var i = 0; i < node.Elements.Count; i++)
        {
            var e = node.Elements[i];
            e = i is 0 ? e.WithLeadingTrivia() : e.WithLeadingTrivia(s_space);
            e = e.WithTrailingTrivia();
            elems.Add(e);
        }

        var seps =
            RebuildSeparators(
                [.. node.Elements.GetSeparators()],
                node.Elements.Count - 1);

        return
            node
            .WithOpenBracketToken(node.OpenBracketToken.WithTrailingTrivia())
            .WithCloseBracketToken(node.CloseBracketToken.WithLeadingTrivia())
            .WithElements(SyntaxFactory.SeparatedList(elems.Cast<CollectionElementSyntax>(), seps));
    }

    /// <summary>Formats a collection expression across multiple lines, one element per line.</summary>
    private static CollectionExpressionSyntax FormatCollectionMultiLine(CollectionExpressionSyntax node, int indent)
    {
        var elems = new List<SyntaxNode>();

        for (var i = 0; i < node.Elements.Count; i++)
        {
            var e = node.Elements[i];
            var origLeading = e.GetLeadingTrivia();

            if (e is ExpressionElementSyntax ee)
                e = ee.WithExpression((ExpressionSyntax)FormatNode(ee.Expression, indent));

            e = e.WithLeadingTrivia(EnsureLeadingBreaks(origLeading, 1, indent));
            e = e.WithTrailingTrivia(StripWhitespace(node.Elements[i].GetTrailingTrivia()));
            elems.Add(e);
        }

        var origSeps = node.Elements.GetSeparators().ToList();
        var seps = RebuildSeparators(origSeps, node.Elements.Count - 1);

        if (origSeps.Count >= node.Elements.Count)
            seps.Add(SyntaxFactory.Token(SyntaxKind.CommaToken).WithLeadingTrivia().WithTrailingTrivia());

        var close = node.CloseBracketToken.WithLeadingTrivia(s_lineFeed, Indent(indent));

        return
            node
            .WithOpenBracketToken(node.OpenBracketToken.WithTrailingTrivia())
            .WithCloseBracketToken(close)
            .WithElements(SyntaxFactory.SeparatedList(elems.Cast<CollectionElementSyntax>(), seps));
    }

    // ──────────────────────────────────────────────────────────
    // Initializer Expression
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats an initializer expression with multi-line layout when needed.</summary>
    private static InitializerExpressionSyntax FormatInitializerExpression(InitializerExpressionSyntax node, int indent)
    {
        if (!SpansMultipleLines(node))
            return node;

        var ci = indent + 1;
        var exprs = new List<SyntaxNode>();

        for (var i = 0; i < node.Expressions.Count; i++)
        {
            var origLeading = node.Expressions[i].GetLeadingTrivia();
            var e = (ExpressionSyntax)FormatNode(node.Expressions[i], ci);
            e = e.WithLeadingTrivia(EnsureLeadingBreaks(origLeading, 1, ci));
            e = e.WithTrailingTrivia();
            exprs.Add(e);
        }

        var origSeps = node.Expressions.GetSeparators().ToList();
        var seps = RebuildSeparators(origSeps, node.Expressions.Count - 1);

        if (origSeps.Count >= node.Expressions.Count)
            seps.Add(SyntaxFactory.Token(SyntaxKind.CommaToken).WithLeadingTrivia().WithTrailingTrivia());
        // Use FormatOpenBrace to avoid double newline when previous token has trailing newline,
        // but strip the trailing linefeed since the first expression already has a leading one.
        var openBrace = FormatOpenBrace(node.OpenBraceToken, indent).WithTrailingTrivia();

        var close = node.CloseBraceToken.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia();

        return
            node.WithOpenBraceToken(openBrace).WithCloseBraceToken(close)
            .WithExpressions(SyntaxFactory.SeparatedList(exprs.Cast<ExpressionSyntax>(), seps));
    }

    // ──────────────────────────────────────────────────────────
    // Expression formatting
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats an arrow (=&gt;) expression clause, breaking to a new line when needed.</summary>
    private static ArrowExpressionClauseSyntax FormatArrowExpressionClause(ArrowExpressionClauseSyntax node, int indent)
    {
        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent + 1);
        var arrowLine = LineOf(node.ArrowToken);
        var exprLine = LineOf(node.Expression);

        if (arrowLine != exprLine)
        {
            fmtExpr =
                fmtExpr.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Expression.GetLeadingTrivia(), 1, indent + 1));

            return node.WithArrowToken(node.ArrowToken.WithTrailingTrivia()).WithExpression(fmtExpr);
        }

        if (LineEndColumn(node) > MaximumLineLength)
        {
            fmtExpr =
                fmtExpr.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Expression.GetLeadingTrivia(), 1, indent + 1));

            return node.WithArrowToken(node.ArrowToken.WithTrailingTrivia()).WithExpression(fmtExpr);
        }

        return node.WithExpression(fmtExpr);
    }

    /// <summary>Formats an equals-value clause, breaking to a new line when needed.</summary>
    private static EqualsValueClauseSyntax FormatEqualsValueClause(EqualsValueClauseSyntax node, int indent)
    {
        var originalHasBreak = LineOf(node.Value) > LineOf(node.EqualsToken);

        // Check line length on the original node
        var exceedsLine = LineEndColumn(node) > MaximumLineLength;

        // Outer-first line breaking: if the line exceeds max and the value is still
        // on the same line as '=', break at '=' first without formatting inner content.
        // The multi-pass convergence will handle inner formatting in the next pass,
        // where the value will be on a shorter line.
        if (exceedsLine && !originalHasBreak)
        {
            var value =
                node.Value.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Value.GetLeadingTrivia(), 1, indent + 1));

            return node.WithEqualsToken(node.EqualsToken.WithTrailingTrivia()).WithValue(value);
        }

        var fmtValue = (ExpressionSyntax)FormatNode(node.Value, indent + 1);
        var valueText = fmtValue.ToFullString().Trim();
        var isMultiLine = SpansMultipleLines(valueText);

        if (isMultiLine || originalHasBreak)
        {
            fmtValue =
                fmtValue.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Value.GetLeadingTrivia(), 1, indent + 1));

            return node.WithEqualsToken(node.EqualsToken.WithTrailingTrivia()).WithValue(fmtValue);
        }

        return
            node.WithEqualsToken(
                node.EqualsToken
                    .WithTrailingTrivia(s_space))
            .WithValue(fmtValue.WithLeadingTrivia());
    }

    /// <summary>Formats a ternary conditional expression with multi-line layout when needed.</summary>
    private static ConditionalExpressionSyntax FormatConditionalExpression(ConditionalExpressionSyntax node, int indent)
    {
        var isMultiLine = SpansMultipleLines(node);

        if (!isMultiLine)
        {
            if (LineEndColumn(node) > MaximumLineLength)
                isMultiLine = true;
        }

        if (!isMultiLine)
            return node;

        var ci = Indent(indent);
        var fmtCond = (ExpressionSyntax)FormatNode(node.Condition, indent);
        var fmtTrue = (ExpressionSyntax)FormatNode(node.WhenTrue, indent);
        var fmtFalse = (ExpressionSyntax)FormatNode(node.WhenFalse, indent);

        return
            node
            .WithCondition(fmtCond.WithTrailingTrivia())
            .WithQuestionToken(node.QuestionToken.WithLeadingTrivia(s_lineFeed, ci).WithTrailingTrivia())
            .WithWhenTrue(
                fmtTrue.WithLeadingTrivia(s_lineFeed, ci)
                .WithTrailingTrivia(StripWhitespace(node.WhenTrue.GetTrailingTrivia())))
            .WithColonToken(
                node.ColonToken.WithLeadingTrivia(s_lineFeed, ci)
                .WithTrailingTrivia(StripWhitespace(node.ColonToken.TrailingTrivia)))
            .WithWhenFalse(fmtFalse.WithLeadingTrivia(s_lineFeed, ci));
    }

    /// <summary>Formats a switch expression with arms on separate lines.</summary>
    private static SwitchExpressionSyntax FormatSwitchExpression(SwitchExpressionSyntax node, int indent)
    {
        var armIndent = indent + 1;
        var origSeps = node.Arms.GetSeparators().ToList();
        var arms = new List<SyntaxNode>();

        for (var i = 0; i < node.Arms.Count; i++)
        {
            var arm = FormatSwitchExpressionArm(node.Arms[i], armIndent);

            var needsBlank =
                i > 0 &&
                (SpansMultipleLines(node.Arms[i - 1]) || SpansMultipleLines(node.Arms[i]) ||
                node.Arms[i].Pattern is
                DiscardPatternSyntax ||
                node.Arms[i - 1].Pattern is
                DiscardPatternSyntax);

            var minBreaks = needsBlank ? 2 : 1;
            // Preserve existing blank lines from original source:
            // A blank line between arms is represented by total line breaks >= 2
            // across the preceding separator's trailing trivia and the arm's leading trivia.
            if (i > 0)
            {
                var origBreaks = CountLineBreaks(node.Arms[i].GetLeadingTrivia());

                if (i - 1 < origSeps.Count)
                    origBreaks += CountLineBreaks(origSeps[i - 1].TrailingTrivia);

                if (origBreaks >= 2 && minBreaks < 2)
                    minBreaks = 2;
            }

            arm = arm.WithLeadingTrivia(EnsureLeadingBreaks(node.Arms[i].GetLeadingTrivia(), minBreaks, armIndent));
            arms.Add(arm);
        }

        var seps = RebuildSeparators(origSeps, node.Arms.Count - 1);
        // Preserve trailing comma if original had one
        if (origSeps.Count >= node.Arms.Count)
            seps.Add(SyntaxFactory.Token(SyntaxKind.CommaToken).WithLeadingTrivia().WithTrailingTrivia());

        var open = node.OpenBraceToken.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia();
        var close = node.CloseBraceToken.WithLeadingTrivia(s_lineFeed, Indent(indent)).WithTrailingTrivia();
        // Normalize switch keyword: exactly one space before, no trailing
        var sw = node.SwitchKeyword.WithLeadingTrivia(s_space).WithTrailingTrivia();
        // Normalize governing expression trailing trivia
        var govExpr = node.GoverningExpression.WithTrailingTrivia();

        return
            node.WithGoverningExpression(govExpr).WithSwitchKeyword(sw)
            .WithOpenBraceToken(open).WithCloseBraceToken(close)
            .WithArms(SyntaxFactory.SeparatedList(arms.Cast<SwitchExpressionArmSyntax>(), seps));
    }

    /// <summary>Formats a binary pattern (e.g. <c>int or string or double</c>) with consistent indentation.</summary>
    private static BinaryPatternSyntax FormatBinaryPattern(BinaryPatternSyntax node, int indent)
    {
        var fmtLeft = (PatternSyntax)FormatNode(node.Left, indent);
        fmtLeft = fmtLeft.WithTrailingTrivia();
        var fmtRight = (PatternSyntax)FormatNode(node.Right, indent);

        var opLine = LineOf(node.OperatorToken);
        var rightLine = LineOf(node.Right);

        if (rightLine > opLine)
        {
            fmtRight =
                fmtRight.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, indent));

            return
                node.WithLeft(fmtLeft)
                .WithOperatorToken(node.OperatorToken.WithLeadingTrivia(s_space).WithTrailingTrivia())
                .WithRight(fmtRight);
        }

        return
            node.WithLeft(fmtLeft)
            .WithOperatorToken(node.OperatorToken.WithLeadingTrivia(s_space).WithTrailingTrivia(s_space))
            .WithRight(fmtRight.WithLeadingTrivia());
    }

    /// <summary>Formats a single switch expression arm with correct spacing and line breaks.</summary>
    private static SwitchExpressionArmSyntax FormatSwitchExpressionArm(SwitchExpressionArmSyntax node, int indent)
    {
        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);

        var putOnNewLine =
            node.Expression is ThrowExpressionSyntax || SpansMultipleLines(node) ||
            node.Pattern is DiscardPatternSyntax;

        // Also check line length
        if (!putOnNewLine)
        {
            if (LineEndColumn(node) > MaximumLineLength)
                putOnNewLine = true;
        }

        // Normalize pattern trailing trivia: preserve space before when clause
        var pattern = (PatternSyntax)FormatNode(node.Pattern, indent);

        pattern =
            node.WhenClause is not null
            ?
            pattern.WithTrailingTrivia(s_space)
            :
            pattern.WithTrailingTrivia();

        // Normalize when clause trailing trivia to avoid extra spaces before =>
        var whenClause = node.WhenClause;

        if (whenClause is not null)
        {
            var fmtCond = (ExpressionSyntax)FormatNode(whenClause.Condition, indent);

            whenClause =
                whenClause
                .WithWhenKeyword(whenClause.WhenKeyword.WithLeadingTrivia().WithTrailingTrivia(s_space))
                .WithCondition(fmtCond.WithLeadingTrivia().WithTrailingTrivia());
        }

        if (putOnNewLine)
        {
            fmtExpr =
                fmtExpr
                .WithLeadingTrivia(EnsureLeadingBreaks(node.Expression.GetLeadingTrivia(), 1, indent))
                .WithTrailingTrivia(StripWhitespace(node.Expression.GetTrailingTrivia()));

            return
                node
                .WithPattern(pattern)
                .WithWhenClause(whenClause)
                .WithEqualsGreaterThanToken(node.EqualsGreaterThanToken.WithLeadingTrivia(s_space).WithTrailingTrivia())
                .WithExpression(fmtExpr);
        }

        return
            node
            .WithPattern(pattern)
            .WithWhenClause(whenClause)
            .WithEqualsGreaterThanToken(
                node.EqualsGreaterThanToken
                    .WithLeadingTrivia(s_space)
                    .WithTrailingTrivia(s_space))
            .WithExpression(
                fmtExpr.WithLeadingTrivia()
                .WithTrailingTrivia(StripWhitespace(node.Expression.GetTrailingTrivia())));
    }

    /// <summary>Formats a member access expression, breaking at dots when lines are too long.</summary>
    private static MemberAccessExpressionSyntax FormatMemberAccessExpression(MemberAccessExpressionSyntax node, int indent)
    {
        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);
        var exprEnd = EndLineOf(node.Expression);
        var dotLine = LineOf(node.OperatorToken);

        if (exprEnd != dotLine)
        {
            // Dot is already on a separate line — determine correct indent level.
            // If the expression is single-line and complex, check if a single-line version
            // would exceed the max line length. If so, the break is for line-length reasons
            // and the dot should be at indent+1.
            // Start from the original dot column when available, floored at indent.
            var dotLoc = node.OperatorToken.GetLocation();

            var dotIndent = indent;

            if (dotLoc != Location.None)
            {
                var dotCol = dotLoc.GetLineSpan().StartLinePosition.Character;
                dotIndent = Math.Max(indent, dotCol / IndentSize);
            }

            if (!SpansMultipleLines(node.Expression) &&
                node.Expression is InvocationExpressionSyntax or MemberAccessExpressionSyntax or
                    ElementAccessExpressionSyntax or ConditionalAccessExpressionSyntax)
            {
                var exprLen = fmtExpr.ToFullString().Trim().Length;
                var nameLen = node.OperatorToken.Text.Length + node.Name.ToString().Length;

                if (dotIndent * IndentSize + exprLen + nameLen > MaximumLineLength)
                    dotIndent = Math.Max(dotIndent, indent + 1);
            }

            return node.WithExpression(fmtExpr.WithTrailingTrivia(StripWhitespace(node.Expression.GetTrailingTrivia())))
                .WithOperatorToken(node.OperatorToken
                    .WithLeadingTrivia(EnsureLeadingBreaks(node.OperatorToken.LeadingTrivia, 1, dotIndent))
                    .WithTrailingTrivia());
        }

        // Check line length — only break at dots after complex expressions (chains)
        if (LineEndColumn(node) > MaximumLineLength)
        {
            // Only break if expression is complex (invocation, member access chain)
            if (node.Expression is InvocationExpressionSyntax or MemberAccessExpressionSyntax or
                ElementAccessExpressionSyntax or ConditionalAccessExpressionSyntax)
            {
                return
                    node.WithExpression(fmtExpr)
                    .WithOperatorToken(
                        node.OperatorToken
                        .WithLeadingTrivia(EnsureLeadingBreaks(node.OperatorToken.LeadingTrivia, 1, indent + 1))
                        .WithTrailingTrivia());
            }
        }

        return node.WithExpression(fmtExpr);
    }

    /// <summary>Formats an invocation expression, including its argument list.</summary>
    private static InvocationExpressionSyntax FormatInvocationExpression(InvocationExpressionSyntax node, int indent)
    {
        var fmtExpr = (ExpressionSyntax)FormatNode(node.Expression, indent);
        var fmtArgs = (ArgumentListSyntax)FormatArgumentList(node.ArgumentList, indent);

        // Propagate line breaks up in method chains: if the argument list spans
        // multiple lines but the args themselves would fit on one line, and the
        // expression is a member access on an invocation (method chain like a.B().C()),
        // collapse the arg list to single-line and break at the dot instead.
        if (fmtExpr is MemberAccessExpressionSyntax memberAccess &&
            memberAccess.Expression is InvocationExpressionSyntax &&
            SpansMultipleLines(fmtArgs))
        {
            // Estimate single-line arg width
            var singleLineArgWidth = 2; // parens

            var allArgsSingleLine = true;

            for (var i = 0; i < node.ArgumentList.Arguments.Count; i++)
            {
                var argText = node.ArgumentList.Arguments[i].ToString().Trim();

                if (SpansMultipleLines(argText))
                {
                    allArgsSingleLine = false;
                    break;
                }

                if (i > 0)
                    singleLineArgWidth += 2; // ", "

                singleLineArgWidth += argText.Length;
            }

            if (allArgsSingleLine && singleLineArgWidth <= MaximumLineLength * 2 / 3)
            {
                // Collapse arg list to single line
                fmtArgs = FormatArgListSingleLine(node.ArgumentList);
                // Break at the dot
                var dotIndent = indent;

                fmtExpr =
                    memberAccess
                    .WithExpression(
                        memberAccess.Expression.WithTrailingTrivia(
                            StripWhitespace(memberAccess.Expression.GetTrailingTrivia())))
                    .WithOperatorToken(
                        memberAccess.OperatorToken
                        .WithLeadingTrivia(EnsureLeadingBreaks(memberAccess.OperatorToken.LeadingTrivia, 1, dotIndent))
                        .WithTrailingTrivia());
            }
        }

        // Preserve line break before argument list's open paren when it was on a
        // separate line from the expression (e.g., invocation of expression result:
        // expr\n    ([args])).
        var origExprEnd = EndLineOf(node.Expression);

        var origOpenLine = LineOf(node.ArgumentList.OpenParenToken);

        if (origOpenLine > origExprEnd)
        {
            var openIndent = indent;
            var openLoc = node.ArgumentList.OpenParenToken.GetLocation();

            if (openLoc != Location.None)
            {
                var openCol = openLoc.GetLineSpan().StartLinePosition.Character;
                openIndent = Math.Max(indent, openCol / IndentSize);
            }

            fmtExpr = fmtExpr.WithTrailingTrivia(StripWhitespace(fmtExpr.GetTrailingTrivia()));

            fmtArgs =
                fmtArgs.WithOpenParenToken(
                    fmtArgs.OpenParenToken.WithLeadingTrivia(s_lineFeed, Indent(openIndent)));
        }

        return node.WithExpression(fmtExpr).WithArgumentList(fmtArgs);
    }

    /// <summary>Formats an object creation expression, including arguments and initializer.</summary>
    private static ObjectCreationExpressionSyntax FormatObjectCreationExpression(ObjectCreationExpressionSyntax node, int indent)
    {
        var r = node;

        if (r.ArgumentList is not null)
            r = r.WithArgumentList((ArgumentListSyntax)FormatArgumentList(r.ArgumentList, indent));

        if (r.Initializer is not null)
            r = r.WithInitializer(FormatInitializerExpression(r.Initializer, indent));

        return r;
    }

    /// <summary>Formats an implicit object creation (<c>new()</c>) expression.</summary>
    private static ImplicitObjectCreationExpressionSyntax FormatImplicitObjectCreationExpression(ImplicitObjectCreationExpressionSyntax node, int indent)
    {
        var r = node.WithArgumentList((ArgumentListSyntax)FormatArgumentList(node.ArgumentList, indent));

        if (r.Initializer is not null)
            r = r.WithInitializer(FormatInitializerExpression(r.Initializer, indent));

        return r;
    }

    /// <summary>Formats an assignment expression with line-break handling for the right side.</summary>
    private static AssignmentExpressionSyntax FormatAssignmentExpression(AssignmentExpressionSyntax node, int indent)
    {
        var fmtLeft = (ExpressionSyntax)FormatNode(node.Left, indent);
        // Strip trailing trivia from left expression to avoid double spaces before =
        fmtLeft = fmtLeft.WithTrailingTrivia();

        // Inside an initializer, VS keeps the value at the same indent as the key
        var isInsideInitializer = node.Parent is InitializerExpressionSyntax;

        var rhsIndent = isInsideInitializer ? indent : indent + 1;

        var fmtRight = (ExpressionSyntax)FormatNode(node.Right, rhsIndent);
        var rightText = fmtRight.ToFullString().Trim();

        // Check if right is multi-line or if the original had a line break
        var opLine = LineOf(node.OperatorToken);

        var rightLine = LineOf(node.Right);

        if (SpansMultipleLines(rightText) || rightLine > opLine)
        {
            fmtRight =
                fmtRight.WithLeadingTrivia(
                    EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, rhsIndent));

            return
                node.WithLeft(fmtLeft)
                .WithOperatorToken(node.OperatorToken.WithLeadingTrivia(s_space).WithTrailingTrivia()).WithRight(fmtRight);
        }

        return
            node.WithLeft(fmtLeft)
            .WithOperatorToken(node.OperatorToken.WithLeadingTrivia(s_space).WithTrailingTrivia(s_space))
            .WithRight(fmtRight.WithLeadingTrivia());
    }

    /// <summary>Formats a binary expression, breaking at the operator when lines are too long.</summary>
    private static BinaryExpressionSyntax FormatBinaryExpression(BinaryExpressionSyntax node, int indent)
    {
        // Outer-first line breaking: if the line exceeds max and the expression is
        // still on one line, break at the operator first without formatting inner nodes.
        // Multi-pass convergence will handle inner formatting in the next pass.
        var originalIsMultiLine = SpansMultipleLines(node);

        if (!originalIsMultiLine)
        {
            var exceedsLine = LineEndColumn(node) > MaximumLineLength;

            if (exceedsLine)
            {
                // Determine the indent for the right operand based on where the left
                // operand starts. This ensures the right side aligns with the visual
                // start of the expression rather than always using indent + 1.
                var breakIndent = indent + 1;

                var leftLoc = node.Left.GetLocation();

                if (leftLoc != Location.None)
                {
                    var leftCol = leftLoc.GetLineSpan().StartLinePosition.Character;
                    breakIndent = leftCol / IndentSize;
                }

                var l = node.Left.WithTrailingTrivia(StripWhitespace(node.Left.GetTrailingTrivia()));
                var opTrailingStripped = StripWhitespace(node.OperatorToken.TrailingTrivia);

                if (opTrailingStripped.Count > 0)
                {
                    // Operator has trailing comment (e.g., `?? /* comment */`):
                    // keep operator at end of left line, right on new line.
                    var op =
                        node.OperatorToken
                        .WithLeadingTrivia(s_space)
                        .WithTrailingTrivia(opTrailingStripped);

                    var r =
                        node.Right.WithLeadingTrivia(
                            EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, breakIndent));

                    return node.WithLeft(l).WithRight(r).WithOperatorToken(op);
                }
                else
                {
                    // Operator at end of left line, right operand on new line.
                    var op =
                        node.OperatorToken
                        .WithLeadingTrivia(s_space)
                        .WithTrailingTrivia(StripWhitespace(node.OperatorToken.TrailingTrivia));

                    var r =
                        node.Right.WithLeadingTrivia(
                            EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, breakIndent));

                    return node.WithLeft(l).WithRight(r).WithOperatorToken(op);
                }
            }
        }

        var fl = (ExpressionSyntax)FormatNode(node.Left, indent);

        // Determine the indent for the right operand of a multi-line binary expression.
        // Per the Binary Expression Chains spec, continuation lines must be at the
        // same indent level as the first operand in the chain.
        var rightIndent = indent;

        var mlLeftLoc = originalIsMultiLine ? node.Left.GetLocation() : Location.None;

        if (mlLeftLoc != Location.None)
        {
            var leftCol = mlLeftLoc.GetLineSpan().StartLinePosition.Character;
            rightIndent = leftCol / IndentSize;
        }

        var fr = (ExpressionSyntax)FormatNode(node.Right, rightIndent);
        // Normalize spacing around the operator: strip whitespace from adjacent trivia
        // and set exactly one space before and after the operator token.
        fl = fl.WithTrailingTrivia(StripWhitespace(fl.GetTrailingTrivia()));

        if (originalIsMultiLine)
        {
            // Preserve any comments from the operator's leading trivia (e.g. comments
            // between the left operand and the operator on separate lines).
            var opLeadingNonWhitespace = StripWhitespace(node.OperatorToken.LeadingTrivia);

            var opTrailingNonWhitespace = StripWhitespace(node.OperatorToken.TrailingTrivia);

            if (opTrailingNonWhitespace.Count > 0)
            {
                // Comments in trailing trivia (e.g., `&& // comment`): operator stays
                // at end of left line, right operand on new line.
                var opLeading =
                    opLeadingNonWhitespace.Count > 0
                    ?
                    EnsureLeadingBreaks(node.OperatorToken.LeadingTrivia, 1, rightIndent)
                    :
                    new SyntaxTriviaList(s_space);

                var op =
                    node.OperatorToken.WithLeadingTrivia(opLeading)
                    .WithTrailingTrivia(opTrailingNonWhitespace);

                fr = fr.WithLeadingTrivia(EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, rightIndent));
                return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op);
            }
            else if (opLeadingNonWhitespace.Count > 0)
            {
                // Comments in leading trivia (e.g., `// comment\n||`): comment on own
                // line, operator follows, right operand on same line as operator.
                var opLeading = EnsureLeadingBreaks(node.OperatorToken.LeadingTrivia, 1, rightIndent);

                var op =
                    node.OperatorToken.WithLeadingTrivia(opLeading)
                    .WithTrailingTrivia(new SyntaxTriviaList(s_space));

                fr = fr.WithLeadingTrivia(StripWhitespace(fr.GetLeadingTrivia()));
                return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op);
            }
            else
            {
                // Check for comments in the right operand's leading trivia
                var rightLeadingNonWhitespace = StripWhitespace(node.Right.GetLeadingTrivia());

                if (rightLeadingNonWhitespace.Count > 0)
                {
                    // Comments before right operand (e.g., `??\n/* comment */\nright`):
                    // operator at end of left line, right with comments on new line.
                    var op =
                        node.OperatorToken
                        .WithLeadingTrivia(s_space)
                        .WithTrailingTrivia(StripWhitespace(node.OperatorToken.TrailingTrivia));

                    fr = fr.WithLeadingTrivia(EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, rightIndent));
                    return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op);
                }
                // Check if operator is already at start of a continuation line
                // (on a different line from the end of the left operand).
                // If so, preserve that position.
                var opLoc = node.OperatorToken.GetLocation();

                var leftEndLine = EndLineOf(node.Left);

                var opLine =
                    opLoc != Location.None
                    ?
                    opLoc.GetLineSpan().StartLinePosition.Line
                    :
                    -1;

                if (opLine >= 0 && opLine > leftEndLine)
                {
                    // Operator was at start of continuation line — preserve that.
                    var op2 =
                        node.OperatorToken
                        .WithLeadingTrivia(s_lineFeed, Indent(rightIndent))
                        .WithTrailingTrivia(new SyntaxTriviaList(s_space));

                    fr = fr.WithLeadingTrivia(StripWhitespace(fr.GetLeadingTrivia()));
                    return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op2);
                }

                if (opLine >= 0 && opLine == leftEndLine)
                {
                    // Operator is on the same line as the left end.
                    // Keep right on the same line only if it was originally there,
                    // the right operand is single-line, and the line fits.
                    var rightLine = LineOf(node.Right);

                    if (rightLine == opLine &&
                        !SpansMultipleLines(node.Right))
                    {
                        var src = node.SyntaxTree.GetText();

                        if (opLine < src.Lines.Count)
                        {
                            var opLineLen = src.Lines[opLine].ToString().TrimEnd().Length;

                            if (opLineLen <= MaximumLineLength)
                            {
                                var opTrailing2 = StripWhitespace(node.OperatorToken.TrailingTrivia);

                                var op2 =
                                    node.OperatorToken
                                    .WithLeadingTrivia(s_space)
                                    .WithTrailingTrivia(opTrailing2.Count > 0 ? opTrailing2 : new SyntaxTriviaList(s_space));

                                fr = fr.WithLeadingTrivia(StripWhitespace(fr.GetLeadingTrivia()));
                                return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op2);
                            }
                        }
                    }
                }
                // Operator at end of left line, right operand on new line.
                var op3 =
                    node.OperatorToken
                    .WithLeadingTrivia(s_space)
                    .WithTrailingTrivia(StripWhitespace(node.OperatorToken.TrailingTrivia));

                fr = fr.WithLeadingTrivia(EnsureLeadingBreaks(node.Right.GetLeadingTrivia(), 1, rightIndent));
                return node.WithLeft(fl).WithRight(fr).WithOperatorToken(op3);
            }
        }

        fr = fr.WithLeadingTrivia(StripWhitespace(fr.GetLeadingTrivia()));
        var opTrailing = StripWhitespace(node.OperatorToken.TrailingTrivia);

        var fop =
            node.OperatorToken.WithLeadingTrivia(s_space)
            .WithTrailingTrivia(opTrailing.Count > 0 ? opTrailing : new SyntaxTriviaList(s_space));

        return node.WithLeft(fl).WithRight(fr).WithOperatorToken(fop);
    }

    // ──────────────────────────────────────────────────────────
    // Lambda expressions
    // ──────────────────────────────────────────────────────────

    /// <summary>Formats a parenthesized lambda expression, including its body.</summary>
    private static ParenthesizedLambdaExpressionSyntax FormatParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node, int indent)
    {
        if (node.Block is not null)
        {
            var fmtBlock = FormatBlock(node.Block, indent);
            return node.WithBlock(fmtBlock);
        }

        if (node.ExpressionBody is not null)
        {
            var fmtExpr = (ExpressionSyntax)FormatNode(node.ExpressionBody, indent);
            return node.WithExpressionBody(fmtExpr);
        }

        return node;
    }

    /// <summary>Formats a simple lambda expression, including its body.</summary>
    private static SimpleLambdaExpressionSyntax FormatSimpleLambdaExpression(SimpleLambdaExpressionSyntax node, int indent)
    {
        if (node.Block is not null)
        {
            var fmtBlock = FormatBlock(node.Block, indent);
            return node.WithBlock(fmtBlock);
        }

        if (node.ExpressionBody is not null)
        {
            var fmtExpr = (ExpressionSyntax)FormatNode(node.ExpressionBody, indent);
            return node.WithExpressionBody(fmtExpr);
        }

        return node;
    }
}
