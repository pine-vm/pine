using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.DotNet;

/// <summary>
/// C# syntax rewriter making code more readable for inspection.
/// It enforces indentation, spacing, and line-breaking conventions across a wide
/// range of syntax constructs.
/// <para>
/// For the formatting specifications, see 'csharp-coding-guidelines.md'
/// </para>
/// </summary>
/// <param name="indentChar">The character used for indentation (for example, space or tab).</param>
/// <param name="indentCharsPerLevel">The number of <paramref name="indentChar"/> characters per indentation level.</param>
public class FormatCSharpSyntaxRewriter(
    char indentChar,
    int indentCharsPerLevel)
    : CSharpSyntaxRewriter
{
    private static readonly FormatCSharpSyntaxRewriter s_instance = new();

    /// <summary>
    /// Maximum allowed line length in characters.
    /// Argument lists and other constructs that would exceed this limit when formatted
    /// on a single line are automatically split across multiple lines.
    /// </summary>
    public const int MaximumLineLength = 120;

    /// <summary>
    /// Reusable single-space whitespace trivia, avoiding repeated allocations
    /// across 24+ call sites in the formatter.
    /// </summary>
    private static readonly SyntaxTrivia s_singleSpace = SyntaxFactory.Whitespace(" ");


    /// <summary>
    /// Initializes a new instance of <see cref="FormatCSharpSyntaxRewriter"/> with default settings
    /// (four spaces per indentation level).
    /// </summary>
    public FormatCSharpSyntaxRewriter()
        : this(
              indentChar: ' ',
              indentCharsPerLevel: 4)
    {
    }


    /// <summary>
    /// Formats the provided <see cref="SyntaxTree"/> and returns a new tree
    /// with the original <see cref="SyntaxTree.Options"/> preserved.
    /// </summary>
    /// <param name="syntaxTree">The input syntax tree to format.</param>
    /// <returns>A new <see cref="SyntaxTree"/> containing the formatted root.</returns>
    public static SyntaxTree FormatSyntaxTree(SyntaxTree syntaxTree)
    {
        var formattedSyntaxRoot = FormatSyntaxTree(syntaxTree.GetRoot());

        var formattedSyntaxTree = syntaxTree.WithRootAndOptions(formattedSyntaxRoot, syntaxTree.Options);

        return formattedSyntaxTree;
    }


    /// <summary>
    /// Formats the provided <see cref="SyntaxNode"/> (or derived type) and returns a new node of the same type.
    /// </summary>
    /// <typeparam name="T">A Roslyn syntax node type.</typeparam>
    /// <param name="syntaxTree">The syntax node to format.</param>
    /// <returns>The formatted node of type <typeparamref name="T"/>.</returns>
    public static T FormatSyntaxTree<T>(T syntaxTree)
        where T : SyntaxNode =>
        (T)s_instance.Visit(syntaxTree);


    // ----- List formatting methods -----
    // VisitArgumentList, VisitParameterList, and VisitCollectionExpression all follow
    // the same high-level pattern for deciding single-line vs multi-line layout:
    //
    //  1. Check if the original code had items on multiple lines (preserve user's choice).
    //  2. If originally single-line: build single-line version, check against MaximumLineLength,
    //     return if it fits; otherwise fall through to multi-line.
    //  3. Format multi-line with proper indentation and comment preservation.

    /// <inheritdoc/>
    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax originalNode)
    {
        var node = (ArgumentListSyntax)base.VisitArgumentList(originalNode)!;

        // Check if the original code already had arguments spanning multiple lines
        // (first argument on a different line from the open paren, or arguments on different lines
        // from each other).
        var openParenLine = originalNode.OpenParenToken.GetLocation().GetLineSpan().StartLinePosition.Line;

        var firstArgOnSameLineAsOpenParen =
            originalNode.Arguments.Count > 0 &&
            originalNode.Arguments.First().GetLocation().GetLineSpan().StartLinePosition.Line == openParenLine;

        var argsOnDifferentLines =
            originalNode.Arguments.Count > 1 &&
            originalNode.Arguments.First().GetLocation().GetLineSpan().StartLinePosition.Line !=
            originalNode.Arguments.Last().GetLocation().GetLineSpan().StartLinePosition.Line;

        // When an argument is an invocation or a lambda with a block body that itself spans
        // multiple lines, the outer argument list should also go multi-line.
        var anyArgContainsMultipleLines =
            firstArgOnSameLineAsOpenParen &&
            originalNode.Arguments.Any(arg =>
                (arg.Expression is InvocationExpressionSyntax ||
                 arg.Expression is LambdaExpressionSyntax { Block: not null }) &&
                SpansMultipleLines(arg));

        var formatOnMultipleLines =
            originalNode.Arguments.Count > 0 &&
            (!firstArgOnSameLineAsOpenParen || argsOnDifferentLines || anyArgContainsMultipleLines);

        if (!formatOnMultipleLines)
        {
            var reformattedArgs =
                node.Arguments
                .Select((argumentSyntax, index) =>
                {
                    var trailingComments = StripWhitespaceTrivia(argumentSyntax.GetTrailingTrivia());

                    if (index is 0)
                        return argumentSyntax.WithLeadingTrivia().WithTrailingTrivia(trailingComments);
                    else
                        return argumentSyntax.WithLeadingTrivia(s_singleSpace).WithTrailingTrivia(trailingComments);
                }).ToList();

            var newArguments =
                RebuildSeparatedListPreservingSeparators(originalNode.Arguments, reformattedArgs);

            var singleLineResult =
                node
                .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
                .WithArguments(newArguments)
                .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia());

            // Estimate the column where the open paren will appear after formatting.
            // Use the indentation level plus the width of the expression preceding the argument list
            // (e.g. the method name), rather than the original source column which may be wrong
            // after outer nodes are reformatted.
            var indentColumn = ComputeIndentationLevel(originalNode) * indentCharsPerLevel;

            var prefixWidth =
                originalNode.Parent switch
                {
                    InvocationExpressionSyntax invocation =>
                    invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                    memberAccess.Expression.GetLocation().GetLineSpan().EndLinePosition.Line !=
                    memberAccess.OperatorToken.GetLocation().GetLineSpan().StartLinePosition.Line
                    ?
                    memberAccess.OperatorToken.Span.Length + memberAccess.Name.Span.Length
                    :
                    invocation.Expression.Span.Length,

                    ObjectCreationExpressionSyntax creation =>
                    creation.NewKeyword.Span.Length + 1 + creation.Type.Span.Length,

                    _ => 0
                };

            var lineEndColumn = indentColumn + prefixWidth + singleLineResult.ToFullString().Length;

            // Skip the MaximumLineLength check when the single argument itself spans multiple
            // lines (e.g., a lambda body), since the line-end estimation doesn't apply to
            // multi-line content that starts on the same line as the open paren.
            // Exclude CollectionExpressionSyntax and InvocationExpressionSyntax: when a
            // collection or invocation exceeds the line length and was reformatted to multi-line,
            // the argument list should also go multi-line.
            var singleArgSpansMultipleLines =
                node.Arguments.Count is 1 && firstArgOnSameLineAsOpenParen &&
                node.Arguments[0].Expression is not CollectionExpressionSyntax and not InvocationExpressionSyntax &&
                SpansMultipleLines(node.Arguments.First());

            if (lineEndColumn <= MaximumLineLength || singleArgSpansMultipleLines)
                return singleLineResult;
            // Single-line version exceeds MaximumLineLength — fall through to multi-line formatting.
        }

        {
            var argumentListIndentLevel = ComputeIndentationLevel(originalNode);

            // When this argument list is part of an invocation via a member access chain where
            // the dot is on a separate line, use the dot's source column to derive the indent level,
            // but ONLY if the chain-derived level is lower (to prevent over-indentation).
            // This avoids under-indenting when the dot was just moved to a new line.
            if (originalNode.Parent is InvocationExpressionSyntax
                {
                    Expression: MemberAccessExpressionSyntax memberAccess
                })
            {
                var maExprEndLine = memberAccess.Expression.GetLocation().GetLineSpan().EndLinePosition.Line;
                var maDotLine = memberAccess.OperatorToken.GetLocation().GetLineSpan().StartLinePosition.Line;

                if (maExprEndLine != maDotLine)
                {
                    var dotColumn = memberAccess.OperatorToken.GetLocation().GetLineSpan().StartLinePosition.Character;
                    var chainDerivedLevel = dotColumn / indentCharsPerLevel;

                    if (chainDerivedLevel < argumentListIndentLevel)
                    {
                        argumentListIndentLevel = chainDerivedLevel;
                    }
                }
            }

            var argumentIndentLevel = argumentListIndentLevel + 1;

            var argumentListIndentationTrivia =
                ComputeIndentationTriviaForLevel(argumentListIndentLevel);

            // For constructor initializer arguments (e.g., `: this(indentChar: ' ',...)`),
            // preserve the original alignment-based indentation when it differs from our
            // computed indent level. This handles column-aligned arguments after `this(`.
            var argumentIndentationTrivia =
                originalNode.Parent is ConstructorInitializerSyntax
                && originalNode.Arguments.Count > 0
                && GetOriginalLeadingWhitespace(originalNode.Arguments.First()) is { } originalWhitespace
                && originalWhitespace.Span.Length != argumentIndentLevel * indentCharsPerLevel
                ?
                originalWhitespace
                :
                ComputeIndentationTriviaForLevel(argumentIndentLevel);

            var reformattedMultiLineArgs =
                node.Arguments
                .Select((argumentSyntax, index) =>
                {
                    var originalArg = originalNode.Arguments[index];
                    var trailingComments = StripWhitespaceTrivia(originalArg.GetTrailingTrivia());

                    // Preserve leading comments on the argument, with LineFeed prefix for multi-line layout
                    var leadingTrivia =
                        HasPreservableTrivia(originalArg.GetLeadingTrivia())
                        ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                            .AddRange(BuildLeadingTriviaPreservingComments(
                                originalArg.GetLeadingTrivia(),
                                argumentIndentationTrivia))
                        :
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, argumentIndentationTrivia);

                    return
                        argumentSyntax
                        .WithLeadingTrivia(leadingTrivia)
                        .WithTrailingTrivia(trailingComments);
                }).ToList();

            var newArguments =
                RebuildSeparatedListPreservingSeparators(originalNode.Arguments, reformattedMultiLineArgs);

            // For the close paren, strip whitespace-only leading trivia to avoid
            // leftover alignment spaces (e.g., "arg                )"). Preserve any comments.
            var closeParenLeading =
                HasPreservableTrivia(node.CloseParenToken.LeadingTrivia)
                ?
                StripWhitespaceTrivia(node.CloseParenToken.LeadingTrivia)
                :
                [];

            return
                node
                .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
                .WithArguments(newArguments)
                .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia(closeParenLeading));
        }
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitArgument(ArgumentSyntax originalNode)
    {
        return (ArgumentSyntax)base.VisitArgument(originalNode)!;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitArrowExpressionClause(ArrowExpressionClauseSyntax originalNode)
    {
        var node = (ArrowExpressionClauseSyntax)base.VisitArrowExpressionClause(originalNode)!;

        // Check if the original had the expression on a new line after =>
        var arrowLine = originalNode.ArrowToken.GetLocation().GetLineSpan().StartLinePosition.Line;
        var exprLine = originalNode.Expression.GetLocation().GetLineSpan().StartLinePosition.Line;
        var originalHadNewline = exprLine > arrowLine;

        // Per spec: break before expression body when line exceeds MaximumLineLength.
        if (!originalHadNewline)
        {
            var indentLevel = ComputeIndentationLevel(originalNode);

            var lineWidth =
                indentLevel * indentCharsPerLevel +
                originalNode.Parent?.Span.Length ?? originalNode.Span.Length;

            if (lineWidth > MaximumLineLength)
                originalHadNewline = true;
        }

        if (originalHadNewline)
        {
            // Preserve the newline: fix indentation of the expression on its own line
            var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Expression);

            // Preserve any comments that were between the arrow token and the expression
            var originalExpressionLeadingComments =
                originalNode.Expression.GetLeadingTrivia().Where(IsPreservableTrivia).ToList();

            var expressionLeadingTrivia = new List<SyntaxTrivia> { SyntaxFactory.LineFeed };

            foreach (var comment in originalExpressionLeadingComments)
            {
                expressionLeadingTrivia.Add(indentationTrivia);
                expressionLeadingTrivia.Add(comment);
                expressionLeadingTrivia.Add(SyntaxFactory.LineFeed);
            }

            expressionLeadingTrivia.Add(indentationTrivia);

            return
                node
                .WithArrowToken(node.ArrowToken.WithTrailingTrivia())
                .WithExpression(
                    node.Expression.WithLeadingTrivia(expressionLeadingTrivia));
        }
        else
        {
            // Expression was on same line as =>. Keep it there.
            return
                node
                .WithArrowToken(node.ArrowToken.WithTrailingTrivia(s_singleSpace))
                .WithExpression(
                    node.Expression.WithLeadingTrivia(
                        StripWhitespaceTrivia(originalNode.Expression.GetLeadingTrivia())));
        }
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitConditionalExpression(ConditionalExpressionSyntax originalNode)
    {
        var node = (ConditionalExpressionSyntax)base.VisitConditionalExpression(originalNode)!;

        // Per spec: reformat when the original conditional already spans multiple lines,
        // or when the line would exceed MaximumLineLength.
        var formatMultiLine = SpansMultipleLines(originalNode);

        if (!formatMultiLine)
        {
            var indentLevel = ComputeIndentationLevel(originalNode);
            var lineWidth = indentLevel * indentCharsPerLevel + originalNode.Span.Length;

            if (lineWidth > MaximumLineLength)
                formatMultiLine = true;
        }

        if (!formatMultiLine)
            return node;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node
            .WithCondition(node.Condition.WithTrailingTrivia(
                    StripWhitespaceTrivia(node.Condition.GetTrailingTrivia())))
            .WithQuestionToken(
                node.QuestionToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia())
            .WithWhenTrue(
                node.WhenTrue.WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia(
                    StripWhitespaceTrivia(node.WhenTrue.GetTrailingTrivia())))
            .WithColonToken(
                node.ColonToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia())
            .WithWhenFalse(node.WhenFalse.WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitIfStatement(IfStatementSyntax originalNode)
    {
        var node = (IfStatementSyntax)base.VisitIfStatement(originalNode)!;

        // Ensure proper spacing between 'if' keyword and the condition parentheses
        node = node.WithIfKeyword(node.IfKeyword.WithTrailingTrivia(s_singleSpace));

        // Remove trailing trivia from condition and close paren to avoid extra spaces
        node =
            node
            .WithCondition(node.Condition.WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithTrailingTrivia());

        if (node.Statement is BlockSyntax block)
        {
            // If the statement is a block, ensure the opening brace is on a new line
            var ifIndentationTrivia = ComputeIndentationTriviaForNode(originalNode);

            var formattedBlock =
                block
                .WithOpenBraceToken(
                    block.OpenBraceToken
                    .WithLeadingTrivia(SyntaxFactory.LineFeed, ifIndentationTrivia));

            node = node.WithStatement(formattedBlock);
        }
        else
        {
            // Non-block body: ensure it starts on a new line with increased indentation
            var bodyIndentationTrivia =
                ComputeIndentationTriviaForLevel(ComputeIndentationLevel(originalNode) + 1);

            var formattedBody =
                node.Statement
                .WithLeadingTrivia(SyntaxFactory.LineFeed, bodyIndentationTrivia);

            // If an else clause follows, strip trailing newline from the body to avoid
            // a blank line between the body and 'else' (VisitElseClause adds its own newline).
            if (node.Else is not null)
                formattedBody =
                formattedBody.WithTrailingTrivia(
                    StripWhitespaceTrivia(formattedBody.GetTrailingTrivia()));

            node = node.WithStatement(formattedBody);
        }

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitWhileStatement(WhileStatementSyntax originalNode)
    {
        var node = (WhileStatementSyntax)base.VisitWhileStatement(originalNode)!;

        // Ensure proper spacing between 'while' keyword and the condition parentheses
        return
            node
            .WithWhileKeyword(node.WhileKeyword.WithTrailingTrivia(s_singleSpace));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitForStatement(ForStatementSyntax originalNode)
    {
        var node = (ForStatementSyntax)base.VisitForStatement(originalNode)!;

        // Ensure proper spacing between 'for' keyword and the condition parentheses
        return
            node
            .WithForKeyword(node.ForKeyword.WithTrailingTrivia(s_singleSpace));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax originalNode)
    {
        var node = (ForEachStatementSyntax)base.VisitForEachStatement(originalNode)!;

        // Ensure proper spacing between 'foreach' keyword and the condition parentheses
        return
            node
            .WithForEachKeyword(node.ForEachKeyword.WithTrailingTrivia(s_singleSpace));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitElseClause(ElseClauseSyntax originalNode)
    {
        var node = (ElseClauseSyntax)base.VisitElseClause(originalNode)!;

        var elseIndentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        // If the else clause contains an if statement (else if), put a space after else
        if (node.Statement is IfStatementSyntax)
        {
            node =
                node
                .WithElseKeyword(
                    node.ElseKeyword
                    .WithLeadingTrivia(SyntaxFactory.LineFeed, elseIndentationTrivia)
                    .WithTrailingTrivia(s_singleSpace));
        }
        else
        {
            node =
                node
                .WithElseKeyword(
                    node.ElseKeyword
                    .WithLeadingTrivia(SyntaxFactory.LineFeed, elseIndentationTrivia)
                    .WithTrailingTrivia());

            // If the statement is a block, ensure the opening brace is on a new line
            if (node.Statement is BlockSyntax block)
            {
                var formattedBlock =
                    block
                    .WithOpenBraceToken(
                        block.OpenBraceToken
                        .WithLeadingTrivia(SyntaxFactory.LineFeed, elseIndentationTrivia));

                node = node.WithStatement(formattedBlock);
            }
            else
            {
                // Non-block body: ensure it starts on a new line with increased indentation
                var bodyIndentationTrivia =
                    ComputeIndentationTriviaForLevel(ComputeIndentationLevel(originalNode) + 1);

                node =
                    node.WithStatement(
                        node.Statement.WithLeadingTrivia(SyntaxFactory.LineFeed, bodyIndentationTrivia));
            }
        }

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitBlock(BlockSyntax originalNode)
    {
        var node = (BlockSyntax)base.VisitBlock(originalNode)!;

        if (originalNode.Statements.Count is 0 && !SpansMultipleLines(originalNode))
        {
            return node;
        }

        var blockIndentationLevel = ComputeIndentationLevel(originalNode);

        var statementIndentationLevel = blockIndentationLevel + 1;

        var blockIndentationTrivia =
            ComputeIndentationTriviaForLevel(blockIndentationLevel);

        var statementIndentationTrivia =
            ComputeIndentationTriviaForLevel(statementIndentationLevel);

        var formattedNode =
            node
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(blockIndentationTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithStatements(
                FormatStatementListWithSpacing(
                    node.Statements,
                    originalNode.Statements,
                    statementIndentationTrivia))
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(
                    BuildLeadingTriviaPreservingComments(
                        originalNode.CloseBraceToken.LeadingTrivia,
                        statementIndentationTrivia,
                        finalIndentationTrivia: blockIndentationTrivia,
                        preserveLeadingBlankLine: true)));

        // If this block is the statement of an if statement followed by an else, remove trailing trivia from the close brace
        if (originalNode.Parent is IfStatementSyntax ifStmt && ifStmt.Else is not null)
        {
            formattedNode =
                formattedNode.WithCloseBraceToken(
                    formattedNode.CloseBraceToken.WithTrailingTrivia());
        }

        // If this block is the try body of a try statement with catches or finally, remove trailing trivia
        // so VisitCatchClause/VisitFinallyClause can add its own newline (matching the if-else pattern).
        if (originalNode.Parent is TryStatementSyntax tryStmt && ReferenceEquals(originalNode, tryStmt.Block)
            && (tryStmt.Catches.Count > 0 || tryStmt.Finally is not null))
        {
            formattedNode =
                formattedNode.WithCloseBraceToken(
                    formattedNode.CloseBraceToken.WithTrailingTrivia());
        }

        // If this block is the body of a catch clause followed by another catch or finally,
        // remove trailing trivia so the next clause can add its own newline.
        if (originalNode.Parent is CatchClauseSyntax catchClause
            && catchClause.Parent is TryStatementSyntax parentTry)
        {
            var catchIndex = parentTry.Catches.IndexOf(catchClause);
            var hasMoreCatches = catchIndex < parentTry.Catches.Count - 1;
            var hasFinally = parentTry.Finally is not null;

            if (hasMoreCatches || hasFinally)
            {
                formattedNode =
                    formattedNode.WithCloseBraceToken(
                        formattedNode.CloseBraceToken.WithTrailingTrivia());
            }
        }

        return formattedNode;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitCatchClause(CatchClauseSyntax originalNode)
    {
        var node = (CatchClauseSyntax)base.VisitCatchClause(originalNode)!;

        var catchIndentTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node.WithCatchKeyword(
                node.CatchKeyword
                .WithLeadingTrivia(SyntaxFactory.LineFeed, catchIndentTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitFinallyClause(FinallyClauseSyntax originalNode)
    {
        var node = (FinallyClauseSyntax)base.VisitFinallyClause(originalNode)!;

        var finallyIndentTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node.WithFinallyKeyword(
                node.FinallyKeyword
                .WithLeadingTrivia(SyntaxFactory.LineFeed, finallyIndentTrivia));
    }


    /// <summary>
    /// Checks if the original statement had a blank line after it by examining the
    /// trivia between the end of this statement and the start of the next statement.
    /// A blank line means two consecutive end-of-line trivia with only whitespace between them.
    /// </summary>
    private static bool OriginalHasBlankLineAfter(StatementSyntax originalStatement)
    {
        // Collect all trivia between end of this statement and start of next:
        // trailing trivia of last token + leading trivia of next token.
        var lastToken = originalStatement.GetLastToken();
        var nextToken = lastToken.GetNextToken();

        var allTriviaBetween =
            lastToken.TrailingTrivia
            .Concat(nextToken.LeadingTrivia)
            .ToList();

        // Look for two EOL trivia with only whitespace between them (= blank line)
        var lastEolIndex = -1;

        for (var i = 0; i < allTriviaBetween.Count; i++)
        {
            var trivia = allTriviaBetween[i];

            if (trivia.IsKind(SyntaxKind.EndOfLineTrivia))
            {
                if (lastEolIndex >= 0)
                {
                    // Check if only whitespace exists between the two EOLs
                    var onlyWhitespaceBetween = true;

                    for (var j = lastEolIndex + 1; j < i; j++)
                    {
                        if (!allTriviaBetween[j].IsKind(SyntaxKind.WhitespaceTrivia))
                        {
                            onlyWhitespaceBetween = false;
                            break;
                        }
                    }

                    if (onlyWhitespaceBetween)
                        return true;
                }

                lastEolIndex = i;
            }
            else if (!trivia.IsKind(SyntaxKind.WhitespaceTrivia))
            {
                // Non-whitespace, non-EOL trivia (like a comment) resets the search
                lastEolIndex = -1;
            }
        }

        return false;
    }


    /// <summary>
    /// Formats a list of statements with proper spacing: blank lines around multi-line
    /// statements, preserved user-placed blank lines, and re-indented leading/trailing trivia.
    /// Used by both <see cref="VisitBlock"/> and <see cref="VisitSwitchStatement"/>.
    /// </summary>
    private static SyntaxList<StatementSyntax> FormatStatementListWithSpacing(
        SyntaxList<StatementSyntax> formattedStatements,
        SyntaxList<StatementSyntax> originalStatements,
        SyntaxTrivia statementIndentationTrivia)
    {
        return
            [
            .. formattedStatements.Select((statement, index) =>
                {
                    var originalStatement = originalStatements[index];
                    var isLastStatement = index == formattedStatements.Count - 1;

                    // Determine if a blank line is needed after this statement.
                    // Rule: if a statement spans multiple lines, it must be separated
                    // from previous and following statements by a blank line.
                    // User-placed blank lines between single-line statements are preserved.
                    var nextStatement =
                        !isLastStatement ? formattedStatements[index + 1] : null;

                    bool needsBlankLineAfter;

                    if (isLastStatement || nextStatement is null)
                    {
                        needsBlankLineAfter = false;
                    }
                    else
                    {
                        var currentIsMultiLine = SpansMultipleLines(statement);
                        var nextIsMultiLine = SpansMultipleLines(nextStatement);

                        needsBlankLineAfter =
                            currentIsMultiLine || nextIsMultiLine ||
                        OriginalHasBlankLineAfter(originalStatement);
                    }

                    var trailingTrivia =
                        BuildTrailingTriviaPreservingComments(
                            originalStatement.GetTrailingTrivia(),
                            isLastStatement || !needsBlankLineAfter
                            ?
                            new SyntaxTriviaList(SyntaxFactory.LineFeed)
                            :
                            new SyntaxTriviaList(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed));

                    var leadingTrivia =
                        BuildLeadingTriviaPreservingComments(
                            originalStatement.GetLeadingTrivia(),
                            statementIndentationTrivia);

                    return
                        statement.WithLeadingTrivia(leadingTrivia)
                .WithTrailingTrivia(trailingTrivia);
                })
            ];
    }


    /// <summary>
    /// Formats a list of member declarations with blank-line separation and comment preservation.
    /// Used by <see cref="VisitCompilationUnit"/>, <see cref="VisitClassDeclaration"/>,
    /// and <see cref="VisitFileScopedNamespaceDeclaration"/>.
    /// </summary>
    /// <param name="formattedMembers">The rewritten member list from the base visitor.</param>
    /// <param name="originalMembers">The original member list (for trivia preservation).</param>
    /// <param name="skipBlankLineBeforeFirst">
    /// If <c>true</c>, the first member gets no blank line before it (e.g. first member in a class
    /// or first top-level member when there are no usings).
    /// If <c>false</c>, the first member gets a blank line before it (e.g. after namespace semicolon).
    /// </param>
    private SyntaxList<MemberDeclarationSyntax> FormatMemberList(
        SyntaxList<MemberDeclarationSyntax> formattedMembers,
        SyntaxList<MemberDeclarationSyntax> originalMembers,
        bool skipBlankLineBeforeFirst) =>
        SyntaxFactory.List(
            formattedMembers.Select((member, i) =>
            {
                var originalMember = originalMembers[i];
                var indentationTrivia = ComputeIndentationTriviaForNode(originalMember);
                var addBlankLineBefore = !(i is 0 && skipBlankLineBeforeFirst);

                // Spec: Type declarations, method declarations and member declarations
                // must be separated by two empty lines.
                // Exclude FieldDeclarationSyntax (fields/variables, especially in scripts)
                // and GlobalStatementSyntax (statements, not declarations).
                var addExtraBlankLineForTypeDeclaration =
                    addBlankLineBefore && i > 0 &&
                    originalMember is not GlobalStatementSyntax and not FieldDeclarationSyntax;

                return
                    member.WithLeadingTrivia(
                        BuildMemberLeadingTriviaPreservingComments(
                            originalMember.GetLeadingTrivia(),
                            indentationTrivia,
                            addBlankLineBefore,
                            addExtraBlankLineForTypeDeclaration));
            }));


    /// <inheritdoc/>
    public override SyntaxNode? VisitCompilationUnit(CompilationUnitSyntax originalNode)
    {
        var node = (CompilationUnitSyntax)base.VisitCompilationUnit(originalNode)!;

        // Format using directives
        if (node.Usings.Count > 0)
        {
            // Separate alias usings from regular usings, and static from non-static.
            // Alias usings (using X = ...) are kept in their original order since they
            // may depend on each other or have specific placement requirements.
            var aliasUsings = new List<UsingDirectiveSyntax>();
            var staticUsings = new List<UsingDirectiveSyntax>();
            var regularUsings = new List<UsingDirectiveSyntax>();

            foreach (var u in node.Usings)
            {
                if (u.Alias is not null)
                    aliasUsings.Add(u);
                else if (u.StaticKeyword.IsKind(SyntaxKind.StaticKeyword))
                    staticUsings.Add(u);
                else
                    regularUsings.Add(u);
            }

            // Sort regular usings alphabetically (case-insensitive).
            regularUsings = [.. regularUsings.OrderBy(u => u.Name?.ToString() ?? "", CaseInsensitiveComparison.Comparer)];
            staticUsings = [.. staticUsings.OrderBy(u => u.Name?.ToString() ?? "", CaseInsensitiveComparison.Comparer)];

            var formattedUsings = new List<UsingDirectiveSyntax>();

            void AddUsingWithPreservedComments(UsingDirectiveSyntax usingDirective, int originalIndex, bool addBlankLineBefore)
            {
                var originalUsing = originalNode.Usings[originalIndex];

                // Preserve all preservable trivia from the original leading trivia
                var preservedLeadingTrivia =
                    originalUsing.GetLeadingTrivia()
                    .Where(IsPreservableTrivia)
                    .ToList();

                var leadingTrivia = new List<SyntaxTrivia>();

                if (addBlankLineBefore)
                    leadingTrivia.Add(SyntaxFactory.LineFeed);

                foreach (var comment in preservedLeadingTrivia)
                {
                    leadingTrivia.Add(comment);
                    leadingTrivia.Add(SyntaxFactory.LineFeed);
                }

                var result =
                    preservedLeadingTrivia.Count > 0
                    ?
                    usingDirective.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(SyntaxFactory.LineFeed)
                    :
                    addBlankLineBefore
                    ?
                    usingDirective.WithLeadingTrivia(SyntaxFactory.LineFeed)
                        .WithTrailingTrivia(SyntaxFactory.LineFeed)
                    :
                    usingDirective.WithTrailingTrivia(SyntaxFactory.LineFeed);

                formattedUsings.Add(result);
            }

            // Add regular usings first (sorted)
            foreach (var usingDirective in regularUsings)
            {
                var originalIndex = node.Usings.IndexOf(usingDirective);

                AddUsingWithPreservedComments(usingDirective, originalIndex, addBlankLineBefore: false);
            }

            // Add alias usings (preserved in original order, with blank line separator)
            foreach (var (usingDirective, i) in aliasUsings.Select((u, i) => (u, i)))
            {
                var originalIndex = node.Usings.IndexOf(usingDirective);
                var addBlankLine = i is 0 && regularUsings.Count > 0;

                AddUsingWithPreservedComments(usingDirective, originalIndex, addBlankLine);
            }

            // Add static usings last (with blank line separator if needed)
            if (staticUsings.Count > 0)
            {
                foreach (var (usingDirective, index) in staticUsings.Select((u, i) => (u, i)))
                {
                    if (index is 0 && (regularUsings.Count > 0 || aliasUsings.Count > 0))
                    {
                        formattedUsings.Add(
                            usingDirective
                            .WithLeadingTrivia(SyntaxFactory.LineFeed)
                            .WithTrailingTrivia(SyntaxFactory.LineFeed));
                    }
                    else
                    {
                        formattedUsings.Add(usingDirective.WithTrailingTrivia(SyntaxFactory.LineFeed));
                    }
                }
            }

            node = node.WithUsings([.. formattedUsings]);
        }

        // Format members (namespace declarations, class declarations, etc.)
        if (node.Members.Count > 0)
        {
            var skipBlankLineBeforeFirst = node.Usings.Count is 0;

            var formattedMembers =
                FormatMemberList(node.Members, originalNode.Members, skipBlankLineBeforeFirst);

            // At compilation unit level, ensure each member has trailing LineFeed
            formattedMembers =
                SyntaxFactory.List(
                    formattedMembers.Select(m => m.WithTrailingTrivia(SyntaxFactory.LineFeed)));

            return node.WithMembers(formattedMembers);
        }

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitSwitchStatement(SwitchStatementSyntax originalNode)
    {
        var node = (SwitchStatementSyntax)base.VisitSwitchStatement(originalNode)!;

        // First ensure proper spacing between 'switch' keyword and expression
        node = node.WithSwitchKeyword(node.SwitchKeyword.WithTrailingTrivia(s_singleSpace));

        var formattedSections = new List<SwitchSectionSyntax>();

        for (var i = 0; i < node.Sections.Count; i++)
        {
            var section = node.Sections[i];
            var originalSection = originalNode.Sections[i];
            var isLastSection = i == node.Sections.Count - 1;

            // For standard C# formatting, case labels should be at the same level as the switch block contents
            var labelIndentationTrivia = ComputeIndentationTriviaForNode(originalSection, addIndentLevels: 0);
            var statementIndentationTrivia = ComputeIndentationTriviaForNode(originalSection, addIndentLevels: 1);

            // Add proper indentation to labels - same as switch block content level
            // Preserve comments in leading trivia (e.g. // comments before case labels)
            var formattedLabels =
                section.Labels.Select((label, labelIndex) =>
                {
                    var originalLabel = originalSection.Labels[labelIndex];

                    var labelLeading =
                        BuildLeadingTriviaPreservingComments(
                            originalLabel.GetLeadingTrivia(),
                            labelIndentationTrivia);

                    return label.WithLeadingTrivia(labelLeading);
                }).ToList();

            // Format statements with shared spacing logic
            var formattedStatementList =
                FormatStatementListWithSpacing(
                    section.Statements,
                    originalSection.Statements,
                    statementIndentationTrivia);

            // Add empty line after section (except for last section)
            if (!isLastSection && formattedStatementList.Count > 0)
            {
                var lastStatement = formattedStatementList[^1];

                // Ensure there's a blank line at the end of the section.
                // Preserve any trailing comments that BuildTrailingTriviaPreservingComments
                // already placed on this statement, by appending an extra LineFeed rather
                // than replacing the entire trailing trivia.
                var currentTrailing = lastStatement.GetTrailingTrivia();
                var eolCount = currentTrailing.Count(t => t.IsKind(SyntaxKind.EndOfLineTrivia));

                if (eolCount < 2)
                {
                    var extendedTrivia = currentTrailing.Add(SyntaxFactory.LineFeed);

                    formattedStatementList =
                        formattedStatementList.Replace(
                            lastStatement,
                            lastStatement.WithTrailingTrivia(extendedTrivia));
                }
            }

            var formattedSection =
                section
                .WithLabels([.. formattedLabels])
                .WithStatements(formattedStatementList);

            formattedSections.Add(formattedSection);
        }

        var switchIndentTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node
            .WithCloseParenToken(node.CloseParenToken.WithTrailingTrivia())
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, switchIndentTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithSections([.. formattedSections])
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(switchIndentTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitCaseSwitchLabel(CaseSwitchLabelSyntax originalNode)
    {
        var node = (CaseSwitchLabelSyntax)base.VisitCaseSwitchLabel(originalNode)!;

        // Normalize: 'case' keyword gets single space trailing, colon gets no leading space
        return
            node
            .WithKeyword(node.Keyword.WithTrailingTrivia(s_singleSpace))
            .WithColonToken(node.ColonToken.WithLeadingTrivia());
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitCasePatternSwitchLabel(CasePatternSwitchLabelSyntax originalNode)
    {
        var node = (CasePatternSwitchLabelSyntax)base.VisitCasePatternSwitchLabel(originalNode)!;

        // Normalize: 'case' keyword gets single space trailing, colon gets no leading space
        return
            node
            .WithKeyword(node.Keyword.WithTrailingTrivia(s_singleSpace))
            .WithColonToken(node.ColonToken.WithLeadingTrivia());
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitDefaultSwitchLabel(DefaultSwitchLabelSyntax originalNode)
    {
        var node = (DefaultSwitchLabelSyntax)base.VisitDefaultSwitchLabel(originalNode)!;

        // Normalize: 'default' keyword has no trailing space, colon directly follows
        return
            node
            .WithKeyword(node.Keyword.WithTrailingTrivia())
            .WithColonToken(node.ColonToken.WithLeadingTrivia());
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitClassDeclaration(ClassDeclarationSyntax originalNode)
    {
        var node = (ClassDeclarationSyntax)base.VisitClassDeclaration(originalNode)!;

        return
            node.WithMembers(
                FormatMemberList(node.Members, originalNode.Members, skipBlankLineBeforeFirst: true));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitRecordDeclaration(RecordDeclarationSyntax originalNode)
    {
        var node = (RecordDeclarationSyntax)base.VisitRecordDeclaration(originalNode)!;

        return
            node.WithMembers(
                FormatMemberList(node.Members, originalNode.Members, skipBlankLineBeforeFirst: true));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitInterfaceDeclaration(InterfaceDeclarationSyntax originalNode)
    {
        var node = (InterfaceDeclarationSyntax)base.VisitInterfaceDeclaration(originalNode)!;

        return
            node.WithMembers(
                FormatMemberList(node.Members, originalNode.Members, skipBlankLineBeforeFirst: true));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax originalNode)
    {
        var node = (FileScopedNamespaceDeclarationSyntax)base.VisitFileScopedNamespaceDeclaration(originalNode)!;

        // Ensure semicolon has only a linefeed as trailing trivia
        node = node.WithSemicolonToken(node.SemicolonToken.WithTrailingTrivia(SyntaxFactory.LineFeed));

        // Preserve namespace-level using directives (e.g. using aliases after namespace X;)
        // These usings should keep their comments and original order.
        if (node.Usings.Count > 0)
        {
            var formattedUsings = new List<UsingDirectiveSyntax>();

            foreach (var (usingDirective, i) in node.Usings.Select((u, i) => (u, i)))
            {
                var originalUsing = originalNode.Usings[i];

                var leadingPreservableTrivia =
                    originalUsing.GetLeadingTrivia()
                    .Where(IsPreservableTrivia)
                    .ToList();

                var leadingTrivia =
                    new List<SyntaxTrivia>
                    {
                        SyntaxFactory.LineFeed
                    };

                foreach (var comment in leadingPreservableTrivia)
                {
                    leadingTrivia.Add(comment);
                    leadingTrivia.Add(SyntaxFactory.LineFeed);
                }

                var result =
                    leadingPreservableTrivia.Count > 0
                    ?
                    usingDirective.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(SyntaxFactory.LineFeed)
                    :
                    usingDirective.WithLeadingTrivia(SyntaxFactory.LineFeed)
                        .WithTrailingTrivia(SyntaxFactory.LineFeed);

                formattedUsings.Add(result);
            }

            node = node.WithUsings([.. formattedUsings]);
        }

        // Format all members with consistent spacing
        if (node.Members.Count > 0)
        {
            return
                node.WithMembers(
                    FormatMemberList(node.Members, originalNode.Members, skipBlankLineBeforeFirst: false));
        }

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax originalNode)
    {
        var node = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(originalNode)!;

        // Check if the dot is already on a separate line from the expression.
        var exprEndLine = originalNode.Expression.GetLocation().GetLineSpan().EndLinePosition.Line;
        var dotLine = originalNode.OperatorToken.GetLocation().GetLineSpan().StartLinePosition.Line;

        var dotAlreadyOnSeparateLine = exprEndLine != dotLine;

        if (dotAlreadyOnSeparateLine)
        {
            // For method chains where the expression is an invocation (e.g., SyntaxFactory.Foo().WithBar()),
            // preserve the user's indentation choice entirely — they may align chains differently.
            if (originalNode.Expression is InvocationExpressionSyntax)
                return node;

            // For non-invocation expressions (e.g., identifier.Method()) that are direct children
            // of an assignment or return (simple pipeline context), re-indent the dot.
            // In other contexts (collection expressions, lambda bodies, etc.), preserve the layout.
            if (originalNode.Parent is InvocationExpressionSyntax
                && IsInSimplePipelineContext(originalNode))
            {
                var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

                // Strip trailing EndOfLine from expression to avoid doubling line feeds.
                var exprTrailingTrivia = node.Expression.GetTrailingTrivia();

                var strippedTrailing =
                    new SyntaxTriviaList(
                        exprTrailingTrivia.Where(t => !t.IsKind(SyntaxKind.EndOfLineTrivia)));

                return
                    node
                    .WithExpression(node.Expression.WithTrailingTrivia(strippedTrailing))
                    .WithOperatorToken(
                        node.OperatorToken.WithLeadingTrivia(
                            new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)));
            }

            return node;
        }

        // Only introduce new line breaks for method chains: expression is an invocation and
        // the member access itself is also used as part of another invocation.
        // Don't break for simple property accesses like .FullName
        if (originalNode.Expression is not InvocationExpressionSyntax ||
            originalNode.Parent is not InvocationExpressionSyntax)
            return node;

        // Per spec: break before dot in member access when line exceeds MaximumLineLength.
        var linePosition = originalNode.GetLocation().GetLineSpan().StartLinePosition;
        var sourceText = originalNode.SyntaxTree.GetText();
        var lineText = sourceText.Lines[linePosition.Line].ToString();
        var lineWidth = lineText.TrimEnd().Length;

        if (lineWidth <= MaximumLineLength)
            return node;

        var breakIndentTrivia = ComputeIndentationTriviaForNode(originalNode.Expression, addIndentLevels: 1);

        return
            node
            .WithOperatorToken(
                node.OperatorToken.WithLeadingTrivia(
                    new SyntaxTriviaList(SyntaxFactory.LineFeed, breakIndentTrivia)));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitCollectionExpression(CollectionExpressionSyntax originalNode)
    {
        var node = (CollectionExpressionSyntax)base.VisitCollectionExpression(originalNode)!;

        // Check if the original code already had elements spanning multiple lines.
        var originalElementsSpanMultipleLines =
            originalNode.Elements.Count > 0 &&
            originalNode.Elements.First().GetLocation().GetLineSpan().StartLinePosition.Line !=
            originalNode.Elements.Last().GetLocation().GetLineSpan().EndLinePosition.Line;

        if (!originalElementsSpanMultipleLines)
        {
            // Simple collection - keep all elements on the same line with proper spacing
            var elementsWithSpacing =
                node.Elements.Select((element, index) =>
                {
                    var trailingComments = StripWhitespaceTrivia(element.GetTrailingTrivia());

                    if (index is 0)
                        return element.WithLeadingTrivia().WithTrailingTrivia(trailingComments);

                    return element.WithLeadingTrivia(s_singleSpace).WithTrailingTrivia(trailingComments);
                });

            // For simple collections, preserve any existing leading trivia.
            // Preserve trailing comma if the original had one.
            var singleLineResult =
                node
                .WithElements(
                    RebuildSeparatedListPreservingSeparators(originalNode.Elements, [.. elementsWithSpacing]))
                .WithCloseBracketToken(node.CloseBracketToken.WithLeadingTrivia());

            // Check if single-line version exceeds MaximumLineLength
            var indentColumn = ComputeIndentationLevel(originalNode) * indentCharsPerLevel;
            var lineEndColumn = indentColumn + singleLineResult.ToFullString().Length;

            if (lineEndColumn <= MaximumLineLength)
                return singleLineResult;
            // Single-line version exceeds MaximumLineLength — fall through to multi-line formatting.
        }

        {
            // Multi-line collection - each element and brackets on their own line.
            // Check if any formatted element spans multiple lines (e.g., nested invocations).
            // Uses ToString() because these are rewritten nodes where SpansMultipleLines()
            // would return original source positions, not the reformatted layout.
            var elementIndentationTrivia =
                ComputeIndentationTriviaForNode(originalNode);

            var bracketIndentationTrivia =
                ComputeIndentationTriviaForNode(originalNode);

            // When the collection is a direct argument in a function call,
            // the close bracket aligns with the open bracket and elements.
            var closeBracketIndentTrivia = bracketIndentationTrivia;

            var formattedElements =
                node.Elements.Select((element, index) =>
                {
                    var originalElement = originalNode.Elements[index];
                    var trailingComments = StripWhitespaceTrivia(originalElement.GetTrailingTrivia());

                    // Preserve leading comments on the element, with LineFeed prefix for multi-line layout
                    var leadingTrivia =
                        HasPreservableTrivia(originalElement.GetLeadingTrivia())
                        ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                            .AddRange(BuildLeadingTriviaPreservingComments(
                                originalElement.GetLeadingTrivia(),
                                elementIndentationTrivia))
                        :
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, elementIndentationTrivia);

                    return
                        element
                        .WithLeadingTrivia(leadingTrivia)
                        .WithTrailingTrivia(trailingComments);
                }).ToList();

            var elementsOnSeparateLines =
                RebuildSeparatedListPreservingSeparators(originalNode.Elements, formattedElements);

            // Check if the token before the open bracket already ends with a newline
            // (e.g., when the collection is inside a parenthesized expression like `(\n[`).
            // If so, don't add another LineFeed to avoid double newlines.
            var prevToken = originalNode.OpenBracketToken.GetPreviousToken();

            var prevEndsWithNewline =
                prevToken.TrailingTrivia.Any(t => t.IsKind(SyntaxKind.EndOfLineTrivia));

            var openBracketLeading =
                prevEndsWithNewline
                ?
                new SyntaxTriviaList(bracketIndentationTrivia)
                :
                new SyntaxTriviaList(SyntaxFactory.LineFeed, bracketIndentationTrivia);

            return
                node
                .WithOpenBracketToken(
                    node.OpenBracketToken
                        .WithLeadingTrivia(openBracketLeading)
                        .WithTrailingTrivia())
                .WithElements(elementsOnSeparateLines)
                .WithCloseBracketToken(
                    node.CloseBracketToken
                        .WithLeadingTrivia(SyntaxFactory.LineFeed, closeBracketIndentTrivia)
                        .WithTrailingTrivia());
        }
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitInitializerExpression(InitializerExpressionSyntax originalNode)
    {
        var node = (InitializerExpressionSyntax)base.VisitInitializerExpression(originalNode)!;

        // Only handle multi-line initializers (object/collection initializers with braces)
        if (!SpansMultipleLines(originalNode))
            return node;

        var braceIndentTrivia = ComputeIndentationTriviaForNode(originalNode);
        var contentIndentTrivia = ComputeIndentationTriviaForNode(originalNode, addIndentLevels: 1);

        // Indent each expression in the initializer
        var newExpressions = new SyntaxList<ExpressionSyntax>();

        for (var i = 0; i < node.Expressions.Count; i++)
        {
            var expr = node.Expressions[i];
            var originalExpr = originalNode.Expressions[i];

            var exprLeading =
                HasPreservableTrivia(originalExpr.GetLeadingTrivia())
                ?
                new SyntaxTriviaList(SyntaxFactory.LineFeed)
                    .AddRange(BuildLeadingTriviaPreservingComments(originalExpr.GetLeadingTrivia(), contentIndentTrivia))
                :
                new SyntaxTriviaList(SyntaxFactory.LineFeed, contentIndentTrivia);

            newExpressions =
                newExpressions.Add(
                    expr
                .WithLeadingTrivia(exprLeading)
                .WithTrailingTrivia(StripWhitespaceTrivia(originalExpr.GetTrailingTrivia())));
        }

        // Rebuild separated list preserving separators (commas)
        var newSeparatedList =
            RebuildSeparatedListPreservingSeparators(
                node.Expressions,
                newExpressions);

        // Always use LineFeed + indent for both braces.
        // Expression trailing newlines are stripped above, so no double-newline risk.
        return
            node
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, braceIndentTrivia)
                .WithTrailingTrivia())
            .WithExpressions(newSeparatedList)
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, braceIndentTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitObjectCreationExpression(ObjectCreationExpressionSyntax originalNode)
    {
        var node = (ObjectCreationExpressionSyntax)base.VisitObjectCreationExpression(originalNode)!;

        // When the object creation has a multi-line initializer, strip trailing newlines
        // from the type/argument list so that VisitInitializerExpression's open brace
        // newline doesn't create a double newline.
        if (node.Initializer is not null && SpansMultipleLines(originalNode))
        {
            // The token before the initializer's `{` is either `)` (from argument list)
            // or the type's last token (like `>`). Strip its trailing whitespace/newlines.
            if (node.ArgumentList is not null)
            {
                node =
                    node.WithArgumentList(
                        node.ArgumentList.WithCloseParenToken(
                            node.ArgumentList.CloseParenToken.WithTrailingTrivia()));
            }
            else
            {
                node = node.WithType(node.Type.WithTrailingTrivia());
            }
        }

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitAssignmentExpression(AssignmentExpressionSyntax originalNode)
    {
        var node = (AssignmentExpressionSyntax)base.VisitAssignmentExpression(originalNode)!;

        var placement =
            DetermineOperatorNewlinePlacement(
                node.Right,
                originalNode.OperatorToken,
                originalNode.Right);

        if (placement is OperatorNewlinePlacement.SameLine)
        {
            return
                node
                .WithLeft(node.Left.WithTrailingTrivia(s_singleSpace))
                .WithOperatorToken(node.OperatorToken.WithTrailingTrivia(s_singleSpace))
                .WithRight(node.Right.WithLeadingTrivia());
        }

        // Inside initializer expressions (e.g., dictionary initializer { [key] = value }),
        // the RHS should be at the same indent level as the LHS, not indented further.
        var rhsIndentationTrivia =
            originalNode.Parent is InitializerExpressionSyntax
            ?
            ComputeIndentationTriviaForNode(originalNode)
            :
            ComputeIndentationTriviaForNode(originalNode.Right);

        return
            node
            .WithLeft(node.Left.WithTrailingTrivia(s_singleSpace))
            .WithOperatorToken(node.OperatorToken.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithRight(node.Right.WithLeadingTrivia(rhsIndentationTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitBinaryExpression(BinaryExpressionSyntax originalNode)
    {
        var node = (BinaryExpressionSyntax)base.VisitBinaryExpression(originalNode)!;

        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitEqualsValueClause(EqualsValueClauseSyntax originalNode)
    {
        var node = (EqualsValueClauseSyntax)base.VisitEqualsValueClause(originalNode)!;

        var placement =
            DetermineOperatorNewlinePlacement(
                node.Value,
                originalNode.EqualsToken,
                originalNode.Value);

        // Rule 4: When the line exceeds MaximumLineLength, break before the initializer.
        if (placement is OperatorNewlinePlacement.SameLine)
        {
            var equalsCol = originalNode.EqualsToken.GetLocation().GetLineSpan().StartLinePosition.Character;
            var valueText = node.Value.ToString();

            // Estimate single-line width: column of '=' + " = " (3 chars) + value text + ";" (1 char)
            if (equalsCol + 3 + valueText.Length + 1 > MaximumLineLength)
                placement = OperatorNewlinePlacement.NewLine;
        }

        if (placement is OperatorNewlinePlacement.SameLine)
        {
            return
                node
                .WithEqualsToken(node.EqualsToken.WithTrailingTrivia(s_singleSpace))
                .WithValue(node.Value.WithLeadingTrivia());
        }

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Value);

        return
            node
            .WithEqualsToken(node.EqualsToken.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithValue(node.Value.WithLeadingTrivia(indentationTrivia));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax originalNode)
    {
        var node = (ReturnStatementSyntax)base.VisitReturnStatement(originalNode)!;

        if (node.Expression is not { } expression)
            return node;

        // Decide whether to put the expression on a new line based on the ORIGINAL expression.
        // Using original positions ensures the decision converges after at most one format pass:
        // once the expression is multi-line (from source or after first format), it stays that way.
        var putOnNewLine =
            originalNode.Expression is { } origExpr && SpansMultipleLines(origExpr);

        if (!putOnNewLine)
        {
            // Also check if the single-line return would exceed MaximumLineLength.
            var returnLevel = ComputeIndentationLevel(originalNode);

            var lineWidth =
                returnLevel * indentCharsPerLevel +
                "return ".Length +
                node.Expression.ToFullString().TrimEnd().Length + 1; // +1 for semicolon

            if (lineWidth > MaximumLineLength)
                putOnNewLine = true;
        }

        if (putOnNewLine)
        {
            var indentationTrivia = ComputeIndentationTriviaForNode(originalNode, addIndentLevels: 1);

            var expressionLeading =
                HasPreservableTrivia(originalNode.Expression!.GetLeadingTrivia())
                ?
                new SyntaxTriviaList(SyntaxFactory.LineFeed)
                    .AddRange(
                    BuildLeadingTriviaPreservingComments(originalNode.Expression!.GetLeadingTrivia(), indentationTrivia))
                :
                new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia);

            return
                node
                .WithReturnKeyword(node.ReturnKeyword.WithTrailingTrivia())
                .WithExpression(expression.WithLeadingTrivia(expressionLeading));
        }

        // Keep simple expressions on the same line
        return node;
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitSwitchExpressionArm(SwitchExpressionArmSyntax originalNode)
    {
        var node = (SwitchExpressionArmSyntax)base.VisitSwitchExpressionArm(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        var isLastArm =
            originalNode.Parent is SwitchExpressionSyntax switchExpression &&
            switchExpression.Arms.Last() == originalNode;

        // Preserve comments in leading trivia of the pattern (e.g. // comment before arm)
        var patternLeading =
            BuildLeadingTriviaPreservingComments(
                originalNode.Pattern.GetLeadingTrivia(),
                indentationTrivia);

        // Per spec:
        // - "throw" expressions must be placed on a new line after the pattern and arrow.
        // - If the arm spans multiple lines, the expression must start on a new line.
        // - Before the expression part of a switch arm when line exceeds MaximumLineLength.
        // - Otherwise, preserve the user's original formatting choice.
        var putExpressionOnNewLine =
            node.Expression is ThrowExpressionSyntax ||
            SpansMultipleLines(originalNode) ||
            originalNode.EqualsGreaterThanToken.TrailingTrivia.Any(t => t.IsKind(SyntaxKind.EndOfLineTrivia));

        if (!putExpressionOnNewLine)
        {
            var indentLevel = ComputeIndentationLevel(originalNode);
            var armWidth = indentLevel * indentCharsPerLevel + originalNode.Span.Length;

            if (armWidth > MaximumLineLength)
                putExpressionOnNewLine = true;
        }

        var arrowTrailing =
            putExpressionOnNewLine
            ?
            SyntaxFactory.LineFeed
            :
            s_singleSpace;

        var expressionLeading =
            putExpressionOnNewLine
            ?
            BuildLeadingTriviaPreservingComments(
                originalNode.Expression.GetLeadingTrivia(),
                indentationTrivia)
            :
            StripWhitespaceTrivia(originalNode.Expression.GetLeadingTrivia());

        return
            node
            .WithPattern(
                node.Pattern.WithLeadingTrivia(patternLeading))
            .WithEqualsGreaterThanToken(
                node.EqualsGreaterThanToken
                .WithTrailingTrivia(arrowTrailing))
            .WithExpression(
                node.Expression
                .WithLeadingTrivia(expressionLeading)
                .WithTrailingTrivia());
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitSwitchExpression(SwitchExpressionSyntax originalNode)
    {
        var node = (SwitchExpressionSyntax)base.VisitSwitchExpression(originalNode)!;

        var braceIndentTrivia = ComputeIndentationTriviaForNode(originalNode);

        var armsList =
            Enumerable
            .Range(0, node.Arms.Count - 1)
            .Aggregate(
                seed: node.Arms,
                func: (aggregate, separatorIndex) =>
                {
                    var separatorBefore = aggregate.GetSeparator(separatorIndex);

                    // Preserve any trailing comments on the separator
                    var separatorComments = StripWhitespaceTrivia(separatorBefore.TrailingTrivia);

                    // Per spec:
                    // - Multi-line arms must be separated from other arms by an empty line.
                    // - DiscardPattern arms must always be separated from other arms by an empty line.
                    var currentArm = originalNode.Arms[separatorIndex];
                    var nextArm = originalNode.Arms[separatorIndex + 1];

                    var currentArmIsMultiLine = SpansMultipleLines(currentArm);
                    var nextArmIsMultiLine = SpansMultipleLines(nextArm);
                    var nextArmIsDiscard = nextArm.Pattern is DiscardPatternSyntax;

                    var forceBlankLine =
                        currentArmIsMultiLine || nextArmIsMultiLine || nextArmIsDiscard;

                    if (forceBlankLine)
                    {
                        var newTrivia = new List<SyntaxTrivia>();
                        newTrivia.AddRange(separatorComments);
                        newTrivia.Add(SyntaxFactory.LineFeed);
                        newTrivia.Add(SyntaxFactory.LineFeed);

                        return
                            aggregate.ReplaceSeparator(
                                separatorBefore,
                                separatorBefore.WithTrailingTrivia(newTrivia));
                    }

                    // For single-line non-discard arms, preserve original blank line behavior
                    var currentArmEndLine =
                        currentArm.GetLocation().GetLineSpan().EndLinePosition.Line;

                    var nextArmStartLine =
                        nextArm.GetLocation().GetLineSpan().StartLinePosition.Line;

                    var lineGap = nextArmStartLine - currentArmEndLine;

                    var preservedTrivia = new List<SyntaxTrivia>();
                    preservedTrivia.AddRange(separatorComments);

                    for (var i = 0; i < Math.Max(1, lineGap); i++)
                        preservedTrivia.Add(SyntaxFactory.LineFeed);

                    return
                        aggregate.ReplaceSeparator(
                            separatorBefore,
                            separatorBefore.WithTrailingTrivia(preservedTrivia));
                });

        // Handle trailing separator (after last arm) if present:
        // normalize its trivia to just a LineFeed before the close brace.
        var hasTrailingSeparator = armsList.SeparatorCount == armsList.Count && armsList.Count > 0;

        if (hasTrailingSeparator)
        {
            var trailingSep = armsList.GetSeparator(armsList.Count - 1);
            armsList = armsList.ReplaceSeparator(
                trailingSep,
                trailingSep.WithTrailingTrivia(SyntaxFactory.LineFeed));
        }

        // If no trailing separator, prefix close brace with LineFeed so there's
        // a newline between the last arm and the close brace.
        var closeBraceLeading = hasTrailingSeparator
            ? new SyntaxTriviaList(braceIndentTrivia)
            : new SyntaxTriviaList(SyntaxFactory.LineFeed, braceIndentTrivia);

        return
            node
            .WithGoverningExpression(
                node.GoverningExpression.WithTrailingTrivia(s_singleSpace))
            .WithSwitchKeyword(
                node.SwitchKeyword.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(braceIndentTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithArms(armsList)
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(closeBraceLeading));
    }


    /// <inheritdoc/>
    public override SyntaxNode? VisitParameterList(ParameterListSyntax originalNode)
    {
        var node = (ParameterListSyntax)base.VisitParameterList(originalNode)!;

        // When the parameter list belongs to a type declaration (e.g., record primary constructor),
        // the type declaration already increases indentation. Don't add an extra level.
        var addIndentLevels = originalNode.Parent is TypeDeclarationSyntax ? 0 : 1;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode, addIndentLevels: addIndentLevels);

        // Only add line breaks if the original code already had parameters on multiple lines
        // (preserve user's choice), or if the single-line version exceeds MaximumLineLength.
        var formatOnMultipleLines = SpansMultipleLines(originalNode);

        if (!formatOnMultipleLines)
        {
            // Keep parameters on single line, but normalize whitespace
            var parameters =
                node.Parameters
                .Select((parameter, index) =>
                {
                    var originalParam = originalNode.Parameters[index];
                    var trailingComments = StripWhitespaceTrivia(originalParam.GetTrailingTrivia());

                    // Normalize internal whitespace: single space between type and identifier
                    var normalizedParam = parameter;

                    if (normalizedParam.Type is not null)
                    {
                        normalizedParam =
                            normalizedParam
                            .WithType(normalizedParam.Type.WithTrailingTrivia(s_singleSpace));
                    }

                    normalizedParam =
                        normalizedParam
                        .WithIdentifier(normalizedParam.Identifier.WithLeadingTrivia());

                    if (index is 0)
                        return normalizedParam.WithLeadingTrivia().WithTrailingTrivia(trailingComments);
                    else
                        return normalizedParam.WithLeadingTrivia(s_singleSpace).WithTrailingTrivia(trailingComments);
                })
                .ToImmutableArray();

            var singleLineResult =
                node
                .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
                .WithParameters(
                    RebuildSeparatedListPreservingSeparators(originalNode.Parameters, [.. parameters]))
                .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia());

            // Check against MaximumLineLength, same pattern as VisitArgumentList.
            var indentColumn = ComputeIndentationLevel(originalNode) * indentCharsPerLevel;

            var prefixWidth =
                originalNode.Parent switch
                {
                    MethodDeclarationSyntax method =>
                    method.ReturnType.Span.Length + 1 + method.Identifier.Span.Length,

                    ConstructorDeclarationSyntax ctor =>
                    ctor.Identifier.Span.Length,

                    _ => 0
                };

            var lineEndColumn = indentColumn + prefixWidth + singleLineResult.ToFullString().Length;

            if (lineEndColumn <= MaximumLineLength)
                return singleLineResult;
            // Single-line version exceeds MaximumLineLength — fall through to multi-line formatting.
        }

        {
            var paramLeadingTrivia =
                new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia);

            var parameters =
                node.Parameters
                .Select((parameter, index) =>
                {
                    var originalParam = originalNode.Parameters[index];
                    var trailingComments = StripWhitespaceTrivia(originalParam.GetTrailingTrivia());

                    // Preserve leading comments on the parameter, with LineFeed prefix for multi-line layout
                    var leadingTrivia =
                        HasPreservableTrivia(originalParam.GetLeadingTrivia())
                        ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                            .AddRange(
                            BuildLeadingTriviaPreservingComments(originalParam.GetLeadingTrivia(), indentationTrivia))
                        :
                        paramLeadingTrivia;

                    return
                        parameter
                        .WithLeadingTrivia(leadingTrivia)
                        .WithTrailingTrivia(trailingComments);
                })
                .ToImmutableArray();

            var result =
                node.WithParameters(
                    RebuildSeparatedListPreservingSeparators(originalNode.Parameters, [.. parameters]));

            // Ensure the open paren's trailing trivia doesn't also contain a newline
            result =
                result.WithOpenParenToken(
                    result.OpenParenToken.WithTrailingTrivia());

            // Strip whitespace-only leading trivia from close paren to avoid leftover alignment spaces.
            var closeParenLeading =
                HasPreservableTrivia(result.CloseParenToken.LeadingTrivia)
                ?
                StripWhitespaceTrivia(result.CloseParenToken.LeadingTrivia)
                :
                [];

            return result.WithCloseParenToken(result.CloseParenToken.WithLeadingTrivia(closeParenLeading));
        }
    }


    /// <summary>
    /// Checks whether a rewritten expression actually spans multiple lines.
    /// Called on expressions that have already been visited/reformatted by child visitors,
    /// so we walk descendant trivia for <see cref="SyntaxKind.EndOfLineTrivia"/> rather
    /// than materializing the full text with <c>ToString()</c>.
    /// </summary>
    private static bool ExpressionCausesLineBreak(ExpressionSyntax expression) =>
        expression.WithTrailingTrivia().DescendantTrivia().Any(t => t.IsKind(SyntaxKind.EndOfLineTrivia));


    /// <summary>
    /// Checks if the given member access expression is in a "simple pipeline" context —
    /// meaning it's part of a direct assignment RHS, return statement, or standalone expression,
    /// rather than nested inside a collection expression, lambda body, or other complex context.
    /// </summary>
    private static bool IsInSimplePipelineContext(MemberAccessExpressionSyntax memberAccess)
    {
        // Walk up to the invocation expression that uses this member access.
        var current = memberAccess.Parent;

        while (current is not null)
        {
            if (current is AssignmentExpressionSyntax or ReturnStatementSyntax or ExpressionStatementSyntax)
                return true;

            if (current is CollectionExpressionSyntax or LambdaExpressionSyntax or ArgumentListSyntax)
                return false;

            current = current.Parent;
        }

        return false;
    }


    /// <summary>
    /// Classification for where a newline should be placed relative to an operator token
    /// (e.g. '=' in assignments or initializers).
    /// </summary>
    private enum OperatorNewlinePlacement
    {
        /// <summary>The RHS expression stays on the same line as the operator.</summary>
        SameLine,

        /// <summary>The RHS expression starts on a new line after the operator.</summary>
        NewLine,
    }


    /// <summary>
    /// Determines whether the RHS of an assignment or initializer should be on a new line.
    /// Three-step decision shared by <see cref="VisitAssignmentExpression"/> and <see cref="VisitEqualsValueClause"/>:
    /// <list type="number">
    /// <item>If the formatted RHS causes line breaks → force newline.</item>
    /// <item>If the original had a newline after the operator → preserve it.</item>
    /// <item>Otherwise → keep on the same line.</item>
    /// </list>
    /// </summary>
    private static OperatorNewlinePlacement DetermineOperatorNewlinePlacement(
        ExpressionSyntax formattedRhs,
        SyntaxToken originalOperator,
        ExpressionSyntax originalRhs)
    {
        if (ExpressionCausesLineBreak(formattedRhs))
            return OperatorNewlinePlacement.NewLine;

        var originalHasNewlineAfterOperator =
            originalOperator.TrailingTrivia.Any(t => t.IsKind(SyntaxKind.EndOfLineTrivia)) ||
            originalRhs.GetLeadingTrivia().Any(t => t.IsKind(SyntaxKind.EndOfLineTrivia));

        if (originalHasNewlineAfterOperator)
            return OperatorNewlinePlacement.NewLine;

        return OperatorNewlinePlacement.SameLine;
    }


    /// <summary>
    /// Checks if a syntax node spans multiple lines in the original source text
    /// by comparing its start and end line positions.
    /// </summary>
    private static bool SpansMultipleLines(SyntaxNode node)
    {
        var lineSpan = node.GetLocation().GetLineSpan();

        return lineSpan.StartLinePosition.Line != lineSpan.EndLinePosition.Line;
    }


    /// <summary>
    /// Computes indentation trivia for the line containing the specified <paramref name="node"/>,
    /// optionally adding extra indentation levels.
    /// </summary>
    /// <param name="node">The node to compute indentation for. If <c>null</c>, zero indentation is returned.</param>
    /// <param name="addIndentLevels">Additional indentation levels to add on top of the computed level.</param>
    /// <returns>A <see cref="SyntaxTrivia"/> representing indentation whitespace.</returns>
    public SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode? node, int addIndentLevels = 0) =>
        ComputeIndentationTriviaForNode(
            node,
            indentChar,
            indentCharsPerLevel,
            addIndentLevels: addIndentLevels);


    /// <summary>
    /// Computes indentation trivia for the line containing the specified <paramref name="node"/> using
    /// the given indentation settings and optional extra levels.
    /// </summary>
    /// <param name="node">The node to compute indentation for. If <c>null</c>, zero indentation is returned.</param>
    /// <param name="indentChar">The indentation character (e.g., space or tab).</param>
    /// <param name="indentCharsPerLevel">The number of <paramref name="indentChar"/> characters per level.</param>
    /// <param name="addIndentLevels">Additional indentation levels to add on top of the computed level.</param>
    /// <returns>A <see cref="SyntaxTrivia"/> representing indentation whitespace.</returns>
    public static SyntaxTrivia ComputeIndentationTriviaForNode(
        SyntaxNode? node,
        char indentChar,
        int indentCharsPerLevel,
        int addIndentLevels)
    {
        var indentationLevel = ComputeIndentationLevel(node);

        return
            IndentationTriviaForLevel(
                indentationLevel + addIndentLevels,
                indentChar,
                indentCharsPerLevel);
    }


    /// <summary>
    /// Computes indentation trivia for a given indentation <paramref name="level"/> using the instance's
    /// configured indentation settings.
    /// </summary>
    /// <param name="level">The indentation level.</param>
    /// <returns>A <see cref="SyntaxTrivia"/> representing indentation whitespace.</returns>
    public SyntaxTrivia ComputeIndentationTriviaForLevel(int level) =>
        IndentationTriviaForLevel(level, indentChar, indentCharsPerLevel);


    /// <summary>
    /// Creates indentation trivia for a given indentation <paramref name="level"/> using explicit settings.
    /// </summary>
    /// <param name="level">The indentation level.</param>
    /// <param name="indentChar">The indentation character (e.g., space or tab).</param>
    /// <param name="indentCharsPerLevel">The number of <paramref name="indentChar"/> characters per level.</param>
    /// <returns>A <see cref="SyntaxTrivia"/> representing indentation whitespace.</returns>
    public static SyntaxTrivia IndentationTriviaForLevel(int level, char indentChar, int indentCharsPerLevel) =>
        SyntaxFactory.Whitespace(IndentationTextForLevel(level, indentChar, indentCharsPerLevel));


    /// <summary>
    /// Builds an indentation string for a given indentation <paramref name="level"/> using explicit settings.
    /// </summary>
    /// <param name="level">The indentation level.</param>
    /// <param name="indentChar">The indentation character (e.g., space or tab).</param>
    /// <param name="indentCharsPerLevel">The number of <paramref name="indentChar"/> characters per level.</param>
    /// <returns>A string containing only indentation characters.</returns>
    public static string IndentationTextForLevel(int level, char indentChar, int indentCharsPerLevel) =>
        string.Intern(new(indentChar, level * indentCharsPerLevel));


    /// <summary>
    /// Computes the indentation level for the specified <paramref name="node"/> by walking up the parent
    /// chain and applying <see cref="ParentNodeIncreasesIndentationLevel(SyntaxNode)"/> to each ancestor.
    /// </summary>
    /// <param name="node">The node whose indentation level should be computed.</param>
    /// <param name="currentLevel">The current level used during recursion; callers should use the default.</param>
    /// <returns>The computed indentation level (non-negative).</returns>
    public static int ComputeIndentationLevel(SyntaxNode? node, int currentLevel = 0, bool insideChainArguments = false, bool chainSkipApplied = false)
    {
        if (node is null)
            return currentLevel;

        if (node.Parent is not { } parent)
            return currentLevel;

        var shouldIncrement = ParentNodeIncreasesIndentationLevel(parent);

        // When we're inside a chain's arguments (tracked by insideChainArguments flag),
        // skip the containing ArgumentList's +1 to prevent double-counting.
        if (shouldIncrement
            && insideChainArguments
            && parent is ArgumentListSyntax
            && node is ArgumentSyntax)
        {
            shouldIncrement = false;
            insideChainArguments = false;
            chainSkipApplied = true; // Mark that we've already applied a chain skip
        }

        // Detect when we walk through an InvocationExpression that's a chain (MemberAccess
        // with dot on separate line) from its ArgumentList side. Set the flag so the
        // containing ArgumentList will be skipped. Only set if no skip has been applied yet.
        if (!chainSkipApplied
            && node is ArgumentListSyntax
            && parent is InvocationExpressionSyntax
            {
                Expression: MemberAccessExpressionSyntax memberAccess
            })
        {
            var maExprEndLine = memberAccess.Expression.GetLocation().GetLineSpan().EndLinePosition.Line;
            var maDotLine = memberAccess.OperatorToken.GetLocation().GetLineSpan().StartLinePosition.Line;

            if (maExprEndLine != maDotLine)
            {
                insideChainArguments = true;
            }
        }

        if (shouldIncrement)
        {
            currentLevel++;
        }

        return ComputeIndentationLevel(parent, currentLevel, insideChainArguments, chainSkipApplied);
    }


    /// <summary>
    /// Determines whether the given <paramref name="node"/> increases the indentation level when formatting.
    /// This is used by <see cref="ComputeIndentationLevel(SyntaxNode?, int)"/> to decide how deep a node should be indented.
    /// </summary>
    /// <param name="node">The parent node to evaluate.</param>
    /// <returns><c>true</c> if the node increases indentation; otherwise, <c>false</c>.</returns>
    public static bool ParentNodeIncreasesIndentationLevel(SyntaxNode node) =>
        node switch
        {
            BlockSyntax => true,
            NamespaceDeclarationSyntax => true,
            ClassDeclarationSyntax => true,
            RecordDeclarationSyntax => true,
            InterfaceDeclarationSyntax => true,
            ArrowExpressionClauseSyntax => true,
            SwitchStatementSyntax => true,
            SwitchSectionSyntax => true,
            IfStatementSyntax => false,
            ElseClauseSyntax => false,

            AssignmentExpressionSyntax assignment =>
            assignment.Parent is not InitializerExpressionSyntax,

            WhileStatementSyntax => false,
            ForStatementSyntax => false,
            ForEachStatementSyntax => false,
            DoStatementSyntax => false,
            TryStatementSyntax => false,
            CatchClauseSyntax => false,
            FinallyClauseSyntax => false,
            UsingStatementSyntax => false,
            LockStatementSyntax => false,
            ArgumentListSyntax => true,

            ReturnStatementSyntax returnStatement =>
            returnStatement.Expression is not null,

            EqualsValueClauseSyntax => true,
            SwitchExpressionSyntax => true,
            InitializerExpressionSyntax => true,

            CollectionExpressionSyntax collectionExpression =>
            collectionExpression.Elements.Count > 0 &&
            collectionExpression.Parent is not ArgumentSyntax { Parent: ArgumentListSyntax },

            // Don't increase indentation for script context nodes
            CompilationUnitSyntax => false,
            GlobalStatementSyntax => false,

            _ => false
        };


    /// <summary>
    /// Builds trailing trivia for a node by preserving inline comment trivia from the original
    /// trailing trivia and appending the given base trailing trivia (line breaks).
    /// Whitespace before comments is normalized to a single space.
    /// </summary>
    private static SyntaxTriviaList BuildTrailingTriviaPreservingComments(
        SyntaxTriviaList originalTrailingTrivia,
        SyntaxTriviaList baseTrailingTrivia)
    {
        var hasTrailingComment =
            originalTrailingTrivia
            .Any(t =>
                t.IsKind(SyntaxKind.SingleLineCommentTrivia) ||
                t.IsKind(SyntaxKind.MultiLineCommentTrivia));

        if (!hasTrailingComment)
        {
            return baseTrailingTrivia;
        }

        var result = new List<SyntaxTrivia>();

        foreach (var trivia in originalTrailingTrivia)
        {
            if (trivia.IsKind(SyntaxKind.WhitespaceTrivia))
            {
                result.Add(s_singleSpace);
            }
            else if (trivia.IsKind(SyntaxKind.SingleLineCommentTrivia) ||
                     trivia.IsKind(SyntaxKind.MultiLineCommentTrivia))
            {
                result.Add(trivia);
            }
        }

        // End with the base trailing trivia (line breaks)
        result.AddRange(baseTrailingTrivia);

        return [.. result];
    }


    /// <summary>
    /// Builds leading trivia for a node by preserving comment trivia from the original leading trivia
    /// and re-indenting them with the given indentation trivia.
    /// When no preservable trivia exists, returns just the indentation trivia.
    /// </summary>
    private static SyntaxTriviaList BuildLeadingTriviaPreservingComments(
        SyntaxTriviaList originalLeadingTrivia,
        SyntaxTrivia indentationTrivia,
        SyntaxTrivia? finalIndentationTrivia = null,
        bool preserveLeadingBlankLine = false) =>
        BuildLeadingTriviaCore(
            originalLeadingTrivia,
            indentationTrivia,
            addBlankLineBefore: false,
            finalIndentationTrivia,
            preserveLeadingBlankLine);


    /// <summary>
    /// Builds leading trivia for a member declaration by preserving comment trivia
    /// from the original leading trivia and applying the given indentation.
    /// Optionally prepends a blank line before the first preserved trivia.
    /// </summary>
    private static SyntaxTriviaList BuildMemberLeadingTriviaPreservingComments(
        SyntaxTriviaList originalLeadingTrivia,
        SyntaxTrivia indentationTrivia,
        bool addBlankLineBefore,
        bool addExtraBlankLine = false)
    {
        var result = BuildLeadingTriviaCore(originalLeadingTrivia, indentationTrivia, addBlankLineBefore);

        if (addExtraBlankLine)
        {
            // Rule 6: Insert an additional blank line before type declarations.
            result = new SyntaxTriviaList(SyntaxFactory.LineFeed).AddRange(result);
        }

        return result;
    }


    /// <summary>
    /// Core implementation shared by <see cref="BuildLeadingTriviaPreservingComments"/> and
    /// <see cref="BuildMemberLeadingTriviaPreservingComments"/>.
    /// Walks the <paramref name="originalLeadingTrivia"/>, preserving comments, doc comments,
    /// pragma directives, regions, and nullable directives while re-indenting them.
    /// </summary>
    private static SyntaxTriviaList BuildLeadingTriviaCore(
        SyntaxTriviaList originalLeadingTrivia,
        SyntaxTrivia indentationTrivia,
        bool addBlankLineBefore,
        SyntaxTrivia? finalIndentationTrivia = null,
        bool preserveLeadingBlankLine = false)
    {
        var tokenIndentation = finalIndentationTrivia ?? indentationTrivia;

        if (!HasPreservableTrivia(originalLeadingTrivia))
        {
            if (addBlankLineBefore)
                return new SyntaxTriviaList(SyntaxFactory.LineFeed, tokenIndentation);

            return new SyntaxTriviaList(tokenIndentation);
        }

        var result = new List<SyntaxTrivia>();

        if (addBlankLineBefore)
            result.Add(SyntaxFactory.LineFeed);

        // Walk through original trivia, keeping comments and their associated line breaks,
        // but replacing whitespace with our indentation.
        // Also preserve blank lines between comment groups.
        var foundFirstComment = false;
        var consecutiveEolCount = 0;

        for (var i = 0; i < originalLeadingTrivia.Count; i++)
        {
            var trivia = originalLeadingTrivia[i];

            if (trivia.IsKind(SyntaxKind.SingleLineCommentTrivia) ||
                trivia.IsKind(SyntaxKind.MultiLineCommentTrivia))
            {
                // When preserveLeadingBlankLine is set, preserve blank lines that appeared
                // before the first comment. A single EOL in leading trivia before a comment
                // indicates a blank line, because the previous token's trailing trivia
                // already contains its own EOL.
                if (preserveLeadingBlankLine && !foundFirstComment && consecutiveEolCount >= 1)
                    result.Add(SyntaxFactory.LineFeed);

                foundFirstComment = true;
                result.Add(indentationTrivia);
                result.Add(trivia);
                result.Add(SyntaxFactory.LineFeed);

                // Skip the original end-of-line trivia if present
                if (i + 1 < originalLeadingTrivia.Count &&
                    originalLeadingTrivia[i + 1].IsKind(SyntaxKind.EndOfLineTrivia))
                {
                    i++;
                }

                // Preserve blank lines between comments
                i = EmitBlankLinesFromOriginal(originalLeadingTrivia, i, result);
            }
            else if (trivia.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia) ||
                     trivia.IsKind(SyntaxKind.MultiLineDocumentationCommentTrivia))
            {
                // Preserve blank lines that appeared before the first comment.
                if (preserveLeadingBlankLine && !foundFirstComment && consecutiveEolCount >= 1)
                    result.Add(SyntaxFactory.LineFeed);

                foundFirstComment = true;

                // Doc comments are structured trivia - preserve them as-is with proper indentation.
                // Ensure there's a LineFeed before the doc comment to prevent it from merging
                // with the previous trivia (e.g., after a multiline comment `*/`).
                if (result.Count > 0 && !result[^1].IsKind(SyntaxKind.EndOfLineTrivia))
                    result.Add(SyntaxFactory.LineFeed);

                result.Add(indentationTrivia);
                result.Add(trivia);

                if (i + 1 < originalLeadingTrivia.Count &&
                    originalLeadingTrivia[i + 1].IsKind(SyntaxKind.EndOfLineTrivia))
                {
                    i++; // skip the original end-of-line since doc comments include it
                }

                // Preserve blank lines between comments
                i = EmitBlankLinesFromOriginal(originalLeadingTrivia, i, result);
            }
            else if (IsPreservableTrivia(trivia))
            {
                // Preserve pragma directives, region directives, nullable directives, etc.
                result.Add(trivia);
                result.Add(SyntaxFactory.LineFeed);

                if (i + 1 < originalLeadingTrivia.Count &&
                    originalLeadingTrivia[i + 1].IsKind(SyntaxKind.EndOfLineTrivia))
                {
                    i++;
                }
            }
            else if (!foundFirstComment)
            {
                // Track consecutive EOL trivia before the first comment to detect blank lines
                if (trivia.IsKind(SyntaxKind.EndOfLineTrivia))
                    consecutiveEolCount++;
                else if (!trivia.IsKind(SyntaxKind.WhitespaceTrivia))
                    consecutiveEolCount = 0;
            }
        }

        // Always end with the indentation for the token itself
        result.Add(tokenIndentation);

        return [.. result];
    }


    /// <summary>
    /// After processing a comment in <see cref="BuildLeadingTriviaCore"/>, scans ahead
    /// past whitespace to detect blank lines (additional <see cref="SyntaxKind.EndOfLineTrivia"/>)
    /// in the original trivia. Emits one <see cref="SyntaxFactory.LineFeed"/> for each blank line found.
    /// Returns the updated index position.
    /// </summary>
    private static int EmitBlankLinesFromOriginal(
        SyntaxTriviaList originalLeadingTrivia,
        int currentIndex,
        List<SyntaxTrivia> result)
    {
        var j = currentIndex + 1;

        while (j < originalLeadingTrivia.Count)
        {
            if (originalLeadingTrivia[j].IsKind(SyntaxKind.WhitespaceTrivia))
            {
                j++;
                continue;
            }

            if (originalLeadingTrivia[j].IsKind(SyntaxKind.EndOfLineTrivia))
            {
                result.Add(SyntaxFactory.LineFeed);
                j++;
                currentIndex = j - 1;
                continue;
            }

            break;
        }

        return currentIndex;
    }


    /// <summary>
    /// Builds leading trivia for a member declaration by preserving comment trivia
    /// from the original leading trivia and applying the given indentation.
    /// </summary>
    private static bool HasPreservableTrivia(SyntaxTriviaList trivia) =>
        trivia.Any(IsPreservableTrivia);


    private static bool IsPreservableTrivia(SyntaxTrivia trivia) =>
        trivia.IsKind(SyntaxKind.SingleLineCommentTrivia) ||
        trivia.IsKind(SyntaxKind.MultiLineCommentTrivia) ||
        trivia.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia) ||
        trivia.IsKind(SyntaxKind.MultiLineDocumentationCommentTrivia) ||
        trivia.IsKind(SyntaxKind.PragmaWarningDirectiveTrivia) ||
        trivia.IsKind(SyntaxKind.PragmaChecksumDirectiveTrivia) ||
        trivia.IsKind(SyntaxKind.RegionDirectiveTrivia) ||
        trivia.IsKind(SyntaxKind.EndRegionDirectiveTrivia) ||
        trivia.IsKind(SyntaxKind.NullableDirectiveTrivia);


    /// <summary>
    /// Extracts the whitespace trivia that immediately precedes the node in the original source.
    /// Returns <c>null</c> if no leading whitespace is found.
    /// </summary>
    private static SyntaxTrivia? GetOriginalLeadingWhitespace(SyntaxNode node)
    {
        var leading = node.GetLeadingTrivia();

        for (var i = leading.Count - 1; i >= 0; i--)
        {
            if (leading[i].IsKind(SyntaxKind.WhitespaceTrivia))
                return leading[i];
        }

        return null;
    }


    /// <summary>
    /// Returns <c>true</c> when the argument list has its first argument starting on the same
    /// line as the open paren (indicating an inline argument layout).
    /// </summary>
    private static bool IsInlineLambdaArgumentList(ArgumentListSyntax argList) =>
        argList.Arguments.Count > 0 &&
        argList.OpenParenToken.GetLocation().GetLineSpan().StartLinePosition.Line ==
        argList.Arguments[0].GetLocation().GetLineSpan().StartLinePosition.Line;


    /// <summary>
    /// Returns a trivia list with whitespace and end-of-line trivia removed,
    /// but comments and other significant trivia preserved.
    /// </summary>
    private static SyntaxTriviaList StripWhitespaceTrivia(SyntaxTriviaList originalTrivia)
    {
        var preserved =
            originalTrivia.Where(t =>
            !t.IsKind(SyntaxKind.WhitespaceTrivia) &&
            !t.IsKind(SyntaxKind.EndOfLineTrivia));

        return [.. preserved];
    }


    /// <summary>
    /// Rebuilds a <see cref="SeparatedSyntaxList{TNode}"/> with new nodes but preserving
    /// the separator count from the original list (including any trailing comma/separator)
    /// and preserving comments on separators.
    /// </summary>
    private static SeparatedSyntaxList<T> RebuildSeparatedListPreservingSeparators<T>(
        SeparatedSyntaxList<T> originalList,
        IReadOnlyList<T> newNodes)
        where T : SyntaxNode
    {
        var originalSeparators = originalList.GetSeparators().ToList();

        // If separator count == element count, there's a trailing separator.
        // SyntaxFactory.SeparatedList only creates N-1 separators, so we need
        // to build the list manually to preserve the trailing one.
        var nodesAndTokens = new List<SyntaxNodeOrToken>();

        for (var i = 0; i < newNodes.Count; i++)
        {
            nodesAndTokens.Add(newNodes[i]);

            if (i < originalSeparators.Count)
            {
                // Preserve the original separator token, including any trailing comments.
                var separator = originalSeparators[i];
                var trailingComments = StripWhitespaceTrivia(separator.TrailingTrivia);

                nodesAndTokens.Add(
                    separator
                    .WithLeadingTrivia()
                    .WithTrailingTrivia(trailingComments));
            }
        }

        return SyntaxFactory.SeparatedList<T>(nodesAndTokens);
    }


    /// <summary>
    /// Determines if an expression is "simple" for the formatting rules - 
    /// literals, identifiers, and simple member access (like Class.Property).
    /// </summary>
    private static bool IsSimpleExpression(ExpressionSyntax expression)
    {
        return
            expression switch
            {
                LiteralExpressionSyntax => true,

                IdentifierNameSyntax => true,

                DeclarationExpressionSyntax => true,

                InvocationExpressionSyntax invocation =>
                invocation.ArgumentList.Arguments.Count is 0 && IsSimpleExpression(invocation.Expression),

                MemberAccessExpressionSyntax memberAccess =>
                IsSimpleExpression(memberAccess.Expression),

                ParenthesizedExpressionSyntax parenthesizedExpr =>
                IsSimpleExpression(parenthesizedExpr.Expression),

                QualifiedNameSyntax qualifiedName =>
                IsSimpleExpression(qualifiedName.Left),

                TupleExpressionSyntax tupleExpr =>
                tupleExpr.Arguments.All(arg => IsSimpleExpression(arg.Expression)),

                ElementAccessExpressionSyntax elementAccess =>
                IsSimpleExpression(elementAccess.Expression) &&
            elementAccess.ArgumentList.Arguments.All(arg => IsSimpleExpression(arg.Expression)),

                _ =>
                false
            };
    }
}
