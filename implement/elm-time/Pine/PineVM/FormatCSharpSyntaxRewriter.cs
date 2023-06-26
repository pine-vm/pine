using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Linq;

namespace Pine.PineVM;

public class FormatCSharpSyntaxRewriter : CSharpSyntaxRewriter
{
    private readonly char indentChar;
    private readonly int indentCharsPerLevel;

    public FormatCSharpSyntaxRewriter()
        : this(
              indentChar: ' ',
              indentCharsPerLevel: 4)
    {
    }

    public FormatCSharpSyntaxRewriter(
        char indentChar,
        int indentCharsPerLevel)
    {
        this.indentChar = indentChar;
        this.indentCharsPerLevel = indentCharsPerLevel;
    }

    public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax nodeBeforeRewriteInner)
    {
        var node = (InvocationExpressionSyntax)base.VisitInvocationExpression(nodeBeforeRewriteInner)!;

        if (node.ArgumentList.Arguments.Count < 1 && node == nodeBeforeRewriteInner)
            return node;

        var indentationTrivia = ComputeIndentationTriviaForNode(nodeBeforeRewriteInner.ArgumentList);

        var newOpenParenToken = node.ArgumentList.OpenParenToken
            .WithTrailingTrivia(
            node.ArgumentList.OpenParenToken.TrailingTrivia
            .Add(SyntaxFactory.LineFeed)
            .Add(indentationTrivia));

        var newArgumentList = node.ArgumentList.WithOpenParenToken(newOpenParenToken);

        var newInvocationExpression = node.WithArgumentList(newArgumentList);

        return newInvocationExpression;
    }

    public override SyntaxNode? VisitArrowExpressionClause(ArrowExpressionClauseSyntax originalNode)
    {
        var node = (ArrowExpressionClauseSyntax)base.VisitArrowExpressionClause(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node
            .WithArrowToken(node.ArrowToken.WithTrailingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
    }

    public override SyntaxNode? VisitConditionalExpression(ConditionalExpressionSyntax originalNode)
    {
        var node = (ConditionalExpressionSyntax)base.VisitConditionalExpression(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node
            .WithQuestionToken(
                node.QuestionToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed, indentationTrivia))
            .WithColonToken(
                node.ColonToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
    }

    public override SyntaxNode? VisitBlock(BlockSyntax originalNode)
    {
        var node = (BlockSyntax)base.VisitBlock(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        return
            node
            .WithStatements(
                new SyntaxList<StatementSyntax>(
                    node.Statements.Select((statement, statementIndex) =>
                    {
                        var trailingTrivia =
                        node.Statements.Count <= statementIndex + 1 ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                        :
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed);

                        return
                        statement.WithLeadingTrivia(indentationTrivia)
                        .WithTrailingTrivia(trailingTrivia);
                    })));
    }

    public SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode node) =>
        ComputeIndentationTriviaForNode(node, indentChar, indentCharsPerLevel);

    public static SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode node, char indentChar, int indentCharsPerLevel)
    {
        var indentationLevel = ComputeIndentationLevel(node);

        var indentationText = new string(indentChar, indentationLevel * indentCharsPerLevel);

        return SyntaxFactory.Whitespace(indentationText);
    }

    public static int ComputeIndentationLevel(SyntaxNode? node, int currentLevel = 0)
    {
        if (node is null)
            return currentLevel;

        if (ParentNodeIncreasesIndentationLevel(node))
            currentLevel++;

        return ComputeIndentationLevel(node.Parent, currentLevel);
    }

    public static bool ParentNodeIncreasesIndentationLevel(SyntaxNode node) =>
        node switch
        {
            BlockSyntax => true,
            NamespaceDeclarationSyntax => true,
            ClassDeclarationSyntax => true,
            ArrowExpressionClauseSyntax => true,
            SwitchStatementSyntax => true,
            SwitchSectionSyntax => true,
            IfStatementSyntax => true,
            ElseClauseSyntax => true,
            WhileStatementSyntax => true,
            ForStatementSyntax => true,
            ForEachStatementSyntax => true,
            DoStatementSyntax => true,
            TryStatementSyntax => true,
            CatchClauseSyntax => true,
            FinallyClauseSyntax => true,
            UsingStatementSyntax => true,
            LockStatementSyntax => true,
            ArgumentListSyntax => true,

            _ => false
        };
}
