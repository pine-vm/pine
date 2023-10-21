using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public class FormatCSharpSyntaxRewriter(
    char indentChar,
    int indentCharsPerLevel)
    : CSharpSyntaxRewriter
{
    public FormatCSharpSyntaxRewriter()
        : this(
              indentChar: ' ',
              indentCharsPerLevel: 4)
    {
    }

    public static SyntaxTree FormatSyntaxTree(SyntaxTree syntaxTree)
    {
        var formattedSyntaxRoot = FormatSyntaxTree(syntaxTree.GetRoot());

        var formattedSyntaxTree = syntaxTree.WithRootAndOptions(formattedSyntaxRoot, syntaxTree.Options);

        return formattedSyntaxTree;
    }

    public static T FormatSyntaxTree<T>(T syntaxTree)
        where T : SyntaxNode =>
        (T)new FormatCSharpSyntaxRewriter().Visit(syntaxTree);

    public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax nodeBeforeRewriteInner)
    {
        var node = (InvocationExpressionSyntax)base.VisitInvocationExpression(nodeBeforeRewriteInner)!;

        if (node.ArgumentList.Arguments.Count < 1 && node == nodeBeforeRewriteInner)
            return node;

        var indentationTrivia = ComputeIndentationTriviaForNode(nodeBeforeRewriteInner.ArgumentList);

        var newArguments =
            SyntaxFactory.SeparatedList(
                node.ArgumentList.Arguments
                .Select(argumentSyntax =>
                argumentSyntax.WithLeadingTrivia(
                    new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)
                    .AddRange(argumentSyntax.GetLeadingTrivia()))));

        var newArgumentList =
            node.ArgumentList
            .WithArguments(newArguments);

        var newInvocationExpression = node.WithArgumentList(newArgumentList);

        return newInvocationExpression;
    }

    public override SyntaxNode? VisitObjectCreationExpression(ObjectCreationExpressionSyntax nodeBeforeRewriteInner)
    {
        var node = (ObjectCreationExpressionSyntax)base.VisitObjectCreationExpression(nodeBeforeRewriteInner)!;

        if (nodeBeforeRewriteInner.ArgumentList is not { } argumentListBefore || argumentListBefore.Arguments.Count < 1)
            return node;

        var indentationTrivia = ComputeIndentationTriviaForNode(argumentListBefore);

        var newArguments =
            SyntaxFactory.SeparatedList(
                argumentListBefore.Arguments
                .Select(argumentSyntax =>
                argumentSyntax.WithLeadingTrivia(
                    new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)
                    .AddRange(argumentSyntax.GetLeadingTrivia()))));

        var newArgumentList =
            argumentListBefore
            .WithArguments(newArguments);

        var newCreationExpression = node.WithArgumentList(newArgumentList);

        return newCreationExpression;
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
            .WithCondition(node.Condition.WithTrailingTrivia())
            .WithQuestionToken(
                node.QuestionToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia())
            .WithWhenTrue(
                node.WhenTrue.WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia())
            .WithColonToken(
                node.ColonToken
                .WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia)
                .WithTrailingTrivia())
            .WithWhenFalse(node.WhenFalse.WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
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

    public override SyntaxNode? VisitClassDeclaration(ClassDeclarationSyntax originalNode)
    {
        var node = (ClassDeclarationSyntax)base.VisitClassDeclaration(originalNode)!;

        var newMembers =
            SyntaxFactory.List(
                node.Members
                .Select((member, i) =>
                {
                    if (i < 1)
                        return member;

                    var indentationTrivia = ComputeIndentationTriviaForNode(member);

                    return
                    member.WithLeadingTrivia(
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia));
                }));

        return node.WithMembers(newMembers);
    }

    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax originalNode)
    {
        var node = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(originalNode)!;

        if (originalNode.Expression is not InvocationExpressionSyntax)
            return node;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Expression);

        return
            node
            .WithOperatorToken(node.OperatorToken.WithLeadingTrivia(
                new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)));
    }

    public override SyntaxNode? VisitEqualsValueClause(EqualsValueClauseSyntax originalNode)
    {
        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Value);

        var node = (EqualsValueClauseSyntax)base.VisitEqualsValueClause(originalNode)!;

        return
            node
            .WithValue(node.Value.WithLeadingTrivia(new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)))
            .WithEqualsToken(node.EqualsToken.WithTrailingTrivia());
    }

    public override SyntaxNode? VisitSwitchExpressionArm(SwitchExpressionArmSyntax originalNode)
    {
        var node = (SwitchExpressionArmSyntax)base.VisitSwitchExpressionArm(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        var isLastArm =
            node.Parent is SwitchExpressionSyntax switchExpression &&
            switchExpression.Arms.Last() == node;

        return
            node
            .WithPattern(
                node.Pattern.WithLeadingTrivia(indentationTrivia))
            .WithEqualsGreaterThanToken(
                node.EqualsGreaterThanToken
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithExpression(
                node.Expression
                .WithLeadingTrivia(new SyntaxTriviaList(indentationTrivia))
                .WithTrailingTrivia(isLastArm ? SyntaxTriviaList.Create(SyntaxFactory.LineFeed) : []));
    }

    public override SyntaxNode? VisitSwitchExpression(SwitchExpressionSyntax originalNode)
    {
        var node = (SwitchExpressionSyntax)base.VisitSwitchExpression(originalNode)!;

        var braceIndentTrivia = ComputeIndentationTriviaForNode(originalNode.Parent);
        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        var armsList =
            Enumerable
            .Range(0, node.Arms.Count - 1)
            .Aggregate(
                seed: node.Arms,
                func: (aggregate, separatorIndex) =>
                {
                    var separatorBefore = aggregate.GetSeparator(separatorIndex);

                    return
                    aggregate.ReplaceSeparator(
                    separatorBefore,
                    separatorBefore.WithTrailingTrivia(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed));
                });

        return
            node
            .WithGoverningExpression(
                node.GoverningExpression.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")))
            .WithSwitchKeyword(
                node.SwitchKeyword.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(braceIndentTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithArms(armsList)
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(braceIndentTrivia));
    }

    public SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode? node) =>
        ComputeIndentationTriviaForNode(node, indentChar, indentCharsPerLevel);

    public static SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode? node, char indentChar, int indentCharsPerLevel)
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
            ReturnStatementSyntax => true,
            EqualsValueClauseSyntax => true,
            SwitchExpressionSyntax => true,

            _ => false
        };
}
