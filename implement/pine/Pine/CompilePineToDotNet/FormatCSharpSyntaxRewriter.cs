using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Linq;
using System.Collections.Immutable;
using System.Collections.Generic;

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

    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax originalNode)
    {
        var node = (ArgumentListSyntax)base.VisitArgumentList(originalNode)!;

        var formatOnMultipleLines =
            node.Arguments.Any(arg => !IsSimpleExpression(arg.Expression));

        if (!formatOnMultipleLines)
        {
            var newArguments =
                SyntaxFactory.SeparatedList(
                    node.Arguments
                    .Select((argumentSyntax, index) =>
                    {
                        if (index is 0)
                            return argumentSyntax.WithLeadingTrivia().WithTrailingTrivia();
                        else
                            return argumentSyntax.WithLeadingTrivia(SyntaxFactory.Whitespace(" ")).WithTrailingTrivia();
                    }));

            return
                node
                .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
                .WithArguments(newArguments)
                .WithCloseParenToken(node.CloseParenToken.WithLeadingTrivia());
        }
        else
        {
            var argumentListIndentLevel = ComputeIndentationLevel(originalNode);

            var argumentIndentLevel = argumentListIndentLevel + 1;

            var argumentListIndentationTrivia =
                ComputeIndentationTriviaForLevel(argumentListIndentLevel);

            var argumentIndentationTrivia =
                ComputeIndentationTriviaForLevel(argumentIndentLevel);

            var newArguments =
                SyntaxFactory.SeparatedList(
                    node.Arguments
                    .Select(argumentSyntax =>
                    argumentSyntax.WithLeadingTrivia(SyntaxFactory.LineFeed, argumentIndentationTrivia)));

            return
                node
                .WithOpenParenToken(node.OpenParenToken.WithTrailingTrivia())
                .WithArguments(newArguments);
        }
    }

    public override SyntaxNode? VisitArrowExpressionClause(ArrowExpressionClauseSyntax originalNode)
    {
        var node = (ArrowExpressionClauseSyntax)base.VisitArrowExpressionClause(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Expression);

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

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax originalNode)
    {
        var node = (IfStatementSyntax)base.VisitIfStatement(originalNode)!;

        // Ensure proper spacing between 'if' keyword and the condition parentheses
        node = node.WithIfKeyword(node.IfKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));

        // Remove trailing trivia from condition and close paren to avoid extra spaces
        node = node
            .WithCondition(node.Condition.WithTrailingTrivia())
            .WithCloseParenToken(node.CloseParenToken.WithTrailingTrivia());

        // If the statement is a block, ensure the opening brace is on a new line
        if (node.Statement is BlockSyntax block)
        {
            var ifIndentationTrivia = ComputeIndentationTriviaForNode(originalNode);

            var formattedBlock = block
                .WithOpenBraceToken(
                    block.OpenBraceToken
                    .WithLeadingTrivia(SyntaxFactory.LineFeed, ifIndentationTrivia));

            node = node.WithStatement(formattedBlock);
        }

        return node;
    }

    public override SyntaxNode? VisitWhileStatement(WhileStatementSyntax originalNode)
    {
        var node = (WhileStatementSyntax)base.VisitWhileStatement(originalNode)!;

        // Ensure proper spacing between 'while' keyword and the condition parentheses
        return node
            .WithWhileKeyword(node.WhileKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    public override SyntaxNode? VisitForStatement(ForStatementSyntax originalNode)
    {
        var node = (ForStatementSyntax)base.VisitForStatement(originalNode)!;

        // Ensure proper spacing between 'for' keyword and the condition parentheses
        return node
            .WithForKeyword(node.ForKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax originalNode)
    {
        var node = (ForEachStatementSyntax)base.VisitForEachStatement(originalNode)!;

        // Ensure proper spacing between 'foreach' keyword and the condition parentheses
        return node
            .WithForEachKeyword(node.ForEachKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    public override SyntaxNode? VisitElseClause(ElseClauseSyntax originalNode)
    {
        var node = (ElseClauseSyntax)base.VisitElseClause(originalNode)!;

        var elseIndentationTrivia = ComputeIndentationTriviaForNode(originalNode);

        node = node
            .WithElseKeyword(
                node.ElseKeyword
                .WithLeadingTrivia(SyntaxFactory.LineFeed, elseIndentationTrivia)
                .WithTrailingTrivia());

        // If the statement is a block, ensure the opening brace is on a new line
        if (node.Statement is BlockSyntax block)
        {
            var formattedBlock = block
                .WithOpenBraceToken(
                    block.OpenBraceToken
                    .WithLeadingTrivia(SyntaxFactory.LineFeed, elseIndentationTrivia));

            node = node.WithStatement(formattedBlock);
        }

        return node;
    }

    public override SyntaxNode? VisitBlock(BlockSyntax originalNode)
    {
        var node = (BlockSyntax)base.VisitBlock(originalNode)!;

        var blockIndentationLevel = ComputeIndentationLevel(originalNode);

        var statementIndentationLevel = blockIndentationLevel + 1;

        var blockIndentationTrivia =
            ComputeIndentationTriviaForLevel(blockIndentationLevel);

        var statementIndentationTrivia =
            ComputeIndentationTriviaForLevel(statementIndentationLevel);

        var formattedNode = node
            .WithOpenBraceToken(
                node.OpenBraceToken
                .WithLeadingTrivia(blockIndentationTrivia)
                .WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithStatements(
                [.. node.Statements.Select((statement, statementIndex) =>
                    {
                        var trailingTrivia =
                        node.Statements.Count <= statementIndex + 1 ?
                        new SyntaxTriviaList(SyntaxFactory.LineFeed)
                        :
                        new SyntaxTriviaList(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed);

                        return
                        statement.WithLeadingTrivia(statementIndentationTrivia)
                        .WithTrailingTrivia(trailingTrivia);
                    })])
            .WithCloseBraceToken(
                node.CloseBraceToken
                .WithLeadingTrivia(blockIndentationTrivia));

        // If this block is the statement of an if statement followed by an else, remove trailing trivia from the close brace
        if (originalNode.Parent is IfStatementSyntax ifStmt && ifStmt.Else is not null)
        {
            formattedNode = formattedNode.WithCloseBraceToken(
                formattedNode.CloseBraceToken.WithTrailingTrivia());
        }

        return formattedNode;
    }

    public override SyntaxNode? VisitCompilationUnit(CompilationUnitSyntax originalNode)
    {
        var node = (CompilationUnitSyntax)base.VisitCompilationUnit(originalNode)!;

        if (node.Members.Count > 0)
        {
            var formattedMembers =
                node.Members
                .Select(member =>
                    member.WithTrailingTrivia(new SyntaxTriviaList(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed)))
                .ToList();

            return node.WithMembers([.. formattedMembers]);
        }

        return node;
    }

    public override SyntaxNode? VisitSwitchStatement(SwitchStatementSyntax originalNode)
    {
        var node = (SwitchStatementSyntax)base.VisitSwitchStatement(originalNode)!;

        // First ensure proper spacing between 'switch' keyword and expression
        node = node.WithSwitchKeyword(node.SwitchKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));

        var formattedSections = new List<SwitchSectionSyntax>();

        for (var i = 0; i < node.Sections.Count; i++)
        {
            var section = node.Sections[i];
            var isLastSection = i == node.Sections.Count - 1;

            // For standard C# formatting, case labels should be at the same level as the switch block contents
            var labelIndentationTrivia = ComputeIndentationTriviaForNode(section, addIndentLevels: 0);
            var statementIndentationTrivia = ComputeIndentationTriviaForNode(section, addIndentLevels: 1);

            // Add proper indentation to labels - same as switch block content level
            var formattedLabels = section.Labels.Select(label =>
                label.WithLeadingTrivia(labelIndentationTrivia)).ToList();

            // Add proper indentation to statements - one level deeper than labels
            var formattedStatements = section.Statements.Select(statement =>
                statement.WithLeadingTrivia(statementIndentationTrivia)
                         .WithTrailingTrivia(SyntaxFactory.LineFeed)).ToList();

            // Add empty line after section (except for last section)
            if (!isLastSection && formattedStatements.Count > 0)
            {
                var lastStatement = formattedStatements[formattedStatements.Count - 1];
                formattedStatements[formattedStatements.Count - 1] =
                    lastStatement.WithTrailingTrivia(SyntaxFactory.LineFeed, SyntaxFactory.LineFeed);
            }

            var formattedSection =
                section
                .WithLabels([.. formattedLabels])
                .WithStatements([.. formattedStatements]);

            formattedSections.Add(formattedSection);
        }

        return node.WithSections([.. formattedSections]);
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

    public override SyntaxNode? VisitCollectionExpression(CollectionExpressionSyntax originalNode)
    {
        var node = (CollectionExpressionSyntax)base.VisitCollectionExpression(originalNode)!;

        // Check if any element is complex (not just literals or simple identifiers)
        var hasComplexElements =
            node.Elements.Any(element =>
            element is ExpressionElementSyntax exprElement && !IsSimpleExpression(exprElement.Expression));

        if (!hasComplexElements)
        {
            // Simple collection - keep all elements on the same line with proper spacing
            var elementsWithSpacing =
                node.Elements.Select((element, index) =>
                {
                    if (index is 0)
                        return element.WithLeadingTrivia().WithTrailingTrivia();

                    return element.WithLeadingTrivia(SyntaxFactory.Whitespace(" ")).WithTrailingTrivia();
                });

            // For simple collections, preserve any existing leading trivia.
            return
                node
                .WithElements(SyntaxFactory.SeparatedList(elementsWithSpacing))
                .WithCloseBracketToken(node.CloseBracketToken.WithLeadingTrivia());
        }
        else
        {
            // Complex collection - each element and brackets on their own line
            var elementIndentationTrivia =
                ComputeIndentationTriviaForNode(originalNode, addIndentLevels: 1);

            var bracketIndentationTrivia =
                ComputeIndentationTriviaForNode(originalNode);

            var elementsOnSeparateLines =
                SyntaxFactory.SeparatedList(
                    node.Elements.Select(element =>
                    element.WithLeadingTrivia(SyntaxFactory.LineFeed, elementIndentationTrivia)
                    .WithTrailingTrivia()));

            return node
                .WithOpenBracketToken(
                    node.OpenBracketToken
                        .WithLeadingTrivia(SyntaxFactory.LineFeed, bracketIndentationTrivia)
                        .WithTrailingTrivia())
                .WithElements(elementsOnSeparateLines)
                .WithCloseBracketToken(
                    node.CloseBracketToken
                        .WithLeadingTrivia(SyntaxFactory.LineFeed, bracketIndentationTrivia)
                        .WithTrailingTrivia());
        }
    }

    public override SyntaxNode? VisitAssignmentExpression(AssignmentExpressionSyntax originalNode)
    {
        var node = (AssignmentExpressionSyntax)base.VisitAssignmentExpression(originalNode)!;

        // For indexer-style setters, we always put the RHS on a new line indented one level deeper than the assignment line.
        // For other assignments, preserve previous behavior (indent based on RHS complexity).
        var isIndexerSetter = originalNode.Left is ElementAccessExpressionSyntax;

        var rhsIndentationTrivia =
            isIndexerSetter
            ? ComputeIndentationTriviaForNode(originalNode, addIndentLevels: 1)
            : ComputeIndentationTriviaForNode(originalNode.Right);

        return
            node
            .WithLeft(node.Left.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")))
            .WithOperatorToken(node.OperatorToken.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithRight(node.Right.WithLeadingTrivia(rhsIndentationTrivia));
    }


    public override SyntaxNode? VisitEqualsValueClause(EqualsValueClauseSyntax originalNode)
    {
        var node = (EqualsValueClauseSyntax)base.VisitEqualsValueClause(originalNode)!;

        // Put expression on new line with proper indentation
        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Value);

        return
            node
            .WithEqualsToken(node.EqualsToken.WithTrailingTrivia(SyntaxFactory.LineFeed))
            .WithValue(node.Value.WithLeadingTrivia(indentationTrivia));
    }


    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax originalNode)
    {
        var node = (ReturnStatementSyntax)base.VisitReturnStatement(originalNode)!;

        if (node.Expression is not { } expression)
            return node;

        if (ExpressionCausesLineBreaks(expression))
        {
            // Put expression on new line with proper indentation
            var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Expression);

            return
                node
                .WithReturnKeyword(node.ReturnKeyword.WithTrailingTrivia())
                .WithExpression(expression.WithLeadingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
        }

        // Keep simple expressions on the same line
        return node;
    }

    /// <summary>
    /// Determines if an expression will cause line breaks when formatted according to our rules
    /// </summary>
    private static bool ExpressionCausesLineBreaks(ExpressionSyntax expression)
    {
        return expression switch
        {
            // Collection expressions cause line breaks if they contain complex elements
            CollectionExpressionSyntax collectionExpr =>
                collectionExpr.Elements.Any(element =>
                    element is ExpressionElementSyntax exprElement &&
                    !IsSimpleExpression(exprElement.Expression)),

            // Function calls cause line breaks if they have complex arguments or are not simple function calls
            // Use the same logic as the invocation expression formatter
            InvocationExpressionSyntax invocationExpr =>
                invocationExpr.Expression is not IdentifierNameSyntax ||
                invocationExpr.ArgumentList.Arguments.Any(arg => !IsSimpleExpression(arg.Expression)),

            // Any expression containing a sub-expression that causes line breaks
            _ => ContainsComplexSubExpressions(expression)
        };
    }

    /// <summary>
    /// Checks if an expression contains sub-expressions that would cause line breaks
    /// </summary>
    private static bool ContainsComplexSubExpressions(ExpressionSyntax expression)
    {
        foreach (var childNode in expression.DescendantNodes())
        {
            if (childNode is ExpressionSyntax childExpr && childExpr != expression)
            {
                if (ExpressionCausesLineBreaks(childExpr))
                    return true;
            }
        }
        return false;
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

    public override SyntaxNode? VisitParameterList(ParameterListSyntax originalNode)
    {
        var node = (ParameterListSyntax)base.VisitParameterList(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode, addIndentLevels: 1);

        var addLineBreaks = node.Parameters.Count > 1;

        var paramLeadingTrivia =
            addLineBreaks
            ?
            new SyntaxTriviaList(SyntaxFactory.LineFeed, indentationTrivia)
            :
            [];

        var parameters =
            node.Parameters
            .Select(parameter =>
                parameter
                .WithLeadingTrivia(paramLeadingTrivia)
                .WithTrailingTrivia())
            .ToImmutableArray();

        return
            node.WithParameters(SyntaxFactory.SeparatedList(parameters));
    }

    public SyntaxTrivia ComputeIndentationTriviaForNode(SyntaxNode? node, int addIndentLevels = 0) =>
        ComputeIndentationTriviaForNode(
            node,
            indentChar,
            indentCharsPerLevel,
            addIndentLevels: addIndentLevels);

    public static SyntaxTrivia ComputeIndentationTriviaForNode(
        SyntaxNode? node, char indentChar, int indentCharsPerLevel, int addIndentLevels)
    {
        var indentationLevel = ComputeIndentationLevel(node);

        return
            IndentationTriviaForLevel(
                indentationLevel + addIndentLevels,
                indentChar,
                indentCharsPerLevel);
    }

    public SyntaxTrivia ComputeIndentationTriviaForLevel(int level) =>
        IndentationTriviaForLevel(level, indentChar, indentCharsPerLevel);

    public static SyntaxTrivia IndentationTriviaForLevel(int level, char indentChar, int indentCharsPerLevel) =>
        SyntaxFactory.Whitespace(IndentationTextForLevel(level, indentChar, indentCharsPerLevel));

    public static string IndentationTextForLevel(int level, char indentChar, int indentCharsPerLevel) =>
        new(indentChar, level * indentCharsPerLevel);

    public static int ComputeIndentationLevel(SyntaxNode? node, int currentLevel = 0)
    {
        if (node is null)
            return currentLevel;

        if (node.Parent is not { } parent)
            return currentLevel;

        if (ParentNodeIncreasesIndentationLevel(parent))
            currentLevel++;

        return ComputeIndentationLevel(parent, currentLevel);
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
            IfStatementSyntax => false,
            ElseClauseSyntax => false,

            AssignmentExpressionSyntax assignmentExpression =>
            ExpressionCausesLineBreaks(assignmentExpression.Right),

            WhileStatementSyntax => false,
            ForStatementSyntax => false,
            ForEachStatementSyntax => false,
            DoStatementSyntax => false,
            TryStatementSyntax => false,
            CatchClauseSyntax => true,
            FinallyClauseSyntax => true,
            UsingStatementSyntax => false,
            LockStatementSyntax => false,
            ArgumentListSyntax => true,
            ReturnStatementSyntax => true,
            EqualsValueClauseSyntax => true,
            SwitchExpressionSyntax => true,
            CollectionExpressionSyntax collectionExpression =>
            ContainsComplexSubExpressions(collectionExpression),

            // Don't increase indentation for script context nodes
            CompilationUnitSyntax => false,
            GlobalStatementSyntax => false,

            _ => false
        };

    /// <summary>
    /// Determines if an expression is "simple" for the new formatting rules - 
    /// literals, identifiers, and simple member access (like Class.Property).
    /// </summary>
    private static bool IsSimpleExpression(ExpressionSyntax expression)
    {
        return expression switch
        {
            LiteralExpressionSyntax => true,

            IdentifierNameSyntax => true,

            MemberAccessExpressionSyntax memberAccess =>
            IsSimpleExpression(memberAccess.Expression),

            _ =>
            false
        };
    }
}
