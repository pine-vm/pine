using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Linq;
using System.Collections.Immutable;
using System.Collections.Generic;

namespace Pine.Core.DotNet;

/// <summary>
/// C# syntax rewriter making code more readable for inspection.
/// It enforces indentation, spacing, and line-breaking conventions across a wide
/// range of syntax constructs.
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitArgument(ArgumentSyntax originalNode)
    {
        var node = (ArgumentSyntax)base.VisitArgument(originalNode)!;

        // Check if this is a named argument with a NameColon
        if (node.NameColon is not null)
        {
            // Check if the expression causes line breaks
            if (!IsSimpleExpression(node.Expression))
            {
                // Add line break after the colon
                // The expression should be at the same indentation level as the argument itself
                var argumentIndentationTrivia = ComputeIndentationTriviaForNode(originalNode);

                return node
                    .WithNameColon(
                        node.NameColon.WithColonToken(
                            node.NameColon.ColonToken.WithTrailingTrivia()))
                    .WithExpression(
                        node.Expression.WithLeadingTrivia(
                            SyntaxFactory.LineFeed,
                            argumentIndentationTrivia));
            }
        }

        return node;
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitArrowExpressionClause(ArrowExpressionClauseSyntax originalNode)
    {
        var node = (ArrowExpressionClauseSyntax)base.VisitArrowExpressionClause(originalNode)!;

        var indentationTrivia = ComputeIndentationTriviaForNode(originalNode.Expression);

        return
            node
            .WithArrowToken(node.ArrowToken.WithTrailingTrivia(SyntaxFactory.LineFeed, indentationTrivia));
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitWhileStatement(WhileStatementSyntax originalNode)
    {
        var node = (WhileStatementSyntax)base.VisitWhileStatement(originalNode)!;

        // Ensure proper spacing between 'while' keyword and the condition parentheses
        return node
            .WithWhileKeyword(node.WhileKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForStatement(ForStatementSyntax originalNode)
    {
        var node = (ForStatementSyntax)base.VisitForStatement(originalNode)!;

        // Ensure proper spacing between 'for' keyword and the condition parentheses
        return node
            .WithForKeyword(node.ForKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    /// <inheritdoc/>
    public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax originalNode)
    {
        var node = (ForEachStatementSyntax)base.VisitForEachStatement(originalNode)!;

        // Ensure proper spacing between 'foreach' keyword and the condition parentheses
        return node
            .WithForEachKeyword(node.ForEachKeyword.WithTrailingTrivia(SyntaxFactory.Whitespace(" ")));
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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
                    })
                ])
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitCompilationUnit(CompilationUnitSyntax originalNode)
    {
        var node = (CompilationUnitSyntax)base.VisitCompilationUnit(originalNode)!;

        // Format using directives
        if (node.Usings.Count > 0)
        {
            // Separate static and non-static usings
            var staticUsings =
                node.Usings
                .Where(u => u.StaticKeyword.IsKind(SyntaxKind.StaticKeyword))
                .OrderBy(u => u.Name?.ToString() ?? "")
                .ToList();

            var nonStaticUsings =
                node.Usings
                .Where(u => !u.StaticKeyword.IsKind(SyntaxKind.StaticKeyword))
                .OrderBy(u => u.Name?.ToString() ?? "")
                .ToList();

            var formattedUsings = new List<UsingDirectiveSyntax>();

            // Add non-static usings first
            foreach (var usingDirective in nonStaticUsings)
            {
                formattedUsings.Add(usingDirective.WithTrailingTrivia(SyntaxFactory.LineFeed));
            }

            // Add an empty line before static usings if we have both types
            if (staticUsings.Count > 0 && nonStaticUsings.Count > 0)
            {
                // Add blank line before static usings
                foreach (var (usingDirective, index) in staticUsings.Select((u, i) => (u, i)))
                {
                    if (index is 0)
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
            else if (staticUsings.Count > 0)
            {
                // Only static usings
                foreach (var usingDirective in staticUsings)
                {
                    formattedUsings.Add(usingDirective.WithTrailingTrivia(SyntaxFactory.LineFeed));
                }
            }

            node = node.WithUsings([.. formattedUsings]);
        }

        // Format members (namespace declarations, class declarations, etc.)
        if (node.Members.Count > 0)
        {
            var formattedMembers =
                node.Members
                .Select((member, index) =>
                {
                    if (index is 0 && node.Usings.Count is 0)
                    {
                        return
                        member.
                        WithLeadingTrivia()
                        .WithTrailingTrivia(SyntaxFactory.LineFeed);
                    }

                    return
                    member.
                    WithLeadingTrivia(SyntaxFactory.LineFeed)
                    .WithTrailingTrivia(SyntaxFactory.LineFeed);
                })
                .ToList();

            return node.WithMembers([.. formattedMembers]);
        }

        return node;
    }

    /// <inheritdoc/>
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
                var lastStatement = formattedStatements[^1];
                formattedStatements[^1] =
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
    public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax originalNode)
    {
        var node = (FileScopedNamespaceDeclarationSyntax)base.VisitFileScopedNamespaceDeclaration(originalNode)!;

        // Ensure semicolon has only a linefeed as trailing trivia
        node = node.WithSemicolonToken(node.SemicolonToken.WithTrailingTrivia(SyntaxFactory.LineFeed));

        // Add two empty lines before the first member
        if (node.Members.Count > 0)
        {
            var firstMember = node.Members[0];
            var indentationTrivia = ComputeIndentationTriviaForNode(firstMember);

            var newMembers =
                SyntaxFactory.List(
                    node.Members
                    .Select((member, i) =>
                    {
                        if (i is 0)
                        {
                            return member.WithLeadingTrivia(
                                SyntaxFactory.LineFeed,
                                indentationTrivia);
                        }

                        return member;
                    }));

            return node.WithMembers(newMembers);
        }

        return node;
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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


    /// <inheritdoc/>
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


    /// <inheritdoc/>
    public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax originalNode)
    {
        var node = (ReturnStatementSyntax)base.VisitReturnStatement(originalNode)!;

        if (node.Expression is not { } expression)
            return node;

        if (ExpressionCausesLineBreak(expression))
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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


    /// <summary>
    /// Determines if an expression will cause line breaks when formatted according to our rules
    /// </summary>
    private static bool ExpressionCausesLineBreak(ExpressionSyntax expression)
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
                (!IsSimpleExpression(invocationExpr.Expression)) ||
                invocationExpr.ArgumentList.Arguments.Any(arg => !IsSimpleExpression(arg.Expression)),

            // Conditional expressions always cause line breaks
            ConditionalExpressionSyntax => true,

            // Any expression containing a sub-expression that causes line breaks
            _ => ContainsSubExpressionCausingLineBreak(expression)
        };
    }

    /// <summary>
    /// Checks if an expression contains sub-expressions that would cause line breaks
    /// </summary>
    private static bool ContainsSubExpressionCausingLineBreak(ExpressionSyntax expression)
    {
        foreach (var childNode in expression.DescendantNodes())
        {
            if (childNode is ExpressionSyntax childExpr && childExpr != expression)
            {
                if (ExpressionCausesLineBreak(childExpr))
                    return true;
            }
        }

        return false;
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
        SyntaxNode? node, char indentChar, int indentCharsPerLevel, int addIndentLevels)
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
            ArrowExpressionClauseSyntax => true,
            SwitchStatementSyntax => true,
            SwitchSectionSyntax => true,
            IfStatementSyntax => false,
            ElseClauseSyntax => false,

            AssignmentExpressionSyntax => true,

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
            ContainsSubExpressionCausingLineBreak(collectionExpression),

            // Don't increase indentation for script context nodes
            CompilationUnitSyntax => false,
            GlobalStatementSyntax => false,

            _ => false
        };

    /// <summary>
    /// Determines if an expression is "simple" for the formatting rules - 
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

            ParenthesizedExpressionSyntax parenthesizedExpr =>
            IsSimpleExpression(parenthesizedExpr.Expression),

            QualifiedNameSyntax qualifiedName =>
            IsSimpleExpression(qualifiedName.Left),

            _ =>
            false
        };
    }
}
