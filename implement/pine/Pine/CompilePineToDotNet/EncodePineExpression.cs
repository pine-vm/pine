using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using System;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public partial class CompileToCSharp
{

    public static Result<string, ExpressionSyntax> EncodePineExpressionAsCSharpExpression(
        Expression expression,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpressionForValue)
    {
        var continueEncode = new Func<Expression, Result<string, ExpressionSyntax>>(
            descendant => EncodePineExpressionAsCSharpExpression(descendant, overrideDefaultExpressionForValue));

        return expression switch
        {
            Expression.Literal literal =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariant(
                    nameof(Expression.Literal),
                    CompileToCSharpLiteralExpression(literal.Value, overrideDefaultExpressionForValue).exprSyntax)),

            Expression.Environment =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariantWithoutArguments(
                    nameof(Expression.Environment))),

            Expression.List list =>
            list.items.Select(continueEncode)
            .ListCombine()
            .MapError(err => "Failed to encode list expression element: " + err)
            .Map(elementsSyntaxes =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.List),
                SyntaxFactory.CollectionExpression(
                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                        elementsSyntaxes
                        .Select(SyntaxFactory.ExpressionElement))))),

            Expression.Conditional conditionalExpression =>
                continueEncode(conditionalExpression.Condition)
                    .MapError(err => "Failed to encode condition: " + err)
                    .AndThen(encodedCondition =>
                        continueEncode(conditionalExpression.FalseBranch)
                            .MapError(err => "Failed to encode falseBranch: " + err)
                            .AndThen(encodedFalseBranch =>
                                continueEncode(conditionalExpression.TrueBranch)
                                    .MapError(err => "Failed to encode trueBranch: " + err)
                                    .Map(encodedTrueBranch =>
                                        NewConstructorOfExpressionVariant(
                                            nameof(Expression.Conditional),

                                            SyntaxFactory.Argument(encodedCondition)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.Conditional.Condition))),

                                            SyntaxFactory.Argument(encodedFalseBranch)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.Conditional.FalseBranch))),

                                            SyntaxFactory.Argument(encodedTrueBranch)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.Conditional.TrueBranch))))))),

            Expression.KernelApplication kernelApplicationExpr =>
            continueEncode(kernelApplicationExpr.Input)
            .MapError(err => "Failed to encode input of kernel application: " + err)
            .Map(encodedInput =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.KernelApplication),

                SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                    SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(kernelApplicationExpr.Function)))
                .WithNameColon(
                    SyntaxFactory.NameColon(nameof(Expression.KernelApplication.Function))),

                SyntaxFactory.Argument(encodedInput)
                .WithNameColon(
                    SyntaxFactory.NameColon(nameof(Expression.KernelApplication.Input)))
            )),

            Expression.ParseAndEval decodeAndEvaluate =>
            continueEncode(decodeAndEvaluate.Encoded)
            .MapError(err => "Failed to encode expression of decode and evaluate: " + err)
            .AndThen(encodedExpression =>
            continueEncode(decodeAndEvaluate.Environment)
            .MapError(err => "Failed to encode environment of decode and evaluate: " + err)
            .Map(encodedEnvironment =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.ParseAndEval),
                encodedExpression,
                encodedEnvironment))),

            _ =>
            Result<string, ExpressionSyntax>.err("Expression type not implemented: " + expression.GetType().FullName)
        };
    }

    public static ExpressionSyntax NewConstructorOfExpressionVariantWithoutArguments(
        string expressionVariantTypeName) =>
        NewConstructorOfExpressionVariant(
            expressionVariantTypeName,
            arguments: []);

    public static ExpressionSyntax NewConstructorOfExpressionVariant(
        string expressionVariantTypeName,
        params ExpressionSyntax[] argumentsExpressions) =>
        NewConstructorOfExpressionVariant(
            expressionVariantTypeName,
            arguments: [.. argumentsExpressions.Select(SyntaxFactory.Argument)]);


    public static ExpressionSyntax NewConstructorOfExpressionVariant(
        string expressionVariantTypeName,
        params ArgumentSyntax[] arguments) =>
        SyntaxFactory.ObjectCreationExpression(
            SyntaxFactory.QualifiedName(
                SyntaxFactory.QualifiedName(
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.IdentifierName("Pine"),
                        SyntaxFactory.IdentifierName("PineVM")),
                    SyntaxFactory.IdentifierName("Expression")),
                SyntaxFactory.IdentifierName(expressionVariantTypeName)))
        .WithArgumentList(
            SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(arguments)));
}
