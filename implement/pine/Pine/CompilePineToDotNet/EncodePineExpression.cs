using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.PineVM;
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
            Expression.LiteralExpression literal =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariant(
                    nameof(Expression.LiteralExpression),
                    CompileToCSharpLiteralExpression(literal.Value, overrideDefaultExpressionForValue).exprSyntax)),

            Expression.EnvironmentExpression =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariantWithoutArguments(
                    nameof(Expression.EnvironmentExpression))),

            Expression.ListExpression list =>
            list.List.Select(continueEncode)
            .ListCombine()
            .MapError(err => "Failed to encode list expression element: " + err)
            .Map(elementsSyntaxes =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.ListExpression),
                SyntaxFactory.CollectionExpression(
                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                        elementsSyntaxes
                        .Select(SyntaxFactory.ExpressionElement))))),

            Expression.ConditionalExpression conditionalExpression =>
                continueEncode(conditionalExpression.condition)
                    .MapError(err => "Failed to encode condition: " + err)
                    .AndThen(encodedCondition =>
                        continueEncode(conditionalExpression.falseBranch)
                            .MapError(err => "Failed to encode falseBranch: " + err)
                            .AndThen(encodedFalseBranch =>
                                continueEncode(conditionalExpression.trueBranch)
                                    .MapError(err => "Failed to encode trueBranch: " + err)
                                    .Map(encodedTrueBranch =>
                                        NewConstructorOfExpressionVariant(
                                            nameof(Expression.ConditionalExpression),

                                            SyntaxFactory.Argument(encodedCondition)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.ConditionalExpression.condition))),

                                            SyntaxFactory.Argument(encodedFalseBranch)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.ConditionalExpression.falseBranch))),

                                            SyntaxFactory.Argument(encodedTrueBranch)
                                            .WithNameColon(
                                                SyntaxFactory.NameColon(nameof(Expression.ConditionalExpression.trueBranch))))))),

            Expression.KernelApplicationExpression kernelApplicationExpr =>
            continueEncode(kernelApplicationExpr.argument)
            .MapError(err => "Failed to encode argument of kernel application: " + err)
            .Map(encodedArgument =>
            (ExpressionSyntax)SyntaxFactory.InvocationExpression(
                SyntaxFactory.QualifiedName(
                    PineCSharpSyntaxFactory.ExpressionEncodingClassQualifiedNameSyntax,
                    SyntaxFactory.IdentifierName(nameof(ExpressionEncoding.ParseKernelApplicationExpressionThrowOnUnknownName))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                    [
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(kernelApplicationExpr.functionName))),
                        SyntaxFactory.Argument(encodedArgument)
                    ])))),

            Expression.ParseAndEvalExpression decodeAndEvaluate =>
            continueEncode(decodeAndEvaluate.expression)
            .MapError(err => "Failed to encode expression of decode and evaluate: " + err)
            .AndThen(encodedExpression =>
            continueEncode(decodeAndEvaluate.environment)
            .MapError(err => "Failed to encode environment of decode and evaluate: " + err)
            .Map(encodedEnvironment =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.ParseAndEvalExpression),
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
