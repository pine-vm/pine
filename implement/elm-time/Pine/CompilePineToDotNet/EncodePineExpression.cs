using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.PineVM;
using System;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public partial class CompileToCSharp
{

    private static Result<string, ExpressionSyntax> EncodePineExpressionAsCSharpExpression(
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
                    CompileToCSharpLiteralExpression(literal.Value, overrideDefaultExpressionForValue))),

            Expression.EnvironmentExpression =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariant(
                    nameof(Expression.EnvironmentExpression))),

            Expression.ListExpression list =>
            list.List.Select(continueEncode)
            .ListCombine()
            .MapError(err => "Failed to encode list expression element: " + err)
            .Map(elementsSyntaxes =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.ListExpression),
                SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName("ImmutableArray"),
                            SyntaxFactory.GenericName(
                                    SyntaxFactory.Identifier("Create"))
                                .WithTypeArgumentList(
                                    SyntaxFactory.TypeArgumentList(
                                        SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                            SyntaxFactory.QualifiedName(
                                                SyntaxFactory.QualifiedName(
                                                    SyntaxFactory.IdentifierName("Pine"),
                                                    SyntaxFactory.IdentifierName("PineVM")),
                                                SyntaxFactory.IdentifierName("Expression")))))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            elementsSyntaxes.Select(SyntaxFactory.Argument)))))),

            Expression.ConditionalExpression conditionalExpression =>
                continueEncode(conditionalExpression.condition)
                    .MapError(err => "Failed to encode condition: " + err)
                    .AndThen(encodedCondition =>
                        continueEncode(conditionalExpression.ifTrue)
                            .MapError(err => "Failed to encode branch if true: " + err)
                            .AndThen(encodedIfTrue =>
                                continueEncode(conditionalExpression.ifFalse)
                                    .MapError(err => "Failed to encode branch if false: " + err)
                                    .Map(encodedIfFalse =>
                                        NewConstructorOfExpressionVariant(
                                            nameof(Expression.ConditionalExpression),
                                            encodedCondition,
                                            encodedIfTrue,
                                            encodedIfFalse)))),

            Expression.KernelApplicationExpression kernelApplicationExpr =>
            continueEncode(kernelApplicationExpr.argument)
            .MapError(err => "Failed to encode argument of kernel application: " + err)
            .Map(encodedArgument =>
            (ExpressionSyntax)SyntaxFactory.InvocationExpression(
                SyntaxFactory.QualifiedName(
                    PineVmClassQualifiedNameSyntax,
                    SyntaxFactory.IdentifierName(nameof(PineVM.PineVM.DecodeKernelApplicationExpressionThrowOnUnknownName))))
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

            Expression.DecodeAndEvaluateExpression decodeAndEvaluate =>
            continueEncode(decodeAndEvaluate.expression)
            .MapError(err => "Failed to encode expression of decode and evaluate: " + err)
            .AndThen(encodedExpression =>
            continueEncode(decodeAndEvaluate.environment)
            .MapError(err => "Failed to encode environment of decode and evaluate: " + err)
            .Map(encodedEnvironment =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.DecodeAndEvaluateExpression),
                encodedExpression,
                encodedEnvironment))),

            _ =>
            Result<string, ExpressionSyntax>.err("Expression type not implemented: " + expression.GetType().FullName)
        };
    }

    private static ExpressionSyntax NewConstructorOfExpressionVariant(
        string expressionVariantTypeName,
        params ExpressionSyntax[] argumentsExpressions) => SyntaxFactory.ObjectCreationExpression(
                SyntaxFactory.QualifiedName(
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.QualifiedName(
                            SyntaxFactory.IdentifierName("Pine"),
                                SyntaxFactory.IdentifierName("PineVM")),
                        SyntaxFactory.IdentifierName("Expression")),
                    SyntaxFactory.IdentifierName(expressionVariantTypeName)))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(argumentsExpressions.Select(SyntaxFactory.Argument))));
}
