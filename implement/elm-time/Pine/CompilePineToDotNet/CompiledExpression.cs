using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public partial class CompileToCSharp
{
    public record CompiledExpression(
        ExpressionSyntax Syntax,

        /*
         * true if the type of the expression is Result<string, PineValue>
         * false if the type of the expression is PineValue
         * */
        bool IsTypeResult)
    {
        public static CompiledExpression WithTypePlainValue(ExpressionSyntax syntax) =>
            new(syntax, IsTypeResult: false);

        public static CompiledExpression WithTypeResult(ExpressionSyntax syntax) =>
            new(syntax, IsTypeResult: true);

        public CompiledExpression MapSyntax(Func<ExpressionSyntax, ExpressionSyntax> map) =>
            this
            with
            {
                Syntax = map(Syntax)
            };

        public CompiledExpression MapOrAndThen(Func<ExpressionSyntax, CompiledExpression> continueWithPlainValue)
        {
            if (!IsTypeResult)
                return continueWithPlainValue(Syntax);

            var syntaxName = GetNameForExpression(Syntax);

            var okIdentifier = SyntaxFactory.Identifier("ok_of_" + syntaxName);

            var combinedExpression = continueWithPlainValue(SyntaxFactory.IdentifierName(okIdentifier));

            var mapErrorExpression =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        Syntax,
                        SyntaxFactory.IdentifierName("MapError")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.SimpleLambdaExpression(
                                        SyntaxFactory.Parameter(
                                            SyntaxFactory.Identifier("err")))
                                    .WithExpressionBody(
                                        SyntaxFactory.BinaryExpression(
                                            SyntaxKind.AddExpression,
                                            SyntaxFactory.LiteralExpression(
                                                SyntaxKind.StringLiteralExpression,
                                                SyntaxFactory.Literal(
                                                    "Failed to evaluate expression " + syntaxName + ":")),
                                            SyntaxFactory.IdentifierName("err")))))));

            return
                WithTypeResult(
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            mapErrorExpression,
                            SyntaxFactory.IdentifierName(combinedExpression.IsTypeResult ? "AndThen" : "Map")))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.SimpleLambdaExpression(
                                            SyntaxFactory.Parameter(okIdentifier))
                                    .WithExpressionBody(combinedExpression.Syntax))))));
        }

        public CompiledExpression Map(Func<ExpressionSyntax, ExpressionSyntax> map)
        {
            return MapOrAndThen(inner => new CompiledExpression(map(inner), IsTypeResult: false));
        }

        public ExpressionSyntax AsCsWithTypeResult()
        {
            if (IsTypeResult)
                return Syntax;

            return WrapExpressionInPineValueResultOk(Syntax);
        }

        public static CompiledExpression ListMapOrAndThen(
            Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
            IReadOnlyList<CompiledExpression> compiledList)
        {
            static CompiledExpression recursive(
                Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
                ImmutableList<CompiledExpression> compiledList,
                ImmutableList<ExpressionSyntax> syntaxesCs)
            {
                if (compiledList.IsEmpty)
                    return combine(syntaxesCs);

                return
                    compiledList.First().MapOrAndThen(
                        itemCs => recursive(
                            combine,
                            compiledList.RemoveAt(0),
                            syntaxesCs.Add(itemCs)));
            }

            return
                recursive(
                    combine,
                    [.. compiledList],
                    []);
        }
    }
}