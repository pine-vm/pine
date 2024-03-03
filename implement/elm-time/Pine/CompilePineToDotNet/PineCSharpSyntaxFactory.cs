using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Globalization;
using System;
using System.Linq;
using System.Collections.Immutable;

namespace Pine.CompilePineToDotNet;

public static class PineCSharpSyntaxFactory
{
    public static LiteralExpressionSyntax ExpressionSyntaxForIntegerLiteral(long integer) =>
        SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxTokenForIntegerLiteral(integer));

    public static SyntaxToken SyntaxTokenForIntegerLiteral(long integer) =>
        SyntaxFactory.Literal(
            integer.ToString("N0", IntegerLiteralNumberFormatInfo),
            integer);

    static readonly NumberFormatInfo IntegerLiteralNumberFormatInfo = new()
    {
        NumberGroupSeparator = "_",
        NumberGroupSizes = [3]
    };

    public static StatementSyntax BranchForEnvId(
        PineVM.ExpressionUsageAnalysis exprUsage,
        FunctionCompilationEnvironment compilationEnv,
        StatementSyntax[]? prependStatments)
    {
        var envListItems =
            exprUsage.EnvId.ParsedEnvItems
            .Select(envListItem =>
            {
                var pathItems =
                envListItem.Key.Select(index =>
                SyntaxFactory.LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal(index)))
                .ToImmutableArray();

                var valueDeclName = CompileToCSharp.DeclarationNameForValue(envListItem.Value);

                return
                SyntaxFactory.TupleExpression(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]{
                            SyntaxFactory.Argument(
                                SyntaxFactory.CollectionExpression(
                                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                        pathItems
                                        .Select(li => (SyntaxNodeOrToken)SyntaxFactory.ExpressionElement(li))
                                        .Intersperse(SyntaxFactory.Token(SyntaxKind.CommaToken))))),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName(valueDeclName))}));
            })
            .ToImmutableArray();

        return
            SyntaxFactory.IfStatement(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.IdentifierName("ValueMatchesPathsPattern"))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.Argument(
                                    SyntaxFactory.CollectionExpression(
                                        SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                            envListItems
                                            .Select(li => (SyntaxNodeOrToken)SyntaxFactory.ExpressionElement(li))
                                            .Intersperse(SyntaxFactory.Token(SyntaxKind.CommaToken))))),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName(compilationEnv.ArgumentEnvironmentName))}))),
                SyntaxFactory.Block(
                    SyntaxFactory.SeparatedList(

                        [..prependStatments,
                        SyntaxFactory.ReturnStatement(
                            InvocationForCompiledExprFunction(
                                exprUsage: exprUsage,
                                compilationEnv:compilationEnv,
                                envArgSyntax: env => env))
                        ])));
    }

    public static ExpressionSyntax InvocationForCompiledExprFunction(
        PineVM.ExpressionUsageAnalysis exprUsage,
        FunctionCompilationEnvironment compilationEnv,
        Func<ExpressionSyntax, ExpressionSyntax> envArgSyntax)
    {
        var generalExprFuncName =
            CompileToCSharp.CompiledExpressionId(exprUsage.Expression)
            .Extract(err => throw new Exception(err));

        var specializedDeclName =
            CompileToCSharp.MemberNameForCompiledExpressionFunction(
                generalExprFuncName,
                exprUsage.EnvId);

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(specializedDeclName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]{
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName(compilationEnv.ArgumentEvalGenericName)),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(
                                envArgSyntax(SyntaxFactory.IdentifierName(compilationEnv.ArgumentEnvironmentName)))})));
    }

    public static readonly ExpressionSyntax PineValueEmptyListSyntax =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName(nameof(PineValue)),
            SyntaxFactory.IdentifierName(nameof(PineValue.EmptyList)));


    public static readonly MethodDeclarationSyntax ValueMatchesPathsPatternDeclaration =
        SyntaxFactory.MethodDeclaration(
            SyntaxFactory.PredefinedType(
                SyntaxFactory.Token(SyntaxKind.BoolKeyword)),
            SyntaxFactory.Identifier("ValueMatchesPathsPattern"))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        [
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword)]))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList<ParameterSyntax>(
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.Parameter(
                                    SyntaxFactory.Identifier("pathsValues"))
                                .WithType(
                                    SyntaxFactory.GenericName(
                                        SyntaxFactory.Identifier("IReadOnlyList"))
                                    .WithTypeArgumentList(
                                        SyntaxFactory.TypeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                SyntaxFactory.TupleType(
                                                    SyntaxFactory.SeparatedList<TupleElementSyntax>(
                                                        new SyntaxNodeOrToken[]{
                                                            SyntaxFactory.TupleElement(
                                                                SyntaxFactory.GenericName(
                                                                    SyntaxFactory.Identifier("IReadOnlyList"))
                                                                .WithTypeArgumentList(
                                                                    SyntaxFactory.TypeArgumentList(
                                                                        SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                                            SyntaxFactory.PredefinedType(
                                                                                SyntaxFactory.Token(SyntaxKind.IntKeyword))))))
                                                            .WithIdentifier(
                                                                SyntaxFactory.Identifier("path")),
                                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                            SyntaxFactory.TupleElement(
                                                                SyntaxFactory.IdentifierName("PineValue"))
                                                            .WithIdentifier(
                                                                SyntaxFactory.Identifier("itemValue"))})))))),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.Parameter(
                                    SyntaxFactory.Identifier("pineValue"))
                                .WithType(
                                    SyntaxFactory.IdentifierName("PineValue"))})))
                .WithExpressionBody(
                    SyntaxFactory.ArrowExpressionClause(
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName("pathsValues"),
                                SyntaxFactory.IdentifierName("All")))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.SimpleLambdaExpression(
                                            SyntaxFactory.Parameter(
                                                SyntaxFactory.Identifier("pv")))
                                        .WithExpressionBody(
                                            SyntaxFactory.BinaryExpression(
                                                SyntaxKind.EqualsExpression,
                                                SyntaxFactory.InvocationExpression(
                                                    SyntaxFactory.IdentifierName("ValueFromPathInValue"))
                                                .WithArgumentList(
                                                    SyntaxFactory.ArgumentList(
                                                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                                            new SyntaxNodeOrToken[]{
                                                                SyntaxFactory.Argument(
                                                                    SyntaxFactory.IdentifierName("pineValue")),
                                                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                SyntaxFactory.Argument(
                                                                    SyntaxFactory.MemberAccessExpression(
                                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                                        SyntaxFactory.IdentifierName("pv"),
                                                                        SyntaxFactory.IdentifierName("path")))}))),
                                                SyntaxFactory.MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    SyntaxFactory.IdentifierName("pv"),
                                                    SyntaxFactory.IdentifierName("itemValue"))))))))))
                .WithSemicolonToken(
                    SyntaxFactory.Token(SyntaxKind.SemicolonToken));


    public static readonly MethodDeclarationSyntax ValueFromPathInValueDeclaration =
        SyntaxFactory.MethodDeclaration(
            SyntaxFactory.NullableType(
                SyntaxFactory.IdentifierName("PineValue")),
            SyntaxFactory.Identifier("ValueFromPathInValue"))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        [
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword)]))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList<ParameterSyntax>(
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.Parameter(
                                    SyntaxFactory.Identifier("environment"))
                                .WithType(
                                    SyntaxFactory.IdentifierName("PineValue")),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.Parameter(
                                    SyntaxFactory.Identifier("path"))
                                .WithType(
                                    SyntaxFactory.GenericName(
                                        SyntaxFactory.Identifier("IReadOnlyList"))
                                    .WithTypeArgumentList(
                                        SyntaxFactory.TypeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                SyntaxFactory.PredefinedType(
                                                    SyntaxFactory.Token(SyntaxKind.IntKeyword))))))})))
                .WithBody(
                    SyntaxFactory.Block(
                        SyntaxFactory.IfStatement(
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.IdentifierName("path"),
                                    SyntaxFactory.IdentifierName("Count")),
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal(0))),
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.IdentifierName("environment"))),
                        SyntaxFactory.IfStatement(
                            SyntaxFactory.IsPatternExpression(
                                SyntaxFactory.IdentifierName("environment"),
                                SyntaxFactory.UnaryPattern(
                                    SyntaxFactory.DeclarationPattern(
                                        SyntaxFactory.QualifiedName(
                                            SyntaxFactory.IdentifierName("PineValue"),
                                            SyntaxFactory.IdentifierName("ListValue")),
                                        SyntaxFactory.SingleVariableDesignation(
                                            SyntaxFactory.Identifier("listValue"))))),
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NullLiteralExpression))),
                        SyntaxFactory.IfStatement(
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.LessThanExpression,
                                SyntaxFactory.ElementAccessExpression(
                                    SyntaxFactory.IdentifierName("path"))
                                .WithArgumentList(
                                    SyntaxFactory.BracketedArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.LiteralExpression(
                                                    SyntaxKind.NumericLiteralExpression,
                                                    SyntaxFactory.Literal(0)))))),
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal(0))),
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NullLiteralExpression))),
                        SyntaxFactory.IfStatement(
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.GreaterThanOrEqualExpression,
                                SyntaxFactory.ElementAccessExpression(
                                    SyntaxFactory.IdentifierName("path"))
                                .WithArgumentList(
                                    SyntaxFactory.BracketedArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.LiteralExpression(
                                                    SyntaxKind.NumericLiteralExpression,
                                                    SyntaxFactory.Literal(0)))))),
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        SyntaxFactory.IdentifierName("listValue"),
                                        SyntaxFactory.IdentifierName("Elements")),
                                    SyntaxFactory.IdentifierName("Count"))),
                            SyntaxFactory.ReturnStatement(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NullLiteralExpression))),
                        SyntaxFactory.ReturnStatement(
                            SyntaxFactory.InvocationExpression(
                                SyntaxFactory.IdentifierName("ValueFromPathInValue"))
                            .WithArgumentList(
                                SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                        new SyntaxNodeOrToken[]{
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.ElementAccessExpression(
                                                    SyntaxFactory.MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        SyntaxFactory.IdentifierName("listValue"),
                                                        SyntaxFactory.IdentifierName("Elements")))
                                                .WithArgumentList(
                                                    SyntaxFactory.BracketedArgumentList(
                                                        SyntaxFactory.SingletonSeparatedList(
                                                            SyntaxFactory.Argument(
                                                                SyntaxFactory.ElementAccessExpression(
                                                                    SyntaxFactory.IdentifierName("path"))
                                                                .WithArgumentList(
                                                                    SyntaxFactory.BracketedArgumentList(
                                                                        SyntaxFactory.SingletonSeparatedList(
                                                                            SyntaxFactory.Argument(
                                                                                SyntaxFactory.LiteralExpression(
                                                                                    SyntaxKind.NumericLiteralExpression,
                                                                                    SyntaxFactory.Literal(0))))))))))),
                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.CollectionExpression(
                                                    SyntaxFactory.SingletonSeparatedList<CollectionElementSyntax>(
                                                        SyntaxFactory.SpreadElement(
                                                            SyntaxFactory.InvocationExpression(
                                                                SyntaxFactory.MemberAccessExpression(
                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                    SyntaxFactory.IdentifierName("path"),
                                                                    SyntaxFactory.IdentifierName("Skip")))
                                                            .WithArgumentList(
                                                                SyntaxFactory.ArgumentList(
                                                                    SyntaxFactory.SingletonSeparatedList(
                                                                        SyntaxFactory.Argument(
                                                                            SyntaxFactory.LiteralExpression(
                                                                                SyntaxKind.NumericLiteralExpression,
                                                                                SyntaxFactory.Literal(1))))))))))}))))));

    public static StatementSyntax ConsoleWriteLineForLiteralString(string logEntry) =>
        SyntaxFactory.ExpressionStatement(
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName("Console"),
                    SyntaxFactory.IdentifierName("WriteLine")))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(logEntry)))))));
}
