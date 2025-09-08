using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Core.CodeAnalysis;
using System.Globalization;
using System;
using System.Linq;
using System.Collections.Immutable;
using System.Collections.Generic;

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
        Expression expr,
        PineValueClass envConstraint,
        FunctionCompilationEnv compilationEnv,
        StatementSyntax[]? prependStatments)
    {
        var branchInvocation =
            CompileToCSharp.InvocationExpressionForCompiledExpressionFunction(
                currentEnv: new ExpressionCompilationEnvironment(
                    FunctionEnvironment: compilationEnv,
                    LetBindings: ImmutableDictionary<Expression, LetBinding>.Empty,
                    ParentEnvironment: null,
                    EnvConstraint: envConstraint),
                invokedExpr: expr,
                envConstraint: envConstraint,
                parseAndEvalEnvExpr: new Expression.Environment())
            .Extract(err => throw new Exception(err));

        var branchInvocationBlock =
            SyntaxFactory.Block(
                SyntaxFactory.SeparatedList(
                    [..prependStatments,
                    SyntaxFactory.ReturnStatement(branchInvocation.Syntax)
                    ]));

        if (envConstraint.ParsedItems.Count is 0)
        {
            return branchInvocationBlock;
        }

        var envListItemsConditionsExprs =
            envConstraint.ParsedItems
            .OrderBy(envListItem => envListItem.Key, IntPathComparer.Instance)
            .Select(envListItem =>
            {
                var valueDeclName = CompileToCSharp.DeclarationNameForValue(envListItem.Value);

                var currentEnvParam =
                compilationEnv.SelfInterface.GetParamForEnvItemPath(envListItem.Key) ?? throw new ArgumentNullException();

                return
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    BuildCSharpExpressionToGetItemFromPathOrNull(
                        SyntaxFactory.IdentifierName(currentEnvParam.paramName),
                        path: currentEnvParam.pathFromParam),
                    SyntaxFactory.IdentifierName(valueDeclName));
            })
            .ToImmutableArray();

        static ExpressionSyntax aggregateRecursive(
            ExpressionSyntax combined,
            IReadOnlyList<ExpressionSyntax> remaining) =>
            remaining switch
            {
                [var next, ..] =>
                aggregateRecursive(
                    SyntaxFactory.BinaryExpression(
                        SyntaxKind.LogicalAndExpression,
                        combined,
                        next),
                    [.. remaining.Skip(1)]),

                _ => combined
            };

        var aggregateConditionExpr =
            aggregateRecursive(
                envListItemsConditionsExprs[0],
                [.. envListItemsConditionsExprs.Skip(1)]);

        return
            SyntaxFactory.IfStatement(aggregateConditionExpr, branchInvocationBlock);
    }

    public static readonly ExpressionSyntax PineValueEmptyListSyntax =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName(nameof(PineValue)),
            SyntaxFactory.IdentifierName(nameof(PineValue.EmptyList)));

    public const string ValueFromPathInValueDeclarationName = "ValueFromPathInValue";

    public const string IsBlobDeclarationName = "IsBlob";

    public const string IsListDeclarationName = "IsList";

    public static readonly MethodDeclarationSyntax ValueFromPathInValueDeclaration =
        SyntaxFactory.MethodDeclaration(
            SyntaxFactory.NullableType(
                SyntaxFactory.IdentifierName("PineValue")),
            SyntaxFactory.Identifier(ValueFromPathInValueDeclarationName))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        [
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)
                        ]))
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
                                SyntaxFactory.Identifier("ReadOnlySpan"))
                                    .WithTypeArgumentList(
                                        SyntaxFactory.TypeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                SyntaxFactory.PredefinedType(
                                                    SyntaxFactory.Token(SyntaxKind.IntKeyword))))))})))
                .WithBody(
                    SyntaxFactory.Block(
                        SyntaxFactory.IfStatement(
                    SyntaxFactory.IsPatternExpression(
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.IdentifierName("path"),
                            SyntaxFactory.IdentifierName("Length")),
                        SyntaxFactory.ConstantPattern(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0)))),
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
                                SyntaxFactory.IdentifierName(ValueFromPathInValueDeclarationName))
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
                                        SyntaxFactory.ElementAccessExpression(
                                            SyntaxFactory.IdentifierName("path"))
                                                            .WithArgumentList(
                                            SyntaxFactory.BracketedArgumentList(
                                                                    SyntaxFactory.SingletonSeparatedList(
                                                                        SyntaxFactory.Argument(
                                                        SyntaxFactory.RangeExpression()
                                                        .WithLeftOperand(
                                                                            SyntaxFactory.LiteralExpression(
                                                                                SyntaxKind.NumericLiteralExpression,
                                                                SyntaxFactory.Literal(1))))))))}))))));

    public static readonly MethodDeclarationSyntax IsBlobDeclaration =
        SyntaxFactory.MethodDeclaration(
            SyntaxFactory.PredefinedType(
                SyntaxFactory.Token(SyntaxKind.BoolKeyword)),
            SyntaxFactory.Identifier(IsBlobDeclarationName))
        .WithModifiers(
            SyntaxFactory.TokenList(
                [
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)
                ]))
        .WithParameterList(
            SyntaxFactory.ParameterList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Parameter(
                        SyntaxFactory.Identifier("pineValue"))
                    .WithType(
                        SyntaxFactory.IdentifierName("PineValue")))))
        .WithExpressionBody(
            SyntaxFactory.ArrowExpressionClause(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.IsExpression,
                    SyntaxFactory.IdentifierName("pineValue"),
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.IdentifierName(nameof(PineValue)),
                        SyntaxFactory.IdentifierName(nameof(PineValue.BlobValue))))))
        .WithSemicolonToken(
            SyntaxFactory.Token(SyntaxKind.SemicolonToken));

    public static readonly MethodDeclarationSyntax IsListDeclaration =
        SyntaxFactory.MethodDeclaration(
            SyntaxFactory.PredefinedType(
                SyntaxFactory.Token(SyntaxKind.BoolKeyword)),
            SyntaxFactory.Identifier(IsListDeclarationName))
        .WithModifiers(
            SyntaxFactory.TokenList(
                [
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)
                ]))
        .WithParameterList(
            SyntaxFactory.ParameterList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Parameter(
                        SyntaxFactory.Identifier("pineValue"))
                    .WithType(
                        SyntaxFactory.IdentifierName("PineValue")))))
        .WithExpressionBody(
            SyntaxFactory.ArrowExpressionClause(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.IsExpression,
                    SyntaxFactory.IdentifierName("pineValue"),
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.IdentifierName(nameof(PineValue)),
                        SyntaxFactory.IdentifierName(nameof(PineValue.ListValue))))))
        .WithSemicolonToken(
            SyntaxFactory.Token(SyntaxKind.SemicolonToken));

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

    public static ExpressionSyntax GenericInvocationThrowingRuntimeExceptionOnError(
        FunctionCompilationEnv compilationEnv,
        ExpressionSyntax exprExpr,
        ExpressionSyntax environmentExpr)
    {
        var evalInvocationExpression =
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(compilationEnv.SelfInterface.ArgumentEvalGenericName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(exprExpr),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(environmentExpr)
                        })));

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    evalInvocationExpression,
                    SyntaxFactory.IdentifierName("Extract")))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.SimpleLambdaExpression(
                                            SyntaxFactory.Parameter(
                                                SyntaxFactory.Identifier("err")))
                                        .WithExpressionBody(
                                            ThrowGenericEvalRuntimeException(messageExpr: SyntaxFactory.IdentifierName("err"))
                                            )))));
    }

    public static ExpressionSyntax ThrowGenericEvalRuntimeException(
        ExpressionSyntax messageExpr)
    {
        return
            SyntaxFactory.ThrowExpression(
                SyntaxFactory.ObjectCreationExpression(
                    SyntaxFactory.IdentifierName(nameof(PineVM.GenericEvalException)))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(messageExpr)))));
    }

    public static ExpressionSyntax ThrowParseExpressionException(
        ExpressionSyntax messageExpr)
    {
        return
            SyntaxFactory.ThrowExpression(
                SyntaxFactory.ObjectCreationExpression(
                    SyntaxFactory.IdentifierName(nameof(PineVM.ParseExpressionException)))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(messageExpr)))));
    }

    public static Expression BuildPineExpressionToGetItemFromPath(
        Expression compositionExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count is 0)
        {
            return compositionExpr;
        }

        var currentOffset = path[0];

        var skippedExpr =
            currentOffset < 1
            ?
            compositionExpr
            :
            new Expression.KernelApplication(
                function: "skip",
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(currentOffset)),
                        compositionExpr
                    ]));

        var currentExpr =
            new Expression.KernelApplication(
                function: "head",
                input: skippedExpr);

        return
            BuildPineExpressionToGetItemFromPath(
                compositionExpr: currentExpr,
                path: [.. path.Skip(1)]);
    }

    public static ExpressionSyntax BuildCSharpExpressionToGetItemFromPathOrEmptyList(
        ExpressionSyntax compositionExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count is 0)
            return compositionExpr;

        return
            SyntaxFactory.ParenthesizedExpression(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.CoalesceExpression,
                    BuildCSharpExpressionToGetItemFromPathOrNull(compositionExpr, path),
                    PineValueEmptyListSyntax));
    }

    public static bool IsList(PineValue pineValue) =>
        pineValue is PineValue.ListValue;

    public static bool IsBlob(PineValue pineValue) =>
        pineValue is PineValue.BlobValue;

    public static ExpressionSyntax BuildCSharpExpressionToGetItemFromPathOrNull(
        ExpressionSyntax compositionExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count is 0)
        {
            return compositionExpr;
        }

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(ValueFromPathInValueDeclarationName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(compositionExpr),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(
                                SyntaxFactory.CollectionExpression(
                                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                        path
                                        .Select(pathItem =>
                                        (SyntaxNodeOrToken)SyntaxFactory.ExpressionElement(SyntaxFactory.LiteralExpression(
                                            SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(pathItem))))
                                        .Intersperse(SyntaxFactory.Token(SyntaxKind.CommaToken)))))})));
    }

    public static ExpressionSyntax BuildCSharpExpressionToCheckIsBlob(
        ExpressionSyntax compositionExpr)
    {
        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(IsBlobDeclarationName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(compositionExpr)
                        })));
    }

    public static ExpressionSyntax BuildCSharpExpressionToCheckIsList(
        ExpressionSyntax compositionExpr)
    {
        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(IsListDeclarationName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(compositionExpr)
                        })));
    }

    public static ExpressionSyntax PineValueFromBoolExpression(ExpressionSyntax expressionSyntax) =>
        InvocationExpressionOnPineVMKernelFunctionClass(nameof(KernelFunction.ValueFromBool))
        .WithArgumentList(
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(expressionSyntax))));

    public static ExpressionSyntax DescribeValueForErrorMessageExpression(
        ExpressionSyntax pineValueExprSyntax) =>
        InvocationExpressionOnPineVMClass(nameof(PineVM.PineVM.DescribeValueForErrorMessage))
        .WithArgumentList(
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(pineValueExprSyntax))));

    public static InvocationExpressionSyntax InvocationExpressionOnPineVMClass(
        string memberIdentifierName) =>
        SyntaxFactory.InvocationExpression(
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("Pine"),
                        SyntaxFactory.IdentifierName("PineVM")),
                    SyntaxFactory.IdentifierName("PineVM")),
                SyntaxFactory.IdentifierName(memberIdentifierName)));

    public static InvocationExpressionSyntax InvocationExpressionOnPineVMKernelFunctionClass(
        string memberIdentifierName) =>
        SyntaxFactory.InvocationExpression(
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("Pine"),
                        SyntaxFactory.IdentifierName("PineVM")),
                    SyntaxFactory.IdentifierName(nameof(KernelFunction))),
                SyntaxFactory.IdentifierName(memberIdentifierName)));

    public static QualifiedNameSyntax EvalExprDelegateTypeSyntax =>
        SyntaxFactory.QualifiedName(
            SyntaxFactory.QualifiedName(
                SyntaxFactory.IdentifierName("Pine"),
                SyntaxFactory.IdentifierName("PineVM")),
            SyntaxFactory.IdentifierName(nameof(EvalExprDelegate)));

    public static QualifiedNameSyntax ExpressionEncodingClassQualifiedNameSyntax =>
        SyntaxFactory.QualifiedName(
            SyntaxFactory.QualifiedName(
                SyntaxFactory.IdentifierName("Pine"),
                SyntaxFactory.IdentifierName("PineVM")),
            SyntaxFactory.IdentifierName(nameof(ExpressionEncoding)));
}
