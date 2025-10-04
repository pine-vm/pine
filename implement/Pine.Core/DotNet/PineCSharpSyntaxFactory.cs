using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Numerics;

namespace Pine.Core.DotNet;

public static class PineCSharpSyntaxFactory
{
    public static LiteralExpressionSyntax ExpressionSyntaxForBooleanLiteral(bool value) =>
        SyntaxFactory.LiteralExpression(
            value ? SyntaxKind.TrueLiteralExpression : SyntaxKind.FalseLiteralExpression);

    public static LiteralExpressionSyntax ExpressionSyntaxForIntegerLiteral(long integer) =>
        SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxTokenForIntegerLiteral(integer));

    public static SyntaxToken SyntaxTokenForIntegerLiteral(long integer)
    {
        var text = CommandLineInterface.FormatIntegerForDisplay(integer);

        if (integer >= int.MinValue && integer <= int.MaxValue)
        {
            // Produce an int-typed literal to enable implicit constant conversions (e.g., to byte and int).
            return SyntaxFactory.Literal(text, (int)integer);
        }

        // For larger values, make the literal explicitly long.
        return SyntaxFactory.Literal(text + "L", integer);
    }

    static readonly NumberFormatInfo s_integerLiteralNumberFormatInfo = new()
    {
        NumberGroupSeparator = "_",
        NumberGroupSizes = [3]
    };


    public static (ExpressionSyntax exprSyntax, ValueSyntaxKind syntaxKind) CompileToCSharpLiteralExpression(
        PineValue pineValue,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpression,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        ExpressionSyntax ContinueCompile(PineValue pineValue) =>
            overrideDefaultExpression(pineValue) is { } fromOverride ?
            fromOverride
            :
            CompileToCSharpLiteralExpression(
                pineValue,
                overrideDefaultExpression,
                declarationSyntaxContext).exprSyntax;

        if (pineValue == PineValue.EmptyList)
        {
            return
                (PineValueEmptyListSyntax(declarationSyntaxContext),
                new ValueSyntaxKind.Other());
        }

        static long? AttemptMapToSignedInteger(PineValue pineValue)
        {
            if (IntegerEncoding.ParseSignedIntegerStrict(pineValue) is Result<string, BigInteger>.Ok okInteger &&
                IntegerEncoding.EncodeSignedInteger(okInteger.Value) == pineValue &&
                okInteger.Value < long.MaxValue && long.MinValue < okInteger.Value)
            {
                return (long)okInteger.Value;
            }

            return null;
        }

        ExpressionSyntax ExpressionSyntaxForSignedInt(
            long asInt64)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(typeof(IntegerEncoding), declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(IntegerEncoding.EncodeSignedInteger))))
                .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(
                            ExpressionSyntaxForIntegerLiteral(asInt64)))));
        }

        if (AttemptMapToSignedInteger(pineValue) is { } asInt64)
        {
            return (ExpressionSyntaxForSignedInt(asInt64), new ValueSyntaxKind.AsSignedInteger(asInt64));
        }

        if (pineValue is PineValue.ListValue list)
        {
            var asIntegers =
                list.Items
                .ToArray()
                .Select(AttemptMapToSignedInteger)
                .WhereHasValue()
                .ToImmutableArray();

            if (asIntegers.Length == list.Items.Length)
            {
                return
                    (SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext),
                            SyntaxFactory.IdentifierName(nameof(PineValue.List))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.CollectionExpression(
                                        SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                            asIntegers
                                            .Select(item => SyntaxFactory.ExpressionElement(ExpressionSyntaxForSignedInt(item))))))))),
                                            new ValueSyntaxKind.AsListOfSignedIntegers(asIntegers));
            }
        }

        ExpressionSyntax DefaultRepresentationOfList(ReadOnlyMemory<PineValue> list)
        {
            var itemSyntaxes = new ExpressionSyntax[list.Length];

            for (var i = 0; i < list.Length; ++i)
            {
                itemSyntaxes[i] = ContinueCompile(list.Span[i]);
            }

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(PineValue.List))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.CollectionExpression(
                                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                        itemSyntaxes
                                        .Select(SyntaxFactory.ExpressionElement)))
                                ))));
        }

        ExpressionSyntax DefaultRepresentationOfBlob(ReadOnlyMemory<byte> blob)
        {
            var bytesSyntaxes = new ExpressionSyntax[blob.Length];

            for (var i = 0; i < blob.Length; ++i)
            {
                bytesSyntaxes[i] = ExpressionSyntaxForIntegerLiteral(blob.Span[i]);
            }

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(PineValue.Blob))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.CollectionExpression(
                                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                        bytesSyntaxes.Select(SyntaxFactory.ExpressionElement))))
                                )));
        }

        if (pineValue is PineValue.BlobValue blobValue)
        {
            if (StringEncoding.StringFromBlobValue(blobValue.Bytes).IsOkOrNull() is { } okString &&
                StringEncoding.ValueFromString(okString) == blobValue)
            {
                return
                    (SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            CompileTypeSyntax.TypeSyntaxFromType(typeof(StringEncoding), declarationSyntaxContext),
                            SyntaxFactory.IdentifierName(nameof(StringEncoding.ValueFromString))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.StringLiteralExpression,
                                        SyntaxFactory.Literal(okString)))))),
                                        new ValueSyntaxKind.AsString(okString));

            }

            return
                (DefaultRepresentationOfBlob(blobValue.Bytes),
                new ValueSyntaxKind.Other());
        }


        if (pineValue is PineValue.ListValue listValue)
        {
            return
                (DefaultRepresentationOfList(listValue.Items),
                new ValueSyntaxKind.Other());
        }

        throw new NotImplementedException(
            "Literal type not implemented: " + pineValue.GetType().FullName);
    }

    public static ExpressionSyntax PineValueFromBoolExpression(
        ExpressionSyntax expressionSyntax) =>
        PineValueFromBoolExpression(
            expressionSyntax,
            declarationSyntaxContext: DeclarationSyntaxContext.None);

    public static ExpressionSyntax PineValueFromBoolExpression(
        ExpressionSyntax expressionSyntax,
        DeclarationSyntaxContext declarationSyntaxContext) =>
        InvocationExpressionOnPineVMKernelFunctionClass(
            nameof(KernelFunction.ValueFromBool),
            declarationSyntaxContext)
        .WithArgumentList(
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(expressionSyntax))));

    public static InvocationExpressionSyntax InvocationExpressionOnPineVMKernelFunctionClass(
        string memberIdentifierName,
        DeclarationSyntaxContext declarationSyntaxContext) =>
        SyntaxFactory.InvocationExpression(
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                CompileTypeSyntax.TypeSyntaxFromType(typeof(KernelFunction), declarationSyntaxContext),
                SyntaxFactory.IdentifierName(memberIdentifierName)));

    public static ExpressionSyntax PineValueEmptyListSyntax(
        DeclarationSyntaxContext declarationSyntaxContext) =>
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext),
            SyntaxFactory.IdentifierName(nameof(PineValue.EmptyList)));

    public static StatementSyntax ConsoleWriteLineForLiteralString(
        string logEntry,
        DeclarationSyntaxContext declarationSyntaxContext) =>
        SyntaxFactory.ExpressionStatement(
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(Console), declarationSyntaxContext),
                    SyntaxFactory.IdentifierName(nameof(Console.WriteLine))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(logEntry)))))));

    public static ExpressionSyntax ThrowParseExpressionException(
        ExpressionSyntax messageExpr,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        return
            SyntaxFactory.ThrowExpression(
                SyntaxFactory.ObjectCreationExpression(
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(ParseExpressionException), declarationSyntaxContext))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(messageExpr)))));
    }

    public static ExpressionSyntax BuildCSharpExpressionToGetItemFromPathOrEmptyList(
        ExpressionSyntax compositionExpr,
        IReadOnlyList<int> path,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (path.Count is 0)
            return compositionExpr;

        var pathCollectionExpr =
            SyntaxFactory.CollectionExpression(
                SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                    path.Select(index => SyntaxFactory.ExpressionElement(ExpressionSyntaxForIntegerLiteral(index)))));

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    CompileTypeSyntax.TypeSyntaxFromType(
                        typeof(PineValueExtension),
                        declarationSyntaxContext),
                    SyntaxFactory.IdentifierName(nameof(PineValueExtension.ValueFromPathOrEmptyList))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(new SyntaxNodeOrToken[]
                    {
                        SyntaxFactory.Argument(compositionExpr),
                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                        SyntaxFactory.Argument(pathCollectionExpr)
                    })));
    }

    public static ExpressionSyntax GenericExpressionFromIntegerExpression(
        ExpressionSyntax intExpr,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        // Invoke IntegerEncoding.EncodeSignedInteger

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(IntegerEncoding), declarationSyntaxContext),
                    SyntaxFactory.IdentifierName(nameof(IntegerEncoding.EncodeSignedInteger))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(intExpr))));
    }
}
