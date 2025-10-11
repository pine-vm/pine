using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.DotNet;

public class CompileKernelFunctionApplication
{
    public static IEnumerable<CompiledCSharpExpression> EnumerateMatchingSpecializedInterface(
        IReadOnlyList<PineKernelFunctions.KernelFunctionSpecializedInfo> specializedInterfaces,
        IReadOnlyList<StaticExpression<DeclQualifiedName>> argumentsList,
        bool isCommutative,
        Func<StaticExpression<DeclQualifiedName>, PineKernelFunctions.KernelFunctionParameterType, ExpressionSyntax?> tryRenderArgument,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        IEnumerable<IReadOnlyList<StaticExpression<DeclQualifiedName>>> EnumerateArgumentsPermutations()
        {
            yield return argumentsList;

            if (isCommutative && argumentsList.Count > 1)
            {
                foreach (var permutation in GetPermutations(argumentsList))
                {
                    // Skip the original order, we already tried it
                    if (permutation.SequenceEqual(argumentsList))
                        continue;

                    yield return permutation;
                }
            }
        }

        foreach (var specializedInterface in specializedInterfaces)
        {
            foreach (var argumentOrder in EnumerateArgumentsPermutations())
            {
                if (TryMatchSpecializedInterfaceWithArgumentsOrder(
                    specializedInterface,
                    argumentOrder,
                    tryRenderArgument,
                    declarationSyntaxContext) is { } matchedExpr)
                {
                    yield return matchedExpr;
                }
            }
        }
    }

    private static CompiledCSharpExpression? TryMatchSpecializedInterfaceWithArgumentsOrder(
        PineKernelFunctions.KernelFunctionSpecializedInfo specializedInterface,
        IReadOnlyList<StaticExpression<DeclQualifiedName>> arguments,
        Func<StaticExpression<DeclQualifiedName>, PineKernelFunctions.KernelFunctionParameterType, ExpressionSyntax?> tryRenderArgument,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (specializedInterface.ParameterTypes.Count != arguments.Count)
            return null;

        var argumentExprs =
            new List<ExpressionSyntax>(capacity: specializedInterface.ParameterTypes.Count);

        for (var paramIndex = 0; paramIndex < specializedInterface.ParameterTypes.Count; paramIndex++)
        {
            var paramType = specializedInterface.ParameterTypes[paramIndex];

            // Can we render the argument to the required parameter type?

            var argumentExpr = tryRenderArgument(arguments[paramIndex], paramType);

            if (argumentExpr is null)
            {
                return null;
            }

            argumentExprs.Add(argumentExpr);
        }

        var csharpExpr =
            specializedInterface.CompileInvocation(
                argumentExprs,
                declarationSyntaxContext);

        return
            specializedInterface.ReturnType switch
            {
                PineKernelFunctions.KernelFunctionSpecializedReturnType.Boolean =>
                CompiledCSharpExpression.Boolean(csharpExpr),

                PineKernelFunctions.KernelFunctionSpecializedReturnType.Generic =>
                CompiledCSharpExpression.Generic(csharpExpr),

                PineKernelFunctions.KernelFunctionSpecializedReturnType.Integer =>
                CompiledCSharpExpression.Integer(csharpExpr),

                _ =>
                throw new NotImplementedException(
                    "Unknown specialized return type " + specializedInterface.ReturnType)
            };
    }

    public static IEnumerable<CompiledCSharpExpression> TryCompileKernelFusion(
        StaticExpression<DeclQualifiedName>.KernelApplication kernelApp,
        ExpressionEmitEnv emitEnv)
    {
        TypeSyntax TypeSyntaxFromType(Type type)
        {
            return CompileTypeSyntax.TypeSyntaxFromType(type, emitEnv.FunctionEnv.DeclarationSyntaxContext);
        }

        {
            // Variant: Fuse take(skip(seq)) where take count is a compile-time integer
            if (kernelApp.Function is nameof(KernelFunction.take) &&
                kernelApp.Input is StaticExpression<DeclQualifiedName>.List takeArgsList &&
                takeArgsList.Items.Count is 2 &&
                takeArgsList.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication skipApp &&
                skipApp.Function is nameof(KernelFunction.skip) &&
                skipApp.Input is StaticExpression<DeclQualifiedName>.List skipArgsList &&
                skipArgsList.Items.Count is 2)
            {
                if (takeArgsList.Items[0] is StaticExpression<DeclQualifiedName>.Literal takeCountLiteral &&
                    KernelFunction.SignedIntegerFromValueRelaxed(takeCountLiteral.Value) is { } takeCountBI &&
                    takeCountBI >= int.MinValue && takeCountBI <= int.MaxValue)
                {
                    var argumentExpr =
                        StaticProgramCSharpClass.CompileToCSharpExpression(
                            skipArgsList.Items[1],
                            emitEnv);

                    var skipCountExpr =
                        StaticProgramCSharpClass.CompileToCSharpExpression(
                            skipArgsList.Items[0],
                            emitEnv);

                    // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.SkipAndTake(argument: ..., skipCountValue: ..., takeCount: ...)
                    var genericCSharpExpr =
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                TypeSyntaxFromType(typeof(KernelFunctionFused)),
                                SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.SkipAndTake))))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]
                                    {
                                    SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((int)takeCountBI))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCount"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(skipCountExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(argumentExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                                    })));

                    yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                }
            }
        }

        // Variant: Fuse skip(take(seq)) where both counts are generic PineValue
        /*
         * public static PineValue TakeAndSkip(
                PineValue skipCountValue,
                PineValue takeCountValue,
                PineValue argument)
         * */

        if (kernelApp.Function is nameof(KernelFunction.skip) &&
            kernelApp.Input is StaticExpression<DeclQualifiedName>.List skipArgsList2 &&
            skipArgsList2.Items.Count is 2 &&
            skipArgsList2.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication takeApp2 &&
            takeApp2.Function is nameof(KernelFunction.take) &&
            takeApp2.Input is StaticExpression<DeclQualifiedName>.List takeArgsList2 &&
            takeArgsList2.Items.Count is 2)
        {
            var argumentExpr =
                StaticProgramCSharpClass.CompileToCSharpExpression(
                    takeArgsList2.Items[1],
                    emitEnv);

            var takeCountExpr =
                StaticProgramCSharpClass.CompileToCSharpExpression(
                    takeArgsList2.Items[0],
                    emitEnv);

            var skipCountExpr =
                StaticProgramCSharpClass.CompileToCSharpExpression(
                    skipArgsList2.Items[0],
                    emitEnv);

            // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.TakeAndSkip(skipCountValue: ..., takeCountValue: ..., argument: ...)
            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        TypeSyntaxFromType(typeof(KernelFunctionFused)),
                        SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.TakeAndSkip))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]
                            {
                                SyntaxFactory.Argument(skipCountExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),

                                SyntaxFactory.Token(SyntaxKind.CommaToken),

                                SyntaxFactory.Argument(takeCountExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCountValue"))),

                                SyntaxFactory.Token(SyntaxKind.CommaToken),

                                SyntaxFactory.Argument(argumentExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                            })));

            yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        if (kernelApp.Function is nameof(KernelFunction.reverse))
        {
            var outerReverseInput = kernelApp.Input;

            {
                // Variant: Fuse reverse(take(n, reverse(seq))) => KernelFunctionFused.TakeLast(n, seq)
                if (outerReverseInput is StaticExpression<DeclQualifiedName>.KernelApplication takeApp &&
                    takeApp.Function is nameof(KernelFunction.take) &&
                    takeApp.Input is StaticExpression<DeclQualifiedName>.List takeArgs &&
                    takeArgs.Items.Count is 2 &&
                    takeArgs.Items[0] is StaticExpression<DeclQualifiedName>.Literal takeLastCountLiteral &&
                    KernelFunction.SignedIntegerFromValueRelaxed(takeLastCountLiteral.Value) is { } takeLastCountBI &&
                    takeLastCountBI >= int.MinValue && takeLastCountBI <= int.MaxValue &&
                    takeArgs.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication innerReverseApp &&
                    innerReverseApp.Function is nameof(KernelFunction.reverse))
                {
                    // seq expression is the input to the inner reverse
                    var seqExpr =
                        StaticProgramCSharpClass.CompileToCSharpExpression(
                            innerReverseApp.Input,
                            emitEnv);

                    // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.TakeLast(takeCount: n, value: seq)
                    var genericCSharpExpr =
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                TypeSyntaxFromType(typeof(KernelFunctionFused)),
                                SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.TakeLast))))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]
                                    {
                                    SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((int)takeLastCountBI))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCount"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(seqExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("value")))
                                    })));

                    yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                }
            }

            {
                // Variant: Fuse reverse(skip(n, reverse(seq))) => KernelFunctionFused.SkipLast(n, seq)
                /*
                    public static PineValue SkipLast(
                        int skipCount,
                        PineValue value)
                 * */

                if (outerReverseInput is StaticExpression<DeclQualifiedName>.KernelApplication skipApp &&
                    skipApp.Function is nameof(KernelFunction.skip) &&
                    skipApp.Input is StaticExpression<DeclQualifiedName>.List skipArgs &&
                    skipArgs.Items.Count is 2 &&
                    skipArgs.Items[0] is StaticExpression<DeclQualifiedName>.Literal skipLastCountLiteral &&
                    KernelFunction.SignedIntegerFromValueRelaxed(skipLastCountLiteral.Value) is { } skipLastCountBI &&
                    skipLastCountBI >= int.MinValue && skipLastCountBI <= int.MaxValue &&
                    skipArgs.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication innerReverseApp &&
                    innerReverseApp.Function is nameof(KernelFunction.reverse))
                {
                    // seq expression is the input to the inner reverse
                    var seqExpr =
                        StaticProgramCSharpClass.CompileToCSharpExpression(
                            innerReverseApp.Input,
                            emitEnv);

                    // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.SkipLast(skipCount: n, value: seq)
                    var genericCSharpExpr =
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                TypeSyntaxFromType(typeof(KernelFunctionFused)),
                                SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.SkipLast))))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]
                                    {
                                        SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((int)skipLastCountBI))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCount"))),

                                        SyntaxFactory.Token(SyntaxKind.CommaToken),

                                        SyntaxFactory.Argument(seqExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("value")))
                                    })));

                    yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                }
            }
        }

        {
            // Variant: concat, where the input is a list whose second item is a list containing only a single item.
            /*
            public static PineValue ListAppendItem(
                PineValue prefix,
                PineValue itemToAppend)
             * */

            if (kernelApp.Function is nameof(KernelFunction.concat) &&
                kernelApp.Input is StaticExpression<DeclQualifiedName>.List concatArgsList &&
                concatArgsList.Items.Count is 2 &&
                concatArgsList.Items[1] is StaticExpression<DeclQualifiedName>.List itemToAppendList &&
                itemToAppendList.Items.Count is 1)
            {
                var prefixExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        concatArgsList.Items[0],
                        emitEnv);

                var itemToAppendExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        itemToAppendList.Items[0],
                        emitEnv);

                // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.ListAppendItem(prefix: ..., itemToAppend: ...)
                var genericCSharpExpr =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            TypeSyntaxFromType(typeof(KernelFunctionFused)),
                            SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.ListAppendItem))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(prefixExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("prefix"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(itemToAppendExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("itemToAppend"))),
                                })));

                yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }

        {
            /*
            public static PineValue ListPrependItem(
                PineValue itemToPrepend,
                PineValue suffix)
             * */

            ExpressionSyntax ListPrependItemSyntax(
                StaticExpression<DeclQualifiedName> itemToPrepend,
                StaticExpression<DeclQualifiedName> suffix)
            {
                var itemToPrependExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        itemToPrepend,
                        emitEnv);

                var suffixExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        suffix,
                        emitEnv);

                // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.ListPrependItem(itemToPrepend: ..., suffix: ...)
                return
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            TypeSyntaxFromType(typeof(KernelFunctionFused)),
                            SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.ListPrependItem))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(itemToPrependExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                    .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("itemToPrepend"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(suffixExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                    .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("suffix"))),
                                })));
            }

            /*
            public static PineValue BlobPrependByte(
                byte byteToPrepend,
                PineValue suffix)
             * */
            ExpressionSyntax BlobPrependByteSyntax(
               byte byteToPrepend,
               StaticExpression<DeclQualifiedName> suffix)
            {
                var suffixExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        suffix,
                        emitEnv);

                // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.BlobPrependByte(byteToPrepend: ..., suffix: ...)
                return
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            TypeSyntaxFromType(typeof(KernelFunctionFused)),
                            SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.BlobPrependByte))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral(byteToPrepend))
                                    .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("byteToPrepend"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(suffixExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                                    .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("suffix"))),
                                })));
            }

            if (kernelApp.Function is nameof(KernelFunction.concat) &&
                kernelApp.Input is StaticExpression<DeclQualifiedName>.List concatArgsList &&
                concatArgsList.Items.Count is 2)
            {
                // Variant: concat, where the input is a list whose first item is a list containing only a single item.

                if (concatArgsList.Items[0] is StaticExpression<DeclQualifiedName>.List itemToPrependList &&
                    itemToPrependList.Items.Count is 1)
                {
                    var genericCSharpExpr =
                        ListPrependItemSyntax(itemToPrependList.Items[0], concatArgsList.Items[1]);

                    yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                }

                if (concatArgsList.Items[0] is StaticExpression<DeclQualifiedName>.Literal itemToPrependLiteral)
                {
                    if (itemToPrependLiteral.Value is PineValue.ListValue toPrependList &&
                        toPrependList.Items.Length is 1)
                    {
                        var genericCSharpExpr =
                            ListPrependItemSyntax(
                                StaticExpression<DeclQualifiedName>.LiteralInstance(toPrependList.Items.Span[0]),
                                concatArgsList.Items[1]);

                        yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                    }

                    if (itemToPrependLiteral.Value is PineValue.BlobValue toPrependBlob &&
                        toPrependBlob.Bytes.Length is 1)
                    {
                        var genericCSharpExpr =
                            BlobPrependByteSyntax(
                                toPrependBlob.Bytes.Span[0],
                                concatArgsList.Items[1]);

                        yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
                    }
                }
            }
        }

        CompiledCSharpExpression? AddingZero(StaticExpression<DeclQualifiedName> otherArgument)
        {
            if (otherArgument is StaticExpression<DeclQualifiedName>.KernelApplication innerKernelApp &&
                innerKernelApp.Function is nameof(KernelFunction.concat) &&
                innerKernelApp.Input is StaticExpression<DeclQualifiedName>.List concatList &&
                concatList.Items.Count is 2 &&
                concatList.Items[0] is StaticExpression<DeclQualifiedName>.Literal prefixLiteral &&
                prefixLiteral.Value is PineValue.BlobValue prefixBlob &&
                prefixBlob.Bytes.Length is 1)
            {
                var argumentExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        concatList.Items[1],
                        emitEnv);

                if (prefixBlob.Bytes.Span[0] is 2)
                {
                    return
                        CompiledCSharpExpression.Generic(
                            CanonicalIntegerFromUnsignedSyntax(
                                signIsPositive: false,
                                argumentExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext),
                                emitEnv.FunctionEnv.DeclarationSyntaxContext));
                }

                if (prefixBlob.Bytes.Span[0] is 4)
                {
                    return
                        CompiledCSharpExpression.Generic(
                            CanonicalIntegerFromUnsignedSyntax(
                                signIsPositive: true,
                                argumentExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext),
                                emitEnv.FunctionEnv.DeclarationSyntaxContext));
                }
            }

            return null;
        }

        {
            if (kernelApp.Function is nameof(KernelFunction.int_add) &&
                kernelApp.Input is StaticExpression<DeclQualifiedName>.List addArgsList &&
                addArgsList.Items.Count is 2)
            {
                if (addArgsList.Items[0] is StaticExpression<DeclQualifiedName>.Literal intLiteral &&
                    KernelFunction.SignedIntegerFromValueRelaxed(intLiteral.Value) is { } intBI &&
                    intBI == 0)
                {
                    if (AddingZero(addArgsList.Items[1]) is { } expr)
                    {
                        yield return expr;
                    }
                }

                if (addArgsList.Items[1] is StaticExpression<DeclQualifiedName>.Literal intLiteral2 &&
                    KernelFunction.SignedIntegerFromValueRelaxed(intLiteral2.Value) is { } intBI2 &&
                    intBI2 == 0)
                {
                    if (AddingZero(addArgsList.Items[0]) is { } expr)
                    {
                        yield return expr;
                    }
                }
            }
        }
    }

    public static ExpressionSyntax CanonicalIntegerFromUnsignedSyntax(
        bool signIsPositive,
        ExpressionSyntax unsignedValueSyntax,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        /*
        public static PineValue CanonicalIntegerFromUnsigned(
            bool signIsPositive,
            PineValue unsignedValue)
         * */

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(KernelFunctionFused), declarationSyntaxContext),
                    SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.CanonicalIntegerFromUnsigned))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(signIsPositive
                                ? SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression)
                                : SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression))
                                .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("signIsPositive"))),

                            SyntaxFactory.Token(SyntaxKind.CommaToken),

                            SyntaxFactory.Argument(unsignedValueSyntax)
                                .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("unsignedValue"))),
                        })));
    }

    private static IEnumerable<IReadOnlyList<T>> GetPermutations<T>(IReadOnlyList<T> items)
    {
        if (items.Count <= 1)
        {
            yield return items;
            yield break;
        }

        for (var i = 0; i < items.Count; i++)
        {
            var item = items[i];
            var remainingItems = items.Where((_, index) => index != i).ToList();

            foreach (var permutation in GetPermutations(remainingItems))
            {
                yield return new List<T> { item }.Concat(permutation).ToList();
            }
        }
    }
}
