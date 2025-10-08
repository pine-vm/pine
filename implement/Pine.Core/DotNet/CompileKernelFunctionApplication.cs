using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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
        Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {

        TypeSyntax TypeSyntaxFromType(Type type)
        {
            return CompileTypeSyntax.TypeSyntaxFromType(type, declarationSyntaxContext);
        }

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
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

                var skipCountExpr =
                    StaticProgramCSharpClass.CompileToCSharpExpression(
                        skipArgsList.Items[0],
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

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

                                    SyntaxFactory.Argument(skipCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(argumentExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                                })));

                yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
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
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var takeCountExpr =
                StaticProgramCSharpClass.CompileToCSharpExpression(
                    takeArgsList2.Items[0],
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var skipCountExpr =
                StaticProgramCSharpClass.CompileToCSharpExpression(
                    skipArgsList2.Items[0],
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

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
                                    SyntaxFactory.Argument(skipCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.Argument(takeCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCountValue"))),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.Argument(argumentExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                            })));

            yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        // Variant: Fuse reverse(take(n, reverse(seq))) => KernelFunctionFused.TakeLast(n, seq)
        if (kernelApp.Function is nameof(KernelFunction.reverse))
        {
            var outerReverseInput = kernelApp.Input;

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
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

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

                                    SyntaxFactory.Argument(seqExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("value")))
                                })));

                yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }
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
