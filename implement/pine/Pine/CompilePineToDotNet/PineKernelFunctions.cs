using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Reflection;

namespace Pine.CompilePineToDotNet;

public partial class CompileToCSharp
{
    public static IReadOnlyList<KernelFunctionSpecializedInfo>? SpecializedInterfacesFromKernelFunctionName(
        string kernelFunctionName)
    {
        if (s_kernelFunctionsInfo.Value.TryGetValue(kernelFunctionName, out var functionInfo))
            return functionInfo.SpecializedImplementations;

        return null;
    }

    public static ExpressionSyntax? CompileKernelFunctionGenericInvocation(
        string kernelFunctionName,
        ExpressionSyntax argumentExpression)
    {
        if (s_kernelFunctionsInfo.Value.TryGetValue(kernelFunctionName, out var functionInfo))
            return functionInfo.CompileGenericInvocation(argumentExpression);

        return null;
    }

    public record ParsedKernelApplicationArgumentExpression(
        IReadOnlyDictionary<KernelFunctionParameterType, CompiledExpression> ArgumentSyntaxFromParameterType,
        long? AsLiteralInt64);

    private record KernelFunctionInfo(
        Func<ExpressionSyntax, InvocationExpressionSyntax> CompileGenericInvocation,
        IReadOnlyList<KernelFunctionSpecializedInfo> SpecializedImplementations,
        Func<Expression, ExpressionCompilationEnvironment, Result<string, CompiledExpression>?>? TryInline);

    public record KernelFunctionSpecializedInfo(
        IReadOnlyList<KernelFunctionParameterType> ParameterTypes,
        KernelFunctionSpecializedInfoReturnType ReturnType,
        Func<IReadOnlyList<ExpressionSyntax>, InvocationExpressionSyntax> CompileInvocation);

    public record KernelFunctionSpecializedInfoReturnType(
        bool IsInstanceOfResult);

    public enum KernelFunctionParameterType
    {
        Generic = 1,
        Integer = 10,

        // ReadOnlySpan<PineValue>
        SpanGeneric = 100,
    }

    public static Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>>? ParseKernelApplicationInputAsList(
        Expression kernelAppInputExpr,
        ExpressionCompilationEnvironment environment)
    {
        Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>> ContinueWithList(IEnumerable<Expression> list) =>
            list
            .Select(e => ParseKernelApplicationArgument(e, environment))
            .ListCombine();

        return
            kernelAppInputExpr switch
            {
                Expression.List listExpressionArgument =>
                    ContinueWithList(listExpressionArgument.Items),

                Expression.Literal literalExpressionArgument =>
                    literalExpressionArgument.Value switch
                    {
                        PineValue.ListValue literalList =>
                            ContinueWithList(
                                literalList.Items.ToArray().Select(Expression.LiteralInstance)),

                        _ => null
                    },

                _ => null
            };
    }

    public static Result<string, ParsedKernelApplicationArgumentExpression> ParseKernelApplicationArgument(
        Expression argumentExpression,
        ExpressionCompilationEnvironment environment)
    {
        var dictionary = new Dictionary<KernelFunctionParameterType, CompiledExpression>();

        long? asLiteralInt64 = null;

        if (argumentExpression is Expression.Literal literal)
        {
            if (IntegerEncoding.ParseSignedIntegerStrict(literal.Value) is Result<string, BigInteger>.Ok okInteger &&
                IntegerEncoding.EncodeSignedInteger(okInteger.Value) == literal.Value)
            {
                if (okInteger.Value >= long.MinValue && okInteger.Value <= long.MaxValue)
                    asLiteralInt64 = (long)okInteger.Value;

                dictionary[KernelFunctionParameterType.Integer] =
                    CompiledExpression.WithTypeGenericValue(
                        PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)okInteger.Value));
            }
        }

        return
            CompileToCSharpExpression(
                argumentExpression,
                environment,
                createLetBindingsForCse: false)
                .Map(csharpExpression =>
                    new ParsedKernelApplicationArgumentExpression(
                        ArgumentSyntaxFromParameterType:
                        ImmutableDictionary<KernelFunctionParameterType, CompiledExpression>.Empty
                            .SetItem(KernelFunctionParameterType.Generic, csharpExpression)
                            .SetItems(dictionary),
                        AsLiteralInt64: asLiteralInt64));
    }

    private static readonly Lazy<IReadOnlyDictionary<string, KernelFunctionInfo>> s_kernelFunctionsInfo =
        new(ReadKernelFunctionsInfoViaReflection);

    private static FrozenDictionary<string, KernelFunctionInfo> ReadKernelFunctionsInfoViaReflection()
    {
        var kernelFunctionContainerType =
            typeof(KernelFunction);

        var kernelFunctionSpecializedContainerType =
            typeof(Core.Internal.KernelFunctionSpecialized);

        var genericMethodsInfos =
            kernelFunctionContainerType.GetMethods(BindingFlags.Static | BindingFlags.Public);


        var usingDirectivesTypes = new[]
        {
            typeof(KernelFunction),
            typeof(Core.Internal.KernelFunctionSpecialized),
        };

        // TODO: Let consumer configure set of usings for emitting C# code.

        var usingDirectives =
            usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
            .ToImmutableList();

        var kernelFunctionsNames =
            genericMethodsInfos
            .Where(methodInfo => methodInfo.ReturnType == typeof(PineValue))
            .Select(m => m.Name.ToLowerInvariant())
            .ToFrozenSet();

        var specializedMethodsInfos =
            kernelFunctionSpecializedContainerType.GetMethods(BindingFlags.Static | BindingFlags.Public);

        IReadOnlyList<MethodInfo> allCandidatesPool =
            [
            .. genericMethodsInfos,
            .. specializedMethodsInfos
            ];

        static KernelFunctionParameterType ParseKernelFunctionParameterType(Type parameterType)
        {
            if (parameterType == typeof(BigInteger))
                return KernelFunctionParameterType.Integer;

            if (parameterType == typeof(PineValue))
                return KernelFunctionParameterType.Generic;

            if (parameterType == typeof(ReadOnlySpan<PineValue>))
                return KernelFunctionParameterType.SpanGeneric;

            throw new NotImplementedException(
                "Unknown parameter type: " + parameterType.FullName);
        }

        InvocationExpressionSyntax CompileInvocationForArgumentList(
            MethodInfo selectedMethodInfo,
            ArgumentListSyntax argumentListSyntax)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                            CompileTypeSyntax.TypeSyntaxFromType(
                                selectedMethodInfo.DeclaringType!,
                                usings: usingDirectives),
                        SyntaxFactory.IdentifierName(selectedMethodInfo.Name)),
                    argumentListSyntax);
        }

        InvocationExpressionSyntax CompileGenericInvocation(
            MethodInfo genericMethodInfo,
            ExpressionSyntax argumentExpression) =>
            CompileInvocationForArgumentList(
                genericMethodInfo,
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argumentExpression))));

        KernelFunctionInfo? ReadKernelFunctionInfo(string functionName)
        {
            var genericVariant =
                genericMethodsInfos
                .FirstOrDefault(mi =>
                    mi.Name.Equals(functionName, StringComparison.OrdinalIgnoreCase) &&
                    mi.ReturnType == typeof(PineValue) &&
                    mi.GetParameters().Length is 1 &&
                    mi.GetParameters()[0].ParameterType == typeof(PineValue));

            if (genericVariant is null)
            {
                // We only consider functions that have a generic variant.
                return null;
            }

            var specializedImpls = new List<KernelFunctionSpecializedInfo>();

            foreach (var methodInfo in allCandidatesPool)
            {
                if (methodInfo.ReturnType != typeof(PineValue))
                {
                    continue;
                }

                if (!methodInfo.Name.Equals(functionName, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                if (methodInfo == genericVariant)
                {
                    continue;
                }

                var parameterTypes =
                    methodInfo
                        .GetParameters().Select(pi => ParseKernelFunctionParameterType(pi.ParameterType))
                        .ToImmutableList();

                specializedImpls.Add(
                    new KernelFunctionSpecializedInfo(
                        ParameterTypes: parameterTypes,
                        ReturnType: new KernelFunctionSpecializedInfoReturnType(IsInstanceOfResult: false),
                        CompileInvocation: argumentsExpressions =>
                            CompileInvocationForArgumentList(
                                methodInfo,
                                SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SeparatedList(
                                        argumentsExpressions.Select(SyntaxFactory.Argument))))));

            }

            var tryInline =
                functionName switch
                {
                    nameof(KernelFunction.equal) =>
                    (Func<Expression, ExpressionCompilationEnvironment, Result<string, CompiledExpression>?>)
                    PineKernelFunctionsInline.TryInlineKernelFunction_Equal,

                    /*
                     * 2024-05-29:
                     * Optimize for readability in intermediate C# code: Keep reference to 'length' as a function call.
                     * 
                    nameof(KernelFunction.length) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_Length,
                    */

                    /*
                     * 2023-12-01:
                     * Disabling the inlining for 'head' to reduce the size of the emitted C# code.
                     * 
                    nameof(KernelFunction.head) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_ListHead,
                    */

                    /*
                     * 2023-12-01:
                     * Disabling the inlining for 'skip' to reduce the size of the emitted C# code.
                     * 
                    nameof(KernelFunction.skip) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_Skip,
                    */

                    _ =>
                    null
                };

            var specializedImplsRanked =
                specializedImpls
                .Order(KernelFunctionSpecializedSpecificityComparer.Instance)
                .ToImmutableList();

            return
                new KernelFunctionInfo(
                    CompileGenericInvocation: argExpr => CompileGenericInvocation(genericVariant, argExpr),
                    SpecializedImplementations: specializedImplsRanked,
                    TryInline: tryInline);
        }

        var mutatedDict = new Dictionary<string, KernelFunctionInfo>();

        foreach (var name in kernelFunctionsNames)
        {
            if (ReadKernelFunctionInfo(name) is KernelFunctionInfo info)
            {
                mutatedDict[name] = info;
            }
        }

        return
            mutatedDict
            .ToFrozenDictionary();
    }
}
