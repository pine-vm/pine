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
        ExpressionSyntax argumentExpression,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (s_kernelFunctionsInfo.Value.TryGetValue(kernelFunctionName, out var functionInfo))
            return functionInfo.CompileGenericInvocation(argumentExpression, declarationSyntaxContext);

        return null;
    }

    public record ParsedKernelApplicationArgumentExpression(
        IReadOnlyDictionary<KernelFunctionParameterType, CompiledExpression> ArgumentSyntaxFromParameterType,
        long? AsLiteralInt64);

    private record KernelFunctionInfo(
        Func<ExpressionSyntax, DeclarationSyntaxContext, InvocationExpressionSyntax> CompileGenericInvocation,
        IReadOnlyList<KernelFunctionSpecializedInfo> SpecializedImplementations,
        Func<Expression, ExpressionCompilationEnvironment, Result<string, CompiledExpression>?>? TryInline);

    public record KernelFunctionSpecializedInfo(
        IReadOnlyList<KernelFunctionParameterType> ParameterTypes,
        KernelFunctionSpecializedReturnType ReturnType,
        Func<IReadOnlyList<ExpressionSyntax>, DeclarationSyntaxContext, InvocationExpressionSyntax> CompileInvocation);

    public enum KernelFunctionSpecializedReturnType
    {
        Generic = 10,

        Boolean = 30,
    }

    public enum KernelFunctionParameterType
    {
        Generic = 10,
        Integer = 40,

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
            ArgumentListSyntax argumentListSyntax,
            DeclarationSyntaxContext declarationSyntaxContext)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                            CompileTypeSyntax.TypeSyntaxFromType(
                                selectedMethodInfo.DeclaringType!,
                                declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(selectedMethodInfo.Name)),
                    argumentListSyntax);
        }

        InvocationExpressionSyntax CompileGenericInvocation(
            MethodInfo genericMethodInfo,
            ExpressionSyntax argumentExpression,
            DeclarationSyntaxContext declarationSyntaxContext) =>
            CompileInvocationForArgumentList(
                genericMethodInfo,
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argumentExpression))),
                declarationSyntaxContext);

        KernelFunctionInfo? ReadKernelFunctionInfo(string functionName)
        {
            bool NameMatchesSpecializedVariant(MethodInfo mi) =>
                mi.Name.Equals(functionName, StringComparison.OrdinalIgnoreCase) ||
                /*
                 * Variants with specialized return types appear with expanded names, like 'int_is_sorted_asc_as_boolean'
                 * */
                mi.Name.StartsWith(functionName + "_", StringComparison.OrdinalIgnoreCase);

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
                if (!NameMatchesSpecializedVariant(methodInfo))
                {
                    continue;
                }

                if (methodInfo == genericVariant)
                {
                    continue;
                }

                if (TryParseKernelFunctionSpecializedReturnType(methodInfo.ReturnType) is not { } returnType)
                {
                    // We only consider functions with a known return type.
                    continue;
                }

                var parameterTypes =
                    methodInfo
                        .GetParameters().Select(pi => ParseKernelFunctionParameterType(pi.ParameterType))
                        .ToImmutableList();

                specializedImpls.Add(
                    new KernelFunctionSpecializedInfo(
                        ParameterTypes: parameterTypes,
                        ReturnType: returnType,
                        CompileInvocation:
                        (argumentsExpressions, declContext) =>
                            CompileInvocationForArgumentList(
                                methodInfo,
                                SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SeparatedList(
                                        argumentsExpressions.Select(SyntaxFactory.Argument))),
                                declContext)));

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
                    CompileGenericInvocation: (argExpr, declContext) => CompileGenericInvocation(genericVariant, argExpr, declContext),
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

    static KernelFunctionSpecializedReturnType? TryParseKernelFunctionSpecializedReturnType(
        Type type)
    {
        if (type == typeof(PineValue))
            return KernelFunctionSpecializedReturnType.Generic;

        if (type == typeof(bool))
            return KernelFunctionSpecializedReturnType.Boolean;

        return null;
    }
}
