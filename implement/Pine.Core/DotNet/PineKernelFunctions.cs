using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Reflection;

namespace Pine.Core.DotNet;

public class PineKernelFunctions
{
    public static IReadOnlyList<KernelFunctionSpecializedInfo>? SpecializedInterfacesFromKernelFunctionName(
        string kernelFunctionName)
    {
        if (KernelFunctionsInfo.Value.TryGetValue(kernelFunctionName, out var functionInfo))
            return functionInfo.SpecializedImplementations;

        return null;
    }

    public static ExpressionSyntax? CompileKernelFunctionGenericInvocation(
        string kernelFunctionName,
        ExpressionSyntax argumentExpression,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (KernelFunctionsInfo.Value.TryGetValue(kernelFunctionName, out var functionInfo))
            return functionInfo.CompileGenericInvocation(argumentExpression, declarationSyntaxContext);

        return null;
    }


    public record KernelFunctionInfo(
        Func<ExpressionSyntax, DeclarationSyntaxContext, InvocationExpressionSyntax> CompileGenericInvocation,
        IReadOnlyList<KernelFunctionSpecializedInfo> SpecializedImplementations);

    public record KernelFunctionSpecializedInfo(
        IReadOnlyList<KernelFunctionParameterType> ParameterTypes,
        KernelFunctionSpecializedReturnType ReturnType,
        Func<IReadOnlyList<ExpressionSyntax>, DeclarationSyntaxContext, InvocationExpressionSyntax> CompileInvocation);

    public enum KernelFunctionSpecializedReturnType
    {
        Generic = 10,

        Boolean = 30,

        Integer = 40,
    }

    public enum KernelFunctionParameterType
    {
        Generic = 10,

        Integer = 40,

        // ReadOnlySpan<PineValue>
        SpanGeneric = 100,
    }

    public static readonly Lazy<IReadOnlyDictionary<string, KernelFunctionInfo>> KernelFunctionsInfo =
        new(ReadKernelFunctionsInfoViaReflection);

    private static FrozenDictionary<string, KernelFunctionInfo> ReadKernelFunctionsInfoViaReflection()
    {
        var kernelFunctionContainerType =
            typeof(KernelFunction);

        var kernelFunctionSpecializedContainerType =
            typeof(Internal.KernelFunctionSpecialized);

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

            var specializedImplsRanked =
                specializedImpls
                .Order(KernelFunctionSpecializedSpecificityComparer.Instance)
                .ToImmutableList();

            return
                new KernelFunctionInfo(
                    CompileGenericInvocation: (argExpr, declContext) => CompileGenericInvocation(genericVariant, argExpr, declContext),
                    SpecializedImplementations: specializedImplsRanked);
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

        /*
         * For a consumer that expects an integer, compilation to C#/.NET will automatically
         * convert Int32 and Int64 to BigInteger. Since 'return type' only means a producing side,
         * we can treat both as 'Integer' here.
         * */

        if (type == typeof(BigInteger) || type == typeof(int) || type == typeof(long))
            return KernelFunctionSpecializedReturnType.Integer;

        return null;
    }
}
