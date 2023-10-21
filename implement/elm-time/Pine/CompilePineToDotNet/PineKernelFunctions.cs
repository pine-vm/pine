using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Reflection;

namespace Pine.CompilePineToDotNet;

public partial class CompileToCSharp
{
    public record ParsedKernelApplicationArgumentExpression(
        IReadOnlyDictionary<KernelFunctionParameterType, CompiledExpression> ArgumentSyntaxFromParameterType,
        long? AsLiteralInt64);

    private record KernelFunctionInfo(
        Func<ExpressionSyntax, InvocationExpressionSyntax> CompileGenericInvocation,
        IReadOnlyList<KernelFunctionSpecializedInfo> SpecializedImplementations,
        Func<Expression, ExpressionCompilationEnvironment, Result<string, CompiledExpression>?>? TryInline);

    private record KernelFunctionSpecializedInfo(
        IReadOnlyList<KernelFunctionParameterType> ParameterTypes,
        KernelFunctionSpecializedInfoReturnType ReturnType,
        Func<IReadOnlyList<ExpressionSyntax>, InvocationExpressionSyntax> CompileInvocation);

    private record KernelFunctionSpecializedInfoReturnType(
        bool IsInstanceOfResult);

    public enum KernelFunctionParameterType
    {
        Generic = 1,
        Integer = 10,
    }

    public static Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>>? ParseKernelApplicationArgumentAsList(
        Expression kernelApplicationArgumentExpression,
        ExpressionCompilationEnvironment environment)
    {
        Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>> continueWithList(IEnumerable<Expression> list) =>
            list
            .Select(e => ParseKernelApplicationArgument(e, environment))
            .ListCombine();

        return
            kernelApplicationArgumentExpression switch
            {
                Expression.ListExpression listExpressionArgument =>
                    continueWithList(listExpressionArgument.List),

                Expression.LiteralExpression literalExpressionArgument =>
                    literalExpressionArgument.Value switch
                    {
                        PineValue.ListValue literalList =>
                            continueWithList(
                                literalList.Elements.Select(elementValue => new Expression.LiteralExpression(elementValue))),

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

        if (argumentExpression is Expression.LiteralExpression literal)
        {
            if (PineValueAsInteger.SignedIntegerFromValue(literal.Value) is Result<string, BigInteger>.Ok okInteger &&
                PineValueAsInteger.ValueFromSignedInteger(okInteger.Value) == literal.Value)
            {
                if (okInteger.Value >= long.MinValue && okInteger.Value <= long.MaxValue)
                    asLiteralInt64 = (long)okInteger.Value;

                dictionary[KernelFunctionParameterType.Integer] =
                    CompiledExpression.WithTypePlainValue(
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

    private static readonly Lazy<IReadOnlyDictionary<string, KernelFunctionInfo>> KernelFunctionsInfo =
        new(ReadKernelFunctionsInfoViaReflection);

    private static IReadOnlyDictionary<string, KernelFunctionInfo> ReadKernelFunctionsInfoViaReflection()
    {
        var kernelFunctionContainerType = typeof(KernelFunction);
        var methodsInfos = kernelFunctionContainerType.GetMethods(BindingFlags.Static | BindingFlags.Public);

        static KernelFunctionParameterType parseKernelFunctionParameterType(Type parameterType)
        {
            if (parameterType == typeof(BigInteger))
                return KernelFunctionParameterType.Integer;

            if (parameterType == typeof(PineValue))
                return KernelFunctionParameterType.Generic;

            throw new Exception("Unknown parameter type: " + parameterType.FullName);
        }

        static Result<string, KernelFunctionSpecializedInfoReturnType> parseKernelFunctionReturnType(Type returnType)
        {
            if (returnType == typeof(PineValue))
                return Result<string, KernelFunctionSpecializedInfoReturnType>.ok(
                    new KernelFunctionSpecializedInfoReturnType(IsInstanceOfResult: false));

            if (returnType == typeof(Result<string, PineValue>))
                return Result<string, KernelFunctionSpecializedInfoReturnType>.ok(
                    new KernelFunctionSpecializedInfoReturnType(IsInstanceOfResult: true));

            return Result<string, KernelFunctionSpecializedInfoReturnType>.err("Not a supported type");
        }

        KernelFunctionInfo ReadKernelFunctionInfo(MethodInfo genericMethodInfo)
        {
            InvocationExpressionSyntax compileInvocationForArgumentList(ArgumentListSyntax argumentListSyntax)
            {
                return
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.QualifiedName(SyntaxFactory.IdentifierName("Pine"),
                                    SyntaxFactory.IdentifierName("PineVM")),
                                SyntaxFactory.IdentifierName(kernelFunctionContainerType.Name)),
                            SyntaxFactory.IdentifierName(genericMethodInfo.Name)),
                        argumentListSyntax);
            }

            InvocationExpressionSyntax compileGenericInvocation(ExpressionSyntax argumentExpression) =>
                compileInvocationForArgumentList(SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argumentExpression))));

            var specializedImplementations =
                methodsInfos
                    .Where(candidateMethod =>
                        candidateMethod.Name == genericMethodInfo.Name &&
                        candidateMethod != genericMethodInfo &&
                        candidateMethod.DeclaringType == kernelFunctionContainerType)
                    .SelectWhere(methodInfo =>
                    parseKernelFunctionReturnType(methodInfo.ReturnType).ToMaybe().Map(returnType => (methodInfo, returnType)))
                    .Select(specializedMethodInfoAndReturnType =>
                    {
                        var parameterTypes =
                            specializedMethodInfoAndReturnType.methodInfo
                                .GetParameters().Select(pi => parseKernelFunctionParameterType(pi.ParameterType))
                                .ToImmutableList();

                        return
                            new KernelFunctionSpecializedInfo(
                                ParameterTypes: parameterTypes,
                                ReturnType: specializedMethodInfoAndReturnType.returnType,
                                CompileInvocation: argumentsExpressions =>
                                    compileInvocationForArgumentList(SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SeparatedList(
                                            argumentsExpressions.Select(SyntaxFactory.Argument)))));
                    })
                    .ToImmutableList();

            var tryInline =
                genericMethodInfo.Name switch
                {
                    nameof(KernelFunction.equal) =>
                    (Func<Expression, ExpressionCompilationEnvironment, Result<string, CompiledExpression>?>)
                    PineKernelFunctionsInline.TryInlineKernelFunction_Equal,

                    nameof(KernelFunction.length) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_Length,

                    nameof(KernelFunction.list_head) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_ListHead,

                    nameof(KernelFunction.skip) =>
                    PineKernelFunctionsInline.TryInlineKernelFunction_Skip,

                    _ =>
                    null
                };

            return
                new KernelFunctionInfo(
                    CompileGenericInvocation: compileGenericInvocation,
                    SpecializedImplementations: specializedImplementations,
                    TryInline: tryInline);
        }

        return
            methodsInfos
            .Where(methodInfo =>
            methodInfo.ReturnType == typeof(Result<string, PineValue>) &&
            methodInfo.GetParameters().Length == 1 && methodInfo.GetParameters()[0].ParameterType == typeof(PineValue))
            .ToImmutableDictionary(m => m.Name, ReadKernelFunctionInfo);
    }
}
