using Microsoft.CodeAnalysis;
using Pine.Core;
using Pine.Core.DotNet;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.CompilePineToDotNet;

using KernelFunctionParameterType =
    PineKernelFunctions.KernelFunctionParameterType;

using CoreSyntaxFactory =
    Core.DotNet.PineCSharpSyntaxFactory;

public partial class CompileToCSharp
{
    public record ParsedKernelApplicationArgumentExpression(
        IReadOnlyDictionary<KernelFunctionParameterType, CompiledExpression> ArgumentSyntaxFromParameterType,
        long? AsLiteralInt64);

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
                        CoreSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)okInteger.Value));
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
}
