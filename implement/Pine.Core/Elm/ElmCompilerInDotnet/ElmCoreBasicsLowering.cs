using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Rewrites type-specialized applications of functions from Elm's <c>Basics</c> module.
/// </summary>
public static class ElmCoreBasicsLowering
{
    /// <summary>
    /// Applies Elm core <c>Basics</c> lowering to canonicalized declarations.
    /// </summary>
    public static Result<string, ImmutableDictionary<DeclQualifiedName, Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, Declaration> declarations) =>
        BuiltinOperatorLowering.ApplyElmCoreBasics(declarations);

    internal static Expression? TryLowerApplication(
        Expression.Application application,
        IReadOnlyList<TypeInference.InferredType> argumentTypes,
        TypeInference.InferredType? expectedType)
    {
        if (application.Function is not Expression.Identifier
            {
                QualifiedName.Namespaces: ["Basics"]
            } functionIdentifier ||
            application.Arguments.Count != argumentTypes.Count ||
            !ProvesInt(argumentTypes, expectedType))
        {
            return null;
        }

        if (functionIdentifier.QualifiedName.DeclName is "min" &&
            application.Arguments is [var minLeft, var minRight])
        {
            return
                new Expression.IfBlock(
                    BuildIntIsSortedAsc(minLeft, minRight),
                    minLeft,
                    minRight);
        }

        if (functionIdentifier.QualifiedName.DeclName is "max" &&
            application.Arguments is [var maxLeft, var maxRight])
        {
            return
                new Expression.IfBlock(
                    BuildIntIsSortedAsc(maxLeft, maxRight),
                    maxRight,
                    maxLeft);
        }

        if (functionIdentifier.QualifiedName.DeclName is "negate" &&
            application.Arguments is [var negateArgument])
        {
            return BuildIntNegation(negateArgument);
        }

        if (functionIdentifier.QualifiedName.DeclName is "abs" &&
            application.Arguments is [var absArgument])
        {
            return
                new Expression.IfBlock(
                    BuildIntIsSortedAsc(
                        new Expression.IntegerLiteral(0, IntegerEncoding.EncodeSignedInteger(0)),
                        absArgument),
                    absArgument,
                    BuildIntNegation(absArgument));
        }

        if (functionIdentifier.QualifiedName.DeclName is "clamp" &&
            application.Arguments is [var low, var high, var number])
        {
            return
                new Expression.IfBlock(
                    BuildIntIsSortedAsc(low, number),
                    new Expression.IfBlock(
                        BuildIntIsSortedAsc(number, high),
                        number,
                        high),
                    low);
        }

        return null;
    }

    private static bool ProvesInt(
        IReadOnlyList<TypeInference.InferredType> argumentTypes,
        TypeInference.InferredType? expectedType)
    {
        if (expectedType is TypeInference.InferredType.IntType)
            return true;

        var containsInt = false;

        foreach (var argumentType in argumentTypes)
        {
            if (argumentType is TypeInference.InferredType.IntType)
            {
                containsInt = true;
                continue;
            }

            if (argumentType is not TypeInference.InferredType.NumberType)
                return false;
        }

        return containsInt;
    }

    private static Expression BuildIntIsSortedAsc(
        Expression left,
        Expression right) =>
        BuildBuiltinApplication(
            "int_is_sorted_asc",
            [
            left,
            right
            ]);

    private static Expression BuildIntNegation(Expression expression) =>
        BuildBuiltinApplication(
            "int_mul",
            [
            new Expression.IntegerLiteral(-1, IntegerEncoding.EncodeSignedInteger(-1)),
            expression
            ]);

    private static Expression BuildBuiltinApplication(
        string builtinName,
        IReadOnlyList<Expression> arguments) =>
        new Expression.Application(
            Expression.Identifier.Create(["Pine_builtin"], builtinName),
            [new Expression.ListExpr(arguments)]);
}
