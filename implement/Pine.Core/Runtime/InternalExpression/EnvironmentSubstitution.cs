using System;
using System.Collections.Generic;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>
/// Functionality to substitute <see cref="Expression.Environment"/> nodes in an expression tree with a specified replacement expression.
/// </summary>
internal static class EnvironmentSubstitution
{
    /// <summary>
    /// Substitutes all <see cref="Expression.Environment"/> nodes in an expression tree with the given replacement expression.
    /// </summary>
    public static Expression SubstituteEnvironmentNode(
        Expression expression,
        Expression environmentReplacement)
    {
        if (expression is Expression.Environment)
            return environmentReplacement;

        if (!expression.ReferencesEnvironment)
            return expression;

        if (environmentReplacement is Expression.Environment)
            return expression;

        return
            SubstituteEnvironmentNode(
                expression,
                environmentReplacement,
                cache: []);
    }

    private static Expression SubstituteEnvironmentNode(
        Expression expression,
        Expression environmentReplacement,
        Dictionary<Expression, Expression> cache)
    {
        if (!expression.ReferencesEnvironment)
            return expression;

        if (cache.TryGetValue(expression, out var cached))
            return cached;

        var substituted =
            SubstituteEnvironmentNodeWithoutCache(
                expression,
                environmentReplacement,
                cache);

        cache[expression] = substituted;

        return substituted;
    }

    private static Expression SubstituteEnvironmentNodeWithoutCache(
        Expression expression,
        Expression environmentReplacement,
        Dictionary<Expression, Expression> cache)
    {
        switch (expression)
        {
            case Expression.BuiltinBinary:
            case Expression.Length_Equal_Binary:
            case Expression.HeadSkip:
            case Expression.Switch:
                return
                    ExpressionFusion.Rebuild(
                        expression,
                        child => SubstituteEnvironmentNode(child, environmentReplacement, cache));

            case Expression.Environment:
                return environmentReplacement;

            case Expression.List list:
                {
                    Expression[]? substitutedItems = null;

                    for (var i = 0; i < list.Items.Length; ++i)
                    {
                        var item = list.Items[i];

                        var substitutedItem =
                            SubstituteEnvironmentNode(
                                item,
                                environmentReplacement,
                                cache);

                        if (substitutedItems is null)
                        {
                            if (substitutedItem == item)
                                continue;

                            substitutedItems = new Expression[list.Items.Length];

                            for (var copiedIndex = 0; copiedIndex < i; ++copiedIndex)
                                substitutedItems[copiedIndex] = list.Items[copiedIndex];
                        }

                        substitutedItems[i] = substitutedItem;
                    }

                    return
                        substitutedItems is null
                        ?
                        list
                        :
                        new Expression.List(
                            System.Runtime.InteropServices.ImmutableCollectionsMarshal.AsImmutableArray(
                                substitutedItems));
                }

            case Expression.Eval eval:
                {
                    var substitutedEncoded =
                        SubstituteEnvironmentNode(
                            eval.Encoded,
                            environmentReplacement,
                            cache);

                    var substitutedEnvironment =
                        SubstituteEnvironmentNode(
                            eval.Environment,
                            environmentReplacement,
                            cache);

                    if (substitutedEncoded == eval.Encoded &&
                        substitutedEnvironment == eval.Environment)
                    {
                        return eval;
                    }

                    return new Expression.Eval(substitutedEncoded, substitutedEnvironment);
                }

            case Expression.Builtin builtin:
                {
                    var substitutedInput =
                        SubstituteEnvironmentNode(
                            builtin.Input,
                            environmentReplacement,
                            cache);

                    if (substitutedInput == builtin.Input)
                        return builtin;

                    return Expression.BuiltinInst(builtin.Function, substitutedInput);
                }

            case Expression.Conditional conditional:
                {
                    var substitutedCondition =
                        SubstituteEnvironmentNode(
                            conditional.Condition,
                            environmentReplacement,
                            cache);

                    var substitutedFalseBranch =
                        SubstituteEnvironmentNode(
                            conditional.FalseBranch,
                            environmentReplacement,
                            cache);

                    var substitutedTrueBranch =
                        SubstituteEnvironmentNode(
                            conditional.TrueBranch,
                            environmentReplacement,
                            cache);

                    if (substitutedCondition == conditional.Condition &&
                        substitutedFalseBranch == conditional.FalseBranch &&
                        substitutedTrueBranch == conditional.TrueBranch)
                    {
                        return conditional;
                    }

                    return
                        Expression.ConditionalInst(
                            substitutedCondition,
                            substitutedFalseBranch,
                            substitutedTrueBranch);
                }

            case Expression.Label label:
                {
                    var substitutedTagged =
                        SubstituteEnvironmentNode(
                            label.Tagged,
                            environmentReplacement,
                            cache);

                    if (substitutedTagged == label.Tagged)
                        return label;

                    return new Expression.Label(label.Value, substitutedTagged);
                }

            case Expression.Literal:
                return expression;

            default:
                throw new NotImplementedException(
                    "Expression type not implemented: " + expression.GetType().FullName);
        }
    }
}
