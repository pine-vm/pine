using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm pattern matching to Pine expressions.
/// </summary>
public class PatternCompiler
{
    /// <summary>
    /// Represents a condition tree for pattern matching.
    /// </summary>
    public abstract record PatternCondition
    {
        /// <summary>Always matches - no condition needed.</summary>
        public sealed record Always : PatternCondition;

        /// <summary>Equality check: scrutinee equals a specific value.</summary>
        public sealed record ValueEquals(Expression Expected) : PatternCondition;

        /// <summary>Length check: scrutinee has exactly the specified length.</summary>
        public sealed record LengthEquals(int ExpectedLength) : PatternCondition;

        /// <summary>Non-empty check: scrutinee is not empty (length > 0).</summary>
        public sealed record NotEmpty : PatternCondition;

        /// <summary>Conjunction of multiple conditions - all must match.</summary>
        public sealed record Conjunction(IReadOnlyList<PatternCondition> Conditions) : PatternCondition;

        /// <summary>Condition on a sub-item accessed by an accessor function.</summary>
        public sealed record ItemCondition(
            Func<Expression, Expression> Accessor,
            PatternCondition SubCondition) : PatternCondition;
    }

    /// <summary>
    /// Result of analyzing a pattern: bindings and condition.
    /// </summary>
    public record PatternAnalysis(
        ImmutableDictionary<string, Expression> Bindings,
        PatternCondition Condition)
    {
        /// <summary>Empty analysis with no bindings and always-matching condition.</summary>
        public static PatternAnalysis Empty { get; } =
            new([], new PatternCondition.Always());

        /// <summary>Creates an analysis with just a condition and no bindings.</summary>
        public static PatternAnalysis WithCondition(PatternCondition condition) =>
            new([], condition);

        /// <summary>Creates an analysis with a single binding and always-matching condition.</summary>
        public static PatternAnalysis WithBinding(string name, Expression value) =>
            new(ImmutableDictionary<string, Expression>.Empty.Add(name, value), new PatternCondition.Always());
    }

    /// <summary>
    /// Compiles a case expression to a Pine expression.
    /// </summary>
    public static Result<CompilationError, Expression> CompileCaseExpression(
        SyntaxTypes.CaseBlock caseBlock,
        ExpressionCompilationContext context)
    {
        var scrutineeResult = ExpressionCompiler.Compile(caseBlock.Expression.Value, context);
        if (scrutineeResult.IsErrOrNull() is { } scrutineeErr)
        {
            return scrutineeErr;
        }

        var scrutinee = scrutineeResult.IsOkOrNull()!;
        Expression? result = null;

        // Infer the type of the scrutinee for pattern type extraction
        var scrutineeType = TypeInference.InferExpressionType(
            caseBlock.Expression.Value,
            context.ParameterNames,
            context.ParameterTypes,
            context.LocalBindingTypes,
            context.CurrentModuleName,
            context.FunctionReturnTypes);

        for (var i = caseBlock.Cases.Count - 1; i >= 0; i--)
        {
            var caseItem = caseBlock.Cases[i];
            var pattern = caseItem.Pattern.Value;

            var patternBindings = ExtractPatternBindings(pattern, scrutinee);

            // Extract binding types from the pattern
            var patternBindingTypes = TypeInference.ExtractPatternBindingTypesFromInferred(
                pattern,
                scrutineeType,
                [],
                context.ModuleCompilationContext.ChoiceTagArgumentTypes);

            // Create case context with both bindings and binding types
            var caseContext = context;
            if (patternBindings.Count > 0)
            {
                caseContext = caseContext.WithLocalBindings(patternBindings);
            }
            if (patternBindingTypes.Count > 0)
            {
                // Merge existing binding types with new pattern binding types
                var mergedBindingTypes = context.LocalBindingTypes is { } existingTypes
                    ? existingTypes.ToImmutableDictionary().SetItems(patternBindingTypes)
                    : patternBindingTypes;
                caseContext = caseContext.WithReplacedLocalBindingsAndTypes(caseContext.LocalBindings, mergedBindingTypes);
            }

            var caseBodyResult = ExpressionCompiler.Compile(caseItem.Expression.Value, caseContext);
            if (caseBodyResult.IsErrOrNull() is { } caseErr)
            {
                return caseErr;
            }

            var caseBody = caseBodyResult.IsOkOrNull()!;
            var conditionExpr = CompilePatternCondition(pattern, scrutinee);

            if (conditionExpr is null)
            {
                result = caseBody;
            }
            else
            {
                result = Expression.ConditionalInstance(
                    condition: conditionExpr,
                    trueBranch: caseBody,
                    falseBranch: result ?? caseBody);
            }
        }

        if (result is null)
        {
            return new CompilationError.CaseExpressionNoPatterns();
        }

        return result;
    }

    /// <summary>
    /// Analyzes a pattern and returns both variable bindings and the condition tree.
    /// </summary>
    public static PatternAnalysis AnalyzePattern(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee) =>
        AnalyzePatternRecursive(pattern, scrutinee);

    private static PatternAnalysis AnalyzePatternRecursive(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.AllPattern => PatternAnalysis.Empty,

            SyntaxTypes.Pattern.VarPattern varPattern =>
                PatternAnalysis.WithBinding(varPattern.Name, scrutinee),

            SyntaxTypes.Pattern.ParenthesizedPattern parenthesized =>
                AnalyzePatternRecursive(parenthesized.Pattern.Value, scrutinee),

            SyntaxTypes.Pattern.IntPattern intPattern =>
                PatternAnalysis.WithCondition(new PatternCondition.ValueEquals(
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(intPattern.Value)))),

            SyntaxTypes.Pattern.ListPattern listPattern =>
                AnalyzeListPattern(listPattern, scrutinee),

            SyntaxTypes.Pattern.UnConsPattern unConsPattern =>
                AnalyzeUnConsPattern(unConsPattern, scrutinee),

            SyntaxTypes.Pattern.TuplePattern tuplePattern =>
                AnalyzeTuplePattern(tuplePattern, scrutinee),

            SyntaxTypes.Pattern.NamedPattern namedPattern =>
                AnalyzeNamedPattern(namedPattern, scrutinee),

            SyntaxTypes.Pattern.RecordPattern recordPattern =>
                AnalyzeRecordPattern(recordPattern, scrutinee),

            _ => throw new NotImplementedException($"Pattern type not yet supported: {pattern.GetType().Name}")
        };
    }

    private static PatternAnalysis AnalyzeListPattern(
        SyntaxTypes.Pattern.ListPattern listPattern,
        Expression scrutinee)
    {
        if (listPattern.Elements.Count is 0)
        {
            return PatternAnalysis.WithCondition(new PatternCondition.LengthEquals(0));
        }

        if (AsConstantPattern(listPattern) is { } constantValue)
        {
            return PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var conditions = new List<PatternCondition>
        {
            new PatternCondition.LengthEquals(listPattern.Elements.Count)
        };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        for (var i = 0; i < listPattern.Elements.Count; i++)
        {
            var elementPattern = listPattern.Elements[i].Value;
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementAnalysis = AnalyzePatternRecursive(elementPattern, elementExpr);
            allBindings = allBindings.SetItems(elementAnalysis.Bindings);

            if (elementAnalysis.Condition is not PatternCondition.Always)
            {
                var index = i;
                conditions.Add(new PatternCondition.ItemCondition(
                    s => GetListElementExpression(s, index),
                    elementAnalysis.Condition));
            }
        }

        var finalCondition =
            conditions.Count is 1
            ?
            conditions[0]
            :
            new PatternCondition.Conjunction(conditions);

        return new PatternAnalysis(allBindings, finalCondition);
    }

    private static PatternAnalysis AnalyzeUnConsPattern(
        SyntaxTypes.Pattern.UnConsPattern unConsPattern,
        Expression scrutinee)
    {
        var headExpr = BuiltinHelpers.ApplyBuiltinHead(scrutinee);
        var tailExpr = BuiltinHelpers.ApplyBuiltinSkip(1, scrutinee);

        var headAnalysis = AnalyzePatternRecursive(unConsPattern.Head.Value, headExpr);
        var tailAnalysis = AnalyzePatternRecursive(unConsPattern.Tail.Value, tailExpr);

        var allBindings = ImmutableDictionary<string, Expression>.Empty
            .SetItems(headAnalysis.Bindings)
            .SetItems(tailAnalysis.Bindings);

        var conditions = new List<PatternCondition>
        {
            new PatternCondition.NotEmpty()
        };

        if (headAnalysis.Condition is not PatternCondition.Always)
        {
            conditions.Add(new PatternCondition.ItemCondition(
                BuiltinHelpers.ApplyBuiltinHead,
                headAnalysis.Condition));
        }

        if (tailAnalysis.Condition is not PatternCondition.Always)
        {
            conditions.Add(new PatternCondition.ItemCondition(
                s => BuiltinHelpers.ApplyBuiltinSkip(1, s),
                tailAnalysis.Condition));
        }

        var finalCondition = conditions.Count is 1
            ? conditions[0]
            : new PatternCondition.Conjunction(conditions);

        return new PatternAnalysis(allBindings, finalCondition);
    }

    private static PatternAnalysis AnalyzeTuplePattern(
        SyntaxTypes.Pattern.TuplePattern tuplePattern,
        Expression scrutinee)
    {
        if (AsConstantPattern(tuplePattern) is { } constantValue)
        {
            return PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var conditions = new List<PatternCondition>
        {
            new PatternCondition.LengthEquals(tuplePattern.Elements.Count)
        };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        for (var i = 0; i < tuplePattern.Elements.Count; i++)
        {
            var elementPattern = tuplePattern.Elements[i].Value;
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementAnalysis = AnalyzePatternRecursive(elementPattern, elementExpr);
            allBindings = allBindings.SetItems(elementAnalysis.Bindings);

            if (elementAnalysis.Condition is not PatternCondition.Always)
            {
                var index = i;
                conditions.Add(new PatternCondition.ItemCondition(
                    s => GetListElementExpression(s, index),
                    elementAnalysis.Condition));
            }
        }

        var finalCondition = conditions.Count is 1
            ? conditions[0]
            : new PatternCondition.Conjunction(conditions);

        return new PatternAnalysis(allBindings, finalCondition);
    }

    private static PatternAnalysis AnalyzeNamedPattern(
        SyntaxTypes.Pattern.NamedPattern namedPattern,
        Expression scrutinee)
    {
        var tagName = namedPattern.Name.Name;

        if (AsConstantPattern(namedPattern) is { } constantValue)
        {
            return PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var expectedTagName = Expression.LiteralInstance(StringEncoding.ValueFromString(tagName));
        var conditions = new List<PatternCondition>
        {
            new PatternCondition.ItemCondition(
                s => GetListElementExpression(s, 0),
                new PatternCondition.ValueEquals(expectedTagName))
        };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        if (namedPattern.Arguments.Count > 0)
        {
            var argsListExpr = GetListElementExpression(scrutinee, 1);

            conditions.Add(new PatternCondition.ItemCondition(
                s => GetListElementExpression(s, 1),
                new PatternCondition.LengthEquals(namedPattern.Arguments.Count)));

            for (var i = 0; i < namedPattern.Arguments.Count; i++)
            {
                var argPattern = namedPattern.Arguments[i].Value;
                var argExpr = GetListElementExpression(argsListExpr, i);

                var argAnalysis = AnalyzePatternRecursive(argPattern, argExpr);
                allBindings = allBindings.SetItems(argAnalysis.Bindings);

                if (argAnalysis.Condition is not PatternCondition.Always)
                {
                    var index = i;
                    conditions.Add(new PatternCondition.ItemCondition(
                        s => GetListElementExpression(GetListElementExpression(s, 1), index),
                        argAnalysis.Condition));
                }
            }
        }

        var finalCondition = conditions.Count is 1
            ? conditions[0]
            : new PatternCondition.Conjunction(conditions);

        return new PatternAnalysis(allBindings, finalCondition);
    }

    private static PatternAnalysis AnalyzeRecordPattern(
        SyntaxTypes.Pattern.RecordPattern recordPattern,
        Expression scrutinee)
    {
        var fieldsListExpr = GetListElementExpression(GetListElementExpression(scrutinee, 1), 0);

        var patternFieldNames = recordPattern.Fields
            .Select(f => f.Value)
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToList();

        var bindings = ImmutableDictionary<string, Expression>.Empty;
        for (var i = 0; i < patternFieldNames.Count; i++)
        {
            var fieldName = patternFieldNames[i];
            var fieldEntryExpr = GetListElementExpression(fieldsListExpr, i);
            var fieldValueExpr = GetListElementExpression(fieldEntryExpr, 1);
            bindings = bindings.Add(fieldName, fieldValueExpr);
        }

        return new PatternAnalysis(bindings, new PatternCondition.Always());
    }

    /// <summary>
    /// Compiles a condition tree into an Expression that evaluates to a boolean.
    /// </summary>
    public static Expression? CompileConditionToExpression(
        PatternCondition condition,
        Expression scrutinee)
    {
        return condition switch
        {
            PatternCondition.Always => null,

            PatternCondition.ValueEquals equals =>
                BuiltinHelpers.ApplyBuiltinEqualBinary(scrutinee, equals.Expected),

            PatternCondition.LengthEquals lengthEquals =>
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinLength(scrutinee),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(lengthEquals.ExpectedLength))),

            PatternCondition.NotEmpty =>
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinEqualBinary(
                        BuiltinHelpers.ApplyBuiltinLength(scrutinee),
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0))),
                    Expression.LiteralInstance(KernelFunction.ValueFromBool(false))),

            PatternCondition.ItemCondition itemCondition =>
                CompileConditionToExpression(itemCondition.SubCondition, itemCondition.Accessor(scrutinee)),

            PatternCondition.Conjunction conjunction =>
                CompileConjunction(conjunction.Conditions, scrutinee),

            _ =>
            throw new NotImplementedException(
                $"Condition type not yet supported: {condition.GetType().Name}")
        };
    }

    private static Expression? CompileConjunction(
        IReadOnlyList<PatternCondition> conditions,
        Expression scrutinee)
    {
        Expression? result = null;

        foreach (var condition in conditions)
        {
            var conditionExpr = CompileConditionToExpression(condition, scrutinee);

            if (conditionExpr is null)
                continue;

            if (result is null)
            {
                result = conditionExpr;
            }
            else
            {
                result = Expression.ConditionalInstance(
                    condition: conditionExpr,
                    trueBranch: result,
                    falseBranch: Expression.LiteralInstance(KernelFunction.ValueFromBool(false)));
            }
        }

        return result;
    }

    /// <summary>
    /// Extract variable bindings from a pattern and return expressions to access their values.
    /// </summary>
    public static IReadOnlyDictionary<string, Expression> ExtractPatternBindings(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        var analysis = AnalyzePattern(pattern, scrutinee);
        return analysis.Bindings;
    }

    /// <summary>
    /// Compiles a pattern's condition by analyzing it and converting the condition tree to an Expression.
    /// </summary>
    public static Expression? CompilePatternCondition(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        var analysis = AnalyzePatternRecursive(pattern, scrutinee);
        return CompileConditionToExpression(analysis.Condition, scrutinee);
    }

    /// <summary>
    /// Get an expression that extracts the element at the given index from a list.
    /// </summary>
    public static Expression GetListElementExpression(Expression listExpr, int index)
    {
        if (index == 0)
        {
            return BuiltinHelpers.ApplyBuiltinHead(listExpr);
        }

        var skipExpr = BuiltinHelpers.ApplyBuiltinSkip(index, listExpr);
        return BuiltinHelpers.ApplyBuiltinHead(skipExpr);
    }

    /// <summary>
    /// Attempts to convert a pattern to a constant PineValue.
    /// Returns the constant value if the pattern contains only constants,
    /// or null if the pattern contains variables.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    /// <returns>The constant PineValue if the pattern is constant, or null if it contains variables.</returns>
    /// <exception cref="NotImplementedException">Thrown for unsupported pattern types.</exception>
    public static PineValue? AsConstantPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.IntPattern intPattern =>
                IntegerEncoding.EncodeSignedInteger(intPattern.Value),

            SyntaxTypes.Pattern.ParenthesizedPattern p =>
                AsConstantPattern(p.Pattern.Value),

            SyntaxTypes.Pattern.AllPattern =>
                null, // Wildcard pattern contains no constant value

            SyntaxTypes.Pattern.VarPattern =>
                null, // Variable pattern is not constant

            SyntaxTypes.Pattern.ListPattern listPattern =>
                AsConstantListPattern(listPattern),

            SyntaxTypes.Pattern.TuplePattern tuplePattern =>
                AsConstantTuplePattern(tuplePattern),

            SyntaxTypes.Pattern.NamedPattern namedPattern =>
                AsConstantNamedPattern(namedPattern),

            SyntaxTypes.Pattern.UnConsPattern =>
                null, // UnCons pattern always binds variables

            SyntaxTypes.Pattern.RecordPattern =>
                null, // Record pattern always binds variables

            _ =>
            throw new NotImplementedException(
                $"Pattern type not yet supported for constant analysis: {pattern.GetType().Name}")
        };
    }

    private static PineValue? AsConstantListPattern(SyntaxTypes.Pattern.ListPattern listPattern)
    {
        var elements = new PineValue[listPattern.Elements.Count];

        for (var i = 0; i < listPattern.Elements.Count; i++)
        {
            var elementValue = AsConstantPattern(listPattern.Elements[i].Value);

            if (elementValue is null)
                return null;

            elements[i] = elementValue;
        }

        return PineValue.List(elements);
    }

    private static PineValue? AsConstantTuplePattern(SyntaxTypes.Pattern.TuplePattern tuplePattern)
    {
        var elements = new PineValue[tuplePattern.Elements.Count];

        for (var i = 0; i < tuplePattern.Elements.Count; i++)
        {
            var elementValue = AsConstantPattern(tuplePattern.Elements[i].Value);

            if (elementValue is null)
                return null;

            elements[i] = elementValue;
        }

        return PineValue.List(elements);
    }

    private static PineValue? AsConstantNamedPattern(SyntaxTypes.Pattern.NamedPattern namedPattern)
    {
        var arguments = new PineValue[namedPattern.Arguments.Count];

        for (var i = 0; i < namedPattern.Arguments.Count; i++)
        {
            var argValue = AsConstantPattern(namedPattern.Arguments[i].Value);
            if (argValue is null)
                return null;
            arguments[i] = argValue;
        }

        return ElmValueEncoding.TagAsPineValue(namedPattern.Name.Name, arguments);
    }

    /// <summary>
    /// Collects pattern names (variables bound by the pattern).
    /// </summary>
    public static void CollectPatternNames(
        SyntaxTypes.Pattern pattern,
        HashSet<string> names)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                names.Add(varPattern.Name);
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                foreach (var elem in tuplePattern.Elements)
                {
                    CollectPatternNames(elem.Value, names);
                }
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesized:
                CollectPatternNames(parenthesized.Pattern.Value, names);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                {
                    CollectPatternNames(elem.Value, names);
                }
                break;

            case SyntaxTypes.Pattern.UnConsPattern unConsPattern:
                CollectPatternNames(unConsPattern.Head.Value, names);
                CollectPatternNames(unConsPattern.Tail.Value, names);
                break;
        }
    }
}
