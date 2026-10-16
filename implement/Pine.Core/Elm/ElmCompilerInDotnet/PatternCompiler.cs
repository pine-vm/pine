using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm pattern matching to Pine expressions.
/// </summary>
public class PatternCompiler
{
    private const long MaxInlinedScrutineeExpansion = 1_000;

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
        SyntaxTypes.Expression.CaseExpression caseExpression,
        ExpressionCompilationContext context)
    {
        var scrutineeResult = ExpressionCompiler.Compile(caseExpression.Expression, context);

        if (scrutineeResult.IsErrOrNull() is { } scrutineeErr)
        {
            return scrutineeErr;
        }

        if (scrutineeResult.IsOkOrNull() is not { } scrutinee)
        {
            throw new NotImplementedException(
                "CompileCaseExpression: unexpected result type: " + scrutineeResult.GetType().Name);
        }

        return CompileCaseBlock(scrutinee, caseExpression.Cases, context);
    }

    /// <summary>
    /// Compiles a case block expression to a Pine expression.
    /// </summary>
    public static Result<CompilationError, Expression> CompileCaseBlock(
        Expression scrutineeExpr,
        IReadOnlyList<SyntaxTypes.Case> cases,
        ExpressionCompilationContext context)
    {
        Result<CompilationError, Expression> CompileViaInvocation()
        {
            var invokedResult =
                CompileCaseBlockViaInvocation(
                    scrutineeExpr,
                    cases,
                    context);

            if (invokedResult.IsErrOrNull() is { } invokedErr)
            {
                return
                    invokedErr
                    .Scoped(
                        "CompileCaseBlockViaInvocation failed for case block with scrutinee expression of size " +
                        scrutineeExpr.SubexpressionCount);
            }

            if (invokedResult.IsOkOrNull() is not { } invokedExpr)
            {
                throw new NotImplementedException(
                    "CompileCaseBlock: unexpected result type from invocation-based compilation: " +
                    invokedResult.GetType().Name);
            }

            return invokedExpr;
        }

        if (scrutineeExpr.SubexpressionCount > 400)
        {
            return CompileViaInvocation();
        }

        var inlinedResult =
            CompileCaseBlockInliningScrutinee(
                scrutineeExpr,
                cases,
                context);

        if (inlinedResult.IsErrOrNull() is { } inlinedErr)
        {
            return
                inlinedErr
                .Scoped(
                    "CompileCaseBlockInliningScrutinee failed for case block with scrutinee expression of size " +
                    scrutineeExpr.SubexpressionCount);
        }

        if (inlinedResult.IsOkOrNull() is not { } inlinedExpr)
        {
            throw new NotImplementedException(
                "CompileCaseBlock: unexpected result type from inlined compilation: " + inlinedResult.GetType().Name);
        }

        var inlinedScrutineeOccurrences =
            CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
                inlinedExpr,
                scrutineeExpr);

        /*
         * The invocation wrapper only pays off when inlining actually duplicates runtime work.
         * Many case blocks use their scrutinee once; wrapping those blocks merely adds an
         * invocation boundary that prevents the VM inliner from seeing through parser helpers.
         */
        if (ShouldCompileCaseBlockViaInvocation(scrutineeExpr, inlinedScrutineeOccurrences))
        {
            return CompileViaInvocation();
        }

        return inlinedExpr;
    }

    internal static bool ShouldCompileCaseBlockViaInvocation(
        Expression scrutineeExpr,
        int inlinedScrutineeOccurrences) =>
        scrutineeExpr.SubexpressionCount > 400 ||
        (MaxInlinedScrutineeExpansion < scrutineeExpr.SubexpressionCount * inlinedScrutineeOccurrences &&
        Expression.EnumerateSelfAndDescendants(scrutineeExpr)
        .Any(descendant => descendant is Expression.Eval));

    /// <summary>
    /// Compiles a case block expression to a Pine expression via inlining the scrutinee expression into each pattern match condition.
    /// This inlining means the scrutinee expression is duplicated in the generated code for each pattern match condition.
    /// In combination with nested case blocks, this can lead to exponential growth in
    /// the size of the resulting expression if there are many patterns or if the scrutinee is complex.
    /// </summary>
    public static Result<CompilationError, Expression> CompileCaseBlockInliningScrutinee(
        Expression scrutineeExpr,
        IReadOnlyList<SyntaxTypes.Case> cases,
        ExpressionCompilationContext context)
    {
        Expression? result = null;

        for (var i = cases.Count - 1; i >= 0; i--)
        {
            var caseItem = cases[i];
            var pattern = caseItem.Pattern;

            var patternBindings =
                ExtractPatternBindings(
                    pattern,
                    scrutineeExpr);

            var caseContext = context.WithLocalBindings(patternBindings);

            var caseBodyResult = ExpressionCompiler.Compile(caseItem.Expression, caseContext);

            if (caseBodyResult.IsErrOrNull() is { } caseErr)
            {
                return caseErr;
            }

            if (caseBodyResult.IsOkOrNull() is not { } caseBody)
            {
                throw new NotImplementedException(
                    "CompileCaseExpression: unexpected result type: " + caseBodyResult.GetType().Name);
            }

            var conditionExpr = CompilePatternCondition(pattern, scrutineeExpr);

            if (conditionExpr is null)
            {
                result = caseBody;
            }
            else
            {
                result =
                    Expression.ConditionalInst(
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
    /// Compiles a case block using an eval invocation to contain all case arms.
    /// </summary>
    public static Result<CompilationError, Expression> CompileCaseBlockViaInvocation(
        Expression scrutineeOuterExpr,
        IReadOnlyList<SyntaxTypes.Case> cases,
        ExpressionCompilationContext outerContext)
    {
        // Find out which bindings are used and must be forwarded

        var bindingsNames = ImmutableHashSet<string>.Empty;

        for (var caseIndex = 0; caseIndex < cases.Count; caseIndex++)
        {
            var caseItem = cases[caseIndex];

            var caseExpressionRefs = new HashSet<string>();
            ExpressionCompiler.CollectExpressionReferences(caseItem.Expression, caseExpressionRefs);

            var casePatternNames = new HashSet<string>();
            CollectPatternNames(caseItem.Pattern, casePatternNames);

            bindingsNames =
                bindingsNames.Union(
                    caseExpressionRefs
                    .Except(casePatternNames)
                    .Where(
                        name =>
                        outerContext.ParameterNames.ContainsKey(name) ||
                        (outerContext.LocalBindings?.ContainsKey(name) ?? false)));
        }

        var bindingsNamesSorted =
            bindingsNames
            .OrderBy(name => name)
            .ToImmutableList();

        /*
         * Compose the layout for the inner expression so it follows the
         * established convention used by all other compiled bodies:
         *
         *   Item 0:     the env-functions list (forwarded from the outer
         *               environment so that sibling-SCC references inside
         *               the case-arm bodies â€” which always look up
         *               Env[0][functionIndex] â€” keep working).
         *   Item 1:     the scrutinee.
         *   Item 2 - N: all bindings required by the case-arm expressions.
         * */

        var argumentList = new Expression[2 + bindingsNames.Count];

        argumentList[0] = GetListElementExpression(Expression.EnvironmentInstance, 0);
        argumentList[1] = scrutineeOuterExpr;

        for (var i = 0; i < bindingsNamesSorted.Count; i++)
        {
            var name = bindingsNamesSorted[i];

            var nameExprResult =
                ExpressionCompiler.CompileFunctionOrValue(
                    SyntaxTypes.Expression.Identifier.Create(moduleName: [], name),
                    outerContext);

            if (nameExprResult.IsErrOrNull() is { } nameExprErr)
            {
                return
                    nameExprErr
                    .Scoped(
                        $"CompileCaseBlockViaInvocation: failed to compile expression for binding {name} required by case arms");
            }

            if (nameExprResult.IsOkOrNull() is not { } nameExpr)
            {
                throw new NotImplementedException(
                    "CompileCaseBlockViaInvocation: unexpected result type when compiling binding expression: " +
                    nameExprResult.GetType().Name);
            }

            argumentList[2 + i] = nameExpr;
        }

        var scrutineeInnerExpr = GetListElementExpression(Expression.EnvironmentInstance, 1);

        var innerBindings =
            bindingsNamesSorted
            .Select((name, index) => (name, expr: GetListElementExpression(Expression.EnvironmentInstance, 2 + index)))
            .ToImmutableDictionary(
                nameAndExpr => nameAndExpr.name,
                nameAndExpr => nameAndExpr.expr);

        var innerContext =
            outerContext
            .WithReplacedLocalBindings(innerBindings);

        Expression? invokedBodyExpr = null;

        for (var i = cases.Count - 1; i >= 0; i--)
        {
            var caseItem = cases[i];
            var pattern = caseItem.Pattern;

            var patternBindings =
                ExtractPatternBindings(
                    pattern,
                    scrutineeInnerExpr);

            var caseContext = innerContext.WithLocalBindings(patternBindings);

            var caseBodyResult = ExpressionCompiler.Compile(caseItem.Expression, caseContext);

            if (caseBodyResult.IsErrOrNull() is { } caseErr)
            {
                return caseErr;
            }

            if (caseBodyResult.IsOkOrNull() is not { } caseBody)
            {
                throw new NotImplementedException(
                    "CompileCaseExpression: unexpected result type: " + caseBodyResult.GetType().Name);
            }

            var conditionExpr = CompilePatternCondition(pattern, scrutineeInnerExpr);

            if (conditionExpr is null)
            {
                invokedBodyExpr = caseBody;
            }
            else
            {
                invokedBodyExpr =
                    Expression.ConditionalInst(
                        condition: conditionExpr,
                        trueBranch: caseBody,
                        falseBranch: invokedBodyExpr ?? caseBody);
            }
        }

        if (invokedBodyExpr is null)
        {
            return new CompilationError.CaseExpressionNoPatterns();
        }

        var invokedBodyExprReduced =
            CodeAnalysis.ReducePineExpression.ReduceExpressionBottomUp(
                invokedBodyExpr,
                parseCache: outerContext.ParseCache,
                reducedExpressionCache: outerContext.ReducedExpressionCache);

        var invokedBodyExprEncoded =
            ExpressionEncoding.EncodeExpressionAsValue(invokedBodyExpr);

        return
            new Expression.Eval(
                encoded: Expression.LitralInst(invokedBodyExprEncoded),
                environment: Expression.ListInst(argumentList));
    }

    /// <summary>
    /// Analyzes a pattern and returns both variable bindings and the condition tree.
    /// </summary>
    /// <param name="pattern">The pattern being analyzed.</param>
    /// <param name="scrutinee">An expression for the value being matched.</param>
    public static PatternAnalysis AnalyzePattern(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee) =>
        AnalyzePatternRecursive(
            pattern,
            scrutinee);

    private static PatternAnalysis AnalyzePatternRecursive(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.AllPattern => PatternAnalysis.Empty,

            SyntaxTypes.Pattern.VarPattern varPattern =>
            PatternAnalysis.WithBinding(varPattern.Name, scrutinee),

            SyntaxTypes.Pattern.IntPattern intPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(intPattern.Value)))),

            SyntaxTypes.Pattern.CharPattern charPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LitralInst(ElmValueEncoding.ElmCharAsPineValue(charPattern.Value)))),

            SyntaxTypes.Pattern.StringPattern stringPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LitralInst(ElmValueEncoding.StringAsPineValue(stringPattern.Value)))),

            SyntaxTypes.Pattern.AsPattern asPattern =>
            AnalyzeAsPattern(
                asPattern,
                scrutinee),

            SyntaxTypes.Pattern.ListPattern listPattern =>
            AnalyzeListPattern(listPattern, scrutinee),

            SyntaxTypes.Pattern.UnConsPattern unConsPattern =>
            AnalyzeUnConsPattern(
                unConsPattern,
                scrutinee),

            SyntaxTypes.Pattern.TuplePattern tuplePattern =>
            AnalyzeTuplePattern(tuplePattern, scrutinee),

            SyntaxTypes.Pattern.NamedPattern namedPattern =>
            AnalyzeNamedPattern(namedPattern, scrutinee),

            SyntaxTypes.Pattern.RecordPattern =>
            throw new InvalidOperationException("Record patterns must be lowered before Pine emission."),

            SyntaxTypes.Pattern.UnitPattern =>
            PatternAnalysis.WithCondition(new PatternCondition.LengthEquals(0)),

            SyntaxTypes.Pattern.FloatPattern =>
            throw new NotImplementedException($"Pattern type not yet supported: {pattern.GetType().Name}"),

            _ =>
            throw new NotImplementedException(
                $"{nameof(AnalyzePatternRecursive)} does not handle pattern variant: {pattern.GetType().Name}")
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
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LitralInst(constantValue)));
        }

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.LengthEquals(listPattern.Elements.Count)
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        for (var i = 0; i < listPattern.Elements.Count; i++)
        {
            var elementPattern = listPattern.Elements[i];
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementAnalysis =
                AnalyzePatternRecursive(
                    elementPattern,
                    elementExpr);

            allBindings = allBindings.SetItems(elementAnalysis.Bindings);

            if (elementAnalysis.Condition is not PatternCondition.Always)
            {
                var index = i;

                conditions.Add(
                    new PatternCondition.ItemCondition(
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

        var headAnalysis =
            AnalyzePatternRecursive(
                unConsPattern.Head,
                headExpr);

        var tailAnalysis =
            AnalyzePatternRecursive(
                unConsPattern.Tail,
                tailExpr);

        var allBindings =
            ImmutableDictionary<string, Expression>.Empty
            .SetItems(headAnalysis.Bindings)
            .SetItems(tailAnalysis.Bindings);

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.NotEmpty()
            };

        if (headAnalysis.Condition is not PatternCondition.Always)
        {
            conditions.Add(
                new PatternCondition.ItemCondition(
                    BuiltinHelpers.ApplyBuiltinHead,
                    headAnalysis.Condition));
        }

        if (tailAnalysis.Condition is not PatternCondition.Always)
        {
            conditions.Add(
                new PatternCondition.ItemCondition(
                    s => BuiltinHelpers.ApplyBuiltinSkip(1, s),
                    tailAnalysis.Condition));
        }

        var finalCondition =
            conditions.Count is 1
            ?
            conditions[0]
            :
            new PatternCondition.Conjunction(conditions);

        return new PatternAnalysis(allBindings, finalCondition);
    }

    private static PatternAnalysis AnalyzeTuplePattern(
        SyntaxTypes.Pattern.TuplePattern tuplePattern,
        Expression scrutinee)
    {
        if (AsConstantPattern(tuplePattern) is { } constantValue)
        {
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LitralInst(constantValue)));
        }

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.LengthEquals(tuplePattern.Elements.Count)
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        for (var i = 0; i < tuplePattern.Elements.Count; i++)
        {
            var elementPattern = tuplePattern.Elements[i];
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementAnalysis =
                AnalyzePatternRecursive(
                    elementPattern,
                    elementExpr);

            allBindings = allBindings.SetItems(elementAnalysis.Bindings);

            if (elementAnalysis.Condition is not PatternCondition.Always)
            {
                var index = i;

                conditions.Add(
                    new PatternCondition.ItemCondition(
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

    private static PatternAnalysis AnalyzeNamedPattern(
        SyntaxTypes.Pattern.NamedPattern namedPattern,
        Expression scrutinee)
    {
        var tagName = namedPattern.Name.Name;

        if (AsConstantPattern(namedPattern) is { } constantValue)
        {
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LitralInst(constantValue)));
        }

        var expectedTagName = Expression.LitralInst(StringEncoding.ValueFromString(tagName));

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.ItemCondition(
                    s => GetListElementExpression(s, 1),
                    new PatternCondition.ValueEquals(expectedTagName))
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        if (namedPattern.Arguments.Count > 0)
        {
            for (var i = 0; i < namedPattern.Arguments.Count; i++)
            {
                var argPattern = namedPattern.Arguments[i];
                var argExpr = GetListElementExpression(scrutinee, 2 + i);

                var argAnalysis =
                    AnalyzePatternRecursive(
                        argPattern,
                        argExpr);

                allBindings = allBindings.SetItems(argAnalysis.Bindings);

                if (argAnalysis.Condition is not PatternCondition.Always)
                {
                    var index = i;

                    conditions.Add(
                        new PatternCondition.ItemCondition(
                            s => GetListElementExpression(s, 2 + index),
                            argAnalysis.Condition));
                }
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

    private static PatternAnalysis AnalyzeAsPattern(
        SyntaxTypes.Pattern.AsPattern asPattern,
        Expression scrutinee)
    {
        // First, analyze the inner pattern
        var innerAnalysis =
            AnalyzePatternRecursive(
                asPattern.Pattern,
                scrutinee);

        // Add the "as" binding - this binds the entire scrutinee to the alias name
        var bindings = innerAnalysis.Bindings.Add(asPattern.Name, scrutinee);

        return new PatternAnalysis(bindings, innerAnalysis.Condition);
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
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(lengthEquals.ExpectedLength))),

            PatternCondition.NotEmpty =>
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinLength(scrutinee),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0))),
                Expression.LitralInst(BuiltinFunction.ValueFromBool(false))),

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
                result =
                    Expression.ConditionalInst(
                        condition: conditionExpr,
                        trueBranch: result,
                        falseBranch: Expression.LitralInst(BuiltinFunction.ValueFromBool(false)));
            }
        }

        return result;
    }

    /// <summary>
    /// Extract variable bindings from a pattern and return expressions to access their values.
    /// </summary>
    /// <param name="pattern">The pattern to extract bindings for.</param>
    /// <param name="scrutinee">An expression for the value being matched.</param>
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
        var analysis =
            AnalyzePatternRecursive(
                pattern,
                scrutinee);

        return CompileConditionToExpression(analysis.Condition, scrutinee);
    }

    /// <summary>
    /// Get an expression that extracts the element at the given index from a list.
    /// </summary>
    public static Expression GetListElementExpression(Expression listExpr, int index)
    {
        if (index is 0)
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

            SyntaxTypes.Pattern.CharPattern charPattern =>
            ElmValueEncoding.ElmCharAsPineValue(charPattern.Value),

            SyntaxTypes.Pattern.StringPattern stringPattern =>
            ElmValueEncoding.StringAsPineValue(stringPattern.Value),

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

            SyntaxTypes.Pattern.AsPattern =>
            null, // As pattern always binds a variable

            SyntaxTypes.Pattern.UnitPattern =>
            PineValue.EmptyList, // Unit is the empty list

            SyntaxTypes.Pattern.FloatPattern =>
            throw new NotImplementedException(
                $"Pattern type not yet supported for constant analysis: {pattern.GetType().Name}"),

            _ =>
            throw new NotImplementedException(
                $"{nameof(AsConstantPattern)} does not handle pattern variant: {pattern.GetType().Name}")
        };
    }

    private static PineValue? AsConstantListPattern(SyntaxTypes.Pattern.ListPattern listPattern)
    {
        var elements = new PineValue[listPattern.Elements.Count];

        for (var i = 0; i < listPattern.Elements.Count; i++)
        {
            var elementValue = AsConstantPattern(listPattern.Elements[i]);

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
            var elementValue = AsConstantPattern(tuplePattern.Elements[i]);

            if (elementValue is null)
                return null;

            elements[i] = elementValue;
        }

        return PineValue.List(elements);
    }

    private static PineValue? AsConstantNamedPattern(SyntaxTypes.Pattern.NamedPattern namedPattern)
    {
        // Special handling for Bool constructors: True and False use the kernel
        // representation (Blob([4]) and Blob([2])) rather than the generic tag representation.
        if (namedPattern.Arguments.Count is 0)
        {
            if (namedPattern.Name.Name is "True")
                return ExpressionCompiler.EmitBooleanLiteral(true);

            if (namedPattern.Name.Name is "False")
                return ExpressionCompiler.EmitBooleanLiteral(false);
        }

        var arguments = new PineValue[namedPattern.Arguments.Count];

        for (var i = 0; i < namedPattern.Arguments.Count; i++)
        {
            var argValue = AsConstantPattern(namedPattern.Arguments[i]);

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
                    CollectPatternNames(elem, names);
                }

                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                {
                    CollectPatternNames(elem, names);
                }

                break;

            case SyntaxTypes.Pattern.UnConsPattern unConsPattern:
                CollectPatternNames(unConsPattern.Head, names);
                CollectPatternNames(unConsPattern.Tail, names);
                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                names.Add(asPattern.Name);
                CollectPatternNames(asPattern.Pattern, names);
                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                {
                    CollectPatternNames(arg, names);
                }

                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                {
                    names.Add(field.FieldName);
                }

                break;

            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.UnitPattern:
            case SyntaxTypes.Pattern.CharPattern:
            case SyntaxTypes.Pattern.StringPattern:
            case SyntaxTypes.Pattern.IntPattern:
            case SyntaxTypes.Pattern.FloatPattern:
                break;

            default:
                throw new NotImplementedException(
                    $"{nameof(CollectPatternNames)} does not handle pattern variant: {pattern.GetType().Name}");
        }
    }
}
