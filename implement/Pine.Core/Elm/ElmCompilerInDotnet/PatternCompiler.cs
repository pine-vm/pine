using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModelTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

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
        var scrutineeType =
            TypeInference.InferExpressionType(
                caseBlock.Expression.Value,
                context.ParameterNames,
                context.ParameterTypes,
                context.LocalBindingTypes,
                context.CurrentModuleName,
                context.FunctionTypes);

        for (var i = caseBlock.Cases.Count - 1; i >= 0; i--)
        {
            var caseItem = caseBlock.Cases[i];
            var pattern = caseItem.Pattern.Value;

            var patternBindings =
                ExtractPatternBindings(
                    pattern,
                    scrutinee,
                    scrutineeType: scrutineeType,
                    recordTypeAliasFields:
                    context.ModuleCompilationContext.RecordTypeAliasConstructors,
                    choiceTagArgumentTypes:
                    context.ModuleCompilationContext.ChoiceTagArgumentTypes);

            // Extract binding types from the pattern
            var patternBindingTypes =
                TypeInference.ExtractPatternBindingTypesFromInferred(
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
                var mergedBindingTypes =
                    context.LocalBindingTypes is { } existingTypes
                    ?
                    existingTypes.ToImmutableDictionary().SetItems(patternBindingTypes)
                    :
                    patternBindingTypes;

                caseContext =
                    caseContext.WithReplacedLocalBindingsAndTypes(caseContext.LocalBindings, mergedBindingTypes);
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
                result =
                    Expression.ConditionalInstance(
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
    /// <param name="pattern">The pattern being analyzed.</param>
    /// <param name="scrutinee">An expression for the value being matched.</param>
    /// <param name="recordFieldNames">
    /// Optional alphabetically-sorted full list of field names of the
    /// scrutinee's record type, used by record-pattern destructure to
    /// look up each named field by its true position in the record
    /// rather than by the pattern's local loop position. When the
    /// caller has type-checker access this should always be supplied
    /// for record-pattern destructures, otherwise the destructure is
    /// only correct when the pattern lists every field of the record.
    /// </param>
    public static PatternAnalysis AnalyzePattern(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        IReadOnlyList<string>? recordFieldNames = null) =>
        AnalyzePatternRecursive(
            pattern,
            scrutinee,
            scrutineeType: null,
            recordTypeAliasFields: null,
            choiceTagArgumentTypes: null,
            topLevelRecordFieldNamesOverride: recordFieldNames);

    /// <summary>
    /// Analyzes a pattern carrying the scrutinee's full
    /// <see cref="TypeInference.InferredType"/> and the module's
    /// record-type-alias dictionary. This enables the recursive
    /// descent — through <see cref="SyntaxTypes.Pattern.UnConsPattern"/>,
    /// <see cref="SyntaxTypes.Pattern.ListPattern"/>,
    /// <see cref="SyntaxTypes.Pattern.TuplePattern"/>,
    /// <see cref="SyntaxTypes.Pattern.NamedPattern"/>, and
    /// <see cref="SyntaxTypes.Pattern.AsPattern"/> — to compute the
    /// nested scrutinee's type at every level, so a record sub-pattern
    /// like the head of <c>{ start, end } :: rest</c> on a
    /// <c>List Layout</c>, or the inner record pattern of a
    /// <c>Node { start } _</c> destructure of a choice-type
    /// constructor whose argument is a record alias, still hits the
    /// static field-access path.
    /// </summary>
    public static PatternAnalysis AnalyzePattern(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes = null) =>
        AnalyzePatternRecursive(
            pattern,
            scrutinee,
            scrutineeType: scrutineeType,
            recordTypeAliasFields: recordTypeAliasFields,
            choiceTagArgumentTypes: choiceTagArgumentTypes,
            topLevelRecordFieldNamesOverride: null);

    private static PatternAnalysis AnalyzePatternRecursive(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes = null,
        IReadOnlyList<string>? topLevelRecordFieldNamesOverride = null)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.AllPattern => PatternAnalysis.Empty,

            SyntaxTypes.Pattern.VarPattern varPattern =>
            PatternAnalysis.WithBinding(varPattern.Name, scrutinee),

            SyntaxTypes.Pattern.ParenthesizedPattern parenthesized =>
            AnalyzePatternRecursive(
                parenthesized.Pattern.Value,
                scrutinee,
                scrutineeType,
                recordTypeAliasFields,
                choiceTagArgumentTypes,
                topLevelRecordFieldNamesOverride),

            SyntaxTypes.Pattern.IntPattern intPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(intPattern.Value)))),

            SyntaxTypes.Pattern.HexPattern hexPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(hexPattern.Value)))),

            SyntaxTypes.Pattern.CharPattern charPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LiteralInstance(ElmValueEncoding.ElmCharAsPineValue(charPattern.Value)))),

            SyntaxTypes.Pattern.StringPattern stringPattern =>
            PatternAnalysis.WithCondition(
                new PatternCondition.ValueEquals(
                    Expression.LiteralInstance(ElmValueEncoding.StringAsPineValue(stringPattern.Value)))),

            SyntaxTypes.Pattern.AsPattern asPattern =>
            AnalyzeAsPattern(
                asPattern,
                scrutinee,
                scrutineeType,
                recordTypeAliasFields,
                choiceTagArgumentTypes,
                topLevelRecordFieldNamesOverride),

            SyntaxTypes.Pattern.ListPattern listPattern =>
            AnalyzeListPattern(listPattern, scrutinee, scrutineeType, recordTypeAliasFields, choiceTagArgumentTypes),

            SyntaxTypes.Pattern.UnConsPattern unConsPattern =>
            AnalyzeUnConsPattern(
                unConsPattern,
                scrutinee,
                scrutineeType,
                recordTypeAliasFields,
                choiceTagArgumentTypes),

            SyntaxTypes.Pattern.TuplePattern tuplePattern =>
            AnalyzeTuplePattern(tuplePattern, scrutinee, scrutineeType, recordTypeAliasFields, choiceTagArgumentTypes),

            SyntaxTypes.Pattern.NamedPattern namedPattern =>
            AnalyzeNamedPattern(namedPattern, scrutinee, recordTypeAliasFields, choiceTagArgumentTypes),

            SyntaxTypes.Pattern.RecordPattern recordPattern =>
            AnalyzeRecordPattern(
                recordPattern,
                scrutinee,
                topLevelRecordFieldNamesOverride
                ?? SortedRecordFieldNamesFromInferredType(scrutineeType, recordTypeAliasFields)),

            SyntaxTypes.Pattern.UnitPattern =>
            PatternAnalysis.WithCondition(new PatternCondition.LengthEquals(0)),

            _ =>
            throw new NotImplementedException($"Pattern type not yet supported: {pattern.GetType().Name}")
        };
    }

    private static PatternAnalysis AnalyzeListPattern(
        SyntaxTypes.Pattern.ListPattern listPattern,
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes)
    {
        if (listPattern.Elements.Count is 0)
        {
            return PatternAnalysis.WithCondition(new PatternCondition.LengthEquals(0));
        }

        if (AsConstantPattern(listPattern) is { } constantValue)
        {
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.LengthEquals(listPattern.Elements.Count)
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        var elementType =
            scrutineeType is TypeInference.InferredType.ListType listType
            ?
            listType.ElementType
            :
            null;

        for (var i = 0; i < listPattern.Elements.Count; i++)
        {
            var elementPattern = listPattern.Elements[i].Value;
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementAnalysis =
                AnalyzePatternRecursive(
                    elementPattern,
                    elementExpr,
                    elementType,
                    recordTypeAliasFields,
                    choiceTagArgumentTypes);

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
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes)
    {
        var headExpr = BuiltinHelpers.ApplyBuiltinHead(scrutinee);
        var tailExpr = BuiltinHelpers.ApplyBuiltinSkip(1, scrutinee);

        // For `head :: tail`, the head's type is the element type of
        // the list, and the tail's type is the list itself.
        var elementType =
            scrutineeType is TypeInference.InferredType.ListType listType
            ?
            listType.ElementType
            :
            null;

        var headAnalysis =
            AnalyzePatternRecursive(
                unConsPattern.Head.Value,
                headExpr,
                elementType,
                recordTypeAliasFields,
                choiceTagArgumentTypes);

        var tailAnalysis =
            AnalyzePatternRecursive(
                unConsPattern.Tail.Value,
                tailExpr,
                scrutineeType,
                recordTypeAliasFields,
                choiceTagArgumentTypes);

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
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes)
    {
        if (AsConstantPattern(tuplePattern) is { } constantValue)
        {
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.LengthEquals(tuplePattern.Elements.Count)
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        var tupleElementTypes =
            scrutineeType is TypeInference.InferredType.TupleType tupleType
            ?
            tupleType.ElementTypes
            :
            null;

        for (var i = 0; i < tuplePattern.Elements.Count; i++)
        {
            var elementPattern = tuplePattern.Elements[i].Value;
            var elementExpr = GetListElementExpression(scrutinee, i);

            var elementType =
                tupleElementTypes is not null && i < tupleElementTypes.Count
                ?
                tupleElementTypes[i]
                :
                null;

            var elementAnalysis =
                AnalyzePatternRecursive(
                    elementPattern,
                    elementExpr,
                    elementType,
                    recordTypeAliasFields,
                    choiceTagArgumentTypes);

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
        Expression scrutinee,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes)
    {
        var tagName = namedPattern.Name.Name;

        if (AsConstantPattern(namedPattern) is { } constantValue)
        {
            return
                PatternAnalysis.WithCondition(
                    new PatternCondition.ValueEquals(Expression.LiteralInstance(constantValue)));
        }

        var expectedTagName = Expression.LiteralInstance(StringEncoding.ValueFromString(tagName));

        var conditions =
            new List<PatternCondition>
            {
                new PatternCondition.ItemCondition(
                    s => GetListElementExpression(s, 0),
                    new PatternCondition.ValueEquals(expectedTagName))
            };

        var allBindings = ImmutableDictionary<string, Expression>.Empty;

        if (namedPattern.Arguments.Count > 0)
        {
            var argsListExpr = GetListElementExpression(scrutinee, 1);

            conditions.Add(
                new PatternCondition.ItemCondition(
                    s => GetListElementExpression(s, 1),
                    new PatternCondition.LengthEquals(namedPattern.Arguments.Count)));

            // Look up the constructor's argument types so that nested
            // record patterns inside choice-type arguments (for example
            // the inner record pattern of `Node { start } _` where
            // `Node` wraps a `Range` record alias) can take the
            // static field-access path.
            IReadOnlyList<TypeInference.InferredType>? ctorArgTypes = null;

            if (choiceTagArgumentTypes is not null)
            {
                var ctorQualifiedName =
                    new SyntaxModelTypes.QualifiedNameRef(
                        namedPattern.Name.ModuleName,
                        namedPattern.Name.Name);

                if (choiceTagArgumentTypes.TryGetValue(ctorQualifiedName, out var argTypes))
                {
                    ctorArgTypes = argTypes;
                }
            }

            for (var i = 0; i < namedPattern.Arguments.Count; i++)
            {
                var argPattern = namedPattern.Arguments[i].Value;
                var argExpr = GetListElementExpression(argsListExpr, i);

                var argType =
                    ctorArgTypes is not null && i < ctorArgTypes.Count
                    ?
                    ctorArgTypes[i]
                    :
                    null;

                var argAnalysis =
                    AnalyzePatternRecursive(
                        argPattern,
                        argExpr,
                        scrutineeType: argType,
                        recordTypeAliasFields: recordTypeAliasFields,
                        choiceTagArgumentTypes: choiceTagArgumentTypes);

                allBindings = allBindings.SetItems(argAnalysis.Bindings);

                if (argAnalysis.Condition is not PatternCondition.Always)
                {
                    var index = i;

                    conditions.Add(
                        new PatternCondition.ItemCondition(
                            s => GetListElementExpression(GetListElementExpression(s, 1), index),
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

    /// <summary>
    /// Builds the bindings for a record-pattern match against
    /// <paramref name="scrutinee"/>. When <paramref name="recordFieldNames"/>
    /// is provided (the alphabetically-sorted field name list of the
    /// scrutinee's full record type, as known to the type checker),
    /// each pattern field is bound by looking up its position in that
    /// list. Otherwise we fall back to the historical positional
    /// approach: index slot <c>i</c> of the record for the i-th
    /// alphabetically-sorted pattern name. The fallback is only correct
    /// when the pattern names all of the record's fields; this is the
    /// case for every record-pattern in the in-tree code outside the
    /// LanguageService, but breaks for the
    /// <c>let { hoverItems } = hoverItemsFromParsedModule ...</c> shape
    /// the LS migration introduced. Always prefer the type-driven path
    /// at the call site.
    /// </summary>
    private static PatternAnalysis AnalyzeRecordPattern(
        SyntaxTypes.Pattern.RecordPattern recordPattern,
        Expression scrutinee,
        IReadOnlyList<string>? recordFieldNames)
    {
        var fieldsListExpr = GetListElementExpression(GetListElementExpression(scrutinee, 1), 0);

        var bindings = ImmutableDictionary<string, Expression>.Empty;

        if (recordFieldNames is not null)
        {
            // Type-driven path: the caller knows the scrutinee's full
            // record-field layout, so we compute each pattern field's
            // position in the record directly. Records are stored with
            // their fields sorted alphabetically, so the index of the
            // pattern field's name in `recordFieldNames` is the slot to
            // read in the record's fields list.
            var fieldNameToIndex =
                recordFieldNames
                .Select((name, idx) => (name, idx))
                .ToDictionary(x => x.name, x => x.idx);

            foreach (var fieldNode in recordPattern.Fields)
            {
                var fieldName = fieldNode.Value;

                if (!fieldNameToIndex.TryGetValue(fieldName, out var indexInRecord))
                {
                    // Pattern names a field absent from the inferred
                    // record type — should not happen for well-typed
                    // programs, but be defensive: fall back to position.
                    indexInRecord = bindings.Count;
                }

                var fieldEntryExpr = GetListElementExpression(fieldsListExpr, indexInRecord);
                var fieldValueExpr = GetListElementExpression(fieldEntryExpr, 1);
                bindings = bindings.Add(fieldName, fieldValueExpr);
            }

            return new PatternAnalysis(bindings, new PatternCondition.Always());
        }

        // Fallback path: we don't know the scrutinee's full layout
        // statically. Look each pattern field up by name at runtime,
        // using the same helper the dot-access fallback uses. This is
        // O(n) over the record's fields (the static path above is
        // O(1)), but is correct in every case — including open record
        // types, records destructured before the type checker has full
        // type info, and strict-subset patterns whose surrounding
        // declaration has no type annotation.
        foreach (var fieldNode in recordPattern.Fields)
        {
            var fieldName = fieldNode.Value;

            var lookupEnv =
                Expression.ListInstance(
                    [
                    scrutinee,
                    Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName))
                    ]);

            var fieldValueExpr =
                new Expression.ParseAndEval(
                    encoded: Expression.LiteralInstance(RecordRuntime.PineFunctionForRecordAccessAsValue),
                    environment: lookupEnv);

            bindings = bindings.Add(fieldName, fieldValueExpr);
        }

        return new PatternAnalysis(bindings, new PatternCondition.Always());
    }

    private static PatternAnalysis AnalyzeAsPattern(
        SyntaxTypes.Pattern.AsPattern asPattern,
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes,
        IReadOnlyList<string>? topLevelRecordFieldNamesOverride)
    {
        // First, analyze the inner pattern
        var innerAnalysis =
            AnalyzePatternRecursive(
                asPattern.Pattern.Value,
                scrutinee,
                scrutineeType,
                recordTypeAliasFields,
                choiceTagArgumentTypes,
                topLevelRecordFieldNamesOverride);

        // Add the "as" binding - this binds the entire scrutinee to the alias name
        var bindings = innerAnalysis.Bindings.Add(asPattern.Name.Value, scrutinee);

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
                result =
                    Expression.ConditionalInstance(
                        condition: conditionExpr,
                        trueBranch: result,
                        falseBranch: Expression.LiteralInstance(KernelFunction.ValueFromBool(false)));
            }
        }

        return result;
    }

    /// <summary>
    /// If <paramref name="inferredType"/> is a closed record type,
    /// returns its field names sorted alphabetically (the order in
    /// which Elm records' fields are stored in the Pine encoding).
    /// If <paramref name="inferredType"/> is a <see cref="TypeInference.InferredType.ChoiceType"/>
    /// whose qualified name resolves to a record type-alias in
    /// <paramref name="recordTypeAliasFields"/>, the alias's field
    /// names are returned (sorted alphabetically) — this is what
    /// makes record-pattern destructuring through type aliases (for
    /// example <c>type alias Layout = { start : Int, end : Int }</c>)
    /// take the static field-access path instead of the runtime
    /// <see cref="RecordRuntime.PineFunctionForRecordAccessAsValue"/>
    /// fallback. Otherwise returns <c>null</c>, signalling to the
    /// pattern compiler that the record's full layout is not known.
    /// </summary>
    /// <param name="inferredType">The type inferred for the scrutinee.</param>
    /// <param name="recordTypeAliasFields">
    /// Optional map of qualified record type-alias names to the
    /// alias's field names (the order does not have to be alphabetical;
    /// this method sorts before returning). Typically the
    /// <see cref="ModuleCompilationContext.RecordTypeAliasConstructors"/>
    /// dictionary.
    /// </param>
    public static IReadOnlyList<string>? SortedRecordFieldNamesFromInferredType(
        TypeInference.InferredType inferredType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields = null)
    {
        if (inferredType is TypeInference.InferredType.RecordType recordType)
        {
            return
                [
                .. recordType.Fields
                .Select(f => f.FieldName)
                .OrderBy(name => name, StringComparer.Ordinal)
                ];
        }

        if (recordTypeAliasFields is not null &&
            inferredType is TypeInference.InferredType.ChoiceType choiceType)
        {
            var qualifiedName =
                QualifiedNameHelper.ToQualifiedNameRef(choiceType.ModuleName, choiceType.TypeName);

            if (recordTypeAliasFields.TryGetValue(qualifiedName, out var aliasFields))
            {
                return
                    [
                    .. aliasFields.OrderBy(name => name, StringComparer.Ordinal)
                    ];
            }
        }

        return null;
    }

    /// <summary>
    /// Extract variable bindings from a pattern and return expressions to access their values.
    /// </summary>
    /// <param name="pattern">The pattern to extract bindings for.</param>
    /// <param name="scrutinee">An expression for the value being matched.</param>
    /// <param name="recordFieldNames">
    /// Optional alphabetically-sorted full list of field names of the
    /// scrutinee's record type, used by record-pattern destructure to
    /// look up each named field by its true position in the record.
    /// See <see cref="AnalyzePattern(SyntaxTypes.Pattern, Expression, IReadOnlyList{string})"/> for details.
    /// </param>
    public static IReadOnlyDictionary<string, Expression> ExtractPatternBindings(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        IReadOnlyList<string>? recordFieldNames = null)
    {
        var analysis = AnalyzePattern(pattern, scrutinee, recordFieldNames);
        return analysis.Bindings;
    }

    /// <summary>
    /// Extract variable bindings from a pattern, threading the
    /// scrutinee's <see cref="TypeInference.InferredType"/> and the
    /// module's record-type-alias dictionary so that nested
    /// record-pattern destructures (for example the head of
    /// <c>{ start, end } :: rest</c> on a list whose element type is a
    /// record alias) can use static field-index lookup.
    /// </summary>
    public static IReadOnlyDictionary<string, Expression> ExtractPatternBindings(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        TypeInference.InferredType? scrutineeType,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? recordTypeAliasFields,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? choiceTagArgumentTypes = null)
    {
        var analysis =
            AnalyzePattern(pattern, scrutinee, scrutineeType, recordTypeAliasFields, choiceTagArgumentTypes);

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
                scrutinee,
                scrutineeType: null,
                recordTypeAliasFields: null);

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

            SyntaxTypes.Pattern.HexPattern hexPattern =>
            IntegerEncoding.EncodeSignedInteger(hexPattern.Value),

            SyntaxTypes.Pattern.CharPattern charPattern =>
            ElmValueEncoding.ElmCharAsPineValue(charPattern.Value),

            SyntaxTypes.Pattern.StringPattern stringPattern =>
            ElmValueEncoding.StringAsPineValue(stringPattern.Value),

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

            SyntaxTypes.Pattern.AsPattern =>
            null, // As pattern always binds a variable

            SyntaxTypes.Pattern.UnitPattern =>
            PineValue.EmptyList, // Unit is the empty list

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

            case SyntaxTypes.Pattern.AsPattern asPattern:
                names.Add(asPattern.Name.Value);
                CollectPatternNames(asPattern.Pattern.Value, names);
                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                {
                    CollectPatternNames(arg.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                {
                    names.Add(field.Value);
                }

                break;
        }
    }
}
