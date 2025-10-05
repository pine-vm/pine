using Pine.Core.Addressing;
using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

using StaticExpressionGen = StaticExpression<StaticFunctionIdentifier>;


/// <summary>
/// Helpers to analyze Pine <see cref="Expression"/> trees with respect to their relationship to the environment.
/// </summary>
public class CodeAnalysis
{


    public static Result<string, (StaticProgram staticProgram, IReadOnlyDictionary<DeclQualifiedName, string> declsFailed)>
        ParseAsStaticMonomorphicProgram(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var namesFromCompiledEnv =
            NamesFromCompiledEnv.FromCompiledEnvironment(parsedEnvironment, parseCache);

        var includedFunctionRecords = new Dictionary<DeclQualifiedName, ElmInteractiveEnvironment.FunctionRecord>();

        foreach (var parsedModule in parsedEnvironment.Modules)
        {
            var moduleName = parsedModule.moduleName.Split('.');

            foreach (var decl in parsedModule.moduleContent.FunctionDeclarations)
            {
                var declQualifiedName = new DeclQualifiedName(moduleName, decl.Key);

                if (!includeDeclaration(declQualifiedName))
                    continue;

                var functionRecordResult =
                    ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(decl.Value, parseCache);

                if (functionRecordResult.IsErrOrNull() is { } err)
                {
                    return "Failed parsing declaration '" + declQualifiedName.FullName + "' as function record: " + err;
                }

                if (functionRecordResult.IsOkOrNull() is not { } functionRecord)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        functionRecordResult.GetType().Name);
                }

                includedFunctionRecords[declQualifiedName] = functionRecord;
            }
        }

        return
            ParseAsStaticMonomorphicProgram(
                includedFunctionRecords,
                namesFromCompiledEnv.NameFromDecl,
                parseCache);
    }

    public static (StaticProgram staticProgram, IReadOnlyDictionary<DeclQualifiedName, string> declsFailed)
        ParseAsStaticMonomorphicProgram(
        IReadOnlyDictionary<DeclQualifiedName, ElmInteractiveEnvironment.FunctionRecord> rootDecls,
        Func<PineValue, PineValueClass, DeclQualifiedName?> nameForDecl,
        PineVMParseCache parseCache)
    {
        /*
         * With the overall design, parsing of roots that are higher-kinded functions (e.g., `List.map`) will fail.
         * Thus, it is normal for us to fail to parse a subset of declarations when feeding whole modules unfiltered into the code analysis.
         * To account for this, we collect all parsing failures in a separate collection,
         * instead of exiting when parsing of one of the given roots fails.
         * */

        Dictionary<StaticFunctionIdentifier, (Expression origExpr, StaticExpressionGen body)> lessSpecializedInterfaces = [];

        HashSet<StaticFunctionIdentifier> rootsIds = [];

        Dictionary<DeclQualifiedName, string> declsFailures = [];


        bool DontInline(PineValue expr, PineValueClass envClass)
        {
            return nameForDecl(expr, envClass) is not null;
        }


        foreach (var rootDecl in rootDecls)
        {
            var parseRootResult =
                ParseAsStaticMonomorphicProgram(
                    rootDecl.Value,
                    DontInline,
                    parseCache);

            if (parseRootResult.IsErrOrNull() is { } err)
            {
                declsFailures[rootDecl.Key] =
                    "Failed to parse from root '" + rootDecl.Key.FullName + "': " + err;

                continue;
            }

            if (parseRootResult.IsOkOrNull() is not { } parsedRoot)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseRootResult.GetType().Name);
            }

            rootsIds.Add(parsedRoot.RootId);

            foreach (var entry in parsedRoot.AllReferenced)
            {
                if (lessSpecializedInterfaces.TryGetValue(entry.Key, out var existing))
                {
                    if (!existing.body.Equals(entry.Value.body))
                    {
                        throw new Exception(
                            "Conflict: Two different function bodies for the same function identifier: " +
                            entry.Key + "\nExisting body:\n" + existing.body + "\nNew body:\n" + entry.Value.body);
                    }
                }
                else
                {
                    lessSpecializedInterfaces[entry.Key] = entry.Value;
                }
            }
        }

        var hashCache = new ConcurrentPineValueHashCache();

        StaticFunctionIdentifier ReduceFunctionIdentifier(StaticFunctionIdentifier functionIdentifier)
        {
            /*
             * The (mapped) environment classes built in call sites in the parsed expressions can
             * contain superfluous items. Therefore, we map the environment value class to a sufficient
             * superclass before using it to look up the name assigned to the combination of
             * expression and environment class.
             * */

            // First, find all matches, then take the most specific one.

            var candidateNames =
                lessSpecializedInterfaces
                .Where(entry => entry.Key.EncodedExpr == functionIdentifier.EncodedExpr)
                .Where(entry => entry.Key.EnvClass.SatisfiedByConstraint(functionIdentifier.EnvClass))
                .Select(entry => entry.Key.EnvClass)
                .ToImmutableArray();

            if (candidateNames.Length is 0)
            {
                return functionIdentifier;
            }

            var mostSpecificName =
                candidateNames
                .OrderDescending(PineValueClassSpecificityComparer.Instance)
                .First();

            return new StaticFunctionIdentifier(functionIdentifier.EncodedExpr, mostSpecificName);
        }

        DeclQualifiedName DeclNameCombined(StaticFunctionIdentifier functionIdentifierOrig)
        {
            var functionIdentifier = ReduceFunctionIdentifier(functionIdentifierOrig);

            if (nameForDecl(functionIdentifier.EncodedExpr, functionIdentifier.EnvClass) is { } direct)
            {
                return direct;
            }

            return
                new DeclQualifiedName(
                    [],
                    AnonymousFunctionName(functionIdentifier.EncodedExpr, functionIdentifier.EnvClass, hashCache));
        }

        var namedFunctions =
            lessSpecializedInterfaces
            .ToFrozenDictionary(
                keySelector:
                entry => DeclNameCombined(entry.Key),
                elementSelector:
                entry =>
                {
                    var tInspectDeclName = DeclNameCombined(entry.Key);



                    var bodyMapped = StaticExpressionGen.MapFunctionIdentifier(entry.Value.body, DeclNameCombined);

                    return
                        (entry.Value.origExpr, bodyMapped, entry.Key.EnvClass);
                });

        var namedFunctionsOnlyBodies =
            namedFunctions
            .ToFrozenDictionary(
                keySelector: entry => entry.Key,
                elementSelector: entry => entry.Value.bodyMapped);

        var simplifiedFunctions =
            SimplifyFunctions(namedFunctionsOnlyBodies);

        var rootsNames =
            rootsIds
            .Select(DeclNameCombined)
            .ToImmutableHashSet();

        var simplifiedFunctionsReachable =
            FilterForRoots(simplifiedFunctions, rootsNames);

        var mergedFunctions =
            simplifiedFunctionsReachable
            .ToFrozenDictionary(
                keySelector:
                entry => entry.Key,
                elementSelector:
                entry =>
                {
                    var origEntry = namedFunctions[entry.Key];

                    var prunedBody = simplifiedFunctions[entry.Key];

                    var functionInterface = InterfaceForFunction(prunedBody, origEntry.EnvClass);

                    return (origEntry.origExpr, interf: functionInterface, prunedBody, origEntry.EnvClass);
                });

        var program = new StaticProgram(mergedFunctions);

        return (program, declsFailures);
    }

    private static StaticFunctionInterface InterfaceForFunction<T>(
        StaticExpression<T> functionBody,
        PineValueClass ignoreDeterminedByEnv)
    {
        var allImplicitParam =
            StaticExpressionExtension.ImplicitFunctionParameterList(
                functionBody,
                ignoreDeterminedByEnv);

        /*
         * TODO: Replace stupid heuristic with an adaptive approach:
         * Goal is to reduce the need for callers to compose list instances to hand over arguments.
         * Perhaps we will switch to a separate stage for determining the specialized function interfaces.
         * (We might want to look at all the call sites to determine the optimal shape of the parameter list)
         * */

        var shortenedParams =
            allImplicitParam
            .Select(p => (IReadOnlyList<int>)[.. p.Take(2)]) // Take at most 2 elements from each implicit param path
            .Distinct(IntPathEqualityComparer.Instance)
            .ToArray();

        static bool PathIsPrefixOfOtherPath(IReadOnlyList<int> path, IReadOnlyList<int> otherPath)
        {
            if (path.Count >= otherPath.Count)
                return false;

            for (var i = 0; i < path.Count; ++i)
            {
                if (path[i] != otherPath[i])
                    return false;
            }

            return true;
        }

        var inludedParams =
            shortenedParams
            .Where(p => !shortenedParams.Any(other => PathIsPrefixOfOtherPath(p, other)))
            .ToArray();

        return StaticFunctionInterface.FromPathsSorted(inludedParams);
    }


    public static Result<string, ParsedFromSingleRoot>
        ParseAsStaticMonomorphicProgram(
        ElmInteractiveEnvironment.FunctionRecord functionRecord,
        Func<PineValue, PineValueClass, bool> dontInline,
        PineVMParseCache parseCache)
    {
        var (_, appliedExpr, appliedEnv) =
            NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(functionRecord, arguments: [], parseCache);

        return
            ParseAsStaticMonomorphicProgram(
                rootExpression: appliedExpr,
                rootEnvValueClass: appliedEnv,
                dontInline: dontInline,
                parseCache: parseCache);
    }

    public record ParsedFromSingleRoot(
        StaticFunctionIdentifier RootId,
        IReadOnlyDictionary<StaticFunctionIdentifier, (Expression origExpr, StaticExpressionGen body)> AllReferenced);

    public static Result<string, ParsedFromSingleRoot>
        ParseAsStaticMonomorphicProgram(
        Expression rootExpression,
        PineValueClass rootEnvValueClass,
        Func<PineValue, PineValueClass, bool> dontInline,
        PineVMParseCache parseCache)
    {
        var observedSetInitial =
            new List<(Expression origExpr, Expression inlinedExpr, PineValueClass constraint)>();

        var queue = new Queue<(Expression expr, PineValueClass envValueClass)>();

        queue.Enqueue((rootExpression, rootEnvValueClass));

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (observedSetInitial.Any(entry => entry.origExpr == current.expr && entry.constraint.Equals(current.envValueClass)))
                continue;

            var currentExprInlined =
                InlineParseAndEvalUsingLiteralFunctionRecursive(
                    current.expr,
                    current.envValueClass,
                    dontInline,
                    parseCache);

            observedSetInitial.Add((current.expr, currentExprInlined, current.envValueClass));

            var allParseAndEvalExpressions =
                Expression.EnumerateSelfAndDescendants(currentExprInlined)
                .OfType<Expression.ParseAndEval>()
                .ToImmutableArray();

            var distinctParseAndEvalExpressions =
                allParseAndEvalExpressions
                .Distinct()
                .ToImmutableArray();

            foreach (var parseAndEvalExpr in distinctParseAndEvalExpressions)
            {
                if (ParseAndEvalCrashingAlways(parseAndEvalExpr, parseCache))
                {
                    /*
                     * Some compilers create crashing branches like these; it's an allowed pattern.
                     * For static analysis, this means we can ignore the parse and eval expression.
                     * */

                    continue;
                }

                /*
                 * Limited support for scenarios:
                 * As a constraint for the current implementation, we only forward parts that do not depend on
                 * any parse&eval expressions themselves.
                 * */


                /*
                 * For now, try an even simpler subset:
                 * Only forward items modeled in the popular environment composition style:
                 * */

                var childEnvClass = PineValueClass.MapValueClass(current.envValueClass, parseAndEvalExpr.Environment);

                if (childEnvClass is null)
                {
                    return "Could not map environment value class through parseAndEval.Environment";
                }

                var childExprValueResult =
                    TryGetChildEncodedExprValue(
                        parseAndEvalExpr,
                        current.envValueClass,
                        parseCache);

                {
                    if (childExprValueResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to resolve parseAndEval.Encoded to a value: " + err;
                    }
                }

                if (childExprValueResult.IsOkOrNull() is not { } childExprValue)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        childExprValueResult.GetType().Name);
                }

                var childParseResult = parseCache.ParseExpression(childExprValue);

                {
                    if (childParseResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to parse parseAndEval.Encoded value: " + err;
                    }
                }

                if (childParseResult.IsOkOrNull() is not { } childExpr)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        childParseResult.GetType().Name);
                }

                queue.Enqueue((childExpr, childEnvClass));
            }
        }

        var observedSet =
            observedSetInitial
            .Select(entry =>
            {

                /*
                * 2025-09-14:
                * So far, the mapped value class can contain items that are not used to decode program code.
                * Therefore we filter out all items that do not encode epressions.
                * This is not precise, since there could be superfluous expression items as well.
                * For proper filtering, we probably need to track which items are actually used in the inlined expression.
                * TODO: Find a more precise way to determine which items are relevant.
                * */

                var constraint =
                    FilterClassRemovingAllNonExpressions(entry.constraint, parseCache);

                return
                (entry.origExpr, entry.inlinedExpr, constraint);
            })
            .DistinctBy(entry => (entry.origExpr, entry.constraint))
            .ToArray();

        /*
         * Filter observed set, to remove redundant entries which are already covered by smaller environments.
         * 
         * Here, we assume that every entry in the 'observed' set is completely static, as we would have exited
         * already if we had encountered something dynamic in the exploration above.
         * */

        bool ObservedIsRedundant(
            (Expression origExpr, Expression inlinedExpr, PineValueClass constraint) observedCombo)
        {
            foreach (var other in observedSet)
            {
                if (observedCombo == other)
                    continue;

                if (other.origExpr == observedCombo.origExpr &&
                    other.constraint.SatisfiedByConstraint(observedCombo.constraint))
                {
                    return true;
                }
            }

            return false;
        }

        var filteredObservedSet =
            observedSet
            .Where(observedCombo => !ObservedIsRedundant(observedCombo))
            .ToImmutableArray();

        var genFunctions =
            new Dictionary<StaticFunctionIdentifier, (Expression origExpr, StaticExpressionGen body)>();

        foreach (var observedCombo in filteredObservedSet)
        {
            var parseExprResult =
                ParseAsStaticExpression(
                    observedCombo.inlinedExpr,
                    observedCombo.constraint,
                    parseCache);

            if (parseExprResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse expression as static expression: " + err;
            }

            if (parseExprResult.IsOkOrNull() is not { } staticExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseExprResult.GetType().Name);
            }

            var exprValue =
                ExpressionEncoding.EncodeExpressionAsValue(observedCombo.origExpr);

            var functionIdent =
                new StaticFunctionIdentifier(exprValue, observedCombo.constraint);

            if (genFunctions.ContainsKey(functionIdent))
            {
                return "Hash collision detected for function name: " + functionIdent;
            }

            genFunctions[functionIdent] = (observedCombo.origExpr, staticExpr);
        }

        var rootExprValue =
            ExpressionEncoding.EncodeExpressionAsValue(rootExpression);

        var rootId =
            genFunctions.Keys
            .Single(c =>
            c.EncodedExpr.Equals(rootExprValue) &&
            c.EnvClass.SatisfiedByConstraint(rootEnvValueClass));

        return new ParsedFromSingleRoot(rootId, genFunctions);
    }

    static PineValueClass FilterClassRemovingAllNonExpressions(
        PineValueClass valueClass,
        PineVMParseCache parseCache)
    {
        bool KeepEntryAsIs(PineValue value)
        {
            var parseResult = parseCache.ParseExpression(value);

            if (parseResult.IsOkOrNull() is { })
            {
                return true;
            }

            if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(value, parseCache).IsOkOrNull() is { } asFunctionRecord)
            {
                /*
                 * Adapt to how the assignment of names currently works:
                 * Keep as is if this entry represents a function record, as 'NamesFromCompiledEnv' currently does.
                 * */

                if (0 < asFunctionRecord.EnvFunctions.Length)
                {
                    return true;
                }
            }

            return false;
        }

        var entriesRemaining = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();

        var candidates = new Queue<KeyValuePair<IReadOnlyList<int>, PineValue>>(valueClass.ParsedItems);

        while (candidates.Count > 0)
        {
            var candidate = candidates.Dequeue();

            if (KeepEntryAsIs(candidate.Value))
            {
                entriesRemaining.Add(candidate);
                continue;
            }

            if (candidate.Value is PineValue.ListValue listValue)
            {
                for (var i = 0; i < listValue.Items.Length; ++i)
                {
                    candidates.Enqueue(
                        new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [.. candidate.Key, i],
                            listValue.Items.Span[i]));
                }
            }
        }

        return PineValueClass.Create(entriesRemaining);
    }

    static Expression InlineParseAndEvalUsingLiteralFunctionRecursive(
        Expression expression,
        PineValueClass envConstraint,
        Func<PineValue, PineValueClass, bool> dontInline,
        PineVMParseCache parseCache)
    {
        return
            InlineParseAndEvalUsingLiteralFunctionRecursive(
                expression,
                envConstraint,
                dontInline,
                parseCache,
                alreadyInlined: []);
    }

    static Expression InlineParseAndEvalUsingLiteralFunctionRecursive(
        Expression expression,
        PineValueClass envValueClass,
        Func<PineValue, PineValueClass, bool> dontInline,
        PineVMParseCache parseCache,
        ImmutableHashSet<Expression> alreadyInlined)
    {
        var reducedExpression =
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
            findReplacement: expr =>
            {
                if (expr is not Expression.ParseAndEval parseAndEval)
                {
                    return null;
                }

                if (parseAndEval.Encoded.ReferencesEnvironment)
                {
                    // Cannot inline parse&eval expressions that reference the environment.
                    return null;
                }

                var parseIndependentResult = ParseIndependentParseAndEvalExpression(parseAndEval, parseCache);

                if (parseIndependentResult.IsOkOrNullable() is not { } childExprAndValue)
                {
                    // Cannot inline parse&eval expressions that cannot be parsed independently.
                    return null;
                }

                var (childExprValue, childExpr) = childExprAndValue;

                var childEnvValueClass = PineValueClass.MapValueClass(envValueClass, parseAndEval.Environment);

                if (childEnvValueClass is null)
                {
                    return null;
                }

                if (dontInline(childExprValue, childEnvValueClass))
                {
                    return null;
                }

                if (alreadyInlined.Contains(childExpr))
                {
                    // Prevent infinite recursion on self-referential parse&eval expressions.
                    return null;
                }

                var inlinedExpr =
                    ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                        findReplacement:
                        descendant =>
                        {
                            if (descendant is Expression.Environment)
                            {
                                return parseAndEval.Environment;
                            }

                            return null;
                        },
                        childExpr).expr;

                var inlinedExprReduced =
                    ReducePineExpression.ReduceExpressionBottomUp(inlinedExpr, parseCache);

                return
                InlineParseAndEvalUsingLiteralFunctionRecursive(
                    expression: inlinedExprReduced,
                    envValueClass: childEnvValueClass,
                    dontInline: dontInline,
                    parseCache: parseCache,
                    alreadyInlined: alreadyInlined.Add(childExpr));
            },
            expression).expr;

        return reducedExpression;
    }

    static Result<string, StaticExpressionGen> ParseAsStaticExpression(
        Expression expression,
        PineValueClass envValueClass,
        PineVMParseCache parseCache)
    {
        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is
            ExprMappedToParentEnv.PathInParentEnv asPath)
        {
            if (envValueClass.TryGetValue(asPath.Path) is { } valueFromEnvClass)
            {
                return
                    StaticExpressionGen.LiteralInstance(valueFromEnvClass);
            }
        }

        if (expression is Expression.Literal literal)
        {
            return StaticExpressionGen.LiteralInstance(literal.Value);
        }

        if (expression is Expression.Environment)
        {
            return StaticExpressionGen.EnvironmentInstance;
        }

        if (expression is Expression.ParseAndEval parseAndEval)
        {
            var parseEncodedResult =
                ParseAsStaticExpression(
                    parseAndEval.Encoded,
                    envValueClass,
                    parseCache);

            {
                if (parseEncodedResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Encoded as static expression: " + err;
                }
            }

            if (parseEncodedResult.IsOkOrNull() is not { } encodedExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseEncodedResult.GetType().Name);
            }

            var parseEnvResult =
                ParseAsStaticExpression(
                    parseAndEval.Environment,
                    envValueClass,
                    parseCache);

            {
                if (parseEnvResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Environment as static expression: " + err;
                }
            }

            if (parseEnvResult.IsOkOrNull() is not { } envExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseEnvResult.GetType().Name);
            }

            if (ParseAndEvalCrashingAlways(parseAndEval, parseCache))
            {
                return new StaticExpressionGen.CrashingParseAndEval(
                    encodedExpr,
                    envExpr);
            }

            var childExprValueResult =
                TryGetChildEncodedExprValue(
                    parseAndEval,
                    envValueClass,
                    parseCache);

            {
                if (childExprValueResult.IsErrOrNull() is { } err)
                {
                    return "Failed to resolve parseAndEval.Encoded to a value: " + err;
                }
            }

            if (childExprValueResult.IsOkOrNull() is not { } childExprValue)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    childExprValueResult.GetType().Name);
            }

            var childEnvClass = PineValueClass.MapValueClass(envValueClass, parseAndEval.Environment);

            if (childEnvClass is null)
            {
                return "Could not map environment value class through parseAndEval.Environment";
            }

            var parseExprResult = parseCache.ParseExpression(childExprValue);

            {
                if (parseExprResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Encoded value: " + err;
                }
            }

            if (parseExprResult.IsOkOrNull() is not { })
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseExprResult.GetType().Name);
            }

            var childFunctionName =
                new StaticFunctionIdentifier(
                    childExprValue,
                    childEnvClass);

            return
                StaticExpressionGen.FunctionApplicationInstance(
                    functionName: childFunctionName,
                    arguments: envExpr);
        }

        if (expression is Expression.List listExpr)
        {
            var parsedItems = new StaticExpressionGen[listExpr.Items.Count];

            for (var itemIndex = 0; itemIndex < parsedItems.Length; ++itemIndex)
            {
                var item = listExpr.Items[itemIndex];

                var parseItemResult =
                    ParseAsStaticExpression(
                        item,
                        envValueClass,
                        parseCache);

                if (parseItemResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse list item: " + err;
                }

                if (parseItemResult.IsOkOrNull() is not { } parsedItem)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        parseItemResult.GetType().Name);
                }

                parsedItems[itemIndex] = parsedItem;
            }

            return StaticExpressionGen.ListInstance(parsedItems);
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            var parseInputResult =
                ParseAsStaticExpression(
                    kernelApp.Input,
                    envValueClass,
                    parseCache);

            if (parseInputResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse input of kernel application: " + err;
            }

            if (parseInputResult.IsOkOrNull() is not { } inputExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseInputResult.GetType().Name);
            }

            return
                StaticExpressionGen.KernelApplicationInstance(
                    function: kernelApp.Function,
                    input: inputExpr);
        }

        if (expression is Expression.Conditional conditional)
        {
            var parseConditionResult =
                ParseAsStaticExpression(
                    conditional.Condition,
                    envValueClass,
                    parseCache);

            {
                if (parseConditionResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse condition of conditional: " + err;
                }
            }

            if (parseConditionResult.IsOkOrNull() is not { } conditionExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseConditionResult.GetType().Name);
            }

            var falseBranchResult =
                ParseAsStaticExpression(
                    conditional.FalseBranch,
                    envValueClass,
                    parseCache);

            {
                if (falseBranchResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse false branch of conditional: " + err;
                }
            }

            if (falseBranchResult.IsOkOrNull() is not { } falseBranchExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    falseBranchResult.GetType().Name);
            }

            var trueBranchResult =
                ParseAsStaticExpression(
                    conditional.TrueBranch,
                    envValueClass,
                    parseCache);

            {
                if (trueBranchResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse true branch of conditional: " + err;
                }
            }

            if (trueBranchResult.IsOkOrNull() is not { } trueBranchExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    trueBranchResult.GetType().Name);
            }

            return
                StaticExpressionGen.ConditionalInstance(
                    condition: conditionExpr,
                    trueBranch: trueBranchExpr,
                    falseBranch: falseBranchExpr);
        }

        throw new NotImplementedException(
            "Unexpected expression type: " +
            expression.GetType().Name);
    }

    static Result<string, PineValue> TryGetChildEncodedExprValue(
        Expression.ParseAndEval parseAndEval,
        PineValueClass envValueClass,
        PineVMParseCache parseCache)
    {
        if (parseAndEval.Encoded.ReferencesEnvironment &&
            Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is
            ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
        {
            // Try get function value from the environment class:

            if (envValueClass.TryGetValue(pathInParentEnv.Path) is not { } valueFromPath)
            {
                // Cannot interpret the encoded part as a path from the environment.
                // This means we cannot statically determine what part of the environment
                // is parsed by this parse&eval expression.
                // Therefore, we have to give up on static analysis of this program.

                return
                    "Could not find value for parseAndEval.Encoded path [" +
                    string.Join(',', pathInParentEnv.Path) + "] in the given environment";
            }

            return valueFromPath;
        }
        else
        {
            if (ReducePineExpression.TryEvaluateExpressionIndependent(
                parseAndEval.Encoded,
                parseCache).IsOkOrNull() is { } fromLiteral)
            {
                return fromLiteral;
            }

            return "Could not evaluate parseAndEval.Encoded independently";
        }
    }

    private static bool ParseAndEvalCrashingAlways(
        Expression.ParseAndEval parseAndEval,
        PineVMParseCache parseCache)
    {
        if (parseAndEval.Encoded.ReferencesEnvironment)
        {
            return false;
        }

        var parseIndependentResult =
            ParseIndependentParseAndEvalExpression(parseAndEval, parseCache);

        if (parseIndependentResult.IsErrOrNull() is { })
        {
            return true;
        }

        return false;
    }

    private static Result<object, (PineValue, Expression)>
        ParseIndependentParseAndEvalExpression(
        Expression.ParseAndEval parseAndEval,
        PineVMParseCache parseCache)
    {
        if (parseAndEval.Encoded.ReferencesEnvironment)
        {
            throw new InvalidOperationException(
                "Cannot parse independently if parseAndEval.Encoded references the environment");
        }

        var evalIndependentResult =
            ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEval.Encoded, parseCache);

        {
            if (evalIndependentResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed to evaluate parseAndEval.Encoded independently: " + err);
            }
        }

        if (evalIndependentResult.IsOkOrNull() is not { } evalIndependentValue)
        {
            throw new Exception(
                "Unexpected return type: " +
                evalIndependentResult.GetType().Name);
        }

        var parseExprResult = parseCache.ParseExpression(evalIndependentValue);

        if (parseExprResult.IsOkOrNull() is not { } parsedExpr)
        {
            /*
             * If parsing of a literal fails, it means the program will crash at runtime if the containing branch is taken.
             * */

            return Result<object, (PineValue, Expression)>.err("");
        }

        return Result<object, (PineValue, Expression)>.ok((evalIndependentValue, parsedExpr));
    }

    private static string AnonymousFunctionName(
        PineValue exprValue,
        PineValueClass pineValueClass,
        ConcurrentPineValueHashCache hashCache)
    {
        var exprHash = hashCache.GetHash(exprValue);

        var exprHashBase16 = Convert.ToHexStringLower(exprHash.Span);

        // Name so that anonymous one appear at the end when sorting alphabetically:

        return $"zzz_anon_{exprHashBase16[..8]}_{pineValueClass.HashBase16[..8]}";
    }

    public static IReadOnlyDictionary<FunctionIdentifier, StaticExpression<FunctionIdentifier>>
        SimplifyFunctions<FunctionIdentifier>(
        IReadOnlyDictionary<FunctionIdentifier, StaticExpression<FunctionIdentifier>> namedFunctionsBeforeInline)
        where FunctionIdentifier : notnull
    {
        var distinctCallSitesByFunc =
            namedFunctionsBeforeInline
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => CollectCallSites(kvp.Value).Distinct().ToImmutableArray());

        var directDependenciesByFunc =
            distinctCallSitesByFunc
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value.Select(app => app.FunctionName).Distinct().ToFrozenSet());

        IEnumerable<FunctionIdentifier> EnumerateDependenciesTransitive(FunctionIdentifier functionName)
        {
            var visited = new HashSet<FunctionIdentifier>();

            var stack = new Stack<FunctionIdentifier>([functionName]);

            while (stack.Count > 0)
            {
                var current = stack.Pop();

                if (!visited.Add(current))
                {
                    continue;
                }

                if (directDependenciesByFunc.TryGetValue(current, out var directDeps))
                {
                    foreach (var dep in directDeps)
                    {
                        yield return dep;

                        stack.Push(dep);
                    }
                }
            }
        }

        var transitiveDependenciesByFunc =
            directDependenciesByFunc
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => EnumerateDependenciesTransitive(kvp.Key).ToFrozenSet());

        bool FunctionIsRecursive(FunctionIdentifier functionName)
        {
            return
                transitiveDependenciesByFunc.TryGetValue(functionName, out var deps) &&
                deps.Contains(functionName);
        }

        var namedFunctions =
            namedFunctionsBeforeInline
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp =>
                {
                    var bodyInlined =
                    StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                        findReplacement: node =>
                        {
                            if (node is StaticExpression<FunctionIdentifier>.FunctionApplication app)
                            {
                                if (FunctionIsRecursive(app.FunctionName))
                                {
                                    // Do not inline recursive functions
                                    return null;
                                }

                                var callee = namedFunctionsBeforeInline[app.FunctionName];

                                if (callee is StaticExpression<FunctionIdentifier>.FunctionApplication calleeApp &&
                                calleeApp.Arguments == StaticExpression<FunctionIdentifier>.EnvironmentInstance)
                                {
                                    return
                                    StaticExpression<FunctionIdentifier>.FunctionApplicationInstance(
                                        functionName: calleeApp.FunctionName,
                                        arguments: app.Arguments);
                                }
                            }
                            return null;
                        },
                        expression: kvp.Value).expr;

                    return bodyInlined;
                });

        distinctCallSitesByFunc =
            namedFunctionsBeforeInline
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => CollectCallSites(kvp.Value).Distinct().ToImmutableArray());

        directDependenciesByFunc =
            distinctCallSitesByFunc
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value.Select(app => app.FunctionName).Distinct().ToFrozenSet());

        var directEnvPathsByFunc =
            namedFunctions
            .ToFrozenDictionary(
                kvp =>
                kvp.Key,
                kvp =>
                StaticExpressionExtension.CollectEnvPathsOutsideFunctionApplications(kvp.Value)
                .ToFrozenSet(IntPathEqualityComparer.Instance));

        IReadOnlySet<IReadOnlyList<int>> CollectAllUsedPathsTransitive(
            FunctionIdentifier functionName,
            ImmutableStack<FunctionIdentifier> stack)
        {
            var collected =
                directEnvPathsByFunc[functionName]
                .ToHashSet(IntPathEqualityComparer.Instance);

            if (!namedFunctions.TryGetValue(functionName, out var funcBody))
            {
                throw new Exception("Function not found in program: " + functionName);
            }

            if (stack.Contains(functionName))
            {
                return collected;
            }

            foreach (var app in distinctCallSitesByFunc[functionName])
            {
                var calleeName = app.FunctionName;

                var calleeUsedPaths =
                    CollectAllUsedPathsTransitive(calleeName, stack.Push(functionName));

                // Map paths from callee back to caller via argument expression

                foreach (var calleePath in calleeUsedPaths)
                {
                    foreach (var callerPath in MapCalleePathToCallerPaths(app.Arguments, calleePath))
                    {
                        collected.Add(callerPath);
                    }
                }
            }

            /*
             * TODO: Replace stupid heuristic with an adaptive approach:
             * Goal is to reduce the need for callers to compose list instances to hand over arguments.
             * Perhaps we will switch to a separate stage for determining the specialized function interfaces.
             * (We might want to look at all the call sites to determine the optimal shape of the parameter list)
             * */

            var shortenedParams =
                collected
                .Select(p => (IReadOnlyList<int>)[.. p.Take(2)]) // Take at most 2 elements from each implicit param path
                .Distinct(IntPathEqualityComparer.Instance)
                .ToArray();

            static bool PathIsPrefixOfOtherPath(IReadOnlyList<int> path, IReadOnlyList<int> otherPath)
            {
                if (path.Count >= otherPath.Count)
                    return false;

                for (var i = 0; i < path.Count; ++i)
                {
                    if (path[i] != otherPath[i])
                        return false;
                }

                return true;
            }

            var inludedParams =
                shortenedParams
                .Where(p => !shortenedParams.Any(other => PathIsPrefixOfOtherPath(p, other)))
                .ToArray();

            return inludedParams.ToFrozenSet(IntPathEqualityComparer.Instance);
        }

        var allUsedPathsByFunc =
            namedFunctions
            .ToFrozenDictionary(kvp => kvp.Key, kvp => CollectAllUsedPathsTransitive(kvp.Key, stack: []));

        bool IsPathOrPrefixUsed(
            FunctionIdentifier functionName,
            IReadOnlyList<int> path,
            ImmutableStack<(FunctionIdentifier functionName, IReadOnlyList<int> path)> stack)
        {
            if (!allUsedPathsByFunc.TryGetValue(functionName, out var allUsedPaths))
            {
                throw new Exception("Function not found in program: " + functionName);
            }

            foreach (var usedPath in allUsedPaths)
            {
                if (PathIsPrefixOfOtherPath(usedPath, path))
                {
                    return true;
                }
            }

            return false;
        }

        static bool PathIsPrefixOfOtherPath(
            IReadOnlyList<int> prefix,
            IReadOnlyList<int> otherPath)
        {
            if (prefix.Count > otherPath.Count)
                return false;

            for (var i = 0; i < prefix.Count; ++i)
            {
                if (prefix[i] != otherPath[i])
                    return false;
            }

            return true;
        }

        StaticExpression<FunctionIdentifier> RemoveUnusedFromFunctionBody(
            FunctionIdentifier currentFunctionName,
            StaticExpression<FunctionIdentifier> functionBody)
        {
            return
                ReplaceEnvPathsInBody(functionBody, path =>
                {
                    if (IsPathOrPrefixUsed(currentFunctionName, path, stack: []))
                    {
                        return null;
                    }

                    return StaticExpression<FunctionIdentifier>.LiteralInstance(PineValue.EmptyList);
                });
        }

        return
            namedFunctions
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => RemoveUnusedFromFunctionBody(kvp.Key, kvp.Value));
    }

    static private IReadOnlyDictionary<FunctionIdentifier, StaticExpression<FunctionIdentifier>>
        FilterForRoots<FunctionIdentifier>(
        IReadOnlyDictionary<FunctionIdentifier, StaticExpression<FunctionIdentifier>> functions,
        IReadOnlySet<FunctionIdentifier> roots)
        where FunctionIdentifier : notnull
    {
        var distinctCallSitesByFunc =
            functions
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => CollectCallSites(kvp.Value).Distinct().ToImmutableArray());

        var directDependenciesByFunc =
            distinctCallSitesByFunc
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value.Select(app => app.FunctionName).Distinct().ToFrozenSet());

        IEnumerable<FunctionIdentifier> EnumerateDependenciesTransitive(FunctionIdentifier functionName)
        {
            var visited = new HashSet<FunctionIdentifier>();

            var stack = new Stack<FunctionIdentifier>([functionName]);

            while (stack.Count > 0)
            {
                var current = stack.Pop();

                if (!visited.Add(current))
                {
                    continue;
                }

                if (directDependenciesByFunc.TryGetValue(current, out var directDeps))
                {
                    foreach (var dep in directDeps)
                    {
                        yield return dep;

                        stack.Push(dep);
                    }
                }
            }
        }

        var transitiveDependenciesByFunc =
            directDependenciesByFunc
            .ToFrozenDictionary(
                kvp => kvp.Key,
                kvp => EnumerateDependenciesTransitive(kvp.Key).ToFrozenSet());

        var rootsTransitiveDependencies =
            roots
            .SelectMany(r => transitiveDependenciesByFunc.TryGetValue(r, out var deps) ? deps : [])
            .Concat(roots)
            .ToFrozenSet();

        return
            functions
            .Where(kvp => rootsTransitiveDependencies.Contains(kvp.Key))
            .ToFrozenDictionary();
    }

    private static IEnumerable<StaticExpression<TFunctionId>.FunctionApplication> CollectCallSites<TFunctionId>(
        StaticExpression<TFunctionId> expr)
    {
        foreach (var node in StaticExpressionExtension.EnumerateAllDescendants(expr, skipDescendants: null))
        {
            if (node is StaticExpression<TFunctionId>.FunctionApplication app)
            {
                yield return app;
            }
        }
    }

    private static IEnumerable<IReadOnlyList<int>> MapCalleePathToCallerPaths<TFuncId>(
        StaticExpression<TFuncId> applicationArgument,
        IReadOnlyList<int> calleePath)
    {
        var (subexpr, pathSuffix) = applicationArgument.GetSubexpressionAtPath(calleePath);

        var queue = new Queue<StaticExpression<TFuncId>>([subexpr]);

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (StaticExpressionExtension.TryParseAsPathToExpression(
                current,
                StaticExpression<TFuncId>.EnvironmentInstance) is { } path)
            {
                yield return [.. path, .. pathSuffix];
                continue;
            }

            if (current is StaticExpression<TFuncId>.FunctionApplication)
            {
                /*
                 * TODO: Map via function application (New environment composition)
                 * Perhaps an incremental substitution helps break these down in an easy way.
                 * */

                continue;
            }

            foreach (var child in StaticExpressionExtension.EnumerateDirectChildren(current))
            {
                queue.Enqueue(child);
            }
        }
    }

    private static StaticExpression<TFuncId> ReplaceEnvPathsInBody<TFuncId>(
        StaticExpression<TFuncId> body,
        Func<IReadOnlyList<int>, StaticExpression<TFuncId>?> replacePath)
    {
        var (mapped, _) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                findReplacement: node =>
                {
                    var path =
                    StaticExpressionExtension.TryParseAsPathToExpression(
                        node,
                        StaticExpression<TFuncId>.EnvironmentInstance);

                    if (path is null)
                        return null;

                    if (path.Count is 0)
                        return null;

                    if (replacePath(path) is { } replacement)
                    {
                        return replacement;
                    }

                    return node;
                },
                expression: body);

        return mapped;
    }

    /// <summary>
    /// Returns the value at the specified <paramref name="path"/> within the given <paramref name="environment"/> value.
    /// The path is interpreted as a sequence of indices, where each index selects an element from a list or skips bytes in a blob.
    /// Returns <c>null</c> if the path cannot be resolved in the given environment.
    /// </summary>
    /// <param name="environment">The root <see cref="PineValue"/> to traverse.</param>
    /// <param name="path">A span of indices representing the path to traverse within the environment.</param>
    /// <returns>
    /// The <see cref="PineValue"/> at the specified path, or <c>null</c> if the path is invalid.
    /// </returns>
    public static PineValue? ValueFromPathInValue(
        PineValue environment,
        ReadOnlySpan<int> path)
    {
        if (path.Length is 0)
            return environment;

        if (environment is PineValue.BlobValue blobValue)
        {
            if (1 < path.Length)
                return null;

            return KernelFunctionSpecialized.skip(path[0], blobValue);
        }

        if (environment is not PineValue.ListValue listValue)
            return null;

        if (path[0] < 0)
            return null;

        if (path[0] >= listValue.Items.Length)
            return null;

        return ValueFromPathInValue(listValue.Items.Span[path[0]], path[1..]);
    }

    /// <summary>
    /// Returns a mapping for the given expression if it corresponds to a path from the environment root,
    /// otherwise returns <c>null</c>.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <returns>
    /// <see cref="ExprMappedToParentEnv.PathInParentEnv"/> with the path indices when the expression is composed of
    /// environment access via skip/head, a <see cref="ExprMappedToParentEnv.LiteralInParentEnv"/> for literals,
    /// or <c>null</c> when it cannot be mapped.
    /// </returns>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPathFromEnv(Expression expression)
    {
        return
            TryParseExpressionAsIndexPath(
                pathExpression: expression,
                rootExpression: Expression.EnvironmentInstance);
    }

    /// <summary>
    /// Tries to interpret <paramref name="pathExpression"/> as a path relative to <paramref name="rootExpression"/>,
    /// following the Pine built-ins <c>skip</c> and <c>head</c>.
    /// </summary>
    /// <param name="pathExpression">The expression to interpret as a path.</param>
    /// <param name="rootExpression">The root expression considered as the origin of the path.</param>
    /// <returns>
    /// <see cref="ExprMappedToParentEnv.PathInParentEnv"/> for a recognized path, <see cref="ExprMappedToParentEnv.LiteralInParentEnv"/> for literals,
    /// or <c>null</c> if the expression does not match a supported pattern.
    /// </returns>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPath(
        Expression pathExpression,
        Expression rootExpression)
    {
        if (pathExpression == rootExpression)
            return new ExprMappedToParentEnv.PathInParentEnv([]);

        if (pathExpression is Expression.Literal literal)
            return new ExprMappedToParentEnv.LiteralInParentEnv(literal.Value);

        if (pathExpression is Expression.StringTag stringTagExpr)
            return TryParseExpressionAsIndexPath(stringTagExpr.Tagged, rootExpression);

        if (pathExpression is not Expression.KernelApplication kernelApplication)
            return null;

        if (kernelApplication.Function is not nameof(KernelFunction.head))
            return null;

        if (kernelApplication.Input is Expression.KernelApplication inputKernelApplication &&
            inputKernelApplication.Function is nameof(KernelFunction.skip))
        {
            if (inputKernelApplication.Input is not Expression.List skipInputList)
                return null;

            if (skipInputList.Items.Count is not 2)
                return null;

            if (skipInputList.Items[0] is not Expression.Literal skipCountLiteral)
                return null;

            if (TryParseExpressionAsIndexPath(skipInputList.Items[1], rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return
                KernelFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is { } skipValue
                ?
                new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, (int)skipValue])
                :
                null;
        }

        {
            if (TryParseExpressionAsIndexPath(kernelApplication.Input, rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, 0]);
        }
    }

    /// <summary>
    /// Returns a sub-expression of <paramref name="expression"/> corresponding to <paramref name="path"/>, if resolvable.
    /// This overload accepts a read-only list for ergonomics when the caller already has a list instance.
    /// </summary>
    /// <param name="expression">Root expression to traverse. List nodes are navigated by index.</param>
    /// <param name="path">Sequence of indices selecting nested list items. An empty path returns <paramref name="expression"/>.</param>
    /// <returns>
    /// The concrete <see cref="Expression"/> reached by the path, or <c>null</c> if any index is out of range.
    /// </returns>
    public static Expression? ExpressionForPathInExpression(
        Expression expression,
        IReadOnlyList<int> path) =>
        ExpressionForPathInExpression(expression, [.. path]);

    /// <summary>
    /// Returns a sub-expression of <paramref name="expression"/> corresponding to <paramref name="path"/>, if resolvable.
    /// The traversal first descends through contiguous <see cref="Expression.List"/> nodes while possible.
    /// Any remaining indices (when a non-list node is reached early) are reified into a chain of <c>skip/head</c> kernel applications
    /// via <see cref="BuildExpressionForPathInExpression(Expression, ReadOnlySpan{int})"/> so that the resulting expression still denotes
    /// the requested position relative to the last concrete node visited.
    /// </summary>
    /// <param name="expression">Root expression to traverse.</param>
    /// <param name="path">Sequence of indices selecting nested list items. Empty path returns <paramref name="expression"/>.</param>
    /// <returns>
    /// The concrete or synthesized <see cref="Expression"/> matching the path, or <c>null</c> if an index is out of range while descending list nodes.
    /// </returns>
    /// <example>
    /// If <paramref name="expression"/> is a list <c>[A,B,C]</c> and <paramref name="path"/> is <c>[1]</c>, returns <c>B</c>.
    /// If <paramref name="expression"/> is <c>A</c> (non-list) and <paramref name="path"/> is <c>[2,0]</c>, returns an expression equivalent to <c>head (skip 2 A)</c> then another <c>head</c> for index 0.
    /// </example>
    public static Expression? ExpressionForPathInExpression(
        Expression expression,
        ReadOnlySpan<int> path)
    {
        var current = expression;

        var itemIndexAfterReduction = 0;

        for (; itemIndexAfterReduction < path.Length; itemIndexAfterReduction++)
        {
            if (current is Expression.List listExpr)
            {
                if (path[itemIndexAfterReduction] < 0 || path[itemIndexAfterReduction] >= listExpr.Items.Count)
                    return null;

                current = listExpr.Items[path[itemIndexAfterReduction]];
            }
            else
            {
                break;
            }
        }

        var pathRemaining = path[itemIndexAfterReduction..];

        return
            BuildExpressionForPathInExpression(current, pathRemaining);
    }

    /// <summary>
    /// Builds (or completes) an expression that selects a nested element identified by <paramref name="path"/> starting from <paramref name="expression"/>.
    /// For each index in <paramref name="path"/>, constructs the equivalent Pine kernel expression using <c>skip</c> (when index &gt; 0) followed by <c>head</c>.
    /// </summary>
    /// <param name="expression">The starting expression. This becomes the base for the generated access chain.</param>
    /// <param name="path">Sequence of indices to navigate. Empty path returns <paramref name="expression"/> unchanged.</param>
    /// <returns>An <see cref="Expression"/> that, when evaluated, yields the value at the requested path.</returns>
    /// <remarks>
    /// This method does not validate that <paramref name="expression"/> structurally supports the path at runtime; it only encodes the navigation.
    /// It is typically used after partially consuming a path through concrete list nesting where remaining indices must be represented symbolically.
    /// </remarks>
    public static Expression BuildExpressionForPathInExpression(
        Expression expression,
        ReadOnlySpan<int> path)
    {
        var current = expression;

        if (path.Length is 0)
        {
            return current;
        }

        var nextOffset = path[0];

        var skipExpr =
            nextOffset is 0
            ? current
            : new Expression.KernelApplication(
                function: nameof(KernelFunction.skip),
                input:
                new Expression.List(
                    items: [new Expression.Literal(IntegerEncoding.EncodeSignedInteger(nextOffset)), current]));

        var headExpr =
            new Expression.KernelApplication(
                function: nameof(KernelFunction.head),
                input: skipExpr);

        return BuildExpressionForPathInExpression(headExpr, path[1..]);
    }


    public static Result<string, PineValueClass> MinimalValueClassForStaticProgram(
        Expression expression,
        PineValueClass availableEnvironment)
    {
        var allParseAndEvalExpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .OfType<Expression.ParseAndEval>()
            .ToImmutableArray();

        var parsedItems = new Dictionary<IReadOnlyList<int>, PineValue>();

        foreach (var parseAndEval in allParseAndEvalExpressions)
        {
            if (parseAndEval.Environment.ReferencesEnvironment)
            {
                return "Not implemented: parseAndEval.Environment.ReferencesEnvironment";
            }

            if (!parseAndEval.Encoded.ReferencesEnvironment)
            {
                continue;
            }

            if (TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is not ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
            {
                return "Could not interpret parseAndEval.Encoded as path from environment";
            }

            if (availableEnvironment.TryGetValue(pathInParentEnv.Path) is not { } valueAtPath)
            {
                return "Environment does not contain value at path required by parseAndEval";
            }

            parsedItems[pathInParentEnv.Path] = valueAtPath;
        }

        return
            PineValueClass.Create([.. parsedItems]);
    }
}
