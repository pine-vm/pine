using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides naming error detection (shadowing clashes) that works directly on declarations
/// without performing full canonicalization (no AST rewriting, no import resolution).
/// <para>
/// This is used post-canonicalization in the optimization pipeline where declarations are
/// already fully qualified. The detection logic is shared with
/// <see cref="Canonicalization.CollectPatternVariablesWithShadowCheck"/> to avoid duplication.
/// </para>
/// </summary>
public static class NamingErrorDetection
{
    /// <summary>
    /// Detects naming errors (shadowings, naming clashes) in a flat dictionary of declarations.
    /// Groups declarations by module namespace and checks each module independently.
    /// </summary>
    /// <returns>
    /// Aggregated errors and shadowings across all modules in the flat dictionary.
    /// </returns>
    public static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInFlatDict(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        // Group declarations by module namespace
        var moduleGroups =
            declarations
            .GroupBy(
                kvp => kvp.Key.Namespaces,
                kvp => (DeclName: kvp.Key.DeclName, Declaration: kvp.Value),
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var moduleGroup in moduleGroups)
        {
            // Build the module-level name set from the flat dict keys for this module
            var moduleLevelNames = BuildModuleLevelNamesFromDeclarations(moduleGroup);

            var (moduleErrors, moduleShadowings) =
                DetectNamingErrorsInModule(
                    moduleGroup.Select(g => g.Declaration).ToList(),
                    moduleLevelNames);

            allErrors.AddRange(moduleErrors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, moduleShadowings);
        }

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Builds the set of module-level names from a group of declarations in a single module.
    /// This includes function names, type names, alias names, port names, infix operator names,
    /// and type constructor names.
    /// </summary>
    private static ImmutableHashSet<string> BuildModuleLevelNamesFromDeclarations(
        IEnumerable<(string DeclName, SyntaxTypes.Declaration Declaration)> declarations)
    {
        var builder = ImmutableHashSet.CreateBuilder<string>();

        foreach (var (declName, declaration) in declarations)
        {
            builder.Add(declName);

            // Also add type constructors for choice types
            if (declaration is SyntaxTypes.Declaration.CustomTypeDeclaration choiceTypeDecl)
            {
                foreach (var ctor in choiceTypeDecl.TypeDeclaration.Constructors)
                {
                    builder.Add(ctor.Value.Name.Value);
                }
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Detects naming errors in a list of declarations belonging to one module.
    /// </summary>
    /// <param name="declarations">The declarations in the module.</param>
    /// <param name="moduleLevelNames">The set of all top-level names in this module.</param>
    /// <returns>Aggregated errors and shadowings from all declarations.</returns>
    public static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInModule(
        IReadOnlyList<SyntaxTypes.Declaration> declarations,
        ImmutableHashSet<string> moduleLevelNames)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var declaration in declarations)
        {
            var (errors, shadowings) =
                DetectNamingErrorsInDeclaration(
                    declaration,
                    moduleLevelNames);

            allErrors.AddRange(errors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, shadowings);
        }

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Detects naming errors within a single declaration.
    /// Only function declarations contain expressions that could have naming clashes.
    /// Type declarations, alias declarations, port declarations, and infix declarations
    /// do not introduce local scopes that can clash.
    /// </summary>
    public static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInDeclaration(
        SyntaxTypes.Declaration declaration,
        ImmutableHashSet<string> moduleLevelNames)
    {
        switch (declaration)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var funcName = funcDecl.Function.Declaration.Value.Name.Value;
                    var declarationPath = ImmutableList.Create(funcName);

                    return
                        DetectNamingErrorsInFunctionImpl(
                            funcDecl.Function.Declaration.Value,
                            moduleLevelNames,
                            declarationPath);
                }

            case SyntaxTypes.Declaration.CustomTypeDeclaration:
            case SyntaxTypes.Declaration.AliasDeclaration:
            case SyntaxTypes.Declaration.PortDeclaration:
            case SyntaxTypes.Declaration.InfixDeclaration:

                // These declaration types don't contain expressions with local scopes
                return ([], []);

            default:
                throw new System.NotImplementedException(
                    "Unexpected declaration type: " + declaration.GetType().Name);
        }
    }

    /// <summary>
    /// Detects naming errors in a function implementation, including parameter names
    /// that shadow module-level declarations and naming clashes within the function body.
    /// In Elm, parameter names are not allowed to shadow any declaration.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInFunctionImpl(
        SyntaxTypes.FunctionImplementation impl,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableList<string> declarationPath)
    {
        // Collect parameter variables while checking for shadowing against both
        // module-level declarations and other parameters.
        // In Elm, parameter names are not allowed to shadow any declaration.
        var parameterVariables = ImmutableHashSet<string>.Empty;

        var errors = new List<CanonicalizationError>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var arg in impl.Arguments)
        {
            var existingScope = moduleLevelNames.Union(parameterVariables);

            var (collectedVars, argErrors, argShadowings) =
                Canonicalization.CollectPatternVariablesWithShadowCheck(
                    arg.Value,
                    arg.Range,
                    existingScope,
                    parameterVariables,
                    declarationPath);

            parameterVariables = parameterVariables.Union(collectedVars);
            errors.AddRange(argErrors);
            shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, argShadowings);
        }

        // Walk the expression body for let-binding and case-pattern shadowings
        var localDeclarations = parameterVariables;

        var (exprErrors, exprShadowings) =
            DetectNamingErrorsInExpression(
                impl.Expression.Value,
                moduleLevelNames,
                localDeclarations,
                declarationPath);

        errors.AddRange(exprErrors);
        shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, exprShadowings);

        return (errors, shadowings);
    }

    /// <summary>
    /// Walks an expression tree detecting naming errors (let-binding shadows, case-pattern clashes)
    /// without rewriting the AST.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInExpression(
        SyntaxTypes.Expression expr,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
                return ([], []);

            case SyntaxTypes.Expression.Negation negation:
                return
                    DetectNamingErrorsInExpression(
                        negation.Expression.Value,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.ListExpr list:
                return
                    DetectNamingErrorsInExpressionNodes(
                        list.Elements,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    AggregateExpressionErrors(
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath,
                        ifBlock.Condition,
                        ifBlock.ThenBlock,
                        ifBlock.ElseBlock);

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return
                    DetectNamingErrorsInExpression(
                        parenExpr.Expression.Value,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.Application application:
                return
                    DetectNamingErrorsInExpressionNodes(
                        application.Arguments,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    AggregateExpressionErrors(
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath,
                        opApp.Left,
                        opApp.Right);

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    DetectNamingErrorsInExpressionNodes(
                        tupled.Elements,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    DetectNamingErrorsInLambda(
                        lambda.Lambda,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return
                    DetectNamingErrorsInCaseBlock(
                        caseExpr.CaseBlock,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.LetExpression letExpr:
                return
                    DetectNamingErrorsInLetBlock(
                        letExpr.Value,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.RecordExpr record:
                return
                    DetectNamingErrorsInRecordFields(
                        record.Fields,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return
                    DetectNamingErrorsInExpression(
                        recordAccess.Record.Value,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    DetectNamingErrorsInRecordFields(
                        recordUpdate.Fields,
                        moduleLevelNames,
                        localDeclarations,
                        declarationPath);

            default:
                throw new System.NotImplementedException(
                    "Unexpected expression type: " + expr.GetType().Name);
        }
    }

    /// <summary>
    /// Detects naming errors in a lambda expression body.
    /// Lambda parameters must not shadow module-level names or outer local declarations.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInLambda(
        SyntaxTypes.LambdaStruct lambda,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        var errors = new List<CanonicalizationError>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        // Check lambda parameters for shadowing against module-level and outer local declarations
        var existingScope = moduleLevelNames.Union(localDeclarations);
        var lambdaParamVars = ImmutableHashSet<string>.Empty;

        foreach (var arg in lambda.Arguments)
        {
            var (collectedVars, argErrors, argShadowings) =
                Canonicalization.CollectPatternVariablesWithShadowCheck(
                    arg.Value,
                    arg.Range,
                    existingScope.Union(lambdaParamVars),
                    lambdaParamVars,
                    declarationPath);

            lambdaParamVars = lambdaParamVars.Union(collectedVars);
            errors.AddRange(argErrors);
            shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, argShadowings);
        }

        var extendedLocalDeclarations = localDeclarations.Union(lambdaParamVars);

        var (exprErrors, exprShadowings) =
            DetectNamingErrorsInExpression(
                lambda.Expression.Value,
                moduleLevelNames,
                extendedLocalDeclarations,
                declarationPath);

        errors.AddRange(exprErrors);
        shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, exprShadowings);

        return (errors, shadowings);
    }

    /// <summary>
    /// Detects naming errors in a case block, including case-pattern variable clashes.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        // Check the scrutinee expression
        var (exprErrors, exprShadowings) =
            DetectNamingErrorsInExpression(
                caseBlock.Expression.Value,
                moduleLevelNames,
                localDeclarations,
                declarationPath);

        allErrors.AddRange(exprErrors);
        allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, exprShadowings);

        // Check each case branch
        foreach (var caseItem in caseBlock.Cases)
        {
            var (caseErrors, caseShadowings) =
                DetectNamingErrorsInCase(
                    caseItem,
                    moduleLevelNames,
                    localDeclarations,
                    declarationPath);

            allErrors.AddRange(caseErrors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, caseShadowings);
        }

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Detects naming errors in a single case branch.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInCase(
        SyntaxTypes.Case caseItem,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        // Collect pattern variables while checking for shadowing against both
        // module-level declarations and local declarations
        var existingScope = moduleLevelNames.Union(localDeclarations);

        var (collectedVars, shadowErrors, patternShadowings) =
            Canonicalization.CollectPatternVariablesWithShadowCheck(
                caseItem.Pattern.Value,
                caseItem.Pattern.Range,
                existingScope,
                [],
                declarationPath);

        // Check the case expression body with the pattern variables in scope
        var extendedLocalDeclarations = localDeclarations.Union(collectedVars);

        var (exprErrors, exprShadowings) =
            DetectNamingErrorsInExpression(
                caseItem.Expression.Value,
                moduleLevelNames,
                extendedLocalDeclarations,
                declarationPath);

        var allErrors = shadowErrors.Concat(exprErrors).ToList();

        var allShadowings =
            CanonicalizationResult<object>.MergeShadowings(patternShadowings, exprShadowings);

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Detects naming errors in a let block, including let-binding shadows.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        // Collect let bindings while checking for shadowing against module-level and outer local declarations
        var extendedLocalDeclarations = localDeclarations;

        // The existing scope includes module-level declarations and outer local declarations
        var existingScope = moduleLevelNames.Union(localDeclarations);

        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                var funcName = letFunc.Function.Declaration.Value.Name.Value;
                var funcNameRange = letFunc.Function.Declaration.Value.Name.Range;

                // Check if let function name shadows existing declarations
                if (existingScope.Contains(funcName))
                {
                    allErrors.Add(
                        new CanonicalizationError.NamingClash(
                            funcNameRange,
                            funcName));

                    if (!allShadowings.ContainsKey(funcName))
                    {
                        allShadowings =
                            allShadowings.Add(
                                funcName,
                                new ShadowingLocation(funcNameRange, declarationPath));
                    }
                }

                extendedLocalDeclarations = extendedLocalDeclarations.Add(funcName);
                existingScope = existingScope.Add(funcName);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                var (collectedVars, errors, destrShadowings) =
                    Canonicalization.CollectPatternVariablesWithShadowCheck(
                        letDestr.Pattern.Value,
                        letDestr.Pattern.Range,
                        existingScope,
                        [],
                        declarationPath);

                allErrors.AddRange(errors);
                allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, destrShadowings);
                extendedLocalDeclarations = extendedLocalDeclarations.Union(collectedVars);
                existingScope = existingScope.Union(collectedVars);
            }
        }

        // Check each let declaration body for naming errors
        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                var (funcErrors, funcShadowings) =
                    DetectNamingErrorsInFunctionImpl(
                        letFunc.Function.Declaration.Value,
                        moduleLevelNames,
                        [letFunc.Function.Declaration.Value.Name.Value]);

                allErrors.AddRange(funcErrors);
                allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, funcShadowings);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                var (exprErrors, exprShadowings) =
                    DetectNamingErrorsInExpression(
                        letDestr.Expression.Value,
                        moduleLevelNames,
                        extendedLocalDeclarations,
                        declarationPath);

                allErrors.AddRange(exprErrors);
                allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, exprShadowings);
            }
        }

        // Check the let expression body
        var (bodyErrors, bodyShadowings) =
            DetectNamingErrorsInExpression(
                letBlock.Expression.Value,
                moduleLevelNames,
                extendedLocalDeclarations,
                declarationPath);

        allErrors.AddRange(bodyErrors);
        allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, bodyShadowings);

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Detects naming errors in a list of expression nodes.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInExpressionNodes(
        IReadOnlyList<Node<SyntaxTypes.Expression>> nodes,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var node in nodes)
        {
            var (errors, shadowings) =
                DetectNamingErrorsInExpression(
                    node.Value,
                    moduleLevelNames,
                    localDeclarations,
                    declarationPath);

            allErrors.AddRange(errors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, shadowings);
        }

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Detects naming errors in record field expressions.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        DetectNamingErrorsInRecordFields(
        IReadOnlyList<Node<(Node<string>, Node<SyntaxTypes.Expression>)>> fields,
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var field in fields)
        {
            var (errors, shadowings) =
                DetectNamingErrorsInExpression(
                    field.Value.Item2.Value,
                    moduleLevelNames,
                    localDeclarations,
                    declarationPath);

            allErrors.AddRange(errors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, shadowings);
        }

        return (allErrors, allShadowings);
    }

    /// <summary>
    /// Helper to aggregate naming errors from multiple expression nodes.
    /// </summary>
    private static (IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        AggregateExpressionErrors(
        ImmutableHashSet<string> moduleLevelNames,
        ImmutableHashSet<string> localDeclarations,
        ImmutableList<string> declarationPath,
        params Node<SyntaxTypes.Expression>[] nodes)
    {
        var allErrors = new List<CanonicalizationError>();
        var allShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var node in nodes)
        {
            var (errors, shadowings) =
                DetectNamingErrorsInExpression(
                    node.Value,
                    moduleLevelNames,
                    localDeclarations,
                    declarationPath);

            allErrors.AddRange(errors);
            allShadowings = CanonicalizationResult<object>.MergeShadowings(allShadowings, shadowings);
        }

        return (allErrors, allShadowings);
    }
}
