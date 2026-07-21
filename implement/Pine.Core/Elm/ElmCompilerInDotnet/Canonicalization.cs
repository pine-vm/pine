using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using CompatibilityTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides canonicalization services for Elm modules, resolving references to their fully qualified forms
/// and detecting errors such as undefined references.
/// </summary>
public class Canonicalization
{
    /// <summary>
    /// Tracks module exports including type-to-constructor relationships for proper name resolution.
    /// </summary>
    /// <param name="TypeExports">Set of exported type names (choice types and type aliases).</param>
    /// <param name="ValueExports">Set of exported value names (functions and constructors).</param>
    /// <param name="TypeConstructors">Mapping from type names to their associated value constructors.</param>
    private record ModuleExports(
        ImmutableHashSet<string> TypeExports,
        ImmutableHashSet<string> ValueExports,
        ImmutableDictionary<string, ImmutableList<string>> TypeConstructors);

    /// <summary>
    /// Encapsulates the context required for canonicalization, including imports, aliases, and declaration scopes.
    /// </summary>
    /// <param name="CurrentModuleName">The name of the module being canonicalized.</param>
    /// <param name="TypeImportMap">Map of imported type names to their source modules.</param>
    /// <param name="ValueImportMap">Map of imported value names (functions, constructors) to their source modules.</param>
    /// <param name="AliasMap">Map of module aliases to their actual module names.</param>
    /// <param name="ModuleLevelDeclarations">Top-level declarations in the current module (functions, types, constructors).</param>
    /// <param name="LocalDeclarations">Local variable bindings from patterns, function parameters, let expressions, etc.</param>
    /// <param name="OperatorToFunction">Map of infix operators to their underlying function (module name and function name).</param>
    private record CanonicalizationContext(
        ModuleName CurrentModuleName,
        ImmutableDictionary<string, ImmutableList<ModuleName>> TypeImportMap,
        ImmutableDictionary<string, ImmutableList<ModuleName>> ValueImportMap,
        ImmutableDictionary<string, ModuleName> AliasMap,
        ImmutableHashSet<string> ModuleLevelDeclarations,
        ImmutableHashSet<string> LocalDeclarations,
        IImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)> OperatorToFunction,
        ImmutableList<string> DeclarationPath)
    {
        /// <summary>
        /// Creates a new context with additional local declarations added.
        /// Used when entering scopes that introduce new bindings (function parameters, let expressions, patterns).
        /// </summary>
        /// <param name="additionalDeclarations">Names to add to the local declarations set.</param>
        /// <returns>A new context with the combined local declarations.</returns>
        public CanonicalizationContext WithLocalDeclarations(ImmutableHashSet<string> additionalDeclarations) =>
            this with { LocalDeclarations = LocalDeclarations.Union(additionalDeclarations) };

        public CanonicalizationContext WithDefaults(ImplicitImportConfig implicitImportConfig)
        {
            // Merge implicit type imports into the type import map
            // Skip names that are already declared locally in this module
            var mergedTypeImportMap = TypeImportMap;

            foreach (var (typeName, moduleName) in implicitImportConfig.TypeImports)
            {
                if (!mergedTypeImportMap.ContainsKey(typeName) && !ModuleLevelDeclarations.Contains(typeName))
                {
                    mergedTypeImportMap = mergedTypeImportMap.Add(typeName, [moduleName]);
                }
            }

            // Merge implicit value imports into the value import map
            // Skip names that are already declared locally in this module
            var mergedValueImportMap = ValueImportMap;

            foreach (var (valueName, moduleName) in implicitImportConfig.ValueImports)
            {
                if (!mergedValueImportMap.ContainsKey(valueName) && !ModuleLevelDeclarations.Contains(valueName))
                {
                    mergedValueImportMap = mergedValueImportMap.Add(valueName, [moduleName]);
                }
            }

            // Also add type imports to value import map, since in Elm, type constructors
            // (like True, False, Just, Nothing, LT, EQ, GT) can be used as values in expressions.
            // Skip names that are already declared locally in this module
            foreach (var (typeName, moduleName) in implicitImportConfig.TypeImports)
            {
                if (!mergedValueImportMap.ContainsKey(typeName) && !ModuleLevelDeclarations.Contains(typeName))
                {
                    mergedValueImportMap = mergedValueImportMap.Add(typeName, [moduleName]);
                }
            }

            // Build operator to function mapping by merging the implicit import config operators
            // with any already-collected operators (e.g., from imported module infix declarations)
            var mergedOperatorToFunction = OperatorToFunction;

            foreach (var kvp in implicitImportConfig.OperatorToFunction)
            {
                if (!mergedOperatorToFunction.ContainsKey(kvp.Key))
                {
                    mergedOperatorToFunction =
                        mergedOperatorToFunction.Add(
                            kvp.Key,
                            ([.. kvp.Value.ModuleName], kvp.Value.FunctionName));
                }
            }

            // Merge implicit module aliases into the alias map
            // This enables e.g. using "Cmd" as an alias for "Platform.Cmd"
            var mergedAliasMap = AliasMap;

            foreach (var importedModule in implicitImportConfig.ModuleImports)
            {
                if (importedModule.Alias is { } alias && !mergedAliasMap.ContainsKey(alias))
                {
                    mergedAliasMap = mergedAliasMap.Add(alias, importedModule.ModuleName);
                }
            }

            return
                this with
                {
                    TypeImportMap = mergedTypeImportMap,
                    ValueImportMap = mergedValueImportMap,
                    OperatorToFunction = mergedOperatorToFunction,
                    AliasMap = mergedAliasMap
                };
        }
    }

    /// <summary>
    /// Renders a list of canonicalization errors into a human-readable error string.
    /// </summary>
    /// <param name="errors">The errors to render.</param>
    /// <returns>A formatted error message string.</returns>
    private static string RenderErrors(IReadOnlyList<CanonicalizationError> errors)
    {
        if (errors.Count is 0)
            return "No errors";

        var errorMessages =
            errors
            .Select(
                err => err switch
                {
                    CanonicalizationError.UnresolvedReference unresolved =>
                    $"Cannot find '{unresolved.Name}'",

                    CanonicalizationError.NamingClash clash =>
                    clash.ShadowedRange is { } shadowedRange
                    ?
                    $"This `{clash.Name}` pattern is shadowing an existing `{clash.Name}` variable at {FormatRange(shadowedRange)}. Rename one of them to avoid the ambiguity."
                    :
                    $"This `{clash.Name}` pattern is shadowing an existing `{clash.Name}` variable. Rename one of them to avoid the ambiguity.",

                    CanonicalizationError.AmbiguousImport ambiguous =>
                    $"Name '{ambiguous.Name}' is exposed by multiple imports: {string.Join(", ", ambiguous.ImportingModules)}",

                    _ =>
                    throw new NotImplementedException(
                        "RenderErrors does not handle canonicalization-error variant: " + err.GetType().Name)
                })
            .ToList();

        return string.Join("\n", errorMessages);
    }

    /// <summary>
    /// Formats a <see cref="Range"/> as a human-readable string showing line:column.
    /// </summary>
    private static string FormatRange(Range range) =>
        $"{range.Start.Row}:{range.Start.Column}-{range.End.Row}:{range.End.Column}";

    /// <summary>
    /// Wraps a value in a CanonicalizationResult with no errors.
    /// </summary>
    /// <typeparam name="T">The type of the value.</typeparam>
    /// <param name="value">The value to wrap.</param>
    /// <returns>A result containing the value with an empty error list.</returns>
    private static CanonicalizationResult<T> NoErrors<T>(T value) =>
        new(value, []);

    /// <summary>
    /// Canonicalizes a list of Elm modules, resolving all references to their fully qualified forms
    /// and detecting errors such as duplicate module names and undefined references.
    /// </summary>
    /// <param name="modules">The modules to canonicalize.</param>
    /// <returns>
    /// On success, returns a dictionary mapping module names to their canonicalized files (which may contain errors).
    /// On failure (e.g., duplicate module names), returns an error message.
    /// </returns>
    public static Result<string, IReadOnlyDictionary<ModuleName, Result<string, File>>> Canonicalize(
        IReadOnlyList<File> modules)
    {
        // Use CanonicalizeAllowingErrors and convert results to error format when there are errors
        var allowingErrorsResult = CanonicalizeAllowingErrors(modules);

        if (allowingErrorsResult.IsErrOrNull() is { } err)
        {
            return Result<string, IReadOnlyDictionary<ModuleName, Result<string, File>>>.err(err);
        }

        var allowingErrorsModules =
            allowingErrorsResult.IsOkOrNull() ??
            throw new NotImplementedException(
                "Unexpected result type from CanonicalizeAllowingErrors");

        var resultDictionary =
            new Dictionary<ModuleName, Result<string, File>>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var (moduleName, (file, errors, _)) in allowingErrorsModules)
        {
            if (errors.Count > 0)
            {
                resultDictionary[moduleName] = RenderErrors(errors);
            }
            else
            {
                resultDictionary[moduleName] = Result<string, File>.ok(file);
            }
        }

        return resultDictionary;
    }

    /// <summary>
    /// Compatibility adapter for callers that explicitly use the stil4m/elm-syntax 7 model.
    /// Canonicalization itself runs on the concrete syntax model.
    /// </summary>
    public static Result<string, IReadOnlyDictionary<ModuleName, Result<string, CompatibilityTypes.File>>> Canonicalize(
        IReadOnlyList<CompatibilityTypes.File> modules)
    {
        var concreteResult =
            Canonicalize(
                modules
                .Select(CompatibilityTypes.ToFullSyntaxModel.Convert)
                .ToList());

        if (concreteResult.IsErrOrNull() is { } error)
            return error;

        var concreteModules =
            concreteResult.IsOkOrNull() ??
            throw new NotImplementedException(
                "Unexpected result type from concrete Canonicalize");

        var compatibilityModules =
            new Dictionary<ModuleName, Result<string, CompatibilityTypes.File>>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var (moduleName, moduleResult) in concreteModules)
        {
            if (moduleResult.IsErrOrNull() is { } moduleError)
            {
                compatibilityModules[moduleName] = moduleError;
                continue;
            }

            var concreteFile =
                moduleResult.IsOkOrNull() ??
                throw new NotImplementedException(
                    "Unexpected module result type from concrete Canonicalize");

            compatibilityModules[moduleName] =
                CompatibilityTypes.FromFullSyntaxModel.Convert(
                    concreteFile with { Imports = [] });
        }

        return compatibilityModules;
    }

    /// <summary>
    /// Canonicalizes a list of Elm modules, returning the results with structured error information.
    /// This method provides a cleaner API that returns a list of errors per module instead of a single error string.
    /// </summary>
    /// <param name="modules">The modules to canonicalize.</param>
    /// <returns>
    /// On success, returns a <see cref="CanonicalizationResultWithErrors"/> containing the canonicalized modules and their errors.
    /// On failure (e.g., duplicate module names), returns an error message.
    /// </returns>
    public static Result<string, CanonicalizationResultWithErrors> CanonicalizeWithErrors(
        IReadOnlyList<File> modules)
    {
        var allowingErrorsResult = CanonicalizeAllowingErrors(modules);

        if (allowingErrorsResult.IsErrOrNull() is { } err)
        {
            return Result<string, CanonicalizationResultWithErrors>.err(err);
        }

        var allowingErrorsModules =
            allowingErrorsResult.IsOkOrNull() ??
            throw new NotImplementedException(
                "Unexpected result type from CanonicalizeAllowingErrors");

        return new CanonicalizationResultWithErrors(allowingErrorsModules);
    }

    /// <summary>
    /// Canonicalizes a list of Elm modules, resolving all references to their fully qualified forms.
    /// Unlike <see cref="Canonicalize"/>, this method always returns the canonicalized files even when
    /// there are errors (such as undefined references). This is useful for type inference where
    /// partial canonicalization (resolving cross-module references) is still valuable even if
    /// some local references cannot be resolved.
    /// </summary>
    /// <param name="modules">
    /// The modules to canonicalize.
    /// </param>
    /// <returns>
    /// On success, returns a dictionary mapping module names to tuples of (canonicalized file, errors).
    /// On failure (e.g., duplicate module names), returns an error message.
    /// </returns>
    public static Result<string, IReadOnlyDictionary<ModuleName, ModuleCanonicalizationResult>> CanonicalizeAllowingErrors(
        IReadOnlyList<File> modules)
    {
        return
            CanonicalizeAllowingErrors(
                modules,
                ImplicitImportConfig.Default);
    }

    /// <summary>
    /// Compatibility adapter for callers that explicitly use the stil4m/elm-syntax 7 model.
    /// Canonicalization itself runs on the concrete syntax model.
    /// </summary>
    public static Result<string, IReadOnlyDictionary<ModuleName, (CompatibilityTypes.File File, IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)>> CanonicalizeAllowingErrors(
        IReadOnlyList<CompatibilityTypes.File> modules)
    {
        var concreteResult =
            CanonicalizeAllowingErrors(
                modules
                .Select(CompatibilityTypes.ToFullSyntaxModel.Convert)
                .ToList());

        if (concreteResult.IsErrOrNull() is { } error)
            return error;

        var concreteModules =
            concreteResult.IsOkOrNull() ??
            throw new NotImplementedException(
                "Unexpected result type from concrete CanonicalizeAllowingErrors");

        var compatibilityModules =
            new Dictionary<ModuleName, (CompatibilityTypes.File, IReadOnlyList<CanonicalizationError>, ImmutableDictionary<string, ShadowingLocation>)>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var (moduleName, moduleResult) in concreteModules)
        {
            compatibilityModules[moduleName] =
                (CompatibilityTypes.FromFullSyntaxModel.Convert(
                    moduleResult.File with { Imports = [] }),
                moduleResult.Errors,
                moduleResult.Shadowings);
        }

        return compatibilityModules;
    }

    /// <summary>
    /// Canonicalizes a list of Elm modules, resolving all references to their fully qualified forms.
    /// Unlike <see cref="Canonicalize"/>, this method always returns the canonicalized files even when
    /// there are errors (such as undefined references). This is useful for type inference where
    /// partial canonicalization (resolving cross-module references) is still valuable even if
    /// some local references cannot be resolved.
    /// </summary>
    /// <param name="modules">
    /// The modules to canonicalize.
    /// </param>
    /// <param name="implicitImportConfig">
    /// Configuration for implicit imports to be added to each module.
    /// </param>
    /// <returns>
    /// On success, returns a dictionary mapping module names to tuples of (canonicalized file, errors, shadowings).
    /// On failure (e.g., duplicate module names), returns an error message.
    /// </returns>
    public static Result<string, IReadOnlyDictionary<ModuleName, ModuleCanonicalizationResult>> CanonicalizeAllowingErrors(
        IReadOnlyList<File> modules,
        ImplicitImportConfig implicitImportConfig)
    {
        // Check for duplicate module names
        var moduleNameGroups =
            modules
            .GroupBy(
                m => SyntaxTypes.Module.GetModuleName(m.ModuleDefinition.Value).Value,
                EnumerableExtensions.EqualityComparer<ModuleName>())
            .ToList();

        var duplicateModules =
            moduleNameGroups
            .Where(g => g.Count() > 1)
            .ToList();

        if (duplicateModules.Count is not 0)
        {
            var duplicateNames =
                duplicateModules
                .Select(g => string.Join(".", g.Key))
                .ToList();

            return
                Result<string, IReadOnlyDictionary<ModuleName, ModuleCanonicalizationResult>>.err(
                    $"Duplicate module names: {string.Join(", ", duplicateNames)}");
        }

        // Build module exports map for resolving exposing (..)
        var moduleExportsMap = BuildModuleExportsMap(modules);

        // Build a map of module names to their infix declarations
        var moduleInfixMap = BuildModuleInfixMap(modules);

        var resultDictionary =
            new Dictionary<ModuleName, ModuleCanonicalizationResult>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var module in modules)
        {
            var currentModuleName =
                SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            // Build import maps and alias map for this module
            var (typeImportMap, valueImportMap, aliasMap) =
                BuildImportMaps(module.Imports, moduleExportsMap);

            // Build set of module-level declarations (top-level names declared in this module)
            var moduleLevelDeclarations = BuildLocalDeclarations(module);

            // Collect infix operators from imported modules
            var operatorToFunction =
                CollectImportedInfixOperators(module.Imports, moduleExportsMap, moduleInfixMap);

            // Create canonicalization context for this module
            var context =
                new CanonicalizationContext(
                    CurrentModuleName: currentModuleName,
                    TypeImportMap: typeImportMap,
                    ValueImportMap: valueImportMap,
                    AliasMap: aliasMap,
                    ModuleLevelDeclarations: moduleLevelDeclarations,
                    LocalDeclarations: [],
                    OperatorToFunction: operatorToFunction,
                    DeclarationPath: [])
                .WithDefaults(implicitImportConfig);

            // Detect module-level declarations that shadow imported names
            var moduleLevelShadowings =
                CollectModuleLevelShadowings(module, context.ValueImportMap, context.TypeImportMap);

            // Canonicalize declarations and collect errors
            var canonicalizedDeclarationsWithErrors =
                module.Declarations
                .Select(decl => CanonicalizeDeclaration(decl, context))
                .ToList();

            // Aggregate all errors from declarations
            // Note: Import clash errors are now detected lazily when names are actually referenced
            var allErrors =
                canonicalizedDeclarationsWithErrors
                .SelectMany(result => result.Errors)
                .ToList();

            // Aggregate all shadowings from declarations and module-level shadowings
            var allShadowings = moduleLevelShadowings;

            foreach (var declResult in canonicalizedDeclarationsWithErrors)
            {
                allShadowings =
                    CanonicalizationResult<object>.MergeShadowings(allShadowings, declResult.Shadowings);
            }

            // Extract canonicalized declarations
            var canonicalizedDeclarations =
                canonicalizedDeclarationsWithErrors
                .Select(result => result.Value)
                .ToList();

            // Preserve the source-shaped file. Imports are still needed for diagnostics and
            // source mapping even though all references in declarations are now canonical.
            var canonicalizedModule =
                module with
                {
                    Declarations = canonicalizedDeclarations
                };

            // Always return the file along with any errors and shadowings
            resultDictionary[currentModuleName] =
                new ModuleCanonicalizationResult(
                    canonicalizedModule,
                    allErrors,
                    allShadowings);
        }

        return resultDictionary;
    }

    private static ImmutableHashSet<string> BuildLocalDeclarations(File module)
    {
        var localDeclarationsBuilder = ImmutableHashSet.CreateBuilder<string>();

        foreach (var decl in module.Declarations)
        {
            var declName =
                decl.Value switch
                {
                    Declaration.FunctionDeclaration funcDecl =>
                    funcDecl.Function.Declaration.Value.Name.Value,

                    Declaration.ChoiceTypeDeclaration typeDecl =>
                    typeDecl.TypeDeclaration.Name.Value,

                    Declaration.AliasDeclaration aliasDecl =>
                    aliasDecl.TypeAlias.Name.Value,

                    Declaration.InfixDeclaration infixDecl =>
                    infixDecl.Infix.Operator.Value,

                    Declaration.PortDeclaration portDecl =>
                    portDecl.Signature.Name.Value,

                    _ =>
                    throw new NotImplementedException(
                        $"Unhandled declaration type in BuildLocalDeclarations: {decl.Value.GetType().Name}")
                };

            localDeclarationsBuilder.Add(declName);

            // Also add type constructors for choice types
            if (decl.Value is Declaration.ChoiceTypeDeclaration choiceTypeDecl)
            {
                foreach (var ctor in choiceTypeDecl.TypeDeclaration.Constructors)
                {
                    localDeclarationsBuilder.Add(ctor.Constructor.Value.Name.Value);
                }
            }
        }

        return localDeclarationsBuilder.ToImmutable();
    }

    /// <summary>
    /// Detects module-level declarations that shadow imported names.
    /// Module-level declarations are allowed to shadow imports in Elm, but we track them for analysis.
    /// </summary>
    private static ImmutableDictionary<string, ShadowingLocation> CollectModuleLevelShadowings(
        File module,
        ImmutableDictionary<string, ImmutableList<ModuleName>> valueImportMap,
        ImmutableDictionary<string, ImmutableList<ModuleName>> typeImportMap)
    {
        var emptyPath = ImmutableList<string>.Empty;
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var decl in module.Declarations)
        {
            switch (decl.Value)
            {
                case Declaration.FunctionDeclaration funcDecl:
                    {
                        var nameNode = funcDecl.Function.Declaration.Value.Name;

                        if (valueImportMap.ContainsKey(nameNode.Value) &&
                            !shadowings.ContainsKey(nameNode.Value))
                        {
                            shadowings =
                                shadowings.Add(
                                    nameNode.Value,
                                    new ShadowingLocation(nameNode.Range, emptyPath));
                        }

                        break;
                    }

                case Declaration.ChoiceTypeDeclaration typeDecl:
                    {
                        var nameNode = typeDecl.TypeDeclaration.Name;

                        if (typeImportMap.ContainsKey(nameNode.Value) &&
                            !shadowings.ContainsKey(nameNode.Value))
                        {
                            shadowings =
                                shadowings.Add(
                                    nameNode.Value,
                                    new ShadowingLocation(nameNode.Range, emptyPath));
                        }

                        // Also check type constructors
                        foreach (var ctor in typeDecl.TypeDeclaration.Constructors)
                        {
                            if (valueImportMap.ContainsKey(ctor.Constructor.Value.Name.Value) &&
                                !shadowings.ContainsKey(ctor.Constructor.Value.Name.Value))
                            {
                                shadowings =
                                    shadowings.Add(
                                        ctor.Constructor.Value.Name.Value,
                                        new ShadowingLocation(ctor.Constructor.Value.Name.Range, emptyPath));
                            }
                        }

                        break;
                    }

                case Declaration.AliasDeclaration aliasDecl:
                    {
                        var nameNode = aliasDecl.TypeAlias.Name;

                        if (typeImportMap.ContainsKey(nameNode.Value) &&
                            !shadowings.ContainsKey(nameNode.Value))
                        {
                            shadowings =
                                shadowings.Add(
                                    nameNode.Value,
                                    new ShadowingLocation(nameNode.Range, emptyPath));
                        }

                        break;
                    }

                case Declaration.PortDeclaration portDecl:
                    {
                        var nameNode = portDecl.Signature.Name;

                        if (valueImportMap.ContainsKey(nameNode.Value) &&
                            !shadowings.ContainsKey(nameNode.Value))
                        {
                            shadowings =
                                shadowings.Add(
                                    nameNode.Value,
                                    new ShadowingLocation(nameNode.Range, emptyPath));
                        }

                        break;
                    }

                case SyntaxTypes.Declaration.InfixDeclaration:
                    break;

                default:
                    throw new NotImplementedException(
                        "CollectModuleLevelShadowings does not handle declaration variant: " +
                        decl.Value.GetType().Name);
            }
        }

        return shadowings;
    }

    private static ImmutableDictionary<string, ModuleExports> BuildModuleExportsMap(
        IReadOnlyList<File> modules)
    {
        var exportsMapBuilder = ImmutableDictionary.CreateBuilder<string, ModuleExports>();

        foreach (var module in modules)
        {
            var moduleName =
                string.Join(".", SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value);

            var typeExportsBuilder = ImmutableHashSet.CreateBuilder<string>();
            var valueExportsBuilder = ImmutableHashSet.CreateBuilder<string>();

            var typeConstructorsBuilder = ImmutableDictionary.CreateBuilder<string, ImmutableList<string>>();

            // Get the exposing list from the module
            var exposingList =
                module.ModuleDefinition.Value switch
                {
                    Module.NormalModule normalModule =>
                    normalModule.ModuleData.ExposingList.Value,

                    Module.PortModule portModule =>
                    portModule.ModuleData.ExposingList.Value,

                    Module.EffectModule effectModule =>
                    effectModule.ModuleData.ExposingList.Value,

                    _ =>
                    throw new NotImplementedException(
                        $"Unhandled module type: {module.ModuleDefinition.Value.GetType().Name}")
                };

            // Check if the module exposes all (..)
            var exposesAll = exposingList is Exposing.All;

            // Build a map of all declarations for filtering
            var allDeclarations = new Dictionary<string, Declaration>();

            foreach (var decl in module.Declarations)
            {
                var declName =
                    decl.Value switch
                    {
                        Declaration.FunctionDeclaration funcDecl =>
                        funcDecl.Function.Declaration.Value.Name.Value,

                        Declaration.ChoiceTypeDeclaration typeDecl =>
                        typeDecl.TypeDeclaration.Name.Value,

                        Declaration.AliasDeclaration aliasDecl =>
                        aliasDecl.TypeAlias.Name.Value,

                        Declaration.InfixDeclaration infixDecl =>
                        infixDecl.Infix.Operator.Value,

                        Declaration.PortDeclaration portDecl =>
                        portDecl.Signature.Name.Value,

                        _ =>
                        throw new NotImplementedException(
                            $"Unhandled declaration type in BuildModuleExportsMap: {decl.Value.GetType().Name}")
                    };

                allDeclarations[declName] = decl.Value;
            }

            // If exposing all, add everything
            if (exposesAll)
            {
                foreach (var decl in module.Declarations)
                {
                    switch (decl.Value)
                    {
                        case Declaration.FunctionDeclaration funcDecl:
                            valueExportsBuilder.Add(funcDecl.Function.Declaration.Value.Name.Value);
                            break;

                        case Declaration.ChoiceTypeDeclaration typeDecl:
                            var typeName = typeDecl.TypeDeclaration.Name.Value;
                            typeExportsBuilder.Add(typeName);

                            var constructorsBuilder = ImmutableList.CreateBuilder<string>();

                            foreach (var ctor in typeDecl.TypeDeclaration.Constructors)
                            {
                                var ctorName = ctor.Constructor.Value.Name.Value;
                                valueExportsBuilder.Add(ctorName);
                                constructorsBuilder.Add(ctorName);
                            }

                            typeConstructorsBuilder[typeName] = constructorsBuilder.ToImmutable();
                            break;

                        case Declaration.AliasDeclaration aliasDecl:
                            typeExportsBuilder.Add(aliasDecl.TypeAlias.Name.Value);

                            // Record type aliases also create an implicit constructor function
                            if (aliasDecl.TypeAlias.TypeAnnotation.Value
                                is TypeAnnotation.Record)
                            {
                                valueExportsBuilder.Add(aliasDecl.TypeAlias.Name.Value);
                            }

                            break;

                        case Declaration.InfixDeclaration infixDecl:
                            valueExportsBuilder.Add(infixDecl.Infix.Operator.Value);
                            break;

                        case Declaration.PortDeclaration portDecl:
                            valueExportsBuilder.Add(portDecl.Signature.Name.Value);
                            break;

                        default:
                            throw new NotImplementedException(
                                $"Unhandled declaration type in BuildModuleExportsMap: {decl.Value.GetType().Name}");
                    }
                }
            }
            else if (exposingList is Exposing.Explicit explicitExposing)
            {
                // Only export what's explicitly listed
                foreach (var exposeNode in explicitExposing.Nodes)
                {
                    var expose = exposeNode.Value;

                    switch (expose)
                    {
                        case TopLevelExpose.InfixExpose infixExpose:
                            valueExportsBuilder.Add(infixExpose.Name);
                            break;

                        case TopLevelExpose.FunctionExpose funcExpose:
                            valueExportsBuilder.Add(funcExpose.Name);
                            break;

                        case TopLevelExpose.TypeOrAliasExpose typeOrAlias:
                            typeExportsBuilder.Add(typeOrAlias.Name);

                            // Record type aliases also create an implicit constructor function
                            if (allDeclarations.TryGetValue(typeOrAlias.Name, out var aliasLookup) &&
                                aliasLookup is Declaration.AliasDeclaration aliasDeclLookup &&
                                aliasDeclLookup.TypeAlias.TypeAnnotation.Value
                                is TypeAnnotation.Record)
                            {
                                valueExportsBuilder.Add(typeOrAlias.Name);
                            }

                            break;

                        case TopLevelExpose.TypeExpose typeExpose:
                            var exposedTypeName = typeExpose.ExposedType.Name;
                            typeExportsBuilder.Add(exposedTypeName);

                            // If exposing constructors (..), add them
                            if (typeExpose.ExposedType.Open is not null &&
                                allDeclarations.TryGetValue(exposedTypeName, out var typeDecl) &&
                                typeDecl is Declaration.ChoiceTypeDeclaration choiceTypeDecl)
                            {
                                var constructorsBuilder = ImmutableList.CreateBuilder<string>();

                                foreach (var ctor in choiceTypeDecl.TypeDeclaration.Constructors)
                                {
                                    var ctorName = ctor.Constructor.Value.Name.Value;
                                    valueExportsBuilder.Add(ctorName);
                                    constructorsBuilder.Add(ctorName);
                                }

                                typeConstructorsBuilder[exposedTypeName] = constructorsBuilder.ToImmutable();
                            }

                            break;

                        default:
                            throw new NotImplementedException(
                                $"Unhandled expose type in BuildModuleExportsMap: {expose.GetType().Name}");
                    }
                }
            }

            exportsMapBuilder[moduleName] =
                new ModuleExports(
                    typeExportsBuilder.ToImmutable(),
                    valueExportsBuilder.ToImmutable(),
                    typeConstructorsBuilder.ToImmutable());
        }

        return exportsMapBuilder.ToImmutable();
    }

    private static (ImmutableDictionary<string, ImmutableList<ModuleName>>, ImmutableDictionary<string, ImmutableList<ModuleName>>, ImmutableDictionary<string, ModuleName>) BuildImportMaps(
        IReadOnlyList<Node<Import>> imports,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        var typeImportMap =
            ImmutableDictionary<string, ImmutableList<ModuleName>>.Empty.ToBuilder();

        var valueImportMap =
            ImmutableDictionary<string, ImmutableList<ModuleName>>.Empty.ToBuilder();

        var aliasMap =
            ImmutableDictionary<string, ModuleName>.Empty.ToBuilder();

        foreach (var importNode in imports)
        {
            var import = importNode.Value;
            var moduleName = import.ModuleName.Value;
            var moduleNameStr = string.Join(".", moduleName);

            // Handle module alias
            if (import.ModuleAlias is { } importModuleAlias)
            {
                var alias = string.Join(".", importModuleAlias.Alias.Value);
                aliasMap[alias] = moduleName;
            }

            // Get exposed items
            if (import.ExposingList is null)
            {
                continue;
            }

            var exposing = import.ExposingList.Value.ExposingList.Value;

            if (exposing is Exposing.All)
            {
                // Handle 'exposing (..)' 
                if (moduleExportsMap.TryGetValue(moduleNameStr, out var moduleExports))
                {
                    // Add type exports to type import map
                    foreach (var exportedTypeName in moduleExports.TypeExports)
                    {
                        if (!typeImportMap.TryGetValue(exportedTypeName, out var value))
                        {
                            value = [];
                            typeImportMap[exportedTypeName] = value;
                        }

                        typeImportMap[exportedTypeName] = value.Add(moduleName);
                    }

                    // Add value exports to value import map
                    foreach (var exportedValueName in moduleExports.ValueExports)
                    {
                        if (!valueImportMap.TryGetValue(exportedValueName, out var value))
                        {
                            value = [];
                            valueImportMap[exportedValueName] = value;
                        }

                        valueImportMap[exportedValueName] = value.Add(moduleName);
                    }
                }

                continue;
            }

            if (exposing is Exposing.Explicit explicitExposing)
            {
                foreach (var exposeNode in explicitExposing.Nodes)
                {
                    var expose = exposeNode.Value;

                    // Get type and value names separately
                    var (typeNames, valueNames) = GetExposedNamesByNamespace(expose, moduleNameStr, moduleExportsMap);

                    foreach (var name in typeNames)
                    {
                        if (!typeImportMap.TryGetValue(name, out var value))
                        {
                            value = [];
                            typeImportMap[name] = value;
                        }

                        // Avoid adding the same module name multiple times for the same name
                        if (!value.Any(m => m.SequenceEqual(moduleName)))
                        {
                            typeImportMap[name] = value.Add(moduleName);
                        }
                    }

                    foreach (var name in valueNames)
                    {
                        if (!valueImportMap.TryGetValue(name, out var value))
                        {
                            value = [];
                            valueImportMap[name] = value;
                        }

                        // Avoid adding the same module name multiple times for the same name
                        if (!value.Any(m => m.SequenceEqual(moduleName)))
                        {
                            valueImportMap[name] = value.Add(moduleName);
                        }
                    }
                }
            }
        }

        return (typeImportMap.ToImmutable(), valueImportMap.ToImmutable(), aliasMap.ToImmutable());
    }

    /// <summary>
    /// Returns type names and value names separately for an exposed item.
    /// </summary>
    private static (ModuleName TypeNames, ModuleName ValueNames) GetExposedNamesByNamespace(
        TopLevelExpose expose,
        string moduleName,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        return expose switch
        {
            // Infix operators are values
            TopLevelExpose.InfixExpose infixExpose =>
            ([], [infixExpose.Name]),

            // Functions are values
            TopLevelExpose.FunctionExpose functionExpose =>
            ([], [functionExpose.Name]),

            // Type or alias without constructors - type name only
            // However, record type aliases also export a value (constructor function)
            TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
            (moduleExportsMap.TryGetValue(moduleName, out var aliasModuleExports) &&
            aliasModuleExports.ValueExports.Contains(typeOrAliasExpose.Name))
            ?
            ([typeOrAliasExpose.Name], [typeOrAliasExpose.Name])
            :
            ([typeOrAliasExpose.Name], []),

            // Type with constructors - type name as type, constructors as values
            TopLevelExpose.TypeExpose typeExpose =>
            GetTypeExposeNamesByNamespace(typeExpose, moduleName, moduleExportsMap),

            _ =>
            throw new NotImplementedException(
                $"Unhandled TopLevelExpose type in GetExposedNamesByNamespace: {expose.GetType().Name}")
        };
    }

    private static (ModuleName TypeNames, ModuleName ValueNames) GetTypeExposeNamesByNamespace(
        TopLevelExpose.TypeExpose typeExpose,
        string moduleName,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        var typeName = typeExpose.ExposedType.Name;
        var typeNames = new List<string> { typeName };
        var valueNames = new List<string>();

        // If Open is not null, it means we have Status(..) syntax - expose all constructors of this type
        if (typeExpose.ExposedType.Open is not null &&
            moduleExportsMap.TryGetValue(moduleName, out var moduleExports) &&
            moduleExports.TypeConstructors.TryGetValue(typeName, out var constructors))
        {
            // Constructors go to value namespace
            valueNames.AddRange(constructors);
        }

        return (typeNames, valueNames);
    }

    private static CanonicalizationResult<Node<Declaration>> CanonicalizeDeclaration(
        Node<Declaration> declNode,
        CanonicalizationContext context)
    {
        var decl = declNode.Value;

        // Canonicalize based on declaration type
        Declaration canonicalizedDecl;

        IReadOnlyList<CanonicalizationError> errors;
        ImmutableDictionary<string, ShadowingLocation> shadowings;

        switch (decl)
        {
            case Declaration.FunctionDeclaration funcDecl:
                {
                    var funcName = funcDecl.Function.Declaration.Value.Name.Value;

                    var contextWithDeclPath =
                        context with { DeclarationPath = [funcName] };

                    var funcResult =
                        CanonicalizeFunctionStruct(
                            funcDecl.Function,
                            contextWithDeclPath);

                    canonicalizedDecl = new Declaration.FunctionDeclaration(funcResult.Value);

                    errors = funcResult.Errors;
                    shadowings = funcResult.Shadowings;
                    break;
                }

            case Declaration.ChoiceTypeDeclaration typeDecl:
                {
                    var typeResult =
                        CanonicalizeTypeStruct(
                            typeDecl.TypeDeclaration,
                            context);

                    canonicalizedDecl = new Declaration.ChoiceTypeDeclaration(typeResult.Value);

                    errors = typeResult.Errors;
                    shadowings = typeResult.Shadowings;
                    break;
                }

            case Declaration.AliasDeclaration aliasDecl:
                {
                    var aliasResult =
                        CanonicalizeTypeAlias(
                            aliasDecl.TypeAlias,
                            context);

                    canonicalizedDecl = new Declaration.AliasDeclaration(aliasResult.Value);

                    errors = aliasResult.Errors;
                    shadowings = aliasResult.Shadowings;
                    break;
                }

            case Declaration.PortDeclaration portDecl:
                {
                    var signatureResult =
                        CanonicalizeSignature(
                            portDecl.Signature,
                            context);

                    canonicalizedDecl =
                        new Declaration.PortDeclaration(portDecl.PortTokenLocation, signatureResult.Value);

                    errors = signatureResult.Errors;
                    shadowings = signatureResult.Shadowings;
                    break;
                }

            case SyntaxTypes.Declaration.InfixDeclaration:
                canonicalizedDecl = decl; // No canonicalization needed for infix declarations
                errors = [];
                shadowings = [];
                break;

            default:
                throw new NotImplementedException(
                    $"Unhandled declaration type in CanonicalizeDeclaration: {decl.GetType().Name}");
        }

        var canonicalizedNode =
            new Node<Declaration>(
                declNode.Range,
                canonicalizedDecl);

        return
            new CanonicalizationResult<Node<Declaration>>(
                canonicalizedNode,
                errors,
                shadowings);
    }

    private static CanonicalizationResult<FunctionStruct> CanonicalizeFunctionStruct(
        FunctionStruct func,
        CanonicalizationContext context)
    {
        var signatureResult =
            func.Signature is null
            ?
            NoErrors<Node<Signature>?>(null)
            :
            CanonicalizeSignature(func.Signature.Value, context)
            .MapValue(
                sig => (Node<Signature>?)new Node<Signature>(
                    func.Signature.Range,
                    sig));

        var implResult =
            CanonicalizeFunctionImplementation(
                func.Declaration.Value,
                context);

        return
            CanonicalizationResultExtensions.Map2(
                signatureResult,
                implResult,
                (canonicalizedSignature, canonicalizedImpl) => new FunctionStruct(
                    Documentation: func.Documentation,
                    Signature: canonicalizedSignature,
                    Declaration: new Node<FunctionImplementation>(
                        func.Declaration.Range,
                        canonicalizedImpl)));
    }

    private static CanonicalizationResult<FunctionImplementation> CanonicalizeFunctionImplementation(
        FunctionImplementation impl,
        CanonicalizationContext context)
    {
        // Collect parameter variables while checking for shadowing.
        // In Elm, parameter names are not allowed to shadow any declaration,
        // including module-level declarations.
        var parameterVariables = ImmutableHashSet<string>.Empty;

        var shadowErrors = new List<CanonicalizationError>();
        var paramShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var arg in impl.Arguments)
        {
            // Check against both other parameters and module-level declarations
            var existingScope = context.ModuleLevelDeclarations.Union(parameterVariables);

            var (collectedVars, errors, argShadowings) =
                CollectPatternVariablesWithShadowCheck(
                    arg.Value,
                    arg.Range,
                    existingScope,
                    parameterVariables,
                    context.DeclarationPath);

            parameterVariables = parameterVariables.Union(collectedVars);
            shadowErrors.AddRange(errors);
            paramShadowings = CanonicalizationResult<object>.MergeShadowings(paramShadowings, argShadowings);
        }

        // Create new context with parameter variables added to local declarations
        var contextWithParams = context.WithLocalDeclarations(parameterVariables);

        var argumentResults =
            impl.Arguments
            .Select(arg => CanonicalizePatternNode(arg, contextWithParams))
            .ToList();

        var canonicalizedArguments =
            argumentResults.Select(r => r.Value).ToList();

        var argumentErrors =
            argumentResults.SelectMany(r => r.Errors).ToList();

        var argumentShadowings =
            argumentResults.Aggregate(
                ImmutableDictionary<string, ShadowingLocation>.Empty,
                (acc, r) => CanonicalizationResult<object>.MergeShadowings(acc, r.Shadowings));

        var exprResult =
            CanonicalizeExpressionNode(
                impl.Expression,
                contextWithParams);

        var functionImplementation =
            new FunctionImplementation(
                Name: impl.Name,
                Arguments: canonicalizedArguments,
                EqualsTokenLocation: impl.EqualsTokenLocation,
                Expression: exprResult.Value);

        var allErrors =
            shadowErrors.Concat(argumentErrors).Concat(exprResult.Errors).ToList();

        var allShadowings =
            CanonicalizationResult<object>.MergeShadowings(
                CanonicalizationResult<object>.MergeShadowings(paramShadowings, argumentShadowings),
                exprResult.Shadowings);

        return new CanonicalizationResult<FunctionImplementation>(functionImplementation, allErrors, allShadowings);
    }

    internal static ImmutableHashSet<string> CollectPatternVariables(
        Pattern pattern,
        ImmutableHashSet<string> variables)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.AllPattern:

                // Matches anything, no variables to collect
                return variables;

            case Pattern.VarPattern varPattern:
                return variables.Add(varPattern.Name);

            case SyntaxTypes.Pattern.UnitPattern:

                // Unit pattern, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.CharPattern:

                // Character literal, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.StringPattern:

                // String literal, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.IntPattern:

                // Integer literal, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.HexPattern:

                // Hex literal, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.FloatPattern:

                // Float literal, no variables to collect
                return variables;

            case Pattern.TuplePattern tuple:
                {
                    var result = variables;

                    foreach (var elem in tuple.Elements)
                    {
                        result = CollectPatternVariables(elem.Value, result);
                    }

                    return result;
                }

            case Pattern.RecordPattern recordPattern:
                return variables.Union(recordPattern.Fields.Select(f => f.Value));

            case Pattern.UnConsPattern unCons:
                {
                    var result = CollectPatternVariables(unCons.Head.Value, variables);
                    return CollectPatternVariables(unCons.Tail.Value, result);
                }

            case Pattern.ListPattern list:
                {
                    var result = variables;

                    foreach (var elem in list.Elements)
                    {
                        result = CollectPatternVariables(elem.Value, result);
                    }

                    return result;
                }

            case Pattern.NamedPattern named:
                {
                    var result = variables;

                    foreach (var arg in named.Arguments)
                    {
                        result = CollectPatternVariables(arg.Value, result);
                    }

                    return result;
                }

            case Pattern.AsPattern asPattern:
                {
                    var result =
                        CollectPatternVariables(asPattern.Pattern.Value, variables);

                    return result.Add(asPattern.Name.Value);
                }

            case Pattern.ParenthesizedPattern parenPattern:
                return CollectPatternVariables(parenPattern.Pattern.Value, variables);

            default:
                throw new NotImplementedException(
                    $"Unhandled pattern type in CollectPatternVariables: {pattern.GetType().Name}");
        }
    }

    /// <summary>
    /// Collects variables from a pattern while detecting shadowing errors.
    /// Elm disallows variable shadowing in the same scope to prevent bugs.
    /// See: https://github.com/elm/compiler/blob/cce7a8bbd8fe690fc83fa795f8d7e02505d1f25f/hints/shadowing.md
    /// </summary>
    /// <param name="pattern">The pattern to collect variables from.</param>
    /// <param name="patternRange">The source range of the pattern for error reporting.</param>
    /// <param name="existingVariables">Variables already in scope that would be shadowed.</param>
    /// <param name="collectedVariables">Variables collected so far in this pattern.</param>
    /// <param name="declarationPath">The declaration path for recording shadowings.</param>
    /// <returns>A tuple of (collected variables, shadowing errors, detected shadowings).</returns>
    internal static (ImmutableHashSet<string> Variables, IReadOnlyList<CanonicalizationError> Errors, ImmutableDictionary<string, ShadowingLocation> Shadowings)
        CollectPatternVariablesWithShadowCheck(
        Pattern pattern,
        Range patternRange,
        ImmutableHashSet<string> existingVariables,
        ImmutableHashSet<string> collectedVariables,
        ImmutableList<string> declarationPath)
    {
        var errors = new List<CanonicalizationError>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        switch (pattern)
        {
            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.UnitPattern:
            case SyntaxTypes.Pattern.CharPattern:
            case SyntaxTypes.Pattern.StringPattern:
            case SyntaxTypes.Pattern.IntPattern:
            case SyntaxTypes.Pattern.HexPattern:
            case SyntaxTypes.Pattern.FloatPattern:
                return (collectedVariables, errors, shadowings);

            case Pattern.VarPattern varPattern:
                {
                    if (existingVariables.Contains(varPattern.Name))
                    {
                        errors.Add(
                            new CanonicalizationError.NamingClash(
                                patternRange,
                                varPattern.Name));

                        if (!shadowings.ContainsKey(varPattern.Name))
                        {
                            shadowings =
                                shadowings.Add(
                                    varPattern.Name,
                                    new ShadowingLocation(patternRange, declarationPath));
                        }
                    }

                    return (collectedVariables.Add(varPattern.Name), errors, shadowings);
                }

            case Pattern.TuplePattern tuple:
                {
                    var result = collectedVariables;

                    foreach (var elem in tuple.Elements)
                    {
                        var (newVars, newErrors, newShadowings) =
                            CollectPatternVariablesWithShadowCheck(
                                elem.Value,
                                elem.Range,
                                existingVariables.Union(result),
                                result,
                                declarationPath);

                        result = newVars;
                        errors.AddRange(newErrors);
                        shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, newShadowings);
                    }

                    return (result, errors, shadowings);
                }

            case Pattern.RecordPattern recordPattern:
                {
                    foreach (var field in recordPattern.Fields)
                    {
                        if (existingVariables.Contains(field.Value))
                        {
                            errors.Add(
                                new CanonicalizationError.NamingClash(
                                    field.Range,
                                    field.Value));

                            if (!shadowings.ContainsKey(field.Value))
                            {
                                shadowings =
                                    shadowings.Add(
                                        field.Value,
                                        new ShadowingLocation(field.Range, declarationPath));
                            }
                        }
                    }

                    return (collectedVariables.Union(recordPattern.Fields.Select(f => f.Value)), errors, shadowings);
                }

            case Pattern.UnConsPattern unCons:
                {
                    var (headVars, headErrors, headShadowings) =
                        CollectPatternVariablesWithShadowCheck(
                            unCons.Head.Value,
                            unCons.Head.Range,
                            existingVariables,
                            collectedVariables,
                            declarationPath);

                    var (tailVars, tailErrors, tailShadowings) =
                        CollectPatternVariablesWithShadowCheck(
                            unCons.Tail.Value,
                            unCons.Tail.Range,
                            existingVariables.Union(headVars),
                            headVars,
                            declarationPath);

                    errors.AddRange(headErrors);
                    errors.AddRange(tailErrors);
                    shadowings = CanonicalizationResult<object>.MergeShadowings(headShadowings, tailShadowings);
                    return (tailVars, errors, shadowings);
                }

            case Pattern.ListPattern list:
                {
                    var result = collectedVariables;

                    foreach (var elem in list.Elements)
                    {
                        var (newVars, newErrors, newShadowings) =
                            CollectPatternVariablesWithShadowCheck(
                                elem.Value,
                                elem.Range,
                                existingVariables.Union(result),
                                result,
                                declarationPath);

                        result = newVars;
                        errors.AddRange(newErrors);
                        shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, newShadowings);
                    }

                    return (result, errors, shadowings);
                }

            case Pattern.NamedPattern named:
                {
                    var result = collectedVariables;

                    foreach (var arg in named.Arguments)
                    {
                        var (newVars, newErrors, newShadowings) =
                            CollectPatternVariablesWithShadowCheck(
                                arg.Value,
                                arg.Range,
                                existingVariables.Union(result),
                                result,
                                declarationPath);

                        result = newVars;
                        errors.AddRange(newErrors);
                        shadowings = CanonicalizationResult<object>.MergeShadowings(shadowings, newShadowings);
                    }

                    return (result, errors, shadowings);
                }

            case Pattern.AsPattern asPattern:
                {
                    var (patternVars, patternErrors, patternShadowings) =
                        CollectPatternVariablesWithShadowCheck(
                            asPattern.Pattern.Value,
                            asPattern.Pattern.Range,
                            existingVariables,
                            collectedVariables,
                            declarationPath);

                    errors.AddRange(patternErrors);
                    shadowings = patternShadowings;

                    if (existingVariables.Contains(asPattern.Name.Value))
                    {
                        errors.Add(
                            new CanonicalizationError.NamingClash(
                                asPattern.Name.Range,
                                asPattern.Name.Value));

                        if (!shadowings.ContainsKey(asPattern.Name.Value))
                        {
                            shadowings =
                                shadowings.Add(
                                    asPattern.Name.Value,
                                    new ShadowingLocation(asPattern.Name.Range, declarationPath));
                        }
                    }

                    return (patternVars.Add(asPattern.Name.Value), errors, shadowings);
                }

            case Pattern.ParenthesizedPattern parenPattern:
                return
                    CollectPatternVariablesWithShadowCheck(
                        parenPattern.Pattern.Value,
                        parenPattern.Pattern.Range,
                        existingVariables,
                        collectedVariables,
                        declarationPath);

            default:
                throw new NotImplementedException(
                    $"Unhandled pattern type in CollectPatternVariablesWithShadowCheck: {pattern.GetType().Name}");
        }
    }

    private static CanonicalizationResult<Signature> CanonicalizeSignature(
        Signature signature,
        CanonicalizationContext context)
    {
        var typeAnnotationResult =
            CanonicalizeTypeAnnotationNode(
                signature.TypeAnnotation,
                context);

        var canonicalizedSignature =
            new Signature(
                Name: signature.Name,
                ColonLocation: signature.ColonLocation,
                TypeAnnotation: typeAnnotationResult.Value);

        return
            new CanonicalizationResult<Signature>(
                canonicalizedSignature,
                typeAnnotationResult.Errors,
                typeAnnotationResult.Shadowings);
    }

    private static CanonicalizationResult<TypeStruct> CanonicalizeTypeStruct(
        TypeStruct typeStruct,
        CanonicalizationContext context)
    {
        return
            CanonicalizationResultExtensions.ConcatMap(
                typeStruct.Constructors,
                ctor => CanonicalizeValueConstructorNode(ctor, context))
            .MapValue(
                canonicalizedConstructors => new TypeStruct(
                    Documentation: typeStruct.Documentation,
                    TypeTokenLocation: typeStruct.TypeTokenLocation,
                    Name: typeStruct.Name,
                    Generics: typeStruct.Generics,
                    EqualsTokenLocation: typeStruct.EqualsTokenLocation,
                    Constructors: [.. canonicalizedConstructors]));
    }

    private static CanonicalizationResult<(Location? PipeTokenLocation, Node<ValueConstructor> Constructor)> CanonicalizeValueConstructorNode(
        (Location? PipeTokenLocation, Node<ValueConstructor> Constructor) ctorEntry,
        CanonicalizationContext context)
    {
        var ctorNode = ctorEntry.Constructor;
        var ctor = ctorNode.Value;

        return
            CanonicalizationResultExtensions.ConcatMap(
                ctor.Arguments,
                arg => CanonicalizeTypeAnnotationNode(arg, context))
            .MapValue(
                canonicalizedArguments =>
                (ctorEntry.PipeTokenLocation,
                new Node<ValueConstructor>(
                    ctorNode.Range,
                    new ValueConstructor(
                        Name: ctor.Name,
                        Arguments: [.. canonicalizedArguments]))));
    }

    private static CanonicalizationResult<TypeAlias> CanonicalizeTypeAlias(
        TypeAlias typeAlias,
        CanonicalizationContext context)
    {
        var typeAnnotationResult =
            CanonicalizeTypeAnnotationNode(
                typeAlias.TypeAnnotation,
                context);

        var canonicalizedTypeAlias =
            new TypeAlias(
                Documentation: typeAlias.Documentation,
                TypeTokenLocation: typeAlias.TypeTokenLocation,
                AliasTokenLocation: typeAlias.AliasTokenLocation,
                Name: typeAlias.Name,
                Generics: typeAlias.Generics,
                EqualsTokenLocation: typeAlias.EqualsTokenLocation,
                TypeAnnotation: typeAnnotationResult.Value);

        return
            new CanonicalizationResult<TypeAlias>(
                canonicalizedTypeAlias,
                typeAnnotationResult.Errors,
                typeAnnotationResult.Shadowings);
    }

    private static CanonicalizationResult<Node<TypeAnnotation>> CanonicalizeTypeAnnotationNode(
        Node<TypeAnnotation> typeNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(typeNode, type => CanonicalizeTypeAnnotation(type, context));

    private static CanonicalizationResult<TypeAnnotation> CanonicalizeTypeAnnotation(
        TypeAnnotation type,
        CanonicalizationContext context) =>
        type switch
        {
            TypeAnnotation.GenericType genericType =>
            NoErrors<TypeAnnotation>(genericType), // Generic types don't need canonicalization

            TypeAnnotation.Typed typed =>
            CanonicalizeTypedAnnotation(typed, context)
            .MapValue(t => (TypeAnnotation)t),

            TypeAnnotation.Unit unit =>
            NoErrors<TypeAnnotation>(unit), // Unit type doesn't need canonicalization

            TypeAnnotation.Tupled tupled =>
            CanonicalizationResultExtensions.ConcatMap(
                tupled.TypeAnnotations,
                t => CanonicalizeTypeAnnotationNode(t, context))
            .MapValue(
                canonicalizedNodes =>
                (TypeAnnotation)new TypeAnnotation.Tupled(RebuildSeparated(tupled.TypeAnnotations, [.. canonicalizedNodes]))),

            TypeAnnotation.Record record =>
            CanonicalizeRecordDefinition(
                record.RecordDefinition,
                context)
            .MapValue(recordDef => (TypeAnnotation)new TypeAnnotation.Record(recordDef)),

            TypeAnnotation.GenericRecord genericRecord =>
            CanonicalizeRecordDefinition(
                genericRecord.RecordDefinition.Value,
                context)
            .MapValue(
                recordDef => (TypeAnnotation)new TypeAnnotation.GenericRecord(
                    genericRecord.GenericName,
                    genericRecord.PipeLocation,
                    new Node<RecordDefinition>(
                        genericRecord.RecordDefinition.Range,
                        recordDef))),

            TypeAnnotation.FunctionTypeAnnotation funcType =>
            CanonicalizationResultExtensions.Map2(
                CanonicalizeTypeAnnotationNode(funcType.ArgumentType, context),
                CanonicalizeTypeAnnotationNode(funcType.ReturnType, context),
                (argNode, retNode) => (TypeAnnotation)new TypeAnnotation.FunctionTypeAnnotation(argNode, funcType.ArrowLocation, retNode)),

            _ =>
            throw new NotImplementedException(
                $"Unhandled type annotation in CanonicalizeTypeAnnotation: {type.GetType().Name}")
        };

    private static CanonicalizationResult<TypeAnnotation.Typed> CanonicalizeTypedAnnotation(
        TypeAnnotation.Typed typed,
        CanonicalizationContext context)
    {
        var (moduleName, name) = typed.TypeName.Value;

        // Type annotations don't reference value-level local variables
        var localVariables = ImmutableHashSet<string>.Empty;

        var (resolvedModuleName, resolveErrors) =
            ResolveQualifiedModuleName(
                moduleName,
                name,
                typed.TypeName.Range,
                context.CurrentModuleName,
                context.TypeImportMap,
                context.AliasMap,
                localVariables,
                context.ModuleLevelDeclarations);

        var canonicalizedTypeName =
            new Node<(ModuleName, string)>(
                typed.TypeName.Range,
                (resolvedModuleName, name));

        var typeArgumentResults =
            typed.TypeArguments
            .Select(arg => CanonicalizeTypeAnnotationNode(arg, context))
            .ToList();

        var canonicalizedTypeArguments =
            typeArgumentResults.Select(r => r.Value).ToList();

        var typeArgumentErrors =
            typeArgumentResults.SelectMany(r => r.Errors).ToList();

        var canonicalizedTyped =
            new TypeAnnotation.Typed(
                TypeName: canonicalizedTypeName,
                TypeArguments: canonicalizedTypeArguments);

        var allErrors = resolveErrors.Concat(typeArgumentErrors).ToList();

        return new CanonicalizationResult<TypeAnnotation.Typed>(canonicalizedTyped, allErrors);
    }

    private static CanonicalizationResult<RecordDefinition> CanonicalizeRecordDefinition(
        RecordDefinition recordDef,
        CanonicalizationContext context)
    {
        return
            CanonicalizationResultExtensions.ConcatMap(
                recordDef.Fields,
                field => CanonicalizeRecordFieldNode(field, context))
            .MapValue(
                canonicalizedFields =>
                new RecordDefinition(RebuildSeparated(recordDef.Fields, [.. canonicalizedFields])));
    }

    private static CanonicalizationResult<Node<RecordField>> CanonicalizeRecordFieldNode(
        Node<RecordField> fieldNode,
        CanonicalizationContext context)
    {
        var field = fieldNode.Value;

        var fieldTypeResult =
            CanonicalizeTypeAnnotationNode(
                field.FieldType,
                context);

        var canonicalizedField =
            new Node<RecordField>(
                fieldNode.Range,
                new RecordField(
                    FieldName: field.FieldName,
                    ColonLocation: field.ColonLocation,
                    FieldType: fieldTypeResult.Value));

        return
            new CanonicalizationResult<Node<RecordField>>(
                canonicalizedField,
                fieldTypeResult.Errors,
                fieldTypeResult.Shadowings);
    }

    /// <summary>
    /// Canonicalizes an Elm expression using the configured implicit imports.
    /// </summary>
    /// <param name="expr">The expression to canonicalize.</param>
    /// <param name="implicitImportConfig">The implicit imports available to the expression.</param>
    /// <returns>The canonicalized expression together with any canonicalization errors.</returns>
    public static CanonicalizationResult<SyntaxTypes.Expression> CanonicalizeExpression(
        SyntaxTypes.Expression expr,
        ImplicitImportConfig implicitImportConfig)
    {
        var context =
            new CanonicalizationContext(
                CurrentModuleName: [],
                TypeImportMap: [],
                ValueImportMap: [],
                AliasMap: [],
                ModuleLevelDeclarations: ImmutableHashSet<string>.Empty,
                LocalDeclarations: [],
                OperatorToFunction: ImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)>.Empty,
                DeclarationPath: [])
            .WithDefaults(implicitImportConfig);

        var fakeLocation = new Location(0, 0);
        var fakeRange = new Range(fakeLocation, fakeLocation);

        var exprNode =
            new Node<SyntaxTypes.Expression>(
                Range: fakeRange,
                Value: expr);

        return CanonicalizeExpressionNode(exprNode, context).MapValue(node => node.Value);
    }

    private static CanonicalizationResult<Node<SyntaxTypes.Expression>> CanonicalizeExpressionNode(
        Node<SyntaxTypes.Expression> exprNode,
        CanonicalizationContext context)
    {
        var expr = exprNode.Value;

        var canonicalizedExpr =
            expr switch
            {
                SyntaxTypes.Expression.UnitExpr unitExpr =>
                NoErrors((SyntaxTypes.Expression)unitExpr),

                SyntaxTypes.Expression.Literal literal =>
                NoErrors((SyntaxTypes.Expression)literal),

                SyntaxTypes.Expression.MultilineStringLiteral multilineString =>
                NoErrors((SyntaxTypes.Expression)multilineString),

                SyntaxTypes.Expression.CharLiteral charLiteral =>
                NoErrors((SyntaxTypes.Expression)charLiteral),

                SyntaxTypes.Expression.Integer integer =>
                NoErrors((SyntaxTypes.Expression)integer),

                SyntaxTypes.Expression.FloatLiteral floatable =>
                NoErrors((SyntaxTypes.Expression)floatable),

                SyntaxTypes.Expression.Negation negation =>
                CanonicalizeExpressionNode(negation.Expression, context)
                .MapValue(negExpr => (SyntaxTypes.Expression)new SyntaxTypes.Expression.Negation(negExpr)),

                SyntaxTypes.Expression.ListExpr list =>
                CanonicalizationResultExtensions.ConcatMap(list.Elements, e => CanonicalizeExpressionNode(e, context))
                .MapValue(
                    elements =>
                    (SyntaxTypes.Expression)new SyntaxTypes.Expression.ListExpr(RebuildSeparated(list.Elements, [.. elements]))),

                SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
                CanonicalizeFunctionOrValue(funcOrValue, exprNode.Range, context)
                .MapValue(f => (SyntaxTypes.Expression)f),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                CanonicalizationResultExtensions.Map3(
                    CanonicalizeExpressionNode(ifBlock.Condition, context),
                    CanonicalizeExpressionNode(ifBlock.ThenBlock, context),
                    CanonicalizeExpressionNode(ifBlock.ElseBlock, context),
                    (cond, thenBlock, elseBlock) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.IfBlock(
                        ifBlock.IfTokenLocation,
                        cond,
                        ifBlock.ThenTokenLocation,
                        thenBlock,
                        ifBlock.ElseTokenLocation,
                        elseBlock)),

                SyntaxTypes.Expression.PrefixOperator prefixOperator =>
                NoErrors((SyntaxTypes.Expression)prefixOperator),

                SyntaxTypes.Expression.ParenthesizedExpression parenExpr =>
                CanonicalizeExpressionNode(parenExpr.Expression, context)
                .MapValue(inner => (SyntaxTypes.Expression)new SyntaxTypes.Expression.ParenthesizedExpression(inner)),

                SyntaxTypes.Expression.Application application =>
                CanonicalizationResultExtensions.Map2(
                    CanonicalizeExpressionNode(application.Function, context),
                    CanonicalizationResultExtensions.ConcatMap(
                        application.Arguments,
                        arg => CanonicalizeExpressionNode(arg, context)),
                    (func, args) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.Application(func, [.. args])),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                CanonicalizeOperatorApplication(opApp, context, exprNode.Range),

                SyntaxTypes.Expression.TupledExpression tupled =>
                CanonicalizationResultExtensions.ConcatMap(
                    tupled.Elements,
                    e => CanonicalizeExpressionNode(e, context))
                .MapValue(
                    elements =>
                    (SyntaxTypes.Expression)new SyntaxTypes.Expression.TupledExpression(RebuildSeparated(tupled.Elements, [.. elements]))),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                CanonicalizeLambdaStruct(lambda.Lambda, context)
                .MapValue(
                    lambdaStruct => (SyntaxTypes.Expression)new SyntaxTypes.Expression.LambdaExpression(lambdaStruct)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                CanonicalizeCaseBlock(caseExpr.CaseBlock, context)
                .MapValue(caseBlock => (SyntaxTypes.Expression)new SyntaxTypes.Expression.CaseExpression(caseBlock)),

                SyntaxTypes.Expression.LetExpression letExpr =>
                CanonicalizeLetBlock(letExpr.Value, context)
                .MapValue(letBlock => (SyntaxTypes.Expression)new SyntaxTypes.Expression.LetExpression(letBlock)),

                SyntaxTypes.Expression.RecordExpr record =>
                CanonicalizationResultExtensions.ConcatMap(
                    record.Fields,
                    f => CanonicalizeRecordFieldExpr(f, context))
                .MapValue(
                    fields =>
                    (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordExpr(RebuildSeparated(record.Fields, [.. fields]))),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                CanonicalizeExpressionNode(recordAccess.Record, context)
                .MapValue(
                    record =>
                    (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordAccess(record, recordAccess.FieldName)),

                SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction =>
                NoErrors((SyntaxTypes.Expression)recordAccessFunction),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                CanonicalizationResultExtensions.ConcatMap(
                    recordUpdate.Fields,
                    f => CanonicalizeRecordFieldExpr(f, context))
                .MapValue(
                    fields =>
                    (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordUpdateExpression(
                        recordUpdate.RecordName,
                        recordUpdate.PipeLocation,
                        RebuildSeparated(recordUpdate.Fields, [.. fields]))),

                SyntaxTypes.Expression.GLSLExpression glslExpression =>
                NoErrors((SyntaxTypes.Expression)glslExpression),

                _ =>
                throw new NotImplementedException(
                    $"Unhandled expression type in CanonicalizeExpressionNode: {expr.GetType().Name}")
            };

        return canonicalizedExpr.MapValue(expr => new Node<SyntaxTypes.Expression>(exprNode.Range, expr));
    }

    /// <summary>
    /// Canonicalizes an operator application by converting it to a function application.
    /// For example, "a + b" becomes "Basics.add a b".
    /// </summary>
    private static CanonicalizationResult<SyntaxTypes.Expression> CanonicalizeOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication opApp,
        CanonicalizationContext context,
        Range range)
    {
        // Canonicalize left and right operands
        var leftResult = CanonicalizeExpressionNode(opApp.Left, context);
        var rightResult = CanonicalizeExpressionNode(opApp.Right, context);

        // Look up the operator in the operator-to-function mapping
        if (context.OperatorToFunction.TryGetValue(opApp.Operator.Value, out var funcMapping))
        {
            // Convert operator application to function application: func left right
            var funcOrValue =
                new SyntaxTypes.Expression.FunctionOrValue(
                    ModuleName: funcMapping.ModuleName,
                    Name: funcMapping.FunctionName);

            var funcNode = new Node<SyntaxTypes.Expression>(range, funcOrValue);

            return
                CanonicalizationResultExtensions.Map2(
                    leftResult,
                    rightResult,
                    (left, right) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.Application(funcNode, [left, right]));
        }

        // Also check if the current module declares this operator via its own infix declarations
        if (context.ModuleLevelDeclarations.Contains(opApp.Operator.Value))
        {
            // The operator is declared in this module; resolve using the function name from local scope.
            // This handles the case where a module uses its own infix operators.
            return
                CanonicalizationResultExtensions.Map2(
                    leftResult,
                    rightResult,
                    (left, right) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.OperatorApplication(opApp.Operator, opApp.Direction, left, right));
        }

        // Operator not found in mapping - preserve as OperatorApplication for built-in operator handling
        return
            CanonicalizationResultExtensions.Map2(
                leftResult,
                rightResult,
                (left, right) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.OperatorApplication(opApp.Operator, opApp.Direction, left, right));
    }

    private static CanonicalizationResult<SyntaxTypes.Expression.FunctionOrValue> CanonicalizeFunctionOrValue(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        Range range,
        CanonicalizationContext context)
    {
        var (resolvedModuleName, errors) =
            ResolveQualifiedModuleName(
                funcOrValue.ModuleName,
                funcOrValue.Name,
                range,
                context.CurrentModuleName,
                context.ValueImportMap,
                context.AliasMap,
                context.LocalDeclarations,
                context.ModuleLevelDeclarations);

        var canonicalizedFuncOrValue =
            new SyntaxTypes.Expression.FunctionOrValue(
                ModuleName: resolvedModuleName,
                Name: funcOrValue.Name);

        return new CanonicalizationResult<SyntaxTypes.Expression.FunctionOrValue>(canonicalizedFuncOrValue, errors);
    }

    private static CanonicalizationResult<ModuleName> ResolveModuleName(
        string name,
        Range range,
        ModuleName currentModuleName,
        ImmutableDictionary<string, ImmutableList<ModuleName>> importMap,
        ImmutableHashSet<string> localVariables,
        ImmutableHashSet<string> localDeclarations)
    {
        // Don't resolve local variables - they should keep empty module names
        if (localVariables.Contains(name))
        {
            return new CanonicalizationResult<ModuleName>([], []);
        }

        // Check if it's declared in the current module - module-level declarations shadow imported names
        if (localDeclarations.Contains(name))
        {
            return new CanonicalizationResult<ModuleName>(currentModuleName, []);
        }

        // Check if this name is imported
        if (importMap.TryGetValue(name, out var importedFrom) && importedFrom.Count > 0)
        {
            // Check for import clashes - only report error when the name is actually referenced
            if (importedFrom.Count > 1)
            {
                var moduleNames = importedFrom.Select(m => string.Join(".", m)).ToList();

                return
                    new CanonicalizationResult<ModuleName>(
                        importedFrom[0],
                        [new CanonicalizationError.AmbiguousImport(range, name, moduleNames)]);
            }

            return new CanonicalizationResult<ModuleName>(importedFrom[0], []);
        }

        // Name not found - report an error
        return
            new CanonicalizationResult<ModuleName>(
                currentModuleName,
                [new CanonicalizationError.UnresolvedReference(range, name)]);
    }

    // Common helper to resolve qualified names, handling both aliases and unqualified names
    private static CanonicalizationResult<ModuleName> ResolveQualifiedModuleName(
        ModuleName qualifiedModuleName,
        string name,
        Range range,
        ModuleName currentModuleName,
        ImmutableDictionary<string, ImmutableList<ModuleName>> importMap,
        ImmutableDictionary<string, ModuleName> aliasMap,
        ImmutableHashSet<string> localVariables,
        ImmutableHashSet<string> localDeclarations)
    {
        // If the module name is already specified, check if it's an alias
        if (qualifiedModuleName.Count > 0)
        {
            var moduleNameStr = string.Join(".", qualifiedModuleName);

            if (aliasMap.TryGetValue(moduleNameStr, out var resolvedModuleName))
            {
                return new CanonicalizationResult<ModuleName>(resolvedModuleName, []);
            }

            return new CanonicalizationResult<ModuleName>(qualifiedModuleName, []);
        }

        // Resolve unqualified name
        return ResolveModuleName(name, range, currentModuleName, importMap, localVariables, localDeclarations);
    }

    private static CanonicalizationResult<RecordExprField>
        CanonicalizeRecordFieldExpr(
        RecordExprField field,
        CanonicalizationContext context)
    {
        var exprResult =
            CanonicalizeExpressionNode(
                field.ValueExpr,
                context);

        var canonicalizedField =
            new RecordExprField(
                field.FieldName,
                field.EqualsLocation,
                exprResult.Value);

        return
            new CanonicalizationResult<RecordExprField>(
                canonicalizedField,
                exprResult.Errors,
                exprResult.Shadowings);
    }

    private static CanonicalizationResult<LambdaStruct> CanonicalizeLambdaStruct(
        LambdaStruct lambda,
        CanonicalizationContext context)
    {
        // Extend local variables with lambda parameters
        var extendedLocalDeclarations = context.LocalDeclarations;

        foreach (var arg in lambda.Arguments)
        {
            extendedLocalDeclarations = CollectPatternVariables(arg.Value, extendedLocalDeclarations);
        }

        var contextWithParams = context.WithLocalDeclarations(extendedLocalDeclarations);

        var argumentsResult =
            CanonicalizationResultExtensions.ConcatMap(
                lambda.Arguments,
                arg => CanonicalizePatternNode(arg, contextWithParams));

        var exprResult =
            CanonicalizeExpressionNode(
                lambda.Expression,
                contextWithParams);

        return
            CanonicalizationResultExtensions.Map2(
                argumentsResult,
                exprResult,
                (canonicalizedArguments, canonicalizedExpr) => new LambdaStruct(
                    BackslashLocation: lambda.BackslashLocation,
                    Arguments: [.. canonicalizedArguments],
                    ArrowLocation: lambda.ArrowLocation,
                    Expression: canonicalizedExpr));
    }

    private static CanonicalizationResult<CaseBlock> CanonicalizeCaseBlock(
        CaseBlock caseBlock,
        CanonicalizationContext context)
    {
        var exprResult =
            CanonicalizeExpressionNode(
                caseBlock.Expression,
                context);

        var casesResult =
            CanonicalizationResultExtensions.ConcatMap(
                caseBlock.Cases,
                c => CanonicalizeCase(c, context));

        return
            CanonicalizationResultExtensions.Map2(
                exprResult,
                casesResult,
                (canonicalizedExpr, canonicalizedCases) => new CaseBlock(
                    CaseTokenLocation: caseBlock.CaseTokenLocation,
                    Expression: canonicalizedExpr,
                    OfTokenLocation: caseBlock.OfTokenLocation,
                    Cases: [.. canonicalizedCases]));
    }

    private static CanonicalizationResult<Case> CanonicalizeCase(
        Case caseItem,
        CanonicalizationContext context)
    {
        // Collect pattern variables while checking for shadowing against both
        // module-level declarations and local declarations
        var existingScope = context.ModuleLevelDeclarations.Union(context.LocalDeclarations);

        var (collectedVars, shadowErrors, patternShadowings) =
            CollectPatternVariablesWithShadowCheck(
                caseItem.Pattern.Value,
                caseItem.Pattern.Range,
                existingScope,
                [],
                context.DeclarationPath);

        var extendedLocalDeclarations = context.LocalDeclarations.Union(collectedVars);
        var contextWithPatternVars = context.WithLocalDeclarations(extendedLocalDeclarations);

        var patternResult =
            CanonicalizePatternNode(
                caseItem.Pattern,
                contextWithPatternVars);

        var exprResult =
            CanonicalizeExpressionNode(
                caseItem.Expression,
                contextWithPatternVars);

        var canonicalizedCase =
            new Case(
                Pattern: patternResult.Value,
                ArrowLocation: caseItem.ArrowLocation,
                Expression: exprResult.Value);

        var allErrors = shadowErrors.Concat(patternResult.Errors).Concat(exprResult.Errors).ToList();

        var allShadowings =
            CanonicalizationResult<object>.MergeShadowings(
                CanonicalizationResult<object>.MergeShadowings(patternShadowings, patternResult.Shadowings),
                exprResult.Shadowings);

        return new CanonicalizationResult<Case>(canonicalizedCase, allErrors, allShadowings);
    }

    private static CanonicalizationResult<SyntaxTypes.Expression.LetBlock> CanonicalizeLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        CanonicalizationContext context)
    {
        // Collect let bindings while checking for shadowing against module-level and outer local declarations
        var extendedLocalDeclarations = context.LocalDeclarations;
        var shadowErrors = new List<CanonicalizationError>();
        var letShadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        // The existing scope includes module-level declarations and outer local declarations
        var existingScope = context.ModuleLevelDeclarations.Union(context.LocalDeclarations);

        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                var funcName = letFunc.Function.Declaration.Value.Name.Value;
                var funcNameRange = letFunc.Function.Declaration.Value.Name.Range;

                // Check if let function name shadows existing declarations
                if (existingScope.Contains(funcName))
                {
                    shadowErrors.Add(
                        new CanonicalizationError.NamingClash(
                            funcNameRange,
                            funcName));

                    if (!letShadowings.ContainsKey(funcName))
                    {
                        letShadowings =
                            letShadowings.Add(
                                funcName,
                                new ShadowingLocation(funcNameRange, context.DeclarationPath));
                    }
                }

                extendedLocalDeclarations = extendedLocalDeclarations.Add(funcName);
                existingScope = existingScope.Add(funcName);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                var (collectedVars, errors, destrShadowings) =
                    CollectPatternVariablesWithShadowCheck(
                        letDestr.Pattern.Value,
                        letDestr.Pattern.Range,
                        existingScope,
                        [],
                        context.DeclarationPath);

                shadowErrors.AddRange(errors);
                letShadowings = CanonicalizationResult<object>.MergeShadowings(letShadowings, destrShadowings);
                extendedLocalDeclarations = extendedLocalDeclarations.Union(collectedVars);
                existingScope = existingScope.Union(collectedVars);
            }
        }

        var contextWithLetBindings = context.WithLocalDeclarations(extendedLocalDeclarations);

        var declsResult =
            CanonicalizationResultExtensions.ConcatMap(
                letBlock.Declarations,
                decl => CanonicalizeLetDeclarationNode(decl, contextWithLetBindings));

        var exprResult =
            CanonicalizeExpressionNode(
                letBlock.Expression,
                contextWithLetBindings);

        var canonicalizedLetBlock =
            CanonicalizationResultExtensions.Map2(
                declsResult,
                exprResult,
                (canonicalizedDecls, canonicalizedExpr) => new SyntaxTypes.Expression.LetBlock(
                    LetTokenLocation: letBlock.LetTokenLocation,
                    Declarations: [.. canonicalizedDecls],
                    InTokenLocation: letBlock.InTokenLocation,
                    Expression: canonicalizedExpr));

        // Combine shadow errors with any errors from canonicalization
        var allErrors = shadowErrors.Concat(canonicalizedLetBlock.Errors).ToList();

        var allShadowings =
            CanonicalizationResult<object>.MergeShadowings(letShadowings, canonicalizedLetBlock.Shadowings);

        return new CanonicalizationResult<SyntaxTypes.Expression.LetBlock>(canonicalizedLetBlock.Value, allErrors, allShadowings);
    }

    private static CanonicalizationResult<Node<SyntaxTypes.Expression.LetDeclaration>> CanonicalizeLetDeclarationNode(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(declNode, decl => CanonicalizeLetDeclaration(decl, context));

    private static CanonicalizationResult<SyntaxTypes.Expression.LetDeclaration> CanonicalizeLetDeclaration(
        SyntaxTypes.Expression.LetDeclaration decl,
        CanonicalizationContext context)
    {
        SyntaxTypes.Expression.LetDeclaration canonicalizedDecl;
        IReadOnlyList<CanonicalizationError> errors;
        ImmutableDictionary<string, ShadowingLocation> shadowings;

        switch (decl)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                {
                    var funcResult =
                        CanonicalizeFunctionStruct(letFunc.Function, context);

                    canonicalizedDecl =
                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(funcResult.Value);

                    errors = funcResult.Errors;
                    shadowings = funcResult.Shadowings;
                    break;
                }

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    var patternResult =
                        CanonicalizePatternNode(letDestr.Pattern, context);

                    var exprResult =
                        CanonicalizeExpressionNode(letDestr.Expression, context);

                    canonicalizedDecl =
                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                            patternResult.Value,
                            letDestr.EqualsTokenLocation,
                            exprResult.Value);

                    errors = [.. patternResult.Errors, .. exprResult.Errors];

                    shadowings =
                        CanonicalizationResult<object>.MergeShadowings(
                            patternResult.Shadowings,
                            exprResult.Shadowings);

                    break;
                }

            default:
                throw new NotImplementedException(
                    $"Unhandled let declaration type in CanonicalizeLetDeclaration: {decl.GetType().Name}");
        }

        return
            new CanonicalizationResult<SyntaxTypes.Expression.LetDeclaration>(
                canonicalizedDecl,
                errors,
                shadowings);
    }

    private static CanonicalizationResult<Node<Pattern>> CanonicalizePatternNode(
        Node<Pattern> patternNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(patternNode, pattern => CanonicalizePattern(pattern, patternNode.Range, context));

    private static CanonicalizationResult<Pattern> CanonicalizePattern(
        Pattern pattern,
        Range range,
        CanonicalizationContext context) =>
        pattern switch
        {
            Pattern.AllPattern allPattern =>
            NoErrors<Pattern>(allPattern),

            Pattern.VarPattern varPattern =>
            NoErrors<Pattern>(varPattern),

            Pattern.UnitPattern unitPattern =>
            NoErrors<Pattern>(unitPattern),

            Pattern.CharPattern charPattern =>
            NoErrors<Pattern>(charPattern),

            Pattern.StringPattern stringPattern =>
            NoErrors<Pattern>(stringPattern),

            Pattern.IntPattern intPattern =>
            NoErrors<Pattern>(intPattern),

            Pattern.HexPattern hexPattern =>
            NoErrors<Pattern>(hexPattern),

            Pattern.FloatPattern floatPattern =>
            NoErrors<Pattern>(floatPattern),

            Pattern.TuplePattern tuple =>
            CanonicalizationResultExtensions.ConcatMap(
                tuple.Elements,
                e => CanonicalizePatternNode(e, context))
            .MapValue(
                canonicalizedElems =>
                (Pattern)new Pattern.TuplePattern(
                    RebuildSeparated(tuple.Elements, [.. canonicalizedElems]))),

            Pattern.RecordPattern recordPattern =>
            NoErrors<Pattern>(recordPattern),

            Pattern.UnConsPattern unCons =>
            CanonicalizationResultExtensions.Map2(
                CanonicalizePatternNode(unCons.Head, context),
                CanonicalizePatternNode(unCons.Tail, context),
                (headNode, tailNode) =>
                (Pattern)new Pattern.UnConsPattern(
                    headNode,
                    unCons.ConsOperatorLocation,
                    tailNode)),

            Pattern.ListPattern list =>
            CanonicalizationResultExtensions.ConcatMap(
                list.Elements,
                e => CanonicalizePatternNode(e, context))
            .MapValue(
                canonicalizedElems =>
                (Pattern)new Pattern.ListPattern(
                    RebuildSeparated(list.Elements, [.. canonicalizedElems]))),

            Pattern.NamedPattern named =>
            CanonicalizeNamedPattern(named, range, context)
            .MapValue(np => (Pattern)np),

            Pattern.AsPattern asPattern =>
            CanonicalizePatternNode(asPattern.Pattern, context)
            .MapValue(
                innerNode =>
                (Pattern)new Pattern.AsPattern(
                    innerNode,
                    asPattern.AsTokenLocation,
                    asPattern.Name)),

            Pattern.ParenthesizedPattern parenPattern =>
            CanonicalizePatternNode(parenPattern.Pattern, context)
            .MapValue(innerNode => (Pattern)new Pattern.ParenthesizedPattern(innerNode)),

            _ =>
            throw new NotImplementedException(
                $"Unhandled pattern type in CanonicalizePattern: {pattern.GetType().Name}")
        };

    private static CanonicalizationResult<Pattern.NamedPattern> CanonicalizeNamedPattern(
        Pattern.NamedPattern namedPattern,
        Range range,
        CanonicalizationContext context)
    {
        var qualifiedName = namedPattern.Name;

        var (resolvedModuleName, resolveErrors) =
            ResolveQualifiedModuleName(
                qualifiedName.ModuleName,
                qualifiedName.Name,
                range,
                context.CurrentModuleName,
                context.ValueImportMap,
                context.AliasMap,
                context.LocalDeclarations,
                context.ModuleLevelDeclarations);

        var argumentResults =
            namedPattern.Arguments
            .Select(arg => CanonicalizePatternNode(arg, context))
            .ToList();

        var canonicalizedArguments =
            argumentResults.Select(r => r.Value).ToList();

        var argumentErrors =
            argumentResults.SelectMany(r => r.Errors).ToList();

        var canonicalizedNamedPattern =
            new Pattern.NamedPattern(
                Name: new QualifiedNameRef(
                    ModuleName: resolvedModuleName,
                    Name: qualifiedName.Name),
                Arguments: canonicalizedArguments);

        var allErrors = resolveErrors.Concat(argumentErrors).ToList();

        return
            new CanonicalizationResult<Pattern.NamedPattern>(
                canonicalizedNamedPattern,
                allErrors);
    }

    private static CanonicalizationResult<Node<T>> MapNodeWithErrors<T>(
        Node<T> node,
        Func<T, CanonicalizationResult<T>> mapper)
    {
        var result = mapper(node.Value);

        return
            new CanonicalizationResult<Node<T>>(
                new Node<T>(node.Range, result.Value),
                result.Errors,
                result.Shadowings);
    }

    private static SeparatedSyntaxList<T> RebuildSeparated<T>(
        SeparatedSyntaxList<T> original,
        IReadOnlyList<T> values)
    {
        if (values.Count != original.Count)
        {
            throw new ArgumentException(
                $"Expected {original.Count} values when rebuilding a separated list, but received {values.Count}.",
                nameof(values));
        }

        return
            original switch
            {
                SeparatedSyntaxList<T>.Empty =>
                new SeparatedSyntaxList<T>.Empty(),

                SeparatedSyntaxList<T>.NonEmpty nonEmpty =>
                new SeparatedSyntaxList<T>.NonEmpty(
                    values[0],
                    nonEmpty.Rest
                    .Select((item, index) => (item.SeparatorLocation, values[index + 1]))
                    .ToList()),

                _ =>
                throw new NotImplementedException(
                    "RebuildSeparated does not handle separated-list variant: " + original.GetType().Name)
            };
    }

    /// <summary>
    /// Builds a map of module names to their exposed infix declarations.
    /// </summary>
    private static ImmutableDictionary<string, ImmutableList<(string Operator, string FunctionName)>>
        BuildModuleInfixMap(IReadOnlyList<File> modules)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, ImmutableList<(string, string)>>();

        foreach (var module in modules)
        {
            var moduleName =
                string.Join(".", SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value);

            var infixDecls = ImmutableList.CreateBuilder<(string, string)>();

            foreach (var decl in module.Declarations)
            {
                if (decl.Value is Declaration.InfixDeclaration infixDecl)
                {
                    infixDecls.Add((infixDecl.Infix.Operator.Value, infixDecl.Infix.FunctionName.Value));
                }
            }

            if (infixDecls.Count > 0)
            {
                builder[moduleName] = infixDecls.ToImmutable();
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Collects infix operators from imported modules that expose them (via exposing (..))
    /// and returns a mapping of operator symbol to (module name, function name).
    /// </summary>
    private static ImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)>
        CollectImportedInfixOperators(
        IReadOnlyList<Node<Import>> imports,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap,
        ImmutableDictionary<string, ImmutableList<(string Operator, string FunctionName)>> moduleInfixMap)
    {
        var operatorToFunction =
            ImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)>.Empty.ToBuilder();

        foreach (var importNode in imports)
        {
            var import = importNode.Value;
            var moduleName = import.ModuleName.Value;
            var moduleNameStr = string.Join(".", moduleName);

            if (!moduleInfixMap.TryGetValue(moduleNameStr, out var infixDecls))
                continue;

            if (import.ExposingList is null)
                continue;

            var exposing = import.ExposingList.Value.ExposingList.Value;

            if (exposing is Exposing.All)
            {
                // exposing (..) - import all infix operators from this module
                foreach (var (op, funcName) in infixDecls)
                {
                    operatorToFunction[op] = (moduleName, funcName);
                }
            }
            else if (exposing is Exposing.Explicit explicitExposing)
            {
                // Check if specific operators are explicitly imported
                foreach (var exposeNode in explicitExposing.Nodes)
                {
                    if (exposeNode.Value is TopLevelExpose.InfixExpose infixExpose)
                    {
                        var opName = infixExpose.Name;

                        foreach (var (op, funcName) in infixDecls)
                        {
                            if (op == opName)
                            {
                                operatorToFunction[op] = (moduleName, funcName);
                            }
                        }
                    }
                }
            }
        }

        return operatorToFunction.ToImmutable();
    }
}
