using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

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
    /// <param name="AllExports">Set of all exported names from the module.</param>
    /// <param name="TypeConstructors">Mapping from type names to their associated value constructors.</param>
    private record ModuleExports(
        ImmutableHashSet<string> AllExports,
        ImmutableDictionary<string, ImmutableList<string>> TypeConstructors);

    /// <summary>
    /// Encapsulates the context required for canonicalization, including imports, aliases, and declaration scopes.
    /// </summary>
    /// <param name="CurrentModuleName">The name of the module being canonicalized.</param>
    /// <param name="ImportMap">Map of imported names to their source modules.</param>
    /// <param name="AliasMap">Map of module aliases to their actual module names.</param>
    /// <param name="ModuleLevelDeclarations">Top-level declarations in the current module (functions, types, constructors).</param>
    /// <param name="LocalDeclarations">Local variable bindings from patterns, function parameters, let expressions, etc.</param>
    private record CanonicalizationContext(
        ModuleName CurrentModuleName,
        ImmutableDictionary<string, ImmutableList<ModuleName>> ImportMap,
        ImmutableDictionary<string, ModuleName> AliasMap,
        ImmutableHashSet<string> ModuleLevelDeclarations,
        ImmutableHashSet<string> LocalDeclarations)
    {
        /// <summary>
        /// Creates a new context with additional local declarations added.
        /// Used when entering scopes that introduce new bindings (function parameters, let expressions, patterns).
        /// </summary>
        /// <param name="additionalDeclarations">Names to add to the local declarations set.</param>
        /// <returns>A new context with the combined local declarations.</returns>
        public CanonicalizationContext WithLocalDeclarations(ImmutableHashSet<string> additionalDeclarations) =>
            this with { LocalDeclarations = LocalDeclarations.Union(additionalDeclarations) };

        /// <summary>
        /// Creates a new context with a single additional local declaration added.
        /// </summary>
        /// <param name="name">Name to add to the local declarations set.</param>
        /// <returns>A new context with the added local declaration.</returns>
        public CanonicalizationContext WithLocalDeclaration(string name) =>
            this with { LocalDeclarations = LocalDeclarations.Add(name) };
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
            .Select(err => $"Cannot find '{err.ReferencedName}'")
            .ToList();

        return string.Join("\n", errorMessages);
    }

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
    public static Result<string, IReadOnlyDictionary<ModuleName, Result<string, SyntaxTypes.File>>> Canonicalize(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        // Check for duplicate module names
        var moduleNameGroups =
            modules
            .GroupBy(m => SyntaxTypes.Module.GetModuleName(m.ModuleDefinition.Value).Value, EnumerableExtensions.EqualityComparer<ModuleName>())
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

            return Result<string, IReadOnlyDictionary<ModuleName, Result<string, SyntaxTypes.File>>>.err(
                $"Duplicate module names: {string.Join(", ", duplicateNames)}");
        }

        // Build module exports map for resolving exposing (..)
        var moduleExportsMap = BuildModuleExportsMap(modules);

        var resultDictionary = new Dictionary<ModuleName, Result<string, SyntaxTypes.File>>(
            EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var module in modules)
        {
            var currentModuleName =
                SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            // Build import map and alias map for this module
            var (importMap, aliasMap) = BuildImportMaps(module.Imports, moduleExportsMap);

            // Build set of module-level declarations (top-level names declared in this module)
            var moduleLevelDeclarations = BuildLocalDeclarations(module);

            // Check for clashing imports
            var clashErrors = DetectImportClashes(importMap);

            if (clashErrors.Any())
            {
                resultDictionary[currentModuleName] = string.Join("\n", clashErrors);

                continue;
            }

            // Create canonicalization context for this module
            var context = new CanonicalizationContext(
                CurrentModuleName: currentModuleName,
                ImportMap: importMap,
                AliasMap: aliasMap,
                ModuleLevelDeclarations: moduleLevelDeclarations,
                LocalDeclarations: ImmutableHashSet<string>.Empty);

            // Canonicalize declarations and collect errors
            var canonicalizedDeclarationsWithErrors =
                module.Declarations
                .Select(decl => CanonicalizeDeclaration(decl, context))
                .ToList();

            // Aggregate all errors from declarations
            var allErrors =
                canonicalizedDeclarationsWithErrors
                .SelectMany(result => result.Errors)
                .ToList();

            // Extract canonicalized declarations
            var canonicalizedDeclarations =
                canonicalizedDeclarationsWithErrors
                .Select(result => result.Value)
                .ToList();

            // Create new file with empty imports
            var canonicalizedModule =
                module
                with
                {
                    Declarations = canonicalizedDeclarations,
                    Imports = []
                };

            // If there are errors, return error result for this module
            if (allErrors.Count > 0)
            {
                resultDictionary[currentModuleName] = RenderErrors(allErrors);
            }
            else
            {
                resultDictionary[currentModuleName] = Result<string, SyntaxTypes.File>.ok(canonicalizedModule);
            }
        }

        return resultDictionary;
    }

    private static ImmutableHashSet<string> BuildLocalDeclarations(SyntaxTypes.File module)
    {
        var localDeclarationsBuilder = ImmutableHashSet.CreateBuilder<string>();

        foreach (var decl in module.Declarations)
        {
            var declName =
                decl.Value switch
                {
                    SyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
                    funcDecl.Function.Declaration.Value.Name.Value,

                    SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
                    typeDecl.TypeDeclaration.Name.Value,

                    SyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
                    aliasDecl.TypeAlias.Name.Value,

                    SyntaxTypes.Declaration.InfixDeclaration infixDecl =>
                    infixDecl.Infix.Operator.Value,

                    SyntaxTypes.Declaration.PortDeclaration portDecl =>
                    portDecl.Signature.Name.Value,

                    _ =>
                    throw new NotImplementedException(
                        $"Unhandled declaration type in BuildLocalDeclarations: {decl.Value.GetType().Name}")
                };

            localDeclarationsBuilder.Add(declName);

            // Also add type constructors for custom types
            if (decl.Value is SyntaxTypes.Declaration.CustomTypeDeclaration customTypeDecl)
            {
                foreach (var ctor in customTypeDecl.TypeDeclaration.Constructors)
                {
                    localDeclarationsBuilder.Add(ctor.Value.Name.Value);
                }
            }
        }

        return localDeclarationsBuilder.ToImmutable();
    }

    private static ImmutableDictionary<string, ModuleExports> BuildModuleExportsMap(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var exportsMapBuilder = ImmutableDictionary.CreateBuilder<string, ModuleExports>();

        foreach (var module in modules)
        {
            var moduleName =
                string.Join(".", SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value);

            var exportsBuilder = ImmutableHashSet.CreateBuilder<string>();

            var typeConstructorsBuilder = ImmutableDictionary.CreateBuilder<string, ImmutableList<string>>();

            // Get the exposing list from the module
            var exposingList =
                module.ModuleDefinition.Value switch
                {
                    SyntaxTypes.Module.NormalModule normalModule =>
                    normalModule.ModuleData.ExposingList.Value,

                    SyntaxTypes.Module.PortModule portModule =>
                    portModule.ModuleData.ExposingList.Value,

                    SyntaxTypes.Module.EffectModule effectModule =>
                    effectModule.ModuleData.ExposingList.Value,

                    _ =>
                    throw new NotImplementedException(
                        $"Unhandled module type: {module.ModuleDefinition.Value.GetType().Name}")
                };

            // Check if the module exposes all (..)
            var exposesAll = exposingList is SyntaxTypes.Exposing.All;

            // Build a map of all declarations for filtering
            var allDeclarations = new Dictionary<string, SyntaxTypes.Declaration>();

            foreach (var decl in module.Declarations)
            {
                var declName =
                    decl.Value switch
                    {
                        SyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
                        funcDecl.Function.Declaration.Value.Name.Value,

                        SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
                        typeDecl.TypeDeclaration.Name.Value,

                        SyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
                        aliasDecl.TypeAlias.Name.Value,

                        SyntaxTypes.Declaration.InfixDeclaration infixDecl =>
                        infixDecl.Infix.Operator.Value,

                        SyntaxTypes.Declaration.PortDeclaration portDecl =>
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
                        case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                            exportsBuilder.Add(funcDecl.Function.Declaration.Value.Name.Value);
                            break;

                        case SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl:
                            var typeName = typeDecl.TypeDeclaration.Name.Value;
                            exportsBuilder.Add(typeName);

                            var constructorsBuilder = ImmutableList.CreateBuilder<string>();

                            foreach (var ctor in typeDecl.TypeDeclaration.Constructors)
                            {
                                var ctorName = ctor.Value.Name.Value;
                                exportsBuilder.Add(ctorName);
                                constructorsBuilder.Add(ctorName);
                            }

                            typeConstructorsBuilder[typeName] = constructorsBuilder.ToImmutable();
                            break;

                        case SyntaxTypes.Declaration.AliasDeclaration aliasDecl:
                            exportsBuilder.Add(aliasDecl.TypeAlias.Name.Value);
                            break;

                        case SyntaxTypes.Declaration.InfixDeclaration infixDecl:
                            exportsBuilder.Add(infixDecl.Infix.Operator.Value);
                            break;

                        case SyntaxTypes.Declaration.PortDeclaration portDecl:
                            exportsBuilder.Add(portDecl.Signature.Name.Value);
                            break;

                        default:
                            throw new NotImplementedException(
                                $"Unhandled declaration type in BuildModuleExportsMap: {decl.Value.GetType().Name}");
                    }
                }
            }
            else if (exposingList is SyntaxTypes.Exposing.Explicit explicitExposing)
            {
                // Only export what's explicitly listed
                foreach (var exposeNode in explicitExposing.Nodes)
                {
                    var expose = exposeNode.Value;

                    switch (expose)
                    {
                        case SyntaxTypes.TopLevelExpose.InfixExpose infixExpose:
                            exportsBuilder.Add(infixExpose.Name);
                            break;

                        case SyntaxTypes.TopLevelExpose.FunctionExpose funcExpose:
                            exportsBuilder.Add(funcExpose.Name);
                            break;

                        case SyntaxTypes.TopLevelExpose.TypeOrAliasExpose typeOrAlias:
                            exportsBuilder.Add(typeOrAlias.Name);
                            break;

                        case SyntaxTypes.TopLevelExpose.TypeExpose typeExpose:
                            var exposedTypeName = typeExpose.ExposedType.Name;
                            exportsBuilder.Add(exposedTypeName);

                            // If exposing constructors (..), add them
                            if (typeExpose.ExposedType.Open is not null &&
                                allDeclarations.TryGetValue(exposedTypeName, out var typeDecl) &&
                                typeDecl is SyntaxTypes.Declaration.CustomTypeDeclaration customTypeDecl)
                            {
                                var constructorsBuilder = ImmutableList.CreateBuilder<string>();
                                foreach (var ctor in customTypeDecl.TypeDeclaration.Constructors)
                                {
                                    var ctorName = ctor.Value.Name.Value;
                                    exportsBuilder.Add(ctorName);
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
                new ModuleExports(exportsBuilder.ToImmutable(), typeConstructorsBuilder.ToImmutable());
        }

        return exportsMapBuilder.ToImmutable();
    }

    private static (ImmutableDictionary<string, ImmutableList<ModuleName>>, ImmutableDictionary<string, ModuleName>) BuildImportMaps(
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Import>> imports,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        var importMap =
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
                var alias = string.Join(".", importModuleAlias.Value);
                aliasMap[alias] = moduleName;
            }

            // Get exposed items
            if (import.ExposingList is null)
            {
                continue;
            }

            var exposing = import.ExposingList.Value;

            if (exposing is SyntaxTypes.Exposing.All)
            {
                // Handle 'exposing (..)' 
                if (moduleExportsMap.TryGetValue(moduleNameStr, out var moduleExports))
                {
                    foreach (var exportedName in moduleExports.AllExports)
                    {
                        if (!importMap.TryGetValue(exportedName, out var value))
                        {
                            value = [];
                            importMap[exportedName] = value;
                        }
                        importMap[exportedName] = value.Add(moduleName);
                    }
                }
                continue;
            }

            if (exposing is SyntaxTypes.Exposing.Explicit explicitExposing)
            {
                foreach (var exposeNode in explicitExposing.Nodes)
                {
                    var expose = exposeNode.Value;
                    var names = GetExposedNames(expose, moduleNameStr, moduleExportsMap);

                    foreach (var name in names)
                    {
                        if (!importMap.TryGetValue(name, out var value))
                        {
                            value = [];
                            importMap[name] = value;
                        }

                        importMap[name] = value.Add(moduleName);
                    }
                }
            }
        }

        // Add core type mappings
        var importMapWithCoreTypes = AddCoreTypeMappings(importMap.ToImmutable());

        return (importMapWithCoreTypes, aliasMap.ToImmutable());
    }

    private static ImmutableDictionary<string, ImmutableList<ModuleName>> AddCoreTypeMappings(
        ImmutableDictionary<string, ImmutableList<ModuleName>> importMap)
    {
        var coreTypes = ImmutableDictionary.CreateRange(new Dictionary<string, ModuleName>
        {
            // Basics module types
            ["Int"] = ["Basics"],
            ["Float"] = ["Basics"],
            ["Bool"] = ["Basics"],
            ["Never"] = ["Basics"],

            // Basics module value constructors
            ["True"] = ["Basics"],
            ["False"] = ["Basics"],

            // String module
            ["Char"] = ["Char"],
            ["String"] = ["String"],

            // Standard library types
            ["List"] = ["List"],
            ["Maybe"] = ["Maybe"],
            ["Result"] = ["Result"],
            ["Order"] = ["Basics"],

            // Platform types
            ["Program"] = ["Platform"],
            ["Task"] = ["Task"],
            ["Cmd"] = ["Platform", "Cmd"],
            ["Sub"] = ["Platform", "Sub"]
        });

        var result = importMap;

        foreach (var (typeName, moduleName) in coreTypes)
        {
            if (!result.ContainsKey(typeName))
            {
                result = result.Add(typeName, [moduleName]);
            }
        }

        return result;
    }

    private static IEnumerable<string> GetExposedNames(
        SyntaxTypes.TopLevelExpose expose,
        string moduleName,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        return expose switch
        {
            SyntaxTypes.TopLevelExpose.InfixExpose infixExpose =>
                [infixExpose.Name],

            SyntaxTypes.TopLevelExpose.FunctionExpose functionExpose =>
                [functionExpose.Name],

            SyntaxTypes.TopLevelExpose.TypeOrAliasExpose typeOrAliasExpose =>
                [typeOrAliasExpose.Name],

            SyntaxTypes.TopLevelExpose.TypeExpose typeExpose =>
                // Handle Status(..) syntax - expose type name and all constructors
                GetTypeExposeNames(typeExpose, moduleName, moduleExportsMap),

            _ => throw new NotImplementedException(
                $"Unhandled TopLevelExpose type in GetExposedNames: {expose.GetType().Name}")
        };
    }

    private static IEnumerable<string> GetTypeExposeNames(
        SyntaxTypes.TopLevelExpose.TypeExpose typeExpose,
        string moduleName,
        ImmutableDictionary<string, ModuleExports> moduleExportsMap)
    {
        var typeName = typeExpose.ExposedType.Name;

        // Always include the type name itself
        yield return typeName;

        // If Open is not null, it means we have Status(..) syntax - expose all constructors of this type
        if (typeExpose.ExposedType.Open is not null &&
            moduleExportsMap.TryGetValue(moduleName, out var moduleExports) &&
            moduleExports.TypeConstructors.TryGetValue(typeName, out var constructors))
        {
            // Yield only the constructors that belong to this specific type
            foreach (var constructor in constructors)
            {
                yield return constructor;
            }
        }
    }

    private static ModuleName DetectImportClashes(
        ImmutableDictionary<string, ImmutableList<ModuleName>> importMap)
    {
        var errors = new List<string>();

        foreach (var (name, sources) in importMap)
        {
            if (sources.Count > 1)
            {
                var moduleNames =
                    sources.Select(m => string.Join(".", m));

                errors.Add(
                    $"Name '{name}' is exposed by multiple imports: {string.Join(", ", moduleNames)}");
            }
        }

        return errors;
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.Declaration>> CanonicalizeDeclaration(
        SyntaxTypes.Node<SyntaxTypes.Declaration> declNode,
        CanonicalizationContext context)
    {
        var decl = declNode.Value;

        // Canonicalize based on declaration type
        SyntaxTypes.Declaration canonicalizedDecl;
        IReadOnlyList<CanonicalizationError> errors;

        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var funcResult =
                        CanonicalizeFunctionStruct(
                            funcDecl.Function,
                            context);

                    canonicalizedDecl = new SyntaxTypes.Declaration.FunctionDeclaration(funcResult.Value);

                    errors = funcResult.Errors;
                    break;
                }

            case SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl:
                {
                    var typeResult =
                        CanonicalizeTypeStruct(
                            typeDecl.TypeDeclaration,
                            context);

                    canonicalizedDecl = new SyntaxTypes.Declaration.CustomTypeDeclaration(typeResult.Value);

                    errors = typeResult.Errors;
                    break;
                }

            case SyntaxTypes.Declaration.AliasDeclaration aliasDecl:
                {
                    var aliasResult =
                        CanonicalizeTypeAlias(
                            aliasDecl.TypeAlias,
                            context);

                    canonicalizedDecl = new SyntaxTypes.Declaration.AliasDeclaration(aliasResult.Value);

                    errors = aliasResult.Errors;
                    break;
                }

            case SyntaxTypes.Declaration.PortDeclaration portDecl:
                {
                    var signatureResult =
                        CanonicalizeSignature(
                            portDecl.Signature,
                            context);

                    canonicalizedDecl = new SyntaxTypes.Declaration.PortDeclaration(signatureResult.Value);

                    errors = signatureResult.Errors;
                    break;
                }

            case SyntaxTypes.Declaration.InfixDeclaration:
                canonicalizedDecl = decl; // No canonicalization needed for infix declarations
                errors = [];
                break;

            default:
                throw new NotImplementedException(
                    $"Unhandled declaration type in CanonicalizeDeclaration: {decl.GetType().Name}");
        }

        var canonicalizedNode =
            new SyntaxTypes.Node<SyntaxTypes.Declaration>(
                declNode.Range,
                canonicalizedDecl);

        return new CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.Declaration>>(
            canonicalizedNode,
            errors);
    }

    private static CanonicalizationResult<SyntaxTypes.FunctionStruct> CanonicalizeFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        CanonicalizationContext context)
    {
        var signatureResult =
            func.Signature is null
            ? NoErrors<SyntaxTypes.Node<SyntaxTypes.Signature>?>(null)
            : CanonicalizeSignature(func.Signature.Value, context)
                .MapValue(sig => (SyntaxTypes.Node<SyntaxTypes.Signature>?)new SyntaxTypes.Node<SyntaxTypes.Signature>(
                    func.Signature.Range,
                    sig));

        var implResult =
            CanonicalizeFunctionImplementation(
                func.Declaration.Value,
                context);

        return CanonicalizationResultExtensions.Map2(
            signatureResult,
            implResult,
            (canonicalizedSignature, canonicalizedImpl) => new SyntaxTypes.FunctionStruct(
                Documentation: func.Documentation,
                Signature: canonicalizedSignature,
                Declaration: new SyntaxTypes.Node<SyntaxTypes.FunctionImplementation>(
                    func.Declaration.Range,
                    canonicalizedImpl)));
    }

    private static CanonicalizationResult<SyntaxTypes.FunctionImplementation> CanonicalizeFunctionImplementation(
        SyntaxTypes.FunctionImplementation impl,
        CanonicalizationContext context)
    {
        // Collect parameter variables
        var parameterVariables = ImmutableHashSet<string>.Empty;

        foreach (var arg in impl.Arguments)
        {
            parameterVariables = CollectPatternVariables(arg.Value, parameterVariables);
        }

        // Create new context with parameter variables added to local declarations
        var contextWithParams = context.WithLocalDeclarations(parameterVariables);

        var argumentResults = impl.Arguments
            .Select(arg => CanonicalizePatternNode(arg, contextWithParams))
            .ToList();

        var canonicalizedArguments =
            argumentResults.Select(r => r.Value).ToList();

        var argumentErrors =
            argumentResults.SelectMany(r => r.Errors).ToList();

        var exprResult =
            CanonicalizeExpressionNode(
                impl.Expression,
                contextWithParams);

        var functionImplementation =
            new SyntaxTypes.FunctionImplementation(
                Name: impl.Name,
                Arguments: canonicalizedArguments,
                Expression: exprResult.Value);

        var allErrors =
            argumentErrors.Concat(exprResult.Errors).ToList();

        return new CanonicalizationResult<SyntaxTypes.FunctionImplementation>(functionImplementation, allErrors);
    }

    private static ImmutableHashSet<string> CollectPatternVariables(
        SyntaxTypes.Pattern pattern,
        ImmutableHashSet<string> variables)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.AllPattern:
                // Matches anything, no variables to collect
                return variables;

            case SyntaxTypes.Pattern.VarPattern varPattern:
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

            case SyntaxTypes.Pattern.TuplePattern tuple:
                {
                    var result = variables;
                    foreach (var elem in tuple.Elements)
                    {
                        result = CollectPatternVariables(elem.Value, result);
                    }
                    return result;
                }

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                return variables.Union(recordPattern.Fields.Select(f => f.Value));

            case SyntaxTypes.Pattern.UnConsPattern unCons:
                {
                    var result = CollectPatternVariables(unCons.Head.Value, variables);
                    return CollectPatternVariables(unCons.Tail.Value, result);
                }

            case SyntaxTypes.Pattern.ListPattern list:
                {
                    var result = variables;

                    foreach (var elem in list.Elements)
                    {
                        result = CollectPatternVariables(elem.Value, result);
                    }

                    return result;
                }

            case SyntaxTypes.Pattern.NamedPattern named:
                {
                    var result = variables;

                    foreach (var arg in named.Arguments)
                    {
                        result = CollectPatternVariables(arg.Value, result);
                    }

                    return result;
                }

            case SyntaxTypes.Pattern.AsPattern asPattern:
                {
                    var result =
                        CollectPatternVariables(asPattern.Pattern.Value, variables);

                    return result.Add(asPattern.Name.Value);
                }

            case SyntaxTypes.Pattern.ParenthesizedPattern parenPattern:
                return CollectPatternVariables(parenPattern.Pattern.Value, variables);

            default:
                throw new NotImplementedException(
                    $"Unhandled pattern type in CollectPatternVariables: {pattern.GetType().Name}");
        }
    }

    private static CanonicalizationResult<SyntaxTypes.Signature> CanonicalizeSignature(
        SyntaxTypes.Signature signature,
        CanonicalizationContext context)
    {
        var typeAnnotationResult =
            CanonicalizeTypeAnnotationNode(
                signature.TypeAnnotation,
                context);

        var canonicalizedSignature =
            new SyntaxTypes.Signature(
                Name: signature.Name,
                TypeAnnotation: typeAnnotationResult.Value);

        return new CanonicalizationResult<SyntaxTypes.Signature>(canonicalizedSignature, typeAnnotationResult.Errors);
    }

    private static CanonicalizationResult<SyntaxTypes.TypeStruct> CanonicalizeTypeStruct(
        SyntaxTypes.TypeStruct typeStruct,
        CanonicalizationContext context)
    {
        return CanonicalizationResultExtensions.ConcatMap(typeStruct.Constructors, ctor => CanonicalizeValueConstructorNode(ctor, context))
            .MapValue(canonicalizedConstructors => new SyntaxTypes.TypeStruct(
                Documentation: typeStruct.Documentation,
                Name: typeStruct.Name,
                Generics: typeStruct.Generics,
                Constructors: [.. canonicalizedConstructors]));
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.ValueConstructor>> CanonicalizeValueConstructorNode(
        SyntaxTypes.Node<SyntaxTypes.ValueConstructor> ctorNode,
        CanonicalizationContext context)
    {
        var ctor = ctorNode.Value;

        return CanonicalizationResultExtensions.ConcatMap(ctor.Arguments, arg => CanonicalizeTypeAnnotationNode(arg, context))
            .MapValue(canonicalizedArguments => new SyntaxTypes.Node<SyntaxTypes.ValueConstructor>(
                ctorNode.Range,
                new SyntaxTypes.ValueConstructor(
                    Name: ctor.Name,
                    Arguments: [.. canonicalizedArguments])));
    }

    private static CanonicalizationResult<SyntaxTypes.TypeAlias> CanonicalizeTypeAlias(
        SyntaxTypes.TypeAlias typeAlias,
        CanonicalizationContext context)
    {
        var typeAnnotationResult =
            CanonicalizeTypeAnnotationNode(
                typeAlias.TypeAnnotation,
                context);

        var canonicalizedTypeAlias =
            new SyntaxTypes.TypeAlias(
                Documentation: typeAlias.Documentation,
                Name: typeAlias.Name,
                Generics: typeAlias.Generics,
                TypeAnnotation: typeAnnotationResult.Value);

        return new CanonicalizationResult<SyntaxTypes.TypeAlias>(canonicalizedTypeAlias, typeAnnotationResult.Errors);
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.TypeAnnotation>> CanonicalizeTypeAnnotationNode(
        SyntaxTypes.Node<SyntaxTypes.TypeAnnotation> typeNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(typeNode, type => CanonicalizeTypeAnnotation(type, context));

    private static CanonicalizationResult<SyntaxTypes.TypeAnnotation> CanonicalizeTypeAnnotation(
        SyntaxTypes.TypeAnnotation type,
        CanonicalizationContext context) =>
        type switch
        {
            SyntaxTypes.TypeAnnotation.GenericType genericType =>
                NoErrors<SyntaxTypes.TypeAnnotation>(genericType), // Generic types don't need canonicalization

            SyntaxTypes.TypeAnnotation.Typed typed =>
                CanonicalizeTypedAnnotation(typed, context)
                .MapValue(t => (SyntaxTypes.TypeAnnotation)t),

            SyntaxTypes.TypeAnnotation.Unit unit =>
                NoErrors<SyntaxTypes.TypeAnnotation>(unit), // Unit type doesn't need canonicalization

            SyntaxTypes.TypeAnnotation.Tupled tupled =>
                CanonicalizationResultExtensions.ConcatMap(tupled.TypeAnnotations, t => CanonicalizeTypeAnnotationNode(t, context))
                .MapValue(canonicalizedNodes =>
                    (SyntaxTypes.TypeAnnotation)new SyntaxTypes.TypeAnnotation.Tupled([.. canonicalizedNodes])),

            SyntaxTypes.TypeAnnotation.Record record =>
                CanonicalizeRecordDefinition(
                    record.RecordDefinition,
                    context)
                .MapValue(recordDef => (SyntaxTypes.TypeAnnotation)new SyntaxTypes.TypeAnnotation.Record(recordDef)),

            SyntaxTypes.TypeAnnotation.GenericRecord genericRecord =>
                CanonicalizeRecordDefinition(
                    genericRecord.RecordDefinition.Value,
                    context)
                .MapValue(recordDef => (SyntaxTypes.TypeAnnotation)new SyntaxTypes.TypeAnnotation.GenericRecord(
                    genericRecord.GenericName,
                    new SyntaxTypes.Node<SyntaxTypes.RecordDefinition>(
                        genericRecord.RecordDefinition.Range,
                        recordDef))),

            SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType =>
                CanonicalizationResultExtensions.Map2(
                    CanonicalizeTypeAnnotationNode(funcType.ArgumentType, context),
                    CanonicalizeTypeAnnotationNode(funcType.ReturnType, context),
                    (argNode, retNode) => (SyntaxTypes.TypeAnnotation)new SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation(argNode, retNode)),

            _ => throw new NotImplementedException(
                $"Unhandled type annotation in CanonicalizeTypeAnnotation: {type.GetType().Name}")
        };

    private static CanonicalizationResult<SyntaxTypes.TypeAnnotation.Typed> CanonicalizeTypedAnnotation(
        SyntaxTypes.TypeAnnotation.Typed typed,
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
                context.ImportMap,
                context.AliasMap,
                localVariables,
                context.ModuleLevelDeclarations);

        var canonicalizedTypeName =
            new SyntaxTypes.Node<(ModuleName, string)>(
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
            new SyntaxTypes.TypeAnnotation.Typed(
                TypeName: canonicalizedTypeName,
                TypeArguments: canonicalizedTypeArguments);

        var allErrors = resolveErrors.Concat(typeArgumentErrors).ToList();

        return new CanonicalizationResult<SyntaxTypes.TypeAnnotation.Typed>(canonicalizedTyped, allErrors);
    }

    private static CanonicalizationResult<SyntaxTypes.RecordDefinition> CanonicalizeRecordDefinition(
        SyntaxTypes.RecordDefinition recordDef,
        CanonicalizationContext context)
    {
        return
            CanonicalizationResultExtensions.ConcatMap(recordDef.Fields, field => CanonicalizeRecordFieldNode(field, context))
            .MapValue(canonicalizedFields => new SyntaxTypes.RecordDefinition([.. canonicalizedFields]));
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.RecordField>> CanonicalizeRecordFieldNode(
        SyntaxTypes.Node<SyntaxTypes.RecordField> fieldNode,
        CanonicalizationContext context)
    {
        var field = fieldNode.Value;

        var fieldTypeResult =
            CanonicalizeTypeAnnotationNode(
                field.FieldType,
                context);

        var canonicalizedField =
            new SyntaxTypes.Node<SyntaxTypes.RecordField>(
                fieldNode.Range,
                new SyntaxTypes.RecordField(
                    FieldName: field.FieldName,
                    FieldType: fieldTypeResult.Value));

        return new CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.RecordField>>(
            canonicalizedField,
            fieldTypeResult.Errors);
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.Expression>> CanonicalizeExpressionNode(
        SyntaxTypes.Node<SyntaxTypes.Expression> exprNode,
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

                SyntaxTypes.Expression.CharLiteral charLiteral =>
                    NoErrors((SyntaxTypes.Expression)charLiteral),

                SyntaxTypes.Expression.Integer integer =>
                    NoErrors((SyntaxTypes.Expression)integer),

                SyntaxTypes.Expression.Hex hex =>
                    NoErrors((SyntaxTypes.Expression)hex),

                SyntaxTypes.Expression.Floatable floatable =>
                    NoErrors((SyntaxTypes.Expression)floatable),

                SyntaxTypes.Expression.Negation negation =>
                    CanonicalizeExpressionNode(negation.Expression, context)
                        .MapValue(negExpr => (SyntaxTypes.Expression)new SyntaxTypes.Expression.Negation(negExpr)),

                SyntaxTypes.Expression.ListExpr list =>
                    CanonicalizationResultExtensions.ConcatMap(list.Elements, e => CanonicalizeExpressionNode(e, context))
                        .MapValue(elements => (SyntaxTypes.Expression)new SyntaxTypes.Expression.ListExpr([.. elements])),

                SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
                    CanonicalizeFunctionOrValue(funcOrValue, exprNode.Range, context)
                        .MapValue(f => (SyntaxTypes.Expression)f),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                    CanonicalizationResultExtensions.Map3(
                        CanonicalizeExpressionNode(ifBlock.Condition, context),
                        CanonicalizeExpressionNode(ifBlock.ThenBlock, context),
                        CanonicalizeExpressionNode(ifBlock.ElseBlock, context),
                        (cond, thenBlock, elseBlock) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.IfBlock(cond, thenBlock, elseBlock)),

                SyntaxTypes.Expression.PrefixOperator prefixOperator =>
                    NoErrors((SyntaxTypes.Expression)prefixOperator),

                SyntaxTypes.Expression.ParenthesizedExpression parenExpr =>
                    CanonicalizeExpressionNode(parenExpr.Expression, context)
                        .MapValue(inner => (SyntaxTypes.Expression)new SyntaxTypes.Expression.ParenthesizedExpression(inner)),

                SyntaxTypes.Expression.Application application =>
                    CanonicalizationResultExtensions.ConcatMap(
                        application.Arguments,
                        arg => CanonicalizeExpressionNode(arg, context))
                        .MapValue(args => (SyntaxTypes.Expression)new SyntaxTypes.Expression.Application([.. args])),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                    CanonicalizationResultExtensions.Map2(
                        CanonicalizeExpressionNode(opApp.Left, context),
                        CanonicalizeExpressionNode(opApp.Right, context),
                        (left, right) => (SyntaxTypes.Expression)new SyntaxTypes.Expression.OperatorApplication(opApp.Operator, opApp.Direction, left, right)),

                SyntaxTypes.Expression.TupledExpression tupled =>
                    CanonicalizationResultExtensions.ConcatMap(
                        tupled.Elements,
                        e => CanonicalizeExpressionNode(e, context))
                        .MapValue(elements => (SyntaxTypes.Expression)new SyntaxTypes.Expression.TupledExpression([.. elements])),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                    CanonicalizeLambdaStruct(lambda.Lambda, context)
                        .MapValue(lambdaStruct => (SyntaxTypes.Expression)new SyntaxTypes.Expression.LambdaExpression(lambdaStruct)),

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
                        .MapValue(fields => (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordExpr([.. fields])),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                    CanonicalizeExpressionNode(recordAccess.Record, context)
                        .MapValue(record => (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordAccess(record, recordAccess.FieldName)),

                SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction =>
                    NoErrors((SyntaxTypes.Expression)recordAccessFunction),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                    CanonicalizationResultExtensions.ConcatMap(recordUpdate.Fields, f => CanonicalizeRecordFieldExpr(f, context))
                        .MapValue(fields => (SyntaxTypes.Expression)new SyntaxTypes.Expression.RecordUpdateExpression(recordUpdate.RecordName, [.. fields])),

                _ =>
                throw new NotImplementedException(
                    $"Unhandled expression type in CanonicalizeExpressionNode: {expr.GetType().Name}")
            };

        return canonicalizedExpr.MapValue(expr => new SyntaxTypes.Node<SyntaxTypes.Expression>(exprNode.Range, expr));
    }

    private static CanonicalizationResult<SyntaxTypes.Expression.FunctionOrValue> CanonicalizeFunctionOrValue(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        SyntaxTypes.Range range,
        CanonicalizationContext context)
    {
        var (resolvedModuleName, errors) =
            ResolveQualifiedModuleName(
                funcOrValue.ModuleName,
                funcOrValue.Name,
                range,
                context.CurrentModuleName,
                context.ImportMap,
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
        SyntaxTypes.Range range,
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

        // Check if this name is imported
        if (importMap.TryGetValue(name, out var importedFrom) && importedFrom.Count > 0)
        {
            return new CanonicalizationResult<ModuleName>(importedFrom[0], []);
        }

        // Check if it's declared in the current module
        if (localDeclarations.Contains(name))
        {
            return new CanonicalizationResult<ModuleName>(currentModuleName, []);
        }

        // Name not found - report an error
        return new CanonicalizationResult<ModuleName>(
            currentModuleName,
            [new CanonicalizationError(range, name)]);
    }

    // Common helper to resolve qualified names, handling both aliases and unqualified names
    private static CanonicalizationResult<ModuleName> ResolveQualifiedModuleName(
        ModuleName qualifiedModuleName,
        string name,
        SyntaxTypes.Range range,
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

    private static CanonicalizationResult<SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)>>
        CanonicalizeRecordFieldExpr(
            SyntaxTypes.Node<(SyntaxTypes.Node<string> fieldName, SyntaxTypes.Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
            CanonicalizationContext context)
    {
        var (fieldName, valueExpr) = fieldNode.Value;

        var exprResult =
            CanonicalizeExpressionNode(
                valueExpr,
                context);

        var canonicalizedField =
            new SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)>(
                fieldNode.Range,
                (fieldName, exprResult.Value));

        return new CanonicalizationResult<SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)>>(
            canonicalizedField,
            exprResult.Errors);
    }

    private static CanonicalizationResult<SyntaxTypes.LambdaStruct> CanonicalizeLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
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

        return CanonicalizationResultExtensions.Map2(
            argumentsResult,
            exprResult,
            (canonicalizedArguments, canonicalizedExpr) => new SyntaxTypes.LambdaStruct(
                Arguments: [.. canonicalizedArguments],
                Expression: canonicalizedExpr));
    }

    private static CanonicalizationResult<SyntaxTypes.CaseBlock> CanonicalizeCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
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

        return CanonicalizationResultExtensions.Map2(
            exprResult,
            casesResult,
            (canonicalizedExpr, canonicalizedCases) => new SyntaxTypes.CaseBlock(
                Expression: canonicalizedExpr,
                Cases: [.. canonicalizedCases]));
    }

    private static CanonicalizationResult<SyntaxTypes.Case> CanonicalizeCase(
        SyntaxTypes.Case caseItem,
        CanonicalizationContext context)
    {
        // Extend local variables with pattern bindings
        var extendedLocalDeclarations =
            CollectPatternVariables(caseItem.Pattern.Value, context.LocalDeclarations);

        var contextWithPatternVars = context.WithLocalDeclarations(extendedLocalDeclarations);

        var patternResult =
            CanonicalizePatternNode(
                caseItem.Pattern,
                contextWithPatternVars);

        var exprResult =
            CanonicalizeExpressionNode(
                caseItem.Expression,
                contextWithPatternVars);

        var canonicalizedCase = new SyntaxTypes.Case(
            Pattern: patternResult.Value,
            Expression: exprResult.Value);

        var allErrors = patternResult.Errors.Concat(exprResult.Errors).ToList();
        return new CanonicalizationResult<SyntaxTypes.Case>(canonicalizedCase, allErrors);
    }

    private static CanonicalizationResult<SyntaxTypes.Expression.LetBlock> CanonicalizeLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        CanonicalizationContext context)
    {
        // Extend local variables with let bindings
        var extendedLocalDeclarations = context.LocalDeclarations;
        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                extendedLocalDeclarations = extendedLocalDeclarations.Add(letFunc.Function.Declaration.Value.Name.Value);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                extendedLocalDeclarations = CollectPatternVariables(letDestr.Pattern.Value, extendedLocalDeclarations);
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

        return CanonicalizationResultExtensions.Map2(
            declsResult,
            exprResult,
            (canonicalizedDecls, canonicalizedExpr) => new SyntaxTypes.Expression.LetBlock(
                Declarations: [.. canonicalizedDecls],
                Expression: canonicalizedExpr));
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration>> CanonicalizeLetDeclarationNode(
        SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(declNode, decl => CanonicalizeLetDeclaration(decl, context));

    private static CanonicalizationResult<SyntaxTypes.Expression.LetDeclaration> CanonicalizeLetDeclaration(
        SyntaxTypes.Expression.LetDeclaration decl,
        CanonicalizationContext context)
    {
        SyntaxTypes.Expression.LetDeclaration canonicalizedDecl;
        IReadOnlyList<CanonicalizationError> errors;

        switch (decl)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                {
                    var funcResult =
                        CanonicalizeFunctionStruct(letFunc.Function, context);

                    canonicalizedDecl =
                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(funcResult.Value);

                    errors = funcResult.Errors;
                    break;
                }

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    var patternResult =
                        CanonicalizePatternNode(letDestr.Pattern, context);

                    var exprResult =
                        CanonicalizeExpressionNode(letDestr.Expression, context);

                    canonicalizedDecl =
                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(patternResult.Value, exprResult.Value);

                    errors = [.. patternResult.Errors, .. exprResult.Errors];
                    break;
                }

            default:
                throw new NotImplementedException(
                    $"Unhandled let declaration type in CanonicalizeLetDeclaration: {decl.GetType().Name}");
        }

        return new CanonicalizationResult<SyntaxTypes.Expression.LetDeclaration>(canonicalizedDecl, errors);
    }

    private static CanonicalizationResult<SyntaxTypes.Node<SyntaxTypes.Pattern>> CanonicalizePatternNode(
        SyntaxTypes.Node<SyntaxTypes.Pattern> patternNode,
        CanonicalizationContext context) =>
        MapNodeWithErrors(patternNode, pattern => CanonicalizePattern(pattern, patternNode.Range, context));

    private static CanonicalizationResult<SyntaxTypes.Pattern> CanonicalizePattern(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.Range range,
        CanonicalizationContext context) =>
        pattern switch
        {
            SyntaxTypes.Pattern.AllPattern allPattern =>
                NoErrors<SyntaxTypes.Pattern>(allPattern),

            SyntaxTypes.Pattern.VarPattern varPattern =>
                NoErrors<SyntaxTypes.Pattern>(varPattern),

            SyntaxTypes.Pattern.UnitPattern unitPattern =>
                NoErrors<SyntaxTypes.Pattern>(unitPattern),

            SyntaxTypes.Pattern.CharPattern charPattern =>
                NoErrors<SyntaxTypes.Pattern>(charPattern),

            SyntaxTypes.Pattern.StringPattern stringPattern =>
                NoErrors<SyntaxTypes.Pattern>(stringPattern),

            SyntaxTypes.Pattern.IntPattern intPattern =>
                NoErrors<SyntaxTypes.Pattern>(intPattern),

            SyntaxTypes.Pattern.HexPattern hexPattern =>
                NoErrors<SyntaxTypes.Pattern>(hexPattern),

            SyntaxTypes.Pattern.FloatPattern floatPattern =>
                NoErrors<SyntaxTypes.Pattern>(floatPattern),

            SyntaxTypes.Pattern.TuplePattern tuple =>
                CanonicalizationResultExtensions.ConcatMap(
                    tuple.Elements,
                    e => CanonicalizePatternNode(e, context))
                .MapValue(canonicalizedElems =>
                    (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.TuplePattern([.. canonicalizedElems])),

            SyntaxTypes.Pattern.RecordPattern recordPattern =>
                NoErrors<SyntaxTypes.Pattern>(recordPattern),

            SyntaxTypes.Pattern.UnConsPattern unCons =>
            CanonicalizationResultExtensions.Map2(
                CanonicalizePatternNode(unCons.Head, context),
                CanonicalizePatternNode(unCons.Tail, context),
                (headNode, tailNode) =>
                (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.UnConsPattern(headNode, tailNode)),

            SyntaxTypes.Pattern.ListPattern list =>
                CanonicalizationResultExtensions.ConcatMap(
                    list.Elements,
                    e => CanonicalizePatternNode(e, context))
                .MapValue(canonicalizedElems =>
                    (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.ListPattern([.. canonicalizedElems])),

            SyntaxTypes.Pattern.NamedPattern named =>
                CanonicalizeNamedPattern(named, range, context)
                .MapValue(np => (SyntaxTypes.Pattern)np),

            SyntaxTypes.Pattern.AsPattern asPattern =>
                CanonicalizePatternNode(asPattern.Pattern, context)
                .MapValue(innerNode => (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.AsPattern(innerNode, asPattern.Name)),

            SyntaxTypes.Pattern.ParenthesizedPattern parenPattern =>
                CanonicalizePatternNode(parenPattern.Pattern, context)
                .MapValue(innerNode => (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.ParenthesizedPattern(innerNode)),

            _ => throw new NotImplementedException(
                $"Unhandled pattern type in CanonicalizePattern: {pattern.GetType().Name}")
        };

    private static CanonicalizationResult<SyntaxTypes.Pattern.NamedPattern> CanonicalizeNamedPattern(
        SyntaxTypes.Pattern.NamedPattern namedPattern,
        SyntaxTypes.Range range,
        CanonicalizationContext context)
    {
        var qualifiedName = namedPattern.Name;

        var (resolvedModuleName, resolveErrors) =
            ResolveQualifiedModuleName(
                qualifiedName.ModuleName,
                qualifiedName.Name,
                range,
                context.CurrentModuleName,
                context.ImportMap,
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
            new SyntaxTypes.Pattern.NamedPattern(
                Name: new SyntaxTypes.QualifiedNameRef(
                    ModuleName: resolvedModuleName,
                    Name: qualifiedName.Name),
                Arguments: canonicalizedArguments);

        var allErrors = resolveErrors.Concat(argumentErrors).ToList();

        return new CanonicalizationResult<SyntaxTypes.Pattern.NamedPattern>(
            canonicalizedNamedPattern,
            allErrors);
    }

    private static CanonicalizationResult<SyntaxTypes.Node<T>> MapNodeWithErrors<T>(
        SyntaxTypes.Node<T> node,
        Func<T, CanonicalizationResult<T>> mapper)
    {
        var result = mapper(node.Value);

        return new CanonicalizationResult<SyntaxTypes.Node<T>>(
            new SyntaxTypes.Node<T>(node.Range, result.Value),
            result.Errors);
    }
}
