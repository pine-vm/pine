using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;

using File = Pine.Core.Elm.ElmSyntax.SyntaxModel.File;
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Canonicalized declarations and the references that demanded them.</summary>
public sealed record DeclarationCanonicalizationResult(
    IReadOnlyDictionary<DeclQualifiedName, CanonicalizationResult<Node<Declaration>>> Declarations,
    IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>> DependencyChains)
{
    /// <summary>Direct semantic dependencies, including signatures and constructor patterns.</summary>
    public IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>> Dependencies { get; init; } =
        ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>>.Empty;

    /// <summary>Resolved source references, including constructors whose owning declaration has another name.</summary>
    public IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>> References { get; init; } =
        ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>>.Empty;
}

/// <summary>A source reference to a demanded declaration, retaining the referenced symbol and exact location.</summary>
public sealed record DeclarationReference(
    DeclQualifiedName Declaration,
    DeclQualifiedName ReferencedName,
    Range Range,
    bool IsTypeReference);

/// <summary>A reused declaration result includes the dependencies to demand without recanonicalizing its body.</summary>
public sealed record DeclarationCanonicalizationCacheEntry(
    CanonicalizationResult<Node<Declaration>> Result,
    IReadOnlyList<DeclQualifiedName> Dependencies)
{
    /// <summary>Reference provenance must survive a cache hit just as the resolved syntax does.</summary>
    public IReadOnlyList<DeclarationReference> References { get; init; } = [];
}

public partial class Canonicalization
{
    /// <summary>
    /// Resolves complete top-level declarations on demand. Reading a scope does not demand its bodies.
    /// </summary>
    public static Result<string, DeclarationCanonicalizationResult> CanonicalizeDeclarations(
        IReadOnlyList<File> sources,
        IReadOnlyCollection<DeclQualifiedName> roots,
        ImplicitImportConfig? implicitImports = null,
        IDictionary<(Node<Declaration> Declaration, string Scope), DeclarationCanonicalizationCacheEntry>? cache = null)
    {
        if (roots.Count is 0)
        {
            return
                new DeclarationCanonicalizationResult(
                    ImmutableDictionary<DeclQualifiedName, CanonicalizationResult<Node<Declaration>>>.Empty,
                    ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>>.Empty);
        }

        implicitImports ??= ImplicitImportConfig.Default;
        var files = sources.Select(AddEffectModuleStubs).ToList();
        var exports = BuildModuleExportsMap(files);
        var infixes = BuildModuleInfixMap(files);

        var duplicateScopes =
            files
            .GroupBy(file => string.Join(".", Module.GetModuleName(file.ModuleDefinition.Value).Value))
            .Where(group => group.Count() > 1)
            .Select(group => group.Key)
            .ToHashSet(StringComparer.Ordinal);

        var contexts = files.ToDictionary(file => file, file => BuildContext(file, exports, infixes, implicitImports));
        var declarations = new Dictionary<DeclQualifiedName, List<(File file, Node<Declaration> declaration)>>();

        var symbols =
            new Dictionary<(DeclQualifiedName Name, bool Type),
            List<(File file, Node<Declaration> declaration)>>();

        foreach (var file in files)
        {
            var moduleName = Module.GetModuleName(file.ModuleDefinition.Value).Value;

            foreach (var declaration in file.Declarations)
            {
                var name = DeclQualifiedName.Create(moduleName, GetDeclarationName(declaration.Value));

                if (!declarations.TryGetValue(name, out var declarationsWithName))
                    declarations.Add(name, declarationsWithName = []);

                declarationsWithName.Add((file, declaration));

                switch (declaration.Value)
                {
                    case Declaration.FunctionDeclaration:
                    case Declaration.PortDeclaration:
                    case Declaration.InfixDeclaration:
                        Add(name.DeclName, false, declaration);
                        break;

                    case Declaration.AliasDeclaration alias:
                        Add(name.DeclName, true, declaration);

                        if (alias.TypeAlias.TypeAnnotation.Value is TypeAnnotation.Record)
                            Add(name.DeclName, false, declaration);

                        break;

                    case Declaration.ChoiceTypeDeclaration choice:
                        Add(name.DeclName, true, declaration);

                        foreach (var constructor in choice.TypeDeclaration.Constructors)
                            Add(constructor.Value.Name.Value, false, declaration);

                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(CanonicalizeDeclarations)} does not handle declaration variant: {declaration.Value.GetType().Name}");
                }
            }

            void Add(string name, bool type, Node<Declaration> declaration)
            {
                var qualifiedName = DeclQualifiedName.Create(moduleName, name);

                if (!symbols.TryGetValue((qualifiedName, type), out var candidates))
                    symbols.Add((qualifiedName, type), candidates = []);

                candidates.Add((file, declaration));
            }
        }

        var pending = new Queue<DeclQualifiedName>();
        var chains = new Dictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>>();
        var results = new Dictionary<DeclQualifiedName, CanonicalizationResult<Node<Declaration>>>();
        var references = new Dictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>>();
        var sourceReferences = new Dictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>>();

        foreach (var root in roots.Distinct().Order())
        {
            if (!declarations.TryGetValue(root, out var candidates))
                return $"Root declaration '{root.FullName}' was not found.";

            if (candidates.Count is not 1)
                return $"Root declaration '{root.FullName}' is ambiguous between multiple declarations.";

            if (duplicateScopes.Contains(string.Join(".", root.Namespaces)))
                return $"Root declaration '{root.FullName}' belongs to a duplicate module namespace.";

            Demand(root, [root]);
        }

        while (pending.TryDequeue(out var name))
        {
            var (file, declaration) = declarations[name][0];
            var dependencies = new HashSet<DeclQualifiedName>();
            var resolvedReferences = new List<DeclarationReference>();

            var context =
                contexts[file] with
                {
                    ResolveReference = ResolveReference,
                    DescribeUnavailableReference = DescribeUnavailableReference
                };

            var scope = cache is null ? "" : ScopeKey(context);

            if (cache is not null && cache.TryGetValue((declaration, scope), out var cached))
            {
                results[name] = cached.Result;
                references[name] = cached.Dependencies;
                sourceReferences[name] = cached.References;

                foreach (var dependency in cached.Dependencies)
                    Demand(dependency, [.. chains[name], dependency]);
            }
            else
            {
                results[name] = CanonicalizeDeclaration(declaration, context);
                references[name] = [.. dependencies.Order()];
                sourceReferences[name] = resolvedReferences;

                cache?[(declaration, scope)] =
                        new(results[name], references[name]) { References = resolvedReferences };
            }

            IReadOnlyList<CanonicalizationError> ResolveReference(
                IReadOnlyList<string> moduleName, string referencedName, bool typeReference, Range range)
            {
                var reference = DeclQualifiedName.Create(moduleName, referencedName);

                if (IsNativeReference(reference))
                {
                    var nativeModule = string.Join(".", moduleName);

                    var available =
                        nativeModule switch
                        {
                            "Basics" =>
                            symbols.ContainsKey((reference, typeReference)) ||
                            (typeReference
                            ?
                            referencedName is "Int" or "Float" or "Bool" or "Never" or "Order"
                            :
                            implicitImports.ValueImports.TryGetValue(referencedName, out var implicitModule) &&
                            implicitModule.Length is 1 && implicitModule[0] is "Basics" ||
                            CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(referencedName) is not null ||
                            CoreLibraryModule.CoreBasics.GetFunctionValue(referencedName) is not null ||
                            referencedName is "True" or "False" or "LT" or "EQ" or "GT"),

                            "Debug" => !typeReference && referencedName is "log" or "todo" or "toString",
                            "List" => typeReference && referencedName is "List",
                            "Pine_kernel" or "Pine_builtin" => true,

                            _ =>
                            throw new NotImplementedException(
                                $"{nameof(ResolveReference)} does not handle native namespace: {nativeModule}")
                        };

                    return available ? [] : [MissingReference(reference, typeReference, range)];
                }

                if (!symbols.TryGetValue((reference, typeReference), out var candidates))
                    return [MissingReference(reference, typeReference, range)];

                if (candidates.Count is not 1 || duplicateScopes.Contains(string.Join(".", reference.Namespaces)))
                    return [new CanonicalizationError.NamingClash(range, reference.FullName)];

                var candidate = candidates[0];

                var ownerName =
                    DeclQualifiedName.Create(
                        moduleName,
                        GetDeclarationName(candidate.declaration.Value));

                dependencies.Add(ownerName);
                resolvedReferences.Add(new(ownerName, reference, range, typeReference));
                Demand(ownerName, [.. chains[name], ownerName]);
                return [];
            }

            CanonicalizationError.UnresolvedReference MissingReference(
                DeclQualifiedName reference, bool typeReference, Range range) =>
                new(range, reference.FullName)
                {
                    Target = reference,
                    IsTypeReference = typeReference,
                    ResolutionDetail =
                    DescribeUnavailableReference(reference.Namespaces, reference.DeclName, typeReference)
                };

            string DescribeUnavailableReference(IReadOnlyList<string> moduleName, string symbol, bool typeReference)
            {
                var namespaceName = string.Join(".", moduleName);
                var reference = DeclQualifiedName.Create(moduleName, symbol);
                var kind = typeReference ? "type" : "value";

                if (symbols.ContainsKey((reference, typeReference)))
                    return $"Module '{namespaceName}' declares {kind} '{symbol}', but does not expose it. Correct the reference or the declaring module's exposing list.";

                if (symbols.ContainsKey((reference, !typeReference)))
                    return $"'{namespaceName}.{symbol}' exists in the {(typeReference ? "value" : "type")} namespace, not the {kind} namespace.";

                if (!files.Any(source => Module.GetModuleName(source.ModuleDefinition.Value).Value.SequenceEqual(moduleName)) &&
                    namespaceName is not ("Basics" or "Debug" or "Pine_kernel" or "Pine_builtin"))
                    return $"Module '{namespaceName}' is unavailable in the supplied sources. Check the import, declared dependencies, and configured package replacements.";

                return $"The supplied module '{namespaceName}' has no {kind} declaration named '{symbol}'. Check the spelling and the API provided by the selected package or replacement.";
            }

            string ScopeKey(CanonicalizationContext scopeContext)
            {
                var namespaces =
                    file.Imports
                    .Select(import => string.Join(".", import.Value.ModuleName.Value))
                    .Concat(implicitImports.ModuleImports.Select(import => string.Join(".", import.ModuleName)))
                    .Concat(implicitImports.TypeImports.Values.Select(module => string.Join(".", module)))
                    .Concat(implicitImports.ValueImports.Values.Select(module => string.Join(".", module)))
                    .Append(string.Join(".", scopeContext.CurrentModuleName))
                    .ToHashSet(StringComparer.Ordinal);

                return
                    JsonSerializer.Serialize(
                        new
                        {
                            Current = scopeContext.CurrentModuleName,
                            Local = scopeContext.ModuleLevelDeclarations.Order(StringComparer.Ordinal),
                            Types =
                            scopeContext.TypeImportMap.OrderBy(entry => entry.Key, StringComparer.Ordinal)
                            .Select(
                                entry =>
                                new { entry.Key, Modules = entry.Value.Select(module => string.Join(".", module)).Order() }),
                            Values =
                            scopeContext.ValueImportMap.OrderBy(entry => entry.Key, StringComparer.Ordinal)
                            .Select(
                                entry =>
                                new { entry.Key, Modules = entry.Value.Select(module => string.Join(".", module)).Order() }),
                            Aliases =
                            scopeContext.AliasMap.OrderBy(entry => entry.Key, StringComparer.Ordinal)
                            .Select(entry => new { entry.Key, Module = string.Join(".", entry.Value) }),
                            Operators =
                            scopeContext.OperatorToFunction.OrderBy(entry => entry.Key, StringComparer.Ordinal)
                            .Select(
                                entry =>
                                new { entry.Key, Module = string.Join(".", entry.Value.ModuleName), entry.Value.FunctionName }),
                            Candidates =
                            symbols.Where(entry => namespaces.Contains(string.Join(".", entry.Key.Name.Namespaces)))
                            .OrderBy(entry => entry.Key.Name).ThenBy(entry => entry.Key.Type)
                            .Select(
                                entry =>
                                new { Name = entry.Key.Name.FullName, entry.Key.Type, Count = entry.Value.Count }),
                            DuplicateScopes = duplicateScopes.Where(namespaces.Contains).Order(StringComparer.Ordinal),
                            AvailableModules =
                            files.Select(
                                source =>
                                string.Join(".", Module.GetModuleName(source.ModuleDefinition.Value).Value))
                            .Where(namespaces.Contains).Distinct().Order(StringComparer.Ordinal)
                        });
            }
        }

        return
            new DeclarationCanonicalizationResult(results, chains)
            {
                Dependencies = references,
                References = sourceReferences
            };

        void Demand(DeclQualifiedName name, IReadOnlyList<DeclQualifiedName> chain)
        {
            if (chains.TryAdd(name, chain))
                pending.Enqueue(name);
        }
    }

    internal static string GetDeclarationName(Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function => function.Function.Declaration.Value.Name.Value,
            Declaration.AliasDeclaration alias => alias.TypeAlias.Name.Value,
            Declaration.ChoiceTypeDeclaration choice => choice.TypeDeclaration.Name.Value,
            Declaration.InfixDeclaration infix => infix.Infix.Operator.Value,
            Declaration.PortDeclaration port => port.Signature.Name.Value,

            _ =>
            throw new NotImplementedException(
                $"{nameof(GetDeclarationName)} does not handle declaration variant: {declaration.GetType().Name}")
        };

    /// <summary>Checks source imports and exposing clauses without demanding declaration bodies.</summary>
    internal static IReadOnlyList<CanonicalizationError> ValidateImports(File file, IReadOnlyList<File> sources)
    {
        var exports = BuildModuleExportsMap(sources);
        var errors = new List<CanonicalizationError>();

        foreach (var import in file.Imports)
        {
            var moduleName = string.Join(".", import.Value.ModuleName.Value);

            if (!exports.TryGetValue(moduleName, out var available))
            {
                if (moduleName is not ("Basics" or "Debug" or "Pine_kernel" or "Pine_builtin"))
                {
                    errors.Add(
                        new CanonicalizationError.UnresolvedReference(import.Value.ModuleName.Range, moduleName));
                }

                continue;
            }

            if (import.Value.ExposingList?.ExposingList.Value is Exposing.Explicit exposed)
                CheckExposing(exposed, available, moduleName);
        }

        var currentModule = Module.GetModuleName(file.ModuleDefinition.Value).Value;

        var localExports =
            BuildModuleExportsMap(
                [
                file with
                {
                    ModuleDefinition =
                    file.ModuleDefinition with
                    {
                        Value =
                        file.ModuleDefinition.Value switch
                        {
                            Module.NormalModule normal =>
                            normal with
                            {
                                ModuleData =
                                normal.ModuleData with
                                {
                                    ExposingList =
                                    normal.ModuleData.ExposingList with { Value = new Exposing.All(normal.ModuleData.ExposingList.Range) }
                                }
                            },

                            Module.PortModule port =>
                            port with
                            {
                                ModuleData =
                                port.ModuleData with
                                {
                                    ExposingList =
                                    port.ModuleData.ExposingList with { Value = new Exposing.All(port.ModuleData.ExposingList.Range) }
                                }
                            },

                            Module.EffectModule effect =>
                            effect with
                            {
                                ModuleData =
                                effect.ModuleData with
                                {
                                    ExposingList =
                                    effect.ModuleData.ExposingList with { Value = new Exposing.All(effect.ModuleData.ExposingList.Range) }
                                }
                            },

                            _ =>
                            throw new NotImplementedException(
                                $"{nameof(ValidateImports)} does not handle module variant: {file.ModuleDefinition.Value.GetType().Name}")
                        }
                    }
                }
                ]);

        var exposing =
            file.ModuleDefinition.Value switch
            {
                Module.NormalModule normal => normal.ModuleData.ExposingList.Value,
                Module.PortModule port => port.ModuleData.ExposingList.Value,
                Module.EffectModule effect => effect.ModuleData.ExposingList.Value,

                _ =>
                throw new NotImplementedException(
                    $"{nameof(ValidateImports)} does not handle module variant: {file.ModuleDefinition.Value.GetType().Name}")
            };

        if (exposing is Exposing.Explicit ownExports)
            CheckExposing(ownExports, localExports[string.Join(".", currentModule)], string.Join(".", currentModule));

        return errors;

        void CheckExposing(Exposing.Explicit exposed, ModuleExports available, string moduleName)
        {
            foreach (var node in exposed.Nodes)
            {
                var valid =
                    node.Value switch
                    {
                        TopLevelExpose.FunctionExpose function => available.ValueExports.Contains(function.Name),
                        TopLevelExpose.InfixExpose infix => available.ValueExports.Contains(infix.Name),
                        TopLevelExpose.TypeOrAliasExpose type => available.TypeExports.Contains(type.Name),

                        TopLevelExpose.TypeExpose type =>
                        available.TypeExports.Contains(type.ExposedType.Name) &&
                        (type.ExposedType.Open is null || available.TypeConstructors.ContainsKey(type.ExposedType.Name)),

                        _ =>
                        throw new NotImplementedException(
                            $"{nameof(CheckExposing)} does not handle exposing variant: {node.Value.GetType().Name}")
                    };

                if (!valid)
                {
                    var name =
                        node.Value switch
                        {
                            TopLevelExpose.FunctionExpose function => function.Name,
                            TopLevelExpose.InfixExpose infix => infix.Name,
                            TopLevelExpose.TypeOrAliasExpose type => type.Name,
                            TopLevelExpose.TypeExpose type => type.ExposedType.Name,

                            _ =>
                            throw new NotImplementedException(
                                $"{nameof(CheckExposing)} does not handle exposing variant: {node.Value.GetType().Name}")
                        };

                    errors.Add(new CanonicalizationError.UnresolvedReference(node.Range, moduleName + "." + name));
                }
            }
        }
    }

    private static bool IsNativeReference(DeclQualifiedName name) =>
        string.Join(".", name.Namespaces) is "Basics" or "Debug" or "Pine_kernel" or "Pine_builtin" ||
        name.FullName is "List.List";
}
