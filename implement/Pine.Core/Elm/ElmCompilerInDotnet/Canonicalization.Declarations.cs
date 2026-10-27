using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using File = Pine.Core.Elm.Syntax.Concrete.File;
using Range = Pine.Core.Elm.Syntax.Concrete.Range;

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

    /// <summary>Signatures without executable bodies, including annotated effect and unavailable-kernel APIs.</summary>
    public ImmutableDictionary<string, TypeInference.InferredType> TypeOnlyFunctionSignatures { get; init; } = [];
}

/// <summary>A source reference to a demanded declaration, retaining the referenced symbol and exact location.</summary>
public sealed record DeclarationReference(
    DeclQualifiedName Declaration,
    DeclQualifiedName ReferencedName,
    Range Range,
    bool IsTypeReference)
{
    /// <summary>The reference resolves to type information, not an executable declaration.</summary>
    public bool IsTypeOnly { get; init; }
}

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
        DeclarationCache? cache = null,
        IReadOnlyDictionary<string, ElmModuleTypeInterface>? moduleTypeInterfaces = null,
        bool useSourceModuleTypeInterfaces = true)
    {
        if (roots.Count is 0)
        {
            return
                new DeclarationCanonicalizationResult(
                    ImmutableDictionary<DeclQualifiedName, CanonicalizationResult<Node<Declaration>>>.Empty,
                    ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>>.Empty);
        }

        implicitImports ??= ImplicitImportConfig.Default;
        moduleTypeInterfaces ??= ElmModuleTypeInterface.Standard;
        var files = sources.ToList();
        var sourceExports = BuildModuleExportsMap(files);
        var exports = AddTypeInterfaceExports(sourceExports, moduleTypeInterfaces);
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

        var native = new SymbolResolution.Native();
        var typeOnly = new SymbolResolution.TypeOnly(new([]));
        var ambiguous = new SymbolResolution.Ambiguous();

        var missing =
            Enum.GetValues<SymbolAvailability>().ToDictionary(
                availability => availability,
                availability => new SymbolResolution.Missing(availability));

        var resolvedSymbols =
            symbols.ToDictionary(
                entry => entry.Key,
                entry =>
                entry.Value.Count is not 1 || duplicateScopes.Contains(string.Join(".", entry.Key.Name.Namespaces))
                ?
                (SymbolResolution)ambiguous
                :
                new SymbolResolution.Declaration(
                    DeclQualifiedName.Create(
                        entry.Key.Name.Namespaces,
                        GetDeclarationName(entry.Value[0].declaration.Value))));

        var moduleNames = files.Select(file => Module.GetModuleName(file.ModuleDefinition.Value).Value).ToArray();

        var sourceSignatures = new Dictionary<DeclQualifiedName, TypeOnlySignature>();
        var effectOperations = new Dictionary<DeclQualifiedName, TypeOnlySignature>();

        foreach (var file in files)
        {
            if (file.ModuleDefinition.Value is not Module.EffectModule effect)
                continue;

            var moduleName = Module.GetModuleName(file.ModuleDefinition.Value).Value;

            if (duplicateScopes.Contains(string.Join(".", moduleName)))
                continue;

            AddOperation("command", effect.ModuleData.Command, ["Platform", "Cmd"], "Cmd");
            AddOperation("subscription", effect.ModuleData.Subscription, ["Platform", "Sub"], "Sub");

            void AddOperation(string name, Node<string>? payloadType, string[] resultModule, string resultType)
            {
                if (payloadType is null)
                    return;

                var message = new TypeInference.InferredType.TypeVariable("msg");
                var payload = DeclQualifiedName.Create(moduleName, payloadType.Value);

                effectOperations.Add(
                    DeclQualifiedName.Create(moduleName, name),
                    new(
                        new TypeInference.InferredType.FunctionType(
                            new TypeInference.InferredType.ChoiceType(moduleName, payloadType.Value, [message]),
                            new TypeInference.InferredType.ChoiceType(resultModule, resultType, [message])),
                        declarations.ContainsKey(payload) ? [payload] : []));
            }
        }

        foreach (var file in files)
        {
            contexts[file] =
                contexts[file] with { LookupSymbol = LookupSymbol, LookupAvailability = LookupAvailability };
        }

        foreach (var file in files.Where(
            file =>
            useSourceModuleTypeInterfaces &&
            (file.ModuleDefinition.Value is Module.EffectModule ||
            file.Imports.Any(
                import =>
                import.Value.ModuleName.Value is ["Elm", "Kernel", ..] or ["PineUnavailable", _, "Elm", "Kernel", ..] &&
                !moduleNames.Any(module => ModuleNameComparer.Equal(module, import.Value.ModuleName.Value))))))
        {
            var moduleName = Module.GetModuleName(file.ModuleDefinition.Value).Value;

            if (duplicateScopes.Contains(string.Join(".", moduleName)))
                continue;

            var publicValues = sourceExports[string.Join(".", moduleName)].ValueExports;

            foreach (var declaration in file.Declarations)
            {
                if (declaration.Value is not Declaration.FunctionDeclaration
                    { Function.Signature: { } signature } function ||
                    !publicValues.Contains(function.Function.Declaration.Value.Name.Value))
                {
                    continue;
                }

                var dependencies = new HashSet<DeclQualifiedName>();
                var context = contexts[file] with { ResolveReference = ResolveSignatureReference };
                var annotation = CanonicalizeTypeAnnotationNode(signature.Value.TypeAnnotation, context);

                if (annotation.Errors.Count is 0)
                {
                    sourceSignatures.Add(
                        DeclQualifiedName.Create(moduleName, function.Function.Declaration.Value.Name.Value),
                        new(
                            TypeInference.TypeAnnotationToInferredType(
                                Syntax.Abstract.ConvertFromConcrete.FromTypeAnnotation(annotation.Value.Value)),
                            [.. dependencies.Order()]));
                }

                IReadOnlyList<CanonicalizationError> ResolveSignatureReference(
                    IReadOnlyList<string> module, string symbol, bool typeReference, Range range)
                {
                    var reference = DeclQualifiedName.Create(module, symbol);

                    var resolution = LookupSymbol(reference, typeReference);

                    switch (resolution)
                    {
                        case SymbolResolution.Native:
                        case SymbolResolution.TypeOnly:
                            return [];

                        case SymbolResolution.Declaration resolved:
                            dependencies.Add(resolved.Owner);
                            return [];

                        case SymbolResolution.Ambiguous:
                            return [new CanonicalizationError.NamingClash(range, reference.FullName)];

                        case SymbolResolution.Missing:
                            return [new CanonicalizationError.UnresolvedReference(range, reference.FullName)];

                        default:
                            throw new NotImplementedException(
                                $"{nameof(ResolveSignatureReference)} does not handle symbol resolution: {resolution.GetType().Name}");
                    }
                }
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
            var context = contexts[file];

            if (cache is not null && cache.TryGet(declaration, context, out var cached))
            {
                results[name] = cached.Result;
                references[name] = cached.Dependencies;
                sourceReferences[name] = cached.References;

                foreach (var dependency in cached.Dependencies)
                    Demand(dependency, [.. chains[name], dependency]);

                continue;
            }

            var dependencies = new HashSet<DeclQualifiedName>();
            var resolvedReferences = new List<DeclarationReference>();
            var inputs = cache is null ? null : new InputObservations();

            context =
                context with
                {
                    ResolveReference = ResolveReference,
                    DescribeUnavailableReference = DescribeUnavailableReference,
                    Inputs = inputs
                };

            results[name] = CanonicalizeDeclaration(declaration, context);
            references[name] = [.. dependencies.Order()];
            sourceReferences[name] = [.. resolvedReferences];

            cache?.Add(declaration, context,
                new(results[name], references[name]) { References = sourceReferences[name] }, inputs!);

            IReadOnlyList<CanonicalizationError> ResolveReference(
                IReadOnlyList<string> moduleName, string referencedName, bool typeReference, Range range)
            {
                var reference = DeclQualifiedName.Create(moduleName, referencedName);

                var resolution = LookupSymbol(reference, typeReference);

                if (inputs is not null && !IsIntrinsicReference(reference, typeReference))
                    inputs.Record(new SymbolInput(reference, typeReference, resolution));

                switch (resolution)
                {
                    case SymbolResolution.Native:
                        return [];

                    case SymbolResolution.TypeOnly typeSignature:
                        resolvedReferences.Add(new(reference, reference, range, typeReference) { IsTypeOnly = true });

                        foreach (var dependency in typeSignature.Dependencies)
                        {
                            dependencies.Add(dependency);
                            Demand(dependency, [.. chains[name], dependency]);
                        }

                        return [];

                    case SymbolResolution.Missing:
                        return [MissingReference(reference, typeReference, range)];

                    case SymbolResolution.Ambiguous:
                        return [new CanonicalizationError.NamingClash(range, reference.FullName)];

                    case SymbolResolution.Declaration resolved:
                        dependencies.Add(resolved.Owner);
                        resolvedReferences.Add(new(resolved.Owner, reference, range, typeReference));
                        Demand(resolved.Owner, [.. chains[name], resolved.Owner]);
                        return [];

                    default:
                        throw new NotImplementedException(
                            $"{nameof(ResolveReference)} does not handle resolution variant: {resolution.GetType().Name}");
                }
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

                var availability = LookupAvailability(reference, typeReference);
                inputs?.Record(new AvailabilityInput(reference, typeReference, availability));

                if (availability is SymbolAvailability.Declared)
                    return $"Module '{namespaceName}' declares {kind} '{symbol}', but does not expose it. Correct the reference or the declaring module's exposing list.";

                if (availability is SymbolAvailability.OtherNamespace)
                    return $"'{namespaceName}.{symbol}' exists in the {(typeReference ? "value" : "type")} namespace, not the {kind} namespace.";

                if (availability is SymbolAvailability.MissingModule)
                {
                    return $"Module '{namespaceName}' is unavailable in the supplied sources. Check the import, declared dependencies, and configured package replacements.";
                }

                return $"The supplied module '{namespaceName}' has no {kind} declaration named '{symbol}'. Check the spelling and the API provided by the selected package or replacement.";
            }

        }

        return
            new DeclarationCanonicalizationResult(results, chains)
            {
                Dependencies = references,
                References = sourceReferences,
                TypeOnlyFunctionSignatures =
                ElmModuleTypeInterface.FunctionSignatures(moduleTypeInterfaces).SetItems(
                    sourceSignatures.Concat(effectOperations).Select(
                        entry => KeyValuePair.Create(entry.Key.FullName, entry.Value.Type)))
            };

        void Demand(DeclQualifiedName name, IReadOnlyList<DeclQualifiedName> chain)
        {
            if (chains.TryAdd(name, chain))
                pending.Enqueue(name);
        }

        SymbolAvailability LookupAvailability(DeclQualifiedName reference, bool typeReference)
        {
            if (symbols.ContainsKey((reference, typeReference)))
                return SymbolAvailability.Declared;

            if (symbols.ContainsKey((reference, !typeReference)))
                return SymbolAvailability.OtherNamespace;

            if (moduleTypeInterfaces.TryGetValue(string.Join(".", reference.Namespaces), out var moduleInterface))
            {
                if (typeReference
                ?
                moduleInterface.Values.ContainsKey(reference.DeclName)
                :
                moduleInterface.Types.ContainsKey(reference.DeclName))
                {
                    return SymbolAvailability.OtherNamespace;
                }

                return SymbolAvailability.MissingName;
            }

            if (reference.Namespaces.Count is 1 &&
                reference.Namespaces[0] is "Basics" or "Debug" or "Pine_kernel" or "Pine_builtin")
            {
                return SymbolAvailability.MissingName;
            }

            foreach (var moduleName in moduleNames)
            {
                if (ModuleNameComparer.Equal(moduleName, reference.Namespaces))
                    return SymbolAvailability.MissingName;
            }

            return SymbolAvailability.MissingModule;
        }

        SymbolResolution LookupSymbol(DeclQualifiedName reference, bool typeReference)
        {
            if (!IsNativeReference(reference))
            {
                var moduleName = string.Join(".", reference.Namespaces);

                if (!typeReference && !duplicateScopes.Contains(moduleName) &&
                    (sourceSignatures.TryGetValue(reference, out var signature) ||
                    effectOperations.TryGetValue(reference, out signature)))
                {
                    return new SymbolResolution.TypeOnly(new(signature.Dependencies));
                }

                if (!duplicateScopes.Contains(moduleName) &&
                    moduleTypeInterfaces.TryGetValue(moduleName, out var moduleInterface) &&
                    (typeReference
                    ?
                    moduleInterface.Types.ContainsKey(reference.DeclName)
                    :
                    moduleInterface.Values.ContainsKey(reference.DeclName)) &&
                    (!resolvedSymbols.ContainsKey((reference, typeReference)) ||
                    !sourceExports.TryGetValue(moduleName, out var implementedExports) ||
                    !(typeReference ? implementedExports.TypeExports : implementedExports.ValueExports).Contains(reference.DeclName)))
                {
                    return typeOnly;
                }

                return
                    resolvedSymbols.TryGetValue((reference, typeReference), out var resolved)
                    ?
                    resolved
                    :
                    missing[LookupAvailability(reference, typeReference)];
            }

            var available =
                IsIntrinsicReference(reference, typeReference) ||
                reference.Namespaces[0] is "Basics" &&
                (symbols.ContainsKey((reference, typeReference)) ||
                !typeReference &&
                implicitImports.ValueImports.TryGetValue(reference.DeclName, out var implicitModule) &&
                implicitModule.Length is 1 && implicitModule[0] is "Basics");

            return available ? native : missing[LookupAvailability(reference, typeReference)];
        }

        bool IsIntrinsicReference(DeclQualifiedName reference, bool typeReference)
        {
            if (!IsNativeReference(reference))
                return false;

            return reference.Namespaces[0] switch
            {
                "Basics" =>
                typeReference
                ?
                reference.DeclName is "Int" or "Float" or "Bool" or "Never" or "Order"
                :
                CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(reference.DeclName) is not null ||
                CoreLibraryModule.CoreBasics.GetFunctionValue(reference.DeclName) is not null ||
                reference.DeclName is "True" or "False" or "LT" or "EQ" or "GT",

                "Debug" => !typeReference && reference.DeclName is "log" or "todo" or "toString",
                "List" => typeReference && reference.DeclName is "List",
                "Pine_kernel" or "Pine_builtin" => true,

                _ =>
                throw new NotImplementedException($"Unexpected native namespace: {reference.Namespaces[0]}")
            };
        }
    }

    private sealed record TypeOnlySignature(
        TypeInference.InferredType Type,
        IReadOnlyList<DeclQualifiedName> Dependencies);

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
    internal static IReadOnlyList<CanonicalizationError> ValidateImports(
        File file,
        IReadOnlyList<File> sources,
        IReadOnlyDictionary<string, ElmModuleTypeInterface>? moduleTypeInterfaces = null)
    {
        var exports =
            AddTypeInterfaceExports(
                BuildModuleExportsMap(sources),
                moduleTypeInterfaces ?? ElmModuleTypeInterface.Standard);

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

    private static ImmutableDictionary<string, ModuleExports> AddTypeInterfaceExports(
        ImmutableDictionary<string, ModuleExports> exports,
        IReadOnlyDictionary<string, ElmModuleTypeInterface> interfaces)
    {
        foreach (var (name, moduleInterface) in interfaces)
        {
            var existing =
                exports.GetValueOrDefault(name) ??
                new ModuleExports([], [], ImmutableDictionary<string, ImmutableList<string>>.Empty);

            exports =
                exports.SetItem(
                    name,
                    existing with
                    {
                        TypeExports = existing.TypeExports.Union(moduleInterface.Types.Keys),
                        ValueExports = existing.ValueExports.Union(moduleInterface.Values.Keys),
                        TypeConstructors = moduleInterface.TypeConstructors.SetItems(existing.TypeConstructors)
                    });
        }

        return exports;
    }

    private static bool IsNativeReference(DeclQualifiedName name) =>
        name.Namespaces.Count is 1 && name.Namespaces[0] is "Basics" or "Debug" or "Pine_kernel" or "Pine_builtin" ||
        name.FullName is "List.List";
}
