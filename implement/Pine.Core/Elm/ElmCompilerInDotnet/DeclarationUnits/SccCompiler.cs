using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>One pure syntax pass, including the options consulted by that pass.</summary>
public sealed record SccSyntaxInput(SccModel Model, bool LowerOperators, bool LowerCoreBasics, bool RemoveDebugLog);

/// <summary>Own compiler functions and binding spellings, without dependency bodies or caller provenance.</summary>
public sealed record SccLoweringResult(
    ImmutableDictionary<Model.FunctionId, Model.Function> Functions,
    ImmutableDictionary<Model.BindingId, string> BindingNames);

/// <summary>A derived, binding-preserving view of own syntax and direct dependency metadata.</summary>
public sealed record SccCompilerView(
    ImmutableDictionary<DeclQualifiedName, Declaration> OwnDeclarations,
    ImmutableDictionary<DeclQualifiedName, Declaration> DependencyMetadata,
    ImmutableDictionary<DeclQualifiedName, DeclQualifiedName> OutwardNames,
    ImmutableDictionary<DeclQualifiedName, DeclQualifiedName> InwardValues,
    ImmutableDictionary<DeclQualifiedName, DeclQualifiedName> InwardTypes);

/// <summary>Compiler transformations over SCC graphs, with invocation-owned or caller-supplied caches.</summary>
public static class SccCompiler
{
    /// <summary>
    /// Applies a syntax pass and returns each requested declaration's resulting export.
    /// Dependency changes are propagated without preserving obsolete SCC partitions.
    /// </summary>
    public static Result<string, ImmutableDictionary<string, UnitExport>> Transform(
        IReadOnlyDictionary<string, UnitExport> roots,
        bool lowerOperators,
        bool lowerCoreBasics,
        bool removeDebugLog,
        DeclarationUnitCache? cache = null)
    {
        cache ??= new();
        var models = UnitConstruction.Closure(roots.Values.Select(root => root.Unit)).ToArray();
        var results = new Dictionary<SccModel, ImmutableDictionary<string, UnitExport>>();
        var cachedResults = new Dictionary<SccModel, ImmutableDictionary<string, UnitExport>>();
        var missing = new HashSet<SccModel>();

        foreach (var model in models)
        {
            var input = new SccSyntaxInput(model, lowerOperators, lowerCoreBasics, removeDebugLog);

            if (cache.SyntaxTransforms.TryGetValue(input, out var cached))
                cachedResults.Add(model, cached);

            else
                missing.Add(model);
        }

        BuiltinOperatorLowering.SccLowering? operatorLowering = null;

        if (missing.Count > 0 && (lowerOperators || lowerCoreBasics))
        {
            operatorLowering =
                new(
                    RequiredInferenceModels(missing, models),
                    new(lowerOperators, lowerCoreBasics),
                    cache);
        }

        foreach (var model in models)
        {
            if (cachedResults.TryGetValue(model, out var cached))
            {
                results[model] = cached;
                continue;
            }

            var members = ImmutableDictionary.CreateBuilder<string, Declaration>(StringComparer.Ordinal);

            foreach (var (member, original) in operatorLowering?.Rewrite(model) ?? SccValidation.Members(model))
            {
                var syntax = original;

                if (removeDebugLog)
                    syntax = DebugLogRemoval.RewriteDeclaration(syntax);

                members.Add(member, syntax);
            }

            var transformed =
                SccGraph.Repartition(
                    model,
                    members.ToImmutable(),
                    cache,
                    (dependency, member) => results[dependency][member]);

            cache.SyntaxTransforms.Set(new(model, lowerOperators, lowerCoreBasics, removeDebugLog), transformed);
            results[model] = transformed;
        }

        return roots.ToImmutableDictionary(entry => entry.Key, entry => results[entry.Value.Unit][entry.Value.Member]);
    }

    /// <summary>Converts a graph to compiler functions, skipping already converted components.</summary>
    public static Model.Program Lower(
        IEnumerable<UnitExport> roots,
        DeclarationUnitCache? cache = null)
    {
        cache ??= new();
        var rootExports = roots.ToArray();
        var models = UnitConstruction.Closure(rootExports.Select(root => root.Unit)).ToArray();
        var functions = ImmutableDictionary.CreateBuilder<Model.FunctionId, Model.Function>();
        var bindings = ImmutableDictionary.CreateBuilder<Model.BindingId, string>();
        var missing = models.Where(model => !cache.CompilerModels.TryGetValue(model, out _)).ToArray();

        if (missing.Length > 0)
        {
            var lowering = new Model.FromCanonicalSyntax.SccLowering(RequiredInferenceModels(missing, models), cache);

            foreach (var model in missing)
                cache.CompilerModels.Set(model, lowering.Lower(model));
        }

        foreach (var model in models)
        {
            if (!cache.CompilerModels.TryGetValue(model, out var own))
                throw new InvalidOperationException("A required SCC was not lowered.");

            functions.AddRange(own.Functions);
            bindings.AddRange(own.BindingNames);
        }

        var metadata =
            UnitConstruction.Flatten(rootExports, cache.Projections)
            .Where(entry => entry.Value is not Declaration.FunctionDeclaration).ToImmutableDictionary();

        return new(functions.ToImmutable(), metadata) { BindingNames = bindings.ToImmutable() };
    }

    private static IEnumerable<SccModel> RequiredInferenceModels(
        IEnumerable<SccModel> missing, IEnumerable<SccModel> models) =>
        UnitConstruction.Closure(
            missing.Concat(
                models.OfType<SccModel.TypeScc>()
                .Where(type => type.Value.IntrinsicRepresentations.Count > 0)));

    /// <summary>Builds only own syntax and direct dependency metadata; no transitive bodies are copied.</summary>
    public static SccCompilerView View(SccModel model, DeclarationUnitCache? cache = null)
    {
        if (cache?.CompilerViews.TryGetValue(model, out var cached) is true)
            return cached;

        var metadata = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();
        var outward = ImmutableDictionary.CreateBuilder<DeclQualifiedName, DeclQualifiedName>();
        var inwardValues = ImmutableDictionary.CreateBuilder<DeclQualifiedName, DeclQualifiedName>();
        var inwardTypes = ImmutableDictionary.CreateBuilder<DeclQualifiedName, DeclQualifiedName>();
        var metadataScopes = new HashSet<(SccModel Model, string Qualifier)>();
        var ownNamespace = new[] { "PineArtifact", "U" + model.Fingerprint };

        DeclQualifiedName Project(DeclQualifiedName reference, bool isType)
        {
            if (SccValidation.Native(reference))
                return reference;

            var export = UnitConstruction.Lookup(model, reference, isType);
            var target = UnitConstruction.MemberName(export, reference.DeclName);

            var intrinsic =
                export.Unit is SccModel.TypeScc intrinsicType &&
                intrinsicType.Value.IntrinsicRepresentations.ContainsKey(export.Member);

            var projected =
                reference.Namespaces.Count is 0 || intrinsic
                ?
                target
                :
                DeclQualifiedName.Create(
                    [.. ownNamespace, "Scope", .. reference.Namespaces, "Target", "U" + export.Unit.Fingerprint],
                    reference.DeclName);

            (isType ? inwardTypes : inwardValues)[projected] = reference;
            outward[projected] = target;

            if (reference.Namespaces.Count > 0)
            {
                if (metadataScopes.Add((export.Unit, string.Join(".", projected.Namespaces))))
                {
                    foreach (var (member, originalDeclaration) in SccValidation.Members(export.Unit))
                    {
                        var alias = DeclQualifiedName.Create(projected.Namespaces, member);
                        var declaration = originalDeclaration;

                        if (declaration is Declaration.FunctionDeclaration function)
                        {
                            declaration =
                                new Declaration.FunctionDeclaration(
                                    function.Function with
                                    {
                                        Declaration =
                                        function.Function.Declaration with { Expression = new Expression.UnitExpr() }
                                    });
                        }

                        metadata[alias] =
                            Canonicalize.MapNames(
                                declaration,
                                (name, type) =>
                                {
                                    if (SccValidation.Native(name))
                                        return name;

                                    var dependency = UnitConstruction.Lookup(export.Unit, name, type);

                                    return
                                        dependency.Unit.Equals(export.Unit)
                                        ?
                                        DeclQualifiedName.Create(projected.Namespaces, name.DeclName)
                                        :
                                        UnitConstruction.MemberName(dependency, name.DeclName);
                                });

                        outward[alias] = UnitConstruction.MemberName(new(export.Unit, member), member);

                        if (declaration is Declaration.ChoiceTypeDeclaration choice)
                        {
                            foreach (var constructor in choice.TypeDeclaration.Constructors)
                            {
                                outward[DeclQualifiedName.Create(projected.Namespaces, constructor.Name)] =
                                    UnitConstruction.MemberName(new(export.Unit, member), constructor.Name);
                            }
                        }
                    }
                }
            }

            return projected;
        }

        var own =
            SccValidation.Members(model).ToImmutableDictionary(
                entry => UnitConstruction.MemberName(new(model, entry.Key), entry.Key),
                entry => Canonicalize.MapNames(entry.Value, Project));

        var view =
            new SccCompilerView(
                own,
                metadata.ToImmutable(),
                outward.ToImmutable(),
                inwardValues.ToImmutable(),
                inwardTypes.ToImmutable());

        cache?.CompilerViews.Set(model, view);
        return view;
    }
}
