using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Derived compiler views of the shared SCC graph; fingerprints are runtime implementation details.</summary>
public static class UnitConstruction
{
    /// <summary>Enumerates each shared component once, dependencies before consumers.</summary>
    public static IEnumerable<SccModel> Closure(IEnumerable<SccModel> roots)
    {
        var visited = new HashSet<SccModel>();
        var pending = new Stack<(SccModel Model, bool Expanded)>(roots.Select(root => (root, false)));

        while (pending.TryPop(out var item))
        {
            if (item.Expanded)
            {
                yield return item.Model;
                continue;
            }

            if (!visited.Add(item.Model))
                continue;

            pending.Push((item.Model, true));

            foreach (var dependency in Dependencies(item.Model))
                pending.Push((dependency, false));
        }
    }

    /// <summary>Direct edges in both namespaces, without scanning dependency bodies.</summary>
    public static IEnumerable<SccModel> Dependencies(SccModel model) =>
        model switch
        {
            SccModel.ValueScc value =>
            value.Value.Dependencies.Values.Concat(
                value.Value.TypeDependencies.Values.Select(type => new SccModel.TypeScc(type))),

            SccModel.TypeScc type =>
            type.Value.TypeDependencies.Values.Select(dependency => new SccModel.TypeScc(dependency)),

            _ =>
            throw new NotImplementedException(nameof(Dependencies) + " does not handle SCC: " + model.GetType().Name)
        };

    /// <summary>Uses the target's namespace-specific symbol index; constructors retain their own names.</summary>
    public static UnitExport Lookup(SccModel model, DeclQualifiedName reference, bool isType)
    {
        if (reference.Namespaces.Count is 0)
            return new(model, SccValidation.Select(model, reference.DeclName, isType));

        return model switch
        {
            SccModel.ValueScc value when isType && value.Value.TypeDependencies.TryGetValue(reference, out var type) =>
            new(new SccModel.TypeScc(type), SccValidation.Select(type, reference.DeclName, true)),

            SccModel.ValueScc value when !isType && value.Value.Dependencies.TryGetValue(reference, out var dependency) =>
            new(dependency, SccValidation.Select(dependency, reference.DeclName, false)),

            SccModel.ValueScc =>
            throw new ArgumentException("An SCC has no dependency for the requested symbol: " + reference),

            SccModel.TypeScc type when isType && type.Value.TypeDependencies.TryGetValue(reference, out var dependency) =>
            new(new SccModel.TypeScc(dependency), SccValidation.Select(dependency, reference.DeclName, true)),

            SccModel.TypeScc =>
            throw new ArgumentException("A type SCC has no dependency for the requested type: " + reference),

            _ =>
            throw new NotImplementedException(nameof(Lookup) + " does not handle SCC: " + model.GetType().Name)
        };
    }

    /// <summary>Compiler names are derived handles, not semantic SCC identities.</summary>
    public static DeclQualifiedName MemberName(UnitExport export, string symbol) =>
        export.Unit is SccModel.TypeScc type &&
        type.Value.IntrinsicRepresentations.TryGetValue(export.Member, out var intrinsic)
        ?
        DeclQualifiedName.Create([intrinsic], symbol)
        :
        DeclQualifiedName.Create(["PineArtifact", "U" + export.Unit.Fingerprint], symbol);

    /// <summary>Projects each distinct SCC once, following value and type dependencies.</summary>
    public static ImmutableDictionary<DeclQualifiedName, Declaration> Flatten(
        IEnumerable<UnitExport> roots,
        ICache<SccModel, ImmutableDictionary<DeclQualifiedName, Declaration>>? projections = null)
    {
        var declarations = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();

        foreach (var model in Closure(roots.Select(root => root.Unit)))
        {
            if (projections?.TryGetValue(model, out var own) is not true)
            {
                own = ProjectOwn(model);
                projections?.Set(model, own);
            }

            declarations.AddRange(own!);
        }

        return declarations.ToImmutable();
    }

    /// <summary>Projects own syntax by consulting only direct symbol-keyed dependencies.</summary>
    public static ImmutableDictionary<DeclQualifiedName, Declaration> ProjectOwn(SccModel model) =>
        SccValidation.Members(model).ToImmutableDictionary(
            entry => MemberName(new(model, entry.Key), entry.Key),
            entry => Canonicalize.MapNames(
                entry.Value,
                (name, isType) =>
                SccValidation.Native(name)
                ?
                name
                :
                MemberName(Lookup(model, name, isType), name.DeclName)));
}
