using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Construction and repartitioning of immutable, dependency-closed declaration graphs.</summary>
public static class SccGraph
{
    /// <summary>
    /// Rebuilds the SCC partition after rewriting own declarations. Newly external member references
    /// receive fresh deterministic qualifiers. Requested member mappings may select different SCCs.
    /// </summary>
    public static ImmutableDictionary<string, UnitExport> Repartition(
        SccModel input,
        ImmutableDictionary<string, Declaration> members,
        DeclarationUnitCache? cache = null,
        Func<SccModel, string, UnitExport>? dependencyResult = null)
    {
        var edges = members.Keys.ToDictionary(name => name, _ => new HashSet<string>(), StringComparer.Ordinal);

        foreach (var (member, syntax) in members)
        {
            Canonicalize.MapNames(
                syntax,
                (reference, _) =>
                {
                    if (reference.Namespaces.Count is 0)
                    {
                        if (!members.ContainsKey(reference.DeclName))
                            throw new ArgumentException("A transformed SCC contains an unbound member: " + reference);

                        edges[member].Add(reference.DeclName);
                    }

                    return reference;
                });
        }

        var indices = new Dictionary<string, int>(StringComparer.Ordinal);
        var low = new Dictionary<string, int>(StringComparer.Ordinal);
        var active = new HashSet<string>(StringComparer.Ordinal);
        var stack = new Stack<string>();
        var results = ImmutableDictionary.CreateBuilder<string, UnitExport>(StringComparer.Ordinal);
        var next = 0;
        var occupied = new HashSet<string>(StringComparer.Ordinal);

        foreach (var syntax in members.Values)
        {
            Canonicalize.MapNames(
                syntax,
                name =>
                {
                    occupied.Add(string.Join(".", name.Namespaces));
                    return name;
                });
        }

        var freshPrefix = "Split";

        for (var suffix = 0; occupied.Any(name => name == freshPrefix || name.StartsWith(freshPrefix + ".", StringComparison.Ordinal)); suffix++)
            freshPrefix = "Split" + suffix;

        void Visit(string name)
        {
            indices.Add(name, next);
            low.Add(name, next++);
            active.Add(name);
            stack.Push(name);

            foreach (var target in edges[name].Order(StringComparer.Ordinal))
            {
                if (!indices.TryGetValue(target, out var index))
                {
                    Visit(target);
                    low[name] = Math.Min(low[name], low[target]);
                }
                else if (active.Contains(target))
                    low[name] = Math.Min(low[name], index);
            }

            if (low[name] != indices[name])
                return;

            var group = new HashSet<string>(StringComparer.Ordinal);
            string item;

            do
            {
                item = stack.Pop();
                active.Remove(item);
                group.Add(item);
            }
            while (item != name);

            var values = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SccModel>();
            var types = ImmutableDictionary.CreateBuilder<DeclQualifiedName, TypeSccModel>();
            var syntax = ImmutableDictionary.CreateBuilder<string, Declaration>(StringComparer.Ordinal);

            foreach (var member in group.Order(StringComparer.Ordinal))
            {
                syntax.Add(
                    member,
                    Canonicalize.MapNames(
                        members[member],
                        (reference, isType) =>
                        {
                            if (SccValidation.Native(reference))
                                return reference;

                            if (reference.Namespaces.Count is 0 && group.Contains(reference.DeclName))
                                return reference;

                            UnitExport export;
                            DeclQualifiedName key;

                            if (reference.Namespaces.Count is 0)
                            {
                                export = results[reference.DeclName];
                                key = DeclQualifiedName.Create([freshPrefix, "Member"], reference.DeclName);
                            }
                            else
                            {
                                var original = UnitConstruction.Lookup(input, reference, isType);
                                export = dependencyResult?.Invoke(original.Unit, original.Member) ?? original;
                                key = reference;
                            }

                            if (isType)
                            {
                                types[key] =
                                    export.Unit is SccModel.TypeScc type
                                    ?
                                    type.Value
                                    :
                                    throw new ArgumentException("A transformed type dependency must select a type SCC.");
                            }
                            else
                                values[key] = export.Unit;

                            return key;
                        }));
            }

            var intrinsics =
                input is SccModel.TypeScc typeInput
                ?
                typeInput.Value
                .IntrinsicRepresentations.Where(entry => group.Contains(entry.Key)).ToImmutableDictionary()
                :
                [];

            var model = Create(syntax.ToImmutable(), values.ToImmutable(), types.ToImmutable(), intrinsics, cache);

            foreach (var member in group)
                results.Add(member, new(model, member));
        }

        foreach (var member in members.Keys.Order(StringComparer.Ordinal))
        {
            if (!indices.ContainsKey(member))
                Visit(member);
        }

        return results.ToImmutable();
    }

    /// <summary>Creates a typed SCC, rejecting mixed member kinds and invalid references.</summary>
    public static SccModel Create(
        ImmutableDictionary<string, Declaration> members,
        ImmutableDictionary<DeclQualifiedName, SccModel> dependencies,
        ImmutableDictionary<DeclQualifiedName, TypeSccModel> typeDependencies,
        ImmutableDictionary<string, string> intrinsicRepresentations,
        DeclarationUnitCache? cache = null)
    {
        if (cache is not null)
            return cache.Construct(new(members, dependencies, typeDependencies, intrinsicRepresentations));

        var values = ImmutableDictionary.CreateBuilder<string, ValueDeclaration>(StringComparer.Ordinal);
        var types = ImmutableDictionary.CreateBuilder<string, TypeDeclaration>(StringComparer.Ordinal);

        foreach (var (name, declaration) in members)
        {
            switch (declaration)
            {
                case Declaration.FunctionDeclaration function:
                    values.Add(name, new ValueDeclaration.FunctionDeclaration(function.Function));
                    break;

                case Declaration.PortDeclaration port:
                    values.Add(name, new ValueDeclaration.PortDeclaration(port.Signature));
                    break;

                case Declaration.InfixDeclaration infix:
                    values.Add(name, new ValueDeclaration.InfixDeclaration(infix.Infix));
                    break;

                case Declaration.AliasDeclaration alias:
                    types.Add(name, new TypeDeclaration.AliasDeclaration(alias.TypeAlias));
                    break;

                case Declaration.ChoiceTypeDeclaration choice:
                    types.Add(name, new TypeDeclaration.ChoiceTypeDeclaration(choice.TypeDeclaration));
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(Create) + " does not handle declaration: " + declaration.GetType().Name);
            }
        }

        SccModel model;

        if (types.Count > 0)
        {
            if (values.Count > 0 || dependencies.Count > 0)
                throw new ArgumentException("Type SCCs cannot contain value members or value dependencies.");

            model = new SccModel.TypeScc(new(types.ToImmutable(), typeDependencies, intrinsicRepresentations));
        }
        else
        {
            if (intrinsicRepresentations.Count > 0)
                throw new ArgumentException("Intrinsic type representations cannot belong to value SCCs.");

            model = new SccModel.ValueScc(new(values.ToImmutable(), dependencies, typeDependencies));
        }

        return model;
    }
}
