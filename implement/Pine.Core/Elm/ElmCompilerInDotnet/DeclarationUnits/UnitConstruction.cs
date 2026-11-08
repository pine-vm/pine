using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text.Json;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Pure content addressing and projection of finite, dependency-closed units.</summary>
public static class UnitConstruction
{
    /// <summary>Hashes direct edges once; transitive dependency bodies are not serialized again.</summary>
    public static DeclarationUnit Create(UnitInput input)
    {
        if (input.Members.Count is 0)
            throw new ArgumentException("A declaration unit must have at least one member.", nameof(input));

        if (input.MemberIntrinsics.Count != 0 && input.MemberIntrinsics.Count != input.Members.Count ||
            input.MemberIntrinsics.Any(intrinsic => intrinsic is not ("" or "String" or "Char")))
        {
            throw new ArgumentException(
                "Member intrinsic roles must align with the unit's declarations.",
                nameof(input));
        }

        foreach (var dependency in input.Dependencies)
        {
            if (dependency.Member < 0 || dependency.Member >= dependency.Unit.Input.Members.Count)
                throw new ArgumentException("A dependency export selects a nonexistent member.", nameof(input));
        }

        var localEdges = Enumerable.Range(0, input.Members.Count).Select(_ => new HashSet<int>()).ToArray();

        for (var member = 0; member < input.Members.Count; member++)
            CheckReferences(input.Members[member], null, member);

        foreach (var definition in input.Interfaces)
            CheckReferences(definition.Syntax, definition);

        if (input.Members.Count > 1)
        {
            var reverseEdges = Enumerable.Range(0, input.Members.Count).Select(_ => new HashSet<int>()).ToArray();

            for (var member = 0; member < localEdges.Length; member++)
            {
                foreach (var target in localEdges[member])
                    reverseEdges[target].Add(member);
            }

            if (!ReachesAll(localEdges) || !ReachesAll(reverseEdges))
                throw new ArgumentException("Unit members must form a strongly connected component.", nameof(input));
        }

        bool ReachesAll(HashSet<int>[] edges)
        {
            var visited = new HashSet<int>();
            var pending = new Stack<int>([0]);

            while (pending.TryPop(out var member))
            {
                if (visited.Add(member))
                {
                    foreach (var target in edges[member])
                        pending.Push(target);
                }
            }

            return visited.Count == input.Members.Count;
        }

        void CheckReferences(Declaration declaration, InterfaceDefinition? definition, int owner = -1)
        {
            Canonicalize.MapNames(
                declaration,
                reference =>
                {
                    if (reference.Namespaces.Count is 0 ||
                        reference.Namespaces is ["Basics"] or ["Debug"] or ["Pine_kernel"] or ["Pine_builtin"] ||
                        reference.Namespaces is ["List"] && reference.DeclName is "List" ||
                        reference.Namespaces is ["Char"] && reference.DeclName is "Char" ||
                        reference.Namespaces is ["String"] && reference.DeclName is "String")
                    {
                        return reference;
                    }

                    if (owner >= 0 && reference.Namespaces is ["PineUnit", var target] &&
                        ValidSlot(target, 'M', input.Members.Count))
                    {
                        localEdges[owner].Add(int.Parse(target.AsSpan(1)));
                    }

                    if (reference.Namespaces is ["PineUnit", var member] && ValidSlot(member, 'M', input.Members.Count) &&
                        Exports(input.Members[int.Parse(member.AsSpan(1))], reference.DeclName) ||
                        reference.Namespaces is ["PineDependency", var dependency] &&
                        ValidSlot(dependency, 'D', input.Dependencies.Count) &&
                        Exports(
                            input.Dependencies[int.Parse(dependency.AsSpan(1))].Unit.Input.Members[
                            input.Dependencies[int.Parse(dependency.AsSpan(1))].Member],
                            reference.DeclName) ||
                        reference.Namespaces is ["PineInterface", var facility] &&
                        ValidSlot(facility, 'I', input.Interfaces.Count) &&
                        Exports(input.Interfaces[int.Parse(facility.AsSpan(1))].Syntax, reference.DeclName) ||
                        definition is not null && reference.Namespaces is ["PineNominal", var nominal] &&
                        ValidSlot(nominal, 'T', definition.Nominals.Count) &&
                        definition.Nominals[int.Parse(nominal.AsSpan(1))].Name == reference.DeclName)
                    {
                        return reference;
                    }

                    throw new ArgumentException("A unit contains an unbound reference: " + reference, nameof(input));
                });
        }

        var document =
            new
            {
                Domain = "pine-elm-declaration-unit-v1",
                input.PreserveOperators,
                input.Members,
                input.Interfaces,
                input.MemberIntrinsics,
                Dependencies = input.Dependencies.Select(dependency => dependency.Address).ToArray()
            };

        var address =
            Convert.ToHexStringLower(
                SHA256.HashData(
                    JsonSerializer.SerializeToUtf8Bytes(document, ElmSyntaxAbstractJson.JsonSerializerOptions)));

        return new(address, input);
    }

    private static bool ValidSlot(string text, char prefix, int count) =>
        text.Length > 1 && text[0] == prefix && int.TryParse(text.AsSpan(1), out var slot) && slot >= 0 && slot < count;

    private static bool Exports(Declaration declaration, string symbol) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function => function.Function.Declaration.Name == symbol,
            Declaration.AliasDeclaration alias => alias.TypeAlias.Name == symbol,

            Declaration.ChoiceTypeDeclaration choice =>
            choice.TypeDeclaration.Name == symbol ||
            choice.TypeDeclaration.Constructors.Any(constructor => constructor.Name == symbol),

            Declaration.PortDeclaration port => port.Signature.Name == symbol,
            Declaration.InfixDeclaration infix => infix.Infix.Operator == symbol,

            _ =>
            throw new NotImplementedException(nameof(Exports) + " does not handle declaration: " + declaration.GetType().Name)
        };

    /// <summary>Artifact names contain content addresses and local member slots, never source names.</summary>
    public static DeclQualifiedName MemberName(UnitExport export, string symbol) =>
        export.Unit.Input.MemberIntrinsics.Count > 0 &&
        export.Unit.Input.MemberIntrinsics[export.Member] is { Length: > 0 } intrinsic
        ?
        DeclQualifiedName.Create([intrinsic], symbol)
        :
        DeclQualifiedName.Create(["PineArtifact", "U" + export.Unit.Address, "M" + export.Member], symbol);

    /// <summary>Projects each shared unit exactly once, following direct dependency edges.</summary>
    public static ImmutableDictionary<DeclQualifiedName, Declaration> Flatten(
        IEnumerable<UnitExport> roots,
        ICache<string, ImmutableDictionary<DeclQualifiedName, Declaration>>? projections = null)
    {
        var declarations = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();
        var visited = new HashSet<string>(StringComparer.Ordinal);

        void Visit(DeclarationUnit unit)
        {
            if (!visited.Add(unit.Address))
                return;

            foreach (var dependency in unit.Input.Dependencies)
                Visit(dependency.Unit);

            if (projections?.TryGetValue(unit.Address, out var own) is not true)
            {
                own = ProjectOwn(unit);
                projections?.Set(unit.Address, own);
            }

            foreach (var declaration in own!)
                declarations.Add(declaration.Key, declaration.Value);
        }

        foreach (var root in roots)
            Visit(root.Unit);

        return declarations.ToImmutable();
    }

    /// <summary>Pure projection of own members; only direct export descriptors are consulted.</summary>
    public static ImmutableDictionary<DeclQualifiedName, Declaration> ProjectOwn(DeclarationUnit unit)
    {
        var declarations = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();

        for (var index = 0; index < unit.Input.Interfaces.Count; index++)
        {
            var definition = unit.Input.Interfaces[index];
            var facility = "I" + index;

            var linked =
                Canonicalize.MapNames(
                    definition.Syntax,
                    name =>
                    name.Namespaces is ["PineNominal", var nominal]
                    ?
                    DeclQualifiedName.Create(["PineArtifact", "U" + unit.Address, facility, nominal], name.DeclName)
                    :
                    name);

            declarations.Add(
                DeclQualifiedName.Create(
                    ["PineArtifact", "U" + unit.Address, facility],
                    ElmCompiler.GetDeclarationName(linked) ??
                    throw new InvalidOperationException("An interface declaration must have a name.")),
                linked);

            for (var nominal = 0; nominal < definition.Nominals.Count; nominal++)
            {
                var shape = definition.Nominals[nominal];

                declarations.Add(
                    DeclQualifiedName.Create(
                        ["PineArtifact", "U" + unit.Address, facility, "T" + nominal],
                        shape.Name),
                    InterfaceShapes.OpaqueType(shape.Name, shape.Arity).Syntax);
            }
        }

        for (var member = 0; member < unit.Input.Members.Count; member++)
        {
            var syntax = unit.Input.Members[member];
            var export = new UnitExport(unit, member);

            var linked =
                Canonicalize.MapNames(
                    syntax,
                    name =>
                    {
                        if (name.Namespaces is ["PineUnit", var slot] && int.TryParse(slot.AsSpan(1), out var index))
                            return MemberName(new(unit, index), name.DeclName);

                        if (name.Namespaces is ["PineDependency", var dependency] &&
                            int.TryParse(dependency.AsSpan(1), out var dependencyIndex))
                        {
                            return MemberName(unit.Input.Dependencies[dependencyIndex], name.DeclName);
                        }

                        if (name.Namespaces is ["PineInterface", var facility])
                            return DeclQualifiedName.Create(["PineArtifact", "U" + unit.Address, facility], name.DeclName);

                        return name;
                    });

            declarations.Add(
                MemberName(
                    export,
                    ElmCompiler.GetDeclarationName(syntax) ??
                    throw new InvalidOperationException("A unit member must have a name.")),
                linked);
        }

        return declarations.ToImmutable();
    }
}
