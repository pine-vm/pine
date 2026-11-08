using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Canonical SCC input and the projection from original member slots.</summary>
public sealed record OrderedUnitInput(UnitInput Input, ValueList<int> MemberSlots);

/// <summary>Deterministic SCC labeling based on syntax and reference paths, never source identities.</summary>
public static class UnitOrdering
{
    /// <summary>
    /// Uses reference-order traversal from the least lexical member; indistinguishable seeds
    /// are compared structurally. No dependency body is transformed or serialized.
    /// </summary>
    public static OrderedUnitInput Apply(UnitInput input)
    {
        var signatures =
            input.Members.Select(
                member =>
                {
                    var dependencyRoles = new Dictionary<int, int>();
                    var interfaceRoles = new Dictionary<int, int>();

                    int Role(Dictionary<int, int> roles, int original)
                    {
                        if (!roles.TryGetValue(original, out var role))
                            roles.Add(original, role = roles.Count);

                        return role;
                    }

                    return
                        ElmSyntaxAbstractJson.ToJsonString(
                            Canonicalize.MapNames(
                                member,
                                reference =>
                                {
                                    if (reference.Namespaces is ["PineUnit", _])
                                        return DeclQualifiedName.Create(["PineUnit", "Member"], reference.DeclName);

                                    if (reference.Namespaces is ["PineDependency", var dependency])
                                    {
                                        var original = int.Parse(dependency.AsSpan(1));

                                        return
                                            DeclQualifiedName.Create(
                                                [
                                                "PineClosed",
                                                input.Dependencies[original].Address,
                                                "R" + Role(dependencyRoles, original)
                                                ],
                                                reference.DeclName);
                                    }

                                    if (reference.Namespaces is ["PineInterface", var facility])
                                    {
                                        var original = int.Parse(facility.AsSpan(1));

                                        return
                                            DeclQualifiedName.Create(
                                                [
                                                    "PineInterfaceShape",
                                                    JsonSerializer.Serialize(
                                                        input.Interfaces[original],
                                                        ElmSyntaxAbstractJson.JsonSerializerOptions),
                                                    "R" + Role(interfaceRoles, original)
                                                ],
                                                reference.DeclName);
                                    }

                                    return reference;
                                }));
                }).ToArray();

        var minimum =
            signatures.Min(StringComparer.Ordinal) ??
            throw new ArgumentException("A unit must contain a member.", nameof(input));

        OrderedUnitInput? best = null;
        string? bestDocument = null;

        for (var seed = 0; seed < input.Members.Count; seed++)
        {
            if (signatures[seed] != minimum)
                continue;

            var memberSlots = new Dictionary<int, int> { [seed] = 0 };
            var dependencySlots = new Dictionary<int, int>();
            var interfaceSlots = new Dictionary<int, int>();
            var pending = new Queue<int>([seed]);
            var members = new List<Declaration>();

            int Slot(Dictionary<int, int> slots, int original)
            {
                if (!slots.TryGetValue(original, out var slot))
                {
                    slot = slots.Count;
                    slots.Add(original, slot);
                }

                return slot;
            }

            DeclQualifiedName Map(DeclQualifiedName reference)
            {
                if (reference.Namespaces is ["PineUnit", var member])
                {
                    var original = int.Parse(member.AsSpan(1));

                    if (!memberSlots.ContainsKey(original))
                        pending.Enqueue(original);

                    return DeclQualifiedName.Create(["PineUnit", "M" + Slot(memberSlots, original)], reference.DeclName);
                }

                if (reference.Namespaces is ["PineDependency", var dependency])
                {
                    return
                        DeclQualifiedName.Create(
                            [
                            "PineDependency",
                            "D" + Slot(dependencySlots, int.Parse(dependency.AsSpan(1)))
                            ],
                            reference.DeclName);
                }

                if (reference.Namespaces is ["PineInterface", var facility])
                {
                    return
                        DeclQualifiedName.Create(
                            [
                            "PineInterface",
                            "I" + Slot(interfaceSlots, int.Parse(facility.AsSpan(1)))
                            ],
                            reference.DeclName);
                }

                return reference;
            }

            while (pending.TryDequeue(out var member))
                members.Add(Canonicalize.MapNames(input.Members[member], Map));

            if (members.Count != input.Members.Count)
                throw new ArgumentException("Unit members must form a strongly connected component.", nameof(input));

            foreach (var dependency in Enumerable.Range(0, input.Dependencies.Count)
                .Where(index => !dependencySlots.ContainsKey(index))
                .OrderBy(index => input.Dependencies[index].Address, StringComparer.Ordinal))
            {
                Slot(dependencySlots, dependency);
            }

            foreach (var facility in Enumerable.Range(0, input.Interfaces.Count))
                Slot(interfaceSlots, facility);

            var ordered =
                new UnitInput(
                    new(members),
                    new(dependencySlots.OrderBy(pair => pair.Value).Select(pair => input.Dependencies[pair.Key])),
                    input.PreserveOperators)
                {
                    Interfaces =
                    new(
                        interfaceSlots.OrderBy(pair => pair.Value).Select(pair => input.Interfaces[pair.Key])),
                    MemberIntrinsics =
                    input.MemberIntrinsics.All(intrinsic => intrinsic.Length is 0)
                    ?
                    []
                    :
                    new(memberSlots.OrderBy(pair => pair.Value).Select(pair => input.MemberIntrinsics[pair.Key]))
                };

            var document =
                JsonSerializer.Serialize(
                    new
                    {
                        ordered.Members,
                        ordered.Interfaces,
                        ordered.MemberIntrinsics,
                        Dependencies = ordered.Dependencies.Select(dependency => dependency.Address).ToArray()
                    },
                    ElmSyntaxAbstractJson.JsonSerializerOptions);

            if (bestDocument is null || string.CompareOrdinal(document, bestDocument) < 0)
            {
                bestDocument = document;
                best = new(ordered, new(Enumerable.Range(0, input.Members.Count).Select(index => memberSlots[index])));
            }
        }

        return best ?? throw new InvalidOperationException("A unit has no canonical ordering.");
    }
}
