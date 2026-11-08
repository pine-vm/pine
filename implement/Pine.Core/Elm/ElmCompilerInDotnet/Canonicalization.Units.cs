using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Abstract = Pine.Core.Elm.Syntax.Abstract;
using Range = Pine.Core.Elm.Syntax.Concrete.Range;
using UnitCanonicalization = Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits.Canonicalize;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class Canonicalization
{
    private sealed record SourceSlot(DeclQualifiedName Reference, DeclQualifiedName? Owner,
        IReadOnlyList<DeclQualifiedName> Dependencies, InterfaceDefinition? Interface = null);

    private sealed record UnitPreparation(
        CanonicalDeclaration Canonical,
        IReadOnlyList<SourceSlot> Slots,
        CanonicalizationResult<Node<Declaration>> Presentation,
        bool Transformed,
        bool PreserveOperators,
        string Intrinsic,
        bool Closed);

    private static UnitPreparation PrepareUnitDeclaration(
        Node<Declaration> declaration,
        CanonicalizationContext context,
        DeclarationUnitCache storage,
        Func<DeclQualifiedName, bool, InterfaceDefinition> interfaceShape,
        bool frozen = false,
        bool allowUnboundQualifiedReferences = false)
    {
        var ownerQualifierIsImportAlias =
            !frozen &&
            context.AliasMap.TryGetValue("module:" + string.Join(".", context.CurrentModuleName), out var aliasedModule) &&
            !ModuleNameComparer.Equal(aliasedModule, context.CurrentModuleName);

        var syntax =
            UnitCanonicalization.MapNames(
                Abstract.ConvertFromConcrete.FromDeclaration(declaration.Value),
                name => !ownerQualifierIsImportAlias && name.Namespaces.SequenceEqual(context.CurrentModuleName)
                ?
                DeclQualifiedName.Create(["Self"], name.DeclName)
                :
                name);

        if (!storage.Queries.TryGetValue(syntax, out var questions))
        {
            questions = UnitCanonicalization.Questions(syntax);
            storage.Queries.Set(syntax, questions);
        }

        var slots = new List<SourceSlot>();
        var slotIndices = new Dictionary<DeclQualifiedName, int>();
        var answers = new List<ResolutionAnswer>();
        var targets = new Dictionary<NameQuery, DeclQualifiedName>();

        var queryNames =
            questions.Where(query => query.Kind is not QueryKind.FreshBinding)
            .Select(query => query.Name).ToHashSet(StringComparer.Ordinal);

        foreach (var query in questions)
        {
            if (query.Kind is QueryKind.FreshBinding)
            {
                var candidate = query.Name;

                for (var index = 1; ; index++)
                {
                    var present =
                        queryNames.Contains(candidate) || context.ContainsModuleDeclaration(candidate) ||
                        context.GetImport(false, candidate) is not null;

                    answers.Add(
                        new(
                            new(QueryKind.FreshBinding, "", candidate),
                            present ? ResolutionKind.Present : ResolutionKind.Absent));

                    if (!present)
                        break;

                    candidate = query.Name + index;
                }

                continue;
            }

            answers.Add(Resolve(query));
        }

        var input = new CanonicalizationInput(syntax, new(answers), context.PreserveOperatorApplications);
        var transformed = !storage.Canonical.TryGetValue(input, out var canonical);

        if (transformed)
        {
            canonical = UnitCanonicalization.Apply(input);
            storage.Canonical.Set(input, canonical);
        }

        var rebound =
            UnitCanonicalization.MapNames(
                canonical!.Syntax,
                name =>
                UnitCanonicalization.ReferenceSlot(name.Namespaces) is { } slot
                ?
                DeclQualifiedName.Create(slots[slot].Reference.Namespaces, name.DeclName)
                :
                name);

        var errors = canonical.Diagnostics.Select(PresentError).ToArray();

        var shadowings =
            canonical.Diagnostics.Where(diagnostic => diagnostic.Kind is ResolutionKind.Present)
            .GroupBy(diagnostic => diagnostic.Query.Name, StringComparer.Ordinal)
            .ToImmutableDictionary(
                group => group.Key,
                group => new ShadowingLocation(
                    RequiredRange(group.First().Path, true),
                    [GetDeclarationName(declaration.Value)]),
                StringComparer.Ordinal);

        return
            new(
                canonical,
                slots,
                new(
                    ConcreteProjection.Reify(
                        rebound,
                        canonical,
                        declaration,
                        preservePrefixOperators: allowUnboundQualifiedReferences),
                    errors,
                    shadowings),
                transformed,
                context.PreserveOperatorApplications,
                declaration.Value is Declaration.ChoiceTypeDeclaration &&
                context.CurrentModuleName is ["String"] &&
                GetDeclarationName(declaration.Value) is "String"
                ?
                "String"
                :
                declaration.Value is Declaration.ChoiceTypeDeclaration &&
                context.CurrentModuleName is ["Char"] && GetDeclarationName(declaration.Value) is "Char"
                ?
                "Char"
                :
                "",
                answers.All(answer => answer.Kind is not ResolutionKind.DeferredQualified));

        ResolutionAnswer Resolve(NameQuery query)
        {
            if (query.Kind is QueryKind.ModuleBinding)
            {
                return
                    new(
                        query,
                        context.ContainsModuleDeclaration(query.Name) ? ResolutionKind.Present : ResolutionKind.Absent);
            }

            var type = query.Kind is QueryKind.Type;
            IReadOnlyList<string> module;
            var symbol = query.Name;

            if (query.Kind is QueryKind.Operator)
            {
                if (!context.OperatorToFunction.TryGetValue(query.Name, out var function))
                    return new(query, ResolutionKind.MissingName);

                if (!context.ContainsModuleDeclaration(query.Name) &&
                    context.GetImport(false, query.Name) is { Count: > 1 })
                {
                    return new(query, ResolutionKind.Ambiguous);
                }

                module = function.ModuleName;
                symbol = function.FunctionName;
            }
            else if (query.Qualifier.Length is 0)
            {
                if (context.ModuleDeclarations(type).Contains(query.Name))
                    module = context.CurrentModuleName;

                else if (context.GetImport(type, query.Name) is { Count: > 0 } imports)
                {
                    if (imports.Count > 1)
                        return new(query, ResolutionKind.Ambiguous);

                    module = imports[0];
                }
                else
                    return new(query, ResolutionKind.MissingName);
            }
            else if (query.Qualifier is "Self")
                module = context.CurrentModuleName;

            else if (frozen)
                module = query.Qualifier.Split('.');

            else if (context.AliasMap.TryGetValue(
                (type ? "type:" : "value:") + query.Qualifier + "." + query.Name,
                out var exported))
            {
                module = exported;
            }
            else if (query.Qualifier is "Basics" or "Debug" or "Pine_kernel" or "Pine_builtin" ||
                query.Qualifier is "List" && query.Name is "List")
            {
                module = query.Qualifier.Split('.');
            }
            else
            {
                var imported = context.AliasMap.GetValueOrDefault("module:" + query.Qualifier);

                if (allowUnboundQualifiedReferences)
                    module = imported ?? query.Qualifier.Split('.');

                else
                {
                    if (imported is null)
                        return new(query, ResolutionKind.UnimportedQualifier);

                    var target = DeclQualifiedName.Create(imported, symbol);
                    targets[query] = target;
                    return new(query, Availability(context.LookupAvailability!(target, type)));
                }
            }

            var reference = DeclQualifiedName.Create(module, symbol);
            targets[query] = reference;
            var resolution = context.LookupSymbol!(reference, type);

            switch (resolution)
            {
                case SymbolResolution.Native:
                    return new(query, ResolutionKind.Native, Symbol: symbol, NativeModule: string.Join(".", module));

                case SymbolResolution.Declaration resolved:
                    return
                        new(
                            query,
                            ResolutionKind.Declaration,
                            Slot(new(reference, resolved.Owner, [resolved.Owner])),
                            symbol);

                case SymbolResolution.TypeOnly typeOnly:
                    var shape = interfaceShape(reference, type);

                    return
                        new(
                            query,
                            ResolutionKind.TypeOnly,
                            Slot(new(reference, null, typeOnly.Dependencies, shape)),
                            symbol,
                            Interface: shape);

                case SymbolResolution.Ambiguous:
                    return new(query, ResolutionKind.Ambiguous);

                case SymbolResolution.Missing missing:
                    if (allowUnboundQualifiedReferences)
                        return new(query, ResolutionKind.DeferredQualified, Slot(new(reference, null, [])), symbol);

                    return new(query, Availability(missing.Availability));

                default:
                    throw new NotImplementedException(
                        nameof(PrepareUnitDeclaration) + " does not handle resolution: " + resolution.GetType().Name);
            }
        }

        int Slot(SourceSlot slot)
        {
            // Constructor and type references share their owning declaration but retain distinct symbols.
            var identity = slot.Owner ?? slot.Reference;

            if (slotIndices.TryGetValue(identity, out var existing))
                return existing;

            var index = slots.Count;
            slots.Add(slot);
            slotIndices.Add(identity, index);
            return index;
        }

        Range RequiredRange(SyntaxPath path, bool name = false) =>
            SourceLookup.RangeAtPath(declaration, path, name) ??
            throw new InvalidOperationException("A canonicalization diagnostic has no matching source path.");

        CanonicalizationError PresentError(SyntaxDiagnostic diagnostic)
        {
            var query = diagnostic.Query;
            var range = RequiredRange(diagnostic.Path, true);

            if (diagnostic.Kind is ResolutionKind.Present)
                return new CanonicalizationError.NamingClash(range, query.Name);

            if (diagnostic.Kind is ResolutionKind.Ambiguous)
            {
                var imports = context.GetImport(query.Kind is QueryKind.Type, query.Name) ?? [];

                return
                    new CanonicalizationError.AmbiguousImport(
                        range,
                        query.Name,
                        [.. imports.Select(module => string.Join(".", module))]);
            }

            var target = targets.GetValueOrDefault(query);
            var name = (query.Qualifier.Length is 0 ? "" : query.Qualifier + ".") + query.Name;
            var kind = query.Kind is QueryKind.Type ? "type" : "value";

            var detail =
                diagnostic.Kind switch
                {
                    ResolutionKind.UnimportedQualifier =>
                    $"Qualifier '{query.Qualifier}' is not an explicit or implicit import in this source scope. Add the required import or correct the qualifier.",

                    ResolutionKind.Private =>
                    $"Module '{string.Join(".", target!.Namespaces)}' declares {kind} '{query.Name}', but does not expose it. Correct the reference or the declaring module's exposing list.",

                    ResolutionKind.WrongNamespace =>
                    $"'{target}' exists in the {(query.Kind is QueryKind.Type ? "value" : "type")} namespace, not the {kind} namespace.",

                    ResolutionKind.MissingModule =>
                    $"Module '{string.Join(".", target!.Namespaces)}' is unavailable in the supplied sources. Check the import, declared dependencies, and configured package replacements.",

                    _ =>
                    target is null
                    ?
                    $"No local binding, {kind} declaration, or exposed import provides '{query.Name}'."
                    :
                    $"The supplied module '{string.Join(".", target.Namespaces)}' has no {kind} declaration named '{query.Name}'. Check the spelling and the API provided by the selected package or replacement."
                };

            return
                new CanonicalizationError.UnresolvedReference(
                    range,
                    query.Qualifier is "Self" ? string.Join(".", context.CurrentModuleName) + "." + query.Name : name)
                {
                    Target = target,
                    IsTypeReference = query.Kind is QueryKind.Type,
                    ResolutionDetail = detail
                };
        }
    }

    private static ResolutionKind Availability(SymbolAvailability availability) =>
        availability switch
        {
            SymbolAvailability.Declared => ResolutionKind.Private,
            SymbolAvailability.OtherNamespace => ResolutionKind.WrongNamespace,
            SymbolAvailability.MissingModule => ResolutionKind.MissingModule,
            SymbolAvailability.MissingName => ResolutionKind.MissingName,

            _ =>
            throw new NotImplementedException("Unknown symbol availability: " + availability)
        };

    private static IReadOnlyDictionary<DeclQualifiedName, UnitExport> BuildUnits(
        IReadOnlyDictionary<DeclQualifiedName, UnitPreparation> preparations,
        IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclQualifiedName>> dependencies,
        DeclarationUnitCache storage)
    {
        var indices = new Dictionary<DeclQualifiedName, int>();
        var low = new Dictionary<DeclQualifiedName, int>();
        var stack = new Stack<DeclQualifiedName>();
        var active = new HashSet<DeclQualifiedName>();
        var exports = new Dictionary<DeclQualifiedName, UnitExport>();
        var index = 0;

        void Visit(DeclQualifiedName name)
        {
            indices.Add(name, index);
            low.Add(name, index++);
            stack.Push(name);
            active.Add(name);

            foreach (var dependency in dependencies[name])
            {
                if (!preparations.ContainsKey(dependency))
                    continue;

                if (!indices.TryGetValue(dependency, out var dependencyIndex))
                {
                    Visit(dependency);
                    low[name] = Math.Min(low[name], low[dependency]);
                }
                else if (active.Contains(dependency))
                    low[name] = Math.Min(low[name], dependencyIndex);
            }

            if (low[name] != indices[name])
                return;

            var members = new List<DeclQualifiedName>();
            DeclQualifiedName member;

            do
            {
                member = stack.Pop();
                active.Remove(member);
                members.Add(member);
            }
            while (!member.Equals(name));

            members =
                [
                .. members.OrderBy(item => item.DeclName, StringComparer.Ordinal)
                .ThenBy(
                    item => Abstract.ElmSyntaxAbstractJson.ToJsonString(preparations[item].Canonical.Syntax),
                    StringComparer.Ordinal)
                ];

            var localSlots =
                members.Select((item, slot) => (item, slot)).ToDictionary(entry => entry.item, entry => entry.slot);

            if (members.Any(
                item => preparations[item].Canonical.Diagnostics.Count > 0 ||
                    !preparations[item].Closed ||
                    dependencies[item].Any(
                        dependency => !localSlots.ContainsKey(dependency) && !exports.ContainsKey(dependency))))
            {
                return;
            }

            var external = new List<UnitExport>();
            var externalSlots = new Dictionary<DeclQualifiedName, int>();

            int External(DeclQualifiedName owner)
            {
                if (externalSlots.TryGetValue(owner, out var slot))
                    return slot;

                slot = external.Count;
                external.Add(exports[owner]);
                externalSlots.Add(owner, slot);
                return slot;
            }

            var syntax = new List<Abstract.Declaration>();
            var interfaces = new List<InterfaceDefinition>();
            var interfaceSlots = new Dictionary<DeclQualifiedName, int>();

            int Interface(SourceSlot source)
            {
                if (interfaceSlots.TryGetValue(source.Reference, out var slot))
                    return slot;

                slot = interfaces.Count;

                interfaces.Add(
                    source.Interface ??
                    throw new InvalidOperationException("Type-only binding has no interface shape."));

                interfaceSlots.Add(source.Reference, slot);
                return slot;
            }

            foreach (var dependency in members.SelectMany(item => preparations[item].Slots.SelectMany(slot => slot.Dependencies))
                .Where(dependency => !localSlots.ContainsKey(dependency))
                .Distinct())
            {
                External(dependency);
            }

            foreach (var item in members)
            {
                var preparation = preparations[item];

                syntax.Add(
                    UnitCanonicalization.MapNames(
                        preparation.Canonical.Syntax,
                        reference =>
                        {
                            if (UnitCanonicalization.ReferenceSlot(reference.Namespaces) is not { } slot)
                                return reference;

                            var source = preparation.Slots[slot];

                            if (source.Owner is null)
                            {
                                return
                                    DeclQualifiedName.Create(
                                        ["PineInterface", "I" + Interface(source)],
                                        reference.DeclName);
                            }

                            if (localSlots.TryGetValue(source.Owner, out var local))
                                return DeclQualifiedName.Create(["PineUnit", "M" + local], reference.DeclName);

                            return
                                DeclQualifiedName.Create(
                                    [
                                    "PineDependency",
                                    "D" + External(source.Owner)
                                    ],
                                    reference.DeclName);
                        }));
            }

            var input =
                new UnitInput(new(syntax), new(external), preparations[members[0]].PreserveOperators)
                {
                    Interfaces = new(interfaces),
                    MemberIntrinsics = new(members.Select(item => preparations[item].Intrinsic))
                };

            var ordered = storage.Order(input);
            var unit = storage.Intern(ordered.Input);

            for (var slot = 0; slot < members.Count; slot++)
                exports.Add(members[slot], new(unit, ordered.MemberSlots[slot]));
        }

        foreach (var name in preparations.Keys.Order())
        {
            if (!indices.ContainsKey(name))
                Visit(name);
        }

        return exports;
    }
}
