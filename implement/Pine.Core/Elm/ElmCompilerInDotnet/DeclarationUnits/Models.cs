using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Structural addressing relative to a declaration, not a source location.</summary>
public sealed record SyntaxPath(ValueList<SyntaxStep> Steps)
{
    /// <summary>The declaration itself.</summary>
    public static SyntaxPath Root { get; } = new([]);

    /// <summary>Descends into a structural child.</summary>
    public SyntaxPath Append(SyntaxStepKind kind, int index = 0, string name = "", int occurrence = 0) =>
        new(new ValueList<SyntaxStep>([.. Steps, new(kind, index, name, occurrence)]));
}

/// <summary>One edge in the range-free syntax tree, following ElmSyntax.Path.</summary>
public sealed record SyntaxStep(SyntaxStepKind Kind, int Index, string Name, int Occurrence);

/// <summary>Declaration-local structural edges.</summary>
public enum SyntaxStepKind
{
    /// <summary>Declared signature.</summary>
    Signature,
    /// <summary>Function implementation.</summary>
    Implementation,
    /// <summary>Attached type annotation.</summary>
    TypeAnnotation,
    /// <summary>Indexed choice constructor.</summary>
    Constructor,
    /// <summary>Indexed argument.</summary>
    Argument,
    /// <summary>Expression body.</summary>
    Body,
    /// <summary>Indexed local declaration.</summary>
    LetDeclaration,
    /// <summary>Indexed case branch.</summary>
    CaseBranch,
    /// <summary>Binding pattern.</summary>
    Pattern,
    /// <summary>Indexed expression or type child.</summary>
    Child,
    /// <summary>Record field identified by name and occurrence.</summary>
    RecordField
}

/// <summary>The namespace consulted by a lexical query.</summary>
public enum QueryKind
{
    /// <summary>Value or constructor reference.</summary>
    Value,
    /// <summary>Type reference.</summary>
    Type,
    /// <summary>Operator implementation.</summary>
    Operator,
    /// <summary>Potential shadowing of a module binding.</summary>
    ModuleBinding,
    /// <summary>Availability of a generated local name.</summary>
    FreshBinding
}

/// <summary>A lexical question. Qualifiers are syntax, never package or source identities.</summary>
public sealed record NameQuery(QueryKind Kind, string Qualifier, string Name);

/// <summary>A resolved answer uses declaration-local slots, not source identities.</summary>
public sealed record ResolutionAnswer(
    NameQuery Query,
    ResolutionKind Kind,
    int Slot = -1,
    string Symbol = "",
    string NativeModule = "",
    InterfaceDefinition? Interface = null);

/// <summary>Resolution outcomes, including negative and ambiguous observations.</summary>
public enum ResolutionKind
{
    /// <summary>Available source declaration.</summary>
    Declaration,
    /// <summary>Compiler-native facility.</summary>
    Native,
    /// <summary>API with type information only.</summary>
    TypeOnly,
    /// <summary>Missing symbol.</summary>
    MissingName,
    /// <summary>Missing module implementation.</summary>
    MissingModule,
    /// <summary>Declaration is not exposed.</summary>
    Private,
    /// <summary>Symbol exists only in the other namespace.</summary>
    WrongNamespace,
    /// <summary>Qualifier is not imported.</summary>
    UnimportedQualifier,
    /// <summary>Multiple visible candidates.</summary>
    Ambiguous,
    /// <summary>A binding exists.</summary>
    Present,
    /// <summary>A binding does not exist.</summary>
    Absent,
    /// <summary>A deferred qualified binding in the legacy whole-module presentation adapter.</summary>
    DeferredQualified
}

/// <summary>Exact input to the reusable canonicalization worker. No CST or provenance is retained.</summary>
public sealed record CanonicalizationInput(
    Declaration Syntax,
    ValueList<ResolutionAnswer> Answers,
    bool PreserveOperators);

/// <summary>A source-free reference edge, addressed from the owning declaration.</summary>
public sealed record ReferenceUse(int Slot, string Symbol, bool IsType, bool IsTypeOnly, SyntaxPath Path);

/// <summary>A diagnostic is a failed structural operation; presentation follows the caller's path.</summary>
public sealed record SyntaxDiagnostic(SyntaxPath Path, NameQuery Query, ResolutionKind Kind);

/// <summary>An output subtree corresponds to an input subtree even when lowering changes its shape.</summary>
public sealed record PathProjection(SyntaxPath Output, SyntaxPath Input);

/// <summary>Reusable canonical syntax and path-based observations.</summary>
public sealed record CanonicalDeclaration(
    Declaration Syntax,
    ValueList<ReferenceUse> References,
    ValueList<SyntaxDiagnostic> Diagnostics,
    ValueList<PathProjection> Projections);

/// <summary>A particular export from a shared, dependency-closed unit.</summary>
public sealed record UnitExport
{
    /// <summary>Selects an existing member of a validated immutable unit.</summary>
    public UnitExport(DeclarationUnit unit, int member)
    {
        if (member < 0 || member >= unit.Input.Members.Count)
            throw new ArgumentException("A dependency export selects a nonexistent member.", nameof(member));

        Unit = unit;
        Member = member;
    }

    /// <summary>The shared artifact, not a copy of its transitive closure.</summary>
    public DeclarationUnit Unit { get; }

    /// <summary>The selected own-member slot.</summary>
    public int Member { get; }

    /// <summary>The content address and member slot, independent of source ownership.</summary>
    public string Address => Unit.Address + ":" + Member;

    /// <inheritdoc/>
    public bool Equals(UnitExport? other) => other is not null && Member == other.Member && Unit.Equals(other.Unit);

    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Unit, Member);
}

/// <summary>Only own members and direct, already-closed dependencies are stored in an SCC.</summary>
public sealed record UnitInput(
    ValueList<Declaration> Members,
    ValueList<UnitExport> Dependencies,
    bool PreserveOperators)
{
    /// <summary>Type-only facilities expressed as local nominal templates, not executable implementations.</summary>
    public ValueList<InterfaceDefinition> Interfaces { get; init; } = [];

    /// <summary>Intrinsic runtime representations for own members; empty strings denote ordinary declarations.</summary>
    public ValueList<string> MemberIntrinsics { get; init; } = [];
}

/// <summary>A nominal type parameter in a type-only API; source ownership is supplied by the caller.</summary>
public sealed record NominalShape(string Name, int Arity);

/// <summary>Closed interface syntax and its local nominal parameters.</summary>
public sealed record InterfaceDefinition(Declaration Syntax, ValueList<NominalShape> Nominals);

/// <summary>A shared immutable SCC artifact. Source bindings are deliberately outside this model.</summary>
public sealed record DeclarationUnit
{
    internal DeclarationUnit(string address, UnitInput input)
    {
        Address = address;
        Input = input;
    }

    /// <summary>Validated content identity; dependency equality does not walk transitive bodies.</summary>
    public string Address { get; }

    /// <summary>Own syntax and direct, already-closed dependencies.</summary>
    public UnitInput Input { get; }

    /// <inheritdoc/>
    public bool Equals(DeclarationUnit? other) => other is not null && Address == other.Address;

    /// <inheritdoc/>
    public override int GetHashCode() => StringComparer.Ordinal.GetHashCode(Address);
}

/// <summary>Caller-owned, independently retained memoization of source-free transformations.</summary>
public sealed class DeclarationUnitCache : IMarkerCacheLifetime<long>
{
    private readonly Lock _unitGate = new();

    /// <summary>Atomically interns a unit without retaining duplicate representations on concurrent misses.</summary>
    public DeclarationUnit Intern(UnitInput input)
    {
        lock (_unitGate)
        {
            if (Units.TryGetValue(input, out var unit))
                return unit;

            unit = UnitConstruction.Create(input);
            Units.Set(input, unit);
            return unit;
        }
    }

    /// <summary>Cached declaration-local transforms.</summary>
    public MemoryCache<CanonicalizationInput, CanonicalDeclaration, long> Canonical { get; } = new(0);

    /// <summary>Cached syntax questions; consulting a scope does not transform dependency bodies.</summary>
    public MemoryCache<Declaration, ValueList<NameQuery>, long> Queries { get; } = new(0);

    /// <summary>Interned SCC artifacts.</summary>
    public MemoryCache<UnitInput, DeclarationUnit, long> Units { get; } = new(0);

    /// <summary>Source-independent recursive slot labeling.</summary>
    public MemoryCache<UnitInput, OrderedUnitInput, long> Orderings { get; } = new(0);

    /// <summary>Own-member compiler projections, keyed by source-free content address.</summary>
    public MemoryCache<string, ImmutableDictionary<DeclQualifiedName, Declaration>, long> Projections { get; } = new(0);

    /// <summary>Labels recursive slots without repeating this transformation on equal inputs.</summary>
    public OrderedUnitInput Order(UnitInput input)
    {
        if (Orderings.TryGetValue(input, out var ordered))
            return ordered;

        ordered = UnitOrdering.Apply(input);
        Orderings.Set(input, ordered);
        return ordered;
    }

    /// <summary>Source-free lowering artifacts; artifact names encode content and local slots only.</summary>
    public SourceCompilationCaches Lowering { get; }

    private readonly MemoryCache<SourceTransformationInput, Declaration, long> _operators = new(0);

    private readonly MemoryCache<SourceTransformationInput, LoweredSourceFunction, long> _functions = new(0);

    /// <summary>Creates independently owned storage; no global interning or history is required.</summary>
    public DeclarationUnitCache()
    {
        Lowering = new(_operators, _functions) { OperatorLifetime = _operators, SemanticLifetime = _functions };
    }

    /// <summary>Pure SCC emission results, independent of the calling source path.</summary>
    public MemoryCache<SccCompilationInput,
        Functional.Result<CompilationError, ImmutableDictionary<FunctionId, CompiledCallable>>, long> Emission
    { get; } = new(0);

    /// <summary>Number of retained canonical declaration/input combinations.</summary>
    public int Count => Canonical.Count;

    /// <inheritdoc/>
    public ICacheUsage SetMarker(long marker)
    {
        var usages =
            new[]
            {
                Canonical.SetMarker(marker), Queries.SetMarker(marker), Units.SetMarker(marker), Orderings.SetMarker(marker),
                Projections.SetMarker(marker), Emission.SetMarker(marker),
                Lowering.OperatorLifetime!.SetMarker(marker), Lowering.SemanticLifetime!.SetMarker(marker)
            };

        return new CombinedUsage(usages);
    }

    /// <inheritdoc/>
    public async Task TrimAsync(
        int budget,
        Func<long, CacheRetention> retention,
        CancellationToken cancellationToken = default)
    {
        await Canonical.TrimAsync(budget, retention, cancellationToken);
        await Queries.TrimAsync(budget, retention, cancellationToken);
        await Units.TrimAsync(budget, retention, cancellationToken);
        await Orderings.TrimAsync(budget, retention, cancellationToken);
        await Projections.TrimAsync(budget, retention, cancellationToken);
        await Emission.TrimAsync(budget, retention, cancellationToken);
        await Lowering.OperatorLifetime!.TrimAsync(budget, retention, cancellationToken);
        await Lowering.SemanticLifetime!.TrimAsync(budget, retention, cancellationToken);
    }

    /// <summary>Releases all retained semantic artifacts.</summary>
    public void Clear()
    {
        Canonical.Clear();
        Queries.Clear();
        Units.Clear();
        Orderings.Clear();
        Projections.Clear();
        Emission.Clear();
        _operators.Clear();
        _functions.Clear();
    }

    private sealed class CombinedUsage(ICacheUsage[] usages) : ICacheUsage
    {
        public int EntryCount => System.Linq.Enumerable.Sum(usages, usage => usage.EntryCount);
    }
}
