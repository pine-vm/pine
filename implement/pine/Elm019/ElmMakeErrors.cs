using Pine.Core.Elm;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Elm019;

public abstract record ElmMakeFile
{
    public sealed record Application(ImmutableArray<string> Path) : ElmMakeFile;

    public sealed record Package(ElmPackageIdentity Identity, ImmutableArray<string> Path) : ElmMakeFile;
}

public sealed record ElmMakePosition
{
    public int Line { get; }

    public int Column { get; }

    public ElmMakePosition(int line, int column)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(line, 1);
        ArgumentOutOfRangeException.ThrowIfLessThan(column, 1);
        Line = line;
        Column = column;
    }
}

/// <summary>One-based Unicode character positions (a tab counts as one); inclusive start, exclusive end.</summary>
public sealed record ElmMakeRegion
{
    public ElmMakePosition Start { get; }

    public ElmMakePosition End { get; }

    public ElmMakeRegion(ElmMakePosition start, ElmMakePosition end)
    {
        ArgumentNullException.ThrowIfNull(start);
        ArgumentNullException.ThrowIfNull(end);

        if (end.Line < start.Line || end.Line == start.Line && end.Column < start.Column)
            throw new ArgumentException("A source region must not end before it starts.", nameof(end));

        Start = start;
        End = end;
    }
}

public abstract record ElmMakeLocation
{
    public sealed record Project : ElmMakeLocation;

    public sealed record File(ElmMakeFile FileId) : ElmMakeLocation;

    public sealed record Source(ElmMakeFile FileId, ElmMakeRegion Region) : ElmMakeLocation;
}

public enum ElmMakeProblemCode
{
    Syntax, Manifest, NoManifest, EntryPointNotFound, NoMain, CyclicTypeAlias, DebugRemnants,
    UnknownValue, UnknownType, MissingModule, AmbiguousModule, NotExposed, AmbiguousName,
    DuplicateModule, DuplicateDeclaration, DuplicateBinding, ShadowedBinding,
    WrongConstructorArity, NotAConstructor, InvalidExposure, RestrictedKernelImport,
}

/// <summary>Semantic diagnostic identity and payload; presentation belongs to the consuming UI/test framework.</summary>
public sealed record ElmMakeProblemKind(
    ElmMakeProblemCode Code,
    ImmutableArray<string> Names,
    string? Details = null,
    int? ExpectedArity = null,
    int? ActualArity = null);

public sealed record ElmMakeProblem(ElmMakeLocation Location, ElmMakeProblemKind Kind);

/// <summary>A rejected compilation always has at least one diagnostic, retaining order and duplicates.</summary>
public sealed class ElmMakeProblems : IReadOnlyList<ElmMakeProblem>
{
    private readonly ImmutableArray<ElmMakeProblem> _items;

    public ElmMakeProblems(ElmMakeProblem first, IEnumerable<ElmMakeProblem> rest)
    {
        ArgumentNullException.ThrowIfNull(first);
        _items = [first, .. rest];

        if (_items.Any(item => item is null))
            throw new ArgumentException("Diagnostic collections cannot contain null.", nameof(rest));
    }

    public ElmMakeProblem this[int index] => _items[index];

    public int Count => _items.Length;

    public IEnumerator<ElmMakeProblem> GetEnumerator() => ((IEnumerable<ElmMakeProblem>)_items).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

public enum ElmMakeCapability
{
    Ports, EffectManagers, Glsl, CustomOperator, RecursiveValueInitialization,
    RecursiveMainType, PartialMainType, MainTypeSubstitution, HigherOrderMain, UnsupportedMainType,
    NonHtmlMain, RefutableArgumentPattern, RefutableLetPattern, ConstructorFieldCount, TupleArity,
    FloatPatterns, IncompleteCase, UnsupportedFlags,
}

public sealed record ElmMakeUnsupportedProblem(
    ElmMakeLocation Location,
    ElmMakeCapability Feature,
    string? Name = null,
    int? Arity = null);

public enum ElmMakeInternalProblemKind
{
    MissingPackageSources, InvalidPackageSources, InvalidRequest, InvalidSourceLocation,
    MissingGraphNode, CompilerInvariant, PackageResolution, PineVM, Interpreter,
}

public sealed record ElmMakeInternalProblem(ElmMakeInternalProblemKind Kind, string Context);

public abstract record ElmMakeError
{
    public sealed record Diagnostics(ElmMakeProblems Problems) : ElmMakeError;

    public sealed record Unsupported(ElmMakeUnsupportedProblem Problem) : ElmMakeError;

    public sealed record Internal(ElmMakeInternalProblem Problem) : ElmMakeError;
}
