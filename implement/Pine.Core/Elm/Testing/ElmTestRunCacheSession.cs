using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Caller-owned, unbounded caches for repeated Elm test runs. Retain this object, not VMs,
/// instrumentation, cancellation state, or test reports. Native leaves must remain immutable.
/// </summary>
public sealed class ElmTestRunCacheSession
{
    /// <summary>Compilation, parsing, encoding and reduction caches shared by preparation and workers.</summary>
    public PineVMSharedCaches VmCaches { get; } = new();

    /// <summary>Successful function invocations, keyed by expression and complete frame input.</summary>
    public ConcurrentInvocationCache InvocationResults { get; } = new();

    /// <summary>Source parsing keyed by complete source text, never by path or timestamp.</summary>
    public IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>> SourceParsing { get; } =
        new ConcurrentDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>(StringComparer.Ordinal);

    /// <summary>Declaration canonicalization keyed by structural declaration and its consulted scope.</summary>
    public IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope), DeclarationCanonicalizationCacheEntry>
        DeclarationCanonicalization
    { get; } =
        new ConcurrentDictionary<(Syntax.Node<Syntax.Declaration>, string), DeclarationCanonicalizationCacheEntry>();

    internal Result<ElmSyntaxParseError, Syntax.File> ParseSource(string text)
    {
        if (SourceParsing.TryGetValue(text, out var parsed))
            return parsed;

        var result = ElmSyntaxParser.ParseModuleText(text);

        if (result.IsOkOrNull() is not null)
            SourceParsing[text] = result;

        return result;
    }

    internal IReadOnlyDictionary<PineValue, PrecompiledLeaf> PrecompiledLeaves { get; }

    /// <summary>Creates a session with a fixed native-leaf configuration.</summary>
    public ElmTestRunCacheSession(IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null)
    {
        PrecompiledLeaves = precompiledLeaves ?? SetupVM.DefaultPrecompiledLeaves;
    }

    internal void ValidateInstrumentation(ElmTestInstrumentation? instrumentation)
    {
        if (instrumentation is null)
            return;

        if (instrumentation.RecordDiagnostics)
        {
            throw new ArgumentException(
                "Diagnostic profiles require isolated caches; do not supply a cache session.",
                nameof(instrumentation));
        }

        var options = instrumentation.Options;

        if (options.DisableReduction || options.DisableInlining || options.DisableTailRecursion ||
            options.DisablePrecompiledLeaves ||
            options.DisableApplicationFastPaths || options.DisableInvocationCache ||
            !ReferenceEquals(instrumentation.ResolvePrecompiledLeaves(), PrecompiledLeaves))
        {
            throw new ArgumentException(
                "A cache session requires the same default VM compilation controls and native leaves for every run.",
                nameof(instrumentation));
        }
    }
}
