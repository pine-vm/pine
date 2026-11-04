using Pine.Core.Caching;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.Threading.Tasks;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Caller-owned syntax-preserving discovery caches. Canonicalization validates its observed resolution
/// inputs and re-demands dependencies; prepared values contain only pure, unreduced declaration data.
/// Keep memory tiers session-local; a persistent prepared-declaration backing store may be shared.
/// </summary>
public sealed record ElmTestDiscoveryCaches(
    Canonicalization.DeclarationCache CanonicalDeclarations,
    ICache<PreparedDeclarationInput, PreparedSourceDeclaration> PreparedDeclarations)
{
    private readonly CacheInvocationLifetime _lifetime = new();

    /// <summary>Optional control of the prepared cache's memory tier.</summary>
    public IMarkerCacheLifetime<long>? PreparedLifetime { get; init; }

    /// <summary>Creates bounded, thread-safe memory tiers, optionally backed by a file-store subtree.</summary>
    public static ElmTestDiscoveryCaches CreateInMemory(
        IFileStore? backingStore = null, IImmutableList<string>? directory = null)
    {
        var prepared = new MemoryCache<PreparedDeclarationInput, PreparedSourceDeclaration, long>(0);

        return
            new(
                new(),
                backingStore is null
                ?
                prepared
                :
                new LayeredCache<PreparedDeclarationInput, PreparedSourceDeclaration>(
                    prepared,
                    new FileStoreCache<PreparedDeclarationInput, PreparedSourceDeclaration>(
                        backingStore,
                        (directory ?? ImmutableList<string>.Empty).Add("prepared-declarations-v1"),
                        input => input.ComputeHash(),
                        PreparedSourceDeclaration.Codec)))
            {
                PreparedLifetime = prepared
            };
    }

    /// <summary>Starts independent marker/working-set accounting for one discovery, including retries.</summary>
    public IDisposable BeginInvocation() =>
        _lifetime.BeginInvocation(CanonicalDeclarations, PreparedLifetime);

    /// <summary>Waits for cleanup of both tiers without owning/discarding the supplied storage.</summary>
    public Task WaitForMaintenanceAsync() => _lifetime.WaitForMaintenanceAsync();
}
