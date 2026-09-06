using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Creates language-service sessions backed by a bounded pool of Pine VM workers.
/// </summary>
public class LanguageServiceSessionFactory : ILanguageServiceSessionFactory
{
    private readonly Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM>
        _pineVMFactory;

    private readonly IFileStore? _compilationCache;

    private readonly Action<string>? _logDelegate;

    /// <summary>
    /// Creates a factory using VMs that do not consume the supplied shared-cache access.
    /// </summary>
    public LanguageServiceSessionFactory(
        Func<IPineVM> pineVMFactory,
        IFileStore? compilationCache = null,
        Action<string>? logDelegate = null)
        : this(
            (Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM>)
            ((_, _) => pineVMFactory()),
            compilationCache,
            logDelegate)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);
    }

    /// <summary>
    /// Creates a factory whose VM instances use worker-local cache access.
    /// </summary>
    public LanguageServiceSessionFactory(
        Func<IInvocationCacheAccess, IPineVM> pineVMFactory,
        IFileStore? compilationCache = null,
        Action<string>? logDelegate = null)
        : this(
            (Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM>)
            ((invocationCache, _) => pineVMFactory(invocationCache)),
            compilationCache,
            logDelegate)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);
    }

    /// <summary>
    /// Creates a factory whose VM instances use shared invocation and expression-compilation caches.
    /// </summary>
    public LanguageServiceSessionFactory(
        Func<IInvocationCacheAccess, ConcurrentExpressionCompilationCache, IPineVM> pineVMFactory,
        IFileStore? compilationCache = null,
        Action<string>? logDelegate = null)
        : this(
            (invocationCache, sharedCaches) =>
            pineVMFactory(invocationCache, sharedCaches.ExpressionCompilations),
            compilationCache,
            logDelegate)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);
    }

    /// <summary>
    /// Creates a factory whose VM instances use shared invocation, expression-compilation, and parse caches.
    /// </summary>
    public LanguageServiceSessionFactory(
        Func<
            IInvocationCacheAccess,
            ConcurrentExpressionCompilationCache,
            PineVMParseCache,
            IPineVM>
        pineVMFactory,
        IFileStore? compilationCache = null,
        Action<string>? logDelegate = null)
        : this(
            (invocationCache, sharedCaches) =>
            pineVMFactory(
                invocationCache,
                sharedCaches.ExpressionCompilations,
                sharedCaches.ParsedExpressions),
            compilationCache,
            logDelegate)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);
    }

    /// <summary>
    /// Creates a factory whose VM instances consume a complete set of shared caches.
    /// </summary>
    public LanguageServiceSessionFactory(
        Func<IInvocationCacheAccess, PineVMSharedCaches, IPineVM> pineVMFactory,
        IFileStore? compilationCache = null,
        Action<string>? logDelegate = null)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);

        _pineVMFactory = pineVMFactory;
        _compilationCache = compilationCache;
        _logDelegate = logDelegate;
    }

    /// <inheritdoc/>
    public ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
        LanguageServerOptions options,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(options);
        cancellationToken.ThrowIfCancellationRequested();

        var sharedCache = new ConcurrentInvocationCache();
        var sharedPineVMCaches = new PineVMSharedCaches();

        ScheduledLanguageServiceSession.Worker CreateWorker()
        {
            var workerCache = new BufferedInvocationCacheAccess(sharedCache);

            return
                new ScheduledLanguageServiceSession.Worker(
                    CreatePineVM: () => _pineVMFactory(workerCache, sharedPineVMCaches),
                    InvocationCache: workerCache);
        }

        var firstWorker = CreateWorker();
        var initializationPineVM = firstWorker.CreatePineVM();

        var programResult =
            LanguageServiceState.CompileLanguageServiceProgram(
                initializationPineVM,
                _compilationCache,
                _logDelegate);

        firstWorker.InvocationCache.MergeIntoShared();

        if (programResult.IsErrOrNull() is { } err)
        {
            return
                ValueTask.FromResult(
                    Result<string, ILanguageServiceSession>.err(err));
        }

        if (programResult.IsOkOrNull() is not { } program)
        {
            throw new InvalidOperationException(
                "Unexpected language service program result type: " + programResult.GetType());
        }

        var session =
            new ScheduledLanguageServiceSession(
                program,
                options.MaxConcurrencyCount,
                firstWorker,
                CreateWorker,
                _logDelegate);

        return
            ValueTask.FromResult(
                Result<string, ILanguageServiceSession>.ok(session));
    }
}
