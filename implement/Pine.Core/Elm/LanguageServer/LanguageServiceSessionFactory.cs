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

    private readonly Func<ContinuousInvocationCacheLearning>? _createCacheLearning;

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
        Action<string>? logDelegate = null,
        Func<ContinuousInvocationCacheLearning>? createCacheLearning = null)
    {
        ArgumentNullException.ThrowIfNull(pineVMFactory);

        _pineVMFactory = pineVMFactory;
        _compilationCache = compilationCache;
        _logDelegate = logDelegate;
        _createCacheLearning = createCacheLearning;
    }

    /// <inheritdoc/>
    public ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
        LanguageServerOptions options,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(options);
        cancellationToken.ThrowIfCancellationRequested();

        var sharedCache = new GenerationalInvocationCache(options.InvocationCacheOptions);
        var sharedPineVMCaches = new PineVMSharedCaches();
        var cacheStatistics = new InvocationCacheStatistics();
        var cacheLearning = _createCacheLearning?.Invoke();

        InvocationCacheExecutionContext<IPineVM> CreateWorker() =>
            new(
                sharedCache,
                buffer =>
                {
                    var vm = _pineVMFactory(buffer, sharedPineVMCaches);
                    return cacheLearning?.Wrap(vm) ?? vm;
                },
                onCompleted: () => _ = RequestInvocationCacheMaintenance(sharedCache, _logDelegate),
                statistics: cacheStatistics,
                getPolicy: cacheLearning is null ? null : () => cacheLearning.CurrentPolicy);

        var firstWorker = CreateWorker();

        Result<string, LanguageServiceState.LanguageServiceProgram> programResult;

        try
        {
            programResult =
                firstWorker.Run(
                    pineVM =>
                    LanguageServiceState.CompileLanguageServiceProgram(
                        pineVM,
                        _compilationCache,
                        _logDelegate));
        }
        catch
        {
            cacheLearning?.Dispose();
            throw;
        }

        if (programResult.IsErrOrNull() is { } err)
        {
            cacheLearning?.Dispose();

            return
                ValueTask.FromResult(
                    Result<string, ILanguageServiceSession>.err(err));
        }

        if (programResult.IsOkOrNull() is not { } program)
        {
            cacheLearning?.Dispose();

            throw new InvalidOperationException(
                "Unexpected language service program result type: " + programResult.GetType());
        }

        var session =
            new ScheduledLanguageServiceSession(
                program,
                options.MaxConcurrencyCount,
                firstWorker,
                CreateWorker,
                _logDelegate,
                cacheLearning);

        return
            ValueTask.FromResult(
                Result<string, ILanguageServiceSession>.ok(session));
    }

    internal static Task RequestInvocationCacheMaintenance(
        GenerationalInvocationCache cache,
        Action<string>? logDelegate = null)
    {
        if (cache.NewestGenerationEntryCount < cache.Options.RotationEntryCount)
            return Task.CompletedTask;

        return ObserveMaintenance(cache.MaintainAsync());

        async Task ObserveMaintenance(Task maintenance)
        {
            try
            {
                await maintenance.ConfigureAwait(false);
            }
            catch (Exception exception)
            {
                logDelegate?.Invoke("Invocation-cache maintenance failed: " + exception);
            }
        }
    }
}
