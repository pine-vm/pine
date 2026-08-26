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
    private readonly Func<IInvocationCacheAccess, IPineVM> _pineVMFactory;

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
            _ => pineVMFactory(),
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

        ScheduledLanguageServiceSession.Worker CreateWorker()
        {
            var workerCache = new BufferedInvocationCacheAccess(sharedCache);
            var pineVM = _pineVMFactory(workerCache);

            return new ScheduledLanguageServiceSession.Worker(pineVM, workerCache);
        }

        var firstWorker = CreateWorker();

        var programResult =
            LanguageServiceState.CompileLanguageServiceProgram(
                firstWorker.PineVM,
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
                CreateWorker);

        return
            ValueTask.FromResult(
                Result<string, ILanguageServiceSession>.ok(session));
    }
}
