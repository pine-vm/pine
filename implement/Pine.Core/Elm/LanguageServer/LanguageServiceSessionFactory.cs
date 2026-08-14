using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Creates language-service sessions backed by the Elm language service program compiled for the
/// Pine VM.
/// </summary>
/// <param name="pineVMFactory">
/// Creates the virtual machine running the language service program.
/// </param>
/// <param name="compilationCache">
/// Optional store caching the compiled environment between sessions and processes.
/// </param>
/// <param name="logDelegate">Optional delegate receiving progress reports from compilation.</param>
public class LanguageServiceSessionFactory(
    Func<IPineVM> pineVMFactory,
    IFileStore? compilationCache = null,
    Action<string>? logDelegate = null)
    : ILanguageServiceSessionFactory
{
    /// <inheritdoc/>
    public ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var initResult =
            LanguageServiceState.InitLanguageServiceState(
                pineVMFactory(),
                compilationCache,
                logDelegate);

        if (initResult.IsErrOrNull() is { } err)
        {
            return
                ValueTask.FromResult(
                    Result<string, ILanguageServiceSession>.err(err));
        }

        if (initResult.IsOkOrNull() is not { } session)
        {
            throw new InvalidOperationException(
                "Unexpected language service state result type: " + initResult.GetType());
        }

        return
            ValueTask.FromResult(
                Result<string, ILanguageServiceSession>.ok(session));
    }
}
