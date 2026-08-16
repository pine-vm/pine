using Pine.Core.Files;
using Pine.WebBrowserTesting;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

public static class ElmAppWebBrowser
{
    public static async Task<WebBrowserPage> CreateElmAppPageAsync(
        this WebBrowserInstance browser,
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath,
        string? entryPointDeclarationName = null,
        WebBrowserContextOptions? browserContextOptions = null,
        CancellationToken cancellationToken = default,
        WebBrowserRenderWaitOptions? renderWaitOptions = null)
    {
        ArgumentNullException.ThrowIfNull(browser);
        ArgumentNullException.ThrowIfNull(sourceFiles);
        ArgumentNullException.ThrowIfNull(entryPointFilePath);

        var htmlDocument =
            await ElmAppCompilationToHtml.CompileHtmlDocumentAsync(
                sourceFiles,
                entryPointFilePath,
                entryPointDeclarationName,
                cancellationToken);

        var page =
            await browser.CreatePageAsync(browserContextOptions, cancellationToken)
            .ConfigureAwait(false);

        try
        {
            await page.LoadHtmlAsync(htmlDocument, cancellationToken: cancellationToken)
            .ConfigureAwait(false);

            await page.WaitForReadyAsync(
                "() => document.documentElement.dataset.elmAppReady === 'true'",
                cancellationToken: cancellationToken)
            .ConfigureAwait(false);

            await page.WaitForRenderReadyAsync(renderWaitOptions, cancellationToken)
            .ConfigureAwait(false);

            return page;
        }
        catch
        {
            await page.DisposeAsync().ConfigureAwait(false);
            throw;
        }
    }

}
