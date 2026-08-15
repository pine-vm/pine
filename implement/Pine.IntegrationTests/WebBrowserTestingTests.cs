using AwesomeAssertions;
using Pine.WebBrowserTesting;
using System;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class WebBrowserTestingTests
{
    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Browser_loads_in_memory_html_and_snapshot_contains_javascript_dom_mutation()
    {
        var executionMode =
            string.Equals(
                Environment.GetEnvironmentVariable("GITHUB_ACTIONS"),
                "true",
                StringComparison.OrdinalIgnoreCase)
            ?
            WebBrowserExecutionMode.Host
            :
            WebBrowserExecutionMode.Container;

        await using var browser =
            await WebBrowserInstance.StartAsync(executionMode);

        await using var page =
            await browser.CreatePageAsync(
                new WebBrowserContextOptions
                {
                    ViewportWidth = 800,
                    ViewportHeight = 600,
                    DeviceScaleFactor = 2,
                });

        var html =
            """
            <!doctype html>
            <html>
            <head>
              <meta charset="utf-8">
              <style>
                #status { color: rgb(17, 34, 51); }
              </style>
            </head>
            <body>
              <main>
                <p id="status" data-testid="status">before script</p>
              </main>
              <script>
                const status = document.querySelector("#status");
                status.textContent = "mutated by JavaScript";
                status.dataset.ready = "true";
              </script>
            </body>
            </html>
            """;

        await page.LoadHtmlAsync(
            Encoding.UTF8.GetBytes(html),
            new HtmlDocumentOptions
            {
                Url = new("http://pine.test/#/integration-test"),
            });

        await page.WaitForReadyAsync("() => document.querySelector('#status')?.dataset.ready === 'true'");

        var domSnapshot = await page.GetDomSnapshotAsync();

        domSnapshot.Should().Contain("mutated by JavaScript");
        domSnapshot.Should().Contain("data-ready=\"true\"");

        var status = page.GetByTestId("status");
        (await status.GetTextAsync()).Should().Be("mutated by JavaScript");
        (await status.GetComputedStyleAsync("color")).Should().Be("rgb(17, 34, 51)");
    }
}
