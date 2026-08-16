using AwesomeAssertions;
using Pine.Core.Files;
using Pine.Elm019;
using Pine.WebBrowserTesting;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using System;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class WebBrowserTestingTests
{
    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Html_document_screenshot_renders_html_css_and_svg_pixels()
    {
        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: TestWebBrowserExecutionMode());

        await using var page =
            await browser.CreatePageAsync(
                new WebBrowserContextOptions
                {
                    ViewportWidth = 120,
                    ViewportHeight = 90,
                    DeviceScaleFactor = 1,
                });

        var html =
            """
            <!doctype html>
            <html>
            <head>
              <meta charset="utf-8">
              <style>
                html, body { margin: 0; }
                .css-layout { width: 120px; height: 30px; background: rgb(0, 255, 0); }
                svg { display: block; }
              </style>
            </head>
            <body>
              <table width="120" height="30" border="0" cellpadding="0" cellspacing="0" bgcolor="#ff0000">
                <tr><td></td></tr>
              </table>
              <div class="css-layout"></div>
              <svg width="120" height="30" xmlns="http://www.w3.org/2000/svg">
                <rect width="120" height="30" fill="#0000ff" />
              </svg>
            </body>
            </html>
            """;

        await page.LoadHtmlAsync(Encoding.UTF8.GetBytes(html));

        var screenshot = await page.TakeScreenshotAsync();

        using var image = Image.Load<Rgba32>(screenshot.Span);

        image.Width.Should().Be(120);
        image.Height.Should().Be(90);

        var redPixels = 0;
        var greenPixels = 0;
        var bluePixels = 0;

        for (var y = 0; y < image.Height; y++)
        {
            for (var x = 0; x < image.Width; x++)
            {
                var pixel = image[x, y];

                if (pixel is { R: 255, G: 0, B: 0, A: 255 })
                    redPixels++;

                if (pixel is { R: 0, G: 255, B: 0, A: 255 })
                    greenPixels++;

                if (pixel is { R: 0, G: 0, B: 255, A: 255 })
                    bluePixels++;
            }
        }

        redPixels.Should().BeGreaterThan(1000);
        greenPixels.Should().BeGreaterThan(1000);
        bluePixels.Should().BeGreaterThan(1000);
    }

    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Container_loads_in_memory_html_and_snapshot_contains_javascript_dom_mutation()
    {
        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: TestWebBrowserExecutionMode());

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

    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Elm_app_initial_state_can_be_screenshot_and_updates_after_browser_inputs()
    {
        var elmJson =
            """
            {
                "type": "application",
                "source-directories": [ "src" ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/browser": "1.0.2",
                        "elm/core": "1.0.5",
                        "elm/html": "1.0.0"
                    },
                    "indirect": {
                        "elm/json": "1.1.3",
                        "elm/time": "1.0.0",
                        "elm/url": "1.0.0",
                        "elm/virtual-dom": "1.0.3"
                    }
                },
                "test-dependencies": {
                    "direct": {},
                    "indirect": {}
                }
            }
            """;

        var elmModule =
            """
            module Counter exposing (counterApp)

            import Browser
            import Html
            import Html.Attributes
            import Html.Events


            type alias Model =
                { count : Int
                , name : String
                }


            type Msg
                = Increment
                | SetName String


            counterApp : Program () Model Msg
            counterApp =
                Browser.sandbox
                    { init = { count = 0, name = "anonymous" }
                    , update = update
                    , view = view
                    }


            update : Msg -> Model -> Model
            update msg model =
                case msg of
                    Increment ->
                        { model | count = model.count + 1 }

                    SetName name ->
                        { model | name = name }


            view : Model -> Html.Html Msg
            view model =
                Html.main_ []
                    [ Html.input
                        [ Html.Attributes.attribute "data-testid" "name-input"
                        , Html.Attributes.value model.name
                        , Html.Events.onInput SetName
                        ]
                        []
                    , Html.button
                        [ Html.Attributes.attribute "data-testid" "increment"
                        , Html.Events.onClick Increment
                        ]
                        [ Html.text "Increment" ]
                    , Html.p
                        [ Html.Attributes.attribute "data-testid" "status" ]
                        [ Html.text
                            ("Count: "
                                ++ String.fromInt model.count
                                ++ "; Name: "
                                ++ model.name
                            )
                        ]
                    ]
            """;

        var sourceFiles =
            FileTree.SortedDirectory(
                [
                ("elm.json", FileTree.File(Encoding.UTF8.GetBytes(elmJson))),
                ("src",
                FileTree.SortedDirectory(
                    [
                    ("Counter.elm", FileTree.File(Encoding.UTF8.GetBytes(elmModule))),
                    ])),
                ]);

        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: TestWebBrowserExecutionMode());

        await using var page =
            await browser.CreateElmAppPageAsync(
                sourceFiles,
                ["src", "Counter.elm"],
                entryPointDeclarationName: "counterApp",
                browserContextOptions:
                new WebBrowserContextOptions
                {
                    ViewportWidth = 640,
                    ViewportHeight = 480,
                    DeviceScaleFactor = 1,
                });

        var initialScreenshot = await page.TakeScreenshotAsync();

        initialScreenshot.Length.Should().BeGreaterThan(8);
        Convert.ToHexString(initialScreenshot.Span[..8]).Should().Be("89504E470D0A1A0A");

        var status = page.GetByTestId("status");

        (await status.GetTextAsync()).Should().Be("Count: 0; Name: anonymous");

        await page.GetByTestId("name-input").FillAsync("Elm");
        await page.GetByTestId("increment").ClickAsync();
        await page.GetByTestId("increment").ClickAsync();

        await page.WaitForReadyAsync(
            "() => document.querySelector('[data-testid=\"status\"]')?.textContent === " +
            "'Count: 2; Name: Elm'");

        (await status.GetTextAsync()).Should().Be("Count: 2; Name: Elm");
    }

    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Render_wait_failure_reports_image_and_page_diagnostics()
    {
        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: TestWebBrowserExecutionMode());

        await using var page = await browser.CreatePageAsync();

        var html =
            """
            <!doctype html>
            <html>
            <head><title>Broken image diagnostics</title></head>
            <body><img src="/missing-image.png" alt="missing"></body>
            </html>
            """;

        await page.LoadHtmlAsync(Encoding.UTF8.GetBytes(html));

        Func<Task> takeScreenshot = async () => await page.TakeScreenshotAsync();

        var exception =
            await takeScreenshot.Should().ThrowAsync<WebBrowserOperationException>();

        exception.Which.Message.Should().Contain("Images failed to load or decode");
        exception.Which.Diagnostics.CollectionErrors.Should().BeEmpty();
        exception.Which.Diagnostics.Document.Should().NotBeNull();
        exception.Which.Diagnostics.Document!.Title.Should().Be("Broken image diagnostics");
        exception.Which.Diagnostics.Document.Images.Should().ContainSingle();
        exception.Which.Diagnostics.Document.Images[0].Complete.Should().BeTrue();
        exception.Which.Diagnostics.Document.Images[0].NaturalWidth.Should().Be(0);

        exception.Which.Diagnostics.RequestFailures
            .Should()
            .Contain(request => request.Url.EndsWith("/missing-image.png", StringComparison.Ordinal));
    }

    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Failure_artifact_capture_keeps_diagnostics_when_visual_artifacts_fail()
    {
        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: TestWebBrowserExecutionMode());

        await using var page = await browser.CreatePageAsync();
        await page.AdvancedPage.CloseAsync();

        var artifacts =
            await page.CaptureFailureArtifactsAsync(
                new WebBrowserFailureArtifactOptions
                {
                    CaptureTrace = false,
                });

        artifacts.DomSnapshot.Should().BeEmpty();
        artifacts.Screenshot.Length.Should().Be(0);
        artifacts.CaptureErrors.Should().Contain(error => error.StartsWith("DOM snapshot:"));
        artifacts.CaptureErrors.Should().Contain(error => error.StartsWith("Screenshot:"));
        artifacts.Diagnostics.PageClosed.Should().BeTrue();
        artifacts.Diagnostics.CollectionErrors.Should().NotBeEmpty();
    }

    internal static WebBrowserExecutionMode TestWebBrowserExecutionMode()
    {
        if (string.Equals(
            Environment.GetEnvironmentVariable("GITHUB_ACTIONS"),
            "true",
            StringComparison.OrdinalIgnoreCase))
        {
            if (Environment.OSVersion.Platform is not PlatformID.Unix)
            {
                return WebBrowserExecutionMode.Host;
            }
        }

        return WebBrowserExecutionMode.Container;
    }
}
