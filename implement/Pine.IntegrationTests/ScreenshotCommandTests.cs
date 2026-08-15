using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.Files;
using Pine.WebBrowserTesting;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class ScreenshotCommandTests
{
    [Fact(Timeout = 1000 * 60 * 4)]
    public async Task Elm_app_screenshot_embeds_image_from_compilation_interface()
    {
        const int CellsPerSide = 4;
        const int CellSize = 10;
        const int ImageSize = CellsPerSide * CellSize;

        var cellColors =
            new Rgba32[,]
            {
                {
                    new(230, 25, 75), new(60, 180, 75), new(255, 225, 25), new(0, 130, 200),
                },
                {
                    new(245, 130, 48), new(145, 30, 180), new(70, 240, 240), new(240, 50, 230),
                },
                {
                    new(210, 245, 60), new(250, 190, 212), new(0, 128, 128), new(220, 190, 255),
                },
                {
                    new(170, 110, 40), new(255, 250, 200), new(128, 0, 0), new(0, 0, 128),
                },
            };

        ReadOnlyMemory<byte> embeddedImageBytes;

        using (var embeddedImage = new Image<Rgba32>(ImageSize, ImageSize))
        {
            for (var cellY = 0; cellY < CellsPerSide; cellY++)
            {
                for (var cellX = 0; cellX < CellsPerSide; cellX++)
                {
                    for (var y = cellY * CellSize; y < (cellY + 1) * CellSize; y++)
                    {
                        for (var x = cellX * CellSize; x < (cellX + 1) * CellSize; x++)
                            embeddedImage[x, y] = cellColors[cellY, cellX];
                    }
                }
            }

            using var embeddedImageStream = new MemoryStream();
            embeddedImage.SaveAsPng(embeddedImageStream);
            embeddedImageBytes = embeddedImageStream.ToArray();
        }

        var elmJson =
            """
            {
                "type": "application",
                "source-directories": [ "src" ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/bytes": "1.0.8",
                        "elm/core": "1.0.5",
                        "elm/html": "1.0.0"
                    },
                    "indirect": {
                        "elm/json": "1.1.3",
                        "elm/virtual-dom": "1.0.3"
                    }
                },
                "test-dependencies": {
                    "direct": {},
                    "indirect": {}
                }
            }
            """;

        var sourceFilesModule =
            """
            module CompilationInterface.SourceFiles exposing (..)


            file____embedded_image_png : { base64 : String }
            file____embedded_image_png =
                { base64 = "The compiler replaces this value." }
            """;

        var mainModule =
            """
            module Main exposing (main)

            import CompilationInterface.SourceFiles
            import Html
            import Html.Attributes


            main : Html.Html msg
            main =
                Html.img
                    [ Html.Attributes.src
                        ("data:image/png;base64,"
                            ++ CompilationInterface.SourceFiles.file____embedded_image_png.base64
                        )
                    , Html.Attributes.width 40
                    , Html.Attributes.height 40
                    , Html.Attributes.style "display" "block"
                    ]
                    []
            """;

        var sourceFiles =
            FileTree.SortedDirectory(
                [
                ("elm.json", FileTree.File(Encoding.UTF8.GetBytes(elmJson))),
                ("embedded-image.png", FileTree.File(embeddedImageBytes)),
                ("src",
                    FileTree.SortedDirectory(
                        [
                        ("CompilationInterface",
                            FileTree.SortedDirectory(
                                [
                                ("SourceFiles.elm", FileTree.File(Encoding.UTF8.GetBytes(sourceFilesModule))),
                                ])),
                        ("Main.elm", FileTree.File(Encoding.UTF8.GetBytes(mainModule))),
                        ])),
                ]);

        var screenshotBytes =
            await ScreenshotCommand.TakeElmAppScreenshotAsync(
                sourceFiles,
                ["src", "Main.elm"],
                executionMode: WebBrowserTestingTests.TestWebBrowserExecutionMode(),
                browserContextOptions:
                new WebBrowserContextOptions
                {
                    ViewportWidth = ImageSize,
                    ViewportHeight = ImageSize,
                    DeviceScaleFactor = 1,
                });

        using var screenshot = Image.Load<Rgba32>(screenshotBytes.Span);

        screenshot.Width.Should().Be(ImageSize);
        screenshot.Height.Should().Be(ImageSize);

        for (var cellY = 0; cellY < CellsPerSide; cellY++)
        {
            for (var cellX = 0; cellX < CellsPerSide; cellX++)
            {
                screenshot[
                    cellX * CellSize + CellSize / 2,
                    cellY * CellSize + CellSize / 2]
                .Should().Be(cellColors[cellY, cellX]);
            }
        }
    }

    [Fact]
    public void Default_output_path_for_html_includes_time_file_name_and_width()
    {
        var outputPath =
            ScreenshotCommand.BuildDefaultOutputPath(
                Path.Combine("pages", "landing.html"),
                declarationName: null,
                viewportWidth: 1280,
                WebBrowserScreenshotImageFormat.Png,
                new DateTimeOffset(2026, 8, 15, 16, 32, 10, TimeSpan.Zero));

        outputPath.Should().Be(
            "2026-08-15T16-32-10-screenshot-landing-1280px.png");
    }

    [Fact]
    public void Default_output_path_for_elm_includes_default_declaration_name()
    {
        var outputPath =
            ScreenshotCommand.BuildDefaultOutputPath(
                Path.Combine("src", "AppModule.elm"),
                declarationName: null,
                viewportWidth: 1280,
                WebBrowserScreenshotImageFormat.Png,
                new DateTimeOffset(2026, 8, 15, 16, 32, 10, TimeSpan.Zero));

        outputPath.Should().Be(
            "2026-08-15T16-32-10-screenshot-AppModule-main-1280px.png");
    }

    [Fact]
    public void Default_output_path_includes_custom_declaration_width_and_image_type()
    {
        var outputPath =
            ScreenshotCommand.BuildDefaultOutputPath(
                Path.Combine("src", "AppModule.elm"),
                declarationName: "preview",
                viewportWidth: 800,
                WebBrowserScreenshotImageFormat.Jpeg,
                new DateTimeOffset(2026, 8, 15, 16, 32, 10, TimeSpan.Zero));

        outputPath.Should().Be(
            "2026-08-15T16-32-10-screenshot-AppModule-preview-800px.jpeg");
    }
}
