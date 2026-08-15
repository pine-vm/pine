using Pine.Core.Files;
using Pine.Elm019;
using Pine.WebBrowserTesting;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.CLI;

public static class ScreenshotCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "screenshot",
                "Render an HTML, SVG, or Elm entry point and save a screenshot.");

        var entryPointArgument = new Argument<string>("entry-point");
        var declarationNameOption = new Option<string?>("--declaration-name");
        var outputOption = new Option<string?>("--output", ["-o"]);

        var imageFormatOption =
            new Option<string?>("--type", ["--format"])
            {
                Description = "Image encoding: png (default) or jpeg.",
            };

        var qualityOption =
            new Option<int?>("--quality")
            {
                Description = "JPEG quality from 0 to 100.",
            };

        var viewportWidthOption =
            new Option<int?>("--viewport-width")
            {
                Description = "Viewport width in CSS pixels. Defaults to 1280.",
            };

        var viewportHeightOption =
            new Option<int?>("--viewport-height")
            {
                Description = "Viewport height in CSS pixels. Defaults to 720.",
            };

        var deviceScaleFactorOption =
            new Option<float?>("--device-scale-factor")
            {
                Description = "Device scale factor. Defaults to 1.",
            };

        var screenWidthOption = new Option<int?>("--screen-width");
        var screenHeightOption = new Option<int?>("--screen-height");
        var isMobileOption = new Option<bool>("--is-mobile");
        var hasTouchOption = new Option<bool>("--has-touch");
        var disableJavaScriptOption = new Option<bool>("--disable-javascript");
        var localeOption = new Option<string?>("--locale");
        var timezoneOption = new Option<string?>("--timezone");
        var userAgentOption = new Option<string?>("--user-agent");

        var colorSchemeOption =
            new Option<string?>("--color-scheme")
            {
                Description = "Emulated color scheme: no-preference (default), light, or dark.",
            };

        var reducedMotionOption = new Option<bool>("--reduced-motion");
        var fullPageOption = new Option<bool>("--full-page");
        var omitBackgroundOption = new Option<bool>("--omit-background");

        command.Add(entryPointArgument);
        command.Add(declarationNameOption);
        command.Add(outputOption);
        command.Add(imageFormatOption);
        command.Add(qualityOption);
        command.Add(viewportWidthOption);
        command.Add(viewportHeightOption);
        command.Add(deviceScaleFactorOption);
        command.Add(screenWidthOption);
        command.Add(screenHeightOption);
        command.Add(isMobileOption);
        command.Add(hasTouchOption);
        command.Add(disableJavaScriptOption);
        command.Add(localeOption);
        command.Add(timezoneOption);
        command.Add(userAgentOption);
        command.Add(colorSchemeOption);
        command.Add(reducedMotionOption);
        command.Add(fullPageOption);
        command.Add(omitBackgroundOption);

        command.SetAction(
            (parseResult) =>
            {
                try
                {
                    var imageFormat = ParseImageFormat(parseResult.GetValue(imageFormatOption));
                    var colorScheme = ParseColorScheme(parseResult.GetValue(colorSchemeOption));
                    var contextDefaults = new WebBrowserContextOptions();
                    var entryPoint = parseResult.GetValue(entryPointArgument)!;
                    var declarationName = parseResult.GetValue(declarationNameOption);

                    var executionMode = WebBrowserExecutionMode.Container;

                    var viewportWidth =
                        parseResult.GetValue(viewportWidthOption)
                        ??
                        contextDefaults.ViewportWidth;

                    var options =
                        new ScreenshotCommandOptions(
                            EntryPoint: entryPoint,
                            DeclarationName: declarationName,
                            OutputPath:
                            parseResult.GetValue(outputOption)
                            ??
                            BuildDefaultOutputPath(
                                entryPoint,
                                declarationName,
                                viewportWidth,
                                imageFormat,
                                DateTimeOffset.Now),
                            BrowserContext:
                            new WebBrowserContextOptions
                            {
                                ViewportWidth = viewportWidth,
                                ViewportHeight =
                                parseResult.GetValue(viewportHeightOption)
                                ??
                                contextDefaults.ViewportHeight,
                                DeviceScaleFactor =
                                parseResult.GetValue(deviceScaleFactorOption)
                                ??
                                contextDefaults.DeviceScaleFactor,
                                ScreenWidth = parseResult.GetValue(screenWidthOption),
                                ScreenHeight = parseResult.GetValue(screenHeightOption),
                                IsMobile = parseResult.GetValue(isMobileOption),
                                HasTouch = parseResult.GetValue(hasTouchOption),
                                JavaScriptEnabled = !parseResult.GetValue(disableJavaScriptOption),
                                Locale = parseResult.GetValue(localeOption),
                                TimezoneId = parseResult.GetValue(timezoneOption),
                                UserAgent = parseResult.GetValue(userAgentOption),
                                ColorScheme = colorScheme,
                                ReducedMotion = parseResult.GetValue(reducedMotionOption),
                            },
                            Screenshot:
                            new WebBrowserScreenshotOptions
                            {
                                ImageFormat = imageFormat,
                                Quality = parseResult.GetValue(qualityOption),
                                FullPage = parseResult.GetValue(fullPageOption),
                                OmitBackground = parseResult.GetValue(omitBackgroundOption),
                            });

                    ExecuteAsync(options, executionMode).GetAwaiter().GetResult();
                    return 0;
                }
                catch (Exception exception)
                {
                    Console.Error.WriteLine("Failed to take screenshot: " + exception.Message);
                    return 1;
                }
            });

        return command;
    }

    public static string BuildDefaultOutputPath(
        string entryPoint,
        string? declarationName,
        int viewportWidth,
        WebBrowserScreenshotImageFormat imageFormat,
        DateTimeOffset currentTime)
    {
        var entryPointName = Path.GetFileNameWithoutExtension(entryPoint);

        var pathDescription =
            Path.GetExtension(entryPoint).Equals(".elm", StringComparison.OrdinalIgnoreCase)
            ?
            entryPointName + "-" + (declarationName ?? "main")
            :
            entryPointName;

        var fileExtension =
            imageFormat switch
            {
                WebBrowserScreenshotImageFormat.Png => "png",
                WebBrowserScreenshotImageFormat.Jpeg => "jpeg",

                _ =>
                throw new ArgumentOutOfRangeException(nameof(imageFormat)),
            };

        return
            currentTime.ToString("yyyy-MM-ddTHH-mm-ss", CultureInfo.InvariantCulture) +
            "-screenshot-" +
            pathDescription +
            "-" +
            viewportWidth +
            "px." +
            fileExtension;
    }

    public static async Task<ReadOnlyMemory<byte>> TakeElmAppScreenshotAsync(
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath,
        WebBrowserExecutionMode executionMode,
        string? entryPointDeclarationName = null,
        WebBrowserContextOptions? browserContextOptions = null,
        WebBrowserScreenshotOptions? screenshotOptions = null,
        CancellationToken cancellationToken = default)
    {
        var document =
            await ElmAppCompilationToHtml.CompileHtmlDocumentAsync(
                sourceFiles,
                entryPointFilePath,
                entryPointDeclarationName,
                cancellationToken)
            .ConfigureAwait(false);

        return
            await TakeDocumentScreenshotAsync(
                document,
                contentType: "text/html; charset=utf-8",
                waitForElmApp: true,
                browserContextOptions,
                screenshotOptions,
                executionMode,
                cancellationToken)
            .ConfigureAwait(false);
    }

    private static async Task ExecuteAsync(
        ScreenshotCommandOptions options,
        WebBrowserExecutionMode executionMode)
    {
        var entryPointPath = Path.GetFullPath(options.EntryPoint);

        if (!File.Exists(entryPointPath))
            throw new FileNotFoundException("Did not find the entry point file.", entryPointPath);

        var extension = Path.GetExtension(entryPointPath).ToLowerInvariant();
        var isElmEntryPoint = extension is ".elm";

        if (!isElmEntryPoint && options.DeclarationName is not null)
        {
            throw new ArgumentException(
                "--declaration-name can only be used with an Elm entry point.");
        }

        ReadOnlyMemory<byte> screenshot;

        switch (extension)
        {
            case ".html":
            case ".htm":
                screenshot =
                    await TakeDocumentScreenshotAsync(
                        await File.ReadAllBytesAsync(entryPointPath),
                        contentType: "text/html; charset=utf-8",
                        waitForElmApp: false,
                        options.BrowserContext,
                        options.Screenshot,
                        executionMode);

                break;

            case ".svg":
                screenshot =
                    await TakeDocumentScreenshotAsync(
                        await File.ReadAllBytesAsync(entryPointPath),
                        contentType: "image/svg+xml",
                        waitForElmApp: false,
                        options.BrowserContext,
                        options.Screenshot,
                        executionMode);

                break;

            case ".elm":
                var projectRoot = FindElmProjectRoot(entryPointPath);

                var sourceFiles =
                    LoadFromLocalFilesystem.LoadSortedTreeFromPath(projectRoot)
                    ??
                    throw new DirectoryNotFoundException(
                        "Did not find the Elm project directory '" + projectRoot + "'.");

                sourceFiles =
                    LoadFromLocalFilesystem.RemoveNoiseFromTree(
                        sourceFiles,
                        discardGitDirectory: true);

                var relativeEntryPointPath =
                    Path.GetRelativePath(projectRoot, entryPointPath)
                    .Split(
                        [Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar],
                        StringSplitOptions.RemoveEmptyEntries);

                screenshot =
                    await TakeElmAppScreenshotAsync(
                        sourceFiles,
                        relativeEntryPointPath,
                        executionMode,
                        options.DeclarationName,
                        options.BrowserContext,
                        options.Screenshot);

                break;

            default:
                throw new ArgumentException(
                    "Entry point files must use an .html, .htm, .svg, or .elm extension.");
        }

        await File.WriteAllBytesAsync(options.OutputPath, screenshot);

        Console.WriteLine(
            "Saved screenshot to '" + Path.GetFullPath(options.OutputPath) + "'.");
    }

    private static async Task<ReadOnlyMemory<byte>> TakeDocumentScreenshotAsync(
        ReadOnlyMemory<byte> document,
        string contentType,
        bool waitForElmApp,
        WebBrowserContextOptions? browserContextOptions,
        WebBrowserScreenshotOptions? screenshotOptions,
        WebBrowserExecutionMode executionMode,
        CancellationToken cancellationToken = default)
    {
        await using var browser =
            await WebBrowserInstance.StartAsync(
                executionMode: executionMode,
                cancellationToken: cancellationToken)
            .ConfigureAwait(false);

        await using var page =
            await browser.CreatePageAsync(browserContextOptions, cancellationToken)
            .ConfigureAwait(false);

        await page.LoadHtmlAsync(
            document,
            new HtmlDocumentOptions
            {
                ContentType = contentType,
            },
            cancellationToken)
        .ConfigureAwait(false);

        if (waitForElmApp)
        {
            await page.WaitForReadyAsync(
                "() => document.documentElement.dataset.elmAppReady === 'true'",
                cancellationToken: cancellationToken)
            .ConfigureAwait(false);
        }

        return
            await page.TakeScreenshotAsync(screenshotOptions, cancellationToken)
            .ConfigureAwait(false);
    }

    private static string FindElmProjectRoot(string entryPointPath)
    {
        var directory = Directory.GetParent(entryPointPath);

        while (directory is not null)
        {
            if (File.Exists(Path.Combine(directory.FullName, "elm.json")))
                return directory.FullName;

            directory = directory.Parent;
        }

        throw new FileNotFoundException(
            "Did not find an elm.json file in the entry point directory or any parent directory.",
            entryPointPath);
    }

    private static WebBrowserScreenshotImageFormat ParseImageFormat(string? imageFormat) =>
        imageFormat?.ToLowerInvariant() switch
        {
            null or "png" => WebBrowserScreenshotImageFormat.Png,
            "jpeg" or "jpg" => WebBrowserScreenshotImageFormat.Jpeg,

            _ =>
            throw new ArgumentException("Image type must be png or jpeg."),
        };

    private static WebBrowserColorScheme ParseColorScheme(string? colorScheme) =>
        colorScheme?.ToLowerInvariant() switch
        {
            null or "no-preference" => WebBrowserColorScheme.NoPreference,
            "light" => WebBrowserColorScheme.Light,
            "dark" => WebBrowserColorScheme.Dark,

            _ =>
            throw new ArgumentException(
                "Color scheme must be no-preference, light, or dark."),
        };

    private sealed record ScreenshotCommandOptions(
        string EntryPoint,
        string? DeclarationName,
        string OutputPath,
        WebBrowserContextOptions BrowserContext,
        WebBrowserScreenshotOptions Screenshot);
}
