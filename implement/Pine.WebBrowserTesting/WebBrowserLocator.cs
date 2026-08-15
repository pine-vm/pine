using Microsoft.Playwright;
using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.WebBrowserTesting;

/// <summary>
/// A strict, retrying locator resolved against the current DOM for every operation.
/// </summary>
public sealed class WebBrowserLocator
{
    private readonly ILocator _locator;

    private readonly TimeSpan _operationTimeout;

    private readonly Func<Task> _abortOperations;

    internal WebBrowserLocator(
        ILocator locator,
        TimeSpan operationTimeout,
        Func<Task> abortOperations)
    {
        _locator = locator;
        _operationTimeout = operationTimeout;
        _abortOperations = abortOperations;
    }

    /// <summary>
    /// Exposes the underlying Playwright locator for advanced capabilities.
    /// </summary>
    public ILocator AdvancedLocator => _locator;

    public async Task WaitForAsync(
        WebBrowserLocatorState state = WebBrowserLocatorState.Visible,
        CancellationToken cancellationToken = default) =>
        await _locator.WaitForAsync(
            new LocatorWaitForOptions
            {
                State =
                state switch
                {
                    WebBrowserLocatorState.Attached => WaitForSelectorState.Attached,
                    WebBrowserLocatorState.Detached => WaitForSelectorState.Detached,
                    WebBrowserLocatorState.Visible => WaitForSelectorState.Visible,
                    WebBrowserLocatorState.Hidden => WaitForSelectorState.Hidden,

                    _ =>
                    throw new ArgumentOutOfRangeException(
                        nameof(state),
                        state,
                        "Unknown locator state."),
                },
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task ClickAsync(
        WebBrowserMouseButton button = WebBrowserMouseButton.Left,
        CancellationToken cancellationToken = default) =>
        await _locator.ClickAsync(
            new LocatorClickOptions
            {
                Button =
                button switch
                {
                    WebBrowserMouseButton.Left => MouseButton.Left,
                    WebBrowserMouseButton.Right => MouseButton.Right,
                    WebBrowserMouseButton.Middle => MouseButton.Middle,

                    _ =>
                    throw new ArgumentOutOfRangeException(
                        nameof(button),
                        button,
                        "Unknown mouse button."),
                },
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task DoubleClickAsync(CancellationToken cancellationToken = default) =>
        await _locator.DblClickAsync(
            new LocatorDblClickOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task FillAsync(string value, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(value);

        await _locator.FillAsync(
            value,
            new LocatorFillOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);
    }

    public async Task PressAsync(string key, CancellationToken cancellationToken = default)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(key);

        await _locator.PressAsync(
            key,
            new LocatorPressOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);
    }

    public async Task CheckAsync(CancellationToken cancellationToken = default) =>
        await _locator.CheckAsync(
            new LocatorCheckOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task UncheckAsync(CancellationToken cancellationToken = default) =>
        await _locator.UncheckAsync(
            new LocatorUncheckOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task SelectOptionAsync(string value, CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(value);

        await _locator.SelectOptionAsync(
            value,
            new LocatorSelectOptionOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);
    }

    public async Task<string> GetTextAsync(CancellationToken cancellationToken = default) =>
        await _locator.InnerTextAsync(
            new LocatorInnerTextOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task<string> GetInnerHtmlAsync(CancellationToken cancellationToken = default) =>
        await _locator.InnerHTMLAsync(
            new LocatorInnerHTMLOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task<string?> GetAttributeAsync(
        string name,
        CancellationToken cancellationToken = default)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(name);

        return
            await _locator.GetAttributeAsync(
                name,
                new LocatorGetAttributeOptions
                {
                    Timeout = (float)_operationTimeout.TotalMilliseconds,
                })
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                _abortOperations)
            .ConfigureAwait(false);
    }

    public async Task<JsonElement> GetPropertyAsync(
        string name,
        CancellationToken cancellationToken = default)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(name);

        return
            await _locator.EvaluateAsync<JsonElement>(
                "(element, propertyName) => element[propertyName]",
                name)
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                _abortOperations)
            .ConfigureAwait(false);
    }

    public async Task<string> GetComputedStyleAsync(
        string propertyName,
        CancellationToken cancellationToken = default)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(propertyName);

        return
            await _locator.EvaluateAsync<string>(
                "(element, name) => getComputedStyle(element).getPropertyValue(name)",
                propertyName)
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                _abortOperations)
            .ConfigureAwait(false);
    }

    public async Task<bool> IsVisibleAsync(CancellationToken cancellationToken = default) =>
        await _locator.IsVisibleAsync()
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task<bool> IsEnabledAsync(CancellationToken cancellationToken = default) =>
        await _locator.IsEnabledAsync(
            new LocatorIsEnabledOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task<bool> IsCheckedAsync(CancellationToken cancellationToken = default) =>
        await _locator.IsCheckedAsync(
            new LocatorIsCheckedOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);

    public async Task<ReadOnlyMemory<byte>> TakeScreenshotAsync(
        CancellationToken cancellationToken = default) =>
        await _locator.ScreenshotAsync(
            new LocatorScreenshotOptions
            {
                Type = ScreenshotType.Png,
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            _abortOperations)
        .ConfigureAwait(false);
}
