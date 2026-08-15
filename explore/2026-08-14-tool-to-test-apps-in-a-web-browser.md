# 2026-08-14 Tool to Test Apps in a Web Browser

## Motivation

When developing apps to run in a web browser, we reach for various tools to enable fast and efficient testing:

+ For visual aspects, a function that takes an HTML document and simulated device parameters as input and returns a screenshot.
  + As we embed resources such as CSS, fonts, and images in this HTML file, this should allow testing the layout and rendering in most states of a web app.
  + A lightweight rendering implementation could use a component such as Servo.
+ Applying a sequence of events to the app's initial state should support testing scenarios involving interactions.
  + Since inputs, including time and network responses, can be modelled as events, this should allow reaching most states and then testing them with the rendering tool mentioned above. Separate integration tests remain necessary for the effects and browser mechanisms that produce those events.

However, several complications motivate running at least some validations in an actual web browser process:

+ Verifying the implementation of event handlers.
+ Integration of custom elements and web components.
+ Interactions between different event handlers.
+ Emission of effects and commands (for example, `pushUrl`) and subscriptions (for example, timers) to browser APIs.
+ Dependencies on state that cannot be modelled in the DOM, such as selection in text boxes or caret position.

To support automated testing, we therefore want a way to run a web browser and automate interactions with that browser instance.

## Scope

+ Test only a single version of a single browser initially: a pinned recent Chromium build.
+ Provide a .NET library for controlling the browser. The ordinary API should use task-oriented .NET types; a lower-level escape hatch may expose Chrome DevTools Protocol (CDP) commands and events.
+ Provide a .NET library for starting, probing, and disposing browser containers.
+ Accept a complete HTML document as `ReadOnlyMemory<byte>`. Our standard build process bundles the app into one HTML file for portability, including running an app received as an email attachment.
+ Keep assertions and the choice of .NET test framework outside the library. Return values, events, and diagnostics that any test framework can assert on.
+ Assume Docker installation and daemon configuration are handled outside this component.
+ Prefer deterministic, isolated browser contexts. Parallel tests must not share cookies, storage, permissions, routes, or pages unless requested explicitly.

This component is not intended to replace pure app-state tests. Browser tests are slower and should cover browser integration, a representative set of visual states, and critical end-to-end paths.

## Recommended functionality

The following is the target capability inventory. The first implementation can expose a smaller façade, but its design should not prevent adding the remaining capabilities.

### Browser, context, and page lifecycle

+ Start a pinned browser image, wait for actual protocol readiness, expose startup logs on failure, and dispose the container and its child processes reliably.
+ Reuse one browser process across tests when useful, but create a new non-persistent browser context for each test. A context is the appropriate isolation boundary for cookies, cache, permissions, storage, and network routes.
+ Create, enumerate, and close pages. Observe popups, new tabs, page closure, crashes, and browser disconnection.
+ Work with the main frame, nested frames, cross-origin frames, open and closed shadow DOM where the automation library supports it, workers, and service workers.
+ Navigate to a URL and support back, forward, reload, stop, and configurable navigation timeouts.
+ Wait for `commit`, `DOMContentLoaded`, or `load`, but normally wait for an app-specific visible state or a particular request/response. A generic "network idle" wait is unreliable for apps with polling, analytics, server-sent events, or WebSockets.
+ Propagate `CancellationToken` and bounded timeouts through all asynchronous operations.

### Loading the HTML document

The API should accept `ReadOnlyMemory<byte>` rather than requiring a temporary file. It should also accept:

+ Content type and character encoding, defaulting to `text/html; charset=utf-8`.
+ A virtual HTTPS or HTTP URL/origin, defaulting to an internally allocated test origin.
+ Optional response status and headers, including Content Security Policy and cross-origin isolation headers.
+ Optional additional path-to-response mappings, even though the normal build is one self-contained HTML document. These mappings are useful for manifests, service workers, source maps, and negative tests.

The default should load the bytes as an HTTP response at a normal origin, not with a `file:`, `data:`, or `about:blank` URL. Those schemes have different origin, storage, cookie, relative-URL, CORS, secure-context, and service-worker behaviour and can hide integration defects.

Two implementation modes are useful:

1. **Route-backed virtual origin**: Navigate to a stable test URL and fulfil its request through browser automation routing. This is fast, requires no additional listener, preserves an HTTP origin for most browser behaviour, and is a good default for a self-contained document.
2. **Real origin server**: Serve the bytes through a real Kestrel or containerized HTTP server. Use this when testing service workers, streaming, redirects, TLS, HTTP caching, connection failures, or other behaviour where browser interception would alter the mechanism under test.

### Viewport and device properties

The API should distinguish properties that are often incorrectly combined under "resolution" or "scale":

+ **Layout viewport width and height**, in CSS pixels. This drives responsive breakpoints and the initial containing block.
+ **Screen width and height**, in CSS pixels, as reported by `window.screen`. This is distinct from the viewport and is relevant to code that opens or positions windows.
+ **Device scale factor/device pixel ratio (DPR)**. This controls the ratio of device pixels to CSS pixels and therefore high-density image selection, canvas backing size, and screenshot pixel dimensions.
+ **Mobile viewport behaviour**. This determines whether the page honours the mobile viewport meta tag and applies Chromium's mobile layout behaviours.
+ **Touch capability** independently of mobile layout. Desktop-sized touch devices exist.
+ **Orientation**, including portrait/landscape type and angle, and an API to change orientation during a test.
+ **Browser window bounds** for headed debugging, separately from viewport emulation.
+ **User agent and user-agent client hints/platform** when server or client code branches on them.
+ **Preset device profiles** containing a coherent viewport, screen, DPR, mobile flag, touch flag, user agent, and orientation. Also allow explicit custom values.

Physical DPI is not a useful browser-facing input: web layout operates in CSS pixels and exposes DPR, not a dependable monitor DPI. The primary API should therefore offer DPR rather than "DPI". Similarly, browser page zoom, OS display scaling, pinch zoom, and screenshot output scale are separate concepts and should not be represented by one ambiguous `Scale` property:

+ DPR belongs in the context/device configuration.
+ Screenshot scale chooses CSS-pixel or device-pixel output.
+ Page/pinch zoom is an advanced CDP emulation feature and can be added separately when a test requires `visualViewport.scale` or zoom-specific behaviour.

Additional environment emulation should include:

+ Locale, accepted languages, timezone, and optional extra HTTP headers.
+ Geolocation with latitude, longitude, and optional accuracy.
+ Permission grant, denial, prompt/default state, and reset, scoped by origin where supported.
+ Light/dark/no-preference colour scheme.
+ Reduced motion, forced colours, contrast preference, and print versus screen media.
+ Online/offline state and, through CDP when needed, latency and upload/download throughput.
+ JavaScript enabled/disabled and HTTP credentials/proxy settings.
+ Optional CPU throttling for performance-oriented scenarios.

Safe-area insets, viewport segments/folds, display posture, device orientation sensors, and pressure/pen properties are useful advanced additions, but are not required for the first responsive-layout implementation.

### Finding and inspecting content

+ Locator APIs by accessible role and name, label, visible text, placeholder, alternate text, title, test identifier, and CSS selector. XPath can remain an escape hatch.
+ Strict matching by default: an action intended for one element should fail clearly when zero or multiple elements match.
+ Locator composition, filtering, lists, and support inside frames and shadow DOM.
+ Auto-waiting and retrying against the current DOM rather than retaining stale element handles.
+ Read text, HTML, attributes, properties, form values, checked/selected state, visibility, enabled/editable state, bounding boxes, computed styles, scroll positions, and focus.
+ Inspect selection and caret state with `selectionStart`, `selectionEnd`, and `document.getSelection()`.
+ Evaluate serializable JavaScript in a page, frame, or element context and install an initialization script before app scripts run. This is the general escape hatch for browser features not covered by the façade and can also seed sources such as `Math.random`.
+ Explicit waits for locator state, URL, app-provided predicate, console event, download, dialog, popup, request, or response.

An action should normally wait until its target is attached, visible, stable, enabled/editable as appropriate, scrolled into view, and able to receive the pointer event. A `Force` option can bypass actionability checks for exceptional tests, while direct DOM event dispatch should be clearly identified as less representative of user input.

### Simulating user input

#### Pointer, mouse, and wheel

+ Move to absolute coordinates or an element-relative position, with optional interpolated steps.
+ Hover, pointer down/up, click, double-click, and arbitrary click count.
+ Left, middle, right, back, and forward buttons; held-button bitmask; modifier keys.
+ Scroll/wheel with horizontal and vertical deltas.
+ Drag and drop between elements or coordinates, including a `DataTransfer` payload and file drop.
+ Pointer type and properties for mouse and pen where CDP supports them, including pressure/tilt as an advanced API.

#### Keyboard and text

+ Press and release physical/logical keys, including modifiers and chords.
+ Insert text efficiently for ordinary form filling.
+ Type character by character, with optional delay, when testing `keydown`, `keypress`, `beforeinput`, `input`, composition, or shortcut handlers.
+ Support non-ASCII text and input-method/composition events through a dedicated advanced API rather than assuming one character equals one key.
+ Read and set focus, selection ranges, and caret position.

#### Touch

+ Tap, long press, swipe, and sequences of touch start/move/end/cancel.
+ Multiple simultaneous touch points for pinch and multi-touch interactions.
+ Keep "device supports touch" separate from dispatching a touch sequence.

#### Forms, files, and browser UI

+ Fill text, date, time, datetime-local, number, range, and content-editable controls.
+ Check/uncheck checkboxes and select radio buttons.
+ Select one or multiple options by value, label, or index.
+ Upload one or more files supplied as in-memory bytes with file name and MIME type; support file chooser events and clearing a selection.
+ Focus, blur, and dispatch a named DOM event as an explicit low-fidelity escape hatch.
+ Accept or dismiss `alert`, `confirm`, `prompt`, and `beforeunload` dialogs, including supplying prompt text.
+ Read/write the clipboard when permissions permit.
+ Initiate and inspect downloads, returning suggested file name, failure information, and bytes without requiring a host path.

The high-level APIs should use locators and actionability checks. Coordinate-level CDP input is still needed for canvas, custom controls, pen, and multi-touch testing.

### Screenshots and visual comparison

Screenshot capture should return bytes and metadata; writing files should be a convenience rather than the only output mode.

+ Capture the current viewport.
+ Capture the complete scrollable page.
+ Capture a rectangle in page CSS coordinates.
+ Capture one located element, scrolling it into view and using its current bounding box.
+ Use lossless PNG by default. Optionally support JPEG/WebP with quality where the underlying Chromium API supports them.
+ Choose transparent or default background.
+ Choose CSS-pixel or device-pixel output scale and report the resulting pixel dimensions and DPR.
+ Mask selected elements with a configured colour.
+ Hide the text caret.
+ Disable or fast-forward animations and transitions for the capture, then restore them.
+ Inject temporary capture-only CSS for hiding volatile content or normalizing styling.
+ Configure timeout and capture beyond the viewport without permanently changing test state.

For visual regression support, either this library or a companion package should:

+ Compare an actual image to a baseline and return the diff image, differing-pixel count/ratio, dimensions, and threshold used.
+ Support both a strict pixel comparison and a configurable perceptual/anti-aliasing tolerance.
+ Never update baselines implicitly during a normal test run.
+ Include the browser image/version, OS, viewport, DPR, colour scheme, and font set in diagnostic metadata.

These comparison facilities are not part of the plain `Microsoft.Playwright` .NET library; they would need to be implemented here or supplied by a separate .NET image-comparison package.

Visual baselines are environment-specific. Chromium build, container OS, fonts, headless/headed mode, graphics settings, and DPR must be pinned. Capturing repeatedly until two consecutive images match and allowing masks or capture-only CSS can reduce volatility, but does not replace waiting for an app-specific ready state and `document.fonts.ready`.

Print-media emulation and Chromium PDF output are useful adjacent capabilities for apps that produce printable documents. Playwright PDF output is limited to headless Chromium.

### HTTP requests and network behaviour

There are three related but distinct needs:

1. Let the page make genuine browser HTTP, HTTPS, WebSocket, and server-sent-event requests.
2. Let a test configure, observe, modify, or replace those exchanges.
3. Let the controlling .NET process make direct HTTP requests for setup, verification, or API-only steps, optionally sharing authentication state with the browser.

The API should cover:

+ Request, response, request-completed, and request-failed events, including method, URL, resource type, initiator/frame, headers, body where available, status, timing, redirects, and failure reason.
+ Wait for a particular request or response while performing an action.
+ Route by URL pattern or predicate at page and context scope.
+ Continue unchanged, continue with modified URL/method/headers/body, abort with a chosen network error, fulfil with status/headers/body, or fetch the real response and patch it.
+ Inspect a request journal and assert that expected requests occurred in the required order/count.
+ Record and replay HTTP Archive (HAR) traffic, with an explicit policy for unmatched requests.
+ Observe and mock WebSocket connections and messages. Use a real server for protocol timing, handshake, fragmentation, or reconnect tests that exceed the automation library's routing capabilities.
+ Use a real server for incremental server-sent events; a route fulfilled with one static body does not reproduce streaming timing.
+ Configure offline mode, latency, bandwidth, failures, redirects, authentication, proxying, TLS errors, caching, and service-worker policy.
+ Create a direct .NET HTTP client with explicit cookie import/export or use Playwright's request context when sharing cookies with a browser context is desirable.

Service workers require special treatment. A service worker can handle a request before a page-level route sees it. Ordinary route-based mock tests should be able to block service workers for predictability. Separate tests should enable service workers and use a real reachable origin to validate registration, activation, update, cache, offline, and fetch-handler behaviour.

### Ways to host the app and mock HTTP services

Starting a web server in the same test process that controls the browser container is common and reasonable. The browser is nevertheless a different process, so an in-memory ASP.NET Core `TestServer` is not reachable from it. Use Kestrel on a real dynamically allocated port. When Chromium runs in Docker, expose that host port with Testcontainers and address it as `host.testcontainers.internal`; do not assume the browser container's `localhost` is the test process.

An alternative is to run the app or mock server in another container on the same temporary Docker network as Chromium. Give it a network alias and use its container port rather than a host-mapped port. This costs more startup time but has advantages:

+ The topology is consistent across developer machines, CI, and remote Docker daemons.
+ No host-port forwarding is needed for browser-to-server traffic.
+ The test exercises a real network peer and server implementation.
+ TLS, streaming, WebSockets, connection closure, and service-worker traffic are more representative.

Use dynamically mapped host ports only when the .NET controller also needs to reach a container. Never hard-code `localhost` or a host port; Testcontainers exposes the correct host name and mapped port.

The available mock approaches are complementary:

| Approach | Best for | Advantages | Limitations |
| --- | --- | --- | --- |
| Browser route fulfilment | Small per-test HTTP/HTTPS/WebSocket examples | Fast, no extra process, easy access to test state | Can alter CORS/service-worker behaviour; static fulfilment is not real streaming; browser-only |
| HAR replay | Stable snapshots of a third-party API | Low setup after recording; captures realistic headers and bodies | Recordings age, may contain secrets, and have limited dynamic/stateful behaviour |
| In-process `WireMock.Net` | .NET tests needing a real HTTP endpoint | Fluent .NET configuration, request journal, stateful scenarios, delay, proxy/record, response templating | The browser container still needs host-port forwarding |
| Containerized `WireMock.Net` or MockServer | Multi-process tests, TLS, richer failure/chaos scenarios | Language-independent network peer; reusable by browser and backend; realistic latency and connection errors | Additional image, lifecycle, and startup cost |
| Mock Service Worker (MSW) | Projects already sharing browser/dev/test mock definitions | Mock appears inside browser DevTools and can be reused in development | Adds app/service-worker integration and is not a general external server; unnecessary when browser routing suffices |
| Real dependency in Testcontainers | Compatibility and highest-fidelity integration paths | Exercises the actual dependency and protocol | Slowest/heaviest; state reset and deterministic data are still required |

`WireMock.Net` is the natural default standalone mock for this .NET codebase. It supports request matching, stateful scenarios, proxy/record and replay, templating, GraphQL, gRPC, and WebSockets, and it can run embedded or in Docker. MockServer is worth considering when sophisticated deterministic chaos testing, raw connection drops, dribbled/corrupt responses, or a language-neutral standalone service are primary requirements.

The core browser library should not require one mock server. It should provide network and topology primitives, while a separate optional integration can manage WireMock.Net or any real dependency with Testcontainers.

### Browser state and browser APIs

+ Get, add, and clear cookies, including security and same-site attributes.
+ Get, seed, clear, export, and import local storage and session storage. Support IndexedDB and Cache Storage inspection/clearing through CDP or JavaScript as advanced features.
+ Test clean, authenticated, and persisted browser states without sharing state accidentally.
+ Set permissions and geolocation, and cover notification, clipboard, camera/microphone denial, and other permission-sensitive paths where headless Chromium supports them.
+ Control time: fixed `Date`, paused/resumed time, timer advancement, animation frames, idle callbacks, and system time. Installation must happen before app scripts use the affected globals.
+ Seed or replace randomness through an initialization script when deterministic app behaviour requires it.
+ Emulate media and environmental preferences described in the viewport section.
+ Virtual WebAuthn credentials, device orientation/sensors, Bluetooth, and other CDP emulation domains can be later extensions for applications that require them.

### Observability, assertions, and diagnostics

+ Capture console messages with level, text, arguments, source URL, and location.
+ Capture uncaught page errors, unhandled promise rejections, failed resources, CSP violations where observable, browser/page crashes, and protocol disconnections.
+ Provide DOM/content snapshots and a final screenshot automatically on failure.
+ Record a Playwright trace with actions, network, DOM snapshots, and screenshots, returning the trace archive as bytes or an artifact path.
+ Optionally record video for debugging, recognizing its storage and runtime cost.
+ Expose Chromium performance metrics, performance timeline entries, and tracing for targeted performance tests. Performance thresholds should be environment-specific.
+ Expose JavaScript/CSS coverage as an advanced Chromium-only diagnostic.
+ Export HAR and server/request logs while providing hooks to redact authorization headers, cookies, query secrets, and response bodies.

### Accessibility

+ Provide role/name-based locators because they exercise the same semantics used by assistive technology.
+ Expose an accessibility-tree snapshot through CDP for focused assertions about roles, names, states, and custom elements.
+ Permit injecting an established accessibility engine such as axe-core and return its structured report. Automated rules find only a subset of accessibility defects and do not replace keyboard-only, focus-order, screen-reader, contrast, zoom, and manual review.
+ Include keyboard navigation, focus visibility/order, reduced motion, forced colours, and high zoom in the scenario inventory.

### Reliability and security

+ Pin the automation package, browser server, browser image, and fonts together. Report their versions in every failure artifact.
+ Use readiness checks against the actual browser protocol, not only "container is running" or "port is open".
+ Use unique networks, random host ports, and Testcontainers' resource cleanup. Avoid fixed container names and host bind mounts.
+ Bound startup, action, navigation, and shutdown times and include the last browser/container logs when a timeout occurs.
+ Treat Docker socket access as privileged and use this component only in trusted development/CI environments.
+ Treat HTML under test as potentially hostile. Prefer a non-root browser user with the Chromium sandbox enabled and an appropriate seccomp profile. Do not expose the browser protocol port beyond the test network.
+ Default-deny unexpected external network access where practical so a test cannot accidentally depend on the public internet or exfiltrate test data.
+ Redact secrets from traces, HARs, screenshots, console logs, request journals, and recorded mocks before retaining artifacts.

## Implementation options

### Recommended default: Microsoft.Playwright plus Testcontainers for .NET

Use `Microsoft.Playwright` as the browser automation layer and `Testcontainers.Playwright` as the container lifecycle layer.

This combination is the best fit for broad testing coverage because Playwright already supplies:

+ Isolated browser contexts and pages.
+ Resilient locators, actionability checks, and auto-waiting.
+ Mouse, keyboard, touch, forms, files, dialogs, downloads, frames, and popups.
+ Device/environment emulation.
+ Page, element, clipped, and full-page screenshots.
+ HTTP and WebSocket routing, HAR, direct request contexts, storage state, clock control, and tracing.
+ A .NET API and an official pinned browser image.

Testcontainers is a good fit for browser lifetime, cleanup, wait strategies, logs, random ports, host-port exposure, and temporary Docker networks. Its Playwright module starts a Playwright server and lets the .NET client connect to it. The browser and app/mock containers can share its network.

Important implementation details:

+ Pin a specific Playwright container image. Keep the Playwright client and server versions exactly compatible; remote Playwright protocol versions are not intended to drift independently.
+ Use Playwright's native `ConnectAsync` protocol for the container. `ConnectOverCDPAsync` is Chromium-only and officially described as lower fidelity than a Playwright-protocol connection.
+ Do not build ordinary operations directly on raw CDP. Playwright has already solved target attachment, context isolation, locator retrying, input sequencing, and many Chromium edge cases.
+ Expose `ICDPSession` only for Chromium capabilities missing from the high-level API. Playwright's CDP session takes command names and returns `JsonElement`; it is not a complete strongly typed CDP object model.
+ Prefer a small project-owned façade and immutable option/result records over re-exporting every Playwright interface. The façade can remain stable as Playwright evolves, while an explicitly marked advanced property can expose the underlying Playwright page/context.

### What "types aligned with CDP" should mean

CDP is large, versioned with Chromium, and its tip-of-tree schema can change. Generating and maintaining public C# types for the whole protocol would create a substantial compatibility obligation unrelated to the initial testing API.

For this component, "aligned" should normally mean:

+ Use the same units and concepts as CDP, such as CSS pixels, device scale factor, screen orientation, mouse button, key modifiers, screenshot clip, and network error reason.
+ Keep project-owned records serializable and forward-compatible.
+ Offer raw CDP command/event access for uncommon capabilities.

If complete compile-time-typed CDP access is a firm requirement, perform a focused spike before choosing the stack:

+ **Selenium WebDriver** includes generated, version-specific `OpenQA.Selenium.DevTools` bindings and pairs with `Testcontainers.WebDriver`, but it retains only a moving window of Chromium protocol versions and adds the Selenium/Grid layer.
+ **`dotnet-chrome-protocol`** generates C# types from the upstream CDP schema and can connect to a generic Chromium container's raw debugging endpoint, but it has a smaller ecosystem and leaves browser lifecycle, target/session management, locators, waiting, input reliability, and most conveniences to this project.
+ **PuppeteerSharp** is mature Chromium-focused .NET prior art with many high-level features and CDP-oriented internals, but it has no dedicated Testcontainers module and still does not remove the need to own a stable public façade.

A raw typed-CDP implementation offers maximum protocol coverage but is not the shortest path to reliable browser testing. It should supplement, not replace, a high-level automation layer unless avoiding Playwright is itself a requirement.

### Other alternatives

+ **Selenium WebDriver/WebDriver BiDi**: Best when cross-browser standards, Selenium Grid, or an existing Selenium estate are priorities. It is unnecessary infrastructure for the current single-Chromium scope, and BiDi does not yet replace every Chromium-specific CDP capability.
+ **PuppeteerSharp**: A reasonable Chromium-only alternative and useful implementation reference. Compared with Playwright it would require custom generic Testcontainers configuration and gives up Playwright's ready-made container module and some cross-feature consistency.
+ **Direct CDP**: Maximum Chromium control and minimum protocol layering, but the project would need to implement robust navigation, target/frame/session management, selector semantics, actionability, input sequencing, route handling, retries, protocol versioning, and diagnostics. This is the highest-maintenance option.
+ **Browserless**: Adds a remote browser service, queuing, and operational APIs. It is unnecessary for an in-process test library, adds another compatibility and deployment layer, and its current licensing must be reviewed before commercial use.
+ **Selenium standalone containers**: Mature and useful for Grid/VNC/video scenarios, but heavier than the Playwright server for this scope.
+ **Running Playwright directly on the host**: Faster and simpler when the host is controlled, but it loses the requested container boundary and makes browser libraries, fonts, and rendering environment less reproducible.
+ **Servo or another embedded renderer**: Still useful for fast render-only tests, but not a substitute for Chromium integration tests involving browser APIs, event dispatch, web components, networking, accessibility, or browser state.

## Suggested delivery order

This is not yet an implementation plan, but the capability inventory suggests natural increments:

1. Container lifecycle, isolated context/page, in-memory HTML loading at a virtual HTTP origin, explicit ready waits, semantic locators, core mouse/keyboard/form input, DOM inspection, viewport/DPR/environment options, screenshots, console/page errors, and failure traces.
2. Kestrel real-origin mode, host/container networking, request observation/routing, direct HTTP requests, storage/cookies, clock, files/downloads/dialogs/popups/frames, and visual comparison.
3. WireMock.Net integration, HAR, WebSocket and service-worker scenarios, network/CPU conditions, accessibility reporting, performance/coverage, video, multi-touch/pen, and advanced CDP emulation.

## Sources

Primary project and standards documentation consulted:

+ [Playwright .NET documentation](https://playwright.dev/dotnet/docs/intro)
  + [Emulation](https://playwright.dev/dotnet/docs/emulation)
  + [Actions](https://playwright.dev/dotnet/docs/input)
  + [Locators](https://playwright.dev/dotnet/docs/locators)
  + [Screenshots](https://playwright.dev/dotnet/docs/screenshots)
  + [Network](https://playwright.dev/dotnet/docs/network)
  + [Mock APIs and HAR](https://playwright.dev/dotnet/docs/mock)
  + [Clock](https://playwright.dev/dotnet/docs/clock)
  + [Trace viewer](https://playwright.dev/dotnet/docs/trace-viewer)
  + [Docker and remote connections](https://playwright.dev/dotnet/docs/docker)
  + [CDP session](https://playwright.dev/dotnet/docs/api/class-cdpsession)
  + [BrowserType connection APIs](https://playwright.dev/dotnet/docs/api/class-browsertype)
+ [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
  + [Emulation domain](https://chromedevtools.github.io/devtools-protocol/tot/Emulation/)
  + [Input domain](https://chromedevtools.github.io/devtools-protocol/tot/Input/)
  + [Page domain](https://chromedevtools.github.io/devtools-protocol/tot/Page/)
  + [Fetch domain](https://chromedevtools.github.io/devtools-protocol/tot/Fetch/)
  + [Accessibility domain](https://chromedevtools.github.io/devtools-protocol/tot/Accessibility/)
+ [Testcontainers for .NET](https://dotnet.testcontainers.org/)
  + [Playwright module](https://dotnet.testcontainers.org/modules/playwright/)
  + [Network communication](https://dotnet.testcontainers.org/api/create_docker_network/)
  + [Wait strategies](https://dotnet.testcontainers.org/api/wait_strategies/)
  + [Best practices](https://dotnet.testcontainers.org/api/best_practices/)
+ [WireMock.Net](https://wiremock.org/dotnet/)
+ [MockServer](https://www.mock-server.com/)
+ [Mock Service Worker](https://mswjs.io/docs/)
+ [ASP.NET Core integration tests](https://learn.microsoft.com/aspnet/core/test/integration-tests)
+ [Selenium WebDriver BiDi](https://www.selenium.dev/documentation/webdriver/bidi/)
+ [PuppeteerSharp](https://github.com/hardkoded/puppeteer-sharp)
+ [`dotnet-chrome-protocol`](https://github.com/seclerp/dotnet-chrome-protocol)
+ [W3C WebDriver](https://www.w3.org/TR/webdriver2/)
