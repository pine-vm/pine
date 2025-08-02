using ElmTime.Platform.WebService;
using AwesomeAssertions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.Elm.Platform;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using Xunit;
using static MoreLinq.Extensions.BatchExtension;
using static MoreLinq.Extensions.SkipLastWhileExtension;

namespace Pine.IntegrationTests;

public class WebServiceTests
{
    [Fact]
    public async System.Threading.Tasks.Task Web_host_stores_process_reduction_every_ten_minutes_by_default()
    {
        var persistentProcessHostDateTime =
            new DateTimeOffset(2018, 11, 4, 8, 17, 13, TimeSpan.Zero);

        void letTimePassInPersistentProcessHost(TimeSpan amount) =>
            persistentProcessHostDateTime = persistentProcessHostDateTime + amount;

        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        eventsAndExpectedResponsesBatches.Should()
            .HaveCountGreaterThan(2, "More than two batches of events to test with.");

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp,
                persistentProcessHostDateTime: () => persistentProcessHostDateTime);

        IEnumerable<string> ReadStoredReductionFileRelativePaths()
        {
            System.Threading.Thread.Sleep(1111);  //  Storing reduction may be completed after client has received response.
            return testSetup.BuildProcessStoreReaderInFileDirectory().ReductionsFilesNames();
        }

        using var server = testSetup.StartWebHost();

        //  Do not depend on an exact number of reductions stored on initialization: Send one request before measuring the number of stored reductions.
        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse =
                await client.PostAsync(
                    "",
                    new StringContent("", System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();
        }

        foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
        {
            var beforeBatchStoredReductionsCount = ReadStoredReductionFileRelativePaths().Count();

            letTimePassInPersistentProcessHost(TimeSpan.FromMinutes(10.1));

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
            {
                letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(4));

                using var client = testSetup.BuildPublicAppHttpClient();

                var httpResponse =
                    await client.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should()
                    .Be(expectedResponse, "server response");
            }

            ReadStoredReductionFileRelativePaths().Count().Should()
                .Be(beforeBatchStoredReductionsCount + 1,
                    "Number of stored reductions has increased by one since previous batch.");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_serves_static_content_from_source_file()
    {
        var defaultAppSourceFiles = TestSetup.ReadSourceFileWebApp;

        var demoFiles = new[]
        {
            new
            {
                path= ImmutableList.Create("demo-file.mp3"),
                content= (ReadOnlyMemory<byte>)Enumerable.Range(0, 10_000).SelectMany(elem => BitConverter.GetBytes((UInt16)elem))
                    .Concat(System.Text.Encoding.UTF8.GetBytes("Default static file content from String\nAnother line"))
                    .Concat(Enumerable.Range(0, 100_000).SelectMany(elem => BitConverter.GetBytes((ushort)elem)))
                    .ToImmutableList()
                    .ToArray()
            },
            new
            {
                path= ImmutableList.Create("alpha", "beta","demo-file-gamma.text"),
                content= (ReadOnlyMemory<byte>)System.Text.Encoding.UTF8.GetBytes("Some file content")
            }
        };

        var webAppSourceFiles =
            demoFiles
            .Aggregate(
                seed: defaultAppSourceFiles,
                (prev, demoFile) =>
                {
                    var demoSourceFilePath = ImmutableList.Create("static-content").AddRange(demoFile.path);

                    return prev.SetItem(demoSourceFilePath, demoFile.content);
                });

        var webAppSource =
            PineValueComposition.FromTreeWithStringPath(
                PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(webAppSourceFiles));

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);

        using var server = testSetup.StartWebHost();

        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        foreach (var demoFile in demoFiles)
        {
            var httpResponse =
                await publicAppClient.GetAsync(string.Join("/", demoFile.path));

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent =
                await httpResponse.Content.ReadAsByteArrayAsync();

            var inspectResponseContent =
                System.Text.Encoding.UTF8.GetString(responseContent);

            responseContent.Should().BeEquivalentTo(demoFile.content.ToArray());
        }

        {
            var httpResponse =
                await publicAppClient.GetAsync("readme-md");

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent =
                await httpResponse.Content.ReadAsStringAsync();

            responseContent.Should().Be(
                "A text file we will integrate using UTF8 encoding ⚓\nNewline and special chars:\"'",
                "Content of the readme-md file should match the expected value.");
        }

        {
            var httpResponse =
                await publicAppClient.GetAsync("alpha-file-via-other-interface-module");

            httpResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var responseContent =
                await httpResponse.Content.ReadAsStringAsync();

            responseContent.Should().Be("Text file content", "Content of the alpha-file-via-other-interface-module should match the expected value.");
        }
    }

    [Fact]
    public void Web_host_rate_limits_requests_before_reaching_persistent_process()
    {
        const int requestBatchSize = 100;
        const int minimumNumberOfRequestsInFastBatchExpectedToBeBlockedByRateLimit = 85;
        const int rateLimitWindowSize = 10;

        var fileStoreWriter = new RecordingFileStoreWriter();

        int fileStoreHistoryCountAppendOperations() =>
            fileStoreWriter.History
            .Count(op => op.AppendFileContent is not null);

        IFileStoreReader getCurrentFileStoreReader() =>
            fileStoreWriter.ReaderFromAppliedOperationsOnEmptyStore();

        var fileStoreReader = new DelegatingFileStoreReader
        (
            GetFileContentDelegate: path => getCurrentFileStoreReader().GetFileContent(path),
            ListFilesInDirectoryDelegate: path => getCurrentFileStoreReader().ListFilesInDirectory(path)
        );

        var fileStore = new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);

        var persistentProcessHostDateTime =
            new DateTimeOffset(2018, 11, 12, 19, 51, 13, TimeSpan.Zero);

        void letTimePassInPersistentProcessHost(TimeSpan amount) =>
            persistentProcessHostDateTime += amount;

        var requestsBatches =
            Enumerable.Range(0, 3)
            .Select(batchIndex =>
                Enumerable.Range(0, requestBatchSize)
                .Select(indexInBatch =>
                new
                {
                    batchIndex,
                    indexInBatch,
                    addition = batchIndex * requestBatchSize + indexInBatch
                }).ToList())
                .ToList();

        var deploymentFiles =
            TestSetup.WithWebServiceConfigJson(
                TestSetup.CounterElmWebApp,
                new WebServiceConfigJson
                (
                    singleRateLimitWindowPerClientIPv4Address: new RateLimitWindow
                    (
                        windowSizeInMs: 1000 * rateLimitWindowSize,
                        limit: rateLimitWindowSize
                    )
                ));

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                fileStore: fileStore,
                deployAppAndInitElmState:
                PineValueComposition.FromTreeWithStringPath(
                    PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(deploymentFiles)),
                persistentProcessHostDateTime: () => persistentProcessHostDateTime);

        long expectedSum = 0;

        async System.Threading.Tasks.Task<HttpResponseMessage> PostStringContentToPublicAppAsync(
            int addition)
        {
            using var client = testSetup.BuildPublicAppHttpClient();

            var requestContent = System.Text.Json.JsonSerializer.Serialize(new { addition });

            var response =
                await client.PostAsync(
                    requestContent,
                    new StringContent(requestContent, System.Text.Encoding.UTF8));

            if (response.StatusCode == HttpStatusCode.OK)
            {
                expectedSum += addition;
            }

            return response;
        }

        IEnumerable<HttpResponseMessage> whereStatusCodeTooManyRequests(
            IEnumerable<HttpResponseMessage> httpResponses) =>
            httpResponses.Where(httpResponse => httpResponse.StatusCode == HttpStatusCode.TooManyRequests);

        using var server = testSetup.StartWebHost();
        {
            var firstBatchHttpResponses =
                requestsBatches[0].Select(processEvent =>
                {
                    letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(1));

                    return PostStringContentToPublicAppAsync(processEvent.addition).Result;
                }).ToList();

            whereStatusCodeTooManyRequests(firstBatchHttpResponses).Count()
            .Should().Be(0, "No HTTP response from the first batch has status code 'Too Many Requests'.");
        }

        var storeAppendCountBeforeSecondBatch =
            fileStoreHistoryCountAppendOperations();

        {
            var secondBatchHttpResponses =
                requestsBatches[1].Select(processEvent =>
                {
                    //  Process the events in the second batch in smaller timespan so that effect of the rate-limit should be observable.
                    letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(0.1));

                    return PostStringContentToPublicAppAsync(processEvent.addition).Result;
                }).ToList();

            whereStatusCodeTooManyRequests(secondBatchHttpResponses).Count()
            .Should().BeGreaterThan(minimumNumberOfRequestsInFastBatchExpectedToBeBlockedByRateLimit,
                "At least this many requests in the fast batch are expected to be answered with status code 'Too Many Requests'.");
        }

        var storeAppendCountDuringSecondBatch =
            fileStoreHistoryCountAppendOperations() - storeAppendCountBeforeSecondBatch;

        storeAppendCountDuringSecondBatch.Should()
            .BeLessThan(
            requestBatchSize / 2,
            "Not all events from the second batch have been stored.");

        letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(rateLimitWindowSize));

        var storeAppendCountBeforeThirdBatch =
            fileStoreHistoryCountAppendOperations();

        {
            var thirdBatchHttpResponses =
                requestsBatches[2].Select(processEvent =>
                {
                    letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(1));

                    return PostStringContentToPublicAppAsync(processEvent.addition).Result;
                }).ToList();

            whereStatusCodeTooManyRequests(thirdBatchHttpResponses).Count()
            .Should().Be(0, "No HTTP response from the third batch has status code 'Too Many Requests'.");
        }

        var storeAppendCountAfterThirdBatch =
            fileStoreHistoryCountAppendOperations();

        storeAppendCountAfterThirdBatch.Should()
            .BeGreaterThanOrEqualTo(storeAppendCountBeforeThirdBatch + requestBatchSize,
            "All events from the third batch have been stored.");
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_limits_http_request_size()
    {
        const int requestSizeLimit = 20_000;

        var deploymentFiles =
            TestSetup.WithWebServiceConfigJson(
                TestSetup.StringBuilderElmWebApp,
                new WebServiceConfigJson
                {
                    httpRequestEventSizeLimit = requestSizeLimit,
                });

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: PineValueComposition.FromTreeWithStringPath(
                PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(deploymentFiles)));

        async System.Threading.Tasks.Task<HttpResponseMessage> PostStringContentToPublicAppAsync(
            string requestContent)
        {
            using var client = testSetup.BuildPublicAppHttpClient();

            return
                await client.PostAsync(
                    "",
                    new StringContent(requestContent, System.Text.Encoding.UTF8));
        }

        using var server = testSetup.StartWebHost();

        var sufficientlySmallRequestContentSize =
            // Consider overhead from base64 encoding plus additional properties of an HTTP request.
            requestSizeLimit / 4 * 3 - 2000;

        (await PostStringContentToPublicAppAsync(
            "small enough content" + new string('_', sufficientlySmallRequestContentSize))).StatusCode.Should()
            .Be(HttpStatusCode.OK, "Receive OK status code for sufficiently small request.");

        (await PostStringContentToPublicAppAsync("too large content" + new string('_', requestSizeLimit))).StatusCode.Should()
            .Be(HttpStatusCode.RequestEntityTooLarge, "Receive non-OK status code for too large request.");
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_setting_elm_app_state_only_after_authorization()
    {
        const string AdminPassword = "Password_1234567";

        static async System.Threading.Tasks.Task<HttpResponseMessage> HttpSetElmAppStateAsync(
            HttpClient client, string state) =>
            await client.PostAsync(
                StartupAdminInterface.PathApiElmAppState,
                new StringContent(state, System.Text.Encoding.UTF8));

        static async System.Threading.Tasks.Task<HttpResponseMessage> HttpGetElmAppStateAsync(
            HttpClient client) =>
            await client.GetAsync(StartupAdminInterface.PathApiElmAppState);

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.StringBuilderWebApp,
                adminPassword: AdminPassword);

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            (await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync())
                .Should().BeEmpty("Initial state should be empty.");

            HttpPostStringContentAtRoot(publicAppClient, "part-a").StatusCode
                .Should().Be(HttpStatusCode.OK, "HTTP status code for posting to root.");

            (await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync())
                .Should().Be("part-a", "State after first post should be 'part-a'.");

            HttpPostStringContentAtRoot(publicAppClient, "-⚙️-part-b").StatusCode
                .Should().Be(HttpStatusCode.OK, "HTTP status code for posting to root with part-b.");

            (await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync())
                .Should().Be("part-a-⚙️-part-b", "State after posting to root.");

            using (var client = testSetup.BuildAdminInterfaceHttpClient())
            {
                (await HttpGetElmAppStateAsync(client)).StatusCode
                    .Should().Be(HttpStatusCode.Unauthorized,
                        "HTTP status code for authorized request to get elm app state.");

                (await HttpSetElmAppStateAsync(client, "new-state")).StatusCode
                    .Should().Be(HttpStatusCode.Unauthorized,
                        "HTTP status code for unauthorized request to set elm app state.");

                (await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync())
                    .Should().Be("part-a-⚙️-part-b",
                    "State after failing to set elm app state.");

                client.DefaultRequestHeaders.Authorization =
                    new AuthenticationHeaderValue(
                        "Basic",
                        Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                            Configuration.BasicAuthenticationForAdmin(AdminPassword))));

                {
                    var getElmAppStateResponse = await HttpGetElmAppStateAsync(client);

                    getElmAppStateResponse.StatusCode
                        .Should().Be(HttpStatusCode.OK,
                            "HTTP status code for authorized request to get elm app state after authorization.");

                    (await getElmAppStateResponse.Content.ReadAsStringAsync())
                        .Should().Be(@"""part-a-⚙️-part-b""",
                            "State after getting elm app state after authorization.");
                }

                (await HttpSetElmAppStateAsync(client, @"""new-state""")).StatusCode
                    .Should().Be(HttpStatusCode.OK,
                        "HTTP status code for authorized request to set elm app state.");
            }

            (await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync())
                .Should().Be("new-state",
                "State after setting elm app state.");

            HttpPostStringContentAtRoot(publicAppClient, "_appendix").StatusCode
                .Should().Be(HttpStatusCode.OK);
        }

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result
                .Should().Be("new-state_appendix",
                "State after setting elm app state, appending, and restarting server.");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_propagates_HTTP_headers()
    {
        // This name needs to be consistent with the code in Elm app CrossPropagateHttpHeadersToAndFromBody.
        const string appSpecificHttpResponseHeaderName = "response-header-name";

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.CrossPropagateHttpHeadersToAndFromBody);

        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        var requestHeaderValue = "HTTP request header value.";
        var requestContentString = "HTTP request content.";

        const string appSpecificHttpRequestHeaderName = "request-header-name";

        publicAppClient.DefaultRequestHeaders.Add(
            appSpecificHttpRequestHeaderName,
            WebUtility.UrlEncode(requestHeaderValue));

        var response =
            await publicAppClient.PostAsync(
                "",
                new StringContent(requestContentString));

        response.Content.Headers.ContentType?.ToString().Should().Be("application/json");

        var responseContentString =
            await response.Content.ReadAsStringAsync();

        var collectionFromResponseContent =
            System.Text.Json.JsonSerializer.Deserialize<Web_host_propagates_HTTP_headers_Response_Entry[]>(
                responseContentString)!;

        var matchingEntryFromResponseContent =
            collectionFromResponseContent
            .First(entry => entry.name == appSpecificHttpRequestHeaderName);

        WebUtility.UrlDecode(matchingEntryFromResponseContent.values.FirstOrDefault())
            .Should().Be(requestHeaderValue,
            "Expect the HTTP request header was propagated to an entry in the response content.");

        response.Headers.TryGetValues(appSpecificHttpResponseHeaderName, out var appSpecificHttpHeaderValues);

        WebUtility.UrlDecode(appSpecificHttpHeaderValues?.FirstOrDefault())
            .Should().Be(requestContentString,
            "Expect the HTTP request content was propagated to the response header with name '" + appSpecificHttpResponseHeaderName + "'");
    }

    [Fact]
    public async System.Threading.Tasks.Task Host_supports_sending_HTTP_requests()
    {
        const string echoServerUrl = "http://localhost:6789/";

        var echoWebHostBuilder =
            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
            .Configure(app =>
            {
                app.Run(async context =>
                {
                    var requestRecord = await Asp.AsInterfaceHttpRequestAsync(context.Request);

                    var requestRecordSerialFormat =
                    new ElmTime.Platform.WebService.InterfaceToHost.HttpRequest(
                        method: requestRecord.Method,
                        uri: requestRecord.Uri,
                        bodyAsBase64:
                        Maybe.NothingFromNull(
                            requestRecord.Body is { } body
                            ?
                            Convert.ToBase64String(body.Span)
                            :
                            null),
                        headers:
                        [..requestRecord.Headers
                        .Select(h => new ElmTime.Platform.WebService.InterfaceToHost.HttpHeader(
                            name: h.Name,
                            values: [..h.Values]))
                        ]);

                    context.Response.StatusCode = 200;

                    await context.Response.WriteAsync(
                        System.Text.Json.JsonSerializer.Serialize(requestRecordSerialFormat));
                });
            })
            .UseUrls(echoServerUrl);

        using var echoServer = echoWebHostBuilder.Build();

        echoServer.Start();

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.HttpProxyWebApp);

        using var server = testSetup.StartWebHost();

        using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
        {
            var customHeaderName = "My-custom-header";
            var customHeaderValue = "Hello!";

            var requestContentBytes =
                Enumerable.Range(0, 777).Select(i => (ushort)i).SelectMany(BitConverter.GetBytes).ToArray();

            var httpRequestMessage =
                new HttpRequestMessage(new HttpMethod("post"), "")
                {
                    Content = new ByteArrayContent(requestContentBytes),
                };

            httpRequestMessage.Headers.Add("forward-to", echoServerUrl);

            httpRequestMessage.Headers.Add(customHeaderName, customHeaderValue);

            var response =
                await publicAppClient.SendAsync(httpRequestMessage);

            var responseContentString =
                await response.Content.ReadAsStringAsync();

            ((int)response.StatusCode).Should().Be(200, "Response.status code, " + responseContentString);

            var echoRequestStructure =
                System.Text.Json.JsonSerializer.Deserialize<ElmTime.Platform.WebService.InterfaceToHost.HttpRequest>(
                    responseContentString)!;

            echoRequestStructure.bodyAsBase64.WithDefault("").ToLowerInvariant()
                .Should().Be(Convert.ToBase64String(requestContentBytes).ToLowerInvariant(),
                "Request body is propagated.");

            var echoCustomHeaderValues =
                echoRequestStructure.headers
                .FirstOrDefault(header => header.name == customHeaderName)
                ?.values;

            echoCustomHeaderValues?.Length.Should().Be(1, "Custom header name is propagated.");

            echoCustomHeaderValues?[0].Should().Be(customHeaderValue, "Custom header value is propagated.");
        }

        using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
        {
            var contentTypeHeaderName = "content-type";
            var customContentType = "application/octet-stream";

            var httpRequestMessage =
                new HttpRequestMessage(new HttpMethod("post"), "")
                {
                    Content = new ByteArrayContent(System.Text.Encoding.UTF8.GetBytes("Hello!")),
                };

            httpRequestMessage.Content.Headers.ContentType =
                new MediaTypeHeaderValue(customContentType);

            httpRequestMessage.Headers.Add("forward-to", echoServerUrl);

            var response =
                await publicAppClient.SendAsync(httpRequestMessage);

            var responseContentString =
                await response.Content.ReadAsStringAsync();

            var echoRequestStructure =
                System.Text.Json.JsonSerializer.Deserialize<ElmTime.Platform.WebService.InterfaceToHost.HttpRequest>(
                    responseContentString)!;

            var observedContentType =
                echoRequestStructure.headers
                .SingleOrDefault(header => string.Equals(
                    header.name, contentTypeHeaderName, StringComparison.InvariantCultureIgnoreCase))
                ?.values.SingleOrDefault();

            observedContentType.Should().Be(customContentType,
                "Sent HTTP request with content type '" + customContentType + "'.");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_sends_HTTP_response_only_after_write_to_history()
    {
        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp);

        async System.Threading.Tasks.Task<HttpResponseMessage> postStringContentToPublicAppAsync(
            string postContent)
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            return await
                publicAppClient.PostAsync(
                    "",
                    new StringContent(postContent, System.Text.Encoding.UTF8));
        }

        Action? runBeforeMutateInFileStore = null;

        using var server = testSetup.StartWebHost(
            processStoreFileStoreMap: originalFileStore =>
            {
                return new FileStoreFromDelegates(
                    setFileContent: new Action<IImmutableList<string>, ReadOnlyMemory<byte>>((path, fileContent) =>
                    {
                        runBeforeMutateInFileStore?.Invoke();
                        originalFileStore.SetFileContent(path, fileContent);
                    }),
                    appendFileContent: new Action<IImmutableList<string>, ReadOnlyMemory<byte>>((path, fileContent) =>
                    {
                        runBeforeMutateInFileStore?.Invoke();
                        originalFileStore.AppendFileContent(path, fileContent);
                    }),
                    deleteFile: new Action<IImmutableList<string>>(path =>
                    {
                        runBeforeMutateInFileStore?.Invoke();
                        originalFileStore.DeleteFile(path);
                    }),
                    getFileContent: originalFileStore.GetFileContent,
                    listFilesInDirectory: originalFileStore.ListFilesInDirectory);
            });

        var delayMutateInFileStore = true;

        runBeforeMutateInFileStore = new Action(() =>
        {
            while (delayMutateInFileStore)
            {
                System.Threading.Tasks.Task.Delay(TimeSpan.FromMilliseconds(11)).Wait();
            }
        });

        /*
         * We need to cause an event that actually changes the state of the Elm app,
         * because events that do not change the state can be filtered as an optimization.
         * */
        var httpPostTask =
            postStringContentToPublicAppAsync(
                System.Text.Json.JsonSerializer.Serialize(new { addition = 123 }));

        await System.Threading.Tasks.Task.Delay(TimeSpan.FromSeconds(5));

        httpPostTask.IsCompleted.Should().BeFalse("HTTP task is not completed.");

        delayMutateInFileStore = false;

        await System.Threading.Tasks.Task.Delay(TimeSpan.FromSeconds(5));

        httpPostTask.IsCompleted.Should().BeTrue("HTTP task is completed.");
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_deploy_app_config_and_init_elm_app_state()
    {
        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        eventsAndExpectedResponsesBatches.Count.Should()
            .BeGreaterThan(2, "More than two batches of events to test with.");

        var deploymentZipArchive =
            ZipArchive.ZipArchiveFromEntries(TestSetup.CounterElmWebApp);

        var deploymentTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                ZipArchive.EntriesFromZipArchive(deploymentZipArchive).ToImmutableList());

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup();

        using (var server = testSetup.StartWebHost())
        {
            using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
            {
                var deployAppConfigResponse =
                    await adminClient.PostAsync(
                        StartupAdminInterface.PathApiDeployAndInitAppState,
                        new ByteArrayContent(deploymentZipArchive));

                deployAppConfigResponse.IsSuccessStatusCode
                    .Should().BeTrue("deploy response IsSuccessStatusCode");

                var getAppConfigResponse =
                    await adminClient.GetAsync(StartupAdminInterface.PathApiGetDeployedAppConfig);

                getAppConfigResponse.IsSuccessStatusCode
                    .Should().BeTrue("get-app-config response IsSuccessStatusCode");

                var getAppResponseContent =
                    await getAppConfigResponse.Content.ReadAsByteArrayAsync();

                var responseAppConfigTree =
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(getAppResponseContent).ToImmutableList());

                PineValueHashTree.ComputeHashSorted(responseAppConfigTree).ToArray()
                    .Should().Equal(PineValueHashTree.ComputeHashSorted(deploymentTree).ToArray(),
                    "Get the same configuration back.");
            }

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatches.First())
            {
                using var publicAppClient = testSetup.BuildPublicAppHttpClient();

                var httpResponse =
                    await publicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse,
                    "server response matches " + expectedResponse);
            }

            using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
            {
                var deployHttpResponse =
                    await adminClient.PostAsync(
                        StartupAdminInterface.PathApiDeployAndInitAppState,
                        new ByteArrayContent(deploymentZipArchive));
            }
        }

        foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
        {
            using var server = testSetup.StartWebHost();

            using (var adminClient =
                testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                    testSetup.BuildAdminInterfaceHttpClient()))
            {
                var deployHttpResponse =
                    await adminClient.PostAsync(
                        StartupAdminInterface.PathApiDeployAndMigrateAppState,
                        new ByteArrayContent(deploymentZipArchive));

                var deployHttpResponseBody =
                    await deployHttpResponse.Content.ReadAsStringAsync();

                deployHttpResponse.IsSuccessStatusCode
                    .Should().BeTrue("deploy response IsSuccessStatusCode (response body: " + deployHttpResponseBody + ")");
            }

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
            {
                using var client = testSetup.BuildPublicAppHttpClient();

                var httpResponse =
                    await client.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse,
                    "server response matches " + expectedResponse);
            }
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_prevents_damaging_backend_state_with_invalid_migration()
    {
        var deploymentFiles =
            TestSetup.GetElmAppFromSubdirectoryName("test-prevent-damage-by-migrate-webapp");

        var deploymentZipArchive =
            ZipArchive.ZipArchiveFromEntries(deploymentFiles);

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: TestSetup.AppConfigComponentFromFiles(deploymentFiles));

        using var server = testSetup.StartWebHost();

        var stateToTriggerInvalidMigration =
            @"{""maybeString"":{""Just"":[""a string""]},""otherState"":""""}";

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse =
                await client.PostAsync(
                    "",
                    new StringContent(stateToTriggerInvalidMigration, System.Text.Encoding.UTF8));

            httpResponse.IsSuccessStatusCode
                .Should().BeTrue("Set state httpResponse.IsSuccessStatusCode (" + await httpResponse.Content?.ReadAsStringAsync() + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            (await httpResponse.Content?.ReadAsStringAsync())
                .Should().Be(stateToTriggerInvalidMigration,
                "Get same state back.");
        }

        using (var adminClient =
            testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
        {
            var migrateHttpResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiDeployAndMigrateAppState,
                    new ByteArrayContent(deploymentZipArchive));

            migrateHttpResponse.StatusCode.Should().Be(HttpStatusCode.BadRequest,
                "migrate-elm-state response status code is BadRequest");

            if (migrateHttpResponse.Content is not { } migrateHttpResponseContent)
            {
                throw new InvalidOperationException("No response content.");
            }

            var migrateHttpResponseContentString =
                await migrateHttpResponseContent.ReadAsStringAsync();

            migrateHttpResponseContentString.Contains("maybeString")
                .Should().BeTrue("HTTP response content contains matching message (" + migrateHttpResponseContentString + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            if (httpResponse.Content is not { } responseContent)
            {
                throw new InvalidOperationException("No response content.");
            }

            (await responseContent.ReadAsStringAsync())
                .Should().Be(stateToTriggerInvalidMigration,
                "Get same state back after attempted migration.");
        }

        var stateNotTriggeringInvalidMigration =
            @"{""maybeString"":{""Nothing"":[]},""otherState"":""sometext""}";

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse =
                await client.PostAsync(
                    "",
                    new StringContent(stateNotTriggeringInvalidMigration, System.Text.Encoding.UTF8));

            var responseContentString =
                httpResponse.Content is { } responseContent ?
                await responseContent.ReadAsStringAsync() :
                null;

            httpResponse.IsSuccessStatusCode.Should()
                .BeTrue(because: "Set state httpResponse.IsSuccessStatusCode (" + responseContentString + ")");
        }

        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
            testSetup.BuildAdminInterfaceHttpClient()))
        {
            var migrateHttpResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiDeployAndMigrateAppState,
                    new ByteArrayContent(deploymentZipArchive));

            migrateHttpResponse.IsSuccessStatusCode
                .Should().BeTrue("migrateHttpResponse.IsSuccessStatusCode (" + await migrateHttpResponse.Content?.ReadAsStringAsync() + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            (await httpResponse.Content?.ReadAsStringAsync())
                .Should().Be(stateNotTriggeringInvalidMigration.Replace("sometext", "sometext8"),
                "Get expected state from public app, reflecting the mapping coded in the Elm migration code.");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_deploy_app_config_and_migrate_elm_app_state()
    {
        var initialDeployment = ElmWebServiceAppTests.CounterWebApp;

        var migrateAndSecondDeployment =
            TestSetup.GetElmAppFromSubdirectoryName("migrate-from-int-to-string-builder-web-app");

        var migrateAndSecondDeploymentZipArchive =
            ZipArchive.ZipArchiveFromEntries(migrateAndSecondDeployment);

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: initialDeployment);

        using var server = testSetup.StartWebHost();

        var counterEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (13, 14),
                ]).ToList();

        foreach (var (serializedEvent, expectedResponse) in counterEventsAndExpectedResponses)
        {
            using var client = testSetup.BuildPublicAppHttpClient();

            var httpResponse =
                await client.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse,
                "server response matches " + expectedResponse);
        }

        using (var adminClient =
            testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
        {
            var deployAppConfigAndMigrateElmStateResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiDeployAndMigrateAppState,
                    new ByteArrayContent(migrateAndSecondDeploymentZipArchive));

            var contentString =
                await deployAppConfigAndMigrateElmStateResponse.Content.ReadAsStringAsync();

            deployAppConfigAndMigrateElmStateResponse.IsSuccessStatusCode
                .Should().BeTrue("deployAppConfigAndMigrateElmStateResponse IsSuccessStatusCode (" + contentString + ")");
        }

        await System.Threading.Tasks.Task.Delay(TimeSpan.FromSeconds(1));

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var initialGetResponse = await client.GetAsync("");

            (await initialGetResponse.Content.ReadAsStringAsync())
                .Should().Be("14",
                "State migrated from counter app");

            var firstPostResponse =
                await client.PostAsync("", new StringContent("-part-a", System.Text.Encoding.UTF8));

            firstPostResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var secondPostResponse =
                await client.PostAsync("", new StringContent("-part-b", System.Text.Encoding.UTF8));

            secondPostResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var finalGetResponse = await client.GetAsync("");

            (await finalGetResponse.Content.ReadAsStringAsync())
                .Should().Be("14-part-a-part-b",
                "State after multiple posts");
        }
    }

    [Fact(Skip = "TODO: Review revert functionality")]
    public async System.Threading.Tasks.Task Web_host_supports_revert_to_earlier_process_state()
    {
        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        var firstBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(0);

        var secondBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(1);

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp);

        using var server = testSetup.StartWebHost();

        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }

        var processVersionAfterFirstBatch =
            ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.HashBase16FromCompositionRecord(
                testSetup
                .BuildProcessStoreReaderInFileDirectory()
                .EnumerateSerializedCompositionLogRecordsReverse().First());

        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }

        using (var adminClient =
            testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
        {
            var revertResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiRevertProcessTo + "/" + processVersionAfterFirstBatch,
                    null);

            revertResponse.IsSuccessStatusCode
                .Should().BeTrue("revertResponse IsSuccessStatusCode (" +
                await revertResponse.Content.ReadAsStringAsync() + ")");
        }

        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Tooling_supports_replicate_process_from_remote_host()
    {
        var originalHostAdminPassword = "original-host-password-678";

        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        var firstBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(0);

        var secondBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(1);

        HttpClient? originalServerPublicAppClient = null;

        var replicaAdminInterfaceUrl = "http://localhost:18790";
        var replicaPublicAppUrl = "http://localhost:18791";
        var replicaAdminPassword = "replica-admin-password";

        WebHostAdminInterfaceTestSetup? replicaSetup = null;

        using (var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                adminPassword: originalHostAdminPassword,
                deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp))
        {
            using var server = testSetup.StartWebHost();

            originalServerPublicAppClient = testSetup.BuildPublicAppHttpClient();

            foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
            {
                var httpResponse =
                    await originalServerPublicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }

            replicaSetup =
                WebHostAdminInterfaceTestSetup.Setup(
                    webHostBuilderMap: null,
                    adminWebHostUrlOverride: replicaAdminInterfaceUrl,
                    publicWebHostUrlOverride: replicaPublicAppUrl,
                    adminPassword: replicaAdminPassword);

            using var replicaHost = replicaSetup.StartWebHost();

            ElmTime.RunServer.ReplicateProcessAndLogToConsole(
                site: replicaAdminInterfaceUrl,
                sitePassword: replicaAdminPassword,
                sourcePath: testSetup.AdminWebHostUrl,
                sourcePassword: originalHostAdminPassword);
        }

        using (var replicaHost = replicaSetup.StartWebHost())
        {
            {
                // Assert the original server does not run anymore.

                try
                {
                    var httpResponse =
                        await originalServerPublicAppClient.PostAsync(
                            "",
                            new StringContent("test", System.Text.Encoding.UTF8));

                    throw new Exception("HTTP client should signal connection failed.");
                }
                catch (Exception e)
                    // Message seen on Windows
                    when (e.ToString().Contains("No connection could be made because the target machine actively refused it"))
                {
                }
                catch (Exception e)
                    // Message seen on Ubuntu
                    when (e.ToString().Contains("Connection refused"))
                {
                }
            }

            foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
            {
                using var client = replicaSetup.BuildPublicAppHttpClient();

                var httpResponse =
                    await client.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_truncate_process_history()
    {
        DateTimeOffset persistentProcessHostDateTime =
            new(year: 2021, month: 7, day: 13, hour: 13, 0, 0, TimeSpan.Zero);

        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        var firstBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(0);

        var secondBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(1);

        var thirdBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(2);

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                persistentProcessHostDateTime: () => persistentProcessHostDateTime,
                deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp);

        int countFilesInProcessFileStore() =>
            testSetup.BuildProcessStoreFileStoreReaderInFileDirectory()
            .ListFilesInDirectory(ImmutableList<string>.Empty)?.Count() ?? 0;

        using var server = testSetup.StartWebHost();

        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }

        persistentProcessHostDateTime += TimeSpan.FromHours(3);

        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }

        var numberOfFilesBefore = countFilesInProcessFileStore();

        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
            testSetup.BuildAdminInterfaceHttpClient()))
        {
            var truncateResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiTruncateProcessHistory, null);

            truncateResponse.IsSuccessStatusCode
                .Should().BeTrue("truncateResponse IsSuccessStatusCode (" +
                await truncateResponse.Content.ReadAsStringAsync() + ")");
        }

        var numberOfFilesAfter = countFilesInProcessFileStore();

        numberOfFilesAfter.Should().BeLessThan(numberOfFilesBefore, because:
            "Number of files in store is lower after truncate request.");

        foreach (var (serializedEvent, expectedResponse) in thirdBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be(expectedResponse, "server response");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_clock_jumping_back_does_not_prevent_restoring_process_state()
    {
        DateTimeOffset persistentProcessHostDateTime =
            new(year: 2021, month: 7, day: 13, hour: 13, 0, 0, TimeSpan.Zero);

        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches =
            allEventsAndExpectedResponses.Batch(3).ToList();

        var firstBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(0);

        var secondBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(1);

        var thirdBatchOfCounterAppEvents =
            eventsAndExpectedResponsesBatches.ElementAt(2);

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            persistentProcessHostDateTime: () => persistentProcessHostDateTime,
            deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp);

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
            {
                var httpResponse =
                    await publicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }

            persistentProcessHostDateTime -= TimeSpan.FromDays(1);

            foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
            {
                var httpResponse =
                    await publicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }
        }

        persistentProcessHostDateTime -= TimeSpan.FromDays(1);

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            foreach (var (serializedEvent, expectedResponse) in thirdBatchOfCounterAppEvents)
            {
                var httpResponse =
                    await publicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }
        }
    }

    /// <summary>
    /// Since we cannot depend on writes to the process store happening atomic, we consider scenarios in which a starting 
    /// server finds a store with the last composition partially written to the representation on the file system.
    /// </summary>
    [Fact]
    public async System.Threading.Tasks.Task Web_host_crash_while_writing_to_store_does_not_prevent_restoring_process_state()
    {
        var persistentProcessHostDateTime =
            new DateTimeOffset(year: 2021, month: 8, day: 17, hour: 16, 0, 0, TimeSpan.Zero);

        var allEventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                ]).ToList();

        var fileStoreWriter = new RecordingFileStoreWriter();

        IFileStoreReader getCurrentFileStoreReader() =>
            fileStoreWriter.ReaderFromAppliedOperationsOnEmptyStore();

        var fileStoreReader = new DelegatingFileStoreReader
        (
            GetFileContentDelegate: path => getCurrentFileStoreReader().GetFileContent(path),
            ListFilesInDirectoryDelegate: path => getCurrentFileStoreReader().ListFilesInDirectory(path)
        );

        var fileStore = new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);

        static async System.Threading.Tasks.Task<string> getCurrentCounterValueFromHttpClientAsync(
            HttpClient httpClient)
        {
            var httpResponse =
                await httpClient.PostAsync(
                    "",
                    new StringContent(
                        System.Text.Json.JsonSerializer.Serialize(new { addition = 0 }), System.Text.Encoding.UTF8));

            return await httpResponse.Content.ReadAsStringAsync();
        }

        using (var originalTestSetup = WebHostAdminInterfaceTestSetup.Setup(
            fileStore: fileStore,
            persistentProcessHostDateTime: () => persistentProcessHostDateTime,
            deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp))
        {
            using var server = originalTestSetup.StartWebHost();

            using var publicAppClient = originalTestSetup.BuildPublicAppHttpClient();

            foreach (var (serializedEvent, expectedResponse) in allEventsAndExpectedResponses)
            {
                var httpResponse =
                    await publicAppClient.PostAsync(
                        "",
                        new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");

                persistentProcessHostDateTime += TimeSpan.FromSeconds(1);
            }
        }

        var storeOriginalHistory =
            fileStoreWriter.History.ToImmutableList();

        var storeHistoryLessTrailingReduction =
            storeOriginalHistory
            .SkipLastWhile(writeOperation =>
            writeOperation.SetFileContent?.path
            ?.Any((segment) => segment.Contains("reduction", StringComparison.OrdinalIgnoreCase)) ?? false);

        var lastWriteOperation = storeHistoryLessTrailingReduction.Last();

        lastWriteOperation.AppendFileContent?.path.Should().NotBeNull("Last write operation was append");

        var storeHistoryWithCrash =
            storeHistoryLessTrailingReduction
            .SkipLast(1)
            .Append(new RecordingFileStoreWriter.WriteOperation
            {
                AppendFileContent = (lastWriteOperation.AppendFileContent.Value.path, Enumerable.Repeat((byte)4, 123).ToArray()),
            });

        var fileStoreReaderAfterCrash =
            RecordingFileStoreWriter.WriteOperation.ReaderFromAppliedOperationsOnEmptyStore(storeHistoryWithCrash);

        using (var testSetupAfterCrash = WebHostAdminInterfaceTestSetup.Setup(
            fileStore: new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReaderAfterCrash),
            persistentProcessHostDateTime: () => persistentProcessHostDateTime))
        {
            using var server = testSetupAfterCrash.StartWebHost();

            using var publicAppClient = testSetupAfterCrash.BuildPublicAppHttpClient();

            (await getCurrentCounterValueFromHttpClientAsync(publicAppClient))
                .Should().Be(allEventsAndExpectedResponses.SkipLast(1).Last().expectedResponse);
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Tooling_supports_deploy_app_directly_on_process_store()
    {
        var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        var deployReport = ElmTime.Program.DeployApp(
            sourcePath: "./../../../../example-apps/docker-image-default-app",
            site: testDirectory,
            siteDefaultPassword: null,
            initElmAppState: true,
            promptForPasswordOnConsole: false);

        await using (var restoredProcess =
            PersistentProcessLiveRepresentation.LoadFromStoreAndRestoreProcess(
                new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(
                    new FileStoreFromSystemIOFile(testDirectory)),
                new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.DiscardingStoreWriter(),
                cancellationToken: default,
                getDateTimeOffset: () => DateTimeOffset.UtcNow,
                logger: null)
            .Extract(err => throw new Exception(err)).process)
        {
            var restoredProcessLastDeployedAppComponent = restoredProcess.LastAppConfig.AppConfigComponent;

            restoredProcessLastDeployedAppComponent.Should().NotBeNull("Restored process has app deployed.");

            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(restoredProcessLastDeployedAppComponent).Span)
                .Should().Be(deployReport.filteredSourceCompositionId,
                "App ID in restored process equals app ID from deployment report.");
        }

        Directory.Delete(testDirectory, recursive: true);
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_long_polling()
    {
        var appSourceFiles =
            TestSetup.GetElmAppFromSubdirectoryName("http-long-polling");

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: TestSetup.AppConfigComponentFromFiles(appSourceFiles));

        using var server = testSetup.StartWebHost();

        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        for (var delay = 0; delay < 5; ++delay)
        {
            var expectedDelayMilliseconds = delay * 1000;

            var httpRequest = new HttpRequestMessage(HttpMethod.Get, "");

            httpRequest.Headers.Add("delay-milliseconds", expectedDelayMilliseconds.ToString());

            var httpStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var httpResponse =
                await publicAppClient.SendAsync(httpRequest);

            var responseContent =
                await httpResponse.Content.ReadAsByteArrayAsync();

            httpStopwatch.Stop();

            httpStopwatch.ElapsedMilliseconds.Should().BeGreaterThanOrEqualTo(expectedDelayMilliseconds,
                "Actual delay should be greater than or equal to expected delay");

            var additionalDelayMilliseconds =
                httpStopwatch.ElapsedMilliseconds - expectedDelayMilliseconds;

            additionalDelayMilliseconds.Should().BeLessThan(1_700);
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Web_host_supports_long_polling_sandbox()
    {
        var appSourceFiles =
            TestSetup.GetElmAppFromSubdirectoryName("http-long-polling");

        var appSourceTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(appSourceFiles);

        var webServiceConfig =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                appSourceTree,
                ["src", "Backend", "Main.elm"]);

        var webServiceApp =
            new MutatingWebServiceApp(webServiceConfig);

        var timeUpdateTaskCancellation =
            new System.Threading.CancellationTokenSource();

        var timeUpdateTask =
            System.Threading.Tasks.Task.Run(
                () =>
                {
                    while (!timeUpdateTaskCancellation.Token.IsCancellationRequested)
                    {
                        System.Threading.Tasks.Task.Delay(100).Wait();

                        webServiceApp.UpdateForPosixTime(
                            posixTimeMilli: DateTimeOffset.UtcNow.ToUnixTimeMilliseconds());
                    }
                },
                cancellationToken: timeUpdateTaskCancellation.Token);

        for (var delay = 0; delay < 5; ++delay)
        {
            var expectedDelayMilliseconds = delay * 1000;

            var httpRequest =
                new WebServiceInterface.HttpRequestProperties(
                    Method: "POST",
                    Uri: "/test",
                    Body: null,
                    Headers:
                    [new WebServiceInterface.HttpHeader(
                        Name: "delay-milliseconds",
                        Values: [expectedDelayMilliseconds.ToString()])]);

            var httpStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var httpResponse =
                await webServiceApp.HttpRequestSendAsync(
                    new WebServiceInterface.HttpRequestEventStruct
                    (
                        HttpRequestId: "r-" + DateTimeOffset.UtcNow.ToUnixTimeMilliseconds(),
                        PosixTimeMilli: DateTimeOffset.UtcNow.ToUnixTimeMilliseconds(),
                        Request: httpRequest,
                        RequestContext: new WebServiceInterface.HttpRequestContext(ClientAddress: null)
                    ));

            httpStopwatch.Stop();

            httpStopwatch.ElapsedMilliseconds.Should().BeGreaterThanOrEqualTo(expectedDelayMilliseconds,
                "Actual delay should be greater than or equal to expected delay");

            var additionalDelayMilliseconds =
                httpStopwatch.ElapsedMilliseconds - expectedDelayMilliseconds;

            additionalDelayMilliseconds.Should().BeLessThan(1_700);
        }

        await timeUpdateTaskCancellation.CancelAsync();
    }

    [Fact]
    public async System.Threading.Tasks.Task Event_resulting_in_same_state_incurs_no_storage_expenses()
    {
        var fileStoreWriter = new RecordingFileStoreWriter();

        int fileStoreHistoryCountAllOperations() =>
            fileStoreWriter.History.Count();

        IFileStoreReader getCurrentFileStoreReader() =>
            fileStoreWriter.ReaderFromAppliedOperationsOnEmptyStore();

        var fileStoreReader = new DelegatingFileStoreReader
        (
            GetFileContentDelegate:
            path => getCurrentFileStoreReader().GetFileContent(path),

            ListFilesInDirectoryDelegate:
            path => getCurrentFileStoreReader().ListFilesInDirectory(path)
        );

        var fileStore = new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                fileStore: fileStore,
                deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp);

        using var server = testSetup.StartWebHost();

        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        {
            // Warmup

            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    System.Net.Http.Json.JsonContent.Create(new { addition = 0 }));
        }

        {
            var storeOperationCountBefore =
                fileStoreHistoryCountAllOperations();

            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    System.Net.Http.Json.JsonContent.Create(new { addition = 13 }));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be("13", "server response");

            var storeOperationCountIncrease =
                fileStoreHistoryCountAllOperations() - storeOperationCountBefore;

            storeOperationCountIncrease.Should()
                .BeGreaterThan(0, "Store operations increase should be greater than 0");
        }


        {
            var storeOperationCountBefore =
                fileStoreHistoryCountAllOperations();

            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    System.Net.Http.Json.JsonContent.Create(new { addition = 0 }));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be("13", "server response");

            var storeOperationCountIncrease =
                fileStoreHistoryCountAllOperations() - storeOperationCountBefore;

            storeOperationCountIncrease.Should().Be(0, "Store operations increase");
        }
    }

    [Fact]
    public async System.Threading.Tasks.Task Volatile_process_from_local_blob()
    {
        var appSourceFiles =
            TestSetup.GetElmAppFromSubdirectoryName("volatile-process-from-local-blob");

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: TestSetup.AppConfigComponentFromFiles(appSourceFiles));

        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        var httpResponse =
            await publicAppClient.GetAsync("");

        var responseContent =
            await httpResponse.Content.ReadAsStringAsync();

        responseContent.Should().Be("value from local assembly");
    }

    [Fact]
    public async System.Threading.Tasks.Task Elm_webservice_json_decoder_accepts_pascal_case_record_fields()
    {
        var adminPassword = "test";

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: ElmWebServiceAppTests.CounterWebApp,
            adminPassword: adminPassword);

        using var server = testSetup.StartWebHost();

        using var publicClient = testSetup.BuildPublicAppHttpClient();

        using var adminClient = testSetup.BuildAdminInterfaceHttpClient();

        testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(adminClient);

        {
            var httpResponse =
                await publicClient.PostAsync("", new StringContent("""{ "addition" : 3 }"""));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            httpResponseContent.Should().Be("3", "response content");
        }

        {
            var httpResponse =
                await publicClient.PostAsync("", new StringContent("""{ "Addition" : 1 }"""));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            httpResponse.IsSuccessStatusCode.Should().BeTrue();

            httpResponseContent.Should().Be("4", "response content");
        }

        {
            var httpResponse =
                await publicClient.PostAsync("", new StringContent("""{ "aDdition" : 1 }"""));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            httpResponse.IsSuccessStatusCode.Should().BeFalse();

            httpResponseContent.Should().NotBe("5", "response content");
        }
    }

    private class FileStoreFromDelegates(
        Action<IImmutableList<string>, ReadOnlyMemory<byte>> setFileContent,
        Action<IImmutableList<string>, ReadOnlyMemory<byte>> appendFileContent,
        Action<IImmutableList<string>> deleteFile,
        Func<IImmutableList<string>, ReadOnlyMemory<byte>?> getFileContent,
        Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> listFilesInDirectory)
        : IFileStore
    {
        public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
            appendFileContent(path, fileContent);

        public void DeleteFile(IImmutableList<string> path) =>
            deleteFile(path);

        public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path) =>
            getFileContent(path);

        public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
            listFilesInDirectory(directoryPath);

        public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent) =>
            setFileContent(path, fileContent);
    }

    private record Web_host_propagates_HTTP_headers_Response_Entry(
        string name,
        IReadOnlyList<string> values);

    private static HttpResponseMessage HttpPostStringContentAtRoot(
        HttpClient client, string requestContent) =>
            client.PostAsync("", new StringContent(requestContent, System.Text.Encoding.UTF8)).Result;
}
