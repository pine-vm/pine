using ElmTime.Platform.WebService;
using FluentAssertions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
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
using static MoreLinq.Extensions.BatchExtension;
using static MoreLinq.Extensions.SkipLastWhileExtension;

namespace TestElmTime;

[TestClass]
public class WebServiceTests
{
    [TestMethod]
    [Ignore("Current simplified persistence stores reduction on every event")]
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

        Assert.IsTrue(
            2 < eventsAndExpectedResponsesBatches.Count,
            "More than two batches of events to test with.");

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

                using (var client = testSetup.BuildPublicAppHttpClient())
                {
                    var httpResponse =
                        await client.PostAsync(
                            "",
                            new StringContent(serializedEvent, System.Text.Encoding.UTF8));

                    var httpResponseContent =
                        await httpResponse.Content.ReadAsStringAsync();

                    Assert.AreEqual(
                        expectedResponse,
                        httpResponseContent,
                        false,
                        "server response");
                }

                Assert.AreEqual(
                    beforeBatchStoredReductionsCount + 1,
                    ReadStoredReductionFileRelativePaths().Count(),
                    "Number of stored reductions has increased by one since previous batch.");
            }
        }
    }

    [TestMethod]
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

            Assert.AreEqual(HttpStatusCode.OK, httpResponse.StatusCode);

            var responseContent =
                await httpResponse.Content.ReadAsByteArrayAsync();

            var inspectResponseContent =
                System.Text.Encoding.UTF8.GetString(responseContent);

            CollectionAssert.AreEqual(demoFile.content.ToArray(), responseContent);
        }

        {
            var httpResponse =
                await publicAppClient.GetAsync("readme-md");

            Assert.AreEqual(HttpStatusCode.OK, httpResponse.StatusCode);

            var responseContent =
                await httpResponse.Content.ReadAsStringAsync();

            Assert.AreEqual(
                "A text file we will integrate using UTF8 encoding ⚓\nNewline and special chars:\"'",
                responseContent);
        }

        {
            var httpResponse =
                await publicAppClient.GetAsync("alpha-file-via-other-interface-module");

            Assert.AreEqual(HttpStatusCode.OK, httpResponse.StatusCode);

            var responseContent =
                await httpResponse.Content.ReadAsStringAsync();

            Assert.AreEqual("Text file content", responseContent);
        }
    }

    [TestMethod]
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
            fileStoreWriter.Apply(new EmptyFileStoreReader());

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

        int storeAppendCountBeforeSecondBatch =
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

        int storeAppendCountDuringSecondBatch =
            fileStoreHistoryCountAppendOperations() - storeAppendCountBeforeSecondBatch;

        storeAppendCountDuringSecondBatch.Should()
            .BeLessThan(
            requestBatchSize / 2,
            "Not all events from the second batch have been stored.");

        letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(rateLimitWindowSize));

        int storeAppendCountBeforeThirdBatch =
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

        int storeAppendCountAfterThirdBatch =
            fileStoreHistoryCountAppendOperations();

        storeAppendCountAfterThirdBatch.Should()
            .BeGreaterThanOrEqualTo(storeAppendCountBeforeThirdBatch + requestBatchSize,
            "All events from the third batch have been stored.");
    }

    [TestMethod]
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

        Assert.AreEqual(
            HttpStatusCode.OK,
            (await PostStringContentToPublicAppAsync(
                "small enough content" + new string('_', sufficientlySmallRequestContentSize))).StatusCode,
            "Receive OK status code for sufficiently small request.");

        Assert.AreEqual(
            HttpStatusCode.RequestEntityTooLarge,
            (await PostStringContentToPublicAppAsync("too large content" + new string('_', requestSizeLimit))).StatusCode,
            "Receive non-OK status code for too large request.");
    }

    [TestMethod]
    public async System.Threading.Tasks.Task Web_host_supports_setting_elm_app_state_only_after_authorization()
    {
        const string adminPassword = "Password_1234567";

        static async System.Threading.Tasks.Task<HttpResponseMessage> HttpSetElmAppStateAsync(
            HttpClient client, string state) =>
            await client.PostAsync(
                StartupAdminInterface.PathApiElmAppState,
                new StringContent(state, System.Text.Encoding.UTF8));

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceAppTests.StringBuilderWebApp,
                adminPassword: adminPassword);

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            Assert.AreEqual(
                "",
                await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync(),
                "Initial State");

            Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "part-a").StatusCode);

            Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "-⚙️-part-b").StatusCode);

            Assert.AreEqual(
                "part-a-⚙️-part-b",
                await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync(),
                "State After Multiple Posts");

            using (var client = testSetup.BuildAdminInterfaceHttpClient())
            {
                Assert.AreEqual(
                    HttpStatusCode.Unauthorized,
                    (await HttpSetElmAppStateAsync(client, "new-state")).StatusCode,
                        "HTTP status code for unauthorized request to set elm app state.");

                Assert.AreEqual(
                    "part-a-⚙️-part-b",
                    await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync(),
                    "State after failing to set elm app state.");

                client.DefaultRequestHeaders.Authorization =
                    new AuthenticationHeaderValue(
                        "Basic",
                        Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                            Configuration.BasicAuthenticationForAdmin(adminPassword))));

                Assert.AreEqual(
                    HttpStatusCode.OK,
                    (await HttpSetElmAppStateAsync(client, @"""new-state""")).StatusCode,
                        "HTTP status code for authorized request to set elm app state.");
            }

            Assert.AreEqual(
                "new-state",
                await (await publicAppClient.GetAsync("")).Content.ReadAsStringAsync(),
                "State after setting elm app state.");

            Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "_appendix").StatusCode);
        }

        using (var server = testSetup.StartWebHost())
        {
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            Assert.AreEqual(
                "new-state_appendix",
                publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                "State after setting elm app state, appending, and restarting server.");
        }
    }

    [TestMethod]
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

        Assert.AreEqual("application/json", response.Content.Headers.ContentType?.ToString());

        var responseContentString =
            await response.Content.ReadAsStringAsync();

        var collectionFromResponseContent =
            System.Text.Json.JsonSerializer.Deserialize<Web_host_propagates_HTTP_headers_Response_Entry[]>(
                responseContentString)!;

        var matchingEntryFromResponseContent =
            collectionFromResponseContent
            .First(entry => entry.name == appSpecificHttpRequestHeaderName);

        Assert.AreEqual(
            requestHeaderValue,
            WebUtility.UrlDecode(matchingEntryFromResponseContent.values.FirstOrDefault()),
            "Expect the HTTP request header was propagated to an entry in the response content.");

        response.Headers.TryGetValues(appSpecificHttpResponseHeaderName, out var appSpecificHttpHeaderValues);

        Assert.AreEqual(
            requestContentString,
            WebUtility.UrlDecode(appSpecificHttpHeaderValues?.FirstOrDefault()),
            "Expect the HTTP request content was propagated to the response header with name '" + appSpecificHttpResponseHeaderName + "'");
    }

    [TestMethod]
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
                        bodyAsBase64: Maybe.NothingFromNull(requestRecord.BodyAsBase64),
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

            Assert.AreEqual(
                200,
                (int)response.StatusCode,
                "Response.status code, " + responseContentString);

            var echoRequestStructure =
                System.Text.Json.JsonSerializer.Deserialize<ElmTime.Platform.WebService.InterfaceToHost.HttpRequest>(
                    responseContentString)!;

            Assert.AreEqual(
                Convert.ToBase64String(requestContentBytes).ToLowerInvariant(),
                echoRequestStructure.bodyAsBase64.WithDefault("").ToLowerInvariant(),
                "Request body is propagated.");

            var echoCustomHeaderValues =
                echoRequestStructure.headers
                .FirstOrDefault(header => header.name == customHeaderName)
                ?.values;

            Assert.AreEqual(
                1,
                echoCustomHeaderValues?.Length,
                "Custom header name is propagated.");

            Assert.AreEqual(
                customHeaderValue,
                echoCustomHeaderValues?[0],
                "Custom header value is propagated.");
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

            Assert.AreEqual(
                customContentType,
                observedContentType,
                "Sent HTTP request with content type '" + customContentType + "'.");
        }
    }

    [TestMethod]
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

        Assert.IsFalse(httpPostTask.IsCompleted, "HTTP task is not completed.");

        delayMutateInFileStore = false;

        await System.Threading.Tasks.Task.Delay(TimeSpan.FromSeconds(5));

        Assert.IsTrue(httpPostTask.IsCompleted, "HTTP task is completed.");
    }

    [TestMethod]
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

        Assert.IsTrue(
            2 < eventsAndExpectedResponsesBatches.Count,
            "More than two batches of events to test with.");

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

                Assert.IsTrue(
                    deployAppConfigResponse.IsSuccessStatusCode,
                    "deploy response IsSuccessStatusCode");

                var getAppConfigResponse =
                    await adminClient.GetAsync(StartupAdminInterface.PathApiGetDeployedAppConfig);

                Assert.IsTrue(
                    getAppConfigResponse.IsSuccessStatusCode,
                    "get-app-config response IsSuccessStatusCode");

                var getAppResponseContent =
                    await getAppConfigResponse.Content.ReadAsByteArrayAsync();

                var responseAppConfigTree =
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(getAppResponseContent).ToImmutableList());

                CollectionAssert.AreEqual(
                    PineValueHashTree.ComputeHashSorted(deploymentTree).ToArray(),
                    PineValueHashTree.ComputeHashSorted(responseAppConfigTree).ToArray(),
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
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

                Assert.IsTrue(
                    deployHttpResponse.IsSuccessStatusCode,
                    "deploy response IsSuccessStatusCode");
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response matches " + expectedResponse);
            }
        }
    }

    [TestMethod]
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

            Assert.IsTrue(
                httpResponse.IsSuccessStatusCode,
                "Set state httpResponse.IsSuccessStatusCode (" + await httpResponse.Content?.ReadAsStringAsync() + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            Assert.AreEqual(
                await httpResponse.Content?.ReadAsStringAsync(),
                stateToTriggerInvalidMigration,
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

            Assert.AreEqual(
                HttpStatusCode.BadRequest,
                migrateHttpResponse.StatusCode,
                "migrate-elm-state response status code is BadRequest");

            if (migrateHttpResponse.Content is not { } migrateHttpResponseContent)
            {
                throw new InvalidOperationException("No response content.");
            }

            var migrateHttpResponseContentString =
                await migrateHttpResponseContent.ReadAsStringAsync();

            Assert.IsTrue(
                migrateHttpResponseContentString.Contains("maybeString"),
                "HTTP response content contains matching message (" + migrateHttpResponseContentString + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            if (httpResponse.Content is not { } responseContent)
            {
                throw new InvalidOperationException("No response content.");
            }

            Assert.AreEqual(
                stateToTriggerInvalidMigration,
                await responseContent.ReadAsStringAsync(),
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

            Assert.IsTrue(
                httpResponse.IsSuccessStatusCode,
                "Set state httpResponse.IsSuccessStatusCode (" + await httpResponse.Content?.ReadAsStringAsync() + ")");
        }

        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
            testSetup.BuildAdminInterfaceHttpClient()))
        {
            var migrateHttpResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiDeployAndMigrateAppState,
                    new ByteArrayContent(deploymentZipArchive));

            Assert.IsTrue(
                migrateHttpResponse.IsSuccessStatusCode,
                "migrateHttpResponse.IsSuccessStatusCode (" + await migrateHttpResponse.Content?.ReadAsStringAsync() + ")");
        }

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var httpResponse = await client.GetAsync("");

            Assert.AreEqual(
                stateNotTriggeringInvalidMigration.Replace("sometext", "sometext8"),
                await httpResponse.Content?.ReadAsStringAsync(),
                "Get expected state from public app, reflecting the mapping coded in the Elm migration code.");
        }
    }

    [TestMethod]
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

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
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

            Assert.IsTrue(
                deployAppConfigAndMigrateElmStateResponse.IsSuccessStatusCode,
                "deployAppConfigAndMigrateElmStateResponse IsSuccessStatusCode (" + contentString + ")");
        }

        await System.Threading.Tasks.Task.Delay(TimeSpan.FromSeconds(1));

        using (var client = testSetup.BuildPublicAppHttpClient())
        {
            var initialGetResponse = await client.GetAsync("");

            Assert.AreEqual(
                "14",
                await initialGetResponse.Content.ReadAsStringAsync(),
                "State migrated from counter app");

            var firstPostResponse =
                await client.PostAsync("", new StringContent("-part-a", System.Text.Encoding.UTF8));

            Assert.AreEqual(
                HttpStatusCode.OK,
                firstPostResponse.StatusCode);

            var secondPostResponse =
                await client.PostAsync("", new StringContent("-part-b", System.Text.Encoding.UTF8));

            Assert.AreEqual(
                HttpStatusCode.OK,
                secondPostResponse.StatusCode);

            var finalGetResponse = await client.GetAsync("");

            Assert.AreEqual(
                "14-part-a-part-b",
                await finalGetResponse.Content.ReadAsStringAsync(),
                "State after multiple posts");
        }
    }

    [TestMethod]
    [Ignore("TODO: Review revert functionality")]
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

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
                "server response");
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

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
                "server response");
        }

        using (var adminClient =
            testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
                testSetup.BuildAdminInterfaceHttpClient()))
        {
            var revertResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiRevertProcessTo + "/" + processVersionAfterFirstBatch,
                    null);

            Assert.IsTrue(
                revertResponse.IsSuccessStatusCode,
                "revertResponse IsSuccessStatusCode (" +
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

            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
        }
    }

    [TestMethod]
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");
            }
        }
    }

    [TestMethod]
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

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
                "server response");
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

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
                "server response");
        }

        var numberOfFilesBefore = countFilesInProcessFileStore();

        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(
            testSetup.BuildAdminInterfaceHttpClient()))
        {
            var truncateResponse =
                await adminClient.PostAsync(
                    StartupAdminInterface.PathApiTruncateProcessHistory, null);

            Assert.IsTrue(
                truncateResponse.IsSuccessStatusCode,
                "truncateResponse IsSuccessStatusCode (" +
                await truncateResponse.Content.ReadAsStringAsync() + ")");
        }

        var numberOfFilesAfter = countFilesInProcessFileStore();

        Assert.IsTrue(
            numberOfFilesAfter < numberOfFilesBefore,
            "Number of files in store is lower after truncate request.");

        foreach (var (serializedEvent, expectedResponse) in thirdBatchOfCounterAppEvents)
        {
            var httpResponse =
                await publicAppClient.PostAsync(
                    "",
                    new StringContent(serializedEvent, System.Text.Encoding.UTF8));

            var httpResponseContent =
                await httpResponse.Content.ReadAsStringAsync();

            Assert.AreEqual(
                expectedResponse,
                httpResponseContent,
                false,
                "server response");
        }
    }

    [TestMethod]
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");
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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");
            }
        }
    }

    /// <summary>
    /// Since we cannot depend on writes to the process store happening atomic, we consider scenarios in which a starting 
    /// server finds a store with the last composition partially written to the representation on the file system.
    /// </summary>
    [TestMethod]
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

        IFileStoreReader getCurrentFileStoreReader() => fileStoreWriter.Apply(new EmptyFileStoreReader());

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

                Assert.AreEqual(
                    expectedResponse,
                    httpResponseContent,
                    false,
                    "server response");

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

        Assert.IsNotNull(lastWriteOperation.AppendFileContent?.path, "Last write operation was append");

        var storeHistoryWithCrash =
            storeHistoryLessTrailingReduction
            .SkipLast(1)
            .Append(new RecordingFileStoreWriter.WriteOperation
            {
                AppendFileContent = (lastWriteOperation.AppendFileContent.Value.path, Enumerable.Repeat((byte)4, 123).ToArray()),
            });

        var fileStoreReaderAfterCrash =
            RecordingFileStoreWriter.WriteOperation.Apply(storeHistoryWithCrash, new EmptyFileStoreReader());

        using (var testSetupAfterCrash = WebHostAdminInterfaceTestSetup.Setup(
            fileStore: new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReaderAfterCrash),
            persistentProcessHostDateTime: () => persistentProcessHostDateTime))
        {
            using var server = testSetupAfterCrash.StartWebHost();

            using var publicAppClient = testSetupAfterCrash.BuildPublicAppHttpClient();

            Assert.AreEqual(
                allEventsAndExpectedResponses.SkipLast(1).Last().expectedResponse,
                await getCurrentCounterValueFromHttpClientAsync(publicAppClient));
        }
    }

    [TestMethod]
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
            var restoredProcessLastDeployedAppComponent = restoredProcess.lastAppConfig.appConfigComponent;

            Assert.IsNotNull(
                restoredProcessLastDeployedAppComponent,
                "Restored process has app deployed.");

            Assert.AreEqual(
                deployReport.filteredSourceCompositionId,
                Convert.ToHexStringLower(PineValueHashTree.ComputeHash(restoredProcessLastDeployedAppComponent).Span),
                "App ID in restored process equals app ID from deployment report.");
        }

        Directory.Delete(testDirectory, recursive: true);
    }

    [TestMethod]
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

            Assert.IsTrue(
                expectedDelayMilliseconds <= httpStopwatch.ElapsedMilliseconds,
                "expectedDelayMilliseconds <= httpStopwatch.ElapsedMilliseconds");

            var additionalDelayMilliseconds =
                httpStopwatch.ElapsedMilliseconds - expectedDelayMilliseconds;

            Assert.IsTrue(
                additionalDelayMilliseconds < 1500,
                "additionalDelayMilliseconds < 1500");
        }
    }

    [TestMethod]
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
                    BodyAsBase64: null,
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

            Assert.IsTrue(
                expectedDelayMilliseconds <= httpStopwatch.ElapsedMilliseconds,
                "expectedDelayMilliseconds <= httpStopwatch.ElapsedMilliseconds");

            var additionalDelayMilliseconds =
                httpStopwatch.ElapsedMilliseconds - expectedDelayMilliseconds;

            Assert.IsTrue(
                additionalDelayMilliseconds < 1500,
                "additionalDelayMilliseconds < 1500");
        }

        await timeUpdateTaskCancellation.CancelAsync();
    }

    [TestMethod]
    public async System.Threading.Tasks.Task Event_resulting_in_same_state_incurs_no_storage_expenses()
    {
        var fileStoreWriter = new RecordingFileStoreWriter();

        int fileStoreHistoryCountAllOperations() =>
            fileStoreWriter.History.Count();

        IFileStoreReader getCurrentFileStoreReader() =>
            fileStoreWriter.Apply(new EmptyFileStoreReader());

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

            Assert.AreEqual(
                "13",
                httpResponseContent,
                false,
                "server response");

            var storeOperationCountIncrease =
                fileStoreHistoryCountAllOperations() - storeOperationCountBefore;

            Assert.IsTrue(
                0 < storeOperationCountIncrease,
                "Store operations increase");
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

            Assert.AreEqual(
                "13",
                httpResponseContent,
                false,
                "server response");

            var storeOperationCountIncrease =
                fileStoreHistoryCountAllOperations() - storeOperationCountBefore;

            Assert.AreEqual(
                0,
                storeOperationCountIncrease,
                "Store operations increase");
        }
    }

    [TestMethod]
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

        Assert.AreEqual("value from local assembly", responseContent);
    }

    [TestMethod]
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

            Assert.AreEqual("3", httpResponseContent, false, "response content");
        }

        {
            var httpResponse =
                await publicClient.PostAsync("", new StringContent("""{ "Addition" : 1 }"""));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            Assert.IsTrue(httpResponse.IsSuccessStatusCode);

            Assert.AreEqual("4", httpResponseContent, false, "response content");
        }

        {
            var httpResponse =
                await publicClient.PostAsync("", new StringContent("""{ "aDdition" : 1 }"""));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            Assert.IsFalse(httpResponse.IsSuccessStatusCode);

            Assert.AreNotEqual("5", httpResponseContent, false, "response content");
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
