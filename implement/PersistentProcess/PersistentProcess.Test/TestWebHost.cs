using FluentAssertions;
using Kalmit.PersistentProcess.WebHost;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestWebHost
    {
        [TestMethod]
        public void Web_host_stores_process_reduction_every_ten_minutes_by_default()
        {
            var persistentProcessHostDateTime = new DateTimeOffset(2018, 11, 4, 8, 17, 13, TimeSpan.Zero);

            void letTimePassInPersistentProcessHost(TimeSpan amount) =>
                persistentProcessHostDateTime = persistentProcessHostDateTime + amount;

            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CounterWebApp,
                persistentProcessHostDateTime: () => persistentProcessHostDateTime))
            {
                IEnumerable<string> ReadStoredReductionFileRelativePaths()
                {
                    System.Threading.Thread.Sleep(1111);  //  Storing reduction may be completed after client has received response.
                    return testSetup.BuildProcessStoreReaderInFileDirectory().ReductionsFilesNames();
                }

                using (var server = testSetup.StartWebHost())
                {
                    //  Do not depend on an exact number of reductions stored on initialization: Send one request before measuring the number of stored reductions.
                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new StringContent("", System.Text.Encoding.UTF8)).Result;

                        var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;
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
                                    client.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                                var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                                Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                            }

                            Assert.AreEqual(
                                beforeBatchStoredReductionsCount + 1,
                                ReadStoredReductionFileRelativePaths().Count(),
                                "Number of stored reductions has increased by one since previous batch.");
                        }
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_serves_static_files_besides_hosting_persistent_process()
        {
            var defaultStaticFile =
                Enumerable.Range(0, 10_000).SelectMany(elem => BitConverter.GetBytes((UInt16)elem))
                .Concat(System.Text.Encoding.UTF8.GetBytes("Default static file content from String\nAnother line"))
                .Concat(Enumerable.Range(0, 100_000).SelectMany(elem => BitConverter.GetBytes((UInt16)elem)))
                .ToArray();

            var defaultStaticFileInSubdirectory = System.Text.Encoding.UTF8.GetBytes("Default static file in 'subdirectory/'.");

            const string processEventPath = "process-event";

            var webAppConfig =
                TestElmWebAppHttpServer.CounterWebApp
                .WithJsonStructure(
                    new WebAppConfigurationJsonStructure
                    {
                        mapsFromRequestUrlToStaticFileName = new[]
                        {
                            new WebAppConfigurationJsonStructure.ConditionalMapFromStringToString
                            {
                                matchingRegexPattern = "^.+/subdirectory/(.+)$",
                                resultString = nameof(defaultStaticFileInSubdirectory),
                            },
                            new WebAppConfigurationJsonStructure.ConditionalMapFromStringToString
                            {
                                matchingRegexPattern = "^(?!.+/" + processEventPath + "$).*",
                                resultString = nameof(defaultStaticFile),
                            },
                        },
                    })
                .WithStaticFiles(
                    new[]
                    {
                        ((IImmutableList<string>)ImmutableList.Create(nameof(defaultStaticFile)), (IImmutableList<byte>)defaultStaticFile.ToImmutableList()),
                        (ImmutableList.Create(nameof(defaultStaticFileInSubdirectory)), defaultStaticFileInSubdirectory.ToImmutableList()),
                    });

            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: webAppConfig))
            {
                string PostEventToPublicAppAndGetResponseString(string appEvent)
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse =
                            publicAppClient.PostAsync(processEventPath, new StringContent(appEvent, System.Text.Encoding.UTF8)).Result;

                        return httpResponse.Content.ReadAsStringAsync().Result;
                    }
                }

                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        void assertHttpResponseForStaticFile(string path, byte[] expectedFile)
                        {
                            try
                            {
                                var httpResponse = publicAppClient.GetAsync(path).Result;

                                Assert.AreEqual(HttpStatusCode.OK, httpResponse.StatusCode, "HTTP GET response status code.");

                                var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

                                CollectionAssert.AreEqual(expectedFile, responseContent, "HTTP GET response content.");

                                Assert.AreEqual(
                                    HttpStatusCode.MethodNotAllowed,
                                    publicAppClient.PostAsync(path, new StringContent("")).Result.StatusCode,
                                    "HTTP POST response status code.");

                                Assert.AreEqual(
                                    HttpStatusCode.MethodNotAllowed,
                                    publicAppClient.PutAsync(path, new StringContent("")).Result.StatusCode,
                                    "HTTP PUT response status code.");
                            }
                            catch (Exception e)
                            {
                                throw new Exception("Failed for static file at path '" + path + "'", e);
                            }
                        }

                        Assert.ThrowsException<Exception>(
                            () => assertHttpResponseForStaticFile(
                                "", System.Text.Encoding.UTF8.GetBytes("This should not match")));

                        foreach (var pathWhichShouldBeMappedToDefaultStaticFile in new[] { "", "index.html", "almost-anything-else-too" })
                        {
                            assertHttpResponseForStaticFile(
                                pathWhichShouldBeMappedToDefaultStaticFile, defaultStaticFile);
                        }

                        foreach (var pathWhichShouldBeMappedToDefaultStaticFile in new[] { "subdirectory/a-path", "subdirectory/another-path" })
                        {
                            assertHttpResponseForStaticFile(
                                pathWhichShouldBeMappedToDefaultStaticFile, defaultStaticFileInSubdirectory);
                        }
                    }

                    foreach (var (serializedEvent, expectedResponse) in allEventsAndExpectedResponses)
                    {
                        var httpResponseContent = PostEventToPublicAppAndGetResponseString(serializedEvent);

                        Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_rate_limits_requests_before_reaching_persistent_process()
        {
            const int requestBatchSize = 100;
            const int minimumNumberOfRequestsInFastBatchExpectedToBeBlockedByRateLimit = 85;
            const int rateLimitWindowSize = 10;

            var persistentProcessHostDateTime = new DateTimeOffset(2018, 11, 12, 19, 51, 13, TimeSpan.Zero);

            void letTimePassInPersistentProcessHost(TimeSpan amount) =>
                persistentProcessHostDateTime = persistentProcessHostDateTime + amount;

            var requestsBatches =
                Enumerable.Range(0, 3)
                .Select(batchIndex =>
                    Enumerable.Range(0, requestBatchSize)
                    .Select(indexInBatch => "Batch " + batchIndex + ", Event " + indexInBatch).ToList())
                    .ToList();

            var webAppConfig =
                TestElmWebAppHttpServer.CounterWebApp
                .WithJsonStructure(
                    new WebAppConfigurationJsonStructure
                    {
                        singleRateLimitWindowPerClientIPv4Address = new WebAppConfigurationJsonStructure.RateLimitWindow
                        {
                            windowSizeInMs = 1000 * rateLimitWindowSize,
                            limit = rateLimitWindowSize,
                        },
                    });

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                deployAppConfigAndInitElmState: webAppConfig,
                persistentProcessHostDateTime: () => persistentProcessHostDateTime))
            {
                IEnumerable<string> EnumerateStoredProcessEventsHttpRequestsBodies() =>
                    testSetup.EnumerateStoredUpdateElmAppStateForEvents()
                    .Select(processEvent => processEvent?.httpRequest?.request?.bodyAsString)
                    .WhereNotNull();

                HttpResponseMessage PostStringContentToPublicApp(string requestContent)
                {
                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        return
                            client.PostAsync("", new StringContent(requestContent, System.Text.Encoding.UTF8)).Result;
                    }
                }

                IEnumerable<HttpResponseMessage> whereStatusCodeTooManyRequests(
                    IEnumerable<HttpResponseMessage> httpResponses) =>
                    httpResponses.Where(httpResponse => httpResponse.StatusCode == HttpStatusCode.TooManyRequests);

                using (var server = testSetup.StartWebHost())
                {
                    {
                        var firstBatchHttpResponses =
                            requestsBatches[0].Select(processEvent =>
                            {
                                letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(1));

                                return PostStringContentToPublicApp(processEvent);
                            }).ToList();

                        whereStatusCodeTooManyRequests(firstBatchHttpResponses).Count()
                        .Should().Be(0, "No HTTP response from the first batch has status code 'Too Many Requests'.");

                        CollectionAssert.IsSubsetOf(
                            requestsBatches[0],
                            EnumerateStoredProcessEventsHttpRequestsBodies().ToList(),
                            "All events from the first batch have been stored.");
                    }

                    {
                        var secondBatchHttpResponses =
                            requestsBatches[1].Select(processEvent =>
                            {
                                //  Process the events in the second batch in smaller timespan so that effect of the rate-limit should be observable.
                                letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(0.1));

                                return PostStringContentToPublicApp(processEvent);
                            }).ToList();

                        whereStatusCodeTooManyRequests(secondBatchHttpResponses).Count()
                        .Should().BeGreaterThan(minimumNumberOfRequestsInFastBatchExpectedToBeBlockedByRateLimit,
                            "At least this many requests in the fast batch are expected to be answered with status code 'Too Many Requests'.");

                        requestsBatches[1].Except(EnumerateStoredProcessEventsHttpRequestsBodies()).Count()
                        .Should().BeGreaterThan(minimumNumberOfRequestsInFastBatchExpectedToBeBlockedByRateLimit,
                            "At least this many requests in the fast batch are expected to be filtered out by rate-limit before reaching the persistent process.");
                    }

                    {
                        letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(rateLimitWindowSize));

                        var thirdBatchHttpResponses =
                            requestsBatches[2].Select(processEvent =>
                            {
                                letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(1));

                                return PostStringContentToPublicApp(processEvent);
                            }).ToList();

                        whereStatusCodeTooManyRequests(thirdBatchHttpResponses).Count()
                        .Should().Be(0, "No HTTP response from the third batch has status code 'Too Many Requests'.");

                        CollectionAssert.IsSubsetOf(
                            requestsBatches[2],
                            EnumerateStoredProcessEventsHttpRequestsBodies().ToList(),
                            "All events from the third batch have been stored.");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_supports_setting_elm_app_state_only_after_authorization()
        {
            const string rootPassword = "Root-Password_1234567";

            static System.Threading.Tasks.Task<HttpResponseMessage> HttpSetElmAppState(
                HttpClient client, string state) =>
                client.PostAsync(
                    Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PathApiElmAppState,
                    new StringContent(state, System.Text.Encoding.UTF8));

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                deployAppConfigAndInitElmState: TestElmWebAppHttpServer.StringBuilderWebApp,
                adminRootPassword: rootPassword))
            {
                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        Assert.AreEqual(
                            "",
                            publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "Initial State");

                        Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "part-a").StatusCode);

                        Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "-⚙️-part-b").StatusCode);

                        Assert.AreEqual(
                            "part-a-⚙️-part-b",
                            publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "State After Multiple Posts");

                        using (var client = testSetup.BuildAdminInterfaceHttpClient())
                        {
                            Assert.AreEqual(
                                HttpStatusCode.Unauthorized,
                                HttpSetElmAppState(client, "new-state").Result.StatusCode,
                                    "HTTP status code for unauthorized request to set elm app state.");

                            Assert.AreEqual(
                                "part-a-⚙️-part-b",
                                publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                                "State after failing to set elm app state.");

                            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
                                "Basic",
                                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                                    WebHost.Configuration.BasicAuthenticationForAdminRoot(rootPassword))));

                            Assert.AreEqual(
                                HttpStatusCode.OK,
                                HttpSetElmAppState(client, @"""new-state""").Result.StatusCode,
                                    "HTTP status code for authorized request to set elm app state.");
                        }

                        Assert.AreEqual(
                            "new-state",
                            publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "State after setting elm app state.");

                        Assert.AreEqual(HttpStatusCode.OK, HttpPostStringContentAtRoot(publicAppClient, "_appendix").StatusCode);
                    }
                }

                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        Assert.AreEqual(
                            "new-state_appendix",
                            publicAppClient.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "State after setting elm app state, appending, and restarting server.");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_propagates_HTTP_headers()
        {
            // This name needs to be consistent with the code in Elm app CrossPropagateHttpHeadersToAndFromBody.
            const string appSpecificHttpResponseHeaderName = "response-header-name";

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CrossPropagateHttpHeadersToAndFromBody))
            {
                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        var requestHeaderValue = "HTTP request header value.";
                        var requestContentString = "HTTP request content.";

                        const string appSpecificHttpRequestHeaderName = "request-header-name";

                        publicAppClient.DefaultRequestHeaders.Add(appSpecificHttpRequestHeaderName, WebUtility.UrlEncode(requestHeaderValue));

                        var response = publicAppClient.PostAsync("", new StringContent(requestContentString)).Result;

                        Assert.AreEqual("application/json", response.Content.Headers.ContentType.ToString());

                        var responseContentString = response.Content.ReadAsStringAsync().Result;

                        var collectionFromResponseContent =
                            Newtonsoft.Json.JsonConvert.DeserializeObject<Web_host_propagates_HTTP_headers_Response_Entry[]>(
                                responseContentString);

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
                }
            }
        }

        [Ignore("TODO. The problem here might have been masked by the use of a client from `TestServer.CreateClient` before.")]
        /*
        2020-04-29
        Observing problem at publicAppClient.SendAsync:
        One or more errors occurred. (A task was canceled.) ---> System.Threading.Tasks.TaskCanceledException: A task was canceled.
        */
        [TestMethod]
        public void Host_supports_sending_HTTP_requests()
        {
            const string echoServerUrl = "http://localhost:6789/";

            var echoWebHostBuilder =
                Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                .Configure(app =>
                {
                    app.Run(async (context) =>
                    {
                        var requestRecord = Asp.AsPersistentProcessInterfaceHttpRequest(context.Request);

                        context.Response.StatusCode = 200;

                        await context.Response.WriteAsync(Newtonsoft.Json.JsonConvert.SerializeObject(requestRecord));
                    });
                })
                .UseUrls(echoServerUrl);

            using (var echoServer = echoWebHostBuilder.Build())
            {
                echoServer.Start();

                using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: TestElmWebAppHttpServer.HttpProxyWebApp))
                {
                    using (var server = testSetup.StartWebHost())
                    {
                        using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                        {
                            var customHeaderName = "My-custom-header";
                            var customHeaderValue = "Hello!";
                            var requestContentString = "HTTP request content.";

                            var httpRequestMessage = new HttpRequestMessage(new HttpMethod("post"), "")
                            {
                                Content = new StringContent(requestContentString),
                            };

                            httpRequestMessage.Headers.Add("forward-to", echoServerUrl);

                            httpRequestMessage.Headers.Add(customHeaderName, customHeaderValue);

                            var response = publicAppClient.SendAsync(httpRequestMessage).Result;

                            var responseContentString = response.Content.ReadAsStringAsync().Result;

                            var echoRequestStructure =
                                Newtonsoft.Json.JsonConvert.DeserializeObject<InterfaceToHost.HttpRequest>(responseContentString);

                            Assert.AreEqual(
                                requestContentString,
                                echoRequestStructure.bodyAsString,
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
                                echoCustomHeaderValues[0],
                                "Custom header value is propagated.");
                        }

                        using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                        {
                            var contentTypeHeaderName = "content-type";
                            var customContentType = "application/octet-stream";

                            var httpRequestMessage = new HttpRequestMessage(new HttpMethod("post"), "")
                            {
                                Content = new ByteArrayContent(System.Text.Encoding.UTF8.GetBytes("Hello!")),
                            };

                            httpRequestMessage.Content.Headers.ContentType = new MediaTypeHeaderValue(customContentType);

                            httpRequestMessage.Headers.Add("forward-to", echoServerUrl);

                            var response = publicAppClient.SendAsync(httpRequestMessage).Result;

                            var responseContentString = response.Content.ReadAsStringAsync().Result;

                            var echoRequestStructure =
                                Newtonsoft.Json.JsonConvert.DeserializeObject<InterfaceToHost.HttpRequest>(responseContentString);

                            var observedContentType =
                                echoRequestStructure.headers
                                .SingleOrDefault(header => string.Equals(header.name, contentTypeHeaderName, StringComparison.InvariantCultureIgnoreCase))
                                ?.values.SingleOrDefault();

                            Assert.AreEqual(
                                customContentType,
                                observedContentType,
                                "Sent HTTP request with content type '" + customContentType + "'.");
                        }
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_sends_HTTP_response_only_after_write_to_history()
        {
            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CounterWebApp))
            {
                async System.Threading.Tasks.Task<HttpResponseMessage> postStringContentToPublicApp(string postContent)
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        return await
                            publicAppClient.PostAsync("", new StringContent(postContent, System.Text.Encoding.UTF8));
                    }
                }

                Action runBeforeMutateInFileStore = null;

                using (var server = testSetup.StartWebHost(
                    processStoreFileStoreMap: originalFileStore =>
                    {
                        return new FileStoreFromDelegates(
                            setFileContent: new Action<IImmutableList<string>, byte[]>((path, fileContent) =>
                            {
                                runBeforeMutateInFileStore?.Invoke();
                                originalFileStore.SetFileContent(path, fileContent);
                            }),
                            appendFileContent: new Action<IImmutableList<string>, byte[]>((path, fileContent) =>
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
                    }))
                {
                    var delayMutateInFileStore = false;

                    runBeforeMutateInFileStore = new Action(() =>
                    {
                        while (!delayMutateInFileStore)
                        {
                            System.Threading.Tasks.Task.Delay(11).Wait();
                        }
                    });

                    var httpPostTask = postStringContentToPublicApp("");

                    System.Threading.Tasks.Task.Delay(4000).Wait();

                    Assert.IsFalse(httpPostTask.IsCompleted, "HTTP task is not completed.");

                    delayMutateInFileStore = true;

                    System.Threading.Tasks.Task.Delay(1000).Wait();

                    Assert.IsTrue(httpPostTask.IsCompleted, "HTTP task is completed.");
                }
            }
        }

        [TestMethod]
        public void Web_host_supports_deploy_app_config_and_init_elm_app_state()
        {
            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            var webAppConfigZipArchive = ZipArchive.ZipArchiveFromEntries(
                TestElmWebAppHttpServer.CounterWebApp.AsFiles());

            var webAppConfigTree =
                Composition.TreeFromSetOfBlobsWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive).ToImmutableList());


            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup())
            {
                using (var server = testSetup.StartWebHost())
                {
                    using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                        testSetup.BuildAdminInterfaceHttpClient()))
                    {
                        var deployAppConfigResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState,
                            new ByteArrayContent(webAppConfigZipArchive)).Result;

                        Assert.IsTrue(deployAppConfigResponse.IsSuccessStatusCode, "deploy-app-config response IsSuccessStatusCode");

                        var getAppConfigResponse = adminClient.GetAsync(StartupAdminInterface.PathApiGetDeployedAppConfig).Result;

                        Assert.IsTrue(getAppConfigResponse.IsSuccessStatusCode, "get-app-config response IsSuccessStatusCode");

                        var getAppResponseContent = getAppConfigResponse.Content.ReadAsByteArrayAsync().Result;

                        var responseAppConfigTree =
                            Composition.TreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(getAppResponseContent).ToImmutableList());

                        CollectionAssert.AreEqual(
                            Composition.GetHash(webAppConfigTree),
                            Composition.GetHash(responseAppConfigTree),
                            "Get the same configuration back.");
                    }

                    foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatches.First())
                    {
                        using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response matches " + expectedResponse);
                        }
                    }

                    using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                        testSetup.BuildAdminInterfaceHttpClient()))
                    {
                        var deployHttpResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState,
                            new ByteArrayContent(webAppConfigZipArchive)).Result;
                    }
                }

                foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
                {
                    using (var server = testSetup.StartWebHost())
                    {
                        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                            testSetup.BuildAdminInterfaceHttpClient()))
                        {
                            var deployHttpResponse = adminClient.PostAsync(
                                StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState,
                                new ByteArrayContent(webAppConfigZipArchive)).Result;

                            Assert.IsTrue(deployHttpResponse.IsSuccessStatusCode, "deploy response IsSuccessStatusCode");
                        }

                        foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                        {
                            using (var client = testSetup.BuildPublicAppHttpClient())
                            {
                                var httpResponse =
                                    client.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                                var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                                Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response matches " + expectedResponse);
                            }
                        }
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_prevents_damaging_backend_state_with_invalid_migration()
        {
            var elmApp = TestSetup.GetElmAppFromExampleName("test-prevent-damage-by-migrate-webapp");

            var webAppConfig = new WebAppConfiguration().WithElmApp(elmApp);

            var webAppConfigZipArchive = ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles());

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: webAppConfig))
            {
                using (var server = testSetup.StartWebHost())
                {
                    var stateToTriggerInvalidMigration =
                        @"{""attemptSetMaybeStringOnMigration"":true,""maybeString"":{""Nothing"":[]},""otherState"":""""}";

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new StringContent(stateToTriggerInvalidMigration, System.Text.Encoding.UTF8)).Result;

                        Assert.IsTrue(
                            httpResponse.IsSuccessStatusCode,
                            "Set state httpResponse.IsSuccessStatusCode (" + httpResponse.Content?.ReadAsStringAsync().Result + ")");
                    }

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse = client.GetAsync("").Result;

                        Assert.AreEqual(
                            httpResponse.Content?.ReadAsStringAsync().Result,
                            stateToTriggerInvalidMigration,
                            "Get same state back.");
                    }

                    using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                        testSetup.BuildAdminInterfaceHttpClient()))
                    {
                        var migrateHttpResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState,
                            new ByteArrayContent(webAppConfigZipArchive)).Result;

                        Assert.AreEqual(
                            System.Net.HttpStatusCode.BadRequest,
                            migrateHttpResponse.StatusCode,
                            "migrate-elm-state response status code is BadRequest");

                        Assert.IsTrue(
                            (migrateHttpResponse.Content?.ReadAsStringAsync().Result ?? "").Contains("Failed to load the migrated serialized state"),
                            "HTTP response content contains matching message");
                    }

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse = client.GetAsync("").Result;

                        Assert.AreEqual(
                            stateToTriggerInvalidMigration,
                            httpResponse.Content?.ReadAsStringAsync().Result,
                            "Get same state back after attempted migration.");
                    }

                    var stateNotTriggeringInvalidMigration =
                        @"{""attemptSetMaybeStringOnMigration"":false,""maybeString"":{""Nothing"":[]},""otherState"":""sometext""}";

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new StringContent(stateNotTriggeringInvalidMigration, System.Text.Encoding.UTF8)).Result;

                        Assert.IsTrue(
                            httpResponse.IsSuccessStatusCode,
                            "Set state httpResponse.IsSuccessStatusCode (" + httpResponse.Content?.ReadAsStringAsync().Result + ")");
                    }

                    using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                        testSetup.BuildAdminInterfaceHttpClient()))
                    {
                        var migrateHttpResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState,
                            new ByteArrayContent(webAppConfigZipArchive)).Result;

                        Assert.IsTrue(
                            migrateHttpResponse.IsSuccessStatusCode,
                            "migrateHttpResponse.IsSuccessStatusCode (" + migrateHttpResponse.Content?.ReadAsStringAsync().Result + ")");
                    }

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse = client.GetAsync("").Result;

                        Assert.AreEqual(
                            stateNotTriggeringInvalidMigration.Replace("sometext", "sometext8"),
                            httpResponse.Content?.ReadAsStringAsync().Result,
                            "Get expected state from public app, reflecting the mapping coded in the Elm migration code.");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_supports_deploy_app_config_and_migrate_elm_app_state()
        {
            var initialWebAppConfig = TestElmWebAppHttpServer.CounterWebApp;

            var migrateAndSecondElmApp =
                TestSetup.GetElmAppFromExampleName("migrate-from-int-to-string-builder-web-app");

            var migrateAndSecondElmAppWebAppConfig =
                initialWebAppConfig.WithElmApp(migrateAndSecondElmApp);

            var migrateAndSecondElmAppWebAppConfigZipArchive =
                ZipArchive.ZipArchiveFromEntries(migrateAndSecondElmAppWebAppConfig.AsFiles());

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: initialWebAppConfig))
            {
                using (var server = testSetup.StartWebHost())
                {
                    var counterEventsAndExpectedResponses =
                        TestSetup.CounterProcessTestEventsAndExpectedResponses(
                            new (int addition, int expectedResponse)[]
                            {
                                (0, 0),
                                (1, 1),
                                (13, 14),
                            }).ToList();

                    foreach (var (serializedEvent, expectedResponse) in counterEventsAndExpectedResponses)
                    {
                        using (var client = testSetup.BuildPublicAppHttpClient())
                        {
                            var httpResponse =
                                client.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response matches " + expectedResponse);
                        }
                    }

                    using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                        testSetup.BuildAdminInterfaceHttpClient()))
                    {
                        var deployAppConfigAndMigrateElmStateResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState,
                            new ByteArrayContent(migrateAndSecondElmAppWebAppConfigZipArchive)).Result;

                        Assert.IsTrue(
                            deployAppConfigAndMigrateElmStateResponse.IsSuccessStatusCode,
                            "deployAppConfigAndMigrateElmStateResponse IsSuccessStatusCode (" +
                            deployAppConfigAndMigrateElmStateResponse.Content.ReadAsStringAsync().Result + ")");
                    }

                    using (var client = testSetup.BuildPublicAppHttpClient())
                    {
                        Assert.AreEqual(
                            "14",
                            client.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "State migrated from counter app");

                        Assert.AreEqual(HttpStatusCode.OK, client.PostAsync("", new StringContent("-part-a")).Result.StatusCode);

                        Assert.AreEqual(HttpStatusCode.OK, client.PostAsync("", new StringContent("-part-b")).Result.StatusCode);

                        Assert.AreEqual(
                            "14-part-a-part-b",
                            client.GetAsync("").Result.Content.ReadAsStringAsync().Result,
                            "State after multiple posts");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_supports_revert_to_earlier_process_state()
        {
            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            var firstBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.First();

            var secondBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.Skip(1).First();

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CounterWebApp))
            {
                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }

                        var processVersionAfterFirstBatch =
                            WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.HashBase16FromCompositionRecord(
                                testSetup
                                .BuildProcessStoreReaderInFileDirectory()
                                .EnumerateSerializedCompositionLogRecordsReverse().First());

                        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }

                        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                            testSetup.BuildAdminInterfaceHttpClient()))
                        {
                            var revertResponse = adminClient.PostAsync(
                                StartupAdminInterface.PathApiRevertProcessTo + "/" + processVersionAfterFirstBatch,
                                null).Result;

                            Assert.IsTrue(
                                revertResponse.IsSuccessStatusCode,
                                "revertResponse IsSuccessStatusCode (" +
                                revertResponse.Content.ReadAsStringAsync().Result + ")");
                        }

                        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }
                    }
                }
            }
        }

        [TestMethod]
        public void tooling_supports_replicate_process_from_remote_host()
        {
            var originalHostAdminPassword = "original-host-password-678";

            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            var firstBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.First();

            var secondBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.Skip(1).First();

            HttpClient originalServerPublicAppClient = null;

            var replicaAdminInterfaceUrl = "http://localhost:18790";
            var replicaPublicAppUrl = "http://localhost:18791";
            var replicaAdminPassword = "replica-admin-password";

            WebHostAdminInterfaceTestSetup replicaSetup = null;

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                adminRootPassword: originalHostAdminPassword,
                deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CounterWebApp))
            {
                using (var server = testSetup.StartWebHost())
                {
                    originalServerPublicAppClient = testSetup.BuildPublicAppHttpClient();

                    foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
                    {
                        var httpResponse =
                            originalServerPublicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                        var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                        Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                    }

                    replicaSetup = WebHostAdminInterfaceTestSetup.Setup(
                        webHostBuilderMap: null,
                        adminWebHostUrlOverride: replicaAdminInterfaceUrl,
                        publicWebHostUrlOverride: replicaPublicAppUrl,
                        adminRootPassword: replicaAdminPassword);

                    using (var replicaHost = replicaSetup.StartWebHost())
                    {
                        elm_fullstack.Program.replicateProcess(
                            destinationAdminInterface: replicaAdminInterfaceUrl,
                            destinationAdminRootPassword: replicaAdminPassword,
                            sourceAdminInterface: testSetup.AdminWebHostUrl,
                            sourceAdminRootPassword: originalHostAdminPassword);
                    }
                }
            }

            using (var replicaHost = replicaSetup.StartWebHost())
            {
                {
                    // Assert the original server does not run anymore.

                    try
                    {
                        var httpResponse =
                            originalServerPublicAppClient.PostAsync(
                                "", new StringContent("test", System.Text.Encoding.UTF8)).Result;

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
                    using (var client = replicaSetup.BuildPublicAppHttpClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                        var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                        Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                    }
                }
            }
        }

        [TestMethod]
        public void Web_host_supports_truncate_process_history()
        {
            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            var firstBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.First();

            var secondBatchOfCounterAppEvents = eventsAndExpectedResponsesBatches.Skip(1).First();

            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(
                deployAppConfigAndInitElmState: TestElmWebAppHttpServer.CounterWebApp))
            {
                int countFilesInProcessFileStore() =>
                    testSetup.BuildProcessStoreFileStoreReaderInFileDirectory()
                    .ListFilesInDirectory(ImmutableList<string>.Empty).Count();

                using (var server = testSetup.StartWebHost())
                {
                    using (var publicAppClient = testSetup.BuildPublicAppHttpClient())
                    {
                        foreach (var (serializedEvent, expectedResponse) in firstBatchOfCounterAppEvents)
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }

                        var numberOfFilesBefore = countFilesInProcessFileStore();

                        using (var adminClient = testSetup.SetDefaultRequestHeaderAuthorizeForAdminRoot(
                            testSetup.BuildAdminInterfaceHttpClient()))
                        {
                            var truncateResponse = adminClient.PostAsync(
                                StartupAdminInterface.PathApiTruncateProcessHistory, null).Result;

                            Assert.IsTrue(
                                truncateResponse.IsSuccessStatusCode,
                                "truncateResponse IsSuccessStatusCode (" +
                                truncateResponse.Content.ReadAsStringAsync().Result + ")");
                        }

                        var numberOfFilesAfter = countFilesInProcessFileStore();

                        Assert.IsTrue(
                            numberOfFilesAfter < numberOfFilesBefore,
                            "Number of files in store is lower after truncate request.");

                        foreach (var (serializedEvent, expectedResponse) in secondBatchOfCounterAppEvents)
                        {
                            var httpResponse =
                                publicAppClient.PostAsync("", new StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }
                    }
                }
            }
        }


        class FileStoreFromDelegates : IFileStore
        {
            readonly Action<IImmutableList<string>, byte[]> setFileContent;

            readonly Action<IImmutableList<string>, byte[]> appendFileContent;

            readonly Action<IImmutableList<string>> deleteFile;

            readonly Func<IImmutableList<string>, byte[]> getFileContent;

            readonly Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> listFilesInDirectory;

            public FileStoreFromDelegates(
                Action<IImmutableList<string>, byte[]> setFileContent,
                Action<IImmutableList<string>, byte[]> appendFileContent,
                 Action<IImmutableList<string>> deleteFile,
                Func<IImmutableList<string>, byte[]> getFileContent,
                Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> listFilesInDirectory)
            {
                this.setFileContent = setFileContent;
                this.appendFileContent = appendFileContent;
                this.deleteFile = deleteFile;
                this.getFileContent = getFileContent;
                this.listFilesInDirectory = listFilesInDirectory;
            }

            public void AppendFileContent(IImmutableList<string> path, byte[] fileContent) =>
                appendFileContent(path, fileContent);

            public void DeleteFile(IImmutableList<string> path) =>
                deleteFile(path);

            public byte[] GetFileContent(IImmutableList<string> path) =>
                getFileContent(path);

            public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
                listFilesInDirectory(directoryPath);

            public void SetFileContent(IImmutableList<string> path, byte[] fileContent) =>
                setFileContent(path, fileContent);
        }

        class Web_host_propagates_HTTP_headers_Response_Entry
        {
            public string name = null;

            public string[] values = null;
        }

        static HttpResponseMessage HttpPostStringContentAtRoot(
            HttpClient client, string requestContent) =>
                client.PostAsync("", new StringContent(requestContent, System.Text.Encoding.UTF8)).Result;
    }
}