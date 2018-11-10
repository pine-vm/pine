using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Kalmit.PersistentProcess.WebHost;
using MoreLinq;
using System.Collections.Generic;
using System;
using System.Net;
using System.Net.Http;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostTestSetup : IDisposable
    {
        readonly string testDirectory;

        readonly Func<DateTimeOffset> persistentProcessHostDateTime;

        string WebAppConfigFilePath => Path.Combine(testDirectory, "web-app");

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer() =>
            new Microsoft.AspNetCore.TestHost.TestServer(
                Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                .WithSettingProcessStoreDirectoryPath(ProcessStoreDirectory)
                .WithSettingWebAppConfigurationFilePath(WebAppConfigFilePath)
                .WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostTestSetup Setup(
            WebAppConfiguration webAppConfig,
            Func<DateTimeOffset> persistentProcessHostDateTime = null)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostTestSetup(testDirectory, persistentProcessHostDateTime);

            var webAppConfigFilePath = setup.WebAppConfigFilePath;

            Directory.CreateDirectory(Path.GetDirectoryName(webAppConfigFilePath));

            File.WriteAllBytes(webAppConfigFilePath, ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles()));

            return setup;
        }

        public void Dispose()
        {
            Directory.Delete(testDirectory, true);
        }

        WebHostTestSetup(string testDirectory, Func<DateTimeOffset> persistentProcessHostDateTime)
        {
            this.testDirectory = testDirectory;
            this.persistentProcessHostDateTime = persistentProcessHostDateTime;
        }
    }

    [TestClass]
    public class TestWebHost
    {
        [TestMethod]
        public void Web_host_stores_reduction_every_hour()
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

            using (var testSetup = WebHostTestSetup.Setup(
                TestElmWebAppHttpServer.CounterWebApp, () => persistentProcessHostDateTime))
            {
                IEnumerable<string> ReadStoredReductionFileRelativePaths()
                {
                    System.Threading.Thread.Sleep(1111);  //  Storing reduction may be completed after client has received response.
                    return new ProcessStore.ProcessStoreInFileDirectory(testSetup.ProcessStoreDirectory, null).ReductionsFilesNames();
                }

                using (var server = testSetup.BuildServer())
                {
                    //  Do not depend on an exact number of reductions stored on initialization: Send one request before measuring the number of stored reductions.
                    using (var client = server.CreateClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new StringContent("", System.Text.Encoding.UTF8)).Result;

                        var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;
                    }

                    foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
                    {
                        var beforeBatchStoredReductionsCount = ReadStoredReductionFileRelativePaths().Count();

                        letTimePassInPersistentProcessHost(TimeSpan.FromHours(1));

                        foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                        {
                            letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(4));

                            using (var client = server.CreateClient())
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
        public void Web_host_continues_composition_log_on_new_file_every_calendar_day()
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

            using (var testSetup = WebHostTestSetup.Setup(
                TestElmWebAppHttpServer.CounterWebApp, () => persistentProcessHostDateTime))
            {
                ProcessStore.ProcessStoreInFileDirectory BuildProcessStore() =>
                    new ProcessStore.ProcessStoreInFileDirectory(testSetup.ProcessStoreDirectory, null);

                IEnumerable<string> ReadCompositionLogFilesNames() =>
                    BuildProcessStore().EnumerateCompositionsLogFilesNames();

                string FirstReductionRecordFile = null;

                void deleteAllReductionRecordFilesExceptTheFirstOne()
                {
                    var filesNamesExceptTheFirst =
                        BuildProcessStore().ReductionsFilesNames().Except(new[] { FirstReductionRecordFile }).ToList();

                    filesNamesExceptTheFirst.ForEach(File.Delete);

                    Assert.IsTrue(BuildProcessStore().ReductionsFilesNames().Count() <= 1);
                }

                string PostStringContentAndGetResponseStringFromServer(
                    Microsoft.AspNetCore.TestHost.TestServer server, string requestContent)
                {
                    try
                    {
                        using (var client = server.CreateClient())
                        {
                            var httpResponse =
                                client.PostAsync("", new StringContent(requestContent, System.Text.Encoding.UTF8)).Result;

                            return httpResponse.Content.ReadAsStringAsync().Result;
                        }
                    }
                    finally
                    {
                        FirstReductionRecordFile =
                            FirstReductionRecordFile ?? BuildProcessStore().ReductionsFilesNames().FirstOrDefault();
                    }
                }

                using (var server = testSetup.BuildServer())
                {
                    //  Do not depend on an exact number of files stored on initialization: Send one request before measuring the number of stored files.
                    PostStringContentAndGetResponseStringFromServer(server, "");
                }

                foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
                {
                    var beforeBatchCompositionLogFiles = ReadCompositionLogFilesNames().ToList();

                    letTimePassInPersistentProcessHost(TimeSpan.FromDays(1));

                    var batchDayCompositionLogFileNameExpectedStart =
                        persistentProcessHostDateTime.ToString("yyyy-MM-dd");

                    //  Test that restoring works over all composition log files.
                    deleteAllReductionRecordFilesExceptTheFirstOne();

                    using (var server = testSetup.BuildServer())
                    {
                        foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                        {
                            letTimePassInPersistentProcessHost(TimeSpan.FromMinutes(4));

                            var httpResponseContent = PostStringContentAndGetResponseStringFromServer(server, serializedEvent);

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");

                            var inBatchNewCompositionLogFiles =
                                ReadCompositionLogFilesNames().Except(beforeBatchCompositionLogFiles).ToList();

                            Assert.AreEqual(
                                1,
                                inBatchNewCompositionLogFiles.Count,
                                "Exactly one new composition log files since start of batch.");

                            var newCompositionLogFileName = Path.GetFileName(inBatchNewCompositionLogFiles.Single());

                            Assert.IsTrue(
                                newCompositionLogFileName.StartsWith(batchDayCompositionLogFileNameExpectedStart),
                                "File name of new composition log file ('" + newCompositionLogFileName +
                                "') starts with '" + batchDayCompositionLogFileNameExpectedStart + "')");
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
                .WithMap(
                    new WebAppConfigurationMap
                    {
                        MapsFromRequestUrlToStaticFileName = new[]
                        {
                            new WebAppConfigurationMap.ConditionalMapFromStringToString
                            {
                                matchingRegexPattern = "^.+/subdirectory/(.+)$",
                                resultString = nameof(defaultStaticFileInSubdirectory),
                            },
                            new WebAppConfigurationMap.ConditionalMapFromStringToString
                            {
                                matchingRegexPattern = "^(?!.+/" + processEventPath + "$).*",
                                resultString = nameof(defaultStaticFile),
                            },
                        },
                    })
                .WithStaticFiles(
                    new[]
                    {
                        (nameof(defaultStaticFile), defaultStaticFile),
                        (nameof(defaultStaticFileInSubdirectory), defaultStaticFileInSubdirectory),
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

            using (var testSetup = WebHostTestSetup.Setup(webAppConfig))
            {
                string PostAppEventAndGetResponseStringFromServer(
                    Microsoft.AspNetCore.TestHost.TestServer server, string appEvent)
                {
                    using (var client = server.CreateClient())
                    {
                        var httpResponse =
                            client.PostAsync(processEventPath, new StringContent(appEvent, System.Text.Encoding.UTF8)).Result;

                        return httpResponse.Content.ReadAsStringAsync().Result;
                    }
                }

                using (var server = testSetup.BuildServer())
                {
                    using (var client = server.CreateClient())
                    {
                        void assertHttpResponseForStaticFile(string path, byte[] expectedFile)
                        {
                            try
                            {
                                var httpResponse = client.GetAsync(path).Result;

                                Assert.AreEqual(HttpStatusCode.OK, httpResponse.StatusCode, "HTTP GET response status code.");

                                var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

                                CollectionAssert.AreEqual(expectedFile, responseContent, "HTTP GET response content.");

                                Assert.AreEqual(
                                    HttpStatusCode.MethodNotAllowed,
                                    client.PostAsync(path, new StringContent("")).Result.StatusCode,
                                    "HTTP POST response status code.");

                                Assert.AreEqual(
                                    HttpStatusCode.MethodNotAllowed,
                                    client.PutAsync(path, new StringContent("")).Result.StatusCode,
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
                        var httpResponseContent = PostAppEventAndGetResponseStringFromServer(server, serializedEvent);

                        Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                    }
                }
            }
        }
    }
}
