using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;
using System.Security.Cryptography;
using System.Text.Json;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class TestDatabaseScale
{
    [TestMethod]
    public async Task Test_database_scale()
    {
        var sourceFiles =
            TestSetup.GetElmAppFromDirectoryPath(
                ImmutableList.Create(".", "..", "..", "..", "..", "example-apps", "test-database-scale"));

        var webAppSource = TestSetup.AppConfigComponentFromFiles(sourceFiles);

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        async Task<HttpResponseMessage> apiPostEntry(string entryId, ReadOnlyMemory<byte> entryContent)
        {
            return await publicAppClient.PostAsync("/api/entry/" + entryId, new ReadOnlyMemoryContent(entryContent));
        }

        async Task<HttpResponseMessage> apiGetEntry(string entryId)
        {
            return await publicAppClient.GetAsync("/api/entry/" + entryId);
        }

        var lastEntryIndex = 0;

        async Task postEntriesBatch(
            int entriesCount,
            int entrySizeMin,
            int entrySizeRandom)
        {
            var entriesToPost =
                Enumerable
                .Range(0, entriesCount).Select(entryIndexInBatch => new
                {
                    entryId = lastEntryIndex + entryIndexInBatch.ToString(),
                    entryContent = RandomNumberGenerator.GetBytes(entrySizeMin + RandomNumberGenerator.GetInt32(0, entrySizeRandom))
                })
                .ToImmutableList();

            foreach (var item in entriesToPost)
            {
                await apiPostEntry(item.entryId, item.entryContent);
            }

            foreach (var item in entriesToPost)
            {
                var getResponse = await apiGetEntry(item.entryId);

                CollectionAssert.AreEqual(item.entryContent, await getResponse.Content.ReadAsByteArrayAsync());
            }

            lastEntryIndex += entriesToPost.Count;
        }

        await postEntriesBatch(entriesCount: 100, entrySizeMin: 10_000, entrySizeRandom: 10);

        await postEntriesBatch(entriesCount: 400, entrySizeMin: 10_000, entrySizeRandom: 10);

        var responseJsonSerializerOptions = new JsonSerializerOptions();

        responseJsonSerializerOptions.Converters.Add(new TupleAsJsonArray.TupleConverterFactory());

        for (int i = 0; i < 100; i++)
        {
            var getResponse = await publicAppClient.GetAsync("/api/entry");

            var responseBody = await getResponse.Content.ReadAsStringAsync();

            var parsedResponse =
                JsonSerializer.Deserialize<IReadOnlyList<(int entryId, GetEntryOverviewResponseEntry entryOverview)>>(
                    responseBody,
                    responseJsonSerializerOptions);

            var aggregateLength = parsedResponse.Sum(e => e.entryOverview.length);

            Assert.IsTrue(100 * 10_000 + 400 * 10_000 <= aggregateLength);
        }
    }
}

internal record GetEntryOverviewResponseEntry(int length);