using FluentAssertions;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;
using System.Security.Cryptography;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace TestElmTime;

public class DatabaseScaleTests
{
    [Fact]
    public async Task Test_database_scale()
    {
        var webAppSource =
            ExampleAppsTests.ExampleAppValueFromExampleName("test-database-scale");

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        async Task<HttpResponseMessage> apiPostEntryAsync(string entryId, ReadOnlyMemory<byte> entryContent)
        {
            return await publicAppClient.PostAsync("/api/entry/" + entryId, new ReadOnlyMemoryContent(entryContent));
        }

        async Task<HttpResponseMessage> apiGetEntryAsync(string entryId)
        {
            return await publicAppClient.GetAsync("/api/entry/" + entryId);
        }

        var lastEntryIndex = 0;

        async Task postEntriesBatchAsync(
            int entriesCount,
            int entrySizeMin,
            int entrySizeRandom)
        {
            var entriesToPost =
                Enumerable
                .Range(0, entriesCount).Select(entryIndexInBatch => new
                {
                    entryId = lastEntryIndex + entryIndexInBatch.ToString(),
                    entryContent =
                    RandomNumberGenerator.GetBytes(
                        entrySizeMin + RandomNumberGenerator.GetInt32(0, entrySizeRandom))
                })
                .ToImmutableList();

            foreach (var item in entriesToPost)
            {
                await apiPostEntryAsync(item.entryId, item.entryContent);
            }

            foreach (var item in entriesToPost)
            {
                var getResponse = await apiGetEntryAsync(item.entryId);

                ((int)getResponse.StatusCode).Should().Be(200,
                    "Check status code of entry " + item.entryId);

                var responseContentAsByteArray =
                    await getResponse.Content.ReadAsByteArrayAsync();

                responseContentAsByteArray.Should().Equal(
                    item.entryContent,
                    "Check content of entry " + item.entryId);
            }

            lastEntryIndex += entriesToPost.Count;
        }

        await postEntriesBatchAsync(
            entriesCount: 100,
            entrySizeMin: 10_000,
            entrySizeRandom: 10);

        await postEntriesBatchAsync(
            entriesCount: 400,
            entrySizeMin: 10_000,
            entrySizeRandom: 10);

        var responseJsonSerializerOptions = new JsonSerializerOptions();

        responseJsonSerializerOptions.Converters.Add(new TupleAsJsonArray.TupleConverterFactory());

        for (int i = 0; i < 100; i++)
        {
            var getResponse = await publicAppClient.GetAsync("/api/entry");

            var responseBody = await getResponse.Content.ReadAsStringAsync();

            var parsedResponse =
                JsonSerializer.Deserialize<IReadOnlyList<(int entryId, GetEntryOverviewResponseEntry entryOverview)>>(
                    responseBody,
                    responseJsonSerializerOptions)
                ??
                throw new Exception("parsedResponse is null");

            var aggregateLength = parsedResponse.Sum(e => e.entryOverview.length);

            aggregateLength.Should().BeGreaterThanOrEqualTo(100 * 10_000 + 400 * 10_000);
        }
    }
}

internal record GetEntryOverviewResponseEntry(int length);