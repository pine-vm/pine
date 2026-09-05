using AwesomeAssertions;
using Pine.Core.LanguageServerProtocol;
using System.Text.Json;
using System.Text.Json.Serialization;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class LanguageServerProtocolSerializationTests
{
    private static readonly JsonSerializerOptions JsonOptions =
        new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        };

    [Fact]
    public void Code_lens_data_and_command_round_trip()
    {
        var data =
            JsonSerializer.SerializeToElement(
                new
                {
                    uri = "file:///workspace/src/Main.elm",
                    position = new { line = 3, character = 0 },
                    clientVersion = 7,
                    documentGeneration = 9,
                },
                JsonOptions);

        var lens =
            new CodeLens(
                new Range(new Position(2, 0), new Position(2, 0)),
                new Command(
                    "2 references",
                    "pine.client.peekReferences",
                    ["file:///workspace/src/Main.elm", new { line = 3, character = 0 }]),
                data);

        var json = JsonSerializer.Serialize(lens, JsonOptions);
        var roundTripped = JsonSerializer.Deserialize<CodeLens>(json, JsonOptions);

        json.Should().Contain("\"command\":\"pine.client.peekReferences\"");
        json.Should().NotContain("commandIdentifier");
        roundTripped.Should().NotBeNull();
        roundTripped!.Data.Should().NotBeNull();
        roundTripped.Data!.Value.GetProperty("documentGeneration").GetInt64().Should().Be(9);
    }

    [Fact]
    public void Reference_params_deserialize_include_declaration()
    {
        const string json =
            """
            {
              "textDocument": { "uri": "file:///workspace/src/Main.elm" },
              "position": { "line": 3, "character": 0 },
              "context": { "includeDeclaration": true }
            }
            """;

        var referenceParams = JsonSerializer.Deserialize<ReferenceParams>(json, JsonOptions);

        referenceParams.Should().NotBeNull();
        referenceParams!.Context.IncludeDeclaration.Should().BeTrue();
    }
}
