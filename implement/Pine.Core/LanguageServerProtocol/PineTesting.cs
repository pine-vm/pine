using Pine.Core.Elm.Testing;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>Client opt-in to the versioned Pine testing protocol, independently of LSP and executable versions.</summary>
public sealed record PineTestsClientCapabilities(int ProtocolVersion);

/// <summary>Experimental client capabilities understood by Pine.</summary>
[JsonConverter(typeof(PineClientExperimentalCapabilitiesConverter))]
public sealed record PineClientExperimentalCapabilities(PineTestsClientCapabilities? PineTests = null);

/// <summary>Ignores unrelated LSPAny experimental values without rejecting otherwise valid initialization.</summary>
public sealed class PineClientExperimentalCapabilitiesConverter : JsonConverter<PineClientExperimentalCapabilities>
{
    /// <inheritdoc/>
    public override PineClientExperimentalCapabilities Read(
        ref Utf8JsonReader reader,
        System.Type typeToConvert,
        JsonSerializerOptions options)
    {
        using var document = JsonDocument.ParseValue(ref reader);
        var root = document.RootElement;

        if (root.ValueKind is JsonValueKind.Object &&
            root.TryGetProperty("pineTests", out var testing) && testing.ValueKind is JsonValueKind.Object &&
            testing.TryGetProperty("protocolVersion", out var version) && version.ValueKind is JsonValueKind.Number &&
            version.TryGetInt32(out var number))
        {
            return new(new(number));
        }

        return new();
    }

    /// <inheritdoc/>
    public override void Write(
        Utf8JsonWriter writer,
        PineClientExperimentalCapabilities value,
        JsonSerializerOptions options)
    {
        writer.WriteStartObject();

        if (value.PineTests is { } testing)
        {
            writer.WritePropertyName("pineTests");
            writer.WriteStartObject();
            writer.WriteNumber("protocolVersion", testing.ProtocolVersion);
            writer.WriteEndObject();
        }

        writer.WriteEndObject();
    }
}

/// <summary>Experimental server capabilities understood by Pine clients.</summary>
public sealed record PineServerExperimentalCapabilities(PineTestsServerCapabilities? PineTests = null);

/// <summary>Features available in version 1 of the testing protocol.</summary>
public sealed record PineTestsServerCapabilities(
    int ProtocolVersion,
    IReadOnlyList<string> VersionKinds,
    bool ChangeNotifications,
    bool SourceText,
    bool FuzzReplay,
    int RetainedVersions);

/// <summary>A file in an explicitly supplied immutable input tree. Paths contain individual, relative segments.</summary>
public sealed record PineTestSourceFile(IReadOnlyList<string> Path, string ContentBase64);

/// <summary>
/// Discover a workspace snapshot, an existing immutable version, or a verified supplied file tree.
/// ProjectUri is a directory URI. SourceRootUri, when supplied, must contain that project.
/// </summary>
public sealed record PineTestsDiscoverParams(
    string ProjectUri,
    string? Version = null,
    string? SourceRootUri = null,
    IReadOnlyList<PineTestSourceFile>? Files = null);

/// <summary>One complete inventory entry. Location is the construction site, not an assertion stack frame.</summary>
public sealed record PineTestItem(
    string Id,
    string? ParentId,
    string Label,
    string FilePath,
    string ModuleName,
    string DeclarationName,
    IReadOnlyList<string> DescriptionPath,
    ElmTestDiscoveryItemKind Kind,
    ElmTestDiscoverySelection Selection,
    bool IsOnly,
    bool IsEmptyGroup,
    uint? FuzzRuns,
    string SourcePath,
    Location Location);

/// <summary>A failure in discovery or run preparation; never a successful empty inventory.</summary>
public sealed record PineTestingError(string Code, string Message);

/// <summary>Complete replacement for exactly one project and immutable version, or an explicit failure.</summary>
public sealed record PineTestsDiscoverResult(
    string ProjectUri,
    string Version,
    string SourceRootUri,
    string TreeHash,
    bool Complete,
    IReadOnlyList<PineTestItem>? Items,
    bool HasOnly,
    bool HasSkipped,
    IReadOnlyDictionary<string, int> DocumentVersions,
    string? ResolutionFingerprint,
    PineTestingError? Error = null,
    string? DiscoveryId = null);

/// <summary>Include/exclude IDs are scoped to Version. Null include means all; an empty include means none.</summary>
public sealed record PineTestsRunParams(
    string ProjectUri,
    string Version,
    string RunId,
    IReadOnlyList<string>? Include = null,
    IReadOnlyList<string>? Exclude = null,
    uint? Seed = null,
    uint FuzzRuns = 100,
    string? DiscoveryId = null);

/// <summary>Structured expectation information, with optional comparison values.</summary>
public sealed record PineTestMessage(
    string Message,
    string? Expected = null,
    string? Actual = null,
    Location? Location = null);

/// <summary>Terminal state for an individual test, also retained in the run response.</summary>
public sealed record PineTestResult(
    string TestId,
    string State,
    double DurationMilliseconds,
    IReadOnlyList<PineTestMessage> Messages,
    ElmFuzzResult? Fuzz = null,
    string? ReproductionCommand = null);

/// <summary>Ordered progress for one immutable run. Result is present on terminal test events.</summary>
public sealed record PineTestsEvent(
    string ProjectUri,
    string Version,
    string RunId,
    long Sequence,
    string Kind,
    string? TestId = null,
    string? State = null,
    PineTestResult? Result = null,
    string? Message = null);

/// <summary>Terminal response, including results so clients can reconcile missed progress events.</summary>
public sealed record PineTestsRunResult(
    string ProjectUri,
    string Version,
    string RunId,
    string Status,
    ElmTestExecutionSettings Settings,
    bool Incomplete,
    IReadOnlyList<PineTestResult> Results,
    PineTestingError? Error = null,
    string? ResolutionFingerprint = null,
    string? DiscoveryId = null);

/// <summary>Invalidation only: clients rediscover current workspace inputs rather than applying a partial inventory.</summary>
public sealed record PineTestsChangedParams(long WorkspaceRevision);

/// <summary>Reads exact source text from a retained test version, including dependency construction sites.</summary>
public sealed record PineTestsSourceParams(string ProjectUri, string Version, string Path);

/// <summary>Source text correlated with its immutable version.</summary>
public sealed record PineTestsSourceResult(string ProjectUri, string Version, string Path, string Text);
