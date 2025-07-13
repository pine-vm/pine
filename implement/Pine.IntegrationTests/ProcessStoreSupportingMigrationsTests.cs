using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using FluentAssertions;
using System.Collections.Immutable;
using Xunit;

namespace Pine.IntegrationTests;

public class ProcessStoreSupportingMigrationsTests
{
    [Fact]
    public void Test_ProjectFileStoreReaderForAppendedCompositionLogEvent()
    {
        var compositionLogEvent = new CompositionLogRecordInFile.CompositionEvent
        {
            DeployAppConfigAndMigrateElmAppState =
            new ValueInFileStructure { LiteralStringUtf8 = "Actually not a valid value for an app config" }
        };

        var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
            originalFileStore: new EmptyFileStoreReader(),
            compositionLogEvent: compositionLogEvent);

        var processStoreReader = new ProcessStoreReaderInFileStore(projectionResult.ProjectedReader);

        var compositionLogRecords =
            processStoreReader.EnumerateSerializedCompositionLogRecordsReverse().ToImmutableList();

        compositionLogRecords.Should().NotBeEmpty();
    }
}
