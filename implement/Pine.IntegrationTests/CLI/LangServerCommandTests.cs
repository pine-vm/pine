using AwesomeAssertions;
using Pine.CLI;
using System.CommandLine;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public class LangServerCommandTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Language_server_command_collects_cache_statistics_only_on_explicit_opt_in(bool collectStatistics)
    {
        var command = LangServerCommand.Create();
        var option = command.Options.OfType<Option<bool>>().Single(option => option.Name == "--cache-statistics");

        var parsed =
            command.Parse(
                collectStatistics
                ?
                ["--stdio", "--cache-statistics"]
                :
                ["--stdio"]);

        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(option).Should().Be(collectStatistics);
    }
}
