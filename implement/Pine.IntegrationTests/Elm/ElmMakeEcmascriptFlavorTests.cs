using AwesomeAssertions;
using Pine.Core.Testing;
using Pine.Elm019;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm;

public class ElmMakeEcmascriptFlavorTests
{
    private static string CorpusDirectory => TestResultSummary.FindTestDataDirectory("ElmMakeEcmascript");

    [Fact]
    public async Task ElmMakeEcmascript_debug_expectations_include_real_debugger_runtime_and_message_metadata()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

        var expectations = scenario.Expectations.Where(item => item.Flavor == ElmMakeFlavor.Debug).ToArray();
        expectations.Should().NotBeEmpty().And.OnlyContain(item => !item.Skipped);

        foreach (var expectation in expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            output.Should().Contain("Compiled in DEBUG mode").And.Contain("var _Debugger_element = F4(")
                .And.Contain("var _Debugger_document = F4(");

            var offset = output.IndexOf("{\"versions\":", StringComparison.Ordinal);
            offset.Should().BeGreaterThan(0);
            var bytes = Encoding.UTF8.GetBytes(output[offset..]);
            var reader = new Utf8JsonReader(bytes);
            using var metadata = JsonDocument.ParseValue(ref reader);
            metadata.RootElement.GetProperty("versions").GetProperty("elm").GetString().Should().Be("0.19.1");
            var module = Path.GetFileNameWithoutExtension(expectation.SourcePath);
            metadata.RootElement.GetProperty("types").GetProperty("message").GetString().Should().Be(module + ".Msg");

            metadata.RootElement.GetProperty("types").GetProperty("unions").GetProperty(module + ".Msg")
                .GetProperty("tags").EnumerateObject().Should().NotBeEmpty();
        }
    }
}
