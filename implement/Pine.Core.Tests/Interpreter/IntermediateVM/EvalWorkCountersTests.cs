using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System.Linq;
using System.Text.Json;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvalWorkCountersTests
{
    [Fact]
    public void Arithmetic_preserves_every_evaluation_and_cached_work_field()
    {
        var fields =
            typeof(EvalWorkCounters).GetProperties()
            .ToDictionary(property => property.Name, _ => 7L);

        var eval = JsonSerializer.Deserialize<EvalWorkCounters>(JsonSerializer.Serialize(fields));
        var cached = new CachedWorkCounters(3, eval);
        var origins = new EvalWorkCountersByOrigin(eval, cached, eval);
        var doubledEval = EvalWorkCounters.Add(eval, eval);

        foreach (var property in typeof(EvalWorkCounters).GetProperties())
            property.GetValue(doubledEval).Should().Be(14L, property.Name);

        EvalWorkCounters.Subtract(doubledEval, eval).Should().Be(eval);
        EvalWorkCounters.Aggregate([eval, eval]).Should().Be(doubledEval);
        EvalWorkCounters.Aggregate([]).Should().Be(default(EvalWorkCounters));

        var doubledCached = CachedWorkCounters.Add(cached, cached);
        doubledCached.Should().Be(new CachedWorkCounters(6, doubledEval));
        CachedWorkCounters.Subtract(doubledCached, cached).Should().Be(cached);
        CachedWorkCounters.Aggregate([cached, cached]).Should().Be(doubledCached);
        CachedWorkCounters.Aggregate([]).Should().Be(default(CachedWorkCounters));

        origins.Total.Should().Be(doubledEval);

        origins.Enumerate().Select(origin => origin.Origin).Should()
            .Equal("VirtualMachine", "DeferredTemplateValueMaterialization");

        var doubledOrigins = EvalWorkCountersByOrigin.Add(origins, origins);
        doubledOrigins.CachedWorkCounters.Should().Be(doubledCached);
        doubledOrigins.Total.Should().Be(EvalWorkCounters.Add(doubledEval, doubledEval));
        EvalWorkCountersByOrigin.Subtract(doubledOrigins, origins).Should().Be(origins);
    }

    [Fact]
    public void Cached_work_has_a_separate_text_and_json_representation()
    {
        var cached =
            new CachedWorkCounters(
                3,
                new EvalWorkCounters(0, 0, 0, 0, DirectInterpreterExpressionCount: 11));

        var formatted = CachedWorkCountersFormatting.FormatCounts(cached);
        formatted.Should().Contain("ExpressionTemplatePlanParseCount: 3");
        formatted.Should().Contain("ExpressionTemplatePlanParsing:");
        formatted.Should().Contain("DirectInterpreterExpressionCount: 11");

        EvalWorkCountersFormatting.FormatCounts(default(EvalWorkCounters))
            .Should().NotContain("ExpressionTemplatePlanParseCount");

        var origins = new EvalWorkCountersByOrigin(default, cached, default);

        EvalWorkCountersFormatting.FormatCountsByPhase(
            new System.Collections.Generic.Dictionary<string, EvalWorkCountersByOrigin> { ["execution"] = origins })
            .Should().Contain("CachedWorkCounters:").And.Contain(formatted);

        using var json = JsonDocument.Parse(JsonSerializer.Serialize(origins));
        json.RootElement.GetProperty("Total").GetProperty("DirectInterpreterExpressionCount").GetInt64().Should().Be(0);

        json.RootElement.GetProperty("CachedWorkCounters").GetProperty("ExpressionTemplatePlanParseCount")
            .GetInt64().Should().Be(3);
    }
}
