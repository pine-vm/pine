using AwesomeAssertions;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class PineCSharpScenarioTests
{
    [Fact]
    public void Pine_CSharp_scenario_references_expand_to_canonical_expressions_and_values()
    {
        var scenario = PineCSharpScenarios.Load("spread-call");

        scenario.Values["functions"].Should().Be(
            PineValue.List(
                [ExpressionEncoding.EncodeExpressionAsValue(scenario.Expressions["producer"])]));

        scenario.Expressions["call"].Should().BeOfType<Expression.Eval>();
        var root = scenario.Entry.Should().BeOfType<Expression.Builtin>().Subject;
        var items = root.Input.Should().BeOfType<Expression.List>().Subject.Items;
        items[0].Should().Be(ExpressionBuilder.BuildExpressionForPathInExpression([0], scenario.Expressions["call"]));
        items[1].Should().Be(ExpressionBuilder.BuildExpressionForPathInExpression([1], scenario.Expressions["call"]));

        var specialized = PineCSharpScenarios.Load("dynamic-root");

        specialized.Variant("specialized-only").Specializations.Single().TryGetValue([0])
            .Should().Be(ExpressionEncoding.EncodeExpressionAsValue(specialized.Expressions["target"]));
    }

    [Theory]
    [InlineData("""{"ExpressionRef":"missing"}""", "Undefined expression reference: missing")]
    [InlineData("""{"ExpressionRef":"main"}""", "Cyclic fixture reference: expression:main")]
    [InlineData("""{"Literal":[{"ValueRef":"missing"}]}""", "Undefined value reference: missing")]
    [InlineData("""{"Literal":[{"EncodedExpression":"main"}]}""", "Cyclic fixture reference: expression:main")]
    [InlineData("""{"EnvPath":[-1]}""", "Expected an integer at least 0")]
    [InlineData("""{"EnvPath":[1.25]}""", "Expected an integer at least 0")]
    [InlineData("""{"Environment":[1]}""", "Expected 0 arguments")]
    [InlineData("""{"Literal":[1,2]}""", "Expected 1 arguments")]
    [InlineData("""{"Unsupported":[]}""", "Unknown expression variant")]
    [InlineData("""{"Environment":[],"Literal":[1]}""", "Expected exactly one variant property")]
    [InlineData("""{"Literal":[{"AsBase64":"?"}]}""", "Invalid base64 blob")]
    [InlineData("""{"Builtin":["not-a-builtin",{"Environment":[]}]}""", "Unknown Pine builtin function")]
    public void Pine_CSharp_scenario_malformed_expressions_report_file_and_location(string expression, string error)
    {
        using var scenario = new TemporaryScenario();

        scenario.Write(
            "program.json",
            """{"entry":"main","expressions":{"main":""" + expression + """},"values":{}}""");

        Action load = () => PineCSharpScenarios.LoadDirectory(scenario.Path);
        load.Should().Throw<InvalidDataException>().WithMessage("*program.json:$.expressions.main*" + error + "*");
    }

    [Fact]
    public void Pine_CSharp_scenario_cross_kind_reference_cycles_and_unused_invalid_definitions_are_rejected()
    {
        using var scenario = new TemporaryScenario();

        scenario.Write(
            "program.json",
            """{"entry":"main","expressions":{"main":{"Literal":[{"ValueRef":"code"}]}},"values":{"code":{"EncodedExpression":"main"}}}""");

        Action load = () => PineCSharpScenarios.LoadDirectory(scenario.Path);
        load.Should().Throw<InvalidDataException>().WithMessage("*program.json:$.values.code*Cyclic fixture reference*");

        scenario.Write(
            "program.json",
            """{"entry":"main","expressions":{"main":{"Environment":[]},"unused":{"ExpressionRef":"missing"}},"values":{}}""");

        load.Should().Throw<InvalidDataException>().WithMessage(
            "*program.json:$.expressions.unused*Undefined expression*");
    }

    [Theory]
    [InlineData("""{"case":{"enableCancelation":false}}""", "Unknown property")]
    [InlineData("""{"case":{"enableCancellation":"false"}}""", "Expected a JSON boolean")]
    [InlineData("""{"case":{"maximumFunctionCount":0}}""", "Expected an integer at least 1")]
    [InlineData("""{"case":{"maximumInlineMatchDifferingPositions":-1}}""", "Expected an integer at least 0")]
    [InlineData("""{"case":{"includeGenericEntry":false}}""", "must request a generic entry")]
    [InlineData("""{"case":{},"case":{}}""", "Duplicate property")]
    [InlineData("""{}""", "Expected at least one compilation variant")]
    [InlineData("""{"..":{}}""", "Expected a safe directory name")]
    [InlineData("""{"case":{"specializations":[[{"path":[0],"value":1},{"path":[0],"value":2}]]}}""", "Duplicate constraint path")]
    public void Pine_CSharp_scenario_malformed_compilation_variants_are_rejected(string compilations, string error)
    {
        using var scenario = new TemporaryScenario();
        scenario.Write("compilations.json", compilations);
        Action load = () => PineCSharpScenarios.LoadDirectory(scenario.Path);
        load.Should().Throw<InvalidDataException>().WithMessage("*compilations.json:$*" + error + "*");
    }

    [Fact]
    public void Pine_CSharp_scenario_duplicate_evaluations_and_unknown_expectation_variants_are_rejected()
    {
        using var scenario = new TemporaryScenario();

        scenario.Write(
            "evaluations.json",
            """[{"name":"same","environment":[]},{"name":"same","environment":[]} ]""");

        Action load = () => PineCSharpScenarios.LoadDirectory(scenario.Path);

        load.Should().Throw<InvalidDataException>().WithMessage(
            "*evaluations.json:$[1].name*Duplicate evaluation name*");

        scenario.Write("evaluations.json", "[]");
        Directory.CreateDirectory(System.IO.Path.Combine(scenario.Path, "expect", "unknown"));

        load.Should().Throw<InvalidDataException>().WithMessage(
            "*Expectations for unknown compilation variants: unknown*");
    }

    [Fact]
    public void Pine_CSharp_scenario_value_encodings_preserve_exact_blobs_and_large_integers()
    {
        using var temporary = new TemporaryScenario();

        temporary.Write(
            "program.json",
            """{"entry":"main","expressions":{"main":{"Literal":[{"ValueRef":"empty"}]}},"values":{"empty":{"AsBase64":""},"text":{"BlobAsString":""},"large":1208925819614629174706176}}""");

        temporary.Write(
            "evaluations.json",
            """[{"name":"nested","environment":[{"ValueRef":"empty"},[{"AsBase64":"AAI="}]]}]""");

        var scenario = PineCSharpScenarios.LoadDirectory(temporary.Path);
        scenario.Values["empty"].Should().Be(PineValue.EmptyBlob);
        scenario.Values["text"].Should().Be(PineValue.EmptyBlob);
        scenario.Values["large"].Should().Be(IntegerEncoding.EncodeSignedInteger(System.Numerics.BigInteger.One << 80));

        scenario.Evaluation("nested").Environment.Should().Be(
            PineValue.List([PineValue.EmptyBlob, PineValue.List([PineValue.Blob([0, 2])])]));
    }

    [Fact]
    public void Pine_CSharp_scenario_deep_named_expressions_and_empty_labels_use_existing_JSON_semantics()
    {
        using var temporary = new TemporaryScenario();
        var expression = """{"Environment":[]}""";

        for (var index = 0; index < 40; index++)
            expression = """{"Label":[[], """ + expression + "]}";

        temporary.Write(
            "program.json",
            """{"entry":"main","expressions":{"main":{"StringTag":["",{"ExpressionRef":"deep"}]},"deep":""" +
            expression +
            """},"values":{}}""");

        var scenario = PineCSharpScenarios.LoadDirectory(temporary.Path);
        var label = scenario.Entry.Should().BeOfType<Expression.Label>().Subject;
        label.LabelValue.Should().Be(PineValue.EmptyBlob);
        label.Tagged.Should().Be(scenario.Expressions["deep"]);
    }

    [Fact]
    public void Pine_CSharp_scenario_snapshot_mismatches_write_all_actual_files_and_preserve_expectations()
    {
        using var scenario = new TemporaryScenario();
        var directory = System.IO.Path.Combine(scenario.Path, "expect", "test");
        Directory.CreateDirectory(directory);

        var outputs =
            new Dictionary<string, string>
            {
                ["methods.csharp-snapshot"] = "new methods",
                ["metadata.json"] = "new metadata",
            };

        foreach (var name in outputs.Keys)
            File.WriteAllText(System.IO.Path.Combine(directory, name), "expected");

        File.WriteAllText(System.IO.Path.Combine(directory, "leftover.json"), "{}");
        var mismatches = PineCSharpScenarios.CompareSnapshots(directory, outputs, allowRecording: false);
        mismatches.Should().HaveCount(3);

        foreach (var (name, actual) in outputs)
        {
            File.ReadAllText(System.IO.Path.Combine(directory, name) + ".actual").Should().Be(actual);
            File.ReadAllText(System.IO.Path.Combine(directory, name)).Should().Be("expected");
        }
    }

    [Fact]
    public void Pine_CSharp_scenario_snapshot_comparison_normalizes_newlines_and_removes_stale_actuals()
    {
        using var scenario = new TemporaryScenario();
        var directory = System.IO.Path.Combine(scenario.Path, "expect", "test");
        Directory.CreateDirectory(directory);
        var path = System.IO.Path.Combine(directory, "methods.csharp-snapshot");
        File.WriteAllText(path, "first\r\nsecond\r\n");
        File.WriteAllText(path + ".actual", "stale");

        PineCSharpScenarios.CompareSnapshots(
            directory,
            new Dictionary<string, string> { ["methods.csharp-snapshot"] = "first\nsecond\n" },
            allowRecording: false).Should().BeEmpty();

        File.Exists(path + ".actual").Should().BeFalse();
    }

    private sealed class TemporaryScenario : IDisposable
    {
        public string Path { get; } =
            System.IO.Path.GetFullPath(
                System.IO.Path.Combine("artifacts", "csharp-scenario-" + Guid.NewGuid().ToString("N")));

        public TemporaryScenario()
        {
            var input = System.IO.Path.Combine(Path, "input");
            Directory.CreateDirectory(input);

            foreach (var file in Directory.GetFiles(
                System.IO.Path.Combine(PineCSharpScenarios.CorpusDirectory, "identity", "input")))
            {
                File.Copy(file, System.IO.Path.Combine(input, System.IO.Path.GetFileName(file)));
            }
        }

        public void Write(string file, string content) =>
            File.WriteAllText(System.IO.Path.Combine(Path, "input", file), content);

        public void Dispose() => Directory.Delete(Path, recursive: true);
    }
}
