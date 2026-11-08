using AwesomeAssertions;
using Pine.Core.Functional;
using Pine.Core.Json;
using System;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;
using Xunit;

namespace Pine.Core.Tests.Json;

public class JsonConverterForChoiceTypeTests
{
    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record InheritedChoice
    {
        public record BaseVariant(int Value) : InheritedChoice;

        public sealed record RegisteredDerived(int Value) : BaseVariant(Value);
    }

    public sealed record UnregisteredDerived(int Value) : InheritedChoice.BaseVariant(Value);

    public sealed record UnregisteredChoice : InheritedChoice;

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Runtime_variant_lookup_preserves_first_assignable_variant_order(bool registered)
    {
        InheritedChoice value =
            registered ? new InheritedChoice.RegisteredDerived(7) : new UnregisteredDerived(7);

        var variants =
            JsonConverterForChoiceType.CachedParseTypeToConvert(typeof(InheritedChoice))
            .Extract(error => throw new Exception(error)).Variants;

        var expected = variants.First(entry => entry.Value.ClrType.IsInstanceOfType(value)).Value;
        JsonSerializer.Serialize(value).Should().Be("{\"" + expected.Name + "\":[7]}");
    }

    [Fact]
    public void Unknown_runtime_variant_still_reports_an_explicit_serialization_error()
    {
        Action serialize = () => JsonSerializer.Serialize<InheritedChoice>(new UnregisteredChoice());
        serialize.Should().Throw<JsonException>().WithMessage("*not registered as variant*");
        JsonSerializer.Serialize<InheritedChoice?>(null).Should().Be("null");
    }

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record SimpleClass
    {
        public record Alfa : SimpleClass;

        public record Beta : SimpleClass;
    }

    [Fact]
    public void JSON_coding_union_type_simple_class()
    {
        JsonSerializer.Deserialize<SimpleClass>($$"""{ "Alfa" : [] }""")
            .Should().Be(new SimpleClass.Alfa());

        JsonSerializer.Deserialize<SimpleClass>($$"""{ "Beta" : [] }""")
            .Should().Be(new SimpleClass.Beta());

        JsonSerializer.Serialize<SimpleClass>(new SimpleClass.Alfa())
            .Should().Be($$"""{"Alfa":[]}""");

        JsonSerializer.Serialize<SimpleClass>(new SimpleClass.Beta())
            .Should().Be($$"""{"Beta":[]}""");
    }

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record MixedClass<T0>
    {
        public record VariantWithoutArgs : MixedClass<T0>;

        public record VariantWithTwoArgs(int First, string Second) : MixedClass<T0>;

        public record VariantWithThreeArgs(int Mela, T0 Arancia, string Limone) : MixedClass<T0>;

        public record SimplestRecursion(MixedClass<T0> Value) : MixedClass<T0>;

        public record VariantWithRecursion(
            int Number,
            MixedClass<T0> RecursingWithSameArg,
            MixedClass<int> RecursingWithInt)
            : MixedClass<T0>;
    }

    [Fact]
    public void JSON_coding_union_type_mixed_class()
    {
        JsonSerializer.Deserialize<MixedClass<object>>($$"""{ "VariantWithoutArgs" : [] }""")
            .Should().Be(new MixedClass<object>.VariantWithoutArgs());

        JsonSerializer.Deserialize<MixedClass<object>>($$"""{ "VariantWithTwoArgs" : [ 67, "hello world" ] }""")
            .Should().Be(new MixedClass<object>.VariantWithTwoArgs(67, "hello world"));

        JsonSerializer.Deserialize<MixedClass<long>>(
            $$"""
            { "VariantWithRecursion" :
            [ 123, { "VariantWithThreeArgs" : [ -1, 876036854775808, "another text" ] }, { "VariantWithTwoArgs" : [ 78, "a text" ] } ]
            }
            """)
            .Should().Be(
            new MixedClass<long>.VariantWithRecursion(
                123,
                new MixedClass<long>.VariantWithThreeArgs(-1, 876036854775808, "another text"),
                new MixedClass<int>.VariantWithTwoArgs(78, "a text")));

        JsonSerializer.Serialize<MixedClass<object>>(new MixedClass<object>.VariantWithoutArgs())
            .Should().Be($$"""{"VariantWithoutArgs":[]}""");

        JsonSerializer.Serialize<MixedClass<object>>(new MixedClass<object>.VariantWithTwoArgs(4, "a string"))
            .Should().Be($$"""{"VariantWithTwoArgs":[4,"a string"]}""");

        JsonSerializer.Serialize<MixedClass<long>>(
            new MixedClass<long>.VariantWithRecursion(
                345,
                new MixedClass<long>.VariantWithThreeArgs(-987, 376036858765801, "another text"),
                new MixedClass<int>.VariantWithTwoArgs(56, "a text")))
            .Should().Be($$"""{"VariantWithRecursion":[345,{"VariantWithThreeArgs":[-987,376036858765801,"another text"]},{"VariantWithTwoArgs":[56,"a text"]}]}""");

        JsonSerializer.Serialize<MixedClass<long>>(
            new MixedClass<long>.SimplestRecursion(new MixedClass<long>.VariantWithoutArgs()))
            .Should().Be($$"""{"SimplestRecursion":[{"VariantWithoutArgs":[]}]}""");
    }


    [JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record WithResults<T0>
    {
        public record DiverseResults(
            Result<string, int> Independent,
            Result<string, T0> Dependent,
            Result<string, Result<string, long>> Nested) : WithResults<T0>;
    }

    [Fact]
    public void JSON_coding_union_type_with_results()
    {
        JsonSerializer.Deserialize<WithResults<long>>(
            $$"""{ "DiverseResults" : [ { "Err" : [ "error" ] }, { "Ok" : [ 123456789123456789 ] }, { "Ok" : [ { "Err" : [ "nested error" ] } ] } ] }""")
            .Should().Be(
            new WithResults<long>.DiverseResults(
                Result<string, int>.err("error"),
                Result<string, long>.ok(123456789123456789),
                Result<string, Result<string, long>>.ok(Result<string, long>.err("nested error"))));

        JsonSerializer.Serialize<WithResults<long>>(
            new WithResults<long>.DiverseResults(
                Result<string, int>.err("error"),
                Result<string, long>.ok(345678912345678912),
                Result<string, Result<string, long>>.ok(Result<string, long>.err("nested error"))))
            .Should().Be($$"""{"DiverseResults":[{"Err":["error"]},{"Ok":[345678912345678912]},{"Ok":[{"Err":["nested error"]}]}]}""");
    }

    [Fact]
    public void JSON_serialize_record_choice_type_variant_with_ignored_property()
    {
        JsonSerializer.Serialize(
            new DemoChoice.DemoVariant(
                StringProperty: "stringValue",
                IntProperty: 123))
            .Should().Be($$"""{"StringProperty":"stringValue"}""");

        JsonSerializer.Serialize<DemoChoice>(
            new DemoChoice.DemoVariant(
                StringProperty: "stringValue",
                IntProperty: 123))
            .Should().Be($$"""{"DemoVariant":["stringValue"]}""");
    }

    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record DemoChoice
    {
        public record DemoVariant(
            string StringProperty,

            [property: JsonIgnore]
            int IntProperty)
            : DemoChoice;
    }
}
