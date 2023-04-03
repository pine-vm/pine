using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Json;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace TestElmTime;

[TestClass]
public class TestJsonConverterForChoiceType
{
    [JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record SimpleClass
    {
        public record Alfa : SimpleClass;

        public record Beta : SimpleClass;
    }

    [TestMethod]
    public void JSON_coding_union_type_simple_class()
    {
        Assert.AreEqual(
            new SimpleClass.Alfa(),
            JsonSerializer.Deserialize<SimpleClass>($$"""{ "Alfa" : [] }"""));

        Assert.AreEqual(
            new SimpleClass.Beta(),
            JsonSerializer.Deserialize<SimpleClass>($$"""{ "Beta" : [] }"""));

        Assert.AreEqual(
            $$"""{"Alfa":[]}""",
            JsonSerializer.Serialize<SimpleClass>(new SimpleClass.Alfa()));

        Assert.AreEqual(
            $$"""{"Beta":[]}""",
            JsonSerializer.Serialize<SimpleClass>(new SimpleClass.Beta()));
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

    [TestMethod]
    public void JSON_coding_union_type_mixed_class()
    {
        Assert.AreEqual(
            new MixedClass<object>.VariantWithoutArgs(),
            JsonSerializer.Deserialize<MixedClass<object>>($$"""{ "VariantWithoutArgs" : [] }"""));

        Assert.AreEqual(
            new MixedClass<object>.VariantWithTwoArgs(67, "hello world"),
            JsonSerializer.Deserialize<MixedClass<object>>($$"""{ "VariantWithTwoArgs" : [ 67, "hello world" ] }"""));

        Assert.AreEqual(
            new MixedClass<long>.VariantWithRecursion(
                123,
                new MixedClass<long>.VariantWithThreeArgs(-1, 876036854775808, "another text"),
                new MixedClass<int>.VariantWithTwoArgs(78, "a text")),
            JsonSerializer.Deserialize<MixedClass<long>>(
                $$"""
                { "VariantWithRecursion" :
                [ 123, { "VariantWithThreeArgs" : [ -1, 876036854775808, "another text" ] }, { "VariantWithTwoArgs" : [ 78, "a text" ] } ]
                }
                """));

        Assert.AreEqual(
            $$"""{"VariantWithoutArgs":[]}""",
            JsonSerializer.Serialize<MixedClass<object>>(new MixedClass<object>.VariantWithoutArgs()));

        Assert.AreEqual(
            $$"""{"VariantWithTwoArgs":[4,"a string"]}""",
            JsonSerializer.Serialize<MixedClass<object>>(new MixedClass<object>.VariantWithTwoArgs(4, "a string")));

        Assert.AreEqual(
            $$"""{"VariantWithRecursion":[345,{"VariantWithThreeArgs":[-987,376036858765801,"another text"]},{"VariantWithTwoArgs":[56,"a text"]}]}""",
            JsonSerializer.Serialize<MixedClass<long>>(
                new MixedClass<long>.VariantWithRecursion(
                345,
                new MixedClass<long>.VariantWithThreeArgs(-987, 376036858765801, "another text"),
                new MixedClass<int>.VariantWithTwoArgs(56, "a text"))));

        Assert.AreEqual(
            $$"""{"SimplestRecursion":[{"VariantWithoutArgs":[]}]}""",
            JsonSerializer.Serialize<MixedClass<long>>(
                new MixedClass<long>.SimplestRecursion(new MixedClass<long>.VariantWithoutArgs())));
    }


    [JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record WithResults<T0>
    {
        public record DiverseResults(
            Result<string, int> Independent,
            Result<string, T0> Dependent,
            Result<string, Result<string, long>> Nested) : WithResults<T0>;
    }

    [TestMethod]
    public void JSON_coding_union_type_with_results()
    {
        Assert.AreEqual(
            new WithResults<long>.DiverseResults(
                Result<string, int>.err("error"),
                Result<string, long>.ok(123456789123456789),
                Result<string, Result<string, long>>.ok(Result<string, long>.err("nested error"))),
            JsonSerializer.Deserialize<WithResults<long>>(
                $$"""{ "DiverseResults" : [ { "Err" : [ "error" ] }, { "Ok" : [ 123456789123456789 ] }, { "Ok" : [ { "Err" : [ "nested error" ] } ] } ] }"""));

        Assert.AreEqual(
            $$"""{"DiverseResults":[{"Err":["error"]},{"Ok":[345678912345678912]},{"Ok":[{"Err":["nested error"]}]}]}""",
            JsonSerializer.Serialize<WithResults<long>>(
                new WithResults<long>.DiverseResults(
                    Result<string, int>.err("error"),
                    Result<string, long>.ok(345678912345678912),
                    Result<string, Result<string, long>>.ok(Result<string, long>.err("nested error")))));
    }

    [TestMethod]
    public void JSON_serialize_record_choice_type_variant_with_ignored_property()
    {
        Assert.AreEqual(
            $$"""{"StringProperty":"stringValue"}""",
            JsonSerializer.Serialize(
                new DemoChoice.DemoVariant(
                    StringProperty: "stringValue",
                    IntProperty: 123)));

        Assert.AreEqual(
            $$"""{"DemoVariant":["stringValue"]}""",
            JsonSerializer.Serialize<DemoChoice>(
                new DemoChoice.DemoVariant(
                    StringProperty: "stringValue",
                    IntProperty: 123)));
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
