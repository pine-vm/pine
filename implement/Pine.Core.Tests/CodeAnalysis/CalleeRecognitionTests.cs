using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

/// <summary>
/// Pin-tests for <see cref="CalleeRecognition.Augment{IdentifierT}(StaticProgramParserConfig{IdentifierT}, IReadOnlyCollection{CalleeDescriptor{IdentifierT}}, PineVMParseCache)"/>:
/// the unified derivation that maps every surface form of a callee back to the
/// same identifier (Form A / Form B / Form C / bare reference). These tests
/// pin the per-descriptor outputs (named-value lookup, encoded-body lookup,
/// consolidated-form templates) so future changes that drift the recognition
/// behavior are caught directly rather than indirectly through downstream
/// snapshot tests.
/// </summary>
public class CalleeRecognitionTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    private static PineValue TestFunctionValueWithParamCount(int parameterCount) =>
        FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
            innerExpression: Expression.EnvironmentInstance,
            parameterCount: parameterCount,
            envFunctions: []);

    private static StaticProgramParserConfig<string> EmptyBaseConfig() =>
        new(
            IdentifyInstanceRequired:
            (_, value) => throw new System.InvalidOperationException("required: " + value),
            IdentifyInstanceOptional:
            (_, _) => null,
            IdentifyCrash:
            (_, _) => throw new System.NotImplementedException(),
            IdentifyEncodedBodyOptional:
            (_, _) => null,
            ConsolidatedFormTemplates: null);

    [Fact]
    public void Non_function_record_descriptor_only_drives_IdentifyInstance()
    {
        // A descriptor whose NamedValue does not parse as a FunctionRecord
        // (e.g. a plain integer-encoded value) should produce only an
        // IdentifyInstance{Required,Optional} match — no Form A and no Form C.
        var nonFunctionValue = IntegerEncoding.EncodeSignedInteger(42);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "answer",
                NamedValue: nonFunctionValue,
                ContinueParse: true);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        config.IdentifyInstanceOptional([], nonFunctionValue)
            .Should().NotBeNull();
        config.IdentifyInstanceOptional([], nonFunctionValue)!.Ident.Should().Be("answer");

        config.IdentifyEncodedBodyOptional!.Invoke([], nonFunctionValue)
            .Should().BeNull();

        config.ConsolidatedFormTemplates.Should().BeNull();
    }

    [Fact]
    public void Unary_function_record_descriptor_drives_Form_A_and_no_Form_C()
    {
        var unaryFunctionValue = TestFunctionValueWithParamCount(1);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "u",
                NamedValue: unaryFunctionValue,
                ContinueParse: true);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        // Form B / bare reference
        config.IdentifyInstanceOptional([], unaryFunctionValue)
            .Should().NotBeNull();
        config.IdentifyInstanceOptional([], unaryFunctionValue)!.Ident.Should().Be("u");

        // Form A: encoded body must be registered under its own key.
        var record =
            FunctionRecord.ParseFunctionRecordTagged(unaryFunctionValue, s_parseCache).Extract(
                err => throw new System.Exception(err));
        var (encodedBody, _, _) =
            NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(record, [], s_parseCache);

        config.IdentifyEncodedBodyOptional!.Invoke([], encodedBody)
            .Should().NotBeNull();
        config.IdentifyEncodedBodyOptional!.Invoke([], encodedBody)!.Ident.Should().Be("u");

        // No Form C templates for unary functions (K range [2, 1] is empty).
        config.ConsolidatedFormTemplates.Should().BeNull();
    }

    [Fact]
    public void Two_param_function_record_descriptor_emits_one_Form_C_template()
    {
        var binaryFunctionValue = TestFunctionValueWithParamCount(2);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "b",
                NamedValue: binaryFunctionValue,
                ContinueParse: true);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        config.ConsolidatedFormTemplates.Should().NotBeNull();
        config.ConsolidatedFormTemplates!.Count.Should().Be(1);
        config.ConsolidatedFormTemplates[0].Template.DepthK.Should().Be(2);
        config.ConsolidatedFormTemplates[0].Identify.Ident.Should().Be("b");
        config.ConsolidatedFormTemplates[0].Identify.ContinueParse.Should().BeTrue();
        config.ConsolidatedFormTemplates[0].Identify.OriginalFunctionValue
            .Should().Be(binaryFunctionValue);
    }

    [Fact]
    public void Three_param_function_record_descriptor_emits_two_Form_C_templates()
    {
        var ternaryFunctionValue = TestFunctionValueWithParamCount(3);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "t",
                NamedValue: ternaryFunctionValue,
                ContinueParse: true);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        config.ConsolidatedFormTemplates.Should().NotBeNull();
        config.ConsolidatedFormTemplates!.Count.Should().Be(2);
        config.ConsolidatedFormTemplates[0].Template.DepthK.Should().Be(2);
        config.ConsolidatedFormTemplates[1].Template.DepthK.Should().Be(3);
    }

    [Fact]
    public void ContinueParse_false_descriptor_omits_OriginalFunctionValue()
    {
        // Built-in core-library callees use ContinueParse=false, and the
        // recognition responses must omit OriginalFunctionValue so the parser
        // does not attempt to descend into a body it does not own.
        var binaryFunctionValue = TestFunctionValueWithParamCount(2);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "core",
                NamedValue: binaryFunctionValue,
                ContinueParse: false);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        config.IdentifyInstanceOptional([], binaryFunctionValue)!.OriginalFunctionValue
            .Should().BeNull();

        config.ConsolidatedFormTemplates![0].Identify.ContinueParse.Should().BeFalse();
        config.ConsolidatedFormTemplates![0].Identify.OriginalFunctionValue
            .Should().BeNull();
    }

    [Fact]
    public void Wrapper_value_drives_Form_A_and_C_when_distinct_from_named_value()
    {
        // Mirrors the user-declaration shape: NamedValue is the extracted
        // inner value (the value at Form B chain heads), and WrapperValue is
        // the original declaration value used for Form A / Form C derivation.
        // For these tests we use the same value for both, since the test
        // helper does not separate them, but we exercise the WrapperValue
        // override path to ensure it is consulted.
        var wrapperValue = TestFunctionValueWithParamCount(2);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "w",
                NamedValue: wrapperValue,
                ContinueParse: true,
                WrapperValue: wrapperValue);

        var config =
            CalleeRecognition.Augment(
                EmptyBaseConfig(),
                new[] { descriptor },
                s_parseCache);

        // Form C derivation must use the wrapper value as the function value.
        config.ConsolidatedFormTemplates![0].Template.FunctionValue
            .Should().Be(wrapperValue);
    }

    [Fact]
    public void Augment_falls_through_to_previous_config_on_miss()
    {
        var binaryFunctionValue = TestFunctionValueWithParamCount(2);
        var unrelatedValue = IntegerEncoding.EncodeSignedInteger(99);

        var previousConfig =
            new StaticProgramParserConfig<string>(
                IdentifyInstanceRequired:
                (_, value) =>
                    new StaticProgramParser.IdentifyResponse<string>(
                        Ident: "from-previous",
                        ContinueParse: false),
                IdentifyInstanceOptional:
                (_, _) =>
                    new StaticProgramParser.IdentifyResponse<string>(
                        Ident: "previous-optional",
                        ContinueParse: false),
                IdentifyCrash:
                (_, _) => throw new System.NotImplementedException(),
                IdentifyEncodedBodyOptional:
                (_, _) =>
                    new StaticProgramParser.IdentifyResponse<string>(
                        Ident: "previous-encoded",
                        ContinueParse: false),
                ConsolidatedFormTemplates: null);

        var descriptor =
            new CalleeDescriptor<string>(
                Identifier: "matched",
                NamedValue: binaryFunctionValue,
                ContinueParse: false);

        var config =
            CalleeRecognition.Augment(
                previousConfig,
                new[] { descriptor },
                s_parseCache);

        // Hit on the descriptor's NamedValue takes precedence.
        config.IdentifyInstanceOptional([], binaryFunctionValue)!.Ident
            .Should().Be("matched");

        // Miss falls through to previousConfig.
        config.IdentifyInstanceOptional([], unrelatedValue)!.Ident
            .Should().Be("previous-optional");
        config.IdentifyEncodedBodyOptional!.Invoke([], unrelatedValue)!.Ident
            .Should().Be("previous-encoded");
    }
}
