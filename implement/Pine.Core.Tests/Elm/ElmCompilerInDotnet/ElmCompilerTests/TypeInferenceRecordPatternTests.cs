using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests that pattern-deconstruction usages of a parameter (record patterns
/// inside <c>let</c> destructuring bindings) drive open-record constraints
/// for that parameter, the same way <see cref="TypeInferenceRecordAccessTests"/>
/// covers <c>r.field</c> record-access usages.
/// </summary>
public class TypeInferenceRecordPatternTests
{
    /// <summary>
    /// Parses a single-function Elm module text and returns the parameter
    /// types inferred from how the function body uses each parameter.
    /// Only parameters bound by a simple <see cref="SyntaxTypes.Pattern.VarPattern"/>
    /// are tracked, mirroring the typical usage of
    /// <see cref="TypeInference.InferParameterTypesFromUsage"/>.
    /// </summary>
    private static System.Collections.Immutable.ImmutableDictionary<string, TypeInference.InferredType>
        InferParameterTypesFromModuleText(string elmModuleText, string functionName)
    {
        var parsedFileFull =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception("Failed to parse module: " + err));

        var parsedFile = SyntaxTypes.FromFullSyntaxModel.Convert(parsedFileFull);

        var function =
            parsedFile.Declarations
            .Select(d => d.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
            .Single(d => d.Function.Declaration.Value.Name.Value == functionName)
            .Function;

        var implementation = function.Declaration.Value;

        var parameterNames = new Dictionary<string, int>();

        for (var i = 0; i < implementation.Arguments.Count; i++)
        {
            if (implementation.Arguments[i].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                parameterNames[varPattern.Name] = i;
            }
        }

        return
            TypeInference.InferParameterTypesFromUsage(
                implementation.Expression.Value,
                parameterNames: parameterNames,
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());
    }

    [Fact]
    public void Multiple_record_pattern_destructurings_of_same_parameter_accumulate_all_required_fields()
    {
        // fn r =
        //     let
        //         { alfa } = r
        //         { beta } = r
        //     in
        //     r
        //
        // Both destructurings target the same parameter `r`. Each record
        // pattern requires exactly one of the fields `alfa` and `beta`.
        // Inference should accumulate both fields into a single
        // open-record constraint on `r`.

        var elmModuleText =
            """"
            module Test exposing (..)


            fn r =
                let
                    { alfa } = r
                    { beta } = r
                in
                r

            """";

        var parameterTypes =
            InferParameterTypesFromModuleText(elmModuleText, functionName: "fn");

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];

        var fieldNames = openRecord.KnownFields.Select(f => f.FieldName).ToHashSet();

        fieldNames.Should().BeEquivalentTo(["alfa", "beta"]);
    }

    [Fact]
    public void Multiple_record_pattern_destructurings_with_one_field_constrained_to_Int()
    {
        // fn r =
        //     let
        //         { alfa } = r
        //         { beta } = r
        //     in
        //     alfa // 17
        //
        // Same scenario as above, but additionally the destructured field
        // `alfa` is used as the operand of integer division `//`, which
        // forces its inferred type to Int. The inferred open-record type
        // for `r` should still contain both fields, with `alfa` further
        // constrained to Int and `beta` left as a free type variable.

        var elmModuleText =
            """"
            module Test exposing (..)


            fn r =
                let
                    { alfa } = r
                    { beta } = r
                in
                alfa // 17

            """";

        var parameterTypes =
            InferParameterTypesFromModuleText(elmModuleText, functionName: "fn");

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];

        var fieldsByName =
            openRecord.KnownFields
            .ToDictionary(f => f.FieldName, f => f.FieldType);

        fieldsByName.Keys.Should().BeEquivalentTo(["alfa", "beta"]);

        fieldsByName["alfa"].Should().BeOfType<TypeInference.InferredType.IntType>();
    }
}
