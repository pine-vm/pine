using ElmTime.ElmInteractive;
using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using Pine.PineVM;
using System;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ElmValueJsonValueEncodingTests
{
    [TestMethod]
    public void EncodeElmValueAsJson_tests()
    {
        var testCases =
            new[]
            {
                new
                {
                    input =
                    ElmValue.Integer(42),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "IntValue",
                        [
                            ElmValue.Integer(42)
                        ]),

                    expectedJsonString =
                    "42"
                },

                new
                {
                    input =
                    ElmValue.StringInstance("Hello, world!"),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "StringValue",
                        [
                            ElmValue.StringInstance("Hello, world!")
                        ]),

                    expectedJsonString =
                    "\"Hello, world!\""
                },

                new
                {
                    input =
                    ElmValue.ListInstance(
                        [
                            ElmValue.Integer(17),
                            ElmValue.StringInstance("Arancini"),
                            ElmValue.Integer(13)
                        ]),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "ArrayValue",
                        [
                            ElmValue.ListInstance(
                                [
                                    ElmValue.TagInstance(
                                        "IntValue",
                                        [
                                            ElmValue.Integer(17)
                                        ]),

                                    ElmValue.TagInstance(
                                        "StringValue",
                                        [
                                            ElmValue.StringInstance("Arancini")
                                        ]),

                                    ElmValue.TagInstance(
                                        "IntValue",
                                        [
                                            ElmValue.Integer(13)
                                        ])
                                ])
                        ]),

                    expectedJsonString =
                    "[17,\"Arancini\",13]"
                },

                new
                {
                    input =
                    (ElmValue)
                    ElmValue.TagInstance("True", []),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "BoolValue",
                        [
                            ElmValue.TagInstance("True", [])
                        ]),

                    expectedJsonString =
                    "true"
                },

                new
                {
                    input =
                    (ElmValue)
                    ElmValue.TagInstance("False", []),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "BoolValue",
                        [
                            ElmValue.TagInstance("False", [])
                        ]),

                    expectedJsonString =
                    "false"
                },

                new
                {
                    input =
                    (ElmValue)
                    ElmValue.TagInstance("Nothing", []),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "ObjectValue",
                        [
                            ElmValue.ListInstance(
                                [
                                    ElmValue.ListInstance(
                                        [
                                            ElmValue.StringInstance("Nothing"),
                                            ElmValue.TagInstance("ArrayValue",
                                            [ElmValue.ListInstance([])])
                                        ])
                                ])
                        ]),

                        expectedJsonString =
                        "{\"Nothing\":[]}"
                },

                new
                {
                    input =
                    (ElmValue)
                    ElmValue.TagInstance(
                        "Just",
                        [
                            ElmValue.Integer(42)
                        ]),

                        expectedLibJsonValue =
                        ElmValue.TagInstance(
                            "ObjectValue",
                            [
                                ElmValue.ListInstance(
                                    [
                                        ElmValue.ListInstance(
                                            [
                                                ElmValue.StringInstance("Just"),
                                                ElmValue.TagInstance(
                                                    "ArrayValue",
                                                    [
                                                        ElmValue.ListInstance(
                                                            [
                                                                ElmValue.TagInstance(
                                                                    "IntValue",
                                                                    [
                                                                        ElmValue.Integer(42)
                                                                    ])
                                                            ])
                                                    ])
                                            ])
                                    ])
                            ]),

                            expectedJsonString =
                            "{\"Just\":[42]}"
                },

                new
                {
                    input =
                    (ElmValue)
                    new ElmValue.ElmRecord(
                        [
                            ("name", ElmValue.StringInstance("Alice")),
                            ("age", ElmValue.Integer(42))
                        ]),

                        expectedLibJsonValue =
                        ElmValue.TagInstance(
                            "ObjectValue",
                            [
                                ElmValue.ListInstance(
                                    [
                                        ElmValue.ListInstance(
                                            [
                                                ElmValue.StringInstance("name"),
                                                ElmValue.TagInstance(
                                                    "StringValue",
                                                    [
                                                        ElmValue.StringInstance("Alice")
                                                    ])
                                            ]),

                                        ElmValue.ListInstance(
                                        [
                                            ElmValue.StringInstance("age"),
                                            ElmValue.TagInstance(
                                                "IntValue",
                                                [
                                                    ElmValue.Integer(42)
                                                ])
                                        ])
                                    ])
                            ]),

                            expectedJsonString =
                            "{\"name\":\"Alice\",\"age\":42}"
                },

                new
                {
                    input =
                    ElmValue.CharInstance('A'),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "StringValue",
                        [
                            ElmValue.StringInstance("A")
                        ]),

                    expectedJsonString =
                    "\"A\""
                },

                new
                {
                    input =
                    (ElmValue)
                    new ElmValue.ElmBytes(
                        System.Text.Encoding.UTF8.GetBytes("Test")),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "ObjectValue",
                        [
                            ElmValue.ListInstance(
                                [
                                    ElmValue.ListInstance(
                                        [
                                            ElmValue.StringInstance("AsBase64"),
                                            ElmValue.TagInstance(
                                                "StringValue",
                                                [
                                                    ElmValue.StringInstance("VGVzdA==")
                                                ])
                                        ])
                                ])
                        ]),

                        expectedJsonString =
                        "{\"AsBase64\":\"VGVzdA==\"}"
                },

                new
                {
                    input =
                    (ElmValue)
                    ElmValue.ElmFloat.Normalized(
                        Numerator: 1,
                        Denominator: 4),

                    expectedLibJsonValue =
                    ElmValue.TagInstance(
                        "FloatValue",
                        [
                            ElmValue.StringInstance("0.25")
                        ]),

                        expectedJsonString =
                        "0.25"
                },
            };

        var parseCache = new PineVMParseCache();

        var pineVM = new PineVM();

        var bundledElmCompilerValue =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue();

        if (bundledElmCompilerValue is null)
        {
            throw new Exception("Failed to get bundled Elm compiler value.");
        }

        var parseEnvResult =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(bundledElmCompilerValue);

        {
            if (parseEnvResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed to parse Elm compiler environment: " + err);
            }
        }

        if (parseEnvResult.IsOkOrNull() is not { } parseEnvOk)
        {
            throw new Exception(
                "Unexpected null parseEnvResult: " + parseEnvResult);
        }

        var moduleJsonEncode =
            parseEnvOk.Modules
            .FirstOrDefault(module => module.moduleName is "Json.Encode");

        if (moduleJsonEncode.moduleValue is null)
        {
            throw new Exception(
                "Module 'Json.Encode' not found among " +
                parseEnvOk.Modules.Count + " modules: " +
                string.Join(", ", parseEnvOk.Modules.Select(module => module.moduleName)));
        }

        moduleJsonEncode.moduleContent.FunctionDeclarations.TryGetValue(
            "encode",
            out var encodeFunctionValue);

        if (encodeFunctionValue is null)
        {
            throw new Exception("Function 'encode' not found in module 'Json.Encode'");
        }

        var parseEncodeFunctionResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                encodeFunctionValue,
                parseCache);
        {
            if (parseEncodeFunctionResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed to parse 'encode' function: " + err);
            }
        }

        if (parseEncodeFunctionResult.IsOkOrNull() is not { } parseEncodeFunctionOk)
        {
            throw new Exception(
                "Unexpected parseEncodeFunctionResult: " + parseEncodeFunctionResult);
        }


        for (var i = 0; i < testCases.Length; ++i)
        {
            var testCase = testCases[i];

            try
            {
                var actualPineValue =
                    ElmValueJsonValueEncoding.EncodeAsJsonValuePineValue(testCase.input);

                var asElmValue =
                    ElmValueEncoding.PineValueAsElmValue(actualPineValue, null, null)
                    .Extract(err => throw new Exception("Failed decoding back as Elm value: " + err));

                asElmValue.Should().Be(testCase.expectedLibJsonValue);

                var applyResult =
                    ElmInteractiveEnvironment.ApplyFunction(
                        pineVM,
                        parseEncodeFunctionOk,
                        [
                            IntegerEncoding.EncodeSignedInteger(0),
                            actualPineValue
                        ]);

                if (applyResult.IsErrOrNull() is { } err)
                {
                    throw new Exception("Failed applying 'encode' function: " + err);
                }

                if (applyResult.IsOkOrNull() is not { } applyOk)
                {
                    throw new Exception("Unexpected applyResult: " + applyResult);
                }

                var encodeJsonValue =
                    ElmValueEncoding.PineValueAsElmValue(applyOk, null, null)
                    .Extract(err => throw new Exception("Failed decoding as Elm value: " + err));

                if (encodeJsonValue is not ElmValue.ElmString encodeJsonString)
                {
                    throw new Exception("Expected ElmString, got: " + encodeJsonValue);
                }

                encodeJsonString.Value.Should().Be(testCase.expectedJsonString);
            }
            catch (Exception e)
            {
                throw new Exception(
                    $"Test case {i} failed: {e.Message} (" + testCase.input.ToString() + ")", e);
            }
        }
    }
}
