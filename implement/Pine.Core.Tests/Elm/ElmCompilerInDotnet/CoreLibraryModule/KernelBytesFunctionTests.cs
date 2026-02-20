using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class KernelBytesFunctionTests
{
    /// <summary>
    /// Elm test module that imports Bytes.Encode and Bytes.Decode and provides
    /// roundtrip/decode/encode functions. Decoders stay inside Elm source so
    /// we never have to pass closures through C#.
    /// </summary>
    private const string TestModuleSource =
        """
        module BytesTest exposing (..)

        import Bitwise
        import Bytes
        import Bytes.Decode
        import Bytes.Encode
        import Char
        import String


        -- Roundtrip helpers

        roundtripUnsignedInt8 : Int -> Maybe Int
        roundtripUnsignedInt8 n =
            Bytes.Decode.decode Bytes.Decode.unsignedInt8
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 n))

        roundtripSignedInt8 : Int -> Maybe Int
        roundtripSignedInt8 n =
            Bytes.Decode.decode Bytes.Decode.signedInt8
                (Bytes.Encode.encode (Bytes.Encode.signedInt8 n))

        roundtripUnsignedInt16BE : Int -> Maybe Int
        roundtripUnsignedInt16BE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.BE n))

        roundtripUnsignedInt16LE : Int -> Maybe Int
        roundtripUnsignedInt16LE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.LE n))

        roundtripSignedInt16BE : Int -> Maybe Int
        roundtripSignedInt16BE n =
            Bytes.Decode.decode (Bytes.Decode.signedInt16 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.signedInt16 Bytes.BE n))

        roundtripSignedInt16LE : Int -> Maybe Int
        roundtripSignedInt16LE n =
            Bytes.Decode.decode (Bytes.Decode.signedInt16 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.signedInt16 Bytes.LE n))

        roundtripUnsignedInt32BE : Int -> Maybe Int
        roundtripUnsignedInt32BE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt32 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE n))

        roundtripUnsignedInt32LE : Int -> Maybe Int
        roundtripUnsignedInt32LE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt32 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.LE n))

        roundtripSignedInt32BE : Int -> Maybe Int
        roundtripSignedInt32BE n =
            Bytes.Decode.decode (Bytes.Decode.signedInt32 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.signedInt32 Bytes.BE n))

        roundtripSignedInt32LE : Int -> Maybe Int
        roundtripSignedInt32LE n =
            Bytes.Decode.decode (Bytes.Decode.signedInt32 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.signedInt32 Bytes.LE n))

        roundtripString : String -> Maybe String
        roundtripString s =
            let
                encoded = Bytes.Encode.encode (Bytes.Encode.string s)
                width = Bytes.width encoded
            in
            Bytes.Decode.decode (Bytes.Decode.string width) encoded


        -- Encode width helpers

        encodeWidthU8 : Int -> Int
        encodeWidthU8 n =
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 n))

        encodeWidthS8 : Int -> Int
        encodeWidthS8 n =
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.signedInt8 n))

        encodeWidthU16 : Int -> Int
        encodeWidthU16 n =
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.BE n))

        encodeWidthU32 : Int -> Int
        encodeWidthU32 n =
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE n))


        -- Sequence helpers

        encodeWidthSequenceEmpty : Int -> Int
        encodeWidthSequenceEmpty _ =
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.sequence []))

        encodeWidthSequenceTwoU8 : Int -> Int
        encodeWidthSequenceTwoU8 _ =
            Bytes.width
                (Bytes.Encode.encode
                    (Bytes.Encode.sequence
                        [ Bytes.Encode.unsignedInt8 1
                        , Bytes.Encode.unsignedInt8 2
                        ]
                    )
                )

        encodeWidthSequenceMixed : Int -> Int
        encodeWidthSequenceMixed _ =
            Bytes.width
                (Bytes.Encode.encode
                    (Bytes.Encode.sequence
                        [ Bytes.Encode.unsignedInt8 1
                        , Bytes.Encode.unsignedInt16 Bytes.BE 2
                        ]
                    )
                )


        -- Decode: succeed / fail

        decodeSucceed42 : Int -> Maybe Int
        decodeSucceed42 _ =
            Bytes.Decode.decode (Bytes.Decode.succeed 42)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0))

        decodeFail : Int -> Maybe Int
        decodeFail _ =
            Bytes.Decode.decode Bytes.Decode.fail
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 0))


        -- Decode: not enough bytes

        decodeU16From1Byte : Int -> Maybe Int
        decodeU16From1Byte _ =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 7))

        decodeU32From2Bytes : Int -> Maybe Int
        decodeU32From2Bytes _ =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt32 Bytes.BE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.BE 7))


        -- Endianness cross-test

        encodeU16BEDecodeLE : Int -> Maybe Int
        encodeU16BEDecodeLE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.BE n))

        encodeU32BEDecodeLE : Int -> Maybe Int
        encodeU32BEDecodeLE n =
            Bytes.Decode.decode (Bytes.Decode.unsignedInt32 Bytes.LE)
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE n))


        -- Sequence decode (first byte)

        decodeFirstOfTwoU8 : Int -> Maybe Int
        decodeFirstOfTwoU8 _ =
            Bytes.Decode.decode Bytes.Decode.unsignedInt8
                (Bytes.Encode.encode
                    (Bytes.Encode.sequence
                        [ Bytes.Encode.unsignedInt8 10
                        , Bytes.Encode.unsignedInt8 20
                        ]
                    )
                )


        -- Bytes pass-through

        encodeBytesPassthrough : Int -> Int
        encodeBytesPassthrough _ =
            let
                inner = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
            in
            Bytes.width (Bytes.Encode.encode (Bytes.Encode.bytes inner))


        -- String width

        getStringWidth : String -> Int
        getStringWidth s =
            Bytes.Encode.getStringWidth s


        -- map2 test

        decodeMap2TwoU8 : Int -> Maybe (List Int)
        decodeMap2TwoU8 _ =
            let
                decoder =
                    Bytes.Decode.map2 (\a b -> [ a, b ])
                        Bytes.Decode.unsignedInt8
                        Bytes.Decode.unsignedInt8
            in
            Bytes.Decode.decode decoder
                (Bytes.Encode.encode
                    (Bytes.Encode.sequence
                        [ Bytes.Encode.unsignedInt8 10
                        , Bytes.Encode.unsignedInt8 20
                        ]
                    )
                )


        -- loop decoder test: sum 3 bytes using Bytes.Decode.loop

        loopSumThreeBytes : Int -> Maybe Int
        loopSumThreeBytes _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 10
                            , Bytes.Encode.unsignedInt8 20
                            , Bytes.Encode.unsignedInt8 30
                            ]
                        )

                decoder =
                    Bytes.Decode.loop ( 3, 0 ) loopSumStep
            in
            Bytes.Decode.decode decoder bites


        loopSumStep : ( Int, Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, Int ) Int)
        loopSumStep ( remaining, total ) =
            if remaining <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done total)

            else
                Bytes.Decode.map
                    (\byte -> Bytes.Decode.Loop ( remaining - 1, total + byte ))
                    Bytes.Decode.unsignedInt8


        -- loop decoder with map3: similar to Base64.Decode.loopHelp
        -- Reads groups of 3 bytes and sums them, loop until done

        loopMap3ThreeBytes : Int -> Maybe Int
        loopMap3ThreeBytes _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 10
                            , Bytes.Encode.unsignedInt8 20
                            , Bytes.Encode.unsignedInt8 30
                            ]
                        )

                decoder =
                    Bytes.Decode.loop ( 3, 0 ) loopMap3Step
            in
            Bytes.Decode.decode decoder bites


        loopMap3Step : ( Int, Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, Int ) Int)
        loopMap3Step ( remaining, total ) =
            if remaining >= 3 then
                let
                    helper a b c =
                        Bytes.Decode.Loop ( remaining - 3, total + a + b + c )
                in
                Bytes.Decode.map3 helper
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8

            else
                Bytes.Decode.succeed (Bytes.Decode.Done total)


        -- Minimal test: loop with map3 and string concatenation
        -- This isolates whether ++ in a map3 callback inside a loop works
        loopMap3StringConcat : Int -> Maybe String
        loopMap3StringConcat _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 65
                            , Bytes.Encode.unsignedInt8 66
                            , Bytes.Encode.unsignedInt8 67
                            ]
                        )

                decoder =
                    Bytes.Decode.loop { remaining = 3, result = "" } loopStringStep
            in
            Bytes.Decode.decode decoder bites


        loopStringStep : { remaining : Int, result : String } -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, result : String } String)
        loopStringStep { remaining, result } =
            if remaining >= 3 then
                let
                    helper a b c =
                        Bytes.Decode.Loop { remaining = remaining - 3, result = result ++ "XYZ" }
                in
                Bytes.Decode.map3 helper
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8

            else
                Bytes.Decode.succeed (Bytes.Decode.Done result)


        -- Test: loop with function call inside map3 callback
        dummyConvert : Int -> Int -> Int -> String
        dummyConvert a b c =
            String.fromList
                [ Char.fromCode a
                , Char.fromCode b
                , Char.fromCode c
                ]


        loopMap3FuncCall : Int -> Maybe String
        loopMap3FuncCall _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 65
                            , Bytes.Encode.unsignedInt8 66
                            , Bytes.Encode.unsignedInt8 67
                            ]
                        )

                decoder =
                    Bytes.Decode.loop { remaining = 3, result = "" } loopFuncCallStep
            in
            Bytes.Decode.decode decoder bites


        loopFuncCallStep : { remaining : Int, result : String } -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, result : String } String)
        loopFuncCallStep { remaining, result } =
            if remaining >= 3 then
                let
                    helper a b c =
                        Bytes.Decode.Loop { remaining = remaining - 3, result = result ++ dummyConvert a b c }
                in
                Bytes.Decode.map3 helper
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8

            else
                Bytes.Decode.succeed (Bytes.Decode.Done result)


        -- Test: loop with Bitwise and let-expression inside map3 callback
        loopMap3Bitwise : Int -> Maybe String
        loopMap3Bitwise _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 77
                            , Bytes.Encode.unsignedInt8 97
                            , Bytes.Encode.unsignedInt8 110
                            ]
                        )

                decoder =
                    Bytes.Decode.loop { remaining = 3, result = "" } loopBitwiseStep
            in
            Bytes.Decode.decode decoder bites


        loopBitwiseStep : { remaining : Int, result : String } -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, result : String } String)
        loopBitwiseStep { remaining, result } =
            if remaining >= 3 then
                let
                    helper a b c =
                        let
                            combined =
                                Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)) c
                        in
                        Bytes.Decode.Loop { remaining = remaining - 3, result = result ++ dummyBitsToChars combined }
                in
                Bytes.Decode.map3 helper
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8

            else
                Bytes.Decode.succeed (Bytes.Decode.Done result)


        dummyBitsToChars : Int -> String
        dummyBitsToChars bits =
            String.fromList
                [ Char.fromCode (Bitwise.shiftRightZfBy 16 bits)
                , Char.fromCode (Bitwise.and (Bitwise.shiftRightZfBy 8 bits) 0xFF)
                , Char.fromCode (Bitwise.and bits 0xFF)
                ]
        """;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                // Add our test module to the kernel modules tree
                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["BytesTest.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleSource)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("BytesTest.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "BytesTest")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static ElmValue CallThunk(string name) =>
        ApplyUnary(GetTestFunction(name), Integer(0));

    // ========== Encoding width tests ==========

    [Fact]
    public void Encode_unsignedInt8_width()
    {
        ApplyUnary(GetTestFunction("encodeWidthU8"), Integer(0)).Should().Be(Integer(1));
        ApplyUnary(GetTestFunction("encodeWidthU8"), Integer(255)).Should().Be(Integer(1));
        ApplyUnary(GetTestFunction("encodeWidthU8"), Integer(42)).Should().Be(Integer(1));
    }

    [Fact]
    public void Encode_signedInt8_width()
    {
        ApplyUnary(GetTestFunction("encodeWidthS8"), Integer(0)).Should().Be(Integer(1));
        ApplyUnary(GetTestFunction("encodeWidthS8"), Integer(-1)).Should().Be(Integer(1));
    }

    [Fact]
    public void Encode_unsignedInt16_width()
    {
        ApplyUnary(GetTestFunction("encodeWidthU16"), Integer(7)).Should().Be(Integer(2));
        ApplyUnary(GetTestFunction("encodeWidthU16"), Integer(0)).Should().Be(Integer(2));
    }

    [Fact]
    public void Encode_unsignedInt32_width()
    {
        ApplyUnary(GetTestFunction("encodeWidthU32"), Integer(256)).Should().Be(Integer(4));
        ApplyUnary(GetTestFunction("encodeWidthU32"), Integer(0)).Should().Be(Integer(4));
    }

    [Fact]
    public void Encode_sequence_empty_width()
    {
        CallThunk("encodeWidthSequenceEmpty").Should().Be(Integer(0));
    }

    [Fact]
    public void Encode_sequence_two_u8_width()
    {
        CallThunk("encodeWidthSequenceTwoU8").Should().Be(Integer(2));
    }

    [Fact]
    public void Encode_sequence_mixed_width()
    {
        CallThunk("encodeWidthSequenceMixed").Should().Be(Integer(3));
    }

    // ========== Roundtrip: unsignedInt8 ==========

    [Fact]
    public void Roundtrip_unsignedInt8_0()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt8"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_unsignedInt8_42()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt8"), Integer(42))
            .Should().Be(JustOf(Integer(42)));
    }

    [Fact]
    public void Roundtrip_unsignedInt8_255()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt8"), Integer(255))
            .Should().Be(JustOf(Integer(255)));
    }

    [Fact]
    public void Roundtrip_unsignedInt8_128()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt8"), Integer(128))
            .Should().Be(JustOf(Integer(128)));
    }

    // ========== Roundtrip: signedInt8 ==========

    [Fact]
    public void Roundtrip_signedInt8_0()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt8"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_signedInt8_127()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt8"), Integer(127))
            .Should().Be(JustOf(Integer(127)));
    }

    [Fact]
    public void Roundtrip_signedInt8_negative1()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt8"), Integer(-1))
            .Should().Be(JustOf(Integer(-1)));
    }

    [Fact]
    public void Roundtrip_signedInt8_negative128()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt8"), Integer(-128))
            .Should().Be(JustOf(Integer(-128)));
    }

    // ========== Roundtrip: unsignedInt16 ==========

    [Fact]
    public void Roundtrip_unsignedInt16_BE_7()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt16BE"), Integer(7))
            .Should().Be(JustOf(Integer(7)));
    }

    [Fact]
    public void Roundtrip_unsignedInt16_LE_7()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt16LE"), Integer(7))
            .Should().Be(JustOf(Integer(7)));
    }

    [Fact]
    public void Roundtrip_unsignedInt16_BE_1792()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt16BE"), Integer(1792))
            .Should().Be(JustOf(Integer(1792)));
    }

    [Fact]
    public void Roundtrip_unsignedInt16_BE_0()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt16BE"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_unsignedInt16_BE_65535()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt16BE"), Integer(65535))
            .Should().Be(JustOf(Integer(65535)));
    }

    // ========== Roundtrip: signedInt16 ==========

    [Fact]
    public void Roundtrip_signedInt16_BE_positive()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16BE"), Integer(1000))
            .Should().Be(JustOf(Integer(1000)));
    }

    [Fact]
    public void Roundtrip_signedInt16_BE_negative()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16BE"), Integer(-1000))
            .Should().Be(JustOf(Integer(-1000)));
    }

    [Fact]
    public void Roundtrip_signedInt16_LE_negative()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16LE"), Integer(-1))
            .Should().Be(JustOf(Integer(-1)));
    }

    [Fact]
    public void Roundtrip_signedInt16_BE_zero()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16BE"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_signedInt16_max()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16BE"), Integer(32767))
            .Should().Be(JustOf(Integer(32767)));
    }

    [Fact]
    public void Roundtrip_signedInt16_min()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt16BE"), Integer(-32768))
            .Should().Be(JustOf(Integer(-32768)));
    }

    // ========== Roundtrip: unsignedInt32 ==========

    [Fact]
    public void Roundtrip_unsignedInt32_BE_256()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt32BE"), Integer(256))
            .Should().Be(JustOf(Integer(256)));
    }

    [Fact]
    public void Roundtrip_unsignedInt32_LE_256()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt32LE"), Integer(256))
            .Should().Be(JustOf(Integer(256)));
    }

    [Fact]
    public void Roundtrip_unsignedInt32_BE_0()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt32BE"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_unsignedInt32_BE_large()
    {
        ApplyUnary(GetTestFunction("roundtripUnsignedInt32BE"), Integer(0x01020304))
            .Should().Be(JustOf(Integer(0x01020304)));
    }

    // ========== Roundtrip: signedInt32 ==========

    [Fact]
    public void Roundtrip_signedInt32_BE_positive()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32BE"), Integer(100000))
            .Should().Be(JustOf(Integer(100000)));
    }

    [Fact]
    public void Roundtrip_signedInt32_BE_negative()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32BE"), Integer(-100000))
            .Should().Be(JustOf(Integer(-100000)));
    }

    [Fact]
    public void Roundtrip_signedInt32_LE_negative()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32LE"), Integer(-1))
            .Should().Be(JustOf(Integer(-1)));
    }

    [Fact]
    public void Roundtrip_signedInt32_BE_zero()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32BE"), Integer(0))
            .Should().Be(JustOf(Integer(0)));
    }

    [Fact]
    public void Roundtrip_signedInt32_max()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32BE"), Integer(2147483647))
            .Should().Be(JustOf(Integer(2147483647)));
    }

    [Fact]
    public void Roundtrip_signedInt32_min()
    {
        ApplyUnary(GetTestFunction("roundtripSignedInt32BE"), Integer(-2147483648))
            .Should().Be(JustOf(Integer(-2147483648)));
    }

    // ========== Decode: succeed / fail ==========

    [Fact]
    public void Decode_succeed()
    {
        CallThunk("decodeSucceed42").Should().Be(JustOf(Integer(42)));
    }

    [Fact]
    public void Decode_fail()
    {
        CallThunk("decodeFail").Should().Be(s_nothing);
    }

    // ========== Decode: not enough bytes ==========

    [Fact]
    public void Decode_unsignedInt16_from_1_byte_returns_Nothing()
    {
        CallThunk("decodeU16From1Byte").Should().Be(s_nothing);
    }

    [Fact]
    public void Decode_unsignedInt32_from_2_bytes_returns_Nothing()
    {
        CallThunk("decodeU32From2Bytes").Should().Be(s_nothing);
    }

    // ========== Roundtrip: string ==========

    [Fact]
    public void Roundtrip_string_ascii()
    {
        ApplyUnary(GetTestFunction("roundtripString"), String("hello"))
            .Should().Be(JustOf(String("hello")));
    }

    [Fact]
    public void Roundtrip_string_empty()
    {
        ApplyUnary(GetTestFunction("roundtripString"), String(""))
            .Should().Be(JustOf(String("")));
    }

    // ========== Encoding: bytes pass-through ==========

    [Fact]
    public void Encode_bytes_passthrough()
    {
        CallThunk("encodeBytesPassthrough").Should().Be(Integer(1));
    }

    // ========== Endianness cross-tests ==========

    [Fact]
    public void Encode_unsignedInt16_BE_then_decode_LE()
    {
        // 0x0102 encoded BE = [01, 02], decoded LE = 0x0201 = 513
        ApplyUnary(GetTestFunction("encodeU16BEDecodeLE"), Integer(0x0102))
            .Should().Be(JustOf(Integer(0x0201)));
    }

    [Fact]
    public void Encode_unsignedInt32_BE_then_decode_LE()
    {
        // 0x01020304 encoded BE = [01,02,03,04], decoded LE = 0x04030201
        ApplyUnary(GetTestFunction("encodeU32BEDecodeLE"), Integer(0x01020304))
            .Should().Be(JustOf(Integer(0x04030201)));
    }

    // ========== Sequence decode ==========

    [Fact]
    public void Decode_first_of_two_u8_sequence()
    {
        CallThunk("decodeFirstOfTwoU8").Should().Be(JustOf(Integer(10)));
    }

    // ========== String width ==========

    [Fact]
    public void GetStringWidth_ascii()
    {
        ApplyUnary(GetTestFunction("getStringWidth"), String("hello"))
            .Should().Be(Integer(5));
    }

    [Fact]
    public void GetStringWidth_empty()
    {
        ApplyUnary(GetTestFunction("getStringWidth"), String(""))
            .Should().Be(Integer(0));
    }

    // ========== map2 ==========

    [Fact]
    public void Decode_map2_two_u8()
    {
        CallThunk("decodeMap2TwoU8")
            .Should().Be(JustOf(ElmList(Integer(10), Integer(20))));
    }

    [Fact]
    public void Decode_loop_sum_three_bytes()
    {
        // loop decoder: sums 3 unsigned bytes [10, 20, 30] = 60
        CallThunk("loopSumThreeBytes")
            .Should().Be(JustOf(Integer(60)));
    }

    [Fact]
    public void Decode_loop_map3_three_bytes()
    {
        // loop decoder with map3: sums 3 unsigned bytes [10, 20, 30] = 60
        CallThunk("loopMap3ThreeBytes")
            .Should().Be(JustOf(Integer(60)));
    }

    [Fact]
    public void Decode_loop_map3_string_concat()
    {
        // loop decoder with map3 and ++ string concat in callback
        CallThunk("loopMap3StringConcat")
            .Should().Be(JustOf(String("XYZ")));
    }

    [Fact]
    public void Decode_loop_map3_func_call()
    {
        // loop decoder with map3 and function call inside callback
        CallThunk("loopMap3FuncCall")
            .Should().Be(JustOf(String("ABC")));
    }

    [Fact]
    public void Decode_loop_map3_bitwise()
    {
        // loop decoder with map3, Bitwise ops, let-expression, and function call in callback
        // Encoding "Man" (77, 97, 110) should produce the 3 chars back
        CallThunk("loopMap3Bitwise")
            .Should().Be(JustOf(String("Man")));
    }
}
