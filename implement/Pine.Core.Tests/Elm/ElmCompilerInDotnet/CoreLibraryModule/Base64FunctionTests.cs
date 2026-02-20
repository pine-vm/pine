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

public class Base64FunctionTests
{
    private const string TestModuleSource =
        """
        module Base64Test exposing (..)

        import Base64
        import Base64.Decode
        import Bitwise
        import Bytes
        import Bytes.Decode
        import Bytes.Encode


        fromString : String -> Maybe String
        fromString s =
            Base64.fromString s


        toString : String -> Maybe String
        toString b64 =
            Base64.toString b64


        roundtripString : String -> Maybe String
        roundtripString s =
            case Base64.fromString s of
                Nothing ->
                    Nothing

                Just b64 ->
                    Base64.toString b64


        roundtripBase64 : String -> Maybe String
        roundtripBase64 b64 =
            case Base64.toBytes b64 of
                Nothing ->
                    Nothing

                Just bites ->
                    Base64.fromBytes bites


        encodeSingleByte : Int -> Maybe String
        encodeSingleByte n =
            Base64.fromBytes
                (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 n))


        encodeTwoBytes : Int -> Int -> Maybe String
        encodeTwoBytes a b =
            Base64.fromBytes
                (Bytes.Encode.encode
                    (Bytes.Encode.sequence
                        [ Bytes.Encode.unsignedInt8 a
                        , Bytes.Encode.unsignedInt8 b
                        ]
                    )
                )


        encodeEmptyBytes : Int -> Maybe String
        encodeEmptyBytes _ =
            Base64.fromBytes
                (Bytes.Encode.encode (Bytes.Encode.sequence []))


        toBytesWidth : String -> Maybe Int
        toBytesWidth b64 =
            case Base64.toBytes b64 of
                Nothing ->
                    Nothing

                Just bites ->
                    Just (Bytes.width bites)


        toBytesDecodeList : String -> Maybe (List Int)
        toBytesDecodeList b64 =
            case Base64.toBytes b64 of
                Nothing ->
                    Nothing

                Just bites ->
                    let
                        w =
                            Bytes.width bites

                        decoder =
                            decodeListHelper w []
                    in
                    Bytes.Decode.decode decoder bites


        decodeListHelper : Int -> List Int -> Bytes.Decode.Decoder (List Int)
        decodeListHelper remaining accum =
            if remaining <= 0 then
                Bytes.Decode.succeed (List.reverse accum)

            else
                Bytes.Decode.andThen
                    (\byte -> decodeListHelper (remaining - 1) (byte :: accum))
                    Bytes.Decode.unsignedInt8


        testBitsToChars : Int -> String
        testBitsToChars _ =
            Base64.Decode.bitsToChars 5071214 0


        -- Direct test of decoder with specific width
        testDecoder3 : Int -> Maybe String
        testDecoder3 _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 77
                            , Bytes.Encode.unsignedInt8 97
                            , Bytes.Encode.unsignedInt8 110
                            ]
                        )
            in
            Bytes.Decode.decode (Base64.Decode.decoder 3) bites


        -- Inline version of the loopHelp logic to test cross-module issue
        myDecoder : Int -> Bytes.Decode.Decoder String
        myDecoder width =
            Bytes.Decode.loop { remaining = width, string = "" } myLoopHelp


        myLoopHelp : { remaining : Int, string : String } -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, string : String } String)
        myLoopHelp { remaining, string } =
            if remaining >= 3 then
                let
                    helper a b c =
                        Bytes.Decode.Loop { remaining = remaining - 3, string = string ++ "XYZ" }
                in
                Bytes.Decode.map3 helper
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8
                    Bytes.Decode.unsignedInt8

            else
                Bytes.Decode.succeed (Bytes.Decode.Done string)


        testMyDecoder3 : Int -> Maybe String
        testMyDecoder3 _ =
            let
                bites =
                    Bytes.Encode.encode
                        (Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt8 77
                            , Bytes.Encode.unsignedInt8 97
                            , Bytes.Encode.unsignedInt8 110
                            ]
                        )
            in
            Bytes.Decode.decode (myDecoder 3) bites


        testDecoder0 : Int -> Maybe String
        testDecoder0 _ =
            let
                bites =
                    Bytes.Encode.encode (Bytes.Encode.sequence [])
            in
            Bytes.Decode.decode (Base64.Decode.decoder 0) bites


        -- Test calling Base64.Decode.loopHelp directly
        testLoopHelp0 : Int -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, string : String } String)
        testLoopHelp0 _ =
            Base64.Decode.loopHelp { remaining = 0, string = "hello" }


        testLoopHelp3 : Int -> Bytes.Decode.Decoder (Bytes.Decode.Step { remaining : Int, string : String } String)
        testLoopHelp3 _ =
            Base64.Decode.loopHelp { remaining = 3, string = "" }


        -- Test: use Base64.Decode.loopHelp as callback directly in our own loop
        testOwnLoop3 : Int -> Maybe String
        testOwnLoop3 _ =
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
                    Bytes.Decode.loop { remaining = 3, string = "" } Base64.Decode.loopHelp
            in
            Bytes.Decode.decode decoder bites
        """;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                var srcTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["src"])
                    ?? throw new Exception("Did not find src");

                var base64File = srcTree.GetNodeAtPath(["Base64.elm"]);
                var base64DecodeFile = srcTree.GetNodeAtPath(["Base64", "Decode.elm"]);
                var base64EncodeFile = srcTree.GetNodeAtPath(["Base64", "Encode.elm"]);

                var treeWithBase64 = kernelModulesTree;

                if (base64File is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64.elm"], base64File);

                if (base64DecodeFile is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Decode.elm"], base64DecodeFile);

                if (base64EncodeFile is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Encode.elm"], base64EncodeFile);

                var treeWithTest =
                    treeWithBase64.SetNodeAtPathSorted(
                        ["Base64Test.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleSource)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Base64Test.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: true)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "Base64Test")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static ElmValue CallThunk(string name) =>
        ApplyUnary(GetTestFunction(name), Integer(0));

    // ========== fromString tests (encoding: string → base64) ==========

    [Fact]
    public void FromString_empty()
    {
        ApplyUnary(GetTestFunction("fromString"), String(""))
            .Should().Be(JustOf(String("")));
    }

    [Fact]
    public void FromString_a()
    {
        ApplyUnary(GetTestFunction("fromString"), String("a"))
            .Should().Be(JustOf(String("YQ==")));
    }

    [Fact]
    public void FromString_ab()
    {
        ApplyUnary(GetTestFunction("fromString"), String("ab"))
            .Should().Be(JustOf(String("YWI=")));
    }

    [Fact]
    public void FromString_M()
    {
        ApplyUnary(GetTestFunction("fromString"), String("M"))
            .Should().Be(JustOf(String("TQ==")));
    }

    [Fact]
    public void FromString_Ma()
    {
        ApplyUnary(GetTestFunction("fromString"), String("Ma"))
            .Should().Be(JustOf(String("TWE=")));
    }

    // ========== fromString tests with 3+ bytes (triggers remaining >= 3 in loopHelp) ==========

    [Fact]
    public void FromString_abc()
    {
        // 3 bytes → triggers the `remaining >= 3` branch
        ApplyUnary(GetTestFunction("fromString"), String("abc"))
            .Should().Be(JustOf(String("YWJj")));
    }

    [Fact]
    public void FromString_Man()
    {
        // 3 bytes → triggers the `remaining >= 3` branch
        ApplyUnary(GetTestFunction("fromString"), String("Man"))
            .Should().Be(JustOf(String("TWFu")));
    }

    [Fact]
    public void FromString_Hello()
    {
        // 5 bytes → two iterations: 3+2
        ApplyUnary(GetTestFunction("fromString"), String("Hello"))
            .Should().Be(JustOf(String("SGVsbG8=")));
    }

    [Fact]
    public void FromString_Hello_World()
    {
        // 13 bytes → triggers multiple iterations of >= 3 path
        ApplyUnary(GetTestFunction("fromString"), String("Hello, World!"))
            .Should().Be(JustOf(String("SGVsbG8sIFdvcmxkIQ==")));
    }

    // ========== fromString tests with 18+ bytes (triggers decode18Bytes) ==========

    [Fact]
    public void Roundtrip_string_18_bytes()
    {
        // Exactly 18 bytes → triggers decode18Bytes path once then remaining == 0
        ApplyUnary(GetTestFunction("roundtripString"), String("ABCDEFGHIJKLMNOPQR"))
            .Should().Be(JustOf(String("ABCDEFGHIJKLMNOPQR")));
    }

    [Fact]
    public void Roundtrip_string_20_bytes()
    {
        // 20 bytes → decode18Bytes once, then remaining == 2
        ApplyUnary(GetTestFunction("roundtripString"), String("ABCDEFGHIJKLMNOPQRST"))
            .Should().Be(JustOf(String("ABCDEFGHIJKLMNOPQRST")));
    }

    [Fact]
    public void Roundtrip_string_36_bytes()
    {
        // 36 bytes → decode18Bytes twice, then remaining == 0
        ApplyUnary(GetTestFunction("roundtripString"), String("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
            .Should().Be(JustOf(String("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")));
    }

    [Fact]
    public void Roundtrip_string_Hello_World()
    {
        // 13 bytes → multiple iterations of >= 3 path
        ApplyUnary(GetTestFunction("roundtripString"), String("Hello, World!"))
            .Should().Be(JustOf(String("Hello, World!")));
    }

    // ========== toString tests (decoding: base64 → string) ==========

    [Fact]
    public void ToString_empty()
    {
        ApplyUnary(GetTestFunction("toString"), String(""))
            .Should().Be(JustOf(String("")));
    }

    [Fact]
    public void ToString_YWJj()
    {
        ApplyUnary(GetTestFunction("toString"), String("YWJj"))
            .Should().Be(JustOf(String("abc")));
    }

    [Fact]
    public void ToString_TWFu()
    {
        ApplyUnary(GetTestFunction("toString"), String("TWFu"))
            .Should().Be(JustOf(String("Man")));
    }

    [Fact]
    public void ToString_TWE_eq()
    {
        ApplyUnary(GetTestFunction("toString"), String("TWE="))
            .Should().Be(JustOf(String("Ma")));
    }

    [Fact]
    public void ToString_TQ_eq_eq()
    {
        ApplyUnary(GetTestFunction("toString"), String("TQ=="))
            .Should().Be(JustOf(String("M")));
    }

    [Fact]
    public void ToString_hello_world()
    {
        ApplyUnary(GetTestFunction("toString"), String("SGVsbG8sIFdvcmxkIQ=="))
            .Should().Be(JustOf(String("Hello, World!")));
    }

    [Fact]
    public void ToString_invalid_returns_nothing()
    {
        ApplyUnary(GetTestFunction("toString"), String("abc$"))
            .Should().Be(s_nothing);
    }

    [Fact]
    public void ToString_invalid_single_char()
    {
        ApplyUnary(GetTestFunction("toString"), String("A"))
            .Should().Be(s_nothing);
    }

    // ========== Roundtrip tests ==========

    [Fact]
    public void Roundtrip_string_empty()
    {
        ApplyUnary(GetTestFunction("roundtripString"), String(""))
            .Should().Be(JustOf(String("")));
    }

    [Fact]
    public void Roundtrip_string_Ma()
    {
        ApplyUnary(GetTestFunction("roundtripString"), String("Ma"))
            .Should().Be(JustOf(String("Ma")));
    }

    [Fact]
    public void Roundtrip_base64_with_padding()
    {
        ApplyUnary(GetTestFunction("roundtripBase64"), String("TWE="))
            .Should().Be(JustOf(String("TWE=")));
    }

    [Fact]
    public void Roundtrip_base64_invalid()
    {
        ApplyUnary(GetTestFunction("roundtripBase64"), String("!!!"))
            .Should().Be(s_nothing);
    }

    // ========== Byte-level encoding tests ==========

    [Fact]
    public void Encode_single_byte_zero()
    {
        ApplyUnary(GetTestFunction("encodeSingleByte"), Integer(0))
            .Should().Be(JustOf(String("AA==")));
    }

    [Fact]
    public void Encode_single_byte_255()
    {
        ApplyUnary(GetTestFunction("encodeSingleByte"), Integer(255))
            .Should().Be(JustOf(String("/w==")));
    }

    [Fact]
    public void Encode_two_bytes()
    {
        ApplyBinary(GetTestFunction("encodeTwoBytes"), Integer(0), Integer(0))
            .Should().Be(JustOf(String("AAA=")));
    }

    [Fact]
    public void Encode_empty_bytes()
    {
        CallThunk("encodeEmptyBytes")
            .Should().Be(JustOf(String("")));
    }

    // ========== toBytes width tests ==========

    [Fact]
    public void ToBytes_width_AAAA()
    {
        ApplyUnary(GetTestFunction("toBytesWidth"), String("AAAA"))
            .Should().Be(JustOf(Integer(3)));
    }

    [Fact]
    public void ToBytes_width_AAA_eq()
    {
        ApplyUnary(GetTestFunction("toBytesWidth"), String("AAA="))
            .Should().Be(JustOf(Integer(2)));
    }

    [Fact]
    public void ToBytes_width_AA_eq_eq()
    {
        ApplyUnary(GetTestFunction("toBytesWidth"), String("AA=="))
            .Should().Be(JustOf(Integer(1)));
    }

    [Fact]
    public void ToBytes_width_empty()
    {
        ApplyUnary(GetTestFunction("toBytesWidth"), String(""))
            .Should().Be(JustOf(Integer(0)));
    }

    // ========== toBytes content tests ==========

    [Fact]
    public void ToBytes_decode_AAAA()
    {
        var expected = JustOf(ElmValue.ListInstance([Integer(0), Integer(0), Integer(0)]));

        ApplyUnary(GetTestFunction("toBytesDecodeList"), String("AAAA"))
            .Should().Be(expected);
    }

    [Fact]
    public void ToBytes_decode_slash_slash_slash_slash()
    {
        var expected = JustOf(ElmValue.ListInstance([Integer(255), Integer(255), Integer(255)]));

        ApplyUnary(GetTestFunction("toBytesDecodeList"), String("////"))
            .Should().Be(expected);
    }

    [Fact]
    public void ToBytes_decode_TWFu()
    {
        var expected = JustOf(ElmValue.ListInstance([Integer(77), Integer(97), Integer(110)]));

        ApplyUnary(GetTestFunction("toBytesDecodeList"), String("TWFu"))
            .Should().Be(expected);
    }

    [Fact]
    public void ToBytes_decode_invalid()
    {
        ApplyUnary(GetTestFunction("toBytesDecodeList"), String("$$$"))
            .Should().Be(s_nothing);
    }

    // ========== Direct function tests ==========

    [Fact]
    public void Test_bitsToChars_directly()
    {
        CallThunk("testBitsToChars")
            .Should().Be(String("TWFu"));
    }

    [Fact]
    public void TestDecoder3_direct()
    {
        // Directly test Base64.Decode.decoder with 3-byte input (M, a, n)
        CallThunk("testDecoder3")
            .Should().Be(JustOf(String("TWFu")));
    }

    [Fact]
    public void TestMyDecoder3_inline()
    {
        // Test inline loop with map3 in Base64Test module (same compilation context as Base64)
        CallThunk("testMyDecoder3")
            .Should().Be(JustOf(String("XYZ")));
    }

    [Fact]
    public void TestDecoder0_real()
    {
        // Test real Base64.Decode.decoder with 0 bytes (no iteration, just Done)
        CallThunk("testDecoder0")
            .Should().Be(JustOf(String("")));
    }

    [Fact]
    public void TestOwnLoop3()
    {
        // Test using Base64.Decode.loopHelp as callback in our own loop
        CallThunk("testOwnLoop3")
            .Should().Be(JustOf(String("TWFu")));
    }
}
