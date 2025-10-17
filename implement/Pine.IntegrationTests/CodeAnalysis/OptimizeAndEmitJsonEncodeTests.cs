using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitJsonEncodeTests
{
    [Fact]
    public void Parse_and_emit_optimized_Json_Encode_encodeStringUtf32ChunksFromBytes()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            import Char


            encodeStringUtf32ChunksFromBytes : Int -> List Int -> Int -> List Int
            encodeStringUtf32ChunksFromBytes offset encodedChunks sourceBytes =
                let
                    simpleEnd : Int
                    simpleEnd =
                        advanceUtf32OffsetForSimpleChars sourceBytes offset

                    simpleLen : Int
                    simpleLen =
                        Pine_kernel.int_add [ simpleEnd, Pine_kernel.int_mul [ offset, -1 ] ]

                    simpleSlice =
                        Pine_kernel.take [ simpleLen, Pine_kernel.skip [ offset, sourceBytes ] ]

                    chunksWithSimple : List Int
                    chunksWithSimple =
                        if Pine_kernel.equal [ simpleLen, 0 ] then
                            encodedChunks

                        else
                            Pine_kernel.concat [ encodedChunks, [ simpleSlice ] ]

                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ simpleEnd, sourceBytes ] ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    chunksWithSimple

                else
                    case nextChar of
                        '\u{0008}' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'b' ] ])
                                sourceBytes

                        '\t' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\t' ] ])
                                sourceBytes

                        '\n' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\n' ] ])
                                sourceBytes

                        '\u{000C}' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'f' ] ])
                                sourceBytes

                        '\u{000D}' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'r' ] ])
                                sourceBytes

                        '"' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', '"' ] ])
                                sourceBytes

                        '\\' ->
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\\' ] ])
                                sourceBytes

                        _ ->
                            let
                                code : Int
                                code =
                                    Char.toCode nextChar

                                unicodeEscape : List Int
                                unicodeEscape =
                                    if Pine_kernel.int_is_sorted_asc [ 0, code, 0xFFFF ] then
                                        Pine_kernel.concat
                                            [ [ '\\', 'u' ]
                                            , hex4 code
                                            ]

                                    else
                                        let
                                            codePrime =
                                                Pine_kernel.int_add [ code, -0x00010000 ]

                                            hi10 =
                                                Pine_kernel.bit_shift_right [ 10, codePrime ]

                                            lo10 =
                                                Pine_kernel.bit_and [ 0x03FF, codePrime ]

                                            hiUnit =
                                                Pine_kernel.int_add [ 0xD800, hi10 ]

                                            loUnit =
                                                Pine_kernel.int_add [ 0xDC00, lo10 ]
                                        in
                                        Pine_kernel.concat
                                            [ [ '\\', 'u' ]
                                            , hex4 hiUnit
                                            , [ '\\', 'u' ]
                                            , hex4 loUnit
                                            ]
                            in
                            encodeStringUtf32ChunksFromBytes
                                (Pine_kernel.int_add [ simpleEnd, 4 ])
                                (Pine_kernel.concat [ chunksWithSimple, unicodeEscape ])
                                sourceBytes


            {-| Advance the pointer up to the next char that escaping is needed for.
            This is an optimization to skip over simple characters quickly.
            It assumes that the input is valid UTF-32.
            <https://datatracker.ietf.org/doc/html/rfc8259#section-7>
            -}
            advanceUtf32OffsetForSimpleChars : Int -> Int -> Int
            advanceUtf32OffsetForSimpleChars sourceBytes offset =
                let
                    nextChar =
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip [ offset, sourceBytes ]
                            ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    offset

                else
                    case nextChar of
                        '"' ->
                            offset

                        '\\' ->
                            offset

                        _ ->
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0x20

                                    -- Prepend sign byte to nextChar to compare as Int
                                    , Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], nextChar ]
                                    , 0x0010FFFF
                                    ]
                            then
                                advanceUtf32OffsetForSimpleChars
                                    sourceBytes
                                    (Pine_kernel.int_add [ offset, 4 ])

                            else
                                offset


            hex4 : Int -> List Int
            hex4 n =
                let
                    uintBytes =
                        Pine_kernel.skip [ 1, n ]

                    n3 =
                        Pine_kernel.bit_and
                            [ Pine_kernel.skip [ 1, 15 ]
                            , Pine_kernel.bit_shift_right [ 12, uintBytes ]
                            ]

                    n2 =
                        Pine_kernel.bit_and
                            [ Pine_kernel.skip [ 1, 15 ]
                            , Pine_kernel.bit_shift_right [ 8, uintBytes ]
                            ]

                    n1 =
                        Pine_kernel.bit_and
                            [ Pine_kernel.skip [ 1, 15 ]
                            , Pine_kernel.bit_shift_right [ 4, uintBytes ]
                            ]

                    n0 =
                        Pine_kernel.bit_and
                            [ Pine_kernel.skip [ 1, 15 ]
                            , uintBytes
                            ]
                in
                [ hexDigitCharFromNibble n3
                , hexDigitCharFromNibble n2
                , hexDigitCharFromNibble n1
                , hexDigitCharFromNibble n0
                ]


            hexDigitCharFromNibble : Int -> Int
            hexDigitCharFromNibble nibble =
                if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 0 ]
                        ]
                then
                    '0'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 1 ]
                        ]
                then
                    '1'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 2 ]
                        ]
                then
                    '2'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 3 ]
                        ]
                then
                    '3'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 4 ]
                        ]
                then
                    '4'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 5 ]
                        ]
                then
                    '5'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 6 ]
                        ]
                then
                    '6'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 7 ]
                        ]
                then
                    '7'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 8 ]
                        ]
                then
                    '8'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 9 ]
                        ]
                then
                    '9'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 10 ]
                        ]
                then
                    'A'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 11 ]
                        ]
                then
                    'B'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 12 ]
                        ]
                then
                    'C'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 13 ]
                        ]
                then
                    'D'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 14 ]
                        ]
                then
                    'E'

                else if
                    Pine_kernel.equal
                        [ nibble
                        , Pine_kernel.skip [ 1, 15 ]
                        ]
                then
                    'F'

                else
                    '?'
            
            
            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleTest =
            asCSharp.ModulesClasses[new DeclQualifiedName([], "Test")];

        var moduleTestCSharpText =
            moduleTest.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue advanceUtf32OffsetForSimpleChars(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    PineValue local_param_1_1 =
                        param_1_1;

                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_1, argument: local_param_1_0);

                        if (KernelFunctionSpecialized.length_as_int(local_000) == 0)
                        {
                            return local_param_1_1;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_doublequote)
                        {
                            return local_param_1_1;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_backslash)
                        {
                            return local_param_1_1;
                        }

                        if (KernelFunctionSpecialized.int_is_sorted_asc(
                            32,
                            KernelFunctionFused.BlobPrependByte(byteToPrepend: 4, suffix: local_000),
                            1_114_111) == PineKernelValues.TrueValue)
                        {
                            {
                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.int_add(4, local_param_1_1);

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        return local_param_1_1;
                    }
                }


                public static PineValue encodeStringUtf32ChunksFromBytes(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    PineValue local_param_1_1 =
                        param_1_1;

                    PineValue local_param_1_2 =
                        param_1_2;

                    while (true)
                    {
                        PineValue local_000 =
                            Test.advanceUtf32OffsetForSimpleChars(local_param_1_2, local_param_1_0);

                        PineValue local_001 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_000, argument: local_param_1_2);

                        if (KernelFunctionSpecialized.length_as_int(local_001) == 0)
                        {
                            PineValue local_002 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            if (local_002 == CommonReusedValues.Blob_Int_0)
                            {
                                return local_param_1_1;
                            }

                            return
                                KernelFunctionFused.ListAppendItem(
                                    prefix: local_param_1_1,
                                    itemToAppend:
                                    KernelFunctionSpecialized.take(
                                        local_002,
                                        KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2)));
                        }

                        if (local_001 == CommonReusedValues.Blob_f063beda)
                        {
                            PineValue local_003 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_003 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_003,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_11431555);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_tab)
                        {
                            PineValue local_004 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_004 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_004,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_6859d43a);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_newline)
                        {
                            PineValue local_005 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_005 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_005,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_8cc957f8);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_formfeed)
                        {
                            PineValue local_006 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_006 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_006,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_21f12336);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_carriagereturn)
                        {
                            PineValue local_007 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_007 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_007,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_4495e748);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_doublequote)
                        {
                            PineValue local_008 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_008 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_008,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_0593a027);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Char_backslash)
                        {
                            PineValue local_009 =
                                KernelFunctionSpecialized.int_add(
                                    local_000,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_000);

                                PineValue local_param_1_1_temp =
                                    KernelFunctionSpecialized.concat(
                                        local_009 == CommonReusedValues.Blob_Int_0
                                        ?
                                        local_param_1_1
                                        :
                                        KernelFunctionFused.ListAppendItem(
                                            prefix: local_param_1_1,
                                            itemToAppend:
                                            KernelFunctionSpecialized.take(
                                                local_009,
                                                KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                        CommonReusedValues.List_67fa5ac0);

                                local_param_1_0 =
                                    local_param_1_0_temp;

                                local_param_1_1 =
                                    local_param_1_1_temp;
                            }

                            continue;
                        }

                        PineValue local_010 =
                            KernelFunctionSpecialized.int_add(
                                local_000,
                                KernelFunctionSpecialized.int_mul(-1, local_param_1_0));

                        PineValue local_011 =
                            KernelFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive: true, unsignedValue: local_001);

                        {
                            PineValue local_param_1_0_temp =
                                KernelFunctionSpecialized.int_add(4, local_000);

                            PineValue local_param_1_1_temp =
                                KernelFunctionSpecialized.concat(
                                    local_010 == CommonReusedValues.Blob_Int_0
                                    ?
                                    local_param_1_1
                                    :
                                    KernelFunctionFused.ListAppendItem(
                                        prefix: local_param_1_1,
                                        itemToAppend:
                                        KernelFunctionSpecialized.take(
                                            local_010,
                                            KernelFunctionSpecialized.skip(local_param_1_0, local_param_1_2))),
                                    KernelFunctionSpecialized.int_is_sorted_asc(0, local_011, 65_535) == PineKernelValues.TrueValue
                                    ?
                                    KernelFunctionSpecialized.concat(
                                        CommonReusedValues.List_599c92a7,
                                        Test.hex4(local_011))
                                    :
                                    KernelFunction.concat(
                                        PineValue.List(
                                            [
                                                CommonReusedValues.List_599c92a7,
                                                Test.hex4(
                                                    KernelFunctionSpecialized.int_add(
                                                        55_296,
                                                        KernelFunctionSpecialized.bit_shift_right(
                                                            10,
                                                            KernelFunctionSpecialized.int_add(-65_536, local_011)))),
                                                CommonReusedValues.List_599c92a7,
                                                Test.hex4(
                                                    KernelFunctionSpecialized.int_add(
                                                        56_320,
                                                        KernelFunctionSpecialized.bit_and(
                                                            IntegerEncoding.EncodeSignedInteger(1_023),
                                                            KernelFunctionSpecialized.int_add(-65_536, local_011))))
                                            ])));

                            local_param_1_0 =
                                local_param_1_0_temp;

                            local_param_1_1 =
                                local_param_1_1_temp;
                        }

                        continue;
                    }
                }


                public static PineValue hex4(PineValue param_1_0)
                {
                    PineValue local_000 =
                        KernelFunctionSpecialized.skip(1, param_1_0);

                    return
                        PineValue.List(
                            [
                                Test.hexDigitCharFromNibble(
                                    KernelFunctionSpecialized.bit_and(
                                        CommonReusedValues.Blob_2d8b523c,
                                        KernelFunctionSpecialized.bit_shift_right(12, local_000))),
                                Test.hexDigitCharFromNibble(
                                    KernelFunctionSpecialized.bit_and(
                                        CommonReusedValues.Blob_2d8b523c,
                                        KernelFunctionSpecialized.bit_shift_right(8, local_000))),
                                Test.hexDigitCharFromNibble(
                                    KernelFunctionSpecialized.bit_and(
                                        CommonReusedValues.Blob_2d8b523c,
                                        KernelFunctionSpecialized.bit_shift_right(4, local_000))),
                                Test.hexDigitCharFromNibble(
                                    KernelFunctionSpecialized.bit_and(CommonReusedValues.Blob_2d8b523c, local_000))
                            ]);
                }


                public static PineValue hexDigitCharFromNibble(PineValue param_1_0)
                {
                    if (param_1_0 == CommonReusedValues.Blob_449e9b79)
                    {
                        return CommonReusedValues.Blob_Char_digit_0;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_50453b36)
                    {
                        return CommonReusedValues.Blob_Char_digit_1;
                    }

                    if (param_1_0 == PineKernelValues.FalseValue)
                    {
                        return CommonReusedValues.Blob_Char_digit_2;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_bd557c82)
                    {
                        return CommonReusedValues.Blob_Char_digit_3;
                    }

                    if (param_1_0 == PineKernelValues.TrueValue)
                    {
                        return CommonReusedValues.Blob_Char_digit_4;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_7732e8fd)
                    {
                        return CommonReusedValues.Blob_Char_digit_5;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_4c5dc722)
                    {
                        return CommonReusedValues.Blob_Char_digit_6;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_735edfdb)
                    {
                        return CommonReusedValues.Blob_Char_digit_7;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_8db117dc)
                    {
                        return CommonReusedValues.Blob_Char_digit_8;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_fb88d96b)
                    {
                        return CommonReusedValues.Blob_Char_digit_9;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_4c0d52d1)
                    {
                        return CommonReusedValues.Blob_Char_letter_A;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_40896845)
                    {
                        return CommonReusedValues.Blob_Char_letter_B;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_02334608)
                    {
                        return CommonReusedValues.Blob_Char_letter_C;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_015f2803)
                    {
                        return CommonReusedValues.Blob_Char_letter_D;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_63e02745)
                    {
                        return CommonReusedValues.Blob_Char_letter_E;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_2d8b523c)
                    {
                        return CommonReusedValues.Blob_Char_letter_F;
                    }

                    return CommonReusedValues.Blob_Char_question;
                }
            }

            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
            }

            """"
            .Trim());

        // Verify compilation to assembly works
        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var compiledDictionary =
            compileToAssemblyResult.BuildCompiledExpressionsDictionary();
    }
}
