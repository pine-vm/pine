using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class ParserFastTests
{
    [Fact]
    public void ParserFast_benchmark_allPattern()
    {
        var ropeElmModuleText =
            """
            module Rope exposing (..)

            type alias Rope a =
                Maybe (RopeFilled a)


            type RopeFilled a
                = Leaf a ()
                | Branch2 (RopeFilled a) (RopeFilled a)


            empty : Rope a
            empty =
                Nothing

            """;

        var rangeElmModuleText =
            """
            module Range exposing (..)
            
            type alias Location =
                { row : Int
                , column : Int
                }


            {-| Range for a piece of code with a start and end
            -}
            type alias Range =
                { start : Location
                , end : Location
                }
            
            """;

        var nodeElmModuleText =
            """
            module Node exposing (..)

            import Range exposing (Range)


            type Node a
                = Node Range a

            """;

        var parserFastElmModuleText =
            """
            module ParserFastTest exposing (..)


            type Parser a
                = Parser (State -> PStep a)


            type PStep value
                = Good value State
                | Bad Bool Problem


            type State
                = PState
                    -- source Utf32 bytes
                    Int
                    -- offset
                    Int
                    -- indent
                    Int
                    -- row
                    Int
                    -- column
                    Int


            type String
                = String Int
            
            
            type Problem
                = ExpectingNumber Int Int
                | ExpectingSymbol Int Int String
                | ExpectingAnyChar Int Int
                | ExpectingKeyword Int Int String
                | ExpectingCharSatisfyingPredicate Int Int
                | ExpectingStringSatisfyingPredicate Int Int
                | ExpectingCustom Int Int String
                | ExpectingOneOf Problem Problem (List Problem)


            run : Parser a -> String -> Result (List Parser.DeadEnd) a
            run (Parser parse) (String srcBytes) =
                case parse (PState srcBytes 0 1 1 1) of
                    Good value (PState finalSrc finalOffset _ finalRow finalCol) ->
                        if Pine_kernel.equal [ finalOffset, Pine_kernel.length srcBytes ] then
                            Ok value

                        else
                            Err [ { row = finalRow, col = finalCol, problem = Parser.ExpectingEnd } ]

                    Bad _ deadEnds ->
                        Err []


            symbol : String -> res -> Parser res
            symbol ((String strBytes) as str) res =
                let
                    strBytesLength : Int
                    strBytesLength =
                        Pine_kernel.length strBytes

                    strLength : Int
                    strLength =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, strBytesLength ] ]
                            ]
                in
                Parser
                    (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sOffsetInt : Int
                            sOffsetInt =
                                sOffset

                            sColInt : Int
                            sColInt =
                                sCol
                        in
                        if
                            Pine_kernel.equal
                                [ Pine_kernel.take [ strBytesLength, Pine_kernel.skip [ sOffsetInt, sSrcBytes ] ]
                                , strBytes
                                ]
                        then
                            Good res
                                (PState sSrcBytes
                                    (sOffsetInt + strBytesLength)
                                    sIndent
                                    sRow
                                    (sColInt + strLength)
                                )

                        else
                            Bad False (ExpectingSymbol sRow sCol str)
                    )
            

            symbolFollowedBy : String -> Parser next -> Parser next
            symbolFollowedBy str parseNext =
                Parser
                    (symbolFollowedByParser
                        str
                        parseNext
                    )


            symbolFollowedByParser : String -> Parser next -> State -> PStep next
            symbolFollowedByParser ((String strBytes) as str) (Parser parseNext) (PState sSrcBytes sOffset sIndent sRow sCol) =
                let
                    strBytesLength : Int
                    strBytesLength =
                        Pine_kernel.length strBytes

                    strLength : Int
                    strLength =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, strBytesLength ] ]
                            ]

                    sOffsetInt : Int
                    sOffsetInt =
                        sOffset

                    sColInt : Int
                    sColInt =
                        sCol

                    strSliceBytes : Int
                    strSliceBytes =
                        Pine_kernel.take
                            [ strBytesLength
                            , Pine_kernel.skip [ sOffsetInt, sSrcBytes ]
                            ]
                in
                if Pine_kernel.equal [ strBytes, strSliceBytes ] then
                    parseNext
                        (PState
                            sSrcBytes
                            (sOffsetInt + strBytesLength)
                            sIndent
                            sRow
                            (sColInt + strLength)
                        )
                        |> pStepCommit

                else
                    Bad False (ExpectingSymbol sRow sCol str)


            symbolWithRange : String -> (Range -> res) -> Parser res
            symbolWithRange ((String strBytes) as str) startAndEndLocationToRes =
                let
                    strBytesLength : Int
                    strBytesLength =
                        Pine_kernel.length strBytes

                    strLength : Int
                    strLength =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, strBytesLength ] ]
                            ]
                in
                Parser
                    (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sOffsetInt : Int
                            sOffsetInt =
                                sOffset

                            sColInt : Int
                            sColInt =
                                sCol

                            srcSliceBytes : Int
                            srcSliceBytes =
                                Pine_kernel.take
                                    [ strBytesLength
                                    , Pine_kernel.skip [ sOffsetInt, sSrcBytes ]
                                    ]
                        in
                        if Pine_kernel.equal [ srcSliceBytes, strBytes ] then
                            let
                                newCol : Int
                                newCol =
                                    sColInt + strLength
                            in
                            Good
                                (startAndEndLocationToRes
                                    { start = { row = sRow, column = sCol }
                                    , end = { row = sRow, column = newCol }
                                    }
                                )
                                (PState
                                    sSrcBytes
                                    (sOffsetInt + strBytesLength)
                                    sIndent
                                    sRow
                                    newCol
                                )

                        else
                            Bad False (ExpectingSymbol sRow sCol str)
                    )
            

            pStepCommit : PStep a -> PStep a
            pStepCommit pStep =
                case pStep of
                    (Good _ _) as good ->
                        good

                    Bad _ x ->
                        Bad True x
            
            """;

        var tokensElmModuleText =
            """
            module TokensTest exposing (..)

            import ParserFastTest

            
            equal : ParserFastTest.Parser ()
            equal =
                ParserFastTest.symbol "=" ()


            parensEnd : ParserFastTest.Parser ()
            parensEnd =
                ParserFastTest.symbol ")" ()


            """;


        var patternElmModuleText =
            """
            module PatternTest exposing (..)

            import Node exposing (Node)
            import ParserFastTest


            type Pattern
                = AllPattern
                | UnitPattern
                | CharPattern Char
                | StringPattern String
                | IntPattern Int
                | HexPattern Int
                | FloatPattern Float
                | TuplePattern (List (Node Pattern))
                | RecordPattern (List (Node String))
                | UnConsPattern (Node Pattern) (Node Pattern)
                | ListPattern (List (Node Pattern))
                | VarPattern String
                | NamedPattern QualifiedNameRef (List (Node Pattern))
                | AsPattern (Node Pattern) (Node String)
                | ParenthesizedPattern (Node Pattern)


            {-| Qualified name reference such as `Maybe.Just`.
            -}
            type alias QualifiedNameRef =
                { moduleName : List String
                , name : String
                }


            type WithComments res
                = WithComments Comments res


            type alias Comments =
                Rope (Node String)
            

            allPattern : ParserFastTest.Parser (WithComments (Node Pattern))
            allPattern =
                ParserFastTest.symbolWithRange "_"
                    (\range ->
                        WithComments
                            Rope.empty
                            (Node range AllPattern)
                    )


            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [
                rangeElmModuleText,
                nodeElmModuleText,
                ropeElmModuleText,
                parserFastElmModuleText,
                tokensElmModuleText,
                patternElmModuleText
                ],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["PatternTest"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            PatternTest.allPattern =
                [Parser, [[Function, [[ParseAndEval, [[Literal, [[Conditional, [[KernelApplication, [Blob 0x000000650000007100000075000000610000006c, [List, [[[ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 6, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]], Literal []]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]], [List, [[[Literal, [Bad]], [List, [[Literal False, [List, [[[Literal, [ExpectingSymbol]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 10, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 11, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]], [ParseAndEval, [[Literal, [[List, [[[Literal, [Good]], [List, [[[ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 4, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [List, [[[List, [[[Literal, [Elm_Record]], [List, [[[List, [[[List, [[[Literal, [Blob 0x000000650000006e00000064]], [List, [[[Literal, [Elm_Record]], [List, [[[List, [[[List, [[[Literal, [Blob 0x000000630000006f0000006c000000750000006d0000006e]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 12, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]], [List, [[[Literal, [Blob 0x000000720000006f00000077]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 8, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]]]]], [List, [[[Literal, [Blob 0x0000007300000074000000610000007200000074]], [List, [[[Literal, [Elm_Record]], [List, [[[List, [[[List, [[[Literal, [Blob 0x000000630000006f0000006c000000750000006d0000006e]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 9, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]], [List, [[[Literal, [Blob 0x000000720000006f00000077]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 8, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]], [List, [[[Literal, [PState]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 6, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x000000690000006e000000740000005f000000610000006400000064, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 11, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 5, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 7, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 8, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 12, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]]]], [List, [[[List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 4, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 5, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 7, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 9, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 10, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 11, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 12, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 13, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x000000690000006e000000740000005f000000610000006400000064, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 12, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]], Literal []]]]]]]]]]]]]], Literal []]]]]]]]]], [List, [[[List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 4, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 5, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [Literal, [[KernelApplication, [Blob 0x00000074000000610000006b00000065, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 5, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 13, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 7, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 4, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 4, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]]]]], Literal []]]]]], 1, [[Conditional, [[KernelApplication, [Blob 0x000000650000007100000075000000610000006c, [List, [[List [], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]]], [ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]], [Conditional, [[KernelApplication, [Blob 0x000000650000007100000075000000610000006c, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]], [Literal, [Function]]]]]]], [ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]], [Conditional, [[KernelApplication, [Blob 0x000000650000007100000075000000610000006c, [List, [[[KernelApplication, [Blob 0x0000006c000000650000006e000000670000007400000068, [KernelApplication, [Blob 0x000000630000006f0000006e000000630000006100000074, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]]]]]]], [List, [[[Literal, [Function]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x000000630000006f0000006e000000630000006100000074, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]]]]]]]]]]]]]], [ParseAndEval, [[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 2, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x000000630000006f0000006e000000630000006100000074, [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 3, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]]]]]]]]]]]]]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]]]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 2,Environment]]]]]]]]]]]]]]], [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]], [KernelApplication, [Blob 0x000000630000006f0000006e000000630000006100000074, [List, [[Literal True, [KernelApplication, [Blob 0x0000006200000069000000740000005f00000073000000680000006900000066000000740000005f0000007200000069000000670000006800000074, [List, [[Literal 2, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 1, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, [List, [[Literal 5, [KernelApplication, [Blob 0x00000068000000650000006100000064, Environment]]]]]]]]]]]]]]]]]]]]]]]], "_", '_', [Function, [[List, [[[Literal, [WithComments]], [List, [[Literal Nothing, [List, [[[Literal, [Node]], [List, [[[KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x00000068000000650000006100000064, [KernelApplication, [Blob 0x000000730000006b0000006900000070, List [Literal 1,Environment]]]]]]], Literal AllPattern]]]]]]]]]]]], 1, [], []]], 4], []]]]]
            
            """"
            .Trim());

        var parserFastModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "ParserFastTest");

        var patternModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "PatternTest");

        var parserRunDecl =
            parserFastModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "run");

        var allPatternDecl =
            patternModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "allPattern");

        var runParsed =
            FunctionRecord.ParseFunctionRecordTagged(parserRunDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(parserRunDecl) + ": " + err));

        var allPatternParsed =
            FunctionRecord.ParseFunctionRecordTagged(allPatternDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(allPatternDecl) + ": " + err));

        var invocationReports = new List<PineVM.EvaluationReport>();

        var vm =
            CodeAnalysisTestHelper.PineVMForProfiling(invocationReports.Add);

        var applyRunResult =
            ElmInteractiveEnvironment.ApplyFunction(
                vm,
                runParsed,
                arguments:
                [
                    allPatternDecl.Value,
                    ElmValueEncoding.StringAsPineValue("_")
                ])
            .Extract(err => throw new Exception(err));

        var resultAsElmValue =
            ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null)
            .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));

        var resultAsElmExpr =
            ElmValue.RenderAsElmExpression(resultAsElmValue);

        resultAsElmExpr.expressionString.Should().Be(
            "Ok (WithComments Nothing (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } AllPattern))");

        var instructionCount =
            invocationReports
            .Max(ir => ir.InstructionCount);

        invocationReports.Count.Should().BeLessThan(6);

        instructionCount.Should().BeLessThan(180);
    }
}
