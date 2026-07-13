using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using ElmCompilerTestHelper = Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTestHelper;

namespace Pine.IntegrationTests.CodeAnalysis;

public class ParserFastTests
{
    [Fact]
    public void ParserFast_benchmark_allPattern()
    {
        var ropeElmModuleText =
            """"
            module Rope exposing (..)

            type alias Rope a =
                Maybe (RopeFilled a)


            type RopeFilled a
                = Leaf a ()
                | Branch2 (RopeFilled a) (RopeFilled a)


            empty : Rope a
            empty =
                Nothing

            """";

        var rangeElmModuleText =
            """"
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
            
            """";

        var nodeElmModuleText =
            """"
            module Node exposing (..)

            import Range exposing (Range)


            type Node a
                = Node Range a

            """";

        var parserFastElmModuleText =
            """"
            module ParserFastTest exposing (..)

            import Range exposing (Range)


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
            
            """";

        var tokensElmModuleText =
            """"
            module TokensTest exposing (..)

            import ParserFastTest

            
            equal : ParserFastTest.Parser ()
            equal =
                ParserFastTest.symbol "=" ()


            parensEnd : ParserFastTest.Parser ()
            parensEnd =
                ParserFastTest.symbol ")" ()


            """";


        var patternElmModuleText =
            """"
            module PatternTest exposing (..)

            import Node exposing (Node)
            import ParserFastTest
            import Rope exposing (Rope)


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

            
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
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

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            PatternTest.allPattern =
                Parser (Eval, Litral Condition (Builtin, Blob 0x000000650000007100000075000000610000006c, (List, (Builtin, Blob 0x00000074000000610000006b00000065, (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))), (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))))) (List, [ Litral, Bad ], (List, (Litral, False), (List, [ Litral, ExpectingSymbol ], [ List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))) ]))) (List, [ Litral, Good ], (List, (Eval, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))), [ List, [ Litral, Blob 0x0000003c0000005200000065000000630000006f00000072000000640000005f000000540000007900000070000000650000003e ], [ Litral, Blob 0x000000650000006e00000064 ], [ List, [ Litral, Blob 0x0000003c0000005200000065000000630000006f00000072000000640000005f000000540000007900000070000000650000003e ], [ Litral, Blob 0x000000630000006f0000006c000000750000006d0000006e ], (Builtin, Blob 0x000000690000006e000000740000005f000000610000006400000064, (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))))), [ Litral, Blob 0x000000720000006f00000077 ], (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))) ], [ Litral, Blob 0x0000007300000074000000610000007200000074 ], [ List, [ Litral, Blob 0x0000003c0000005200000065000000630000006f00000072000000640000005f000000540000007900000070000000650000003e ], [ Litral, Blob 0x000000630000006f0000006c000000750000006d0000006e ], (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), [ Litral, Blob 0x000000720000006f00000077 ], (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))) ] ]), (List, [ Litral, PState ], [ List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))), (Builtin, Blob 0x000000690000006e000000740000005f000000610000006400000064, (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 3), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x000000690000006e000000740000005f000000610000006400000064, (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 2), [ Environment ]))))))))), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 4), (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ])))))))) ]))), [ List, Litral, Litral (Eval, Litral List [ Litral, WithComments ] (List, Litral Nothing [], (List, [ Litral, Node ], (List, (Builtin, Blob 0x00000068000000650000006100000064, (Builtin, Blob 0x000000730000006b0000006900000070, (List, (Litral, 1), [ Environment ]))), Litral AllPattern []))), (List, Litral, [ Environment ])) "_" '_' 4 1, [ Environment ] ])


            PatternTest.allPattern__lifted__lambda1 param_1 =
                [ WithComments
                , [ Nothing
                  , [ Node
                    , [ param_1
                      , AllPattern
                      ]
                    ]
                  ]
                ]
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

        var invocationReports = new List<Core.Interpreter.IntermediateVM.EvaluationReport>();

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(invocationReports.Add);

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

        invocationReports.Count.Should().BeLessThan(3);

        instructionCount.Should().BeLessThan(110);
    }
}
