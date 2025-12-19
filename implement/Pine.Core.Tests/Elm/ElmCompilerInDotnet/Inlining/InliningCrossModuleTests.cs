using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningCrossModuleTests
{
    [Fact]
    public void Tokens_typeName_Variant_1()
    {
        var parserFastModuleText =
            """"
            module ParserFast exposing (..)


            type Problem
                = ExpectingNumber Int Int
                | ExpectingSymbol Int Int String
                | ExpectingAnyChar Int Int
                | ExpectingKeyword Int Int String
                | ExpectingCharSatisfyingPredicate Int Int
                | ExpectingStringSatisfyingPredicate Int Int
                | ExpectingCustom Int Int String
                | ExpectingOneOf Problem Problem (List Problem)
            
            
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


            ifFollowedByWhileWithoutLinebreak :
                (Char -> Bool)
                -> (Char -> Bool)
                -> Parser String
            ifFollowedByWhileWithoutLinebreak firstIsOkay afterFirstIsOkay =
                Parser
                    (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sColInt : Int
                            sColInt =
                                sCol

                            nextCharBytes : Int
                            nextCharBytes =
                                Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.skip [ sOffset, sSrcBytes ]
                                    ]
                        in
                        if firstIsOkay nextCharBytes then
                            let
                                s1 : State
                                s1 =
                                    skipWhileWithoutLinebreakHelp
                                        afterFirstIsOkay
                                        (Pine_kernel.int_add [ sOffset, 4 ])
                                        sRow
                                        (sColInt + 1)
                                        sSrcBytes
                                        sIndent

                                (PState _ s1Offset _ _ _) =
                                    s1

                                nameSliceBytesLength : Int
                                nameSliceBytesLength =
                                    Pine_kernel.int_add
                                        [ s1Offset
                                        , Pine_kernel.int_mul [ -1, sOffset ]
                                        ]

                                nameSliceBytes : Int
                                nameSliceBytes =
                                    Pine_kernel.take
                                        [ nameSliceBytesLength
                                        , Pine_kernel.skip [ sOffset, sSrcBytes ]
                                        ]
                            in
                            Good (String nameSliceBytes) s1

                        else
                            Bad False (ExpectingCharSatisfyingPredicate sRow sCol)
                    )


            skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> Int -> Int -> State
            skipWhileWithoutLinebreakHelp isGood offset row col srcBytes indent =
                let
                    nextChar : Int
                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    PState srcBytes offset indent row col

                else if isGood nextChar then
                    skipWhileWithoutLinebreakHelp
                        isGood
                        (offset + 4)
                        row
                        (col + 1)
                        srcBytes
                        indent

                else
                    PState srcBytes offset indent row col

            """";

        var tokensModuleText =
            """"
            module Elm.Parser.Tokens exposing (..)


            isAlphaNumOrUnderscore : Char -> Bool
            isAlphaNumOrUnderscore char =
                if Char.isAlphaNum char then
                    True
                else
                    Pine_kernel.equal [ char, 0x5F ]


            typeName : ParserFast.Parser String
            typeName =
                ParserFast.ifFollowedByWhileWithoutLinebreak
                    Char.isUpper
                    isAlphaNumOrUnderscore

            """";

        var expectedTokensModuleText =
            """"            
            module Elm.Parser.Tokens exposing (..)


            isAlphaNumOrUnderscore : Char -> Bool
            isAlphaNumOrUnderscore char =
                if
                    Char.isAlphaNum
                        char
                then
                    Basics.True

                else
                    Pine_kernel.equal
                        [ char, 0x5F ]


            typeName : ParserFast.Parser String
            typeName =
                ParserFast.Parser
                    (\(ParserFast.PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sColInt : Int
                            sColInt =
                                sCol

                            nextCharBytes : Int
                            nextCharBytes =
                                Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.skip
                                        [ sOffset, sSrcBytes ]
                                    ]
                        in
                        if
                            Char.isUpper
                                nextCharBytes
                        then
                            let
                                s1 : ParserFast.State
                                s1 =
                                    ParserFast.skipWhileWithoutLinebreakHelp
                                        Elm.Parser.Tokens.isAlphaNumOrUnderscore
                                        (Pine_kernel.int_add
                                            [ sOffset, 4 ]
                                        )
                                        sRow
                                        (sColInt + 1)
                                        sSrcBytes
                                        sIndent

                                (ParserFast.PState _ s1Offset _ _ _) =
                                    s1

                                nameSliceBytesLength : Int
                                nameSliceBytesLength =
                                    Pine_kernel.int_add
                                        [ s1Offset
                                        , Pine_kernel.int_mul
                                            [ -1, sOffset ]
                                        ]

                                nameSliceBytes : Int
                                nameSliceBytes =
                                    Pine_kernel.take
                                        [ nameSliceBytesLength
                                        , Pine_kernel.skip
                                            [ sOffset, sSrcBytes ]
                                        ]
                            in
                            ParserFast.Good
                                (ParserFast.String
                                    nameSliceBytes
                                )
                                s1

                        else
                            ParserFast.Bad
                                Basics.False
                                (ParserFast.ExpectingCharSatisfyingPredicate
                                    sRow
                                    sCol
                                )
                    )

            """";

        var tokensModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                parserFastModuleText,
                tokensModuleText,
                s_moduleCharText
                ],
                ["Elm", "Parser", "Tokens"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(tokensModule);

        rendered.Trim().Should().Be(
            expectedTokensModuleText.Trim());
    }

    [Fact]
    public void Declarations_infixDirectionOnlyTwo_noRange()
    {
        var parserFastModuleText =
            """"
            module ParserFast exposing (..)


            type alias Location =
                { row : Int
                , column : Int
                }


            type alias Range =
                { start : Location
                , end : Location
                }


            type Node a
                = Node Range a

                        
            type Problem
                = ExpectingNumber Int Int
                | ExpectingSymbol Int Int String
                | ExpectingAnyChar Int Int
                | ExpectingKeyword Int Int String
                | ExpectingCharSatisfyingPredicate Int Int
                | ExpectingStringSatisfyingPredicate Int Int
                | ExpectingCustom Int Int String
                | ExpectingOneOf Problem Problem (List Problem)


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
            
            
            keyword : String -> res -> Parser res
            keyword ((String kwdCharsBytes) as kwd) res =
                let
                    kwdCharsBytesLength : Int
                    kwdCharsBytesLength =
                        Pine_kernel.length kwdCharsBytes

                    kwdLength : Int
                    kwdLength =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, kwdCharsBytesLength ] ]
                            ]
                in
                Parser
                    (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sOffsetInt : Int
                            sOffsetInt =
                                sOffset

                            newOffset : Int
                            newOffset =
                                sOffsetInt + kwdCharsBytesLength

                            sColInt : Int
                            sColInt =
                                sCol
                        in
                        if
                            Pine_kernel.equal
                                [ kwdCharsBytes
                                , Pine_kernel.take [ kwdCharsBytesLength, Pine_kernel.skip [ sOffsetInt, sSrcBytes ] ]
                                ]
                        then
                            if isSubCharAlphaNumOrUnderscore newOffset sSrcBytes then
                                Bad False (ExpectingKeyword sRow sCol kwd)

                            else
                                Good res
                                    (PState
                                        sSrcBytes
                                        newOffset
                                        sIndent
                                        sRow
                                        (sColInt + kwdLength)
                                    )

                        else
                            Bad False (ExpectingKeyword sRow sCol kwd)
                    )


            oneOf2 : Parser a -> Parser a -> Parser a
            oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
                Parser
                    (\s ->
                        case attemptFirst s of
                            (Good _ _) as firstGood ->
                                firstGood

                            (Bad firstCommitted firstX) as firstBad ->
                                if firstCommitted then
                                    firstBad

                                else
                                    case attemptSecond s of
                                        (Good _ _) as secondGood ->
                                            secondGood

                                        (Bad secondCommitted secondX) as secondBad ->
                                            if secondCommitted then
                                                secondBad

                                            else
                                                Bad False (ExpectingOneOf firstX secondX [])
                    )

            
            isSubCharAlphaNumOrUnderscore : Int -> Int -> Bool
            isSubCharAlphaNumOrUnderscore offsetBytes stringBytes =
                case Pine_kernel.take [ 4, Pine_kernel.skip [ offsetBytes, stringBytes ] ] of
                    '_' ->
                        True

                    c ->
                        Char.isAlphaNum c
            
            """";


        var declarationsModuleText =
            """"
            module Elm.Parser.Declarations exposing (..)


            type InfixDirection
                = Left
                | Right
                | Non


            infixDirectionOnlyTwo_noRange : ParserFast.Parser InfixDirection
            infixDirectionOnlyTwo_noRange =
                ParserFast.oneOf2
                    (ParserFast.keyword "right" Right)
                    (ParserFast.keyword "left" Left)
            """";

        var expectedDeclarationsModuleText =
            """"
            module Elm.Parser.Declarations exposing (..)


            type InfixDirection
                = Left
                | Right
                | Non


            infixDirectionOnlyTwo_noRange : ParserFast.Parser Elm.Parser.Declarations.InfixDirection
            infixDirectionOnlyTwo_noRange =
                let
                    (ParserFast.Parser attemptFirst) =
                        (ParserFast.keyword
                            "right"
                            Elm.Parser.Declarations.Right
                        )

                    (ParserFast.Parser attemptSecond) =
                        (ParserFast.keyword
                            "left"
                            Elm.Parser.Declarations.Left
                        )
                in
                ParserFast.Parser
                    (\s ->
                        case attemptFirst s of
                            (ParserFast.Good _ _) as firstGood ->
                                firstGood

                            (ParserFast.Bad firstCommitted firstX) as firstBad ->
                                if firstCommitted then
                                    firstBad

                                else
                                    case attemptSecond s of
                                        (ParserFast.Good _ _) as secondGood ->
                                            secondGood

                                        (ParserFast.Bad secondCommitted secondX) as secondBad ->
                                            if secondCommitted then
                                                secondBad

                                            else
                                                ParserFast.Bad
                                                    Basics.False
                                                    (ParserFast.ExpectingOneOf
                                                        firstX
                                                        secondX
                                                        []
                                                    )
                    )
            """";

        var declarationsModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                parserFastModuleText,
                declarationsModuleText,
                s_moduleCharText
                ],
                ["Elm", "Parser", "Declarations"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(declarationsModule);

        rendered.Trim().Should().Be(
            expectedDeclarationsModuleText.Trim());
    }

    [Fact(Skip = "TODO")]
    public void Declarations_infixDirectionOnlyTwo()
    {
        /*
         * This case is not trivial because we want to inline both 'oneOf2' and 'mapWithRange'
         * */

        var parserFastModuleText =
            """"
            module ParserFast exposing (..)


            type alias Location =
                { row : Int
                , column : Int
                }


            type alias Range =
                { start : Location
                , end : Location
                }


            type Node a
                = Node Range a

                        
            type Problem
                = ExpectingNumber Int Int
                | ExpectingSymbol Int Int String
                | ExpectingAnyChar Int Int
                | ExpectingKeyword Int Int String
                | ExpectingCharSatisfyingPredicate Int Int
                | ExpectingStringSatisfyingPredicate Int Int
                | ExpectingCustom Int Int String
                | ExpectingOneOf Problem Problem (List Problem)


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
            

            keyword : String -> res -> Parser res
            keyword ((String kwdCharsBytes) as kwd) res =
                let
                    kwdCharsBytesLength : Int
                    kwdCharsBytesLength =
                        Pine_kernel.length kwdCharsBytes

                    kwdLength : Int
                    kwdLength =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, kwdCharsBytesLength ] ]
                            ]
                in
                Parser
                    (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                        let
                            sOffsetInt : Int
                            sOffsetInt =
                                sOffset

                            newOffset : Int
                            newOffset =
                                sOffsetInt + kwdCharsBytesLength

                            sColInt : Int
                            sColInt =
                                sCol
                        in
                        if
                            Pine_kernel.equal
                                [ kwdCharsBytes
                                , Pine_kernel.take [ kwdCharsBytesLength, Pine_kernel.skip [ sOffsetInt, sSrcBytes ] ]
                                ]
                        then
                            if isSubCharAlphaNumOrUnderscore newOffset sSrcBytes then
                                Bad False (ExpectingKeyword sRow sCol kwd)

                            else
                                Good res
                                    (PState
                                        sSrcBytes
                                        newOffset
                                        sIndent
                                        sRow
                                        (sColInt + kwdLength)
                                    )

                        else
                            Bad False (ExpectingKeyword sRow sCol kwd)
                    )


            oneOf2 : Parser a -> Parser a -> Parser a
            oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
                Parser
                    (\s ->
                        case attemptFirst s of
                            (Good _ _) as firstGood ->
                                firstGood

                            (Bad firstCommitted firstX) as firstBad ->
                                if firstCommitted then
                                    firstBad

                                else
                                    case attemptSecond s of
                                        (Good _ _) as secondGood ->
                                            secondGood

                                        (Bad secondCommitted secondX) as secondBad ->
                                            if secondCommitted then
                                                secondBad

                                            else
                                                Bad False (ExpectingOneOf firstX secondX [])
                    )

            
            isSubCharAlphaNumOrUnderscore : Int -> Int -> Bool
            isSubCharAlphaNumOrUnderscore offsetBytes stringBytes =
                case Pine_kernel.take [ 4, Pine_kernel.skip [ offsetBytes, stringBytes ] ] of
                    '_' ->
                        True

                    c ->
                        Char.isAlphaNum c
            
            """";


        var declarationsModuleText =
            """"
            module Elm.Parser.Declarations exposing (..)
            

            import ParserFast exposing (Node(..))


            type InfixDirection
                = Left
                | Right
                | Non


            infixDirectionOnlyTwo : ParserFast.Parser (Node Infix.InfixDirection)
            infixDirectionOnlyTwo =
                ParserFast.oneOf2
                    (ParserFast.mapWithRange Node (ParserFast.keyword "right" Infix.Right))
                    (ParserFast.mapWithRange Node (ParserFast.keyword "left" Infix.Left))
            """";

        var expectedDeclarationsModuleText =
            """"
            module Elm.Parser.Declarations exposing (..)


            type InfixDirection
                = Left
                | Right
                | Non
            

            infixDirectionOnlyTwo : Parser (Node InfixDirection)
            infixDirectionOnlyTwo =
                -- TODO: Sketch
            """";

        var declarationsModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                parserFastModuleText,
                declarationsModuleText,
                s_moduleCharText
                ],
                ["Elm", "Parser", "Declarations"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(declarationsModule);

        rendered.Trim().Should().Be(
            expectedDeclarationsModuleText.Trim());
    }

    private static readonly string s_moduleCharText =
        """"
        module Char exposing (..)


        isUpper : Char -> Bool
        isUpper char =
            let
                code : Int
                code =
                    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
            in
            Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ]


        isLower : Char -> Bool
        isLower char =
            let
                code : Int
                code =
                    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
            in
            Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]


        isAlpha : Char -> Bool
        isAlpha char =
            let
                code : Int
                code =
                    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
            in
            if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
                True

            else
                Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]


        isAlphaNum : Char -> Bool
        isAlphaNum char =
            let
                code =
                    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
            in
            if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
                True

            else if Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ] then
                True

            else
                Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ]
        """";
}
