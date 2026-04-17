using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningCrossModuleTests
{
    [Fact]
    public void Tokens_characterLiteralMapWithRange_reduced()
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


            type Parser a
                = Parser (() -> a)


            symbol : String -> res -> Parser res
            symbol _ res =
                Parser (\() -> res)


            symbolFollowedBy : String -> Parser next -> Parser next
            symbolFollowedBy _ (Parser parseNext) =
                Parser (\() -> parseNext ())


            followedBySymbol : String -> Parser a -> Parser a
            followedBySymbol _ (Parser parsePrevious) =
                Parser (\() -> parsePrevious ())


            anyChar : Parser Char
            anyChar =
                Parser (\() -> 'x')


            oneOf2MapWithStartRowColumnAndEndRowColumn :
                (Int -> Int -> first -> Int -> Int -> choice)
                -> Parser first
                -> (Int -> Int -> second -> Int -> Int -> choice)
                -> Parser second
                -> Parser choice
            oneOf2MapWithStartRowColumnAndEndRowColumn firstToChoice (Parser attemptFirst) _ _ =
                Parser
                    (\() ->
                        firstToChoice 11 22 (attemptFirst ()) 33 44
                    )
            """";

        var tokensModuleText =
            """"
            module Elm.Parser.Tokens exposing (..)

            import ParserFast


            identityChar : Char -> Char
            identityChar char =
                char


            escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
            escapedCharValueMap charToRes =
                ParserFast.symbol "n" (charToRes '\n')


            characterLiteralMapWithRange : (ParserFast.Range -> Char -> res) -> ParserFast.Parser res
            characterLiteralMapWithRange rangeAndCharToRes =
                ParserFast.symbolFollowedBy "'"
                    (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
                        (\startRow startColumn char endRow endColumn ->
                            rangeAndCharToRes
                                { start = { row = startRow, column = startColumn - 1 }
                                , end = { row = endRow, column = endColumn + 1 }
                                }
                                char
                        )
                        (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identityChar))
                        (\startRow startColumn char endRow endColumn ->
                            rangeAndCharToRes
                                { start = { row = startRow, column = startColumn - 1 }
                                , end = { row = endRow, column = endColumn + 1 }
                                }
                                char
                        )
                        ParserFast.anyChar
                        |> ParserFast.followedBySymbol "'"
                    )
            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Elm.Parser.Tokens
            import ParserFast


            type alias Captured =
                { startColumn : Int
                , endColumn : Int
                , char : Char
                }


            capture : ParserFast.Range -> Char -> Captured
            capture range char =
                { startColumn = range.start.column
                , endColumn = range.end.column
                , char = char
                }


            charLiteral : ParserFast.Parser Captured
            charLiteral =
                Elm.Parser.Tokens.characterLiteralMapWithRange capture
            """";

        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
                [
                parserFastModuleText,
                tokensModuleText,
                appModuleText,
                ],
                ["App"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        rendered.Trim().Should().Be(
            """
            type alias App.Captured =
                { startColumn : Int, endColumn : Int, char : Char }


            App.capture : ParserFast.Range -> Char -> App.Captured
            App.capture range char =
                { startColumn = range.start.column
                , endColumn = range.end.column
                , char = char
                }


            App.charLiteral : ParserFast.Parser App.Captured
            App.charLiteral =
                (ParserFast.followedBySymbol "'"
                    (ParserFast.Parser
                        App.charLiteral__lifted__lambda1
                    )
                )


            App.charLiteral__lifted__lambda1 () =
                App.capture
                    { start =
                        { row = 11
                        , column =
                            Pine_builtin.int_add
                                [ 22
                                , Pine_builtin.int_mul
                                    [ -1, 1 ]
                                ]
                        }
                    , end =
                        { row = 33
                        , column =
                            Pine_builtin.int_add
                                [ 44, 1 ]
                        }
                    }
                    (Elm.Parser.Tokens.identityChar
                        '\n'
                    )
            """.Trim());
    }

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
                                        (Pine_builtin.int_add
                                            [ sColInt, 1 ]
                                        )
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
            Elm.Parser.Tokens.isAlphaNumOrUnderscore : Char -> Bool
            Elm.Parser.Tokens.isAlphaNumOrUnderscore char =
                if
                    Char.isAlphaNum
                        char
                then
                    Basics.True

                else
                    Pine_kernel.equal
                        [ char, 0x5F ]


            Elm.Parser.Tokens.skipWhileWithoutLinebreakHelp__specialized__1 offset row col srcBytes indent =
                let
                    nextChar : Int
                    nextChar =
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ offset, srcBytes ]
                            ]
                in
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            nextChar
                        , 0
                        ]
                then
                    ParserFast.PState
                        srcBytes
                        offset
                        indent
                        row
                        col

                else if
                    Elm.Parser.Tokens.isAlphaNumOrUnderscore
                        nextChar
                then
                    Elm.Parser.Tokens.skipWhileWithoutLinebreakHelp__specialized__1
                        (Pine_builtin.int_add
                            [ offset, 4 ]
                        )
                        row
                        (Pine_builtin.int_add
                            [ col, 1 ]
                        )
                        srcBytes
                        indent

                else
                    ParserFast.PState
                        srcBytes
                        offset
                        indent
                        row
                        col


            Elm.Parser.Tokens.typeName : ParserFast.Parser String
            Elm.Parser.Tokens.typeName =
                ParserFast.Parser
                    Elm.Parser.Tokens.typeName__lifted__lambda1


            Elm.Parser.Tokens.typeName__lifted__lambda1 (ParserFast.PState sSrcBytes sOffset sIndent sRow sCol) =
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
                            Elm.Parser.Tokens.skipWhileWithoutLinebreakHelp__specialized__1
                                (Pine_kernel.int_add
                                    [ sOffset, 4 ]
                                )
                                sRow
                                (Pine_builtin.int_add
                                    [ sColInt, 1 ]
                                )
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
            """";

        var tokensModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
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
            type Elm.Parser.Declarations.InfixDirection
                = Left
                | Right
                | Non


            Elm.Parser.Declarations.infixDirectionOnlyTwo_noRange : ParserFast.Parser Elm.Parser.Declarations.InfixDirection
            Elm.Parser.Declarations.infixDirectionOnlyTwo_noRange =
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
                    (Elm.Parser.Declarations.infixDirectionOnlyTwo_noRange__lifted__lambda1
                        ( attemptFirst, attemptSecond )
                    )


            Elm.Parser.Declarations.infixDirectionOnlyTwo_noRange__lifted__lambda1 ( attemptFirst, attemptSecond ) s =
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
            """";

        var declarationsModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
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

    [Fact]
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
            type Elm.Parser.Declarations.InfixDirection
                = Left
                | Right
                | Non


            Elm.Parser.Declarations.infixDirectionOnlyTwo : ParserFast.Parser ParserFast.Node Infix.InfixDirection
            Elm.Parser.Declarations.infixDirectionOnlyTwo =
                let
                    (ParserFast.Parser attemptFirst) =
                        (ParserFast.mapWithRange
                            ParserFast.Node
                            (ParserFast.keyword
                                "right"
                                Infix.Right
                            )
                        )

                    (ParserFast.Parser attemptSecond) =
                        (ParserFast.mapWithRange
                            ParserFast.Node
                            (ParserFast.keyword
                                "left"
                                Infix.Left
                            )
                        )
                in
                ParserFast.Parser
                    (Elm.Parser.Declarations.infixDirectionOnlyTwo__lifted__lambda1
                        ( attemptFirst, attemptSecond )
                    )


            Elm.Parser.Declarations.infixDirectionOnlyTwo__lifted__lambda1 ( attemptFirst, attemptSecond ) s =
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
            """";

        var declarationsModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
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

    /// <summary>
    /// Analog to <see cref="Tokens_characterLiteralMapWithRange_reduced"/>, but tests
    /// <c>charLiteralExpression</c> from Expression.elm with all its verbatim dependencies.
    /// This provides a representative scenario for specialization and inlining stages.
    /// </summary>
    [Fact]
    public void Expression_charLiteralExpression()
    {
        var parserFastModuleText = s_expressionParserFastModuleText;

        var elmSyntaxRangeModuleText =
            """"
            module Elm.Syntax.Range exposing (..)


            type alias Location =
                { row : Int
                , column : Int
                }


            type alias Range =
                { start : Location
                , end : Location
                }
            """";

        var elmSyntaxNodeModuleText =
            """"
            module Elm.Syntax.Node exposing (..)

            import Elm.Syntax.Range exposing (Range)


            type Node a
                = Node Range a
            """";

        var ropeModuleText =
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

        var parserWithCommentsModuleText =
            """"
            module ParserWithComments exposing (..)

            import Elm.Syntax.Node exposing (Node)
            import Rope exposing (Rope)


            type WithComments res
                = WithComments Comments res


            type alias Comments =
                Rope (Node String)
            """";

        // Verbatim from elm-syntax/src/Elm/Parser/Tokens.elm
        var tokensModuleText =
            """"
            module Elm.Parser.Tokens exposing (..)

            import Elm.Syntax.Node exposing (Node(..))
            import Elm.Syntax.Range exposing (Range)
            import ParserFast


            escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
            escapedCharValueMap charToRes =
                ParserFast.oneOf7
                    (ParserFast.symbol "'" (charToRes '\''))
                    (ParserFast.symbol "\"" (charToRes '"'))
                    (ParserFast.symbol "n" (charToRes '\n'))
                    (ParserFast.symbol "t" (charToRes '\t'))
                    -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
                    (ParserFast.symbol "r" (charToRes '\u{000D}'))
                    (ParserFast.symbol "\\" (charToRes '\\'))
                    (ParserFast.symbolFollowedBy "u{"
                        (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                            (\hex ->
                                charToRes (Char.fromCode (hexStringToInt hex))
                            )
                            Char.isHexDigit
                            Char.isHexDigit
                            |> ParserFast.followedBySymbol "}"
                        )
                    )


            hexStringToInt : String -> Int
            hexStringToInt (String stringBytes) =
                hexStringBytesToInt 0 0 stringBytes


            hexStringBytesToInt : Int -> Int -> Int -> Int
            hexStringBytesToInt offset sum srcBytes =
                let
                    nextChar : Char
                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    sum

                else
                    hexStringBytesToInt
                        (Pine_kernel.int_add [ offset, 4 ])
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul [ 16, sum ]
                            , charToHex nextChar
                            ]
                        )
                        srcBytes


            charToHex : Char -> Int
            charToHex c =
                case c of
                    '0' ->
                        0

                    '1' ->
                        1

                    '2' ->
                        2

                    '3' ->
                        3

                    '4' ->
                        4

                    '5' ->
                        5

                    '6' ->
                        6

                    '7' ->
                        7

                    '8' ->
                        8

                    '9' ->
                        9

                    'a' ->
                        10

                    'b' ->
                        11

                    'c' ->
                        12

                    'd' ->
                        13

                    'e' ->
                        14

                    'f' ->
                        15

                    'A' ->
                        10

                    'B' ->
                        11

                    'C' ->
                        12

                    'D' ->
                        13

                    'E' ->
                        14

                    -- 'F'
                    _ ->
                        15


            characterLiteralMapWithRange : (Range -> Char -> res) -> ParserFast.Parser res
            characterLiteralMapWithRange rangeAndCharToRes =
                ParserFast.symbolFollowedBy "'"
                    (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
                        (\startRow startColumn char endRow endColumn ->
                            rangeAndCharToRes
                                { start = { row = startRow, column = startColumn - 1 }
                                , end = { row = endRow, column = endColumn + 1 }
                                }
                                char
                        )
                        (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
                        (\startRow startColumn char endRow endColumn ->
                            rangeAndCharToRes
                                { start = { row = startRow, column = startColumn - 1 }
                                , end = { row = endRow, column = endColumn + 1 }
                                }
                                char
                        )
                        ParserFast.anyChar
                        |> ParserFast.followedBySymbol "'"
                    )
            """";

        var charModuleText =
            """"
            module Char exposing (..)


            isHexDigit : Char -> Bool
            isHexDigit char =
                let
                    code : Int
                    code =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
                in
                Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ]
                    || Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x46 ]
                    || Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x66 ]


            fromCode : Int -> Char
            fromCode code =
                Pine_kernel.reverse
                    (Pine_kernel.take
                        [ 4
                        , Pine_kernel.concat
                            [ Pine_kernel.reverse (Pine_kernel.skip [ 1, code ])
                            , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                            ]
                        ]
                    )
            """";

        // Verbatim from elm-syntax/src/Elm/Parser/Expression.elm
        var expressionModuleText =
            """"
            module Elm.Parser.Expression exposing (..)

            import Elm.Parser.Tokens as Tokens
            import Elm.Syntax.Node exposing (Node(..))
            import Elm.Syntax.Range exposing (Range)
            import ParserFast exposing (Parser)
            import ParserWithComments exposing (Comments, WithComments(..))
            import Rope


            type Expression
                = CharLiteral Char


            charLiteralExpression : Parser (WithComments (Node Expression))
            charLiteralExpression =
                Tokens.characterLiteralMapWithRange
                    (\range char ->
                        WithComments
                            Rope.empty
                            (Node range (CharLiteral char))
                    )
            """";

        var expressionModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
                [
                parserFastModuleText,
                elmSyntaxRangeModuleText,
                elmSyntaxNodeModuleText,
                ropeModuleText,
                parserWithCommentsModuleText,
                charModuleText,
                tokensModuleText,
                expressionModuleText,
                ],
                ["Elm", "Parser", "Expression"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(expressionModule);

        var expectedExpressionModuleText =
            """"
            type Elm.Parser.Expression.Expression
                = CharLiteral Char


            Elm.Parser.Expression.charLiteralExpression : ParserFast.Parser ParserWithComments.WithComments Elm.Syntax.Node.Node Elm.Parser.Expression.Expression
            Elm.Parser.Expression.charLiteralExpression =
                ParserFast.symbolFollowedBy
                    "'"
                    (ParserFast.followedBySymbol "'"
                        (Elm.Parser.Expression.oneOf2MapWithStartRowColumnAndEndRowColumn__specialized__1
                            Elm.Parser.Expression.charLiteralExpression__lifted__lambda1
                            (ParserFast.symbolFollowedBy
                                "\\"
                                (Elm.Parser.Tokens.escapedCharValueMap
                                    Basics.identity
                                )
                            )
                            Elm.Parser.Expression.charLiteralExpression__lifted__lambda2
                        )
                    )


            Elm.Parser.Expression.charLiteralExpression__lifted__lambda1 startRow startColumn char endRow endColumn =
                ParserWithComments.WithComments
                    Rope.empty
                    (Elm.Syntax.Node.Node
                        { start =
                            { row = startRow
                            , column =
                                Pine_builtin.int_add
                                    [ startColumn
                                    , Pine_builtin.int_mul
                                        [ -1, 1 ]
                                    ]
                            }
                        , end =
                            { row = endRow
                            , column =
                                Pine_builtin.int_add
                                    [ endColumn, 1 ]
                            }
                        }
                        (Elm.Parser.Expression.CharLiteral
                            char
                        )
                    )


            Elm.Parser.Expression.charLiteralExpression__lifted__lambda2 startRow startColumn char endRow endColumn =
                ParserWithComments.WithComments
                    Rope.empty
                    (Elm.Syntax.Node.Node
                        { start =
                            { row = startRow
                            , column =
                                Pine_builtin.int_add
                                    [ startColumn
                                    , Pine_builtin.int_mul
                                        [ -1, 1 ]
                                    ]
                            }
                        , end =
                            { row = endRow
                            , column =
                                Pine_builtin.int_add
                                    [ endColumn, 1 ]
                            }
                        }
                        (Elm.Parser.Expression.CharLiteral
                            char
                        )
                    )


            Elm.Parser.Expression.oneOf2MapWithStartRowColumnAndEndRowColumn__specialized__1 firstToChoice (ParserFast.Parser attemptFirst) secondToChoice =
                ParserFast.Parser
                    (Elm.Parser.Expression.oneOf2MapWithStartRowColumnAndEndRowColumn__specialized__1__lifted__lambda1
                        ( attemptFirst, firstToChoice, secondToChoice )
                    )


            Elm.Parser.Expression.oneOf2MapWithStartRowColumnAndEndRowColumn__specialized__1__lifted__lambda1 ( attemptFirst, firstToChoice, secondToChoice ) s =
                let
                    (ParserFast.PState _ _ _ sRow sCol) =
                        s
                in
                case attemptFirst s of
                    ParserFast.Good first s1 ->
                        let
                            (ParserFast.PState _ _ _ s1Row s1Col) =
                                s1
                        in
                        ParserFast.Good
                            (firstToChoice
                                sRow
                                sCol
                                first
                                s1Row
                                s1Col
                            )
                            s1

                    ParserFast.Bad firstCommitted firstX ->
                        if firstCommitted then
                            ParserFast.Bad
                                firstCommitted
                                firstX

                        else
                            case
                                let
                                    (ParserFast.PState sSrcBytes sOffsetBytes sIndent sRow_0 sCol_0) =
                                        s
                                in
                                let
                                    sOffsetBytesInt : Int
                                    sOffsetBytesInt =
                                        sOffsetBytes

                                    newOffset : Int
                                    newOffset =
                                        ParserFast.charOrEnd sOffsetBytesInt sSrcBytes

                                    sColInt : Int
                                    sColInt =
                                        sCol_0

                                    sRowInt : Int
                                    sRowInt =
                                        sRow_0
                                in
                                if Pine_builtin.equal [ newOffset, (-1) ] then
                                    ParserFast.Bad Basics.False (ParserFast.ExpectingAnyChar sRow_0 sCol_0)

                                else if Pine_builtin.equal [ newOffset, (-2) ] then
                                    ParserFast.Good '\n' (ParserFast.PState sSrcBytes (Pine_builtin.int_add [ sOffsetBytesInt, 4 ]) sIndent (Pine_builtin.int_add [ sRowInt, 1 ]) 1)

                                else
                                    let
                                        foundChar : Char
                                        foundChar =
                                            Pine_kernel.take [ 4, Pine_kernel.skip [ sOffsetBytesInt, sSrcBytes ] ]
                                    in
                                    if Pine_kernel.equal [ Pine_kernel.length foundChar, 0 ] then
                                        ParserFast.Bad Basics.False (ParserFast.ExpectingAnyChar sRowInt sCol_0)

                                    else
                                        ParserFast.Good foundChar (ParserFast.PState sSrcBytes newOffset sIndent sRowInt (Pine_builtin.int_add [ sColInt, 1 ]))
                            of
                                ParserFast.Good second s1 ->
                                    let
                                        (ParserFast.PState _ _ _ s1Row s1Col) =
                                            s1
                                    in
                                    ParserFast.Good
                                        (secondToChoice
                                            sRow
                                            sCol
                                            second
                                            s1Row
                                            s1Col
                                        )
                                        s1

                                ParserFast.Bad secondCommitted secondX ->
                                    if secondCommitted then
                                        ParserFast.Bad
                                            secondCommitted
                                            secondX

                                    else
                                        ParserFast.Bad
                                            Basics.False
                                            (ParserFast.ExpectingOneOf
                                                firstX
                                                secondX
                                                []
                                            )
            """";

        rendered.Trim().Should().Be(
            expectedExpressionModuleText.Trim());
    }

    /// <summary>
    /// Tests <c>multiRecordAccess</c> from Expression.elm with all its verbatim dependencies.
    /// This provides a representative scenario for specialization and inlining stages.
    /// </summary>
    [Fact]
    public void Expression_multiRecordAccess()
    {
        var parserFastModuleText = s_expressionParserFastModuleText;

        var elmSyntaxRangeModuleText =
            """"
            module Elm.Syntax.Range exposing (..)


            type alias Location =
                { row : Int
                , column : Int
                }


            type alias Range =
                { start : Location
                , end : Location
                }
            """";

        var elmSyntaxNodeModuleText =
            """"
            module Elm.Syntax.Node exposing (..)

            import Elm.Syntax.Range exposing (Range)


            type Node a
                = Node Range a
            """";

        // Verbatim from elm-syntax/src/Elm/Parser/Tokens.elm
        var tokensModuleText =
            """"
            module Elm.Parser.Tokens exposing (..)

            import Elm.Syntax.Node exposing (Node(..))
            import Elm.Syntax.Range exposing (Range)
            import ParserFast


            isAlphaNumOrUnderscore : Char -> Bool
            isAlphaNumOrUnderscore c =
                Char.isAlphaNum c || c == '_'


            isNotReserved : String -> Bool
            isNotReserved name =
                case name of
                    "module" ->
                        False

                    "exposing" ->
                        False

                    "import" ->
                        False

                    "as" ->
                        False

                    "if" ->
                        False

                    "then" ->
                        False

                    "else" ->
                        False

                    "let" ->
                        False

                    "in" ->
                        False

                    "case" ->
                        False

                    "of" ->
                        False

                    "port" ->
                        False

                    "type" ->
                        False

                    "where" ->
                        False

                    _ ->
                        True


            functionNameNode : ParserFast.Parser (Node String)
            functionNameNode =
                ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Node
                    Char.isLower
                    isAlphaNumOrUnderscore
                    isNotReserved
            """";

        // Verbatim from elm-syntax/src/Elm/Parser/Expression.elm
        var expressionModuleText =
            """"
            module Elm.Parser.Expression exposing (..)

            import Elm.Parser.Tokens as Tokens
            import Elm.Syntax.Node exposing (Node(..))
            import Elm.Syntax.Range exposing (Range)
            import ParserFast exposing (Parser)


            multiRecordAccess : ParserFast.Parser (List (Node String))
            multiRecordAccess =
                ParserFast.loopWhileSucceeds
                    (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
                    []
                    (::)
                    List.reverse
            """";

        var expressionModule =
            InliningTestHelper.CanonicalizeAndInlineAndLowerOperatorsAndGetSingleModule(
                [
                parserFastModuleText,
                elmSyntaxRangeModuleText,
                elmSyntaxNodeModuleText,
                s_moduleCharText,
                tokensModuleText,
                expressionModuleText,
                ],
                ["Elm", "Parser", "Expression"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(expressionModule);

        var expectedExpressionModuleText =
            """"
            Elm.Parser.Expression.loopWhileSucceedsHelp__specialized__1 ((ParserFast.Parser parseElement) as element) soFar reduce s0 =
                case parseElement s0 of
                    ParserFast.Good elementResult s1 ->
                        Elm.Parser.Expression.loopWhileSucceedsHelp__specialized__1
                            element
                            (reduce elementResult
                                soFar
                            )
                            reduce
                            s1

                    ParserFast.Bad elementCommitted x ->
                        if elementCommitted then
                            ParserFast.Bad
                                Basics.True
                                x

                        else
                            ParserFast.Good
                                (List.reverse
                                    soFar
                                )
                                s0


            Elm.Parser.Expression.multiRecordAccess : ParserFast.Parser List.List Elm.Syntax.Node.Node String
            Elm.Parser.Expression.multiRecordAccess =
                ParserFast.Parser
                    Elm.Parser.Expression.multiRecordAccess__lifted__lambda1


            Elm.Parser.Expression.multiRecordAccess__lifted__lambda1 s =
                Elm.Parser.Expression.loopWhileSucceedsHelp__specialized__1
                    (ParserFast.Parser
                        (ParserFast.symbolFollowedByParser
                            "."
                            Elm.Parser.Tokens.functionNameNode
                        )
                    )
                    []
                    (::)
                    s
            """";

        rendered.Trim().Should().Be(
            expectedExpressionModuleText.Trim());
    }

    private static readonly string s_expressionParserFastModuleText =
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
                Int
                Int
                Int
                Int
                Int


        type String
            = String Int


        oneOf2MapWithStartRowColumnAndEndRowColumn :
            (Int -> Int -> first -> Int -> Int -> choice)
            -> Parser first
            -> (Int -> Int -> second -> Int -> Int -> choice)
            -> Parser second
            -> Parser choice
        oneOf2MapWithStartRowColumnAndEndRowColumn firstToChoice (Parser attemptFirst) secondToChoice (Parser attemptSecond) =
            Parser
                (\s ->
                    let
                        (PState _ _ _ sRow sCol) =
                            s
                    in
                    case attemptFirst s of
                        Good first s1 ->
                            let
                                (PState _ _ _ s1Row s1Col) =
                                    s1
                            in
                            Good
                                (firstToChoice sRow sCol first s1Row s1Col)
                                s1

                        Bad firstCommitted firstX ->
                            if firstCommitted then
                                Bad firstCommitted firstX

                            else
                                case attemptSecond s of
                                    Good second s1 ->
                                        let
                                            (PState _ _ _ s1Row s1Col) =
                                                s1
                                        in
                                        Good
                                            (secondToChoice sRow sCol second s1Row s1Col)
                                            s1

                                    Bad secondCommitted secondX ->
                                        if secondCommitted then
                                            Bad secondCommitted secondX

                                        else
                                            Bad False (ExpectingOneOf firstX secondX [])
                )


        oneOf7 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
        oneOf7 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) =
            Parser
                (\s ->
                    case attempt0 s of
                        (Good _ _) as good ->
                            good

                        (Bad committed0 x0) as bad0 ->
                            if committed0 then
                                bad0

                            else
                                case attempt1 s of
                                    (Good _ _) as good ->
                                        good

                                    (Bad committed1 x1) as bad1 ->
                                        if committed1 then
                                            bad1

                                        else
                                            case attempt2 s of
                                                (Good _ _) as good ->
                                                    good

                                                (Bad committed2 x2) as bad2 ->
                                                    if committed2 then
                                                        bad2

                                                    else
                                                        case attempt3 s of
                                                            (Good _ _) as good ->
                                                                good

                                                            (Bad committed3 x3) as bad3 ->
                                                                if committed3 then
                                                                    bad3

                                                                else
                                                                    case attempt4 s of
                                                                        (Good _ _) as good ->
                                                                            good

                                                                        (Bad committed4 x4) as bad4 ->
                                                                            if committed4 then
                                                                                bad4

                                                                            else
                                                                                case attempt5 s of
                                                                                    (Good _ _) as good ->
                                                                                        good

                                                                                    (Bad committed5 x5) as bad5 ->
                                                                                        if committed5 then
                                                                                            bad5

                                                                                        else
                                                                                            case attempt6 s of
                                                                                                (Good _ _) as good ->
                                                                                                    good

                                                                                                (Bad committed6 x6) as bad6 ->
                                                                                                    if committed6 then
                                                                                                        bad6

                                                                                                    else
                                                                                                        Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6 ])
                )


        loopWhileSucceeds : Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser res
        loopWhileSucceeds element initialFolded reduce foldedToRes =
            Parser
                (\s -> loopWhileSucceedsHelp element initialFolded reduce foldedToRes s)


        loopWhileSucceedsHelp : Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep res
        loopWhileSucceedsHelp ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
            case parseElement s0 of
                Good elementResult s1 ->
                    loopWhileSucceedsHelp
                        element
                        (soFar |> reduce elementResult)
                        reduce
                        foldedToRes
                        s1

                Bad elementCommitted x ->
                    if elementCommitted then
                        Bad True x

                    else
                        Good (foldedToRes soFar) s0


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


        followedBySymbol : String -> Parser a -> Parser a
        followedBySymbol ((String strBytes) as str) (Parser parsePrevious) =
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
                (\s0 ->
                    case parsePrevious s0 of
                        Good res (PState s1SrcBytes s1Offset s1Indent s1Row s1Col) ->
                            let
                                s1OffsetInt : Int
                                s1OffsetInt =
                                    s1Offset

                                s1ColInt : Int
                                s1ColInt =
                                    s1Col
                            in
                            if
                                Pine_kernel.equal
                                    [ Pine_kernel.take [ strBytesLength, Pine_kernel.skip [ s1OffsetInt, s1SrcBytes ] ]
                                    , strBytes
                                    ]
                            then
                                Good res
                                    (PState
                                        s1SrcBytes
                                        (s1OffsetInt + strBytesLength)
                                        s1Indent
                                        s1Row
                                        (s1ColInt + strLength)
                                    )

                            else
                                Bad True (ExpectingSymbol s1Row s1Col str)

                        bad ->
                            bad
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


        pStepCommit : PStep a -> PStep a
        pStepCommit pStep =
            case pStep of
                (Good _ _) as good ->
                    good

                Bad _ x ->
                    Bad True x


        anyChar : Parser Char
        anyChar =
            Parser
                (\(PState sSrcBytes sOffsetBytes sIndent sRow sCol) ->
                    let
                        sOffsetBytesInt : Int
                        sOffsetBytesInt =
                            sOffsetBytes

                        newOffset : Int
                        newOffset =
                            charOrEnd sOffsetBytesInt sSrcBytes

                        sColInt : Int
                        sColInt =
                            sCol

                        sRowInt : Int
                        sRowInt =
                            sRow
                    in
                    if newOffset == -1 then
                        Bad False (ExpectingAnyChar sRow sCol)

                    else if newOffset == -2 then
                        Good '\n'
                            (PState
                                sSrcBytes
                                (sOffsetBytesInt + 4)
                                sIndent
                                (sRowInt + 1)
                                1
                            )

                    else
                        let
                            foundChar : Char
                            foundChar =
                                Pine_kernel.take [ 4, Pine_kernel.skip [ sOffsetBytesInt, sSrcBytes ] ]
                        in
                        if Pine_kernel.equal [ Pine_kernel.length foundChar, 0 ] then
                            Bad False (ExpectingAnyChar sRowInt sCol)

                        else
                            Good foundChar
                                (PState
                                    sSrcBytes
                                    newOffset
                                    sIndent
                                    sRowInt
                                    (sColInt + 1)
                                )
                )


        charOrEnd : Int -> Int -> Int
        charOrEnd offsetBytes stringBytes =
            let
                nextCharBytes : Int
                nextCharBytes =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offsetBytes, stringBytes ]
                        ]
            in
            if Pine_kernel.equal [ nextCharBytes, '\n' ] then
                -2

            else if Pine_kernel.equal [ Pine_kernel.length nextCharBytes, 0 ] then
                -1

            else
                offsetBytes + 4


        ifFollowedByWhileValidateMapWithRangeWithoutLinebreak :
            (Range -> String -> res)
            -> (Char -> Bool)
            -> (Char -> Bool)
            -> (String -> Bool)
            -> Parser res
        ifFollowedByWhileValidateMapWithRangeWithoutLinebreak toResult firstIsOkay afterFirstIsOkay resultIsOkay =
            Parser
                (\(PState s0SrcBytes s0Offset s0Indent s0Row s0Col) ->
                    let
                        s0ColInt : Int
                        s0ColInt =
                            s0Col

                        nextCharBytes : Int
                        nextCharBytes =
                            Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip [ s0Offset, s0SrcBytes ]
                                ]
                    in
                    if firstIsOkay nextCharBytes then
                        let
                            s1 : State
                            s1 =
                                skipWhileWithoutLinebreakHelp
                                    afterFirstIsOkay
                                    (Pine_kernel.int_add [ s0Offset, 4 ])
                                    s0Row
                                    (Pine_kernel.int_add [ s0ColInt, 1 ])
                                    s0SrcBytes
                                    s0Indent

                            (PState _ s1Offset _ s1Row s1Col) =
                                s1

                            nameSliceBytesLength : Int
                            nameSliceBytesLength =
                                Pine_kernel.int_add
                                    [ s1Offset
                                    , Pine_kernel.int_mul [ -1, s0Offset ]
                                    ]

                            nameSliceBytes : Int
                            nameSliceBytes =
                                Pine_kernel.take
                                    [ nameSliceBytesLength
                                    , Pine_kernel.skip [ s0Offset, s0SrcBytes ]
                                    ]

                            name : String
                            name =
                                String nameSliceBytes
                        in
                        if resultIsOkay name then
                            Good
                                (toResult
                                    { start = { row = s0Row, column = s0Col }
                                    , end = { row = s1Row, column = s1Col }
                                    }
                                    name
                                )
                                s1

                        else
                            Bad False (ExpectingStringSatisfyingPredicate s0Row (Pine_kernel.int_add [ s0ColInt, 1 ]))

                    else
                        Bad False (ExpectingCharSatisfyingPredicate s0Row s0Col)
                )


        ifFollowedByWhileMapWithoutLinebreak :
            (String -> res)
            -> (Char -> Bool)
            -> (Char -> Bool)
            -> Parser res
        ifFollowedByWhileMapWithoutLinebreak consumedStringToRes firstIsOkay afterFirstIsOkay =
            Parser
                (\(PState s0SrcBytes s0Offset s0Indent s0Row s0Col) ->
                    let
                        s0ColInt : Int
                        s0ColInt =
                            s0Col

                        firstOffset : Int
                        firstOffset =
                            isSubCharWithoutLinebreak firstIsOkay s0Offset s0SrcBytes
                    in
                    if firstOffset == -1 then
                        Bad False (ExpectingCharSatisfyingPredicate s0Row s0Col)

                    else
                        let
                            s1 : State
                            s1 =
                                skipWhileWithoutLinebreakHelp
                                    afterFirstIsOkay
                                    firstOffset
                                    s0Row
                                    (Pine_kernel.int_add [ s0ColInt, 1 ])
                                    s0SrcBytes
                                    s0Indent

                            (PState _ s1Offset _ _ _) =
                                s1

                            s1OffsetInt : Int
                            s1OffsetInt =
                                s1Offset

                            consumedCharsBytesLength : Int
                            consumedCharsBytesLength =
                                Pine_kernel.int_add
                                    [ s1OffsetInt
                                    , Pine_kernel.int_mul [ -1, s0Offset ]
                                    ]

                            consumedCharsBytes : Int
                            consumedCharsBytes =
                                Pine_kernel.take
                                    [ consumedCharsBytesLength
                                    , Pine_kernel.skip [ s0Offset, s0SrcBytes ]
                                    ]

                            consumedString : String
                            consumedString =
                                String consumedCharsBytes
                        in
                        Good
                            (consumedStringToRes consumedString)
                            s1
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


        isSubCharWithoutLinebreak : (Char -> Bool) -> Int -> Int -> Int
        isSubCharWithoutLinebreak predicate offsetBytes stringBytes =
            let
                nextCharBytes : Int
                nextCharBytes =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offsetBytes, stringBytes ]
                        ]
            in
            if predicate nextCharBytes then
                offsetBytes + 4

            else
                -1
        """";

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
