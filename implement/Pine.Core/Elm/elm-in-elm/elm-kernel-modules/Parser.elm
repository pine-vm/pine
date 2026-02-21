module Parser exposing
    ( (|.)
    , (|=)
    , DeadEnd
    , Nestable(..)
    , Parser
    , Problem(..)
    , Step(..)
    , Trailing(..)
    , andThen
    , backtrackable
    , chompIf
    , chompUntil
    , chompUntilEndOr
    , chompWhile
    , commit
    , deadEndsToString
    , end
    , float
    , getChompedString
    , getCol
    , getIndent
    , getOffset
    , getPosition
    , getRow
    , getSource
    , int
    , keyword
    , lazy
    , lineComment
    , loop
    , map
    , mapChompedString
    , multiComment
    , number
    , oneOf
    , problem
    , run
    , sequence
    , spaces
    , succeed
    , symbol
    , token
    , variable
    , withIndent
    )

import Parser.Advanced as A exposing ((|.), (|=))
import Set


infix left  5 (|=) = keeper
infix left  6 (|.) = ignorer


type alias Parser a =
    A.Parser Never Problem a


run : Parser a -> String -> Result (List DeadEnd) a
run parser source =
    case A.run parser source of
        Ok a ->
            Ok a

        Err problems ->
            Err (List.map problemToDeadEnd problems)


problemToDeadEnd : A.DeadEnd Never Problem -> DeadEnd
problemToDeadEnd p =
    DeadEnd p.row p.col p.problem


type alias DeadEnd =
    { row : Int
    , col : Int
    , problem : Problem
    }


{-| When you run into a `DeadEnd`, I record some information about why you
got stuck. This data is useful for producing helpful error messages. This is
how [`deadEndsToString`](#deadEndsToString) works!

**Note:** If you feel limited by this type (i.e. having to represent custom
problems as strings) I highly recommend switching to `Parser.Advanced`. It
lets you define your own `Problem` type. It can also track "context" which
can improve error messages a ton! This is how the Elm compiler produces
relatively nice parse errors, and I am excited to see those techniques applied
elsewhere!

-}
type Problem
    = Expecting String
    | ExpectingInt
    | ExpectingHex
    | ExpectingOctal
    | ExpectingBinary
    | ExpectingFloat
    | ExpectingNumber
    | ExpectingVariable
    | ExpectingSymbol String
    | ExpectingKeyword String
    | ExpectingEnd
    | UnexpectedChar
    | Problem String
    | BadRepeat


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    "TODO deadEndsToString"


succeed : a -> Parser a
succeed =
    A.succeed


keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parseFunc =
    {-
       Since compiler currently fails to compile the original form, inline here as a workaround.
       (|=)
    -}
    A.keeper parseFunc


ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keepParser ignoreParser =
    {-
       Since compiler currently fails to compile the original form, inline here as a workaround.
       (|.)
    -}
    A.ignorer keepParser ignoreParser


lazy : (() -> Parser a) -> Parser a
lazy =
    A.lazy


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
    A.andThen


problem : String -> Parser a
problem msg =
    A.problem (Problem msg)


oneOf : List (Parser a) -> Parser a
oneOf =
    A.oneOf


map : (a -> b) -> Parser a -> Parser b
map =
    A.map


backtrackable : Parser a -> Parser a
backtrackable =
    A.backtrackable


commit : a -> Parser a
commit =
    A.commit


token : String -> Parser ()
token str =
    A.token (toToken str)


toToken : String -> A.Token Problem
toToken str =
    A.Token str (Expecting str)


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop state callback =
    A.loop state (\s -> map toAdvancedStep (callback s))


type Step state a
    = Loop state
    | Done a


toAdvancedStep : Step s a -> A.Step s a
toAdvancedStep step =
    case step of
        Loop s ->
            A.Loop s

        Done a ->
            A.Done a


int : Parser Int
int =
    A.int ExpectingInt ExpectingInt


float : Parser Float
float =
    A.float ExpectingFloat ExpectingFloat


number :
    { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }
    -> Parser a
number i =
    A.number
        { int = Result.fromMaybe ExpectingInt i.int
        , hex = Result.fromMaybe ExpectingHex i.hex
        , octal = Result.fromMaybe ExpectingOctal i.octal
        , binary = Result.fromMaybe ExpectingBinary i.binary
        , float = Result.fromMaybe ExpectingFloat i.float
        , invalid = ExpectingNumber
        , expecting = ExpectingNumber
        }


symbol : String -> Parser ()
symbol str =
    A.symbol (A.Token str (ExpectingSymbol str))


keyword : String -> Parser ()
keyword kwd =
    A.keyword (A.Token kwd (ExpectingKeyword kwd))


end : Parser ()
end =
    A.end ExpectingEnd


getChompedString : Parser a -> Parser String
getChompedString =
    A.getChompedString


mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
    A.mapChompedString


chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
    A.chompIf isGood UnexpectedChar


chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
    A.chompWhile


chompUntil : String -> Parser ()
chompUntil str =
    A.chompUntil (toToken str)


chompUntilEndOr : String -> Parser ()
chompUntilEndOr =
    A.chompUntilEndOr


withIndent : Int -> Parser a -> Parser a
withIndent newIndent (A.Parser parse) =
    A.withIndent


getIndent : Parser Int
getIndent =
    A.getIndent


getPosition : Parser ( Int, Int )
getPosition =
    A.getPosition


getRow : Parser Int
getRow =
    A.getRow


getCol : Parser Int
getCol =
    A.getCol


getOffset : Parser Int
getOffset =
    A.getOffset


getSource : Parser String
getSource =
    A.getSource


variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set.Set String
    }
    -> Parser String
variable i =
    A.variable
        { start = i.start
        , inner = i.inner
        , reserved = i.reserved
        , expecting = ExpectingVariable
        }


sequence :
    { start : String
    , separator : String
    , end : String
    , spaces : Parser ()
    , item : Parser a
    , trailing : Trailing
    }
    -> Parser (List a)
sequence i =
    A.sequence
        { start = toToken i.start
        , separator = toToken i.separator
        , end = toToken i.end
        , spaces = i.spaces
        , item = i.item
        , trailing = toAdvancedTrailing i.trailing
        }


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


toAdvancedTrailing : Trailing -> A.Trailing
toAdvancedTrailing trailing =
    case trailing of
        Forbidden ->
            A.Forbidden

        Optional ->
            A.Optional

        Mandatory ->
            A.Mandatory


spaces : Parser ()
spaces =
    A.spaces


lineComment : String -> Parser ()
lineComment str =
    A.lineComment (toToken str)


multiComment : String -> String -> Nestable -> Parser ()
multiComment open close nestable =
    A.multiComment (toToken open) (toToken close) (toAdvancedNestable nestable)


type Nestable
    = NotNestable
    | Nestable


toAdvancedNestable : Nestable -> A.Nestable
toAdvancedNestable nestable =
    case nestable of
        NotNestable ->
            A.NotNestable

        Nestable ->
            A.Nestable
