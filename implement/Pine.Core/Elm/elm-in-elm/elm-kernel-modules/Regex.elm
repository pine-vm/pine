module Regex exposing
    ( Regex, Options, Match
    , fromString, fromStringWith, never
    , contains, find, findAtMost
    , replace, replaceAtMost, split, splitAtMost
    )

{-| Portable regular expressions evaluated entirely as Pine expressions.

Supported syntax includes literals, `.`, `^`, `$`, character classes and ranges,
negated classes, `\d`, `\D`, `\s`, `\S`, `\w`, `\W`, escaped punctuation,
capturing and non-capturing groups, alternatives (including empty alternatives),
and greedy or reluctant `*`, `+`, and `?` quantifiers.

Unsupported syntax or options return `Nothing`. Counted repetitions,
lookarounds, backreferences, and case-insensitive matching are not supported.
Matching and indices use Unicode scalar values, not UTF-16 code units.

@docs Regex, Options, Match
@docs fromString, fromStringWith, never
@docs contains, find, findAtMost
@docs replace, replaceAtMost, split, splitAtMost
-}

import Char
import List
import Maybe
import String


{-| An opaque, serializable parsed regular expression.
-}
type Regex
    = Regex Options Pattern Int


{-| Multiline mode makes anchors recognize line boundaries. Case-insensitive
mode is currently unsupported.
-}
type alias Options =
    { caseInsensitive : Bool
    , multiline : Bool
    }


{-| Match indices count Unicode scalar values. Match numbers start at one.
As in elm/regex 1.0.0, absent and empty captures are both `Nothing`.
-}
type alias Match =
    { match : String
    , index : Int
    , number : Int
    , submatches : List (Maybe String)
    }


type Pattern
    = Sequence (List Pattern)
    | Alternatives (List Pattern)
    | CharacterClass Bool (List ClassItem)
    | AnyCharacter
    | Start
    | End
    | Capture Int Pattern
    | Repeat Int (Maybe Int) Bool Pattern
    | Never


type ClassItem
    = Range Int Int
    | Digit
    | WhiteSpace
    | Word
    | Complement ClassItem


type alias ParseState =
    { remaining : List Char
    , nextCapture : Int
    }


type alias Cursor =
    { remaining : List Char
    , position : Int
    , previous : Maybe Char
    , captures : List ( Int, ( Int, Int ) )
    }


{-| Parse a case-sensitive regular expression.
-}
fromString : String -> Maybe Regex
fromString =
    fromStringWith { caseInsensitive = False, multiline = False }


{-| Parse a regular expression with options. Invalid and unsupported patterns
both return `Nothing`.
-}
fromStringWith : Options -> String -> Maybe Regex
fromStringWith options source =
    if options.caseInsensitive then
        Nothing

    else
        parseAlternatives { remaining = String.toList source, nextCapture = 1 }
            |> Maybe.andThen
                (\( pattern, state ) ->
                    if List.isEmpty state.remaining then
                        Just (Regex options pattern (state.nextCapture - 1))

                    else
                        Nothing
                )


{-| A regular expression that never matches.
-}
never : Regex
never =
    Regex { caseInsensitive = False, multiline = False } Never 0


parseAlternatives : ParseState -> Maybe ( Pattern, ParseState )
parseAlternatives state =
    parseSequence [] state
        |> Maybe.andThen (\( first, after ) -> parseAlternativesHelp [ first ] after)


parseAlternativesHelp : List Pattern -> ParseState -> Maybe ( Pattern, ParseState )
parseAlternativesHelp reversed state =
    case state.remaining of
        '|' :: rest ->
            parseSequence [] { state | remaining = rest }
                |> Maybe.andThen
                    (\( next, after ) -> parseAlternativesHelp (next :: reversed) after)

        _ ->
            Just ( Alternatives (List.reverse reversed), state )


parseSequence : List Pattern -> ParseState -> Maybe ( Pattern, ParseState )
parseSequence reversed state =
    case state.remaining of
        [] ->
            Just ( Sequence (List.reverse reversed), state )

        ')' :: _ ->
            Just ( Sequence (List.reverse reversed), state )

        '|' :: _ ->
            Just ( Sequence (List.reverse reversed), state )

        _ ->
            parseAtom state
                |> Maybe.andThen (\( atom, after ) -> parseQuantifier atom after)
                |> Maybe.andThen (\( pattern, after ) -> parseSequence (pattern :: reversed) after)


parseAtom : ParseState -> Maybe ( Pattern, ParseState )
parseAtom state =
    case state.remaining of
        [] ->
            Nothing

        '(' :: '?' :: ':' :: rest ->
            parseGroup Nothing { state | remaining = rest }

        '(' :: '?' :: _ ->
            Nothing

        '(' :: rest ->
            parseGroup
                (Just state.nextCapture)
                { remaining = rest, nextCapture = state.nextCapture + 1 }

        '[' :: '^' :: rest ->
            parseClass True [] { state | remaining = rest }

        '[' :: rest ->
            parseClass False [] { state | remaining = rest }

        '\\' :: rest ->
            parseEscape False rest
                |> Maybe.map
                    (\( item, after ) -> ( CharacterClass False [ item ], { state | remaining = after } ))

        char :: rest ->
            if List.member char [ '*', '+', '?', '{', '}', ')' ] then
                Nothing

            else
                let
                    atom =
                        case char of
                            '^' ->
                                Start

                            '$' ->
                                End

                            '.' ->
                                AnyCharacter

                            _ ->
                                CharacterClass False [ literal char ]
                in
                Just ( atom, { state | remaining = rest } )


parseGroup : Maybe Int -> ParseState -> Maybe ( Pattern, ParseState )
parseGroup capture state =
    parseAlternatives state
        |> Maybe.andThen
            (\( pattern, after ) ->
                case after.remaining of
                    ')' :: rest ->
                        Just
                            ( case capture of
                                Just index ->
                                    Capture index pattern

                                Nothing ->
                                    pattern
                            , { after | remaining = rest }
                            )

                    _ ->
                        Nothing
            )


parseQuantifier : Pattern -> ParseState -> Maybe ( Pattern, ParseState )
parseQuantifier atom state =
    case state.remaining of
        '*' :: rest ->
            quantified atom 0 Nothing rest state

        '+' :: rest ->
            quantified atom 1 Nothing rest state

        '?' :: rest ->
            quantified atom 0 (Just 1) rest state

        _ ->
            Just ( atom, state )


quantified : Pattern -> Int -> Maybe Int -> List Char -> ParseState -> Maybe ( Pattern, ParseState )
quantified atom minimum maximum rest state =
    case atom of
        Start ->
            Nothing

        End ->
            Nothing

        _ ->
            case rest of
                '?' :: after ->
                    Just ( Repeat minimum maximum False atom, { state | remaining = after } )

                _ ->
                    Just ( Repeat minimum maximum True atom, { state | remaining = rest } )


literal : Char -> ClassItem
literal char =
    Range (Char.toCode char) (Char.toCode char)


parseEscape : Bool -> List Char -> Maybe ( ClassItem, List Char )
parseEscape inClass chars =
    case chars of
        [] ->
            Nothing

        char :: rest ->
            let
                done item =
                    Just ( item, rest )
            in
            case char of
                'd' ->
                    done Digit

                'D' ->
                    done (Complement Digit)

                's' ->
                    done WhiteSpace

                'S' ->
                    done (Complement WhiteSpace)

                'w' ->
                    done Word

                'W' ->
                    done (Complement Word)

                'n' ->
                    done (Range 10 10)

                'r' ->
                    done (Range 13 13)

                't' ->
                    done (Range 9 9)

                'v' ->
                    done (Range 11 11)

                'f' ->
                    done (Range 12 12)

                'b' ->
                    if inClass then
                        done (Range 8 8)

                    else
                        Nothing

                '0' ->
                    if List.head rest |> Maybe.map Char.isDigit |> Maybe.withDefault False then
                        Nothing

                    else
                        done (Range 0 0)

                _ ->
                    if Char.isAlphaNum char then
                        Nothing

                    else
                        done (literal char)


parseClass : Bool -> List ClassItem -> ParseState -> Maybe ( Pattern, ParseState )
parseClass negated reversed state =
    case state.remaining of
        [] ->
            Nothing

        ']' :: rest ->
            Just ( CharacterClass negated (List.reverse reversed), { state | remaining = rest } )

        _ ->
            parseClassItem state.remaining
                |> Maybe.andThen
                    (\( first, rest ) ->
                        case rest of
                            '-' :: ']' :: _ ->
                                parseClass negated (first :: reversed) { state | remaining = rest }

                            '-' :: afterDash ->
                                parseClassItem afterDash
                                    |> Maybe.andThen
                                        (\( last, after ) ->
                                            case ( first, last ) of
                                                ( Range low _, Range _ high ) ->
                                                    if low <= high then
                                                        parseClass negated (Range low high :: reversed) { state | remaining = after }

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )

                            _ ->
                                parseClass negated (first :: reversed) { state | remaining = rest }
                    )


parseClassItem : List Char -> Maybe ( ClassItem, List Char )
parseClassItem chars =
    case chars of
        [] ->
            Nothing

        '\\' :: rest ->
            parseEscape True rest

        ']' :: _ ->
            Nothing

        char :: rest ->
            Just ( literal char, rest )


isLineTerminator : Char -> Bool
isLineTerminator char =
    List.member (Char.toCode char) [ 10, 13, 8232, 8233 ]


matchesClassItem : Int -> ClassItem -> Bool
matchesClassItem code item =
    case item of
        Range low high ->
            low <= code && code <= high

        Digit ->
            48 <= code && code <= 57

        WhiteSpace ->
            (8192 <= code && code <= 8202)
                || List.member code [ 9, 10, 11, 12, 13, 32, 160, 5760, 8232, 8233, 8239, 8287, 12288, 65279 ]

        Word ->
            (48 <= code && code <= 57)
                || (65 <= code && code <= 90)
                || (97 <= code && code <= 122)
                || code == 95

        Complement inner ->
            not (matchesClassItem code inner)


advance : Cursor -> Maybe Cursor
advance cursor =
    case cursor.remaining of
        [] ->
            Nothing

        char :: rest ->
            Just { cursor | remaining = rest, position = cursor.position + 1, previous = Just char }


matchPattern : Options -> Pattern -> Cursor -> (Cursor -> Maybe Cursor) -> Maybe Cursor
matchPattern options pattern cursor continue =
    case pattern of
        Never ->
            Nothing

        Sequence patterns ->
            matchSequence options patterns cursor continue

        Alternatives patterns ->
            matchAlternatives options patterns cursor continue

        CharacterClass negated items ->
            case cursor.remaining of
                [] ->
                    Nothing

                char :: _ ->
                    if negated /= List.any (matchesClassItem (Char.toCode char)) items then
                        advance cursor |> Maybe.andThen continue

                    else
                        Nothing

        AnyCharacter ->
            case cursor.remaining of
                [] ->
                    Nothing

                char :: _ ->
                    if isLineTerminator char then
                        Nothing

                    else
                        advance cursor |> Maybe.andThen continue

        Start ->
            if cursor.position == 0 || (options.multiline && Maybe.withDefault False (Maybe.map isLineTerminator cursor.previous)) then
                continue cursor

            else
                Nothing

        End ->
            if List.isEmpty cursor.remaining || (options.multiline && Maybe.withDefault False (Maybe.map isLineTerminator (List.head cursor.remaining))) then
                continue cursor

            else
                Nothing

        Capture index inner ->
            matchPattern options
                inner
                cursor
                (\after ->
                    continue
                        { after
                            | captures =
                                ( index, ( cursor.position, after.position ) )
                                    :: List.filter (\( id, _ ) -> id /= index) after.captures
                        }
                )

        Repeat minimum maximum greedy inner ->
            matchRepeat options inner minimum maximum greedy cursor continue


matchSequence : Options -> List Pattern -> Cursor -> (Cursor -> Maybe Cursor) -> Maybe Cursor
matchSequence options patterns cursor continue =
    case patterns of
        [] ->
            continue cursor

        first :: rest ->
            matchPattern options first cursor (\after -> matchSequence options rest after continue)


matchAlternatives : Options -> List Pattern -> Cursor -> (Cursor -> Maybe Cursor) -> Maybe Cursor
matchAlternatives options patterns cursor continue =
    case patterns of
        [] ->
            Nothing

        first :: rest ->
            case matchPattern options first cursor continue of
                Just result ->
                    Just result

                Nothing ->
                    matchAlternatives options rest cursor continue


captureIds : Pattern -> List Int
captureIds pattern =
    case pattern of
        Capture index inner ->
            index :: captureIds inner

        Sequence patterns ->
            List.concatMap captureIds patterns

        Alternatives patterns ->
            List.concatMap captureIds patterns

        Repeat _ _ _ inner ->
            captureIds inner

        _ ->
            []


matchRepeat : Options -> Pattern -> Int -> Maybe Int -> Bool -> Cursor -> (Cursor -> Maybe Cursor) -> Maybe Cursor
matchRepeat options inner minimum maximum greedy cursor continue =
    let
        stop () =
            if minimum <= 0 then
                continue cursor

            else
                Nothing

        consume () =
            if maximum == Just 0 then
                Nothing

            else
                let
                    ids =
                        captureIds inner

                    cleared =
                        { cursor | captures = List.filter (\( id, _ ) -> not (List.member id ids)) cursor.captures }
                in
                matchPattern options
                    inner
                    cleared
                    (\after ->
                        -- Optional empty iterations must not erase the preceding iteration's captures.
                        if after.position == cursor.position && minimum <= 0 then
                            Nothing

                        else
                            matchRepeat options inner (max 0 (minimum - 1)) (Maybe.map (\n -> n - 1) maximum) greedy after continue
                    )
    in
    if greedy then
        case consume () of
            Just result ->
                Just result

            Nothing ->
                stop ()

    else
        case stop () of
            Just result ->
                Just result

            Nothing ->
                consume ()


search : Regex -> Cursor -> Maybe ( Int, Cursor )
search ((Regex options pattern _) as regex) cursor =
    case matchPattern options pattern { cursor | captures = [] } Just of
        Just after ->
            Just ( cursor.position, after )

        Nothing ->
            advance cursor |> Maybe.andThen (search regex)


initialCursor : String -> Cursor
initialCursor string =
    { remaining = String.toList string, position = 0, previous = Nothing, captures = [] }


{-| Determine whether a string contains a match.
-}
contains : Regex -> String -> Bool
contains regex string =
    case search regex (initialCursor string) of
        Just _ ->
            True

        Nothing ->
            False


{-| Find all non-overlapping matches. As in elm/regex 1.0.0, enumeration stops
if a match ends at the same position as the previous match.
-}
find : Regex -> String -> List Match
find regex string =
    findAtMost (String.length string + 1) regex string


{-| Find at most the given number of matches.
-}
findAtMost : Int -> Regex -> String -> List Match
findAtMost limit regex string =
    collectMatches False limit regex string (initialCursor string) -1 1 []


collectMatches : Bool -> Int -> Regex -> String -> Cursor -> Int -> Int -> List Match -> List Match
collectMatches progress limit ((Regex _ _ captureCount) as regex) string cursor previousEnd number reversed =
    if limit <= 0 then
        List.reverse reversed

    else
        case search regex cursor of
            Nothing ->
                List.reverse reversed

            Just ( start, after ) ->
                if not progress && after.position == previousEnd then
                    List.reverse reversed

                else
                    let
                        capture index =
                            after.captures
                                |> List.filter (\( id, _ ) -> id == index)
                                |> List.head
                                |> Maybe.andThen
                                    (\( _, ( low, high ) ) ->
                                        if low == high then
                                            Nothing

                                        else
                                            Just (String.slice low high string)
                                    )

                        found =
                            { match = String.slice start after.position string
                            , index = start
                            , number = number
                            , submatches = List.map capture (List.range 1 captureCount)
                            }

                        next =
                            if progress && start == after.position then
                                advance after

                            else
                                Just after
                    in
                    case next of
                        Nothing ->
                            List.reverse (found :: reversed)

                        Just nextCursor ->
                            collectMatches progress (limit - 1) regex string nextCursor after.position (number + 1) (found :: reversed)


{-| Replace every match using an Elm callback. Empty matches advance by one
Unicode scalar value, including a final empty match at the end of the string.
-}
replace : Regex -> (Match -> String) -> String -> String
replace regex replacer string =
    replaceAtMost (String.length string + 1) regex replacer string


{-| Replace at most the given number of matches.
-}
replaceAtMost : Int -> Regex -> (Match -> String) -> String -> String
replaceAtMost limit regex replacer string =
    let
        step found ( previousEnd, reversed ) =
            ( found.index + String.length found.match
            , replacer found :: String.slice previousEnd found.index string :: reversed
            )

        ( end, pieces ) =
            List.foldl step ( 0, [] ) (collectMatches True limit regex string (initialCursor string) -1 1 [])
    in
    String.concat (List.reverse (String.dropLeft end string :: pieces))


{-| Split at every match. Unlike the upstream kernel's non-terminating empty
delimiter loop, empty matches advance by one Unicode scalar value.
-}
split : Regex -> String -> List String
split regex string =
    splitAtMost (String.length string + 1) regex string


{-| Split at most the given number of times. Captured delimiters are not included.
As in elm/regex 1.0.0, negative limits behave as unbounded splitting.
-}
splitAtMost : Int -> Regex -> String -> List String
splitAtMost limit regex string =
    let
        effectiveLimit =
            if limit < 0 then
                String.length string + 1

            else
                limit

        step found ( previousEnd, reversed ) =
            ( found.index + String.length found.match
            , String.slice previousEnd found.index string :: reversed
            )

        ( end, pieces ) =
            List.foldl step ( 0, [] ) (collectMatches True effectiveLimit regex string (initialCursor string) -1 1 [])
    in
    List.reverse (String.dropLeft end string :: pieces)
