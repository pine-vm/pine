module Time exposing
    ( Month(..)
    , Posix
    , Weekday(..)
    , Zone
    , millisToPosix
    , posixToMillis
    , toMillis
    , utc
    )

import Dict


type Posix
    = Posix Int


posixToMillis : Posix -> Int
posixToMillis (Posix millis) =
    millis


millisToPosix : Int -> Posix
millisToPosix =
    Posix


type Zone
    = Zone Int (List Era)


type alias Era =
    { start : Int
    , offset : Int
    }


utc : Zone
utc =
    Zone 0 []



-- DATES


toYear : Zone -> Posix -> Int
toYear zone time =
    (toCivil (toAdjustedMinutes zone time)).year


toMonth : Zone -> Posix -> Month
toMonth zone time =
    case (toCivil (toAdjustedMinutes zone time)).month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


toMillis : Zone -> Posix -> Int
toMillis _ time =
    modBy 1000 (posixToMillis time)


type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
