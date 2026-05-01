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


type alias Civil =
    { year : Int
    , month : Int
    , day : Int
    }


toAdjustedMinutes : Zone -> Posix -> Int
toAdjustedMinutes (Zone defaultOffset eras) time =
    (posixToMillis time // 60000) + offsetForMillis defaultOffset eras (posixToMillis time)


offsetForMillis : Int -> List Era -> Int -> Int
offsetForMillis defaultOffset eras millis =
    case eras of
        [] ->
            defaultOffset

        era :: earlierEras ->
            if era.start <= millis then
                era.offset

            else
                offsetForMillis defaultOffset earlierEras millis


toCivil : Int -> Civil
toCivil adjustedMinutes =
    civilFromDays (adjustedMinutes // minutesPerDay)


minutesPerDay : Int
minutesPerDay =
    24 * 60


civilFromDays : Int -> Civil
civilFromDays days =
    if days < 0 then
        civilFromPreviousYear 1969 (days + daysInYear 1969)

    else
        civilFromYear 1970 days


civilFromPreviousYear : Int -> Int -> Civil
civilFromPreviousYear year dayOfYear =
    -- This path is only used for UTC dates before 1970 in current tests and expected callers.
    -- It walks one year per recursive step; optimize this if broad historical date support is needed.
    if dayOfYear < 0 then
        civilFromPreviousYear (year - 1) (dayOfYear + daysInYear (year - 1))

    else
        civilFromMonth year 1 dayOfYear


civilFromYear : Int -> Int -> Civil
civilFromYear year days =
    let
        yearDays =
            daysInYear year
    in
    if yearDays <= days then
        civilFromYear (year + 1) (days - yearDays)

    else
        civilFromMonth year 1 days


civilFromMonth : Int -> Int -> Int -> Civil
civilFromMonth year month days =
    let
        monthDays =
            daysInMonth year month
    in
    if monthDays <= days then
        civilFromMonth year (month + 1) (days - monthDays)

    else
        { year = year, month = month, day = days + 1 }


daysInYear : Int -> Int
daysInYear year =
    if isLeapYear year then
        366

    else
        365


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if isLeapYear year then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 400 year == 0) || ((modBy 4 year == 0) && (modBy 100 year /= 0))


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
