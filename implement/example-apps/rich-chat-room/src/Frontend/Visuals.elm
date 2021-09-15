module Frontend.Visuals exposing (..)

import Derberos.Date.Delta
import Html
import Html.Attributes as HA
import Http
import Time


type RelativeDayName
    = Today
    | Yesterday


type alias HtmlStyle =
    List ( String, String )


button : List (Html.Attribute a) -> List (Html.Html a) -> Html.Html a
button attributes =
    Html.button (attributes ++ [ HA.style "pointer-events" "all", HA.style "cursor" "pointer" ])


calendarDayTextFromPosixTime : Time.Posix -> String
calendarDayTextFromPosixTime time =
    let
        monthIndex =
            time |> Time.toMonth Time.utc |> calendarMonthIndex
    in
    [ time |> Time.toYear Time.utc, monthIndex, time |> Time.toDay Time.utc ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join "-"


getRelativeDayName : { currentTime : Time.Posix, namedTime : Time.Posix } -> Maybe RelativeDayName
getRelativeDayName { currentTime, namedTime } =
    if posixTimesAreFromSameDay currentTime namedTime then
        Just Today

    else if posixTimesAreFromSameDay currentTime (namedTime |> Derberos.Date.Delta.addDays 1) then
        Just Yesterday

    else
        Nothing


posixTimesAreFromSameDay : Time.Posix -> Time.Posix -> Bool
posixTimesAreFromSameDay timeA timeB =
    [ Time.toYear Time.utc, Time.toMonth Time.utc >> calendarMonthIndex, Time.toDay Time.utc ]
        |> List.all (\getComponent -> (timeA |> getComponent) == (timeB |> getComponent))


calendarMonthIndex : Time.Month -> Int
calendarMonthIndex month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


describeHttpError : Http.Error -> String
describeHttpError httpError =
    case httpError of
        Http.BadUrl errorMessage ->
            "Bad Url: " ++ errorMessage

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "BadStatus: " ++ (statusCode |> String.fromInt)

        Http.BadBody errorMessage ->
            "BadPayload: " ++ errorMessage


globalStylesHtmlElement : Html.Html a
globalStylesHtmlElement =
    """
body {
font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
margin: 0;
background: #111;
color: whitesmoke;
}

a:link {
  color: whitesmoke;
}

a:visited {
  color: whitesmoke;
}

a:hover {
  color: whitesmoke;
}

a:active {
  color: whitesmoke;
}

"""
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


htmlAttributesStyles : List ( String, String ) -> List (Html.Attribute msg)
htmlAttributesStyles =
    List.map (\( property, value ) -> HA.style property value)
