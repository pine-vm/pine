module ElmMakeExecutableFile exposing (..)

import Json.Decode
import Json.Decode.Extra


type alias ElmMakeReportFromJson =
    { type_ : String
    , errors : Maybe (List ElmMakeReportCompileErrorStructure)
    }


type alias ElmMakeReportCompileErrorStructure =
    { path : String
    , name : String
    , problems : List ElmMakeReportProblem
    }


type alias ElmMakeReportProblem =
    { title : String
    , region : ElmMakeReportRegion
    , message : List ElmMakeReportMessageListItem
    }


type alias ElmMakeReportRegion =
    { start : ElmMakeReportLocation
    , end : ElmMakeReportLocation
    }


type alias ElmMakeReportLocation =
    { line : Int
    , column : Int
    }


type ElmMakeReportMessageListItem
    = ElmMakeReportMessageListItemPlain String
    | ElmMakeReportMessageListItemStyled ElmMakeReportMessageListItemStyledStructure


type alias ElmMakeReportMessageListItemStyledStructure =
    { bold : Bool
    , underline : Bool
    , color : Maybe String
    , string : String
    }


jsonDecodeElmMakeReport : Json.Decode.Decoder ElmMakeReportFromJson
jsonDecodeElmMakeReport =
    Json.Decode.map2 ElmMakeReportFromJson
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.Extra.optionalField "errors" (Json.Decode.list jsonDecodeElmMakeReportCompileError))


jsonDecodeElmMakeReportCompileError : Json.Decode.Decoder ElmMakeReportCompileErrorStructure
jsonDecodeElmMakeReportCompileError =
    Json.Decode.map3 ElmMakeReportCompileErrorStructure
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "problems" (Json.Decode.list jsonDecodeElmMakeReportProblem))


jsonDecodeElmMakeReportProblem : Json.Decode.Decoder ElmMakeReportProblem
jsonDecodeElmMakeReportProblem =
    Json.Decode.map3 ElmMakeReportProblem
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "region" jsonDecodeElmMakeReportRegion)
        (Json.Decode.field "message" (Json.Decode.list jsonDecodeElmMakeReportMessageListItem))


jsonDecodeElmMakeReportMessageListItem : Json.Decode.Decoder ElmMakeReportMessageListItem
jsonDecodeElmMakeReportMessageListItem =
    Json.Decode.oneOf
        [ Json.Decode.string |> Json.Decode.map ElmMakeReportMessageListItemPlain
        , jsonDecodeElmMakeReportMessageListItemStyled |> Json.Decode.map ElmMakeReportMessageListItemStyled
        ]


jsonDecodeElmMakeReportMessageListItemStyled : Json.Decode.Decoder ElmMakeReportMessageListItemStyledStructure
jsonDecodeElmMakeReportMessageListItemStyled =
    Json.Decode.map4 ElmMakeReportMessageListItemStyledStructure
        (Json.Decode.field "bold" Json.Decode.bool)
        (Json.Decode.field "underline" Json.Decode.bool)
        (Json.Decode.field "color" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "string" Json.Decode.string)


jsonDecodeElmMakeReportRegion : Json.Decode.Decoder ElmMakeReportRegion
jsonDecodeElmMakeReportRegion =
    Json.Decode.map2 ElmMakeReportRegion
        (Json.Decode.field "start" jsonDecodeElmMakeReportLocation)
        (Json.Decode.field "end" jsonDecodeElmMakeReportLocation)


jsonDecodeElmMakeReportLocation : Json.Decode.Decoder ElmMakeReportLocation
jsonDecodeElmMakeReportLocation =
    Json.Decode.map2 ElmMakeReportLocation
        (Json.Decode.field "line" Json.Decode.int)
        (Json.Decode.field "column" Json.Decode.int)
