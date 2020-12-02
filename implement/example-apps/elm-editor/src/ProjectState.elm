module ProjectState exposing (..)

import Json.Encode


type alias ProjectState =
    ProjectState_2020_12


type alias ProjectState_2020_12 =
    List { filePath : List String, fileContentText : String }
