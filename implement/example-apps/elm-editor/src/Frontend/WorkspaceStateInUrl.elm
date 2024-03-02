module Frontend.WorkspaceStateInUrl exposing (..)

import Base64
import Bytes
import Bytes.Encode
import Common
import CompilationInterface.GenerateJsonConverters
import FileTreeInWorkspace
import Flate
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import LZ77
import Maybe.Extra
import SHA256
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query
import WorkspaceState_2021_01


type WorkspaceStateDescriptionInUrl
    = LiteralWorkspaceState FrontendBackendInterface.FileTreeNode
    | LinkWorkspaceState String
    | DiffWorkspaceState_Version_2021_01 WorkspaceState_2021_01.WorkspaceState


workspaceStateQueryParameterName : String
workspaceStateQueryParameterName =
    "workspace-state"


workspaceStateQueryParameterName_Old : String
workspaceStateQueryParameterName_Old =
    "project-state"


workspaceStateDeflateBase64QueryParameterName : String
workspaceStateDeflateBase64QueryParameterName =
    "workspace-state-deflate-base64"


workspaceStateDeflateBase64QueryParameterName_Old : String
workspaceStateDeflateBase64QueryParameterName_Old =
    "project-state-deflate-base64"


workspaceStateHashQueryParameterName : String
workspaceStateHashQueryParameterName =
    "workspace-hash"


filePathToOpenQueryParameterName : String
filePathToOpenQueryParameterName =
    "file-path-to-open"


setWorkspaceStateInUrl : FileTreeInWorkspace.FileTreeNode -> Maybe { r | urlInCommit : String, fileTree : FileTreeInWorkspace.FileTreeNode } -> { filePathToOpen : Maybe (List String) } -> Url.Url -> Url.Url
setWorkspaceStateInUrl workspaceState maybeWorkspaceStateBase optionalParameters url =
    let
        workspaceStateJustBytes =
            FileTreeInWorkspace.mapBlobsToBytes workspaceState

        workspaceStateDescription =
            case maybeWorkspaceStateBase of
                Nothing ->
                    LiteralWorkspaceState workspaceStateJustBytes

                Just workspaceStateBase ->
                    if workspaceStateCompositionHash workspaceStateJustBytes == workspaceStateCompositionHash (FileTreeInWorkspace.mapBlobsToBytes workspaceStateBase.fileTree) then
                        LinkWorkspaceState workspaceStateBase.urlInCommit

                    else
                        case
                            FileTreeInWorkspace.searchWorkspaceStateDifference_2021_01
                                workspaceState
                                { baseComposition = workspaceStateBase.fileTree }
                        of
                            Err _ ->
                                LiteralWorkspaceState workspaceStateJustBytes

                            Ok diffModel ->
                                DiffWorkspaceState_Version_2021_01
                                    { base = workspaceStateBase.urlInCommit, differenceFromBase = diffModel }

        ( workspaceStateString, applyCompression ) =
            case workspaceStateDescription of
                LinkWorkspaceState link ->
                    ( link, False )

                _ ->
                    ( Json.Encode.encode 0 (jsonEncodeWorkspaceStateDescription workspaceStateDescription), True )

        workspaceStateQueryParameter =
            if applyCompression then
                let
                    workspaceStateStringBytes =
                        workspaceStateString
                            |> Bytes.Encode.string
                            |> Bytes.Encode.encode

                    compression =
                        Flate.WithWindowSize LZ77.maxWindowSize

                    deflatedWithStatic =
                        Flate.deflateWithOptions (Flate.Static compression) workspaceStateStringBytes

                    deflatedWithDynamic =
                        Flate.deflateWithOptions (Flate.Dynamic compression) workspaceStateStringBytes

                    deflatedBytes =
                        [ deflatedWithStatic, deflatedWithDynamic ]
                            |> List.sortBy Bytes.width
                            |> List.head
                            |> Maybe.withDefault deflatedWithStatic
                in
                Url.Builder.string
                    workspaceStateDeflateBase64QueryParameterName
                    (deflatedBytes |> Base64.fromBytes |> Maybe.withDefault "Failed to encode as base64")

            else
                Url.Builder.string workspaceStateQueryParameterName workspaceStateString

        optionalQueryParameters =
            case optionalParameters.filePathToOpen of
                Nothing ->
                    []

                Just filePathToOpen ->
                    [ Url.Builder.string filePathToOpenQueryParameterName (String.join "/" filePathToOpen) ]
    in
    Url.Builder.crossOrigin
        (Url.toString { url | path = "", query = Nothing, fragment = Nothing })
        []
        (workspaceStateQueryParameter
            :: Url.Builder.string workspaceStateHashQueryParameterName
                (workspaceStateCompositionHash workspaceStateJustBytes)
            :: optionalQueryParameters
        )
        |> Url.fromString
        |> Maybe.withDefault { url | host = "Error: Failed to parse URL from String" }


workspaceStateDescriptionFromUrl : Url.Url -> Maybe (Result String WorkspaceStateDescriptionInUrl)
workspaceStateDescriptionFromUrl url =
    let
        stringFromQueryParameterName paramName =
            { url | path = "" }
                |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string paramName)
                |> Maybe.Extra.join

        continueWithWorkspaceStateString workspaceStateString =
            if workspaceStateStringQualifiesAsLink workspaceStateString then
                Ok (LinkWorkspaceState workspaceStateString)

            else
                Json.Decode.decodeString jsonDecodeWorkspaceStateDescription workspaceStateString
                    |> Result.mapError Json.Decode.errorToString
    in
    case
        stringFromQueryParameterName workspaceStateQueryParameterName
            |> Maybe.Extra.orElse (stringFromQueryParameterName workspaceStateQueryParameterName_Old)
    of
        Just workspaceStateString ->
            if workspaceStateStringQualifiesAsLink workspaceStateString then
                Just (Ok (LinkWorkspaceState workspaceStateString))

            else
                Just (continueWithWorkspaceStateString workspaceStateString)

        Nothing ->
            case
                stringFromQueryParameterName workspaceStateDeflateBase64QueryParameterName
                    |> Maybe.Extra.orElse
                        (stringFromQueryParameterName workspaceStateDeflateBase64QueryParameterName_Old)
            of
                Nothing ->
                    Nothing

                Just workspaceStateDeflateBase64 ->
                    workspaceStateDeflateBase64
                        |> Base64.toBytes
                        |> Result.fromMaybe "Failed to decode from base64"
                        |> Result.andThen (Flate.inflate >> Result.fromMaybe "Failed to inflate")
                        |> Result.andThen (Common.decodeBytesToString >> Result.fromMaybe "Failed to decode bytes to string")
                        |> Result.andThen continueWithWorkspaceStateString
                        |> Just


workspaceStateStringQualifiesAsLink : String -> Bool
workspaceStateStringQualifiesAsLink workspaceStateString =
    String.startsWith "https://" workspaceStateString


filePathToOpenFromUrl : Url.Url -> Maybe String
filePathToOpenFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string filePathToOpenQueryParameterName)
        |> Maybe.Extra.join


workspaceStateHashFromUrl : Url.Url -> Maybe String
workspaceStateHashFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string workspaceStateHashQueryParameterName)
        |> Maybe.Extra.join


workspaceStateCompositionHash : FrontendBackendInterface.FileTreeNode -> String
workspaceStateCompositionHash =
    FileTreeInWorkspace.compositionHashFromFileTreeNodeBytes >> SHA256.toHex


jsonEncodeWorkspaceStateDescription : WorkspaceStateDescriptionInUrl -> Json.Encode.Value
jsonEncodeWorkspaceStateDescription workspaceStateDescription =
    case workspaceStateDescription of
        LiteralWorkspaceState literalWorkspaceState ->
            Json.Encode.object
                [ ( "version_2020_12"
                  , CompilationInterface.GenerateJsonConverters.jsonEncodeFileTreeNode literalWorkspaceState
                  )
                ]

        LinkWorkspaceState link ->
            Json.Encode.string link

        DiffWorkspaceState_Version_2021_01 diffWorkspaceState ->
            Json.Encode.object
                [ ( "version_2021_01"
                  , CompilationInterface.GenerateJsonConverters.jsonEncodeWorkspaceState_2021_01 diffWorkspaceState
                  )
                ]


jsonDecodeWorkspaceStateDescription : Json.Decode.Decoder WorkspaceStateDescriptionInUrl
jsonDecodeWorkspaceStateDescription =
    Json.Decode.oneOf
        [ Json.Decode.field "version_2020_12"
            CompilationInterface.GenerateJsonConverters.jsonDecodeFileTreeNode
            |> Json.Decode.map LiteralWorkspaceState
        , Json.Decode.field "version_2021_01"
            CompilationInterface.GenerateJsonConverters.jsonDecodeWorkspaceState_2021_01
            |> Json.Decode.map DiffWorkspaceState_Version_2021_01
        ]


workspaceStateStringFromUrl : Url.Url -> Maybe String
workspaceStateStringFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.oneOf
                [ Url.Parser.top <?> Url.Parser.Query.string workspaceStateQueryParameterName
                , Url.Parser.top <?> Url.Parser.Query.string workspaceStateQueryParameterName_Old
                ]
            )
        |> Maybe.Extra.join
