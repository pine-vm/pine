module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.CustomName.SourceFiles
import CompilationInterface.CustomNameBeta.SourceFiles
import CompilationInterface.SourceFiles
import FileTree
import Platform.WebService
import Url


type alias State =
    {}


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( {}, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        response =
            if (httpRequestEvent.request.method |> String.toLower) /= "get" then
                { statusCode = 405
                , bodyAsBase64 = Nothing
                , headersToAdd = []
                }

            else
                case Url.fromString httpRequestEvent.request.uri of
                    Nothing ->
                        { statusCode = 500
                        , bodyAsBase64 =
                            "Failed to parse URL"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Base64.fromBytes
                        , headersToAdd = []
                        }

                    Just url ->
                        case httpRequestEvent.request.uri |> String.split "/" |> List.reverse |> List.head of
                            Just "readme-md" ->
                                { statusCode = 200
                                , bodyAsBase64 =
                                    CompilationInterface.SourceFiles.file____README_md.utf8
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Base64.fromBytes
                                , headersToAdd =
                                    [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                                    ]
                                }

                            Just "alpha-file-via-other-interface-module" ->
                                { statusCode = 200
                                , bodyAsBase64 =
                                    CompilationInterface.CustomName.SourceFiles.file____static_content_alpha_file_in_directory_txt.utf8
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Base64.fromBytes
                                , headersToAdd =
                                    [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                                    ]
                                }

                            Just "file-via-other-source-dir-beta" ->
                                { statusCode = 200
                                , bodyAsBase64 =
                                    CompilationInterface.CustomNameBeta.SourceFiles.file____file_under_beta_txt.utf8
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Base64.fromBytes
                                , headersToAdd =
                                    [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                                    ]
                                }

                            _ ->
                                case
                                    CompilationInterface.SourceFiles.file_tree____static_content
                                        |> mapFileTreeNodeFromSource
                                        |> FileTree.flatListOfBlobsFromFileTreeNode
                                        |> List.filter (Tuple.first >> (==) (String.split "/" url.path |> List.filter (String.isEmpty >> not)))
                                        |> List.head
                                of
                                    Just ( _, matchingFile ) ->
                                        { statusCode = 200
                                        , bodyAsBase64 = Just matchingFile.base64
                                        , headersToAdd =
                                            [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                                            ]
                                        }

                                    Nothing ->
                                        { statusCode = 404
                                        , bodyAsBase64 = Nothing
                                        , headersToAdd = []
                                        }

        httpResponse =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = response
            }
    in
    ( stateBefore
    , [ Platform.WebService.RespondToHttpRequest httpResponse ]
    )


mapFileTreeNodeFromSource : CompilationInterface.SourceFiles.FileTreeNode a -> FileTree.FileTreeNode a
mapFileTreeNodeFromSource node =
    case node of
        CompilationInterface.SourceFiles.BlobNode blob ->
            FileTree.BlobNode blob

        CompilationInterface.SourceFiles.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromSource) |> FileTree.TreeNode
