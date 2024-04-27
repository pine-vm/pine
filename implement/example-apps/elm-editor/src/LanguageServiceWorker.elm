port module LanguageServiceWorker exposing (..)

{-| The module `LanguageServiceWorker` packages the language service to run it as an application in a web worker: <https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API>
-}

import CompilationInterface.GenerateJsonConverters
import Json.Decode
import Json.Encode
import LanguageService
import LanguageServiceInterface


port receiveRequest : (Json.Encode.Value -> msg) -> Sub msg


port sendResponse : Json.Encode.Value -> Cmd msg


type alias State =
    LanguageService.LanguageServiceState


type Event
    = RequestEvent Json.Encode.Value


main : Program () State Event
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : State -> Sub.Sub Event
subscriptions _ =
    receiveRequest RequestEvent


init : () -> ( State, Cmd.Cmd Event )
init () =
    ( LanguageService.initLanguageServiceState
    , Cmd.none
    )


update : Event -> State -> ( State, Cmd.Cmd Event )
update event stateBefore =
    case event of
        RequestEvent requestJson ->
            case
                Json.Decode.decodeValue
                    CompilationInterface.GenerateJsonConverters.jsonDecodeLanguageServiceRequestInWorkspace
                    requestJson
            of
                Err _ ->
                    ( stateBefore
                    , Cmd.none
                    )

                Ok requestInWorkspace ->
                    let
                        ( result, state ) =
                            handleRequest requestInWorkspace.request stateBefore
                    in
                    ( state
                    , sendResponseCmd
                        { requestId = requestInWorkspace.id
                        , response = result
                        }
                    )


handleRequest :
    LanguageServiceInterface.RequestInWorkspace
    -> State
    -> ( Result String LanguageServiceInterface.Response, State )
handleRequest requestInWorkspace stateBefore =
    let
        languageServiceState =
            LanguageService.updateLanguageServiceState requestInWorkspace.workspace stateBefore

        serviceResponse =
            case requestInWorkspace.request of
                LanguageServiceInterface.ProvideHoverRequest provideHoverRequest ->
                    LanguageServiceInterface.ProvideHoverResponse
                        (LanguageService.provideHover
                            provideHoverRequest
                            languageServiceState
                        )

                LanguageServiceInterface.ProvideCompletionItemsRequest provideCompletionItemsRequest ->
                    LanguageServiceInterface.ProvideCompletionItemsResponse
                        (LanguageService.provideCompletionItems
                            provideCompletionItemsRequest
                            languageServiceState
                        )
    in
    ( Ok serviceResponse
    , languageServiceState
    )


sendResponseCmd : LanguageServiceInterface.ResponseWithId -> Cmd.Cmd e
sendResponseCmd response =
    sendResponse
        (CompilationInterface.GenerateJsonConverters.jsonEncodeLanguageServiceResponse response)
