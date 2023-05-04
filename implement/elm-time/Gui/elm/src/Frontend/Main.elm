module Frontend.Main exposing (..)

import Browser
import Browser.Navigation
import CompilationInterface.GenerateJsonConverters
import Element
import Element.Font
import Frontend.BrowserApplicationInitWithTime
import Frontend.View as View
import Frontend.Visuals as Visuals
import HostInterface
import Html
import Http
import Json.Decode
import Time
import Url


type alias State =
    { time : Time.Posix
    , url : Url.Url
    , adminInterfaceConfig : Maybe (Result String HostInterface.AdminInterfaceConfig)
    }


type Event
    = OnUrlRequestEvent Browser.UrlRequest
    | OnUrlChangeEvent Url.Url
    | MessageToHostResultEvent HostInterface.MessageToHost (Result String (List HostInterface.EventFromHost))
    | DiscardEvent


main : Frontend.BrowserApplicationInitWithTime.Program () State Event
main =
    mainWithCustomInit (identity >> Tuple.pair >> (|>) Cmd.none)


mainWithCustomInit :
    (State -> ( State, Cmd.Cmd Event ))
    -> Frontend.BrowserApplicationInitWithTime.Program () State Event
mainWithCustomInit customInit =
    mainWithCustomizedInitAndUpdate
        (\normalInit ->
            \url navigationKey time ->
                let
                    ( ( state, customCmd ), cmd ) =
                        normalInit url navigationKey time
                            |> Tuple.mapFirst customInit
                in
                ( state, [ cmd, customCmd ] |> Cmd.batch )
        )
        identity


mainWithCustomizedInitAndUpdate :
    ((Url.Url -> Browser.Navigation.Key -> Time.Posix -> ( State, Cmd.Cmd Event ))
     -> (Url.Url -> Browser.Navigation.Key -> Time.Posix -> ( State, Cmd.Cmd Event ))
    )
    -> ((Event -> State -> ( State, Cmd.Cmd Event )) -> (Event -> State -> ( State, Cmd.Cmd Event )))
    -> Frontend.BrowserApplicationInitWithTime.Program () State Event
mainWithCustomizedInitAndUpdate customizeInit customizeUpdate =
    let
        customUpdate =
            customizeUpdate update
    in
    Frontend.BrowserApplicationInitWithTime.application
        { init =
            \_ url navigationKey time ->
                let
                    customInit =
                        customizeInit init

                    ( customInitState, customInitCmd ) =
                        customInit url navigationKey time
                in
                ( customInitState
                , customInitCmd
                )
        , view = view
        , viewWhileWaitingForTime = { body = [ Html.text "Measuring time..." ], title = "Measuring time..." }
        , update = customUpdate
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequestEvent
        , onUrlChange = OnUrlChangeEvent
        }


subscriptions : State -> Sub Event
subscriptions _ =
    Sub.none


init : Url.Url -> Browser.Navigation.Key -> Time.Posix -> ( State, Cmd Event )
init url _ time =
    ( { time = time
      , url = url
      , adminInterfaceConfig = Nothing
      }
    , [ sendMessageToHostCmd HostInterface.ReadAdminInterfaceConfigRequest
      ]
        |> Cmd.batch
    )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        MessageToHostResultEvent message eventsFromHostResult ->
            case message of
                HostInterface.ReadAdminInterfaceConfigRequest ->
                    let
                        result =
                            eventsFromHostResult
                                |> Result.andThen
                                    (\events ->
                                        events
                                            |> List.filterMap
                                                (\fromHostEvent ->
                                                    case fromHostEvent of
                                                        HostInterface.ReadAdminInterfaceConfigEvent config ->
                                                            Just config
                                                )
                                            |> List.head
                                            |> Maybe.map Ok
                                            |> Maybe.withDefault (Err "No AdminInterfaceConfigEvent found")
                                    )
                    in
                    ( { stateBefore
                        | adminInterfaceConfig = Just result
                      }
                    , Cmd.none
                    )

        OnUrlRequestEvent _ ->
            ( stateBefore, Cmd.none )

        OnUrlChangeEvent _ ->
            ( stateBefore, Cmd.none )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


view : State -> Browser.Document e
view state =
    let
        body =
            case state.adminInterfaceConfig of
                Nothing ->
                    Element.text "Loading config..."

                Just (Err error) ->
                    Element.text ("Failed loading config: " ++ error)

                Just (Ok config) ->
                    viewAdminInterfaceConfig state config
    in
    { body =
        [ body ]
            |> Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 10
                ]
            |> Element.layout
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Font.size 16
                ]
            |> List.singleton
    , title = "Elm-Time Admin Interface"
    }


viewAdminInterfaceConfig : State -> HostInterface.AdminInterfaceConfig -> Element.Element e
viewAdminInterfaceConfig state config =
    let
        stateUrl =
            state.url

        instanceUrl =
            { stateUrl
                | path = ""
                , query = Nothing
                , fragment = Nothing
            }

        viewHttpRoute httpRoute =
            [ Visuals.linkElementFromUrlAndTextLabel
                { url = httpRoute.path
                , labelText = httpRoute.path
                , newTabLink = False
                }
            , Element.text ("[ " ++ String.join ", " (List.map String.toUpper httpRoute.methods) ++ " ]")
            ]
                |> Element.wrappedRow [ Element.spacing 10 ]
    in
    [ [ [ Element.text "Welcome to the Elm-Time admin interface for the instance at "
        , Visuals.linkElementFromHref { newTabLink = False } (Url.toString instanceUrl)
        ]
      , [ Element.text ("This instance currently runs version " ++ config.elmTimeVersionId ++ ".") ]
      , [ Element.text "To check for newer versions, see "
        , Visuals.linkElementFromHref { newTabLink = False } "https://elm-time.org/download"
        , Element.text " or "
        , Visuals.linkElementFromHref { newTabLink = False } "https://github.com/elm-time/elm-time/releases"
        ]
      ]
        |> List.map (Element.paragraph [])
        |> Element.column [ Element.spacing 5 ]
    , [ Element.text "HTTP APIs"
            |> Element.el (Visuals.headingAttributes 3)
      , config.httpRoutes
            |> List.map viewHttpRoute
            |> Element.column [ Element.spacing 10 ]
      ]
        |> Element.column [ Element.spacing 10 ]
    , [ [ Element.text "The easiest way to use the APIs is via the command-line interface in the elm-time executable file." ]
      , [ Element.text "To learn about the admin interface and how to deploy an app, see "
        , Visuals.linkElementFromHref { newTabLink = False } "https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md"
        ]
      ]
        |> List.map (Element.paragraph [])
        |> Element.column [ Element.spacing 7 ]
    ]
        |> Element.column [ Element.spacing 20 ]


sendMessageToHostCmd : HostInterface.MessageToHost -> Cmd Event
sendMessageToHostCmd message =
    Http.post
        { url = View.urlFromPath []
        , body =
            message
                |> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToHost
                |> Http.jsonBody
        , expect = expectMessageToHostResultEvent message
        }


expectMessageToHostResultEvent : HostInterface.MessageToHost -> Http.Expect Event
expectMessageToHostResultEvent message =
    Http.expectStringResponse
        (MessageToHostResultEvent message)
        (\response ->
            case response of
                Http.BadUrl_ badUrl ->
                    Err ("Bad Url: " ++ badUrl)

                Http.Timeout_ ->
                    Err "Timeout"

                Http.NetworkError_ ->
                    Err "Network Error"

                Http.BadStatus_ metadata body ->
                    Err
                        ("Bad status code: "
                            ++ String.fromInt metadata.statusCode
                            ++ " from "
                            ++ metadata.url
                            ++ ":\n"
                            ++ body
                        )

                Http.GoodStatus_ metadata bodyString ->
                    bodyString
                        |> Json.Decode.decodeString
                            (Json.Decode.list CompilationInterface.GenerateJsonConverters.jsonDecodeEventFromHost)
                        |> Result.mapError
                            (Json.Decode.errorToString
                                >> (++) ("Failed to decode response from " ++ metadata.url ++ ": ")
                            )
        )
