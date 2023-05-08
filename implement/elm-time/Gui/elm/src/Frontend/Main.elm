module Frontend.Main exposing (..)

import Browser
import Browser.Navigation
import CompilationInterface.GenerateJsonConverters
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Frontend.BrowserApplicationInitWithTime
import Frontend.View as View
import Frontend.Visuals as Visuals
    exposing
        ( defaultFontSize
        , errorColor
        , successColor
        )
import HostInterface
import Html
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Time
import Url


type alias State =
    { time : Time.Posix
    , url : Url.Url
    , adminInterfaceConfig : Maybe (Result String HostInterface.AdminInterfaceConfig)
    , applyFunction : SelectAndApplyFunctionState
    }


type alias SelectAndApplyFunctionState =
    { selectedFunction : Maybe ( String, ApplyFunctionState )
    }


type alias ApplyFunctionState =
    { parametersTexts : Dict.Dict Int String
    , commitResultingState : Bool
    , applicationRequest :
        Maybe
            ( ( HostInterface.ApplyFunctionOnDatabaseRequest, Time.Posix )
            , Maybe (Result String HostInterface.ApplyFunctionOnDatabaseSuccess)
            )
    }


type Event
    = TimeArrivedEvent Time.Posix
    | OnUrlRequestEvent Browser.UrlRequest
    | OnUrlChangeEvent Url.Url
    | MessageToHostResultEvent HostInterface.MessageToHost (Result String (List HostInterface.EventFromHost))
    | SelectAndApplyFunctionEvent SelectAndApplyFunctionEventStruct
    | DiscardEvent


type SelectAndApplyFunctionEventStruct
    = SelectFunctionEvent String
    | UserInputParameterTextEvent Int String
    | UserInputSetCommitResultingStateEvent Bool
    | UserInputApplyFunctionEvent HostInterface.ApplyFunctionOnDatabaseRequest
    | ApplyFunctionResponseEvent HostInterface.ApplyFunctionOnDatabaseRequest (Result String HostInterface.ApplyFunctionOnDatabaseSuccess)


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
    Time.every 1 TimeArrivedEvent


init : Url.Url -> Browser.Navigation.Key -> Time.Posix -> ( State, Cmd Event )
init url _ time =
    ( { time = time
      , url = url
      , adminInterfaceConfig = Nothing
      , applyFunction = { selectedFunction = Nothing }
      }
    , [ sendMessageToHostCmd HostInterface.ReadAdminInterfaceConfigRequest
      ]
        |> Cmd.batch
    )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        TimeArrivedEvent time ->
            ( { stateBefore | time = time }, Cmd.none )

        MessageToHostResultEvent message eventsFromHostResult ->
            case message of
                HostInterface.ReadAdminInterfaceConfigRequest ->
                    let
                        result =
                            eventsFromHostResult
                                |> Result.andThen
                                    (List.filterMap
                                        (\fromHostEvent ->
                                            case fromHostEvent of
                                                HostInterface.ReadAdminInterfaceConfigEvent config ->
                                                    Just config
                                        )
                                        >> List.head
                                        >> Maybe.map Ok
                                        >> Maybe.withDefault (Err "No AdminInterfaceConfigEvent found")
                                    )
                                |> Result.map
                                    (\adminInterfaceConfig ->
                                        { adminInterfaceConfig
                                            | databaseFunctions =
                                                adminInterfaceConfig.databaseFunctions
                                                    -- For now, only show functions with a normal module prefix
                                                    |> List.filter (.functionName >> String.contains ".")
                                        }
                                    )
                    in
                    ( { stateBefore
                        | adminInterfaceConfig = Just result
                      }
                    , Cmd.none
                    )

        SelectAndApplyFunctionEvent selectAndApplyFunctionEvent ->
            let
                ( selectAndApplyFunctionState, cmd ) =
                    stateBefore.applyFunction
                        |> updateApplyFunction { time = stateBefore.time } selectAndApplyFunctionEvent
            in
            ( { stateBefore
                | applyFunction = selectAndApplyFunctionState
              }
            , cmd |> Cmd.map SelectAndApplyFunctionEvent
            )

        OnUrlRequestEvent _ ->
            ( stateBefore, Cmd.none )

        OnUrlChangeEvent _ ->
            ( stateBefore, Cmd.none )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


updateApplyFunction :
    { time : Time.Posix }
    -> SelectAndApplyFunctionEventStruct
    -> SelectAndApplyFunctionState
    -> ( SelectAndApplyFunctionState, Cmd SelectAndApplyFunctionEventStruct )
updateApplyFunction { time } event stateBefore =
    let
        updateSelectedFunction updateIfSelectedFunction =
            case stateBefore.selectedFunction of
                Nothing ->
                    ( stateBefore, Cmd.none )

                Just ( selectedFunctionName, selectedFunctionBefore ) ->
                    let
                        ( selectedFunction, cmd ) =
                            updateIfSelectedFunction selectedFunctionBefore
                    in
                    ( { stateBefore
                        | selectedFunction = Just ( selectedFunctionName, selectedFunction )
                      }
                    , cmd
                    )
    in
    case event of
        SelectFunctionEvent functionName ->
            ( { stateBefore
                | selectedFunction =
                    Just
                        ( functionName
                        , { parametersTexts = Dict.empty
                          , commitResultingState = False
                          , applicationRequest = Nothing
                          }
                        )
              }
            , Cmd.none
            )

        UserInputParameterTextEvent paramIndex paramText ->
            updateSelectedFunction
                (\selectedFunction ->
                    ( { selectedFunction
                        | parametersTexts =
                            selectedFunction.parametersTexts
                                |> Dict.insert paramIndex paramText
                      }
                    , Cmd.none
                    )
                )

        UserInputSetCommitResultingStateEvent commitResultingState ->
            updateSelectedFunction
                (\selectedFunction ->
                    ( { selectedFunction
                        | commitResultingState = commitResultingState
                      }
                    , Cmd.none
                    )
                )

        UserInputApplyFunctionEvent applyFunctionRequest ->
            updateSelectedFunction
                (\selectedFunction ->
                    let
                        readyForNewRequest =
                            {-
                               In case user accidentally clicks button to send request twice, we don't want to send the request twice.
                               To prevent this, we only allow a new request if the previous request was more than 4 seconds ago.
                            -}
                            case selectedFunction.applicationRequest of
                                Nothing ->
                                    True

                                Just ( ( _, previousRequestTime ), _ ) ->
                                    Time.posixToMillis time - Time.posixToMillis previousRequestTime > 4 * 1000
                    in
                    if not readyForNewRequest then
                        ( selectedFunction, Cmd.none )

                    else
                        ( { selectedFunction
                            | applicationRequest =
                                Just
                                    ( ( applyFunctionRequest, time )
                                    , Nothing
                                    )
                          }
                        , Http.post
                            { url = "/api/apply-database-function"
                            , body =
                                applyFunctionRequest
                                    |> CompilationInterface.GenerateJsonConverters.jsonEncodeApplyFunctionOnDatabaseRequest
                                    |> Http.jsonBody
                            , expect = expectApplyFunctionResponse applyFunctionRequest
                            }
                        )
                )

        ApplyFunctionResponseEvent applyFunctionRequest result ->
            updateSelectedFunction
                (\selectedFunction ->
                    case selectedFunction.applicationRequest of
                        Nothing ->
                            ( selectedFunction
                            , Cmd.none
                            )

                        Just ( requestAndTime, _ ) ->
                            if Tuple.first requestAndTime /= applyFunctionRequest then
                                ( selectedFunction
                                , Cmd.none
                                )

                            else
                                ( { selectedFunction
                                    | applicationRequest = Just ( requestAndTime, Just result )
                                  }
                                , Cmd.none
                                )
                )


view : State -> Browser.Document Event
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
                , Element.Font.size defaultFontSize
                ]
            |> List.singleton
    , title = "Elm-Time Admin Interface"
    }


viewAdminInterfaceConfig : State -> HostInterface.AdminInterfaceConfig -> Element.Element Event
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
    , [ Element.text "Database Functions"
            |> Element.el (Visuals.headingAttributes 3)
      , viewApplyFunctionOnDatabase state.applyFunction config
            |> Element.map SelectAndApplyFunctionEvent
            |> Element.el [ Element.paddingXY 20 0, Element.width Element.fill ]
      ]
        |> Element.column
            [ Element.spacing 10
            , Element.width Element.fill
            ]
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
        |> Element.column
            [ Element.spacing 7
            , Element.width Element.fill
            ]
    ]
        |> Element.column
            [ Element.spacing 50
            , Element.width Element.fill
            ]


viewApplyFunctionOnDatabase :
    SelectAndApplyFunctionState
    -> HostInterface.AdminInterfaceConfig
    -> Element.Element SelectAndApplyFunctionEventStruct
viewApplyFunctionOnDatabase state config =
    let
        options =
            config.databaseFunctions
                |> List.map
                    (\exposedFunction ->
                        Element.Input.option
                            exposedFunction.functionName
                            (Element.el viewFunctionNameAttributes
                                (Element.text exposedFunction.functionName)
                            )
                    )

        selectFunctionElement =
            Element.Input.radio [ Element.spacing 5 ]
                { onChange = SelectFunctionEvent
                , options = options
                , selected = Maybe.map Tuple.first state.selectedFunction
                , label = Element.Input.labelAbove [ Element.padding 10 ] (Element.text "Select function")
                }

        selectedFunctionElement =
            case state.selectedFunction of
                Nothing ->
                    Element.none

                Just ( selectedFunctionName, selectedFunctionState ) ->
                    case
                        config.databaseFunctions
                            |> List.filter (\exposedFunction -> exposedFunction.functionName == selectedFunctionName)
                            |> List.head
                    of
                        Nothing ->
                            Element.none

                        Just selectedFunctionDescription ->
                            let
                                functionTypeSourceCodeText =
                                    composeFunctionTypeSourceText selectedFunctionDescription
                                        |> String.lines
                                        |> List.map ((++) "    ")
                                        |> String.join "\n"
                            in
                            [ [ Element.text "Apply function "
                              , Element.text selectedFunctionName
                                    |> Element.el viewFunctionNameAttributes
                              ]
                                |> Element.row (Visuals.headingAttributes 4)
                            , ((selectedFunctionDescription.functionName
                                    |> String.split "."
                                    |> List.reverse
                                    |> List.head
                                    |> Maybe.withDefault selectedFunctionDescription.functionName
                               )
                                ++ " :\n"
                                ++ functionTypeSourceCodeText
                              )
                                |> Html.text
                                |> Element.html
                                |> Element.el
                                    [ Element.Font.family [ Element.Font.monospace ]
                                    , Element.Background.color (Element.rgba 0.5 0.5 0.5 0.2)
                                    , Element.paddingXY 5 3
                                    , Element.Border.rounded 3
                                    , Visuals.elementFontSizePercent 80
                                    , Element.htmlAttribute (Html.Attributes.style "line-height" "normal")
                                    ]
                            , viewPrepareApplyFunctionOnDatabase selectedFunctionState selectedFunctionDescription
                            ]
                                |> Element.column
                                    [ Element.spacing 10
                                    , Element.width Element.fill
                                    ]
    in
    [ Element.text
        ("The currently deployed app exposes "
            ++ String.fromInt (List.length config.databaseFunctions)
            ++ " database functions:"
        )
    , selectFunctionElement
    , selectedFunctionElement
    ]
        |> Element.column
            [ Element.spacing 10
            , Element.width Element.fill
            ]


composeFunctionTypeSourceText : HostInterface.DatabaseFunctionDescription -> String
composeFunctionTypeSourceText function =
    List.map .typeSourceCodeText function.functionDescription.parameters
        ++ [ function.functionDescription.returnType.sourceCodeText ]
        |> String.join "\n-> "


viewPrepareApplyFunctionOnDatabase :
    ApplyFunctionState
    -> HostInterface.DatabaseFunctionDescription
    -> Element.Element SelectAndApplyFunctionEventStruct
viewPrepareApplyFunctionOnDatabase state exposedFunction =
    let
        parametersElement =
            exposedFunction.functionDescription.parameters
                |> List.indexedMap
                    (\paramIndex paramConfig ->
                        let
                            ( inputElement, hintElement ) =
                                if paramConfig.typeIsAppStateType then
                                    ( Element.none
                                    , Element.text "(Type is app state type)"
                                    )

                                else
                                    ( Element.Input.multiline
                                        [ Element.width Element.fill
                                        , Element.Font.family [ Element.Font.monospace ]
                                        ]
                                        { onChange = UserInputParameterTextEvent paramIndex
                                        , text = state.parametersTexts |> Dict.get paramIndex |> Maybe.withDefault ""
                                        , placeholder = Nothing
                                        , label = Element.Input.labelHidden ("Input parameter " ++ paramConfig.patternSourceCodeText)
                                        , spellcheck = False
                                        }
                                    , Element.none
                                    )
                        in
                        [ [ [ Element.text (paramConfig.patternSourceCodeText ++ " : ")
                            , Element.text paramConfig.typeSourceCodeText
                            ]
                                |> Element.wrappedRow
                                    [ Element.Font.family [ Element.Font.monospace ]
                                    , Element.Background.color (Element.rgba 0.5 0.5 0.5 0.2)
                                    , Element.paddingXY 5 3
                                    , Element.Border.rounded 3
                                    , Element.width Element.fill
                                    ]
                          , hintElement
                          ]
                            |> Element.wrappedRow
                                [ Element.spacing 10
                                , Element.width Element.fill
                                ]
                        , inputElement
                            |> Element.el
                                [ Element.paddingXY 20 0
                                , Element.width Element.fill
                                ]
                        ]
                            |> Element.column
                                [ Element.spacing 10
                                , Element.width Element.fill
                                ]
                    )
                |> Element.column
                    [ Element.paddingXY 10 0
                    , Element.spacing 10
                    , Element.width Element.fill
                    ]

        commitResultingStateElement =
            Element.Input.checkbox []
                { onChange = UserInputSetCommitResultingStateEvent
                , icon = Element.Input.defaultCheckbox
                , checked = state.commitResultingState
                , label =
                    Element.Input.labelLeft [ Element.padding 10 ]
                        (Element.text "Commit resulting state to database?")
                }

        ( resultDescriptionText, offerCommitResult ) =
            if exposedFunction.functionDescription.returnType.containsAppStateType then
                ( "contains app state"
                , True
                )

            else
                ( "does not contain app state"
                , False
                )

        resultElement =
            [ [ Element.text "Return type"
              , Element.text " "
              , Element.text exposedFunction.functionDescription.returnType.sourceCodeText
                    |> Element.el viewSourceCodeTextAttributes
              , Element.text " "
              , Element.text resultDescriptionText
              ]
                |> Element.row
                    [ Element.width Element.fill
                    ]
            , commitResultingStateElement
                |> Element.el [ Element.transparent (not offerCommitResult) ]
            ]
                |> Element.column
                    [ Element.spacing 5
                    , Element.width Element.fill
                    ]

        applyFunctionRequest =
            { functionName = exposedFunction.functionName
            , serializedArgumentsJson =
                exposedFunction.functionDescription.parameters
                    |> List.Extra.dropWhileRight (\param -> param.typeIsAppStateType)
                    |> List.indexedMap
                        (\paramIndex _ ->
                            state.parametersTexts
                                |> Dict.get paramIndex
                                |> Maybe.withDefault ""
                        )
            , commitResultingState = state.commitResultingState
            }

        applyFunctionButtonElement =
            Visuals.buttonElement
                []
                { label = Element.text "Apply function"
                , onPress = Just (UserInputApplyFunctionEvent applyFunctionRequest)
                , disabled = False
                }

        applicationRequestElement =
            case state.applicationRequest of
                Nothing ->
                    Element.none

                Just ( ( applicationRequest, _ ), maybeResult ) ->
                    case maybeResult of
                        Nothing ->
                            Element.text "Sending request to apply function..."

                        Just (Err functionApplicationFailed) ->
                            [ Element.text "Failed to apply function "
                            , Element.text applicationRequest.functionName |> Element.el viewFunctionNameAttributes
                            , Element.text (": " ++ functionApplicationFailed)
                            ]
                                |> Element.paragraph [ Element.Font.color errorColor ]

                        Just (Ok functionApplicationSuccess) ->
                            let
                                newAppStateReportElement =
                                    if not functionApplicationSuccess.functionApplicationResult.producedStateDifferentFromStateArgument then
                                        Element.text "Function application did not produce a different application state"

                                    else
                                        Element.text
                                            ("Function application produced a different application state "
                                                ++ (if functionApplicationSuccess.committedResultingState then
                                                        "that was committed to the main branch"

                                                    else
                                                        "that was not committed to the main branch"
                                                   )
                                            )

                                resultLessStateReportElement =
                                    case functionApplicationSuccess.functionApplicationResult.resultLessStateJson of
                                        Nothing ->
                                            Element.text "Did not return a value besides the application state"

                                        Just resultLessStateJson ->
                                            [ Element.text "Returned a value besides the application state:"
                                            , Element.text (Json.Encode.encode 2 resultLessStateJson)
                                                |> Element.el
                                                    [ Element.Font.family [ Element.Font.monospace ]
                                                    , Element.Background.color (Element.rgba 0.5 0.5 0.5 0.2)
                                                    , Element.paddingXY 5 3
                                                    , Element.Border.rounded 3
                                                    , Visuals.elementFontSizePercent 80
                                                    ]
                                            ]
                                                |> Element.column
                                                    [ Element.spacing 10
                                                    , Element.width Element.fill
                                                    ]
                            in
                            [ [ Element.text "Successfully applied function "
                              , Element.text applicationRequest.functionName |> Element.el viewFunctionNameAttributes
                              ]
                                |> Element.paragraph [ Element.Font.color successColor ]
                            , newAppStateReportElement
                            , resultLessStateReportElement
                            ]
                                |> Element.column
                                    [ Element.spacing 10
                                    , Element.width Element.fill
                                    ]
    in
    [ Element.text
        ("This function has "
            ++ String.fromInt (List.length exposedFunction.functionDescription.parameters)
            ++ " parameters:"
        )
    , parametersElement
    , resultElement
    , applyFunctionButtonElement
    , applicationRequestElement
    ]
        |> Element.column
            [ Element.spacing 10
            , Element.width Element.fill
            ]


viewFunctionNameAttributes : List (Element.Attribute msg)
viewFunctionNameAttributes =
    Element.Font.color (Element.rgba 0.4 0.4 0 1)
        :: viewSourceCodeTextAttributes


viewSourceCodeTextAttributes : List (Element.Attribute msg)
viewSourceCodeTextAttributes =
    [ Element.Font.family [ Element.Font.monospace ]
    , Element.Background.color (Element.rgba 0.5 0.5 0.5 0.2)
    , Element.paddingXY 5 3
    , Element.Border.rounded 3
    ]


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


expectApplyFunctionResponse : HostInterface.ApplyFunctionOnDatabaseRequest -> Http.Expect SelectAndApplyFunctionEventStruct
expectApplyFunctionResponse request =
    Http.expectStringResponse
        (ApplyFunctionResponseEvent request)
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
                            CompilationInterface.GenerateJsonConverters.jsonDecodeApplyFunctionOnDatabaseResult
                        |> Result.mapError
                            (Json.Decode.errorToString
                                >> (++) ("Failed to decode response from " ++ metadata.url ++ ": ")
                            )
                        |> Result.andThen identity
        )
