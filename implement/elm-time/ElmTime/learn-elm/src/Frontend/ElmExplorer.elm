module Frontend.ElmExplorer exposing
    ( State
    , main
    , viewInteractive
    )

import Browser
import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Region
import ElmCompiler
import ElmInteractive
import ElmInteractiveParser
import FirCompiler
import Frontend.BrowserApplicationInitWithTime
import Html
import Html.Attributes as HA
import Html.Events
import Pine
import Result.Extra
import Time
import Url


type alias State =
    { time : Time.Posix
    , expression : String
    , evaluationContext : Result String Pine.EvalContext
    , lastUserInputExpressionTime : Time.Posix
    , lastEvaluatedExpression : Maybe ( String, Result String ElmInteractive.SubmissionResponse )
    , lastAddElmModuleAttempt : Maybe (Result String { compiledModuleSize : Int })
    , addElmModuleControl : Maybe AddModuleControlState
    }


type alias InteractiveState =
    { expression : String
    , lastUserInputExpressionTime : Time.Posix
    , lastEvaluatedExpression : Maybe ( String, Result String ElmInteractive.SubmissionResponse )
    }


type Event
    = UserInputExpression String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | TimeArrivedEvent Time.Posix
    | UserInputAddElmModuleEvent { moduleText : String, compile : Bool }
    | UserInputDiscardAddElmModuleEvent


type AddModuleControlState
    = EditingControlState String
    | SubmittedControlState (Result String { compiledModuleSize : Int })


evalDelayFromUserInputMilliseconds : Int
evalDelayFromUserInputMilliseconds =
    500


init : Time.Posix -> ( State, Cmd Event )
init time =
    ( { time = time
      , expression = ""
      , evaluationContext = ElmInteractiveParser.compileEvalContextForElmInteractive ElmInteractive.DefaultContext
      , lastUserInputExpressionTime = Time.millisToPosix 0
      , lastEvaluatedExpression = Nothing
      , lastAddElmModuleAttempt = Nothing
      , addElmModuleControl = Nothing
      }
    , Cmd.none
    )


main : Frontend.BrowserApplicationInitWithTime.Program () State Event
main =
    Frontend.BrowserApplicationInitWithTime.application
        { viewWhileWaitingForTime = viewWhileWaitingForTime
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


viewWhileWaitingForTime : Browser.Document e
viewWhileWaitingForTime =
    { title = "Elm Explorer"
    , body = [ Html.text "Initializing..." ]
    }


subscriptions : State -> Sub.Sub Event
subscriptions state =
    if not (lastEvaluatedExpressionIsLastEntered state) then
        Time.every 100 TimeArrivedEvent

    else
        Sub.none


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputExpression expression ->
            ( { stateBefore
                | expression = expression
                , lastUserInputExpressionTime = stateBefore.time
              }
            , Cmd.none
            )

        UrlRequest _ ->
            ( stateBefore, Cmd.none )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        TimeArrivedEvent time ->
            let
                lastUserInputExpressionAgeMilliseconds =
                    Time.posixToMillis time - Time.posixToMillis stateBefore.lastUserInputExpressionTime
            in
            ( { stateBefore | time = time }
                |> (if evalDelayFromUserInputMilliseconds < lastUserInputExpressionAgeMilliseconds then
                        updateLastEvaluatedExpression

                    else
                        identity
                   )
            , Cmd.none
            )

        UserInputAddElmModuleEvent { moduleText, compile } ->
            if compile then
                let
                    ( compileResult, ( state, cmd ) ) =
                        addElmModule moduleText stateBefore

                    addElmModuleControl =
                        case compileResult of
                            Err error ->
                                SubmittedControlState (Err error)

                            Ok { compiledModuleSize } ->
                                SubmittedControlState (Ok { compiledModuleSize = compiledModuleSize })
                in
                ( { state | addElmModuleControl = Just addElmModuleControl }
                , cmd
                )

            else
                ( { stateBefore
                    | addElmModuleControl =
                        Just (EditingControlState moduleText)
                  }
                , Cmd.none
                )

        UserInputDiscardAddElmModuleEvent ->
            ( { stateBefore | addElmModuleControl = Nothing }
            , Cmd.none
            )


addElmModule : String -> State -> ( Result String { environment : Pine.Value, compiledModuleSize : Int }, ( State, Cmd Event ) )
addElmModule elmModuleText stateBefore =
    let
        addModuleResult : Result String { environment : Pine.Value, compiledModuleSize : Int }
        addModuleResult =
            case stateBefore.evaluationContext of
                Err error ->
                    Err ("Failed to initialize the evaluation context: " ++ error)

                Ok evaluationContext ->
                    parseElmModule elmModuleText
                        |> Result.andThen
                            (\( parsedModule, addModule ) ->
                                addModule evaluationContext.environment
                                    |> Result.map
                                        (\{ environment, addedModule } ->
                                            { environment = environment
                                            , compiledModuleSize = FirCompiler.estimatePineValueSize (Tuple.second addedModule)
                                            }
                                        )
                            )
    in
    ( addModuleResult
    , ( { stateBefore
            | evaluationContext =
                addModuleResult
                    |> Result.map (\{ environment } -> { environment = environment })
                    |> Result.Extra.orElse stateBefore.evaluationContext
            , lastAddElmModuleAttempt =
                addModuleResult
                    |> Result.map (\{ compiledModuleSize } -> { compiledModuleSize = compiledModuleSize })
                    |> Just
        }
      , Cmd.none
      )
    )


parseElmModule :
    String
    ->
        Result
            String
            ( ElmCompiler.ProjectParsedElmFile
            , Pine.Value -> Result String { addedModule : ( List String, Pine.Value ), environment : Pine.Value }
            )
parseElmModule elmModuleText =
    ElmInteractiveParser.expandElmInteractiveEnvironmentWithModuleTexts [ elmModuleText ]
        |> Result.andThen
            (\( parsedModules, addModules ) ->
                case parsedModules of
                    [ parsedModule ] ->
                        Ok
                            ( parsedModule
                            , \evaluationContext ->
                                addModules evaluationContext
                                    |> Result.andThen
                                        (\compilationOk ->
                                            case compilationOk.addedModules of
                                                [ moduleAdded ] ->
                                                    Ok { addedModule = moduleAdded, environment = compilationOk.environment }

                                                _ ->
                                                    Err "Expected exactly one module to be added"
                                        )
                            )

                    _ ->
                        Err "Expected exactly one module to be parsed"
            )


updateLastEvaluatedExpression : State -> State
updateLastEvaluatedExpression stateBefore =
    let
        expression =
            stateBefore.expression
    in
    if Just expression == Maybe.map Tuple.first stateBefore.lastEvaluatedExpression then
        stateBefore

    else
        let
            lastEvaluatedExpression =
                case stateBefore.evaluationContext of
                    Err error ->
                        Just ( expression, Err ("Failed to initialize the evaluation context: " ++ error) )

                    Ok evaluationContext ->
                        Just
                            ( expression
                            , if String.isEmpty (String.trim expression) then
                                Ok { displayText = "" }

                              else
                                ElmInteractiveParser.submissionInInteractiveInPineContext evaluationContext expression
                                    |> Result.map Tuple.second
                            )
        in
        { stateBefore | lastEvaluatedExpression = lastEvaluatedExpression }


view : State -> Browser.Document Event
view state =
    let
        addElmModuleButton =
            buttonElement
                { labelText = "Add Elm module"
                , clickEvent = UserInputAddElmModuleEvent { moduleText = "", compile = False }
                }

        interactiveElement : Element.Element Event
        interactiveElement =
            viewInteractive
                { userInputExpression = UserInputExpression
                }
                { expression = state.expression
                , lastUserInputExpressionTime = state.lastUserInputExpressionTime
                , lastEvaluatedExpression = state.lastEvaluatedExpression
                }

        overlayElement : Element.Element Event
        overlayElement =
            case state.addElmModuleControl of
                Nothing ->
                    Element.none

                Just addElmModuleControl ->
                    viewAddElmModuleControl
                        { userInputAddElmModule =
                            \moduleText -> UserInputAddElmModuleEvent { moduleText = moduleText, compile = True }
                        , userInputEnterText =
                            \moduleText -> UserInputAddElmModuleEvent { moduleText = moduleText, compile = False }
                        , userInputCancel = UserInputDiscardAddElmModuleEvent
                        }
                        addElmModuleControl
                        |> Element.el
                            [ Element.padding 10
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                        |> Element.el
                            [ Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
    in
    { body =
        [ [ interactiveElement
          , addElmModuleButton
          ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.padding 10
                , Element.width Element.fill
                , Element.height Element.fill
                ]
            |> Element.el
                [ Element.inFront overlayElement
                , Element.width Element.fill
                , Element.height Element.fill
                ]
            |> Element.layout
                [ Element.Font.size defaultFontSize
                , Element.Font.color (Element.rgb 1 1 1)
                , Element.Background.color (Element.rgb 0.1 0.1 0.1)
                ]
        ]
    , title = "Elm Explorer"
    }


viewInteractive :
    { userInputExpression : String -> e }
    -> InteractiveState
    -> Element.Element e
viewInteractive config state =
    let
        inputExpressionElement =
            [ editorElement
                { currentText = state.expression
                , userInputEnterText = config.userInputExpression
                , fillSpace = False
                }
                |> Element.html
            ]
                |> Element.column []

        evalResultElementFromEvalResult evalResult isUpToDate =
            let
                monospaceParagraph inParagraph =
                    Element.paragraph [ Element.htmlAttribute attributeMonospaceFont ]
                        [ inParagraph ]

                resultIfCurrentElement =
                    case evalResult of
                        Err error ->
                            ("Error: " ++ error)
                                |> Html.text
                                |> List.singleton
                                |> Html.div [ HA.style "white-space" "pre-wrap" ]
                                |> Element.html
                                |> monospaceParagraph

                        Ok evalSuccess ->
                            evalSuccess.displayText
                                |> Html.text
                                |> List.singleton
                                |> Html.div [ HA.style "white-space" "pre-wrap" ]
                                |> Element.html
                                |> monospaceParagraph
            in
            [ Element.text "Updating..."
                |> Element.el [ Element.Font.size (defaultFontSize * 3 // 2), Element.transparent isUpToDate ]
            , [ resultIfCurrentElement ]
                |> Element.column
                    [ Element.alpha
                        (if isUpToDate then
                            1

                         else
                            0.4
                        )
                    ]
            ]
                |> Element.column []

        evalResultElement =
            case state.lastEvaluatedExpression of
                Nothing ->
                    evalResultElementFromEvalResult (Ok { displayText = "" }) False

                Just ( evaluatedExpression, evaluatedExpressionResult ) ->
                    evalResultElementFromEvalResult evaluatedExpressionResult (evaluatedExpression == state.expression)
    in
    [ Element.text "Expression to evaluate"
    , indentOneLevel inputExpressionElement
    , Element.text "Evaluation result"
    , indentOneLevel evalResultElement
    ]
        |> Element.column
            [ Element.spacing defaultFontSize
            , Element.padding 10
            , Element.width Element.fill
            ]


viewAddElmModuleControl :
    { userInputAddElmModule : String -> e
    , userInputEnterText : String -> e
    , userInputCancel : e
    }
    -> AddModuleControlState
    -> Element.Element e
viewAddElmModuleControl config addElmModuleControlState =
    let
        cancelButtonFromLabel buttonText =
            buttonElement
                { labelText = buttonText
                , clickEvent = config.userInputCancel
                }
    in
    case addElmModuleControlState of
        EditingControlState moduleText ->
            let
                moduleTextEditorElement =
                    [ editorElement
                        { currentText = moduleText
                        , userInputEnterText = config.userInputEnterText
                        , fillSpace = True
                        }
                        |> Element.html
                    ]
                        |> Element.column
                            [ Element.height Element.fill
                            , Element.width Element.fill
                            ]

                submitButtonElement =
                    buttonElement
                        { labelText = "Submit"
                        , clickEvent = config.userInputAddElmModule moduleText
                        }
            in
            [ Element.text "Add Elm module"
                |> Element.el [ Element.Region.heading 2 ]
            , moduleTextEditorElement
            , [ cancelButtonFromLabel "Cancel", submitButtonElement ]
                |> Element.row
                    [ Element.spacing defaultFontSize
                    , Element.padding 10
                    , Element.width Element.fill
                    ]
            ]
                |> Element.column
                    [ Element.spacing defaultFontSize
                    , Element.padding 30
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]

        SubmittedControlState addModuleResult ->
            let
                addModuleResultElement =
                    case addModuleResult of
                        Err error ->
                            [ [ "Failed to compile Elm module:"
                              , error
                              ]
                                |> String.join "\n"
                                |> Html.text
                            ]
                                |> Html.div
                                    [ HA.style "white-space" "pre"
                                    , HA.style "font-family" "monospace, monospace"
                                    , HA.style "line-height" "normal"
                                    ]
                                |> Element.html
                                |> Element.el [ Element.Font.color (Element.rgb 0.9 0.1 0.1) ]

                        Ok { compiledModuleSize } ->
                            Element.text ("Compiled module size: " ++ String.fromInt compiledModuleSize)
                                |> Element.el [ Element.Font.color (Element.rgb 0.1 0.9 0.1) ]
            in
            [ Element.text "Add Elm module"
            , addModuleResultElement
                |> Element.el
                    [ Element.scrollbars
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
            , cancelButtonFromLabel "Close"
            ]
                |> Element.column
                    [ Element.spacing defaultFontSize
                    , Element.padding 30
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]


editorElement :
    { currentText : String
    , userInputEnterText : String -> e
    , fillSpace : Bool
    }
    -> Html.Html e
editorElement config =
    let
        expressionTextareaHeight =
            (((config.currentText
                |> String.lines
                |> List.length
                |> max 2
                |> min 7
                |> toFloat
              )
                * 1.1
                + 1.5
             )
                |> String.fromFloat
            )
                ++ "em"

        layoutAttributes : List (Html.Attribute a)
        layoutAttributes =
            if config.fillSpace then
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "resize" "none"
                ]

            else
                [ HA.style "width" "40em"
                , HA.style "height" expressionTextareaHeight
                ]
    in
    Html.textarea
        ([ HA.value config.currentText
         , Html.Events.onInput config.userInputEnterText
         , HA.style "white-space" "pre"
         , HA.style "font-family" "monospace, monospace"
         , HA.style "font-size" "100%"
         , HA.style "padding" "0.4em"
         , HA.spellcheck False
         ]
            ++ layoutAttributes
        )
        []


buttonElement : { labelText : String, clickEvent : e } -> Element.Element e
buttonElement config =
    Element.text config.labelText
        |> Element.el
            [ Element.Events.onClick config.clickEvent
            , Element.pointer
            , Element.paddingXY 20 10
            , Element.Background.color (Element.rgb 0.1 0.3 0.7)
            , Element.Font.color (Element.rgb 0.9 0.9 0.9)
            , Element.Font.bold
            ]


lastEvaluatedExpressionIsLastEntered : State -> Bool
lastEvaluatedExpressionIsLastEntered state =
    case state.lastEvaluatedExpression of
        Nothing ->
            False

        Just ( expression, _ ) ->
            expression == state.expression


indentOneLevel : Element.Element a -> Element.Element a
indentOneLevel =
    Element.el [ Element.paddingEach { left = defaultFontSize, right = 0, top = 0, bottom = 0 } ]


attributeMonospaceFont : Html.Attribute a
attributeMonospaceFont =
    HA.style "font-family" "monospace, monospace"


defaultFontSize : Int
defaultFontSize =
    16
