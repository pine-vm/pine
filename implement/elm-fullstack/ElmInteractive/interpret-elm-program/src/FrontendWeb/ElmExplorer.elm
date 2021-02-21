module FrontendWeb.ElmExplorer exposing (State, main)

import Browser
import Element
import Element.Font
import ElmInteractive
import Html
import Html.Attributes as HA
import Html.Events
import Pine
import Time
import Url


type alias State =
    { expression : String
    , evaluationContext : Result String Pine.ExpressionContext
    , lastUserInputExpressionTime : Maybe Time.Posix
    , lastEvaluatedExpression : Maybe ( String, Result String ElmInteractive.SubmissionResponse )
    }


type Event
    = UserInputExpression String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | TimeArrivedEvent Time.Posix


evalDelayFromUserInputMilliseconds : Int
evalDelayFromUserInputMilliseconds =
    500


init : ( State, Cmd Event )
init =
    ( { expression = ""
      , evaluationContext = ElmInteractive.pineExpressionContextForElmInteractive ElmInteractive.DefaultContext
      , lastUserInputExpressionTime = Nothing
      , lastEvaluatedExpression = Nothing
      }
    , Cmd.none
    )


main : Program () State Event
main =
    Browser.application
        { init = \_ _ _ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


subscriptions : State -> Sub.Sub Event
subscriptions state =
    if
        (state.lastUserInputExpressionTime == Nothing)
            || not (lastEvaluatedExpressionIsLastEntered state)
    then
        Time.every 100 TimeArrivedEvent

    else
        Sub.none


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputExpression expression ->
            ( { stateBefore
                | expression = expression
                , lastUserInputExpressionTime = Nothing
              }
            , Cmd.none
            )

        UrlRequest _ ->
            ( stateBefore, Cmd.none )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        TimeArrivedEvent time ->
            let
                lastUserInputExpressionTime =
                    stateBefore.lastUserInputExpressionTime |> Maybe.withDefault time

                lastUserInputExpressionAgeMilliseconds =
                    Time.posixToMillis time - Time.posixToMillis lastUserInputExpressionTime
            in
            ( { stateBefore | lastUserInputExpressionTime = Just lastUserInputExpressionTime }
                |> (if evalDelayFromUserInputMilliseconds < lastUserInputExpressionAgeMilliseconds then
                        updateLastEvaluatedExpression

                    else
                        identity
                   )
            , Cmd.none
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
                                Ok ElmInteractive.SubmissionResponseNoValue

                              else
                                ElmInteractive.submissionInInteractiveInPineContext evaluationContext expression
                                    |> Result.map Tuple.second
                            )
        in
        { stateBefore | lastEvaluatedExpression = lastEvaluatedExpression }


view : State -> Browser.Document Event
view state =
    let
        expressionTextareaHeight =
            (((state.expression
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

        inputExpressionElement =
            [ Html.textarea
                [ HA.value state.expression
                , Html.Events.onInput UserInputExpression
                , HA.style "white-space" "pre"
                , HA.style "font-family" "monospace, monospace"
                , HA.style "font-size" "100%"
                , HA.style "padding" "0.4em"
                , HA.style "width" "40em"
                , HA.style "height" expressionTextareaHeight
                ]
                []
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
                            case evalSuccess of
                                ElmInteractive.SubmissionResponseNoValue ->
                                    Element.text "Got no value in response for this submission."

                                ElmInteractive.SubmissionResponseValue responseValue ->
                                    (responseValue.value |> ElmInteractive.elmValueAsExpression)
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
                    evalResultElementFromEvalResult (Ok ElmInteractive.SubmissionResponseNoValue) False

                Just ( evaluatedExpression, evaluatedExpressionResult ) ->
                    evalResultElementFromEvalResult evaluatedExpressionResult (evaluatedExpression == state.expression)
    in
    { body =
        [ [ Element.text "Expression to evaluate"
          , indentOneLevel inputExpressionElement
          , Element.text "Evaluation result"
          , indentOneLevel evalResultElement
          ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.padding 10
                , Element.width Element.fill
                ]
            |> Element.layout [ Element.Font.size defaultFontSize ]
        ]
    , title = "Elm Explorer"
    }


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
