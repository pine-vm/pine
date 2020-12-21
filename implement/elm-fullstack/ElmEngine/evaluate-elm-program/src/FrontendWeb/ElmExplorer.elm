module FrontendWeb.ElmExplorer exposing (State, main)

import Browser
import Element
import Element.Font
import ElmInteractive
import Html
import Html.Attributes as HA
import Html.Events
import Pine


type alias State =
    { expression : String
    , evaluationContext : Result String Pine.ExpressionContext
    }


type Event
    = UserInputExpression String


init : ( State, Cmd Event )
init =
    ( { expression = ""
      , evaluationContext = ElmInteractive.pineExpressionContextForElmInteractive ElmInteractive.DefaultContext
      }
    , Cmd.none
    )


main : Program () State Event
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputExpression expression ->
            ( { stateBefore | expression = expression }, Cmd.none )


view : State -> Html.Html Event
view state =
    let
        evalResult =
            case state.evaluationContext of
                Err error ->
                    Err ("Failed to initialize the evaluation context: " ++ error)

                Ok evaluationContext ->
                    ElmInteractive.submissionInInteractiveInPineContext evaluationContext state.expression
                        |> Result.map Tuple.second

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

        evalResultElement =
            if String.isEmpty (String.trim state.expression) then
                Element.none

            else
                case evalResult of
                    Err error ->
                        Element.text ("Error: " ++ error)

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
    in
    [ Element.text "Expression to evaluate"
    , indentOneLevel inputExpressionElement
    , Element.text "Evaluation result"
    , indentOneLevel
        (Element.paragraph [ Element.htmlAttribute attributeMonospaceFont ]
            [ evalResultElement ]
        )
    ]
        |> Element.column
            [ Element.spacing defaultFontSize
            , Element.padding 10
            , Element.width Element.fill
            ]
        |> Element.layout [ Element.Font.size defaultFontSize ]


indentOneLevel : Element.Element a -> Element.Element a
indentOneLevel =
    Element.el [ Element.paddingEach { left = defaultFontSize, right = 0, top = 0, bottom = 0 } ]


attributeMonospaceFont : Html.Attribute a
attributeMonospaceFont =
    HA.style "font-family" "monospace, monospace"


defaultFontSize : Int
defaultFontSize =
    16
