module FrontendWeb.ElmExplorer exposing (State, main)

import Browser
import Element
import Element.Font
import ElmEvaluation
import Html
import Html.Attributes as HA
import Html.Events


type alias State =
    { expression : String }


type Event
    = UserInputExpression String


init : ( State, Cmd Event )
init =
    ( { expression = "" }
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
            ElmEvaluation.evaluateExpressionString [] state.expression

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
            case evalResult of
                Err error ->
                    Element.text ("Error: " ++ error)

                Ok evalSuccess ->
                    Element.text (evalSuccess.valueAsJsonString ++ " : " ++ evalSuccess.typeText)
    in
    [ Element.text "Expression to evaluate"
    , indentOneLevel inputExpressionElement
    , Element.text "Evaluation result"
    , indentOneLevel (Element.paragraph [ Element.htmlAttribute attributeMonospaceFont ] [ evalResultElement ])
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
