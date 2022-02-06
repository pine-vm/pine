module Frontend.SilentTeacher exposing (State, main)

import Browser
import Browser.Navigation
import Element
import Element.Background
import Element.Font
import Element.Input
import ElmInteractive
import Html
import Html.Attributes as HA
import Html.Events
import Json.Decode
import Pine
import Time
import Url


type alias State =
    { expression : String
    , evaluationContext : Result String Pine.ExpressionContext
    , remainingLessons : List { lesson : Lesson, solution : String }
    , completedLessons : List { lesson : Lesson, solution : String }
    , navigationKey : Browser.Navigation.Key
    }


type alias Lesson =
    String


type Event
    = UserInputChangeExpression String
    | UserInputSubmitSolution String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | TimeArrivedEvent Time.Posix


lessonsExpressions : List Lesson
lessonsExpressions =
    [ "1 + 2"
    , "4 - 3"
    , "3 * 5"
    , """
let
    a = 3
in
    a + 7"""
    , """
let
    a = "n"
    b = "de"
in
    a ++ b"""
    ]
        |> List.map String.trim


init : Url.Url -> Browser.Navigation.Key -> ( State, Cmd Event )
init url navigationKey =
    let
        evaluationContextResult =
            ElmInteractive.pineExpressionContextForElmInteractive ElmInteractive.DefaultContext

        solutionFromLesson lesson =
            case evaluationContextResult of
                Err error ->
                    "Failed to initialize the evaluation context: " ++ error

                Ok evaluationContext ->
                    computeSolutionFromLessonInContext evaluationContext lesson
    in
    ( { expression = ""
      , evaluationContext = evaluationContextResult
      , navigationKey = navigationKey
      , remainingLessons = lessonsExpressions |> List.map (\lesson -> { lesson = lesson, solution = solutionFromLesson lesson })
      , completedLessons = []
      }
    , Cmd.none
    )


main : Program () State Event
main =
    Browser.application
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


subscriptions : State -> Sub.Sub Event
subscriptions _ =
    Sub.none


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputChangeExpression expression ->
            ( { stateBefore | expression = expression }
            , Cmd.none
            )

        UserInputSubmitSolution submission ->
            let
                state =
                    { stateBefore | expression = "" }
            in
            case stateBefore.remainingLessons of
                [] ->
                    ( state, Cmd.none )

                currentLesson :: nextRemainingLessons ->
                    if submission == currentLesson.solution then
                        ( { state
                            | remainingLessons = nextRemainingLessons
                            , completedLessons = stateBefore.completedLessons ++ [ currentLesson ]
                          }
                        , Cmd.none
                        )

                    else
                        ( state, Cmd.none )

        UrlRequest _ ->
            ( stateBefore, Cmd.none )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        TimeArrivedEvent _ ->
            ( stateBefore, Cmd.none )


view : State -> Browser.Document Event
view state =
    let
        totalLessonCount =
            List.length (state.completedLessons ++ state.remainingLessons)

        progressPercent =
            (List.length state.completedLessons * 100) // totalLessonCount
    in
    { body =
        [ [ viewInputElement state
                |> Element.el
                    [ Element.Background.color (Element.rgb255 116 190 254)
                    , Element.padding defaultFontSize
                    ]
          , Element.text "Your progress:"
          , viewProgressBar { progressPercent = progressPercent }
          ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.padding 10
                , Element.width Element.fill
                ]
            |> Element.layout [ Element.Font.size defaultFontSize ]
        ]
    , title = "Silent Teacher"
    }


viewProgressBar : { progressPercent : Int } -> Element.Element e
viewProgressBar { progressPercent } =
    [ Html.div [] []
        |> Element.html
        |> Element.el
            [ Element.width (Element.fillPortion progressPercent)
            , Element.height Element.fill
            , Element.Background.color (Element.rgb255 0 153 15)
            ]
    , Html.div [] []
        |> Element.html
        |> Element.el
            [ Element.width (Element.fillPortion (100 - progressPercent)) ]
    ]
        |> Element.row
            [ Element.width Element.fill
            , Element.height (Element.px (defaultFontSize * 2))
            ]
        |> Element.el
            [ Element.width Element.fill
            , Element.padding (defaultFontSize // 2)
            , Element.Background.color (Element.rgb255 85 85 85)
            , Element.inFront
                (Element.text (String.fromInt progressPercent ++ " %")
                    |> Element.el
                        [ Element.Font.color (Element.rgb255 250 250 250)
                        , Element.centerX
                        , Element.centerY
                        ]
                )
            ]


viewInputElement : State -> Element.Element Event
viewInputElement state =
    let
        inputSideElement =
            case state.remainingLessons of
                currentLesson :: _ ->
                    [ [ currentLesson.lesson
                            |> Html.text
                            |> List.singleton
                            |> Html.div
                                [ HA.style "white-space" "pre"
                                , HA.style "font-family" "monospace, monospace"
                                ]
                            |> Element.html
                      ]
                        |> Element.column []
                    , [ Element.text "="
                      , Element.Input.text
                            [ onEnter (UserInputSubmitSolution state.expression)
                            , Element.Font.family [ Element.Font.monospace ]
                            ]
                            { onChange = UserInputChangeExpression
                            , text = state.expression
                            , placeholder = Just (Element.Input.placeholder [] (Element.text "?"))
                            , label = Element.Input.labelHidden "Solution"
                            }
                      ]
                        |> Element.row [ Element.spacing defaultFontSize ]
                    ]
                        |> Element.column [ Element.spacing defaultFontSize ]

                [] ->
                    Element.text "All lessons complete 🎉"
    in
    inputSideElement


computeSolutionFromLessonInContext : Pine.ExpressionContext -> Lesson -> String
computeSolutionFromLessonInContext evaluationContext lesson =
    case ElmInteractive.submissionInInteractiveInPineContext evaluationContext lesson of
        Err error ->
            "Failed to evaluate: " ++ error

        Ok ( _, response ) ->
            case response of
                ElmInteractive.SubmissionResponseNoValue ->
                    "Submission has no return value"

                ElmInteractive.SubmissionResponseValue responseValue ->
                    responseValue.value |> ElmInteractive.elmValueAsExpression


onEnter : event -> Element.Attribute event
onEnter event =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed event

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


defaultFontSize : Int
defaultFontSize =
    16
