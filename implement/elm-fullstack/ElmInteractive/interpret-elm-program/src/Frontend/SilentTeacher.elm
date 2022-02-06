module Frontend.SilentTeacher exposing (State, main)

import Browser
import Browser.Navigation
import Element
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
                      , Element.Input.text [ onEnter (UserInputSubmitSolution state.expression) ]
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
    { body =
        [ [ inputSideElement
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
