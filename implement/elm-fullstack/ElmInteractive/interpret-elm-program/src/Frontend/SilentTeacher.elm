module Frontend.SilentTeacher exposing (State, main)

import Browser
import Browser.Events
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
    { time : Time.Posix
    , evaluationContext : Result String Pine.ExpressionContext
    , trainingSession : TrainingSessionState
    , navigationKey : Browser.Navigation.Key
    }


type TrainingSessionState
    = SessionInProgress
        { remainingLessons : List { lesson : Lesson, solution : String }
        , currentLesson : LessonWorkspace
        , completedLessons : List { lesson : Lesson, solution : String }
        }
    | SessionCompleted


type alias LessonWorkspace =
    { lesson : { lesson : Lesson, solution : String }
    , submissionInput : String
    , wrongSubmissionTime : Maybe Time.Posix
    }


type alias Lesson =
    String


type Event
    = UserInputPrepareSubmission String
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

        trainingSession =
            case lessonsExpressions |> List.map (\lesson -> { lesson = lesson, solution = solutionFromLesson lesson }) of
                currentLesson :: remainingLessons ->
                    SessionInProgress
                        { remainingLessons = remainingLessons
                        , currentLesson = initLessonWorkspace currentLesson
                        , completedLessons = []
                        }

                [] ->
                    SessionCompleted
    in
    ( { time = Time.millisToPosix 0
      , evaluationContext = evaluationContextResult
      , navigationKey = navigationKey
      , trainingSession = trainingSession
      }
    , Cmd.none
    )


initLessonWorkspace : { lesson : Lesson, solution : String } -> LessonWorkspace
initLessonWorkspace lesson =
    { lesson = lesson
    , submissionInput = ""
    , wrongSubmissionTime = Nothing
    }


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
subscriptions state =
    let
        lowFrequency =
            Time.every 200 TimeArrivedEvent
    in
    case state.trainingSession of
        SessionCompleted ->
            Sub.none

        SessionInProgress sessionInProgress ->
            if hintAnimationDurationProgressFromLesson state sessionInProgress.currentLesson == Nothing then
                lowFrequency

            else
                Browser.Events.onAnimationFrame TimeArrivedEvent


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputPrepareSubmission expression ->
            ( stateBefore
                |> updateTrainingSessionCurrentLesson (workspaceUserInputPrepareSubmission expression)
            , Cmd.none
            )

        UserInputSubmitSolution submission ->
            case stateBefore.trainingSession of
                SessionCompleted ->
                    ( stateBefore, Cmd.none )

                SessionInProgress sessionInProgress ->
                    if submission == sessionInProgress.currentLesson.lesson.solution then
                        case sessionInProgress.remainingLessons of
                            [] ->
                                ( { stateBefore | trainingSession = SessionCompleted }
                                , Cmd.none
                                )

                            currentLesson :: remainingLessons ->
                                ( { stateBefore
                                    | trainingSession =
                                        SessionInProgress
                                            { currentLesson = initLessonWorkspace currentLesson
                                            , remainingLessons = remainingLessons
                                            , completedLessons =
                                                sessionInProgress.completedLessons
                                                    ++ [ sessionInProgress.currentLesson.lesson ]
                                            }
                                  }
                                , Cmd.none
                                )

                    else
                        ( stateBefore
                            |> updateTrainingSessionCurrentLesson
                                (\currentLesson ->
                                    { currentLesson
                                        | wrongSubmissionTime = Just stateBefore.time
                                        , submissionInput = ""
                                    }
                                )
                        , Cmd.none
                        )

        UrlRequest _ ->
            ( stateBefore, Cmd.none )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        TimeArrivedEvent time ->
            ( { stateBefore | time = time }, Cmd.none )


workspaceUserInputPrepareSubmission : String -> LessonWorkspace -> LessonWorkspace
workspaceUserInputPrepareSubmission expression workspace =
    { workspace | submissionInput = expression }


updateTrainingSessionCurrentLesson : (LessonWorkspace -> LessonWorkspace) -> State -> State
updateTrainingSessionCurrentLesson updateCurrentLesson state =
    case state.trainingSession of
        SessionCompleted ->
            state

        SessionInProgress sessionInProgress ->
            { state
                | trainingSession =
                    SessionInProgress
                        { sessionInProgress
                            | currentLesson = updateCurrentLesson sessionInProgress.currentLesson
                        }
            }


view : State -> Browser.Document Event
view state =
    let
        ( progressPercent, workspaceElement ) =
            case state.trainingSession of
                SessionCompleted ->
                    ( 100
                    , Element.text "All lessons complete 🎉"
                        |> Element.el [ Element.padding (defaultFontSize * 2) ]
                    )

                SessionInProgress sessionInProgress ->
                    let
                        totalLessonCount =
                            List.length (sessionInProgress.completedLessons ++ sessionInProgress.remainingLessons) + 1
                    in
                    ( (List.length sessionInProgress.completedLessons * 100) // totalLessonCount
                    , viewLessonWorkspace state sessionInProgress.currentLesson
                    )
    in
    { body =
        [ [ workspaceElement
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


viewLessonWorkspace : { a | time : Time.Posix } -> LessonWorkspace -> Element.Element Event
viewLessonWorkspace stateWithTime workspace =
    let
        hintElement =
            hintAnimationDurationProgressFromLesson stateWithTime workspace
                |> Maybe.map
                    (\hintAnimationProgress ->
                        let
                            progressFloat =
                                toFloat hintAnimationProgress.progressMicro * 1.0e-6

                            envelope =
                                max 0 (1.5 - abs (progressFloat * 2 - 1) * 2)
                        in
                        viewInputElement { isHint = True } workspace
                            |> Element.el
                                [ Element.moveRight (sin (progressFloat * 100) * progressFloat * 4 * envelope)
                                , Element.alpha (min 1 (10 - 10 * progressFloat))
                                ]
                    )
                |> Maybe.withDefault Element.none
    in
    [ viewInputElement { isHint = False } workspace
    , hintElement
    ]
        |> Element.row [ Element.spacing (defaultFontSize * 3) ]


viewInputElement : { isHint : Bool } -> LessonWorkspace -> Element.Element Event
viewInputElement { isHint } workspace =
    let
        inputOrHintRow =
            [ Element.text "="
            , if isHint then
                Element.text workspace.lesson.solution
                    |> Element.el [ Element.Font.family [ Element.Font.monospace ] ]

              else
                Element.Input.text
                    [ onEnter (UserInputSubmitSolution workspace.submissionInput)
                    , Element.Font.family [ Element.Font.monospace ]
                    ]
                    { onChange = UserInputPrepareSubmission
                    , text = workspace.submissionInput
                    , placeholder = Just (Element.Input.placeholder [] (Element.text "?"))
                    , label = Element.Input.labelHidden "Solution"
                    }
            ]
                |> Element.row [ Element.spacing defaultFontSize ]
    in
    [ [ workspace.lesson.lesson
            |> Html.text
            |> List.singleton
            |> Html.div
                [ HA.style "white-space" "pre"
                , HA.style "font-family" "monospace, monospace"
                ]
            |> Element.html
      ]
        |> Element.column []
    , inputOrHintRow
        |> Element.el [ Element.centerY ]
        |> Element.el
            (List.concat
                [ [ Element.width (Element.fill |> Element.minimum 160)
                  , Element.height (Element.px (defaultFontSize * 3))
                  ]
                , if isHint then
                    [ Element.Background.color (Element.rgb255 255 106 0) ]

                  else
                    []
                ]
            )
    ]
        |> Element.column [ Element.spacing defaultFontSize ]
        |> Element.el
            [ Element.Background.color (Element.rgb255 116 190 254)
            , Element.padding defaultFontSize
            ]


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


hintAnimationDurationProgressFromLesson : { a | time : Time.Posix } -> LessonWorkspace -> Maybe { progressMicro : Int }
hintAnimationDurationProgressFromLesson stateWithTime workspace =
    workspace.wrongSubmissionTime
        |> Maybe.andThen
            (\wrongSubmissionTime ->
                let
                    wrongSubmissionAge =
                        Time.posixToMillis stateWithTime.time - Time.posixToMillis wrongSubmissionTime

                    progressMicro =
                        wrongSubmissionAge * 1000000 // hintAnimationDurationMs
                in
                if progressMicro <= 1000000 then
                    Just { progressMicro = progressMicro }

                else
                    Nothing
            )


hintAnimationDurationMs : Int
hintAnimationDurationMs =
    2000
