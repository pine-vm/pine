module Frontend.SilentTeacher exposing (State, main)

import Browser
import Browser.Dom
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
import Random
import Random.Char
import Random.String
import Task
import Time
import Url


type alias State =
    { time : Time.Posix
    , evaluationContextResult : Result String Pine.ExpressionContext
    , trainingSession : TrainingSessionState
    , navigationKey : Browser.Navigation.Key
    }


type TrainingSessionState
    = SessionInProgress
        { remainingLessons : List Lesson
        , currentLesson : LessonWorkspace
        , completedLessons : List Lesson
        }
    | SessionCompleted


type alias LessonWorkspace =
    { lesson : Lesson
    , challenge : LessonWorkspaceChallenge
    }


type alias Lesson =
    { challengeGenerator : Random.Generator LessonChallenge }


type alias LessonChallenge =
    String


type LessonWorkspaceChallenge
    = EditingSubmission { challenge : ChallengeCache, submissionInput : String }
    | FailedSubmission { time : Time.Posix, challenge : ChallengeCache }


type alias ChallengeCache =
    { challenge : LessonChallenge, solution : String }


type Event
    = UserInputPrepareSubmission String
    | UserInputSubmitSolution String
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | TimeArrivedEvent Time.Posix
    | DiscardEvent


lessons : List Lesson
lessons =
    [ Random.map2
        (\x y ->
            String.fromInt x ++ " + " ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)
    , Random.map2
        (\x y ->
            String.fromInt x ++ " - " ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)
    , Random.map2
        (\x y ->
            String.fromInt x ++ " * " ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)
    , Random.map2
        (\x y ->
            """
let
    a = """ ++ String.fromInt x ++ """
in
    a + """ ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)
    , Random.map2
        (\x y ->
            """
let
    a = \"""" ++ x ++ """"
    b = \"""" ++ y ++ """"
in
    a ++ b"""
        )
        (Random.String.string 2 Random.Char.lowerCaseLatin)
        (Random.String.string 1 Random.Char.lowerCaseLatin)
    ]
        ++ ([ "List.length [ 1, 3 ]"
            , """List.head []"""
            , """List.head [ "a", "b" ]"""
            , """List.drop 1 [ "a", "b", "c" ]"""
            ]
                |> List.map String.trim
                |> List.map Random.constant
           )
        |> List.map Lesson


init : Url.Url -> Browser.Navigation.Key -> ( State, Cmd Event )
init url navigationKey =
    let
        time =
            Time.millisToPosix 0

        evaluationContextResult =
            ElmInteractive.pineExpressionContextForElmInteractive ElmInteractive.DefaultContext

        trainingSession =
            case lessons of
                currentLesson :: remainingLessons ->
                    SessionInProgress
                        { remainingLessons = remainingLessons
                        , currentLesson =
                            initLessonWorkspace
                                { evaluationContextResult = evaluationContextResult, time = time }
                                currentLesson
                        , completedLessons = []
                        }

                [] ->
                    SessionCompleted
    in
    ( { time = Time.millisToPosix 0
      , evaluationContextResult = evaluationContextResult
      , navigationKey = navigationKey
      , trainingSession = trainingSession
      }
    , Cmd.none
    )


initLessonWorkspace :
    { a | evaluationContextResult : Result String Pine.ExpressionContext, time : Time.Posix }
    -> Lesson
    -> LessonWorkspace
initLessonWorkspace state lesson =
    { lesson = lesson
    , challenge = EditingSubmission { challenge = generateChallenge state lesson, submissionInput = "" }
    }


generateChallenge :
    { a | evaluationContextResult : Result String Pine.ExpressionContext, time : Time.Posix }
    -> Lesson
    -> ChallengeCache
generateChallenge state lesson =
    let
        challenge =
            Random.initialSeed (Time.posixToMillis state.time)
                |> Random.step lesson.challengeGenerator
                |> Tuple.first
                |> String.trim

        solution =
            case state.evaluationContextResult of
                Err error ->
                    "Failed to initialize the evaluation context: " ++ error

                Ok evaluationContext ->
                    computeSolutionFromLessonInContext evaluationContext challenge
    in
    { challenge = challenge
    , solution = solution
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
            case sessionInProgress.currentLesson.challenge of
                EditingSubmission _ ->
                    lowFrequency

                FailedSubmission _ ->
                    Browser.Events.onAnimationFrame TimeArrivedEvent


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    let
        ( state, cmd ) =
            updateLessFocusCmd event stateBefore

        sessionShowsInputElement stateWithSession =
            case stateWithSession.trainingSession of
                SessionInProgress sessionInProgress ->
                    case sessionInProgress.currentLesson.challenge of
                        EditingSubmission _ ->
                            True

                        _ ->
                            False

                _ ->
                    False

        focusCmd =
            if sessionShowsInputElement state && not (sessionShowsInputElement stateBefore) then
                Browser.Dom.focus challengeSubmissionInputElementId
                    |> Task.attempt (always DiscardEvent)

            else
                Cmd.none
    in
    ( state, [ cmd, focusCmd ] |> Cmd.batch )


updateLessFocusCmd : Event -> State -> ( State, Cmd Event )
updateLessFocusCmd event stateBefore =
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
                    case sessionInProgress.currentLesson.challenge of
                        FailedSubmission _ ->
                            ( stateBefore, Cmd.none )

                        EditingSubmission editingSubmission ->
                            let
                                submissionRepresentations =
                                    submission
                                        :: (stateBefore.evaluationContextResult
                                                |> Result.toMaybe
                                                |> Maybe.map
                                                    (\evaluationContext ->
                                                        computeSolutionFromLessonInContext evaluationContext submission
                                                    )
                                                |> Maybe.map List.singleton
                                                |> Maybe.withDefault []
                                           )
                            in
                            if List.member editingSubmission.challenge.solution submissionRepresentations then
                                case sessionInProgress.remainingLessons of
                                    [] ->
                                        ( { stateBefore | trainingSession = SessionCompleted }
                                        , Cmd.none
                                        )

                                    currentLesson :: remainingLessons ->
                                        ( { stateBefore
                                            | trainingSession =
                                                SessionInProgress
                                                    { currentLesson = initLessonWorkspace stateBefore currentLesson
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
                                                | challenge =
                                                    FailedSubmission
                                                        { time = stateBefore.time
                                                        , challenge = editingSubmission.challenge
                                                        }
                                            }
                                        )
                                , Cmd.none
                                )

        UrlRequest _ ->
            ( stateBefore, Cmd.none )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        TimeArrivedEvent time ->
            let
                state =
                    { stateBefore | time = time }
            in
            ( state |> updateTrainingSessionCurrentLesson (updateLessonWorkspaceTimeArrived state)
            , Cmd.none
            )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


updateLessonWorkspaceTimeArrived : State -> LessonWorkspace -> LessonWorkspace
updateLessonWorkspaceTimeArrived state lessonWorkspace =
    let
        challenge =
            case lessonWorkspace.challenge of
                EditingSubmission _ ->
                    lessonWorkspace.challenge

                FailedSubmission failedSubmission ->
                    let
                        failedSubmissionAge =
                            Time.posixToMillis state.time - Time.posixToMillis failedSubmission.time
                    in
                    if failedSubmissionAge < failedSubmissionAnimationDurationMs then
                        lessonWorkspace.challenge

                    else
                        EditingSubmission
                            { challenge = generateChallenge state lessonWorkspace.lesson
                            , submissionInput = ""
                            }
    in
    { lessonWorkspace | challenge = challenge }


workspaceUserInputPrepareSubmission : String -> LessonWorkspace -> LessonWorkspace
workspaceUserInputPrepareSubmission expression workspace =
    case workspace.challenge of
        FailedSubmission _ ->
            workspace

        EditingSubmission editingSubmission ->
            { workspace | challenge = EditingSubmission { editingSubmission | submissionInput = expression } }


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
        ( challengeElementConfig, animation ) =
            case workspace.challenge of
                EditingSubmission editingSubmission ->
                    ( { isEditingSubmission = Just editingSubmission.submissionInput
                      , challengeCache = editingSubmission.challenge
                      }
                    , { moveRight = 0, opacity = 1 }
                    )

                FailedSubmission failedSubmission ->
                    let
                        failedSubmissionAge =
                            Time.posixToMillis stateWithTime.time - Time.posixToMillis failedSubmission.time

                        animationProgressMicro =
                            failedSubmissionAge * 1000000 // failedSubmissionAnimationDurationMs

                        progressFloat =
                            toFloat animationProgressMicro * 1.0e-6

                        envelope =
                            max 0 (1.5 - abs (progressFloat * 2 - 1) * 2)
                    in
                    ( { isEditingSubmission = Nothing
                      , challengeCache = failedSubmission.challenge
                      }
                    , { moveRight = sin (progressFloat * 100) * progressFloat * 4 * envelope
                      , opacity = min 1 (10 - 10 * progressFloat)
                      }
                    )
    in
    viewChallengeElement challengeElementConfig
        |> Element.el
            [ Element.moveRight animation.moveRight
            , Element.alpha animation.opacity
            ]


viewChallengeElement :
    { challengeCache : ChallengeCache, isEditingSubmission : Maybe String }
    -> Element.Element Event
viewChallengeElement { challengeCache, isEditingSubmission } =
    let
        ( isHint, submissionInput ) =
            case isEditingSubmission of
                Nothing ->
                    ( True, "" )

                Just text ->
                    ( False, text )

        hintElement =
            Element.text challengeCache.solution
                |> Element.el
                    [ Element.Font.family [ Element.Font.monospace ]
                    , Element.centerY
                    ]

        submissionInputElement =
            Element.Input.text
                [ onEnter (UserInputSubmitSolution submissionInput)
                , Element.Font.family [ Element.Font.monospace ]
                , Element.Input.focusedOnLoad
                , Element.htmlAttribute (HA.id challengeSubmissionInputElementId)
                , Element.transparent isHint
                ]
                { onChange = UserInputPrepareSubmission
                , text = submissionInput
                , placeholder = Just (Element.Input.placeholder [] (Element.text "?"))
                , label = Element.Input.labelHidden "Solution"
                }

        inputPartAfterEquals =
            if isHint then
                submissionInputElement
                    |> Element.el [ Element.inFront hintElement ]

            else
                submissionInputElement

        inputOrHintRow =
            [ Element.text "="
            , inputPartAfterEquals
            ]
                |> Element.row [ Element.spacing defaultFontSize ]
    in
    [ [ challengeCache.challenge
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


computeSolutionFromLessonInContext : Pine.ExpressionContext -> LessonChallenge -> String
computeSolutionFromLessonInContext evaluationContext lessonChallenge =
    case ElmInteractive.submissionInInteractiveInPineContext evaluationContext lessonChallenge of
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


challengeSubmissionInputElementId : String
challengeSubmissionInputElementId =
    "challenge-submission-input"


defaultFontSize : Int
defaultFontSize =
    16


failedSubmissionAnimationDurationMs : Int
failedSubmissionAnimationDurationMs =
    2000
