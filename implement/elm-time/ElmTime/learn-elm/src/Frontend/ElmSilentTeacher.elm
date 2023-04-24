module Frontend.ElmSilentTeacher exposing (State, main)

import Browser
import Browser.Dom
import Browser.Events
import Diff
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElmInteractive
import Frontend.ElmExplorer
import Frontend.ElmSilentTeacher.Exercise as Exercise exposing (Exercise, ExerciseChallenge)
import Html
import Html.Attributes as HA
import Html.Events
import Json.Decode
import List.Extra
import Pine
import Random
import Result.Extra
import Svg
import Svg.Attributes
import Task
import Time


evalDelayFromUserInputMilliseconds : Int
evalDelayFromUserInputMilliseconds =
    500


type alias State =
    { time : Time.Posix
    , evaluationContextResult : Result String Pine.EvalContext
    , trainingSession : TrainingSessionState
    }


type TrainingSessionState
    = SessionInProgress SessionInProgressStructure
    | SessionCompleted


type alias SessionInProgressStructure =
    { remainingExercises : List Exercise
    , previousChallenges : List ExerciseChallenge
    , currentExercise : ExerciseWorkspace
    , completedExercises : List Exercise
    , progressBar : AnimatedProgressBar
    }


type alias ExerciseWorkspace =
    { exercise : Exercise
    , challenge : ExerciseWorkspaceChallenge
    }


type alias ExerciseWorkspaceChallenge =
    { challenge : ExerciseChallenge
    , cachedCorrectAnswer : String
    , usersAnswer : WorkspaceAnswer
    , interactive : Maybe InteractiveState
    }


type WorkspaceAnswer
    = WritingAnswer String
    | CheckedAnswer { answer : String, cachedCorrect : Bool }


type Event
    = UserInputWriteAnswer String
    | UserInputCheckAnswer String
    | UserInputContinue
    | UserInputEnterInteractive String
    | TimeArrivedEvent Time.Posix
    | DiscardEvent


type alias AnimatedProgressBar =
    { animationProgressMicro : Int }


type alias InteractiveState =
    { expression : String
    , lastUserInputExpressionTime : Time.Posix
    , lastEvaluatedExpression : Maybe ( String, Result String ElmInteractive.SubmissionResponse )
    }


init : ( State, Cmd Event )
init =
    let
        time =
            Time.millisToPosix 0

        evaluationContextResult =
            ElmInteractive.compileEvalContextForElmInteractive ElmInteractive.DefaultContext

        trainingSession =
            case Exercise.exercises of
                currentExercise :: remainingExercises ->
                    SessionInProgress
                        { remainingExercises = remainingExercises
                        , currentExercise =
                            initExerciseWorkspace
                                { evaluationContextResult = evaluationContextResult, time = time }
                                currentExercise
                                []
                        , previousChallenges = []
                        , completedExercises = []
                        , progressBar = initProgressBar
                        }

                [] ->
                    SessionCompleted
    in
    ( { time = time
      , evaluationContextResult = evaluationContextResult
      , trainingSession = trainingSession
      }
    , Cmd.none
    )


initExerciseWorkspace :
    { a | evaluationContextResult : Result String Pine.EvalContext, time : Time.Posix }
    -> Exercise
    -> List ExerciseChallenge
    -> ExerciseWorkspace
initExerciseWorkspace state exercise previousChallenges =
    let
        ( randomChallenges, randomSeed ) =
            List.repeat 4 0
                |> List.foldl
                    (always
                        (\( aggregate, seed ) ->
                            seed
                                |> Random.step exercise.challengeGenerator
                                |> Tuple.mapFirst (String.trim >> (::) >> (|>) aggregate)
                        )
                    )
                    ( [], Random.initialSeed (Time.posixToMillis state.time) )

        distanceToIdenticalPreviousChallenge c =
            previousChallenges
                |> List.Extra.elemIndex c
                |> Maybe.withDefault 999

        challenge =
            randomChallenges
                |> List.sortBy (distanceToIdenticalPreviousChallenge >> negate)
                |> List.head
                |> Maybe.withDefault (Tuple.first (Random.step exercise.challengeGenerator randomSeed))

        correctAnswer =
            case state.evaluationContextResult of
                Err error ->
                    "Failed to initialize the evaluation context: " ++ error

                Ok evaluationContext ->
                    computeSolutionFromExerciseInContext evaluationContext challenge
    in
    { exercise = exercise
    , challenge =
        { challenge = challenge
        , cachedCorrectAnswer = correctAnswer
        , usersAnswer = WritingAnswer ""
        , interactive = Nothing
        }
    }


main : Program () State Event
main =
    Browser.document
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view >> .document
        }


subscriptions : State -> Sub.Sub Event
subscriptions state =
    let
        maybeOnKeyDownEnter =
            (view state).onKeyDownEnter

        keyDownSubscriptions =
            case maybeOnKeyDownEnter of
                Nothing ->
                    []

                Just onKeyDownEnter ->
                    [ Browser.Events.onKeyDown (keyEventDecoderIsEnter onKeyDownEnter) ]
    in
    timeSubscriptions state
        :: keyDownSubscriptions
        |> Sub.batch


timeSubscriptions : State -> Sub.Sub Event
timeSubscriptions state =
    let
        lowFrequency =
            Time.every 200 TimeArrivedEvent
    in
    case state.trainingSession of
        SessionCompleted ->
            Sub.none

        SessionInProgress sessionInProgress ->
            let
                animationForProgressBar =
                    progressBarAnimationTask sessionInProgress /= Nothing
            in
            if animationForProgressBar then
                Browser.Events.onAnimationFrame TimeArrivedEvent

            else
                lowFrequency


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    let
        ( state, cmd ) =
            updateLessFocusCmd event stateBefore

        sessionShowsInputElement stateWithSession =
            case stateWithSession.trainingSession of
                SessionInProgress sessionInProgress ->
                    case sessionInProgress.currentExercise.challenge.usersAnswer of
                        WritingAnswer _ ->
                            True

                        CheckedAnswer _ ->
                            False

                _ ->
                    False

        focusCmd =
            if sessionShowsInputElement state && not (sessionShowsInputElement stateBefore) then
                Browser.Dom.focus challengeAnswerInputElementId
                    |> Task.attempt (always DiscardEvent)

            else
                Cmd.none
    in
    ( state, [ cmd, focusCmd ] |> Cmd.batch )


updateLessFocusCmd : Event -> State -> ( State, Cmd Event )
updateLessFocusCmd event stateBefore =
    case event of
        UserInputWriteAnswer answer ->
            ( updateTrainingSessionCurrentExercise (workspaceUserInputWriteAnswer answer) stateBefore
            , Cmd.none
            )

        UserInputCheckAnswer answer ->
            ( stateBefore
                |> updateTrainingSessionCurrentExercise
                    (\workspace ->
                        workspaceUserInputCheckAnswer workspace
                            |> Maybe.map ((|>) stateBefore >> (|>) answer)
                            |> Maybe.withDefault workspace
                    )
            , Cmd.none
            )

        UserInputContinue ->
            case stateBefore.trainingSession of
                SessionCompleted ->
                    ( stateBefore, Cmd.none )

                SessionInProgress sessionInProgress ->
                    case sessionInProgress.currentExercise.challenge.usersAnswer of
                        WritingAnswer _ ->
                            ( stateBefore, Cmd.none )

                        CheckedAnswer checkedAnswer ->
                            let
                                previousChallenges =
                                    sessionInProgress.currentExercise.challenge.challenge
                                        :: sessionInProgress.previousChallenges
                            in
                            if checkedAnswer.cachedCorrect then
                                case sessionInProgress.remainingExercises of
                                    [] ->
                                        ( { stateBefore | trainingSession = SessionCompleted }
                                        , Cmd.none
                                        )

                                    currentExercise :: remainingExercises ->
                                        ( { stateBefore
                                            | trainingSession =
                                                SessionInProgress
                                                    { currentExercise =
                                                        initExerciseWorkspace
                                                            stateBefore
                                                            currentExercise
                                                            previousChallenges
                                                    , previousChallenges = previousChallenges
                                                    , remainingExercises = remainingExercises
                                                    , completedExercises =
                                                        sessionInProgress.currentExercise.exercise
                                                            :: sessionInProgress.completedExercises
                                                    , progressBar = sessionInProgress.progressBar
                                                    }
                                          }
                                        , Cmd.none
                                        )

                            else
                                ( { stateBefore
                                    | trainingSession =
                                        SessionInProgress
                                            { sessionInProgress
                                                | currentExercise =
                                                    initExerciseWorkspace
                                                        stateBefore
                                                        sessionInProgress.currentExercise.exercise
                                                        previousChallenges
                                                , previousChallenges = previousChallenges
                                            }
                                  }
                                , Cmd.none
                                )

        UserInputEnterInteractive expression ->
            ( stateBefore
                |> updateTrainingSessionCurrentExercise
                    (\workspace ->
                        workspaceUserInputEnterInteractive workspace
                            |> Maybe.map ((|>) stateBefore >> (|>) expression)
                            |> Maybe.withDefault workspace
                    )
            , Cmd.none
            )

        TimeArrivedEvent time ->
            let
                state =
                    updateTrainingSessionInProgress
                        (updateSessionInProgressTimeArrived { time = time } stateBefore)
                        stateBefore
            in
            ( { state | time = time }
            , Cmd.none
            )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


checkIfAnswerCorrect : State -> ExerciseWorkspaceChallenge -> String -> Bool
checkIfAnswerCorrect state exerciseWorkspace answer =
    let
        answerRepresentations =
            answer
                :: (state.evaluationContextResult
                        |> Result.toMaybe
                        |> Maybe.map
                            (\evaluationContext ->
                                computeSolutionFromExerciseInContext
                                    evaluationContext
                                    answer
                            )
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )

        removeWhitespace =
            String.replace " " ""
    in
    List.member exerciseWorkspace.cachedCorrectAnswer answerRepresentations
        && (removeWhitespace answer == removeWhitespace exerciseWorkspace.cachedCorrectAnswer)


updateSessionInProgressTimeArrived : { time : Time.Posix } -> State -> SessionInProgressStructure -> SessionInProgressStructure
updateSessionInProgressTimeArrived config stateBefore =
    updateSessionInProgressTimeArrivedPartInteractive config stateBefore
        >> updateSessionInProgressTimeArrivedPartAnimation config stateBefore


updateSessionInProgressTimeArrivedPartAnimation : { time : Time.Posix } -> State -> SessionInProgressStructure -> SessionInProgressStructure
updateSessionInProgressTimeArrivedPartAnimation { time } stateBefore sessionInProgress =
    let
        timeDeltaMilli =
            Time.posixToMillis time - Time.posixToMillis stateBefore.time
    in
    sessionInProgress
        |> progressBarAnimationTask
        |> Maybe.map ((|>) { durationMilli = timeDeltaMilli })
        |> Maybe.withDefault sessionInProgress


updateSessionInProgressTimeArrivedPartInteractive :
    { time : Time.Posix }
    -> State
    -> SessionInProgressStructure
    -> SessionInProgressStructure
updateSessionInProgressTimeArrivedPartInteractive { time } stateBefore sessionInProgress =
    let
        currentExerciseBefore =
            sessionInProgress.currentExercise

        challengeBefore =
            currentExerciseBefore.challenge

        interactive =
            challengeBefore.interactive
                |> Maybe.map
                    (updateLastEvaluatedExpression
                        { time = time
                        , evaluationContext = stateBefore.evaluationContextResult
                        }
                    )
    in
    { sessionInProgress
        | currentExercise =
            { currentExerciseBefore
                | challenge =
                    { challengeBefore
                        | interactive = interactive
                    }
            }
    }


progressBarAnimationTask : SessionInProgressStructure -> Maybe ({ durationMilli : Int } -> SessionInProgressStructure)
progressBarAnimationTask sessionInProgress =
    let
        totalExerciseCount =
            List.length sessionInProgress.completedExercises + List.length sessionInProgress.remainingExercises + 1

        currentExerciseCheckedCorrect =
            case sessionInProgress.currentExercise.challenge.usersAnswer of
                WritingAnswer _ ->
                    False

                CheckedAnswer checkedAnswer ->
                    checkedAnswer.cachedCorrect

        completedExercisesCount =
            List.length sessionInProgress.completedExercises
                + (if currentExerciseCheckedCorrect then
                    1

                   else
                    0
                  )

        progressMicro =
            (completedExercisesCount * 1000000) // totalExerciseCount
    in
    animateProgressBar { destMicro = progressMicro } sessionInProgress.progressBar
        |> Maybe.map (\anim -> \time -> { sessionInProgress | progressBar = anim time })


workspaceUserInputWriteAnswer : String -> ExerciseWorkspace -> ExerciseWorkspace
workspaceUserInputWriteAnswer expression workspace =
    case workspace.challenge.usersAnswer of
        CheckedAnswer _ ->
            workspace

        WritingAnswer _ ->
            let
                challengeBefore =
                    workspace.challenge
            in
            { workspace | challenge = { challengeBefore | usersAnswer = WritingAnswer expression } }


workspaceUserInputCheckAnswer : ExerciseWorkspace -> Maybe (State -> String -> ExerciseWorkspace)
workspaceUserInputCheckAnswer workspace =
    case workspace.challenge.usersAnswer of
        CheckedAnswer _ ->
            Nothing

        WritingAnswer answer ->
            if String.isEmpty answer then
                Nothing

            else
                let
                    challengeBefore =
                        workspace.challenge
                in
                Just
                    (\state expression ->
                        { workspace
                            | challenge =
                                { challengeBefore
                                    | usersAnswer =
                                        CheckedAnswer
                                            { answer = expression
                                            , cachedCorrect = checkIfAnswerCorrect state challengeBefore expression
                                            }
                                }
                        }
                    )


workspaceUserInputEnterInteractive : ExerciseWorkspace -> Maybe (State -> String -> ExerciseWorkspace)
workspaceUserInputEnterInteractive workspace =
    case workspace.challenge.usersAnswer of
        WritingAnswer _ ->
            Nothing

        CheckedAnswer _ ->
            Just
                (\stateBefore expression ->
                    let
                        challengeBefore =
                            workspace.challenge

                        interactiveDefault =
                            { expression = expression
                            , lastUserInputExpressionTime = stateBefore.time
                            , lastEvaluatedExpression = Nothing
                            }

                        interactiveBefore =
                            Maybe.withDefault interactiveDefault challengeBefore.interactive
                    in
                    { workspace
                        | challenge =
                            { challengeBefore
                                | interactive =
                                    Just
                                        { interactiveBefore
                                            | expression = expression
                                            , lastUserInputExpressionTime = stateBefore.time
                                        }
                            }
                    }
                )


updateLastEvaluatedExpression :
    { time : Time.Posix, evaluationContext : Result String Pine.EvalContext }
    -> InteractiveState
    -> InteractiveState
updateLastEvaluatedExpression config stateBefore =
    let
        lastUserInputExpressionAgeMilliseconds =
            Time.posixToMillis config.time - Time.posixToMillis stateBefore.lastUserInputExpressionTime

        expression =
            stateBefore.expression
    in
    if Just expression == Maybe.map Tuple.first stateBefore.lastEvaluatedExpression then
        stateBefore

    else if lastUserInputExpressionAgeMilliseconds < evalDelayFromUserInputMilliseconds then
        stateBefore

    else
        let
            lastEvaluatedExpression =
                case config.evaluationContext of
                    Err error ->
                        Just ( expression, Err ("Failed to initialize the evaluation context: " ++ error) )

                    Ok evaluationContext ->
                        Just
                            ( expression
                            , if String.isEmpty (String.trim expression) then
                                Ok { displayText = "" }

                              else
                                ElmInteractive.submissionInInteractiveInPineContext evaluationContext expression
                                    |> Result.map Tuple.second
                            )
        in
        { stateBefore | lastEvaluatedExpression = lastEvaluatedExpression }


updateTrainingSessionCurrentExercise : (ExerciseWorkspace -> ExerciseWorkspace) -> State -> State
updateTrainingSessionCurrentExercise updateCurrentExercise =
    updateTrainingSessionInProgress
        (\sessionInProgress ->
            { sessionInProgress | currentExercise = updateCurrentExercise sessionInProgress.currentExercise }
        )


updateTrainingSessionInProgress : (SessionInProgressStructure -> SessionInProgressStructure) -> State -> State
updateTrainingSessionInProgress updateSessionInProgress state =
    case state.trainingSession of
        SessionCompleted ->
            state

        SessionInProgress sessionInProgress ->
            { state
                | trainingSession = SessionInProgress (updateSessionInProgress sessionInProgress)
            }


view : State -> { document : Browser.Document Event, onKeyDownEnter : Maybe Event }
view state =
    let
        ( progressMicro, workspaceElement ) =
            case state.trainingSession of
                SessionCompleted ->
                    ( 1000000
                    , { visualTree =
                            [ [ Element.text "Congratulations! You have completed the exercises! 🎉"
                                    |> Element.el [ Element.Font.size (defaultFontSize * 2) ]
                              ]
                            , [ Element.text "To continue learning about Elm, you might want to check out the guide at "
                              , linkElementFromHref "https://guide.elm-lang.org"
                              ]
                            , [ Element.text "To work on real apps and games, you can use the development environment at "
                              , linkElementFromHref "https://elm-editor.com"
                              ]
                            ]
                                |> List.map (Element.paragraph [])
                                |> Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing defaultFontSize
                                    ]
                      , onKeyDownEnter = Nothing
                      }
                    )

                SessionInProgress sessionInProgress ->
                    ( sessionInProgress.progressBar.animationProgressMicro
                    , viewExerciseWorkspace sessionInProgress.currentExercise
                    )
    in
    { document =
        { body =
            [ [ [ viewProgressBar { progressMicro = progressMicro }
                ]
                    |> Element.column
                        [ Element.spacing defaultFontSize
                        , Element.width Element.fill
                        , Element.htmlAttribute (HA.style "user-select" "none")
                        ]
              , workspaceElement.visualTree
                    |> Element.el
                        [ Element.Font.size (defaultFontSize + 4)
                        , Element.width Element.fill
                        , Element.height Element.fill
                        ]
              , [ Element.text "To learn about Elm Silent Teacher, see "
                , linkElementFromHref "https://github.com/elm-time/elm-time/blob/main/guide/elm-silent-teacher.md"
                ]
                    |> Element.paragraph [ Element.Font.size (defaultFontSize - 2) ]
              ]
                |> Element.column
                    [ Element.spacing (defaultFontSize * 2)
                    , Element.padding 10
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                |> Element.layout
                    [ Element.Font.size defaultFontSize
                    , Element.Background.color (Element.rgb 1 1 1)
                    , Element.Font.color (Element.rgb 0 0 0)
                    ]
            ]
        , title = "Elm Silent Teacher"
        }
    , onKeyDownEnter = workspaceElement.onKeyDownEnter
    }


viewExerciseWorkspace : ExerciseWorkspace -> { visualTree : Element.Element Event, onKeyDownEnter : Maybe Event }
viewExerciseWorkspace workspace =
    let
        ( answerText, answerIsReadOnly ) =
            case workspace.challenge.usersAnswer of
                WritingAnswer writingAnswer ->
                    ( writingAnswer, False )

                CheckedAnswer checkedAnswer ->
                    ( checkedAnswer.answer, True )

        buttonColorGreen =
            Element.rgb255 88 204 2

        buttonColorRed =
            Element.rgb255 255 75 75

        buttonLabelCheck =
            "Check"

        buildFeedbackElement : Result { submittedAnswer : String, correctSolution : String } () -> Element.Element e
        buildFeedbackElement result =
            let
                icon =
                    answerClassificationIcon { isCorrect = Result.Extra.isOk result }
                        |> Element.el [ Element.width (Element.px 70) ]

                ( title, solutionTextPartsWithUnderlineFlag, color ) =
                    case result of
                        Ok _ ->
                            ( "Great!"
                            , [ ( " ", False ) ]
                            , Element.rgb255 88 167 0
                            )

                        Err error ->
                            let
                                diff =
                                    Diff.diff
                                        (String.toList error.submittedAnswer)
                                        (String.toList error.correctSolution)

                                withUnderlineFlags =
                                    diff
                                        |> List.concatMap
                                            (\charResult ->
                                                case charResult of
                                                    Diff.Removed _ ->
                                                        []

                                                    Diff.Added addedChar ->
                                                        [ ( addedChar, True ) ]

                                                    Diff.NoChange originalChar ->
                                                        [ ( originalChar, False ) ]
                                            )
                            in
                            ( "Correct solution:"
                            , withUnderlineFlags
                                |> List.map (Tuple.mapFirst String.fromChar)
                                |> aggregateConsecutiveStringsWithTag
                            , Element.rgb255 234 43 43
                            )

                textDecorationUnderlineAttributes =
                    [ ( "text-decoration-line", "underline" )
                    , ( "text-decoration-style", "wavy" )
                    , ( "text-underline-offset", "0.4em" )
                    ]
                        |> List.map (\( property, value ) -> Element.htmlAttribute (HA.style property value))

                underlinedCount =
                    solutionTextPartsWithUnderlineFlag
                        |> List.filter Tuple.second
                        |> List.map (Tuple.first >> String.length)
                        |> List.sum

                nonUnderlinedCount =
                    solutionTextPartsWithUnderlineFlag
                        |> List.filter (Tuple.second >> not)
                        |> List.map (Tuple.first >> String.length)
                        |> List.sum

                disableUnderline =
                    nonUnderlinedCount <= underlinedCount

                solutionTextElements =
                    solutionTextPartsWithUnderlineFlag
                        |> List.map
                            (\( textElement, underline ) ->
                                Element.text textElement
                                    |> Element.el
                                        (if underline && not disableUnderline then
                                            textDecorationUnderlineAttributes

                                         else
                                            []
                                        )
                            )
            in
            [ icon
            , [ Element.text title
                    |> Element.el
                        [ Element.Font.size (defaultFontSize * 3 // 2)
                        , Element.Font.bold
                        ]
              , solutionTextElements
                    |> Element.row [ Element.Font.family [ Element.Font.monospace ] ]
              ]
                |> Element.column
                    [ Element.Font.color color
                    , Element.htmlAttribute (HA.style "cursor" "default")
                    , Element.spacing (defaultFontSize // 2)
                    ]
            ]
                |> Element.row [ Element.spacing defaultFontSize ]

        ( ( ( buttonEnabledBackgroundColor, feedbackBackgroundColor, feedbackTextElement ), buttonText ), buttonOnPress ) =
            case workspace.challenge.usersAnswer of
                WritingAnswer answer ->
                    ( ( ( buttonColorGreen
                        , Element.rgba 0 0 0 0
                        , buildFeedbackElement (Ok ())
                            |> Element.el [ Element.transparent True ]
                        )
                      , buttonLabelCheck
                      )
                    , if workspaceUserInputCheckAnswer workspace /= Nothing then
                        Just (UserInputCheckAnswer answer)

                      else
                        Nothing
                    )

                CheckedAnswer checkedAnswer ->
                    ( ( if checkedAnswer.cachedCorrect then
                            ( buttonColorGreen
                            , Element.rgb255 215 255 184
                            , buildFeedbackElement (Ok ())
                            )

                        else
                            ( buttonColorRed
                            , Element.rgb255 255 223 223
                            , buildFeedbackElement
                                (Err
                                    { submittedAnswer = checkedAnswer.answer
                                    , correctSolution = workspace.challenge.cachedCorrectAnswer
                                    }
                                )
                            )
                      , "Continue"
                      )
                    , Just UserInputContinue
                    )

        feedbackElement =
            [ feedbackTextElement
                |> Element.el
                    [ Element.alignLeft
                    , Element.padding defaultFontSize
                    ]
            , feedbackButton
                { labelText = String.toUpper buttonText
                , onPress = buttonOnPress
                , enabledBackgroundColor = buttonEnabledBackgroundColor
                }
                |> Element.el [ Element.alignRight ]
            ]
                |> Element.row
                    [ Element.width (Element.fill |> Element.maximum 900)
                    , Element.centerX
                    , Element.paddingXY (defaultFontSize * 2) defaultFontSize
                    , Element.spacing (defaultFontSize * 2)
                    ]
                |> Element.el
                    [ Element.width Element.fill
                    , Element.Background.color feedbackBackgroundColor
                    ]

        enterInteractiveButton =
            if workspaceUserInputEnterInteractive workspace == Nothing then
                Nothing

            else
                feedbackButton
                    { labelText = "Explore"
                    , onPress = Just (UserInputEnterInteractive workspace.challenge.challenge)
                    , enabledBackgroundColor = buttonEnabledBackgroundColor
                    }
                    |> Element.el [ Element.centerX, Element.padding defaultFontSize ]
                    |> Just

        ( explorationElement, onKeyDownEnter ) =
            case workspace.challenge.interactive of
                Nothing ->
                    ( Maybe.withDefault Element.none enterInteractiveButton
                    , buttonOnPress
                    )

                Just interactive ->
                    ( [ viewInteractive interactive ]
                        |> Element.column
                            [ Element.spacing (defaultFontSize // 2)
                            , Element.Background.color (Element.rgba 0.13 0.13 0.13 0.1)
                            , Element.centerX
                            , Element.centerY
                            , Element.Border.width 2
                            , Element.Border.color <| Element.rgba 1 1 1 0.3
                            , Element.width Element.fill
                            ]
                    , Nothing
                    )
    in
    { visualTree =
        [ viewChallengeElement
            { challenge = workspace.challenge.challenge
            , answerText = answerText
            , answerIsReadOnly = answerIsReadOnly
            }
            |> Element.el [ Element.centerX ]
        , feedbackElement
        , explorationElement
        ]
            |> Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing defaultFontSize
                ]
    , onKeyDownEnter = onKeyDownEnter
    }


aggregateConsecutiveStringsWithTag : List ( appendable, tag ) -> List ( appendable, tag )
aggregateConsecutiveStringsWithTag =
    List.foldr
        (\( nextString, nextTag ) aggregate ->
            case aggregate of
                [] ->
                    [ ( nextString, nextTag ) ]

                ( lastString, lastTag ) :: previous ->
                    if lastTag == nextTag then
                        ( nextString ++ lastString, lastTag ) :: previous

                    else
                        ( nextString, nextTag ) :: aggregate
        )
        []


viewInteractive : InteractiveState -> Element.Element Event
viewInteractive state =
    Frontend.ElmExplorer.viewInteractive
        { userInputExpression = UserInputEnterInteractive }
        state


feedbackButton : { labelText : String, onPress : Maybe e, enabledBackgroundColor : Element.Color } -> Element.Element e
feedbackButton { labelText, onPress, enabledBackgroundColor } =
    let
        isEnabled =
            onPress /= Nothing

        ( fontColor, backgroundColor ) =
            if isEnabled then
                ( Element.rgb 1 1 1, enabledBackgroundColor )

            else
                ( Element.rgb 0.6 0.6 0.6, Element.rgb 0.8 0.8 0.8 )
    in
    Element.Input.button
        [ Element.Font.letterSpacing (toFloat defaultFontSize / 10)

        -- , Element.Font.family [ Element.Font.typeface "din-round", Element.Font.typeface "sans-serif" ]
        , Element.Font.bold
        , Element.Font.color fontColor
        , Element.Background.color backgroundColor
        , Element.htmlAttribute (HA.style "border-radius" "1em")
        ]
        { label =
            Element.text labelText
                |> Element.el [ Element.paddingXY (defaultFontSize * 3) defaultFontSize ]
        , onPress = onPress
        }


viewChallengeElement :
    { challenge : ExerciseChallenge, answerText : String, answerIsReadOnly : Bool }
    -> Element.Element Event
viewChallengeElement { challenge, answerText, answerIsReadOnly } =
    let
        answerInputElement =
            Element.Input.text
                [ onEnter (UserInputCheckAnswer answerText)
                , Element.Font.family [ Element.Font.monospace ]
                , Element.Input.focusedOnLoad
                , Element.htmlAttribute (HA.id challengeAnswerInputElementId)
                , Element.htmlAttribute (HA.readonly answerIsReadOnly)
                , Element.alpha
                    (if answerIsReadOnly then
                        0.4

                     else
                        0.9
                    )
                ]
                { onChange = UserInputWriteAnswer
                , text = answerText
                , placeholder = Just (Element.Input.placeholder [] (Element.text "?"))
                , label = Element.Input.labelHidden "Solution"
                }

        inputOrHintRow =
            [ Element.text "="
            , answerInputElement
            ]
                |> Element.row [ Element.spacing defaultFontSize ]
    in
    [ [ challenge
            |> Html.text
            |> List.singleton
            |> Html.div
                [ HA.style "white-space" "pre"
                , HA.style "font-family" "monospace, monospace"
                , HA.style "line-height" "normal"
                ]
            |> Element.html
      ]
        |> Element.column []
    , inputOrHintRow
        |> Element.el [ Element.centerY ]
        |> Element.el
            [ Element.width (Element.fill |> Element.minimum 160)
            , Element.height (Element.px (defaultFontSize * 3))
            ]
    ]
        |> Element.column [ Element.spacing defaultFontSize ]
        |> Element.el
            [ Element.Background.color (Element.rgb255 116 190 254)
            , Element.padding defaultFontSize
            ]


viewProgressBar : { progressMicro : Int } -> Element.Element e
viewProgressBar { progressMicro } =
    [ Html.div [] []
        |> Element.html
        |> Element.el
            [ Element.width (Element.fillPortion progressMicro)
            , Element.height Element.fill
            , Element.Background.color (Element.rgb255 88 204 2)
            , Element.htmlAttribute (HA.style "border-radius" "1em")
            ]
    , Html.div [] []
        |> Element.html
        |> Element.el
            [ Element.width (Element.fillPortion (1000000 - progressMicro)) ]
    ]
        |> Element.row
            [ Element.width Element.fill
            , Element.height (Element.px (defaultFontSize * 2))
            ]
        |> Element.el
            [ Element.width Element.fill
            , Element.Background.color (Element.rgb255 229 229 229)
            , Element.htmlAttribute (HA.style "border-radius" "1em")
            , Element.inFront
                (Element.text (String.fromInt (progressMicro // 10000) ++ " %")
                    |> Element.el
                        [ Element.Font.color (Element.rgba 0 0 0 0.8)
                        , Element.Font.bold
                        , Element.centerX
                        , Element.centerY
                        ]
                )
            ]


computeSolutionFromExerciseInContext : Pine.EvalContext -> ExerciseChallenge -> String
computeSolutionFromExerciseInContext evaluationContext exerciseChallenge =
    case ElmInteractive.submissionInInteractiveInPineContext evaluationContext exerciseChallenge of
        Err error ->
            "Failed to evaluate: " ++ error

        Ok ( _, response ) ->
            response.displayText


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


challengeAnswerInputElementId : String
challengeAnswerInputElementId =
    "challenge-answer-input"


defaultFontSize : Int
defaultFontSize =
    16


linkElementFromHref : String -> Element.Element event
linkElementFromHref href =
    linkElementFromUrlAndTextLabel { url = href, labelText = href }


linkElementFromUrlAndTextLabel : { url : String, labelText : String } -> Element.Element event
linkElementFromUrlAndTextLabel { url, labelText } =
    Element.link
        [ -- https://github.com/mdgriffith/elm-ui/issues/158#issuecomment-624231895
          Element.Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Element.Border.color <| Element.rgba 0 0 0 0
        , Element.mouseOver [ Element.Border.color <| Element.rgba 0.5 0.5 0.5 0.7 ]
        ]
        { url = url
        , label = labelText |> Element.text
        }


initProgressBar : AnimatedProgressBar
initProgressBar =
    { animationProgressMicro = 0 }


animateProgressBar :
    { destMicro : Int }
    -> AnimatedProgressBar
    -> Maybe ({ durationMilli : Int } -> AnimatedProgressBar)
animateProgressBar { destMicro } progressBar =
    let
        maxDeltaMicro =
            destMicro - progressBar.animationProgressMicro
    in
    if maxDeltaMicro == 0 then
        Nothing

    else
        Just
            (\{ durationMilli } ->
                let
                    speedupByDelta =
                        (maxDeltaMicro // 300)
                            |> abs
                            |> toFloat
                            |> sqrt
                            |> (*) 7
                            |> round
                            |> max 1

                    deltaMicroAbs =
                        maxDeltaMicro
                            |> abs
                            |> min durationMilli
                            |> (*) speedupByDelta
                            |> min (abs maxDeltaMicro)

                    animationProgressMicro =
                        progressBar.animationProgressMicro
                            + (deltaMicroAbs
                                * (if maxDeltaMicro < 0 then
                                    -1

                                   else
                                    1
                                  )
                              )
                in
                { progressBar | animationProgressMicro = animationProgressMicro }
            )


answerClassificationIcon : { isCorrect : Bool } -> Element.Element e
answerClassificationIcon { isCorrect } =
    let
        ( classElementPathData, classElementColor ) =
            if isCorrect then
                ( "M -19,3 L -7,15 M -7,15 L 19,-11", "#8DBC00" )

            else
                ( "M -15,-15 L 15,15 M 15,-15 L -15,15", "#EC0B1B" )
    in
    [ Svg.circle
        [ Svg.Attributes.r "50"
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.fill "white"
        ]
        []
    , Svg.path
        [ Svg.Attributes.d classElementPathData
        , Svg.Attributes.stroke classElementColor
        , Svg.Attributes.strokeWidth "14"
        , Svg.Attributes.strokeLinecap "round"
        ]
        []
    ]
        |> Svg.svg [ Svg.Attributes.viewBox "-50 -50 100 100" ]
        |> Element.html


keyEventDecoderIsEnter : e -> Json.Decode.Decoder e
keyEventDecoderIsEnter event =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                if key == "Enter" then
                    Json.Decode.succeed event

                else
                    Json.Decode.fail "Not the enter key"
            )
