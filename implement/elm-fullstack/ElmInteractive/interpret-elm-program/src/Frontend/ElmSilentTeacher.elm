module Frontend.ElmSilentTeacher exposing (State, main)

import Browser
import Browser.Dom
import Browser.Events
import Element
import Element.Background
import Element.Border
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
import Random.List
import Random.String
import Result.Extra
import String.Extra
import Svg
import Svg.Attributes
import Task
import Time


type alias State =
    { time : Time.Posix
    , evaluationContextResult : Result String Pine.EvalContext
    , trainingSession : TrainingSessionState
    }


type TrainingSessionState
    = SessionInProgress SessionInProgressStructure
    | SessionCompleted


type alias SessionInProgressStructure =
    { remainingLessons : List Lesson
    , currentLesson : LessonWorkspace
    , completedLessons : List Lesson
    , progressBar : AnimatedProgressBar
    }


type alias LessonWorkspace =
    { lesson : Lesson
    , challenge : LessonWorkspaceChallenge
    }


type alias Lesson =
    { challengeGenerator : Random.Generator LessonChallenge }


type alias LessonChallenge =
    String


type alias LessonWorkspaceChallenge =
    { challenge : LessonChallenge
    , cachedCorrectAnswer : String
    , usersAnswer : WorkspaceAnswer
    }


type WorkspaceAnswer
    = WritingAnswer String
    | CheckedAnswer { answer : String, cachedCorrect : Bool }


type Event
    = UserInputWriteAnswer String
    | UserInputCheckAnswer String
    | UserInputContinue
    | TimeArrivedEvent Time.Posix
    | DiscardEvent


type alias AnimatedProgressBar =
    { animationProgressMicro : Int }


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
    , lessonSimplestNamedFunctionChallenge
    , lessonListLength
    , lessonListHead
    , lessonListDrop
    , lessonPipelineChallenge
    ]
        |> List.concatMap (List.repeat 3)
        |> List.map Lesson


lessonSimplestNamedFunctionChallenge : Random.Generator LessonChallenge
lessonSimplestNamedFunctionChallenge =
    Random.map2
        (\x y ->
            """
let
    hello a =
        a + """ ++ String.fromInt x ++ """
in
hello """ ++ String.fromInt y
        )
        (Random.int 0 9)
        (Random.int 0 9)


lessonListLength : Random.Generator LessonChallenge
lessonListLength =
    Random.map2
        (\randomList length ->
            "List.length [ " ++ (String.join ", " (List.take length randomList) ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.map String.fromInt))
        (Random.int 0 5)


lessonListHead : Random.Generator LessonChallenge
lessonListHead =
    Random.map2
        (\randomList length ->
            "List.head [ " ++ (String.join ", " (List.take length randomList) ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.map String.fromInt))
        (Random.int 0 3)


lessonListDrop : Random.Generator LessonChallenge
lessonListDrop =
    Random.map2
        (\randomList dropCount ->
            "List.drop " ++ String.fromInt dropCount ++ " [ " ++ (String.join ", " randomList ++ " ]")
        )
        (List.range 0 9 |> Random.List.shuffle |> Random.map (List.take 3 >> List.map String.fromInt))
        (Random.int 0 4)


lessonPipelineChallenge : Random.Generator LessonChallenge
lessonPipelineChallenge =
    [ "pizza", "lasagna", "risotto", "focaccia", "arancino", "tiramisu" ]
        |> Random.List.shuffle
        |> Random.map (List.take 3 >> List.map (String.Extra.surround "\"") >> String.join ", ")
        |> Random.andThen
            (\list ->
                Random.int 1 2
                    |> Random.map (\dropCount -> "[ " ++ list ++ """ ]
|> List.drop """ ++ String.fromInt dropCount ++ """
|> List.head
""")
            )


init : ( State, Cmd Event )
init =
    let
        time =
            Time.millisToPosix 0

        evaluationContextResult =
            ElmInteractive.pineEvalContextForElmInteractive ElmInteractive.DefaultContext

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
                        , progressBar = initProgressBar
                        }

                [] ->
                    SessionCompleted
    in
    ( { time = Time.millisToPosix 0
      , evaluationContextResult = evaluationContextResult
      , trainingSession = trainingSession
      }
    , Cmd.none
    )


initLessonWorkspace :
    { a | evaluationContextResult : Result String Pine.EvalContext, time : Time.Posix }
    -> Lesson
    -> LessonWorkspace
initLessonWorkspace state lesson =
    let
        challenge =
            Random.initialSeed (Time.posixToMillis state.time)
                |> Random.step lesson.challengeGenerator
                |> Tuple.first
                |> String.trim

        correctAnswer =
            case state.evaluationContextResult of
                Err error ->
                    "Failed to initialize the evaluation context: " ++ error

                Ok evaluationContext ->
                    computeSolutionFromLessonInContext evaluationContext challenge
    in
    { lesson = lesson
    , challenge =
        { challenge = challenge
        , cachedCorrectAnswer = correctAnswer
        , usersAnswer = WritingAnswer ""
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
                    case sessionInProgress.currentLesson.challenge.usersAnswer of
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
            ( updateTrainingSessionCurrentLesson (workspaceUserInputWriteAnswer answer) stateBefore
            , Cmd.none
            )

        UserInputCheckAnswer answer ->
            ( stateBefore
                |> updateTrainingSessionCurrentLesson
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
                    case sessionInProgress.currentLesson.challenge.usersAnswer of
                        WritingAnswer _ ->
                            ( stateBefore, Cmd.none )

                        CheckedAnswer checkedAnswer ->
                            if checkedAnswer.cachedCorrect then
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
                                                    , progressBar = sessionInProgress.progressBar
                                                    }
                                          }
                                        , Cmd.none
                                        )

                            else
                                ( stateBefore
                                    |> updateTrainingSessionCurrentLesson
                                        (\currentLesson -> initLessonWorkspace stateBefore currentLesson.lesson)
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


checkIfAnswerCorrect : State -> LessonWorkspaceChallenge -> String -> Bool
checkIfAnswerCorrect state lessonWorkspace answer =
    let
        answerRepresentations =
            answer
                :: (state.evaluationContextResult
                        |> Result.toMaybe
                        |> Maybe.map
                            (\evaluationContext ->
                                computeSolutionFromLessonInContext
                                    evaluationContext
                                    answer
                            )
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )

        removeWhitespace =
            String.replace " " ""
    in
    List.member lessonWorkspace.cachedCorrectAnswer answerRepresentations
        && (removeWhitespace answer == removeWhitespace lessonWorkspace.cachedCorrectAnswer)


updateSessionInProgressTimeArrived : { time : Time.Posix } -> State -> SessionInProgressStructure -> SessionInProgressStructure
updateSessionInProgressTimeArrived { time } stateBefore sessionInProgress =
    let
        timeDeltaMilli =
            Time.posixToMillis time - Time.posixToMillis stateBefore.time
    in
    sessionInProgress
        |> progressBarAnimationTask
        |> Maybe.map ((|>) { durationMilli = timeDeltaMilli })
        |> Maybe.withDefault sessionInProgress


progressBarAnimationTask : SessionInProgressStructure -> Maybe ({ durationMilli : Int } -> SessionInProgressStructure)
progressBarAnimationTask sessionInProgress =
    let
        totalLessonCount =
            List.length (sessionInProgress.completedLessons ++ sessionInProgress.remainingLessons) + 1

        currentLessonCheckedCorrect =
            case sessionInProgress.currentLesson.challenge.usersAnswer of
                WritingAnswer _ ->
                    False

                CheckedAnswer checkedAnswer ->
                    checkedAnswer.cachedCorrect

        completedLessonsCount =
            List.length sessionInProgress.completedLessons
                + (if currentLessonCheckedCorrect then
                    1

                   else
                    0
                  )

        progressMicro =
            (completedLessonsCount * 1000000) // totalLessonCount
    in
    animateProgressBar { destMicro = progressMicro } sessionInProgress.progressBar
        |> Maybe.map (\anim -> \time -> { sessionInProgress | progressBar = anim time })


workspaceUserInputWriteAnswer : String -> LessonWorkspace -> LessonWorkspace
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


workspaceUserInputCheckAnswer : LessonWorkspace -> Maybe (State -> String -> LessonWorkspace)
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


updateTrainingSessionCurrentLesson : (LessonWorkspace -> LessonWorkspace) -> State -> State
updateTrainingSessionCurrentLesson updateCurrentLesson =
    updateTrainingSessionInProgress
        (\sessionInProgress ->
            { sessionInProgress | currentLesson = updateCurrentLesson sessionInProgress.currentLesson }
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
                            Element.text "All lessons complete 🎉"
                                |> Element.el [ Element.padding (defaultFontSize * 2) ]
                      , onKeyDownEnter = Nothing
                      }
                    )

                SessionInProgress sessionInProgress ->
                    ( sessionInProgress.progressBar.animationProgressMicro
                    , viewLessonWorkspace sessionInProgress.currentLesson
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
                , linkElementFromHref "https://github.com/elm-fullstack/elm-fullstack/blob/main/guide/elm-silent-teacher.md"
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


viewLessonWorkspace : LessonWorkspace -> { visualTree : Element.Element Event, onKeyDownEnter : Maybe Event }
viewLessonWorkspace workspace =
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

        buildFeedbackElement : Result { correctSolution : String } () -> Element.Element e
        buildFeedbackElement result =
            let
                icon =
                    answerClassificationIcon { isCorrect = Result.Extra.isOk result }
                        |> Element.el [ Element.width (Element.px 70) ]

                ( title, correctSolution, color ) =
                    case result of
                        Ok _ ->
                            ( "Great!", " ", Element.rgb255 88 167 0 )

                        Err error ->
                            ( "Correct solution:", error.correctSolution, Element.rgb255 234 43 43 )
            in
            [ icon
            , [ Element.text title
                    |> Element.el
                        [ Element.Font.size (defaultFontSize * 3 // 2)
                        , Element.Font.bold
                        ]
              , Element.text correctSolution
                    |> Element.el [ Element.Font.family [ Element.Font.monospace ] ]
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
                            , buildFeedbackElement (Err { correctSolution = workspace.challenge.cachedCorrectAnswer })
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
                    [ Element.width Element.fill
                    , Element.paddingXY (defaultFontSize * 2) defaultFontSize
                    ]
                |> Element.el
                    [ Element.width Element.fill
                    , Element.alignBottom
                    , Element.Background.color feedbackBackgroundColor
                    ]
    in
    { visualTree =
        [ viewChallengeElement
            { challenge = workspace.challenge.challenge
            , answerText = answerText
            , answerIsReadOnly = answerIsReadOnly
            }
            |> Element.el [ Element.centerX ]
        , feedbackElement
        ]
            |> Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing defaultFontSize
                ]
    , onKeyDownEnter = buttonOnPress
    }


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
    { challenge : LessonChallenge, answerText : String, answerIsReadOnly : Bool }
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


computeSolutionFromLessonInContext : Pine.EvalContext -> LessonChallenge -> String
computeSolutionFromLessonInContext evaluationContext lessonChallenge =
    case ElmInteractive.submissionInInteractiveInPineContext evaluationContext lessonChallenge of
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
