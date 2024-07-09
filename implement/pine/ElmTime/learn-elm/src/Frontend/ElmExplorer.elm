module Frontend.ElmExplorer exposing
    ( CompilationExplorerViewState
    , State
    , main
    , viewInteractive
    )

import Browser
import Dict
import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Region
import ElmCompiler
import ElmCompilerConstruction
import ElmInteractive
import ElmInteractiveParser
import FirCompiler
import FontAwesome
import FontAwesome.Solid
import Frontend.BrowserApplicationInitWithTime
import Html
import Html.Attributes as HA
import Html.Events
import Pine
import Result.Extra
import Set
import Time
import Url


type alias State =
    { time : Time.Posix
    , expression : String
    , evaluationContext : Result String Pine.EvalEnvironment
    , lastUserInputExpressionTime : Time.Posix
    , lastEvaluatedExpression : Maybe ( String, Result String ElmInteractive.SubmissionResponse )
    , lastAddElmModuleAttempt : Maybe (Result String { compiledModuleSize : Int })
    , addElmModuleControl : Maybe AddModuleControlState
    , compilationExplorer : Maybe CompilationExplorerState
    }


type alias CompilationExplorerState =
    { compiledContext : Pine.EvalEnvironment
    , rootNode : CompilationExplorerNode
    , expandedNodes : Set.Set (List String)
    }


type alias CompilationExplorerViewState =
    { expandedNodes : Set.Set (List String)
    }


type CompilationExplorerEvent
    = ExpandNode (List String)
    | CollapseNode (List String)


type alias CompilationExplorerNode =
    { value : ExplorerValueCache
    , category : CompilationNodeCategory
    , parsed : Maybe ExplorerNodeParseState
    }


type alias ExplorerValueCache =
    { value : Pine.Value
    , valueSize : Int
    }


type CompilationNodeCategory
    = RootNode
    | ElmModuleNode
      -- In the Elm Interactive, declarations can appear in the global scope, not just in modules.
    | ElmFunctionDeclarationNode
    | FunctionExpressionNode


type ExplorerNodeParseState
    = Parsed (Result String ExplorerNodeParseSuccess)


type alias ExplorerNodeParseSuccess =
    { otherProperties : List ( String, String )
    , children : Dict.Dict String CompilationExplorerNode
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
    | UserInputOpenCompilationExplorerEvent
    | UserInputCompilationExplorerEvent CompilationExplorerEvent


type AddModuleControlState
    = EditingControlState String
    | SubmittedControlState (Result String { compiledModuleSize : Int })


evalDelayFromUserInputMilliseconds : Int
evalDelayFromUserInputMilliseconds =
    500


explorerValueCache : Pine.Value -> ExplorerValueCache
explorerValueCache value =
    { value = value
    , valueSize = FirCompiler.estimatePineValueSize value
    }


init : Time.Posix -> ( State, Cmd Event )
init time =
    ( { time = time
      , expression = ""
      , evaluationContext = ElmInteractiveParser.compileEvalContextForElmInteractive ElmInteractive.DefaultContext
      , lastUserInputExpressionTime = Time.millisToPosix 0
      , lastEvaluatedExpression = Nothing
      , lastAddElmModuleAttempt = Nothing
      , addElmModuleControl = Nothing
      , compilationExplorer = Nothing
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

        UserInputOpenCompilationExplorerEvent ->
            case stateBefore.evaluationContext of
                Err _ ->
                    ( stateBefore
                    , Cmd.none
                    )

                Ok evaluationContext ->
                    ( { stateBefore
                        | compilationExplorer = Just (initCompilationExplorer evaluationContext)
                      }
                    , Cmd.none
                    )

        UserInputCompilationExplorerEvent compilationExplorerEvent ->
            case stateBefore.compilationExplorer of
                Nothing ->
                    ( stateBefore
                    , Cmd.none
                    )

                Just explorerStateBefore ->
                    let
                        compilationExplorerState =
                            updateCompilationExplorer compilationExplorerEvent explorerStateBefore
                                |> parseExpandedExplorerNodes
                    in
                    ( { stateBefore
                        | compilationExplorer = Just compilationExplorerState
                      }
                    , Cmd.none
                    )


initCompilationExplorer : Pine.EvalEnvironment -> CompilationExplorerState
initCompilationExplorer (Pine.EvalEnvironment environmentValue) =
    { compiledContext = Pine.EvalEnvironment environmentValue
    , rootNode =
        { value = explorerValueCache environmentValue
        , category = RootNode
        , parsed = Just (Parsed (parseElmExplorerNodeValue RootNode environmentValue))
        }
    , expandedNodes = Set.singleton []
    }


updateCompilationExplorer : CompilationExplorerEvent -> CompilationExplorerState -> CompilationExplorerState
updateCompilationExplorer event stateBefore =
    case event of
        ExpandNode node ->
            { stateBefore
                | expandedNodes = Set.insert node stateBefore.expandedNodes
            }

        CollapseNode node ->
            { stateBefore
                | expandedNodes = Set.remove node stateBefore.expandedNodes
            }


parseExpandedExplorerNodes : CompilationExplorerState -> CompilationExplorerState
parseExpandedExplorerNodes stateBefore =
    { stateBefore
        | rootNode =
            parseExpandedExplorerNodesWithPath []
                { expandedNodes = stateBefore.expandedNodes }
                stateBefore.rootNode
    }


parseExpandedExplorerNodesWithPath :
    List String
    -> CompilationExplorerViewState
    -> CompilationExplorerNode
    -> CompilationExplorerNode
parseExpandedExplorerNodesWithPath path viewState node =
    case node.parsed of
        Nothing ->
            if Set.member path viewState.expandedNodes then
                { node
                    | parsed = Just (Parsed (parseElmExplorerNodeValue node.category node.value.value))
                }

            else
                node

        Just (Parsed parseResult) ->
            case parseResult of
                Err _ ->
                    node

                Ok parseSuccess ->
                    let
                        childrenParsed =
                            Dict.map
                                (\childName childNode ->
                                    parseExpandedExplorerNodesWithPath (path ++ [ childName ]) viewState childNode
                                )
                                parseSuccess.children
                    in
                    { node
                        | parsed = Just (Parsed (Ok { parseSuccess | children = childrenParsed }))
                    }


parseElmExplorerNodeValue :
    CompilationNodeCategory
    -> Pine.Value
    -> Result String ExplorerNodeParseSuccess
parseElmExplorerNodeValue nodeCategory nodeValue =
    case nodeCategory of
        RootNode ->
            case ElmCompiler.getDeclarationsFromEnvironment nodeValue of
                Err error ->
                    Err ("Failed to get declarations from environment: " ++ error)

                Ok ( _, environmentBeforeDeclarations ) ->
                    case ElmCompiler.separateEnvironmentDeclarations environmentBeforeDeclarations of
                        Err err ->
                            Err ("Failed to separate declarations from environment: " ++ err)

                        Ok separateEnvironmentDeclarations ->
                            let
                                parsedChildrenDict : Dict.Dict String CompilationExplorerNode
                                parsedChildrenDict =
                                    Dict.foldl
                                        (\moduleName ( moduleValue, _ ) ->
                                            Dict.insert
                                                (String.join "." moduleName)
                                                { value = explorerValueCache moduleValue
                                                , category = ElmModuleNode
                                                , parsed = Nothing
                                                }
                                        )
                                        Dict.empty
                                        separateEnvironmentDeclarations.modules
                            in
                            Ok
                                { children = parsedChildrenDict
                                , otherProperties = []
                                }

        ElmModuleNode ->
            nodeValue
                |> ElmCompiler.getDeclarationsFromEnvironment
                |> Result.andThen (Tuple.second >> ElmCompiler.parseModuleValue)
                |> Result.map
                    (\parsedModule ->
                        { children =
                            parsedModule.functionDeclarations
                                |> List.foldl
                                    (\( declName, declValue ) ->
                                        Dict.insert
                                            declName
                                            { value = explorerValueCache declValue
                                            , category = ElmFunctionDeclarationNode
                                            , parsed = Nothing
                                            }
                                    )
                                    Dict.empty
                        , otherProperties = []
                        }
                    )

        ElmFunctionDeclarationNode ->
            parseAsFunctionRecord nodeValue

        FunctionExpressionNode ->
            case Pine.parseExpressionFromValue nodeValue of
                Err err ->
                    Err ("Failed to parse expression: " ++ err)

                Ok expression ->
                    let
                        childrenFunctionRecord : Dict.Dict String CompilationExplorerNode
                        childrenFunctionRecord =
                            case expression of
                                Pine.LiteralExpression literalValue ->
                                    case parseAsFunctionRecord literalValue of
                                        Err _ ->
                                            Dict.empty

                                        Ok functionRecord ->
                                            Dict.singleton "function-record"
                                                { value = explorerValueCache literalValue
                                                , category = FunctionExpressionNode
                                                , parsed = Just (Parsed (Ok functionRecord))
                                                }

                                _ ->
                                    Dict.empty

                        expressionChildrenDict : Dict.Dict String CompilationExplorerNode
                        expressionChildrenDict =
                            Dict.singleton "expression"
                                { value = explorerValueCache nodeValue
                                , category = FunctionExpressionNode
                                , parsed =
                                    Just
                                        (Parsed
                                            (Ok
                                                { children = Dict.empty
                                                , otherProperties =
                                                    [ ( "expression"
                                                      , ElmCompilerConstruction.buildPineExpressionSyntax
                                                            { attemptEncodeExpression = False }
                                                            expression
                                                            |> String.join "\n"
                                                      )
                                                    ]
                                                }
                                            )
                                        )
                                }
                    in
                    Ok
                        { children =
                            Dict.union
                                expressionChildrenDict
                                childrenFunctionRecord
                        , otherProperties = []
                        }


parseAsFunctionRecord : Pine.Value -> Result String ExplorerNodeParseSuccess
parseAsFunctionRecord nodeValue =
    case FirCompiler.parseFunctionRecordFromValueTagged nodeValue of
        Err err ->
            case ElmInteractive.pineValueAsElmValue nodeValue of
                Err _ ->
                    Err ("Failed to parse function: " ++ err)

                Ok elmValue ->
                    case ElmInteractive.elmValueAsExpression elmValue of
                        ( expressionText, _ ) ->
                            Ok
                                { children = Dict.empty
                                , otherProperties =
                                    [ ( "literal value"
                                      , expressionText
                                      )
                                    ]
                                }

        Ok (FirCompiler.ParsedFunctionValue innerFunctionValue _ parameterCount (FirCompiler.ParsedFunctionEnvFunctions envFunctions) argumentsAlreadyCollected) ->
            let
                envFunctionsChildren : Dict.Dict String CompilationExplorerNode
                envFunctionsChildren =
                    envFunctions
                        |> List.indexedMap
                            (\envFunctionIndex envFunctionValue ->
                                ( "env-decl-" ++ String.fromInt envFunctionIndex
                                , { value = explorerValueCache envFunctionValue
                                  , category = FunctionExpressionNode
                                  , parsed = Nothing
                                  }
                                )
                            )
                        |> Dict.fromList
            in
            Ok
                { children =
                    envFunctionsChildren
                        |> Dict.insert
                            "inner-expression"
                            { value = explorerValueCache innerFunctionValue
                            , category = FunctionExpressionNode
                            , parsed = Nothing
                            }
                , otherProperties =
                    [ ( "parameter count"
                      , String.fromInt parameterCount
                      )
                    , ( "env functions count"
                      , String.fromInt (List.length envFunctions)
                      )
                    , ( "arguments already collected"
                      , String.fromInt (List.length argumentsAlreadyCollected)
                      )
                    ]
                }


addElmModule : String -> State -> ( Result String { environment : Pine.Value, compiledModuleSize : Int }, ( State, Cmd Event ) )
addElmModule elmModuleText stateBefore =
    let
        addModuleResult : Result String { environment : Pine.Value, compiledModuleSize : Int }
        addModuleResult =
            case stateBefore.evaluationContext of
                Err error ->
                    Err ("Failed to initialize the evaluation context: " ++ error)

                Ok (Pine.EvalEnvironment environmentValue) ->
                    parseElmModule elmModuleText
                        |> Result.andThen
                            (\( parsedModule, addModule ) ->
                                addModule environmentValue
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
                    |> Result.map (\{ environment } -> Pine.EvalEnvironment environment)
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

        openCompilationExplorerButton =
            buttonElement
                { labelText = "Open compilation explorer"
                , clickEvent = UserInputOpenCompilationExplorerEvent
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

        addElmModuleOverlayElement : Element.Element Event
        addElmModuleOverlayElement =
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

        compilationExplorerOverlayElement : Element.Element Event
        compilationExplorerOverlayElement =
            case state.compilationExplorer of
                Nothing ->
                    Element.none

                Just compilationExplorerState ->
                    [ Element.text "Compilation explorer"
                        |> Element.el [ Element.Region.heading 2 ]
                    , viewCompilationExplorer compilationExplorerState
                        |> Element.map UserInputCompilationExplorerEvent
                    ]
                        |> Element.column
                            [ Element.padding 10
                            , Element.width Element.fill
                            , Element.height Element.fill
                            , Element.spacing 20
                            ]
                        |> Element.el
                            [ Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
                            , Element.width Element.fill
                            , Element.height Element.fill
                            , Element.htmlAttribute (HA.style "backdrop-filter" "blur(3px)")
                            ]
    in
    { body =
        [ [ interactiveElement
          , addElmModuleButton
          , openCompilationExplorerButton
          ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.padding 10
                , Element.width Element.fill
                , Element.height Element.fill
                ]
            |> Element.el
                [ Element.inFront addElmModuleOverlayElement
                , Element.inFront compilationExplorerOverlayElement
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


viewCompilationExplorer : CompilationExplorerState -> Element.Element CompilationExplorerEvent
viewCompilationExplorer state =
    state.rootNode
        |> viewCompilationExplorerNode
            []
            { expandedNodes = state.expandedNodes }
            (Element.text "Elm Interactive Context")
        |> Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]


viewCompilationExplorerNode :
    List String
    -> CompilationExplorerViewState
    -> Element.Element CompilationExplorerEvent
    -> CompilationExplorerNode
    -> Element.Element CompilationExplorerEvent
viewCompilationExplorerNode path viewConfig labelElement node =
    let
        isExpanded =
            Set.member path viewConfig.expandedNodes

        ( toggleIcon, toggleEvent ) =
            if isExpanded then
                ( FontAwesome.Solid.chevronDown, CollapseNode path )

            else
                ( FontAwesome.Solid.chevronRight, ExpandNode path )

        toggleButtonIconSize =
            20

        toggleButton =
            toggleIcon
                |> FontAwesome.view
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px toggleButtonIconSize)
                    , Element.height (Element.px toggleButtonIconSize)
                    ]
                |> Element.el
                    [ Element.pointer
                    , Element.Events.onClick toggleEvent
                    , Element.centerY
                    , Element.alpha 0.7
                    , Element.mouseOver
                        [ Element.alpha 1 ]
                    , Element.padding 4
                    ]

        expansionElement =
            if not isExpanded then
                Element.none

            else
                case node.parsed of
                    Nothing ->
                        Element.none

                    Just (Parsed parseResult) ->
                        case parseResult of
                            Err err ->
                                [ Element.text ("Failed to parse children: " ++ err) ]
                                    |> Element.paragraph
                                        [ Element.Font.color (Element.rgb 0.9 0.5 0.1)
                                        , Element.padding 10
                                        , Element.width Element.fill
                                        ]

                            Ok parsed ->
                                viewCompilationExplorerNodeParsed path viewConfig parsed
                                    |> Element.el
                                        [ Element.padding 10
                                        , Element.width Element.fill
                                        ]

        sizeElement =
            Element.text (stringFromIntWithThousandSeparator "_" node.value.valueSize)
                |> Element.el
                    [ Element.alpha 0.6
                    , Element.Font.family [ Element.Font.monospace ]
                    , Element.centerY
                    ]
    in
    [ [ toggleButton
      , labelElement
      , sizeElement
      ]
        |> Element.row
            [ Element.spacing defaultFontSize
            , Element.width Element.fill
            ]
    , expansionElement
    ]
        |> Element.column
            [ Element.spacing (defaultFontSize // 2)
            , Element.padding 10
            , Element.width Element.fill
            ]


viewCompilationExplorerNodeParsed :
    List String
    -> CompilationExplorerViewState
    -> ExplorerNodeParseSuccess
    -> Element.Element CompilationExplorerEvent
viewCompilationExplorerNodeParsed path viewConfig parsed =
    [ if parsed.otherProperties == [] then
        Element.none

      else
        Element.table
            [ Element.width Element.fill
            , Element.Font.family [ Element.Font.monospace ]
            ]
            { data = parsed.otherProperties
            , columns =
                [ { header = Element.none
                  , width = Element.shrink
                  , view =
                        Tuple.first
                            >> Element.text
                            >> Element.el [ Element.paddingXY 10 4 ]
                  }
                , { header = Element.none
                  , width = Element.fill
                  , view =
                        Tuple.second
                            >> Html.text
                            >> List.singleton
                            >> Html.div
                                [ HA.style "word-break" "break-all"
                                , HA.style "white-space" "pre-wrap"
                                , HA.style "line-height" "normal"
                                ]
                            >> Element.html
                            >> Element.el
                                [ Element.paddingXY 10 4
                                , Element.width Element.fill
                                ]
                  }
                ]
            }
    , Dict.toList
        parsed.children
        |> List.map
            (\( name, childNode ) ->
                viewCompilationExplorerNode
                    (path ++ [ name ])
                    viewConfig
                    (Element.text name
                        |> Element.el
                            [ Element.Font.family [ Element.Font.monospace ]
                            , Element.centerY
                            ]
                    )
                    childNode
            )
        |> Element.column
            [ Element.width Element.fill
            ]
    ]
        |> Element.column
            [ Element.spacing (defaultFontSize // 2)
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


stringFromIntWithThousandSeparator : String -> Int -> String
stringFromIntWithThousandSeparator separator int =
    if int < 0 then
        "-" ++ stringFromIntWithThousandSeparator separator (abs int)

    else if int < 1000 then
        String.fromInt int

    else
        let
            upperDigits =
                int // 1000

            remainder =
                int - upperDigits * 1000
        in
        stringFromIntWithThousandSeparator separator upperDigits
            ++ separator
            ++ String.padLeft 3 '0' (String.fromInt remainder)
