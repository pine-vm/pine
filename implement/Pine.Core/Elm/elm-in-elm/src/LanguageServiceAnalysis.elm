module LanguageServiceAnalysis exposing (..)

{-| Range-free semantic analysis of Elm modules for the language service.

This module only imports the abstract syntax model and the structural path
model. It therefore cannot observe source ranges, source text, comments or
literal spellings, which is the structural guarantee that the analysis cached
per module stays free of presentation data.

Every occurrence found here is addressed by a `ElmSyntax.Path.Path`. The
presentation layer in `LanguageService` resolves such a path against the
retained concrete syntax of the module it belongs to, and only for the
occurrences it actually returns to the client.

-}

import ElmSyntax.Abstract.Declaration
import ElmSyntax.Abstract.Exposing
import ElmSyntax.Abstract.Expression
import ElmSyntax.Abstract.File
import ElmSyntax.Abstract.Import
import ElmSyntax.Abstract.Module
import ElmSyntax.Abstract.Pattern
import ElmSyntax.Abstract.TypeAnnotation
import ElmSyntax.Path exposing (Path, Selection(..), Step(..))


{-| Visibility of a declared name.

`LocalScope` carries the path of the syntax node that limits visibility, so
containment tests are structural (`ElmSyntax.Path.isPrefixOf`) instead of
range based.

-}
type DeclarationScope
    = TopLevelScope
    | LocalScope Path


type DeclarationKind
    = FunctionOrValueDeclarationKind
    | TypeAliasDeclarationKind
    | ChoiceTypeDeclarationKind
    | ChoiceTypeTagDeclarationKind


{-| Describes how the presentation layer renders the documentation of a
declaration. The variants only name structural locations, never text.
-}
type DocumentationSource
    = -- Path of the type annotation to render after the name, if any
      ValueDocumentation (Maybe Path)
    | DeclarationCodeDocumentation
    | ChoiceTypeTagDocumentation String Path


type alias DeclarationOccurrence =
    { name : String
    , kind : DeclarationKind
    , scope : DeclarationScope
    , isExposed : Bool

    -- Path and selection covering the complete declaration
    , declarationPath : Path
    , declarationSelection : Selection

    -- Paths whose name selection is an instance of the declared name
    , namePaths : List Path
    , documentation : DocumentationSource
    }


{-| A name used (not declared) somewhere in a module.
-}
type alias ReferenceOccurrence =
    { moduleName : List String
    , name : String
    , path : Path
    }


type alias ImportOccurrence =
    { canonicalName : List String
    , importedName : List String
    , exposingList : Maybe ElmSyntax.Abstract.Exposing.Exposing
    , moduleNamePath : Path
    }


type alias ModuleAnalysis =
    { moduleName : List String
    , imports : List ImportOccurrence
    , declarations : List DeclarationOccurrence
    }


analyzeFile : ElmSyntax.Abstract.File.File -> ModuleAnalysis
analyzeFile file =
    let
        exposingList : ElmSyntax.Abstract.Exposing.Exposing
        exposingList =
            exposingListOfModule file.moduleDefinition
    in
    { moduleName = ElmSyntax.Abstract.Module.moduleName file.moduleDefinition
    , imports = List.indexedMap importOccurrence file.imports
    , declarations =
        List.map
            (\occurrence ->
                {- Names bound by patterns are not visible in the module
                   exposing list at the point they are collected, so exposure of
                   top level value declarations is decided here.
                -}
                case occurrence.scope of
                    LocalScope _ ->
                        occurrence

                    TopLevelScope ->
                        case occurrence.kind of
                            FunctionOrValueDeclarationKind ->
                                { occurrence
                                    | isExposed = exposesFunction occurrence.name exposingList
                                }

                            _ ->
                                occurrence
            )
            (List.concat
                (List.indexedMap
                    (\index declaration ->
                        declarationOccurrencesInDeclaration exposingList index declaration
                    )
                    file.declarations
                )
            )
    }


exposingListOfModule : ElmSyntax.Abstract.Module.Module -> ElmSyntax.Abstract.Exposing.Exposing
exposingListOfModule moduleValue =
    case moduleValue of
        ElmSyntax.Abstract.Module.NormalModule normalModule ->
            normalModule.exposingList

        ElmSyntax.Abstract.Module.PortModule portModule ->
            portModule.exposingList

        ElmSyntax.Abstract.Module.EffectModule effectModule ->
            effectModule.exposingList


importOccurrence : Int -> ElmSyntax.Abstract.Import.Import -> ImportOccurrence
importOccurrence index importValue =
    { canonicalName = importValue.moduleName
    , importedName =
        case importValue.moduleAlias of
            Nothing ->
                importValue.moduleName

            Just moduleAlias ->
                moduleAlias
    , exposingList = importValue.exposingList
    , moduleNamePath = [ StepImport index, StepModuleName ]
    }



-- Declarations


declarationOccurrencesInDeclaration :
    ElmSyntax.Abstract.Exposing.Exposing
    -> Int
    -> ElmSyntax.Abstract.Declaration.Declaration
    -> List DeclarationOccurrence
declarationOccurrencesInDeclaration exposingList declarationIndex declaration =
    let
        declarationPath : Path
        declarationPath =
            [ StepDeclaration declarationIndex ]
    in
    case declaration of
        ElmSyntax.Abstract.Declaration.FunctionDeclaration functionStruct ->
            declarationOccurrencesForFunction
                exposingList
                SelectDeclarationWithoutDocumentation
                declarationPath
                functionStruct

        ElmSyntax.Abstract.Declaration.AliasDeclaration typeAlias ->
            [ { name = typeAlias.name
              , kind = TypeAliasDeclarationKind
              , scope = TopLevelScope
              , isExposed = exposesTypeOrAlias typeAlias.name exposingList
              , declarationPath = declarationPath
              , declarationSelection = SelectDeclarationWithoutDocumentation
              , namePaths = [ declarationPath ]
              , documentation = DeclarationCodeDocumentation
              }
            ]

        ElmSyntax.Abstract.Declaration.ChoiceTypeDeclaration choiceType ->
            let
                isExposed : Bool
                isExposed =
                    exposesTypeOrAlias choiceType.name exposingList

                tags : List DeclarationOccurrence
                tags =
                    List.indexedMap
                        (\constructorIndex constructor ->
                            { name = constructor.name
                            , kind = ChoiceTypeTagDeclarationKind
                            , scope = TopLevelScope
                            , isExposed = isExposed
                            , declarationPath =
                                ElmSyntax.Path.appendStep
                                    declarationPath
                                    (StepConstructor constructorIndex)
                            , declarationSelection = SelectWhole
                            , namePaths =
                                [ ElmSyntax.Path.appendStep
                                    declarationPath
                                    (StepConstructor constructorIndex)
                                ]
                            , documentation =
                                ChoiceTypeTagDocumentation choiceType.name declarationPath
                            }
                        )
                        choiceType.constructors
            in
            { name = choiceType.name
            , kind = ChoiceTypeDeclarationKind
            , scope = TopLevelScope
            , isExposed = isExposed
            , declarationPath = declarationPath
            , declarationSelection = SelectDeclarationWithoutDocumentation
            , namePaths = [ declarationPath ]
            , documentation = DeclarationCodeDocumentation
            }
                :: tags

        ElmSyntax.Abstract.Declaration.PortDeclaration _ ->
            []

        ElmSyntax.Abstract.Declaration.InfixDeclaration _ ->
            []


declarationOccurrencesForFunction :
    ElmSyntax.Abstract.Exposing.Exposing
    -> Selection
    -> Path
    -> ElmSyntax.Abstract.Expression.FunctionStruct
    -> List DeclarationOccurrence
declarationOccurrencesForFunction exposingList declarationSelection declarationPath functionStruct =
    let
        implementation : ElmSyntax.Abstract.Expression.FunctionImplementation
        implementation =
            functionStruct.declaration

        implementationPath : Path
        implementationPath =
            ElmSyntax.Path.appendStep declarationPath StepImplementation

        annotationPath : Maybe Path
        annotationPath =
            case functionStruct.signature of
                Nothing ->
                    Nothing

                Just _ ->
                    Just
                        (declarationPath ++ [ StepSignature, StepTypeAnnotation ])

        namePaths : List Path
        namePaths =
            case functionStruct.signature of
                Nothing ->
                    [ implementationPath ]

                Just _ ->
                    [ ElmSyntax.Path.appendStep declarationPath StepSignature
                    , implementationPath
                    ]

        arguments : List DeclarationOccurrence
        arguments =
            List.concat
                (List.indexedMap
                    (\argumentIndex argument ->
                        declarationOccurrencesInPattern
                            (annotationPathForArgument argumentIndex
                                annotationPath
                                (case functionStruct.signature of
                                    Nothing ->
                                        Nothing

                                    Just signature ->
                                        Just signature.typeAnnotation
                                )
                            )
                            (ElmSyntax.Path.appendStep
                                implementationPath
                                (StepArgument argumentIndex)
                            )
                            argument
                    )
                    implementation.arguments
                )
    in
    List.concat
        [ [ { name = implementation.name
            , kind = FunctionOrValueDeclarationKind
            , scope = TopLevelScope
            , isExposed = exposesFunction implementation.name exposingList
            , declarationPath = declarationPath
            , declarationSelection = declarationSelection
            , namePaths = namePaths
            , documentation = ValueDocumentation annotationPath
            }
          ]
        , arguments
        , declarationOccurrencesInExpression
            (ElmSyntax.Path.appendStep implementationPath StepBody)
            implementation.expression
        ]


{-| Path of the type annotation describing the argument at the given index,
mirroring how a function type annotation is consumed from left to right.
-}
annotationPathForArgument :
    Int
    -> Maybe Path
    -> Maybe ElmSyntax.Abstract.TypeAnnotation.TypeAnnotation
    -> Maybe Path
annotationPathForArgument argumentIndex maybeAnnotationPath maybeAnnotation =
    case ( maybeAnnotationPath, maybeAnnotation ) of
        ( Just annotationPath, Just annotation ) ->
            case annotation of
                ElmSyntax.Abstract.TypeAnnotation.FunctionTypeAnnotation argumentType returnType ->
                    if argumentIndex < 1 then
                        Just (ElmSyntax.Path.appendStep annotationPath (StepChild 0))

                    else
                        annotationPathForArgument
                            (argumentIndex - 1)
                            (Just (ElmSyntax.Path.appendStep annotationPath (StepChild 1)))
                            (Just returnType)

                _ ->
                    if argumentIndex < 1 then
                        Just annotationPath

                    else
                        Nothing

        _ ->
            Nothing


declarationOccurrencesInPattern :
    Maybe Path
    -> Path
    -> ElmSyntax.Abstract.Pattern.Pattern
    -> List DeclarationOccurrence
declarationOccurrencesInPattern annotationPath path pattern =
    case pattern of
        ElmSyntax.Abstract.Pattern.TuplePattern items ->
            declarationOccurrencesInPatternList path items

        ElmSyntax.Abstract.Pattern.UnConsPattern head tail ->
            declarationOccurrencesInPatternList path [ head, tail ]

        ElmSyntax.Abstract.Pattern.ListPattern items ->
            declarationOccurrencesInPatternList path items

        ElmSyntax.Abstract.Pattern.VarPattern name ->
            [ { name = name
              , kind = FunctionOrValueDeclarationKind
              , scope = TopLevelScope
              , isExposed = False
              , declarationPath = path
              , declarationSelection = SelectWhole
              , namePaths = [ path ]
              , documentation = ValueDocumentation annotationPath
              }
            ]

        ElmSyntax.Abstract.Pattern.NamedPattern _ arguments ->
            declarationOccurrencesInPatternList path arguments

        _ ->
            []


declarationOccurrencesInPatternList :
    Path
    -> List ElmSyntax.Abstract.Pattern.Pattern
    -> List DeclarationOccurrence
declarationOccurrencesInPatternList path items =
    List.concat
        (List.indexedMap
            (\index item ->
                declarationOccurrencesInPattern
                    Nothing
                    (ElmSyntax.Path.appendStep path (StepChild index))
                    item
            )
            items
        )


declarationOccurrencesInExpression :
    Path
    -> ElmSyntax.Abstract.Expression.Expression
    -> List DeclarationOccurrence
declarationOccurrencesInExpression path expression =
    case expression of
        ElmSyntax.Abstract.Expression.Application function arguments ->
            declarationOccurrencesInExpressionList path (function :: arguments)

        ElmSyntax.Abstract.Expression.OperatorApplication _ _ left right ->
            declarationOccurrencesInExpressionList path [ left, right ]

        ElmSyntax.Abstract.Expression.IfBlock condition thenBranch elseBranch ->
            declarationOccurrencesInExpressionList path [ condition, thenBranch, elseBranch ]

        ElmSyntax.Abstract.Expression.Negation inner ->
            declarationOccurrencesInExpressionList path [ inner ]

        ElmSyntax.Abstract.Expression.TupledExpression items ->
            declarationOccurrencesInExpressionList path items

        ElmSyntax.Abstract.Expression.ListExpr items ->
            declarationOccurrencesInExpressionList path items

        ElmSyntax.Abstract.Expression.LetExpression declarations letBody ->
            {- All declarations found below a let block are visible in that
               block. Nested let blocks widen visibility to the outermost
               enclosing block, matching the behavior before the migration to
               structural scopes.
            -}
            List.map
                (\occurrence ->
                    { occurrence | scope = LocalScope path }
                )
                (List.concat
                    [ List.concat
                        (List.indexedMap
                            (\index letDeclaration ->
                                declarationOccurrencesInLetDeclaration
                                    (ElmSyntax.Path.appendStep path (StepLetDeclaration index))
                                    letDeclaration
                            )
                            declarations
                        )
                    , declarationOccurrencesInExpression
                        (ElmSyntax.Path.appendStep path StepBody)
                        letBody
                    ]
                )

        ElmSyntax.Abstract.Expression.CaseExpression subject cases ->
            List.concat
                [ declarationOccurrencesInExpression
                    (ElmSyntax.Path.appendStep path (StepChild 0))
                    subject
                , List.concat
                    (List.indexedMap
                        (\index caseBranch ->
                            declarationOccurrencesInExpression
                                (path ++ [ StepCaseBranch index, StepBody ])
                                caseBranch.expression
                        )
                        cases
                    )
                ]

        ElmSyntax.Abstract.Expression.LambdaExpression _ lambdaBody ->
            declarationOccurrencesInExpression
                (ElmSyntax.Path.appendStep path StepBody)
                lambdaBody

        ElmSyntax.Abstract.Expression.RecordExpr setters ->
            declarationOccurrencesInRecordSetters path setters

        ElmSyntax.Abstract.Expression.RecordUpdateExpression _ setters ->
            declarationOccurrencesInRecordSetters path setters

        ElmSyntax.Abstract.Expression.RecordAccess record _ ->
            declarationOccurrencesInExpressionList path [ record ]

        _ ->
            []


declarationOccurrencesInExpressionList :
    Path
    -> List ElmSyntax.Abstract.Expression.Expression
    -> List DeclarationOccurrence
declarationOccurrencesInExpressionList path items =
    List.concat
        (List.indexedMap
            (\index item ->
                declarationOccurrencesInExpression
                    (ElmSyntax.Path.appendStep path (StepChild index))
                    item
            )
            items
        )


declarationOccurrencesInRecordSetters :
    Path
    -> List ElmSyntax.Abstract.Expression.RecordSetter
    -> List DeclarationOccurrence
declarationOccurrencesInRecordSetters path setters =
    List.concat
        (List.map
            (\( setter, setterPath ) ->
                declarationOccurrencesInExpression
                    (ElmSyntax.Path.appendStep setterPath (StepChild 0))
                    setter.value
            )
            (recordSettersWithPaths path setters)
        )


declarationOccurrencesInLetDeclaration :
    Path
    -> ElmSyntax.Abstract.Expression.LetDeclaration
    -> List DeclarationOccurrence
declarationOccurrencesInLetDeclaration path letDeclaration =
    case letDeclaration of
        ElmSyntax.Abstract.Expression.LetFunction functionStruct ->
            declarationOccurrencesForFunction
                (ElmSyntax.Abstract.Exposing.Explicit [])
                SelectWhole
                path
                functionStruct

        ElmSyntax.Abstract.Expression.LetDestructuring _ destructured ->
            declarationOccurrencesInExpression
                (ElmSyntax.Path.appendStep path StepBody)
                destructured



-- References


listReferencesInFile : ElmSyntax.Abstract.File.File -> List ReferenceOccurrence
listReferencesInFile file =
    List.concat
        [ referencesInModuleExposing (exposingListOfModule file.moduleDefinition)
        , List.concat
            (List.indexedMap
                (\index declaration ->
                    referencesInDeclaration [ StepDeclaration index ] declaration
                )
                file.declarations
            )
        ]


referencesInModuleExposing : ElmSyntax.Abstract.Exposing.Exposing -> List ReferenceOccurrence
referencesInModuleExposing exposingList =
    case exposingList of
        ElmSyntax.Abstract.Exposing.All ->
            []

        ElmSyntax.Abstract.Exposing.Explicit entries ->
            List.concat
                (List.indexedMap
                    (\index entry ->
                        let
                            entryPath : Path
                            entryPath =
                                [ StepModuleDefinition, StepExposingEntry index ]
                        in
                        case entry of
                            ElmSyntax.Abstract.Exposing.InfixExpose _ ->
                                []

                            ElmSyntax.Abstract.Exposing.FunctionExpose name ->
                                [ { moduleName = [], name = name, path = entryPath } ]

                            ElmSyntax.Abstract.Exposing.TypeOrAliasExpose name ->
                                [ { moduleName = [], name = name, path = entryPath } ]

                            ElmSyntax.Abstract.Exposing.TypeExpose exposedType ->
                                [ { moduleName = []
                                  , name = exposedType.name
                                  , path = entryPath
                                  }
                                ]
                    )
                    entries
                )


referencesInDeclaration :
    Path
    -> ElmSyntax.Abstract.Declaration.Declaration
    -> List ReferenceOccurrence
referencesInDeclaration declarationPath declaration =
    case declaration of
        ElmSyntax.Abstract.Declaration.FunctionDeclaration functionStruct ->
            referencesInFunction declarationPath functionStruct

        ElmSyntax.Abstract.Declaration.AliasDeclaration typeAlias ->
            referencesInTypeAnnotation
                (ElmSyntax.Path.appendStep declarationPath StepTypeAnnotation)
                typeAlias.typeAnnotation

        ElmSyntax.Abstract.Declaration.ChoiceTypeDeclaration choiceType ->
            List.concat
                (List.indexedMap
                    (\constructorIndex constructor ->
                        List.concat
                            (List.indexedMap
                                (\argumentIndex argument ->
                                    referencesInTypeAnnotation
                                        (declarationPath
                                            ++ [ StepConstructor constructorIndex
                                               , StepArgument argumentIndex
                                               ]
                                        )
                                        argument
                                )
                                constructor.arguments
                            )
                    )
                    choiceType.constructors
                )

        ElmSyntax.Abstract.Declaration.PortDeclaration _ ->
            []

        ElmSyntax.Abstract.Declaration.InfixDeclaration _ ->
            []


referencesInFunction :
    Path
    -> ElmSyntax.Abstract.Expression.FunctionStruct
    -> List ReferenceOccurrence
referencesInFunction declarationPath functionStruct =
    let
        implementationPath : Path
        implementationPath =
            ElmSyntax.Path.appendStep declarationPath StepImplementation

        signatureReferences : List ReferenceOccurrence
        signatureReferences =
            case functionStruct.signature of
                Nothing ->
                    []

                Just signature ->
                    referencesInTypeAnnotation
                        (declarationPath ++ [ StepSignature, StepTypeAnnotation ])
                        signature.typeAnnotation

        argumentReferences : List ReferenceOccurrence
        argumentReferences =
            List.concat
                (List.indexedMap
                    (\argumentIndex argument ->
                        referencesInPattern
                            (ElmSyntax.Path.appendStep
                                implementationPath
                                (StepArgument argumentIndex)
                            )
                            argument
                    )
                    functionStruct.declaration.arguments
                )
    in
    List.concat
        [ signatureReferences
        , argumentReferences
        , referencesInExpression
            (ElmSyntax.Path.appendStep implementationPath StepBody)
            functionStruct.declaration.expression
        ]


referencesInTypeAnnotation :
    Path
    -> ElmSyntax.Abstract.TypeAnnotation.TypeAnnotation
    -> List ReferenceOccurrence
referencesInTypeAnnotation path typeAnnotation =
    case typeAnnotation of
        ElmSyntax.Abstract.TypeAnnotation.GenericType _ ->
            []

        ElmSyntax.Abstract.TypeAnnotation.Typed moduleName name arguments ->
            { moduleName = moduleName, name = name, path = path }
                :: referencesInTypeAnnotationList path arguments

        ElmSyntax.Abstract.TypeAnnotation.Unit ->
            []

        ElmSyntax.Abstract.TypeAnnotation.Tupled items ->
            referencesInTypeAnnotationList path items

        ElmSyntax.Abstract.TypeAnnotation.Record fields ->
            referencesInRecordFields path fields

        ElmSyntax.Abstract.TypeAnnotation.GenericRecord _ fields ->
            referencesInRecordFields path fields

        ElmSyntax.Abstract.TypeAnnotation.FunctionTypeAnnotation input return ->
            referencesInTypeAnnotationList path [ input, return ]


referencesInTypeAnnotationList :
    Path
    -> List ElmSyntax.Abstract.TypeAnnotation.TypeAnnotation
    -> List ReferenceOccurrence
referencesInTypeAnnotationList path items =
    List.concat
        (List.indexedMap
            (\index item ->
                referencesInTypeAnnotation
                    (ElmSyntax.Path.appendStep path (StepChild index))
                    item
            )
            items
        )


referencesInRecordFields :
    Path
    -> List ElmSyntax.Abstract.TypeAnnotation.RecordField
    -> List ReferenceOccurrence
referencesInRecordFields path fields =
    List.concat
        (List.map
            (\( field, fieldPath ) ->
                referencesInTypeAnnotation
                    (ElmSyntax.Path.appendStep fieldPath (StepChild 0))
                    field.fieldType
            )
            (recordFieldsWithPaths path fields)
        )


referencesInPattern :
    Path
    -> ElmSyntax.Abstract.Pattern.Pattern
    -> List ReferenceOccurrence
referencesInPattern path pattern =
    case pattern of
        ElmSyntax.Abstract.Pattern.TuplePattern items ->
            referencesInPatternList path items

        ElmSyntax.Abstract.Pattern.UnConsPattern head tail ->
            referencesInPatternList path [ head, tail ]

        ElmSyntax.Abstract.Pattern.ListPattern items ->
            referencesInPatternList path items

        ElmSyntax.Abstract.Pattern.NamedPattern qualifiedNameRef arguments ->
            { moduleName = qualifiedNameRef.moduleName
            , name = qualifiedNameRef.name
            , path = path
            }
                :: referencesInPatternList path arguments

        _ ->
            []


referencesInPatternList :
    Path
    -> List ElmSyntax.Abstract.Pattern.Pattern
    -> List ReferenceOccurrence
referencesInPatternList path items =
    List.concat
        (List.indexedMap
            (\index item ->
                referencesInPattern
                    (ElmSyntax.Path.appendStep path (StepChild index))
                    item
            )
            items
        )


referencesInExpression :
    Path
    -> ElmSyntax.Abstract.Expression.Expression
    -> List ReferenceOccurrence
referencesInExpression path expression =
    case expression of
        ElmSyntax.Abstract.Expression.Identifier moduleName name ->
            [ { moduleName = moduleName, name = name, path = path } ]

        ElmSyntax.Abstract.Expression.Application function arguments ->
            referencesInExpressionList path (function :: arguments)

        ElmSyntax.Abstract.Expression.OperatorApplication _ _ left right ->
            referencesInExpressionList path [ left, right ]

        ElmSyntax.Abstract.Expression.IfBlock condition thenBranch elseBranch ->
            referencesInExpressionList path [ condition, thenBranch, elseBranch ]

        ElmSyntax.Abstract.Expression.Negation inner ->
            referencesInExpressionList path [ inner ]

        ElmSyntax.Abstract.Expression.TupledExpression items ->
            referencesInExpressionList path items

        ElmSyntax.Abstract.Expression.ListExpr items ->
            referencesInExpressionList path items

        ElmSyntax.Abstract.Expression.LetExpression declarations letBody ->
            List.concat
                [ List.concat
                    (List.indexedMap
                        (\index letDeclaration ->
                            referencesInLetDeclaration
                                (ElmSyntax.Path.appendStep path (StepLetDeclaration index))
                                letDeclaration
                        )
                        declarations
                    )
                , referencesInExpression
                    (ElmSyntax.Path.appendStep path StepBody)
                    letBody
                ]

        ElmSyntax.Abstract.Expression.CaseExpression subject cases ->
            List.concat
                [ referencesInExpression
                    (ElmSyntax.Path.appendStep path (StepChild 0))
                    subject
                , List.concat
                    (List.indexedMap
                        (\index caseBranch ->
                            List.concat
                                [ referencesInPattern
                                    (path ++ [ StepCaseBranch index, StepPattern ])
                                    caseBranch.pattern
                                , referencesInExpression
                                    (path ++ [ StepCaseBranch index, StepBody ])
                                    caseBranch.expression
                                ]
                        )
                        cases
                    )
                ]

        ElmSyntax.Abstract.Expression.LambdaExpression _ lambdaBody ->
            referencesInExpression
                (ElmSyntax.Path.appendStep path StepBody)
                lambdaBody

        ElmSyntax.Abstract.Expression.RecordExpr setters ->
            referencesInRecordSetters path setters

        ElmSyntax.Abstract.Expression.RecordUpdateExpression recordName setters ->
            { moduleName = [], name = recordName, path = path }
                :: referencesInRecordSetters path setters

        ElmSyntax.Abstract.Expression.RecordAccess record _ ->
            referencesInExpressionList path [ record ]

        _ ->
            []


referencesInExpressionList :
    Path
    -> List ElmSyntax.Abstract.Expression.Expression
    -> List ReferenceOccurrence
referencesInExpressionList path items =
    List.concat
        (List.indexedMap
            (\index item ->
                referencesInExpression
                    (ElmSyntax.Path.appendStep path (StepChild index))
                    item
            )
            items
        )


referencesInRecordSetters :
    Path
    -> List ElmSyntax.Abstract.Expression.RecordSetter
    -> List ReferenceOccurrence
referencesInRecordSetters path setters =
    List.concat
        (List.map
            (\( setter, setterPath ) ->
                referencesInExpression
                    (ElmSyntax.Path.appendStep setterPath (StepChild 0))
                    setter.value
            )
            (recordSettersWithPaths path setters)
        )


referencesInLetDeclaration :
    Path
    -> ElmSyntax.Abstract.Expression.LetDeclaration
    -> List ReferenceOccurrence
referencesInLetDeclaration path letDeclaration =
    case letDeclaration of
        ElmSyntax.Abstract.Expression.LetFunction functionStruct ->
            referencesInFunction path functionStruct

        ElmSyntax.Abstract.Expression.LetDestructuring _ destructured ->
            referencesInExpression
                (ElmSyntax.Path.appendStep path StepBody)
                destructured



-- Record field addressing


{-| Record fields are reordered in the abstract model, so they are addressed by
field name plus the occurrence index among fields of the same name.
-}
recordSettersWithPaths :
    Path
    -> List ElmSyntax.Abstract.Expression.RecordSetter
    -> List ( ElmSyntax.Abstract.Expression.RecordSetter, Path )
recordSettersWithPaths path setters =
    List.map2
        (\setter step -> ( setter, ElmSyntax.Path.appendStep path step ))
        setters
        (recordFieldSteps (List.map .fieldName setters))


recordFieldsWithPaths :
    Path
    -> List ElmSyntax.Abstract.TypeAnnotation.RecordField
    -> List ( ElmSyntax.Abstract.TypeAnnotation.RecordField, Path )
recordFieldsWithPaths path fields =
    List.map2
        (\field step -> ( field, ElmSyntax.Path.appendStep path step ))
        fields
        (recordFieldSteps (List.map .fieldName fields))


recordFieldSteps : List String -> List Step
recordFieldSteps fieldNames =
    recordFieldStepsHelp fieldNames []


recordFieldStepsHelp : List String -> List String -> List Step
recordFieldStepsHelp remaining seen =
    case remaining of
        [] ->
            []

        fieldName :: rest ->
            StepRecordField
                fieldName
                (List.length (List.filter (\earlier -> earlier == fieldName) seen))
                :: recordFieldStepsHelp rest (fieldName :: seen)



-- Exposing helpers


exposesFunction : String -> ElmSyntax.Abstract.Exposing.Exposing -> Bool
exposesFunction name exposingList =
    case exposingList of
        ElmSyntax.Abstract.Exposing.All ->
            True

        ElmSyntax.Abstract.Exposing.Explicit entries ->
            List.any
                (\entry ->
                    case entry of
                        ElmSyntax.Abstract.Exposing.FunctionExpose exposedName ->
                            exposedName == name

                        _ ->
                            False
                )
                entries


exposesTypeOrAlias : String -> ElmSyntax.Abstract.Exposing.Exposing -> Bool
exposesTypeOrAlias name exposingList =
    case exposingList of
        ElmSyntax.Abstract.Exposing.All ->
            True

        ElmSyntax.Abstract.Exposing.Explicit entries ->
            List.any
                (\entry ->
                    case entry of
                        ElmSyntax.Abstract.Exposing.TypeOrAliasExpose exposedName ->
                            exposedName == name

                        ElmSyntax.Abstract.Exposing.TypeExpose exposedType ->
                            exposedType.name == name

                        ElmSyntax.Abstract.Exposing.InfixExpose _ ->
                            False

                        ElmSyntax.Abstract.Exposing.FunctionExpose exposedName ->
                            exposedName == name
                )
                entries


nameOfTopLevelExpose : ElmSyntax.Abstract.Exposing.TopLevelExpose -> String
nameOfTopLevelExpose expose =
    case expose of
        ElmSyntax.Abstract.Exposing.InfixExpose name ->
            name

        ElmSyntax.Abstract.Exposing.FunctionExpose name ->
            name

        ElmSyntax.Abstract.Exposing.TypeOrAliasExpose name ->
            name

        ElmSyntax.Abstract.Exposing.TypeExpose exposedType ->
            exposedType.name
