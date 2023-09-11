module Tests exposing (..)

import Bytes.Encode
import CompileBackendApp
import CompileElmApp
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Module
import Elm.Syntax.Node
import Expect
import Json.Decode
import Main
import Result.Extra
import Test


defaultSourceDirs : CompileElmApp.SourceDirectories
defaultSourceDirs =
    { mainSourceDirectoryPath = [ "src" ]
    , elmJsonDirectoryPath = []
    }


dependencies_encoding_roundtrip : Test.Test
dependencies_encoding_roundtrip =
    [ ( "ElmMakeDependency Empty "
      , CompileElmApp.ElmMakeDependency
            { files = Dict.empty
            , entryPointFilePath = []
            , enableDebug = False
            , outputType = CompileElmApp.ElmMakeOutputTypeHtml
            }
      )
    , ( "ElmMakeDependency with only Main.elm to HTML"
      , CompileElmApp.ElmMakeDependency
            { files =
                [ ( defaultSourceDirs.elmJsonDirectoryPath ++ [ "elm.json" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "elm.json content")
                  )
                , ( defaultSourceDirs.mainSourceDirectoryPath ++ [ "Main.elm" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "Main.elm content")
                  )
                ]
                    |> Dict.fromList
            , entryPointFilePath = defaultSourceDirs.mainSourceDirectoryPath ++ [ "Main.elm" ]
            , enableDebug = True
            , outputType = CompileElmApp.ElmMakeOutputTypeHtml
            }
      )
    , ( "ElmMakeDependency with only Main.elm to JavaScript"
      , CompileElmApp.ElmMakeDependency
            { files =
                [ ( defaultSourceDirs.elmJsonDirectoryPath ++ [ "elm.json" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "elm.json content")
                  )
                , ( defaultSourceDirs.mainSourceDirectoryPath ++ [ "Main.elm" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "Main.elm content")
                  )
                ]
                    |> Dict.fromList
            , entryPointFilePath = defaultSourceDirs.mainSourceDirectoryPath ++ [ "Main.elm" ]
            , enableDebug = True
            , outputType = CompileElmApp.ElmMakeOutputTypeJs
            }
      )
    ]
        |> List.map
            (\( testName, dependency ) ->
                Test.test testName <|
                    \() ->
                        dependency
                            |> Main.jsonEncodeDependencyKey
                            |> Json.Decode.decodeValue Main.jsonDecodeDependencyKey
                            |> Expect.equal (Ok dependency)
            )
        |> Test.describe "Dependency key encoding roundtrip"


backend_state_type_name_from_root_elm_module : Test.Test
backend_state_type_name_from_root_elm_module =
    [ ( "Without module name qualifier"
      , """module Backend.Main exposing
    ( State
    , webServiceMain
    )

type alias State = { field_name : Int }


webServiceMain : ElmFullstack.BackendConfig State
webServiceMain =
    { init = ( {}, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = always (\\s -> ( s, [] ))
    , posixTimeIsPast = Nothing
    }
"""
      , Ok
            { stateTypeAnnotation =
                CompileElmApp.RecordElmType
                    { fields = [ ( "field_name", CompileElmApp.LeafElmType CompileElmApp.IntLeaf ) ] }
            , dependencies = Dict.empty
            , instantiatedConfigTypeName = [ "ElmFullstack", "BackendConfig" ]
            }
      )
    ]
        |> List.map
            (\( testName, moduleText, expectedResult ) ->
                Test.test testName <|
                    \() ->
                        moduleText
                            |> CompileElmApp.parseElmModuleText
                            |> Result.mapError
                                (\error ->
                                    "Failed to parse supporting module '"
                                        ++ (moduleText
                                                |> String.lines
                                                |> List.head
                                                |> Maybe.withDefault "???"
                                           )
                                        ++ "': "
                                        ++ CompileElmApp.parserDeadEndsToString moduleText error
                                )
                            |> Result.andThen
                                (\parsedModule ->
                                    parsedModule.declarations
                                        |> List.map Elm.Syntax.Node.value
                                        |> List.filterMap
                                            (\declaration ->
                                                case declaration of
                                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                                        Just functionDeclaration

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head
                                        |> Maybe.map (Tuple.pair parsedModule >> Ok)
                                        |> Maybe.withDefault (Err "Did not find root declaration")
                                )
                            |> Result.map
                                (\( parsedModule, rootFunctionDeclaration ) ->
                                    let
                                        moduleFilePath =
                                            defaultSourceDirs.mainSourceDirectoryPath ++ [ "Backend", "Main.elm" ]

                                        sourceModules =
                                            [ ( moduleFilePath
                                              , Bytes.Encode.encode (Bytes.Encode.string moduleText)
                                              )
                                            ]
                                                |> Dict.fromList
                                                |> CompileElmApp.elmModulesDictFromAppFiles
                                                |> Dict.toList
                                                |> List.filterMap
                                                    (\( filePath, moduleResult ) ->
                                                        moduleResult
                                                            |> Result.toMaybe
                                                            |> Maybe.map (Tuple.pair filePath)
                                                    )
                                                |> Dict.fromList
                                    in
                                    CompileBackendApp.parseAppStateElmTypeAndDependenciesRecursively
                                        rootFunctionDeclaration
                                        sourceModules
                                        ( moduleFilePath, parsedModule )
                                        |> Expect.equal expectedResult
                                )
                            |> Result.Extra.unpack Expect.fail identity
            )
        |> Test.describe "state type name from root Elm module"


parse_elm_type_annotation : Test.Test
parse_elm_type_annotation =
    [ { testName = "Leaf String"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.LeafElmType CompileElmApp.StringLeaf, Dict.empty )
      , rootTypeAnnotationText = "String"
      }
    , { testName = "Leaf Int"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.LeafElmType CompileElmApp.IntLeaf, Dict.empty )
      , rootTypeAnnotationText = "Int"
      }
    , { testName = "Leaf Bool"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.LeafElmType CompileElmApp.BoolLeaf, Dict.empty )
      , rootTypeAnnotationText = "Bool"
      }
    , { testName = "Leaf Float"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.LeafElmType CompileElmApp.FloatLeaf, Dict.empty )
      , rootTypeAnnotationText = "Float"
      }
    , { testName = "Leaf Bytes"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.LeafElmType CompileElmApp.BytesLeaf, Dict.empty )
      , rootTypeAnnotationText = "Bytes.Bytes"
      }
    , { testName = "Unit"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.UnitType, Dict.empty )
      , rootTypeAnnotationText = " () "
      }
    , { testName = "Empty Record"
      , modulesTexts = []
      , expectedResult = Ok ( CompileElmApp.RecordElmType { fields = [] }, Dict.empty )
      , rootTypeAnnotationText = " {  } "
      }
    , { testName = "Record with simple fields"
      , modulesTexts = []
      , expectedResult =
            Ok
                ( CompileElmApp.RecordElmType
                    { fields =
                        [ ( "a", CompileElmApp.LeafElmType CompileElmApp.IntLeaf )
                        , ( "b", CompileElmApp.LeafElmType CompileElmApp.StringLeaf )
                        ]
                    }
                , Dict.empty
                )
      , rootTypeAnnotationText = " { a : Int, b : String } "
      }
    , { testName = "Result String Int"
      , modulesTexts = []
      , expectedResult =
            Ok
                ( CompileElmApp.InstanceElmType
                    { instantiated = CompileElmApp.LeafElmType CompileElmApp.ResultLeaf
                    , arguments =
                        [ CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                        , CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                        ]
                    }
                , Dict.empty
                )
      , rootTypeAnnotationText = " Result String Int "
      }
    , { testName = "Alias Int"
      , modulesTexts =
            [ [ "module WithAlias exposing (..)"
              , ""
              , "type alias OurAlias = Int"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                , Dict.empty
                )
      , rootTypeAnnotationText = " WithAlias.OurAlias "
      }
    , { testName = "Choice type"
      , modulesTexts =
            [ [ "module WithChoice exposing (..)"
              , ""
              , "type ChoiceType = TagA | TagB"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileElmApp.ChoiceElmType "WithChoice.ChoiceType"
                , [ ( "WithChoice.ChoiceType"
                    , { parameters = []
                      , tags =
                            [ ( "TagA", [] )
                            , ( "TagB", [] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithChoice.ChoiceType "
      }
    , { testName = "Choice type with parameterized tags"
      , modulesTexts =
            [ [ "module WithChoice exposing (..)"
              , ""
              , "type ChoiceType = TagA String Int | TagB ( Int, String ) | TagC { f0 : Int, f1 : String }"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileElmApp.ChoiceElmType "WithChoice.ChoiceType"
                , [ ( "WithChoice.ChoiceType"
                    , { parameters = []
                      , tags =
                            [ ( "TagA"
                              , [ CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                                , CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                                ]
                              )
                            , ( "TagB"
                              , [ CompileElmApp.TupleElmType
                                    [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                                    , CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                                    ]
                                ]
                              )
                            , ( "TagC"
                              , [ CompileElmApp.RecordElmType
                                    { fields =
                                        [ ( "f0", CompileElmApp.LeafElmType CompileElmApp.IntLeaf )
                                        , ( "f1", CompileElmApp.LeafElmType CompileElmApp.StringLeaf )
                                        ]
                                    }
                                ]
                              )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithChoice.ChoiceType "
      }
    , { testName = "Recursive Choice type"
      , modulesTexts =
            [ [ "module WithChoice exposing (..)"
              , ""
              , "type RecursiveType = TagTerminate Int | TagRecurse RecursiveType"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileElmApp.ChoiceElmType "WithChoice.RecursiveType"
                , [ ( "WithChoice.RecursiveType"
                    , { parameters = []
                      , tags =
                            [ ( "TagTerminate", [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ] )
                            , ( "TagRecurse", [ CompileElmApp.ChoiceElmType "WithChoice.RecursiveType" ] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithChoice.RecursiveType "
      }
    , { testName = "Choice type instance"
      , modulesTexts =
            [ [ "module WithChoice exposing (..)"
              , ""
              , "type ChoiceType a = TagA a | TagB"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileElmApp.InstanceElmType
                    { instantiated = CompileElmApp.ChoiceElmType "WithChoice.ChoiceType"
                    , arguments = [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ]
                    }
                , [ ( "WithChoice.ChoiceType"
                    , { parameters = [ "a" ]
                      , tags =
                            [ ( "TagA", [ CompileElmApp.GenericType "a" ] )
                            , ( "TagB", [] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithChoice.ChoiceType Int "
      }
    , { testName = "Parameterized record alias instance"
      , modulesTexts =
            [ [ "module WithAlias exposing (..)"
              , ""
              , "type alias RecordAlias a = { parameterized_field : a }"
              ]
                |> String.join "\n"
            ]
      , rootTypeAnnotationText = " WithAlias.RecordAlias Int "
      , expectedResult =
            Ok
                ( CompileElmApp.RecordElmType
                    { fields = [ ( "parameterized_field", CompileElmApp.LeafElmType CompileElmApp.IntLeaf ) ]
                    }
                , Dict.empty
                )
      }
    , { testName = "Record field List instance"
      , modulesTexts =
            [ [ "module Main exposing (..)"
              , "import OtherModule"
              , ""
              , "type alias OurAlias = { field_list : List OtherModule.OurType }"
              ]
            , [ "module OtherModule exposing (..)"
              , ""
              , "type OurType = TagA"
              ]
            ]
                |> List.map (String.join "\n")
      , expectedResult =
            Ok
                ( CompileElmApp.RecordElmType
                    { fields =
                        [ ( "field_list"
                          , CompileElmApp.InstanceElmType
                                { instantiated = CompileElmApp.LeafElmType CompileElmApp.ListLeaf
                                , arguments = [ CompileElmApp.ChoiceElmType "OtherModule.OurType" ]
                                }
                          )
                        ]
                    }
                , [ ( "OtherModule.OurType"
                    , { parameters = []
                      , tags = [ ( "TagA", [] ) ] |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " Main.OurAlias "
      }
    , { testName = "Aliased module import"
      , modulesTexts =
            [ [ "module Main exposing (..)"
              , "import Namespace.SomeModule as AliasedModule"
              , ""
              , "type alias OurAlias = List AliasedModule.OurType"
              ]
            , [ "module Namespace.SomeModule exposing (..)"
              , ""
              , "type OurType = TagA"
              ]
            ]
                |> List.map (String.join "\n")
      , expectedResult =
            Ok
                ( CompileElmApp.InstanceElmType
                    { instantiated = CompileElmApp.LeafElmType CompileElmApp.ListLeaf
                    , arguments = [ CompileElmApp.ChoiceElmType "Namespace.SomeModule.OurType" ]
                    }
                , [ ( "Namespace.SomeModule.OurType"
                    , { parameters = []
                      , tags = [ ( "TagA", [] ) ] |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " Main.OurAlias "
      }
    , { testName = "Aliased via exposing module import"
      , modulesTexts =
            [ """
module OpaqueChoiceType exposing (OpaqueChoiceType, constructTagA, constructTagB)


type OpaqueChoiceType
    = TagA
    | TagB Int
            """
            , """
module Structures exposing (..)

import OpaqueChoiceType exposing (OpaqueChoiceType)


type alias MixedRecord =
    { int : Int
    , opaqueChoiceType : OpaqueChoiceType
    }
            """
            ]
                |> List.map String.trim
      , expectedResult =
            Ok
                ( CompileElmApp.RecordElmType
                    { fields =
                        [ ( "int", CompileElmApp.LeafElmType CompileElmApp.IntLeaf )
                        , ( "opaqueChoiceType", CompileElmApp.ChoiceElmType "OpaqueChoiceType.OpaqueChoiceType" )
                        ]
                    }
                , [ ( "OpaqueChoiceType.OpaqueChoiceType"
                    , { parameters = []
                      , tags =
                            [ ( "TagA", [] )
                            , ( "TagB", [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " Structures.MixedRecord "
      }
    ]
        |> List.map
            (\{ testName, modulesTexts, expectedResult, rootTypeAnnotationText } ->
                Test.test testName <|
                    \() ->
                        modulesTexts
                            |> List.map
                                (\moduleText ->
                                    moduleText
                                        |> CompileElmApp.parseElmModuleText
                                        |> Result.mapError
                                            (\error ->
                                                "Failed to parse supporting module '"
                                                    ++ (moduleText
                                                            |> String.lines
                                                            |> List.head
                                                            |> Maybe.withDefault "???"
                                                       )
                                                    ++ "': "
                                                    ++ CompileElmApp.parserDeadEndsToString moduleText error
                                            )
                                )
                            |> Result.Extra.combine
                            |> Result.andThen
                                (\modules ->
                                    let
                                        filePathFromElmModuleName =
                                            CompileElmApp.filePathFromElmModuleName

                                        namedModules =
                                            modules
                                                |> List.map
                                                    (\parsedModule ->
                                                        let
                                                            moduleName =
                                                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)
                                                        in
                                                        ( filePathFromElmModuleName defaultSourceDirs moduleName
                                                        , { fileText = "fake"
                                                          , parsedSyntax = parsedModule
                                                          , moduleName = moduleName
                                                          }
                                                        )
                                                    )
                                                |> Dict.fromList

                                        rootModuleImports =
                                            namedModules
                                                |> Dict.values
                                                |> List.map (.moduleName >> String.join "." >> (++) "import ")

                                        rootModuleText =
                                            ("module Root exposing (..)"
                                                :: ""
                                                :: rootModuleImports
                                                ++ [ "declaration : " ++ rootTypeAnnotationText
                                                   , "declaration = 123"
                                                   ]
                                            )
                                                |> String.join "\n"
                                    in
                                    rootModuleText
                                        |> CompileElmApp.parseElmModuleText
                                        |> Result.mapError (CompileElmApp.parserDeadEndsToString rootModuleText)
                                        |> Result.andThen
                                            (\rootModule ->
                                                rootModule.declarations
                                                    |> List.filterMap
                                                        (\declaration ->
                                                            case Elm.Syntax.Node.value declaration of
                                                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                                                    Maybe.map Elm.Syntax.Node.value functionDeclaration.signature

                                                                _ ->
                                                                    Nothing
                                                        )
                                                    |> List.filterMap
                                                        (\functionSignature ->
                                                            if Elm.Syntax.Node.value functionSignature.name /= "declaration" then
                                                                Nothing

                                                            else
                                                                Just functionSignature.typeAnnotation
                                                        )
                                                    |> List.head
                                                    |> Maybe.map Ok
                                                    |> Maybe.withDefault (Err "Did not find declaration")
                                                    |> Result.map
                                                        (\declarationAnnotation ->
                                                            declarationAnnotation
                                                                |> Tuple.pair
                                                                    (Tuple.pair
                                                                        (rootModule.moduleDefinition
                                                                            |> Elm.Syntax.Node.value
                                                                            |> Elm.Syntax.Module.moduleName
                                                                            |> filePathFromElmModuleName defaultSourceDirs
                                                                        )
                                                                        rootModule
                                                                    )
                                                                |> CompileElmApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation namedModules
                                                                |> Expect.equal expectedResult
                                                        )
                                            )
                                )
                            |> Result.Extra.unpack Expect.fail identity
            )
        |> Test.describe "parse Elm type annotation"


emit_json_coding_expression_from_type : Test.Test
emit_json_coding_expression_from_type =
    [ { testName = "Leaf String"
      , typeAnnotation = CompileElmApp.LeafElmType CompileElmApp.StringLeaf
      , expectedResult =
            { encodeExpression = "Json.Encode.string valueToEncode"
            , decodeExpression = "Json.Decode.string"
            }
      }
    , { testName = "Unit"
      , typeAnnotation = CompileElmApp.UnitType
      , expectedResult =
            { encodeExpression = "Json.Encode.list (always (Json.Encode.object [])) []"
            , decodeExpression = "Json.Decode.succeed ()"
            }
      }
    , { testName = "Empty record"
      , typeAnnotation = CompileElmApp.RecordElmType { fields = [] }
      , expectedResult =
            { encodeExpression = "Json.Encode.object []"
            , decodeExpression = "Json.Decode.succeed {}"
            }
      }
    , { testName = "Record with one primitive field"
      , typeAnnotation =
            CompileElmApp.RecordElmType
                { fields = [ ( "field_name", CompileElmApp.LeafElmType CompileElmApp.StringLeaf ) ] }
      , expectedResult =
            { encodeExpression = String.trim """
Json.Encode.object
    [ ( "field_name"
      , Json.Encode.string valueToEncode.field_name
      )
    ]"""
            , decodeExpression = String.trim """
Json.Decode.succeed (\\field_name -> { field_name = field_name })
    |> jsonDecode_andMap
        ( Json.Decode.field "field_name"
            Json.Decode.string
        )
"""
            }
      }
    , { testName = "Record with two primitive fields"
      , typeAnnotation =
            CompileElmApp.RecordElmType
                { fields =
                    [ ( "field_a", CompileElmApp.LeafElmType CompileElmApp.IntLeaf )
                    , ( "field_b", CompileElmApp.LeafElmType CompileElmApp.StringLeaf )
                    ]
                }
      , expectedResult =
            { encodeExpression = String.trim """
Json.Encode.object
    [ ( "field_a"
      , Json.Encode.int valueToEncode.field_a
      )
    , ( "field_b"
      , Json.Encode.string valueToEncode.field_b
      )
    ]"""
            , decodeExpression = String.trim """
Json.Decode.succeed (\\field_a field_b -> { field_a = field_a, field_b = field_b })
    |> jsonDecode_andMap
        ( Json.Decode.field "field_a"
            Json.Decode.int
        )
    |> jsonDecode_andMap
        ( Json.Decode.field "field_b"
            Json.Decode.string
        )
"""
            }
      }
    , { testName = "Nested record"
      , typeAnnotation =
            CompileElmApp.RecordElmType
                { fields =
                    [ ( "field_a"
                      , CompileElmApp.RecordElmType
                            { fields =
                                [ ( "field_c", CompileElmApp.LeafElmType CompileElmApp.IntLeaf ) ]
                            }
                      )
                    , ( "field_b", CompileElmApp.LeafElmType CompileElmApp.StringLeaf )
                    ]
                }
      , expectedResult =
            { encodeExpression = String.trim """
Json.Encode.object
    [ ( "field_a"
      , Json.Encode.object
            [ ( "field_c"
              , Json.Encode.int valueToEncode.field_a.field_c
              )
            ]
      )
    , ( "field_b"
      , Json.Encode.string valueToEncode.field_b
      )
    ]"""
            , decodeExpression = String.trim """
Json.Decode.succeed (\\field_a field_b -> { field_a = field_a, field_b = field_b })
    |> jsonDecode_andMap
        ( Json.Decode.field "field_a"
            ( Json.Decode.succeed (\\field_c -> { field_c = field_c })
                |> jsonDecode_andMap
                    ( Json.Decode.field "field_c"
                        Json.Decode.int
                    )
            )
        )
    |> jsonDecode_andMap
        ( Json.Decode.field "field_b"
            Json.Decode.string
        )
"""
            }
      }
    , { testName = "Tuple"
      , typeAnnotation =
            CompileElmApp.TupleElmType
                [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                , CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                ]
      , expectedResult =
            { encodeExpression = String.trim """
Json.Encode.list identity
    [ Json.Encode.int ((\\( item_0, item_1 ) -> item_0) valueToEncode)
    , Json.Encode.string ((\\( item_0, item_1 ) -> item_1) valueToEncode)
    ]"""
            , decodeExpression = String.trim """
Json.Decode.map2 (\\item_0 item_1 -> ( item_0, item_1 ))
    (Json.Decode.index 0 Json.Decode.int)
    (Json.Decode.index 1 Json.Decode.string)
"""
            }
      }
    , { testName = "Triple"
      , typeAnnotation =
            CompileElmApp.TupleElmType
                [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                , CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                , CompileElmApp.LeafElmType CompileElmApp.BoolLeaf
                ]
      , expectedResult =
            { encodeExpression = String.trim """
Json.Encode.list identity
    [ Json.Encode.int ((\\( item_0, item_1, item_2 ) -> item_0) valueToEncode)
    , Json.Encode.string ((\\( item_0, item_1, item_2 ) -> item_1) valueToEncode)
    , Json.Encode.bool ((\\( item_0, item_1, item_2 ) -> item_2) valueToEncode)
    ]"""
            , decodeExpression = String.trim """
Json.Decode.map3 (\\item_0 item_1 item_2 -> ( item_0, item_1, item_2 ))
    (Json.Decode.index 0 Json.Decode.int)
    (Json.Decode.index 1 Json.Decode.string)
    (Json.Decode.index 2 Json.Decode.bool)
"""
            }
      }
    , { testName = "List Int"
      , typeAnnotation =
            CompileElmApp.InstanceElmType
                { instantiated = CompileElmApp.LeafElmType CompileElmApp.ListLeaf
                , arguments = [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_List (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_List Json.Decode.int"
            }
      }
    , { testName = "Maybe Int"
      , typeAnnotation =
            CompileElmApp.InstanceElmType
                { instantiated = CompileElmApp.LeafElmType CompileElmApp.MaybeLeaf
                , arguments = [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_Maybe (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_Maybe Json.Decode.int"
            }
      }
    , { testName = "Result String Int"
      , typeAnnotation =
            CompileElmApp.InstanceElmType
                { instantiated = CompileElmApp.LeafElmType CompileElmApp.ResultLeaf
                , arguments =
                    [ CompileElmApp.LeafElmType CompileElmApp.StringLeaf
                    , CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                    ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_Result (\\type_arg -> Json.Encode.string type_arg) (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_Result Json.Decode.string Json.Decode.int"
            }
      }
    , { testName = "Instance of generic choice type"
      , typeAnnotation =
            CompileElmApp.InstanceElmType
                { instantiated = CompileElmApp.ChoiceElmType "OwnModule.ChoiceType"
                , arguments =
                    [ CompileElmApp.LeafElmType CompileElmApp.BoolLeaf
                    ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode_OwnModule_ChoiceType (\\type_arg -> Json.Encode.bool type_arg) valueToEncode"
            , decodeExpression = "jsonDecode_OwnModule_ChoiceType Json.Decode.bool"
            }
      }
    ]
        |> List.map
            (\{ testName, typeAnnotation, expectedResult } ->
                Test.test testName <|
                    \() ->
                        ( typeAnnotation, [] )
                            |> CompileElmApp.jsonConverterExpressionFromType
                                { encodeValueExpression = "valueToEncode", typeArgLocalName = "type_arg" }
                            |> Expect.equal expectedResult
            )
        |> Test.describe "emit json coding expressions from type"


emit_json_coding_expression_from_choice_type : Test.Test
emit_json_coding_expression_from_choice_type =
    [ { testName = "One tag without parameters"
      , choiceTypeName = "ModuleName.ChoiceType"
      , choiceType =
            { parameters = []
            , tags = [ ( "TagA", [] ) ] |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_ChoiceType valueToEncode =
    case valueToEncode of
        ModuleName.TagA ->
            Json.Encode.object [ ( "TagA", Json.Encode.list identity [] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_ChoiceType =
    Json.Decode.oneOf
        [ Json.Decode.field "TagA" (jsonDecodeSucceedWhenNotNull ModuleName.TagA)
        ]
"""
            }
      }
    , { testName = "Recursive type"
      , choiceTypeName = "ModuleName.RecursiveType"
      , choiceType =
            { parameters = []
            , tags =
                [ ( "TagRecurse", [ CompileElmApp.ChoiceElmType "ModuleName.RecursiveType" ] )
                , ( "TagTerminate", [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ] )
                ]
                    |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_RecursiveType valueToEncode =
    case valueToEncode of
        ModuleName.TagRecurse tagArgument0 ->
            Json.Encode.object [ ( "TagRecurse", Json.Encode.list identity [ jsonEncode_ModuleName_RecursiveType tagArgument0 ] ) ]
        ModuleName.TagTerminate tagArgument0 ->
            Json.Encode.object [ ( "TagTerminate", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_RecursiveType =
    Json.Decode.oneOf
        [ Json.Decode.field "TagRecurse" (Json.Decode.lazy (\\_ -> Json.Decode.map ModuleName.TagRecurse (Json.Decode.index 0 jsonDecode_ModuleName_RecursiveType)))
        , Json.Decode.field "TagTerminate" (Json.Decode.lazy (\\_ -> Json.Decode.map ModuleName.TagTerminate (Json.Decode.index 0 Json.Decode.int)))
        ]
"""
            }
      }
    , { testName = "Tag with generic type instance"
      , choiceTypeName = "ModuleName.TypeName"
      , choiceType =
            { parameters = []
            , tags =
                [ ( "TagList"
                  , [ CompileElmApp.InstanceElmType
                        { instantiated = CompileElmApp.LeafElmType CompileElmApp.ListLeaf
                        , arguments = [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf ]
                        }
                    ]
                  )
                ]
                    |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_TypeName valueToEncode =
    case valueToEncode of
        ModuleName.TagList tagArgument0 ->
            Json.Encode.object [ ( "TagList", Json.Encode.list identity [ jsonEncode__generic_List (\\type_arg -> Json.Encode.int type_arg) tagArgument0 ] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_TypeName =
    Json.Decode.oneOf
        [ Json.Decode.field "TagList" (Json.Decode.lazy (\\_ -> Json.Decode.map ModuleName.TagList (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.int))))
        ]
"""
            }
      }
    , { testName = "One tag with two parameters"
      , choiceTypeName = "ModuleName.TypeName"
      , choiceType =
            { parameters = []
            , tags =
                [ ( "TagAlpha"
                  , [ CompileElmApp.LeafElmType CompileElmApp.IntLeaf
                    , CompileElmApp.LeafElmType CompileElmApp.BoolLeaf
                    ]
                  )
                ]
                    |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_TypeName valueToEncode =
    case valueToEncode of
        ModuleName.TagAlpha tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "TagAlpha", Json.Encode.list identity [ Json.Encode.int tagArgument0, Json.Encode.bool tagArgument1 ] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_TypeName =
    Json.Decode.oneOf
        [ Json.Decode.field "TagAlpha" (Json.Decode.lazy (\\_ -> Json.Decode.map2 ModuleName.TagAlpha (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.bool)))
        ]
"""
            }
      }
    , { testName = "One tag with generic parameter"
      , choiceTypeName = "ModuleName.TypeName"
      , choiceType =
            { parameters = [ "test" ]
            , tags =
                [ ( "TagAlpha"
                  , [ CompileElmApp.GenericType "test"
                    ]
                  )
                ]
                    |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_TypeName jsonEncode_type_parameter_test valueToEncode =
    case valueToEncode of
        ModuleName.TagAlpha tagArgument0 ->
            Json.Encode.object [ ( "TagAlpha", Json.Encode.list identity [ jsonEncode_type_parameter_test tagArgument0 ] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_TypeName jsonDecode_type_parameter_test =
    Json.Decode.oneOf
        [ Json.Decode.field "TagAlpha" (Json.Decode.lazy (\\_ -> Json.Decode.map ModuleName.TagAlpha (Json.Decode.index 0 jsonDecode_type_parameter_test)))
        ]
"""
            }
      }
    , { testName = "ListDict"
      , choiceTypeName = "ListDict.Dict"
      , choiceType =
            { parameters = [ "key", "value" ]
            , tags =
                [ ( "Dict"
                  , [ CompileElmApp.InstanceElmType
                        { instantiated = CompileElmApp.LeafElmType CompileElmApp.ListLeaf
                        , arguments =
                            [ CompileElmApp.TupleElmType
                                [ CompileElmApp.GenericType "key"
                                , CompileElmApp.GenericType "value"
                                ]
                            ]
                        }
                    ]
                  )
                ]
                    |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ListDict_Dict jsonEncode_type_parameter_key jsonEncode_type_parameter_value valueToEncode =
    case valueToEncode of
        ListDict.Dict tagArgument0 ->
            Json.Encode.object [ ( "Dict", Json.Encode.list identity [ jsonEncode__generic_List (\\type_arg -> Json.Encode.list identity
                [ jsonEncode_type_parameter_key ((\\( item_0, item_1 ) -> item_0) type_arg)
                , jsonEncode_type_parameter_value ((\\( item_0, item_1 ) -> item_1) type_arg)
                ]) tagArgument0 ] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ListDict_Dict jsonDecode_type_parameter_key jsonDecode_type_parameter_value =
    Json.Decode.oneOf
        [ Json.Decode.field "Dict" (Json.Decode.lazy (\\_ -> Json.Decode.map ListDict.Dict (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.map2 (\\item_0 item_1 -> ( item_0, item_1 ))
            (Json.Decode.index 0 jsonDecode_type_parameter_key)
            (Json.Decode.index 1 jsonDecode_type_parameter_value))))))
        ]
"""
            }
      }
    ]
        |> List.map
            (\{ testName, choiceType, choiceTypeName, expectedResult } ->
                Test.test testName <|
                    \() ->
                        choiceType
                            |> CompileElmApp.jsonConverterFunctionFromChoiceType
                                { choiceTypeName = choiceTypeName
                                , encodeValueExpression = "valueToEncode"
                                , typeArgLocalName = "type_arg"
                                }
                            |> (\result ->
                                    { encodeFunction = result.encodeFunction.text
                                    , decodeFunction = result.decodeFunction.text
                                    }
                               )
                            |> Expect.equal expectedResult
            )
        |> Test.describe "emit json coding expressions from choice type"
