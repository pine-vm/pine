module Tests exposing (..)

import Bytes.Encode
import CompileFullstackApp
import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Module
import Elm.Syntax.Node
import Expect
import Json.Decode
import Main
import Result.Extra
import Test


parseCompilationInterfaceElmMakeFunctionName : Test.Test
parseCompilationInterfaceElmMakeFunctionName =
    [ ( "elm_make__debug__javascript__base64____src_Frontend_Main_elm"
      , Ok
            ( "src_Frontend_Main_elm"
            , { outputType = CompileFullstackApp.ElmMakeOutputTypeJs
              , enableDebug = True
              , encoding = Just CompileFullstackApp.Base64Encoding
              }
            )
      )
    , ( "elm_make__javascript__base64____src_Frontend_Main_elm"
      , Ok
            ( "src_Frontend_Main_elm"
            , { outputType = CompileFullstackApp.ElmMakeOutputTypeJs
              , enableDebug = False
              , encoding = Just CompileFullstackApp.Base64Encoding
              }
            )
      )
    , ( "elm_make__debug__base64____src_Frontend_Main_elm"
      , Ok
            ( "src_Frontend_Main_elm"
            , { outputType = CompileFullstackApp.ElmMakeOutputTypeHtml
              , enableDebug = True
              , encoding = Just CompileFullstackApp.Base64Encoding
              }
            )
      )
    ]
        |> List.map
            (\( functionName, expectedResult ) ->
                Test.test functionName <|
                    \() ->
                        CompileFullstackApp.parseElmMakeModuleFunctionName functionName
                            |> Expect.equal expectedResult
            )
        |> Test.describe "parse Compilation Interface Elm Make function name"


parseCompilationInterfaceSourceFilesFunctionName : Test.Test
parseCompilationInterfaceSourceFilesFunctionName =
    [ ( "file__utf8____readme_md"
      , Ok
            ( "readme_md"
            , { variant = CompileFullstackApp.SourceFile
              , encoding = Just CompileFullstackApp.Utf8Encoding
              }
            )
      )
    , ( "file_tree____static_content"
      , Ok
            ( "static_content"
            , { variant = CompileFullstackApp.SourceFileTree
              , encoding = Nothing
              }
            )
      )
    ]
        |> List.map
            (\( functionName, expectedResult ) ->
                Test.test functionName <|
                    \() ->
                        CompileFullstackApp.parseSourceFileFunctionName functionName
                            |> Expect.equal expectedResult
            )
        |> Test.describe "parse Compilation Interface Source Files function name"


dependencies_encoding_roundtrip : Test.Test
dependencies_encoding_roundtrip =
    [ ( "ElmMakeDependency Empty "
      , CompileFullstackApp.ElmMakeDependency
            { files = Dict.empty
            , entryPointFilePath = []
            , enableDebug = False
            , outputType = CompileFullstackApp.ElmMakeOutputTypeHtml
            }
      )
    , ( "ElmMakeDependency with only Main.elm to HTML"
      , CompileFullstackApp.ElmMakeDependency
            { files =
                [ ( [ "elm.json" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "elm.json content")
                  )
                , ( [ "src", "Main.elm" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "Main.elm content")
                  )
                ]
                    |> Dict.fromList
            , entryPointFilePath = [ "src", "Main.elm" ]
            , enableDebug = True
            , outputType = CompileFullstackApp.ElmMakeOutputTypeHtml
            }
      )
    , ( "ElmMakeDependency with only Main.elm to JS"
      , CompileFullstackApp.ElmMakeDependency
            { files =
                [ ( [ "elm.json" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "elm.json content")
                  )
                , ( [ "src", "Main.elm" ]
                  , Bytes.Encode.encode (Bytes.Encode.string "Main.elm content")
                  )
                ]
                    |> Dict.fromList
            , entryPointFilePath = [ "src", "Main.elm" ]
            , enableDebug = True
            , outputType = CompileFullstackApp.ElmMakeOutputTypeJs
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


state_type_name_from_root_elm_module : Test.Test
state_type_name_from_root_elm_module =
    [ ( "Without module name qualifier"
      , """module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

type alias State = { field_name : Int }


backendMain : ElmFullstack.BackendConfig State
backendMain =
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
            ( CompileFullstackApp.RecordElmType
                { fields = [ ( "field_name", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ) ] }
            , Dict.empty
            )
      )
    ]
        |> List.map
            (\( testName, moduleText, expectedResult ) ->
                Test.test testName <|
                    \() ->
                        moduleText
                            |> CompileFullstackApp.parseElmModuleText
                            |> Result.mapError
                                (\error ->
                                    "Failed to parse supporting module '"
                                        ++ (moduleText
                                                |> String.lines
                                                |> List.head
                                                |> Maybe.withDefault "???"
                                           )
                                        ++ "': "
                                        ++ CompileFullstackApp.parserDeadEndsToString moduleText error
                                )
                            |> Result.map
                                (\parsedModule ->
                                    let
                                        moduleFilePath =
                                            [ "src", "Backend", "Main.elm" ]

                                        sourceModules =
                                            [ ( moduleFilePath
                                              , Bytes.Encode.encode (Bytes.Encode.string moduleText)
                                              )
                                            ]
                                                |> Dict.fromList
                                                |> CompileFullstackApp.elmModulesDictFromAppFiles
                                                |> Dict.map (always (Tuple.mapSecond Tuple.second))
                                    in
                                    CompileFullstackApp.parseAppStateElmTypeAndDependenciesRecursively
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
      , expectedResult = Ok ( CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf, Dict.empty )
      , rootTypeAnnotationText = "String"
      }
    , { testName = "Leaf Int"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf, Dict.empty )
      , rootTypeAnnotationText = "Int"
      }
    , { testName = "Leaf Bool"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.LeafElmType CompileFullstackApp.BoolLeaf, Dict.empty )
      , rootTypeAnnotationText = "Bool"
      }
    , { testName = "Leaf Float"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.LeafElmType CompileFullstackApp.FloatLeaf, Dict.empty )
      , rootTypeAnnotationText = "Float"
      }
    , { testName = "Leaf Bytes"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.LeafElmType CompileFullstackApp.BytesLeaf, Dict.empty )
      , rootTypeAnnotationText = "Bytes.Bytes"
      }
    , { testName = "Unit"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.UnitType, Dict.empty )
      , rootTypeAnnotationText = " () "
      }
    , { testName = "Empty Record"
      , modulesTexts = []
      , expectedResult = Ok ( CompileFullstackApp.RecordElmType { fields = [] }, Dict.empty )
      , rootTypeAnnotationText = " {  } "
      }
    , { testName = "Record with simple fields"
      , modulesTexts = []
      , expectedResult =
            Ok
                ( CompileFullstackApp.RecordElmType
                    { fields =
                        [ ( "a", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf )
                        , ( "b", CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf )
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
                ( CompileFullstackApp.InstanceElmType
                    { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ResultLeaf
                    , arguments =
                        [ CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
                        , CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
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
                ( CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                , Dict.empty
                )
      , rootTypeAnnotationText = " WithAlias.OurAlias "
      }
    , { testName = "Custom type"
      , modulesTexts =
            [ [ "module WithCustom exposing (..)"
              , ""
              , "type CustomType = TagA | TagB"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileFullstackApp.CustomElmType "WithCustom.CustomType"
                , [ ( "WithCustom.CustomType"
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
      , rootTypeAnnotationText = " WithCustom.CustomType "
      }
    , { testName = "Custom type with parameterized tags"
      , modulesTexts =
            [ [ "module WithCustom exposing (..)"
              , ""
              , "type CustomType = TagA String Int | TagB ( Int, String ) | TagC { f0 : Int, f1 : String }"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileFullstackApp.CustomElmType "WithCustom.CustomType"
                , [ ( "WithCustom.CustomType"
                    , { parameters = []
                      , tags =
                            [ ( "TagA"
                              , [ CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
                                , CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                                ]
                              )
                            , ( "TagB"
                              , [ CompileFullstackApp.TupleElmType
                                    [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                                    , CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
                                    ]
                                ]
                              )
                            , ( "TagC"
                              , [ CompileFullstackApp.RecordElmType
                                    { fields =
                                        [ ( "f0", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf )
                                        , ( "f1", CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf )
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
      , rootTypeAnnotationText = " WithCustom.CustomType "
      }
    , { testName = "Recursive Custom type"
      , modulesTexts =
            [ [ "module WithCustom exposing (..)"
              , ""
              , "type RecursiveType = TagTerminate Int | TagRecurse RecursiveType"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileFullstackApp.CustomElmType "WithCustom.RecursiveType"
                , [ ( "WithCustom.RecursiveType"
                    , { parameters = []
                      , tags =
                            [ ( "TagTerminate", [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ] )
                            , ( "TagRecurse", [ CompileFullstackApp.CustomElmType "WithCustom.RecursiveType" ] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithCustom.RecursiveType "
      }
    , { testName = "Custom type instance"
      , modulesTexts =
            [ [ "module WithCustom exposing (..)"
              , ""
              , "type CustomType a = TagA a | TagB"
              ]
                |> String.join "\n"
            ]
      , expectedResult =
            Ok
                ( CompileFullstackApp.InstanceElmType
                    { instantiated = CompileFullstackApp.CustomElmType "WithCustom.CustomType"
                    , arguments = [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ]
                    }
                , [ ( "WithCustom.CustomType"
                    , { parameters = [ "a" ]
                      , tags =
                            [ ( "TagA", [ CompileFullstackApp.GenericType "a" ] )
                            , ( "TagB", [] )
                            ]
                                |> Dict.fromList
                      }
                    )
                  ]
                    |> Dict.fromList
                )
      , rootTypeAnnotationText = " WithCustom.CustomType Int "
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
                ( CompileFullstackApp.RecordElmType
                    { fields = [ ( "parameterized_field", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ) ]
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
                ( CompileFullstackApp.RecordElmType
                    { fields =
                        [ ( "field_list"
                          , CompileFullstackApp.InstanceElmType
                                { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ListLeaf
                                , arguments = [ CompileFullstackApp.CustomElmType "OtherModule.OurType" ]
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
                ( CompileFullstackApp.InstanceElmType
                    { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ListLeaf
                    , arguments = [ CompileFullstackApp.CustomElmType "Namespace.SomeModule.OurType" ]
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
module OpaqueCustomType exposing (OpaqueCustomType, constructTagA, constructTagB)


type OpaqueCustomType
    = TagA
    | TagB Int
            """
            , """
module Structures exposing (..)

import OpaqueCustomType exposing (OpaqueCustomType)


type alias MixedRecord =
    { int : Int
    , opaqueCustomType : OpaqueCustomType
    }
            """
            ]
                |> List.map String.trim
      , expectedResult =
            Ok
                ( CompileFullstackApp.RecordElmType
                    { fields =
                        [ ( "int", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf )
                        , ( "opaqueCustomType", CompileFullstackApp.CustomElmType "OpaqueCustomType.OpaqueCustomType" )
                        ]
                    }
                , [ ( "OpaqueCustomType.OpaqueCustomType"
                    , { parameters = []
                      , tags =
                            [ ( "TagA", [] )
                            , ( "TagB", [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ] )
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
                                        |> CompileFullstackApp.parseElmModuleText
                                        |> Result.mapError
                                            (\error ->
                                                "Failed to parse supporting module '"
                                                    ++ (moduleText
                                                            |> String.lines
                                                            |> List.head
                                                            |> Maybe.withDefault "???"
                                                       )
                                                    ++ "': "
                                                    ++ CompileFullstackApp.parserDeadEndsToString moduleText error
                                            )
                                )
                            |> Result.Extra.combine
                            |> Result.andThen
                                (\modules ->
                                    let
                                        filePathFromElmModuleName =
                                            CompileFullstackApp.filePathFromElmModuleName

                                        namedModules =
                                            modules
                                                |> List.map
                                                    (\parsedModule ->
                                                        ( Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value parsedModule.moduleDefinition)
                                                        , parsedModule
                                                        )
                                                    )
                                                |> List.map (Tuple.mapFirst (String.join "."))
                                                |> Dict.fromList
                                                |> Dict.map (\moduleName -> Tuple.pair (filePathFromElmModuleName moduleName))

                                        rootModuleImports =
                                            namedModules
                                                |> Dict.keys
                                                |> List.map ((++) "import ")

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
                                        |> CompileFullstackApp.parseElmModuleText
                                        |> Result.mapError (CompileFullstackApp.parserDeadEndsToString rootModuleText)
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
                                                                            |> String.join "."
                                                                            |> filePathFromElmModuleName
                                                                        )
                                                                        rootModule
                                                                    )
                                                                |> CompileFullstackApp.parseElmTypeAndDependenciesRecursivelyFromAnnotation namedModules
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
      , typeAnnotation = CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
      , expectedResult =
            { encodeExpression = "Json.Encode.string valueToEncode"
            , decodeExpression = "Json.Decode.string"
            }
      }
    , { testName = "Unit"
      , typeAnnotation = CompileFullstackApp.UnitType
      , expectedResult =
            { encodeExpression = "Json.Encode.list (always (Json.Encode.object [])) []"
            , decodeExpression = "Json.Decode.succeed ()"
            }
      }
    , { testName = "Empty record"
      , typeAnnotation = CompileFullstackApp.RecordElmType { fields = [] }
      , expectedResult =
            { encodeExpression = "Json.Encode.object []"
            , decodeExpression = "Json.Decode.succeed {}"
            }
      }
    , { testName = "Record with one primitive field"
      , typeAnnotation =
            CompileFullstackApp.RecordElmType
                { fields = [ ( "field_name", CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf ) ] }
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
            CompileFullstackApp.RecordElmType
                { fields =
                    [ ( "field_a", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf )
                    , ( "field_b", CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf )
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
            CompileFullstackApp.RecordElmType
                { fields =
                    [ ( "field_a"
                      , CompileFullstackApp.RecordElmType
                            { fields =
                                [ ( "field_c", CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ) ]
                            }
                      )
                    , ( "field_b", CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf )
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
            CompileFullstackApp.TupleElmType
                [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                , CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
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
            CompileFullstackApp.TupleElmType
                [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                , CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
                , CompileFullstackApp.LeafElmType CompileFullstackApp.BoolLeaf
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
            CompileFullstackApp.InstanceElmType
                { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ListLeaf
                , arguments = [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_List (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_List Json.Decode.int"
            }
      }
    , { testName = "Maybe Int"
      , typeAnnotation =
            CompileFullstackApp.InstanceElmType
                { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.MaybeLeaf
                , arguments = [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_Maybe (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_Maybe Json.Decode.int"
            }
      }
    , { testName = "Result String Int"
      , typeAnnotation =
            CompileFullstackApp.InstanceElmType
                { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ResultLeaf
                , arguments =
                    [ CompileFullstackApp.LeafElmType CompileFullstackApp.StringLeaf
                    , CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                    ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode__generic_Result (\\type_arg -> Json.Encode.string type_arg) (\\type_arg -> Json.Encode.int type_arg) valueToEncode"
            , decodeExpression = "jsonDecode__generic_Result Json.Decode.string Json.Decode.int"
            }
      }
    , { testName = "Instance of generic custom type"
      , typeAnnotation =
            CompileFullstackApp.InstanceElmType
                { instantiated = CompileFullstackApp.CustomElmType "OwnModule.CustomType"
                , arguments =
                    [ CompileFullstackApp.LeafElmType CompileFullstackApp.BoolLeaf
                    ]
                }
      , expectedResult =
            { encodeExpression = "jsonEncode_OwnModule_CustomType (\\type_arg -> Json.Encode.bool type_arg) valueToEncode"
            , decodeExpression = "jsonDecode_OwnModule_CustomType Json.Decode.bool"
            }
      }
    ]
        |> List.map
            (\{ testName, typeAnnotation, expectedResult } ->
                Test.test testName <|
                    \() ->
                        ( typeAnnotation, [] )
                            |> CompileFullstackApp.jsonCodingExpressionFromType
                                { encodeValueExpression = "valueToEncode", typeArgLocalName = "type_arg" }
                            |> Expect.equal expectedResult
            )
        |> Test.describe "emit json coding expressions from type"


emit_json_coding_expression_from_custom_type : Test.Test
emit_json_coding_expression_from_custom_type =
    [ { testName = "One tag without parameters"
      , customTypeName = "ModuleName.CustomType"
      , customType =
            { parameters = []
            , tags = [ ( "TagA", [] ) ] |> Dict.fromList
            }
      , expectedResult =
            { encodeFunction = String.trim """
jsonEncode_ModuleName_CustomType valueToEncode =
    case valueToEncode of
        ModuleName.TagA ->
            Json.Encode.object [ ( "TagA", Json.Encode.list identity [] ) ]
"""
            , decodeFunction = String.trim """
jsonDecode_ModuleName_CustomType =
    Json.Decode.oneOf
        [ Json.Decode.field "TagA" (jsonDecodeSucceedWhenNotNull ModuleName.TagA)
        ]
"""
            }
      }
    , { testName = "Recursive type"
      , customTypeName = "ModuleName.RecursiveType"
      , customType =
            { parameters = []
            , tags =
                [ ( "TagRecurse", [ CompileFullstackApp.CustomElmType "ModuleName.RecursiveType" ] )
                , ( "TagTerminate", [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ] )
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
      , customTypeName = "ModuleName.TypeName"
      , customType =
            { parameters = []
            , tags =
                [ ( "TagList"
                  , [ CompileFullstackApp.InstanceElmType
                        { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ListLeaf
                        , arguments = [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf ]
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
      , customTypeName = "ModuleName.TypeName"
      , customType =
            { parameters = []
            , tags =
                [ ( "TagAlpha"
                  , [ CompileFullstackApp.LeafElmType CompileFullstackApp.IntLeaf
                    , CompileFullstackApp.LeafElmType CompileFullstackApp.BoolLeaf
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
      , customTypeName = "ModuleName.TypeName"
      , customType =
            { parameters = [ "test" ]
            , tags =
                [ ( "TagAlpha"
                  , [ CompileFullstackApp.GenericType "test"
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
      , customTypeName = "ListDict.Dict"
      , customType =
            { parameters = [ "key", "value" ]
            , tags =
                [ ( "Dict"
                  , [ CompileFullstackApp.InstanceElmType
                        { instantiated = CompileFullstackApp.LeafElmType CompileFullstackApp.ListLeaf
                        , arguments =
                            [ CompileFullstackApp.TupleElmType
                                [ CompileFullstackApp.GenericType "key"
                                , CompileFullstackApp.GenericType "value"
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
            (\{ testName, customType, customTypeName, expectedResult } ->
                Test.test testName <|
                    \() ->
                        customType
                            |> CompileFullstackApp.jsonCodingFunctionFromCustomType
                                { customTypeName = customTypeName
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
        |> Test.describe "emit json coding expressions from custom type"
