module Tests exposing (..)

import Bytes
import Bytes.Encode
import CompileFullstackApp
import Dict
import Expect
import Json.Decode
import Main
import Test


parseElmTypeText : Test.Test
parseElmTypeText =
    Test.describe "parse Elm type text"
        [ Test.test "Simplest instance" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "String"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.InstanceElmType
                                { typeName = "String", parameters = [] }
                            )
                        )
        , Test.test "Instance with one parameter" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "Maybe Int"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.InstanceElmType
                                { typeName = "Maybe"
                                , parameters = [ "Int" ]
                                }
                            )
                        )
        , Test.test "Instance with two parameters" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "Result String Int"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.InstanceElmType
                                { typeName = "Result"
                                , parameters = [ "String", "Int" ]
                                }
                            )
                        )
        , Test.test "Empty record" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "{ }"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok (CompileFullstackApp.RecordElmType { fields = [] }))
        , Test.test "Simple record with one field" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "{ field_a : Int }"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_a"
                                      , typeText = "Int"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "Int", parameters = [] }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Simple record with two fields" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "{ field_a : Int, field_b : String }"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_a"
                                      , typeText = "Int"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "Int", parameters = [] }
                                      }
                                    , { name = "field_b"
                                      , typeText = "String"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "String", parameters = [] }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Simple record with three fields" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True "{ field_a : Int, field_b : String, field_c : Int }"
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_a"
                                      , typeText = "Int"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "Int", parameters = [] }
                                      }
                                    , { name = "field_b"
                                      , typeText = "String"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "String", parameters = [] }
                                      }
                                    , { name = "field_c"
                                      , typeText = "Int"
                                      , parsedType = CompileFullstackApp.InstanceElmType { typeName = "Int", parameters = [] }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Simple custom type with parameterized tags" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
type SimpleCustomType
    = TagA
    | TagB Int
    | TagC String
    | TagD String Int""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.CustomElmType
                                { typeLocalName = "SimpleCustomType"
                                , parameters = []
                                , tags =
                                    [ ( "TagA", [] )
                                    , ( "TagB", [ "Int" ] )
                                    , ( "TagC", [ "String" ] )
                                    , ( "TagD", [ "String", "Int" ] )
                                    ]
                                        |> Dict.fromList
                                }
                            )
                        )
        , Test.test "Empty Tuple" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """(  )""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok (CompileFullstackApp.TupleElmType []))
        , Test.test "Simple Tuple" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """(Int, String)""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok (CompileFullstackApp.TupleElmType [ "Int", "String" ]))
        , Test.test "Record type with tuple" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
    { field_tuple : ( Int, String )
    , field_int : Int
    }""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_tuple"
                                      , typeText = "( Int, String )"
                                      , parsedType =
                                            CompileFullstackApp.TupleElmType [ "Int", "String" ]
                                      }
                                    , { name = "field_int"
                                      , typeText = "Int"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "Int"
                                                , parameters = []
                                                }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Nested record type" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
    { field_record : SimpleRecordType
    , field_custom : SimpleCustomType
    , field_tuple : ( Int, String )
    , field_list : List Int
    }""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_record"
                                      , typeText = "SimpleRecordType"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "SimpleRecordType", parameters = [] }
                                      }
                                    , { name = "field_custom"
                                      , typeText = "SimpleCustomType"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "SimpleCustomType", parameters = [] }
                                      }
                                    , { name = "field_tuple"
                                      , typeText = "( Int, String )"
                                      , parsedType =
                                            CompileFullstackApp.TupleElmType [ "Int", "String" ]
                                      }
                                    , { name = "field_list"
                                      , typeText = "List Int"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "Int" ]
                                                }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Nested record type with inlined record" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
    { field_record : { field_record_a : Int, field_record_b : String }
    , field_list : List Int
    }""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "field_record"
                                      , typeText = "{ field_record_a : Int, field_record_b : String }"
                                      , parsedType =
                                            CompileFullstackApp.RecordElmType
                                                { fields =
                                                    [ { name = "field_record_a"
                                                      , typeText = "Int"
                                                      , parsedType =
                                                            CompileFullstackApp.InstanceElmType
                                                                { typeName = "Int"
                                                                , parameters = []
                                                                }
                                                      }
                                                    , { name = "field_record_b"
                                                      , typeText = "String"
                                                      , parsedType =
                                                            CompileFullstackApp.InstanceElmType
                                                                { typeName = "String"
                                                                , parameters = []
                                                                }
                                                      }
                                                    ]
                                                }
                                      }
                                    , { name = "field_list"
                                      , typeText = "List Int"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "Int" ]
                                                }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Custom type with type parameter" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
type CustomTypeWithTypeParameter a
    = CustomTypeWithTypeParameterTag a
""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.CustomElmType
                                { typeLocalName = "CustomTypeWithTypeParameter"
                                , parameters = [ "a" ]
                                , tags =
                                    [ ( "CustomTypeWithTypeParameterTag", [ "a" ] ) ]
                                        |> Dict.fromList
                                }
                            )
                        )
        , Test.test "Record with instance with tuple" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
    { instance_field : List String
    , changeBlobs : List ( List String, List BlobChangeSequenceElement )
    }
""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "instance_field"
                                      , typeText = "List String"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "String" ]
                                                }
                                      }
                                    , { name = "changeBlobs"
                                      , typeText = "List ( List String, List BlobChangeSequenceElement )"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "( List String, List BlobChangeSequenceElement )" ]
                                                }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "Record ProjectStateDifference from Elm Editor" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
    { removeNodes : List (List String)
    , changeBlobs : List ( List String, List BlobChangeSequenceElement )
    }
""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.RecordElmType
                                { fields =
                                    [ { name = "removeNodes"
                                      , typeText = "List (List String)"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "(List String)" ]
                                                }
                                      }
                                    , { name = "changeBlobs"
                                      , typeText = "List ( List String, List BlobChangeSequenceElement )"
                                      , parsedType =
                                            CompileFullstackApp.InstanceElmType
                                                { typeName = "List"
                                                , parameters = [ "( List String, List BlobChangeSequenceElement )" ]
                                                }
                                      }
                                    ]
                                }
                            )
                        )
        , Test.test "ListDict.Dict" <|
            \() ->
                CompileFullstackApp.parseElmTypeText True (String.trim """
type Dict key value
    = Dict (List ( key, value ))

""")
                    |> Result.map Tuple.first
                    |> Expect.equal
                        (Ok
                            (CompileFullstackApp.CustomElmType
                                { typeLocalName = "Dict"
                                , parameters = [ "key", "value" ]
                                , tags =
                                    [ ( "Dict", [ "(List ( key, value ))" ] )
                                    ]
                                        |> Dict.fromList
                                }
                            )
                        )
        ]


parseCompilationInterfaceElmMakeFunctionName : Test.Test
parseCompilationInterfaceElmMakeFunctionName =
    [ ( "elm_make__debug__javascript__base64____src_FrontendWeb_Main_elm"
      , Ok
            { filePathRepresentation = "src_FrontendWeb_Main_elm"
            , outputType = CompileFullstackApp.ElmMakeOutputTypeJs
            , enableDebug = True
            , base64 = True
            }
      )
    , ( "elm_make__javascript__base64____src_FrontendWeb_Main_elm"
      , Ok
            { filePathRepresentation = "src_FrontendWeb_Main_elm"
            , outputType = CompileFullstackApp.ElmMakeOutputTypeJs
            , enableDebug = False
            , base64 = True
            }
      )
    , ( "elm_make__debug__base64____src_FrontendWeb_Main_elm"
      , Ok
            { filePathRepresentation = "src_FrontendWeb_Main_elm"
            , outputType = CompileFullstackApp.ElmMakeOutputTypeHtml
            , enableDebug = True
            , base64 = True
            }
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
