module Elm.Syntax.Encode.TypeAnnotation exposing (encode, decoder)

{-| This syntax represents the type annotation syntax.
For example:

    Int -> String


## Types

@docs TypeAnnotation, RecordDefinition, RecordField


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.Encode.ModuleName as EncodeModuleName
import Elm.Syntax.Encode.Node as EncodeNode
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)



-- Serialization


{-| Encode a `TypeAnnotation` syntax element to JSON.
-}
encode : TypeAnnotation -> Value
encode typeAnnotation =
    case typeAnnotation of
        GenericType name ->
            encodeTyped "generic" <|
                JE.object
                    [ ( "value", JE.string name )
                    ]

        Typed moduleNameAndName args ->
            let
                inner : ( ModuleName, String ) -> Value
                inner ( mod, n ) =
                    JE.object
                        [ ( "moduleName", EncodeModuleName.encode mod )
                        , ( "name", JE.string n )
                        ]
            in
            encodeTyped "typed" <|
                JE.object
                    [ ( "moduleNameAndName", EncodeNode.encode inner moduleNameAndName )
                    , ( "args", JE.list (EncodeNode.encode encode) args )
                    ]

        Unit ->
            encodeTyped "unit" (JE.object [])

        Tupled t ->
            encodeTyped "tupled" <|
                JE.object
                    [ ( "values", JE.list (EncodeNode.encode encode) t )
                    ]

        FunctionTypeAnnotation left right ->
            encodeTyped "function" <|
                JE.object
                    [ ( "left", EncodeNode.encode encode left )
                    , ( "right", EncodeNode.encode encode right )
                    ]

        Record recordDefinition ->
            encodeTyped "record" <|
                JE.object
                    [ ( "value", encodeRecordDefinition recordDefinition )
                    ]

        GenericRecord name recordDefinition ->
            encodeTyped "genericRecord" <|
                JE.object
                    [ ( "name", EncodeNode.encode JE.string name )
                    , ( "values", EncodeNode.encode encodeRecordDefinition recordDefinition )
                    ]


encodeRecordDefinition : RecordDefinition -> Value
encodeRecordDefinition =
    JE.list (EncodeNode.encode encodeRecordField)


encodeRecordField : RecordField -> Value
encodeRecordField ( name, ref ) =
    JE.object
        [ ( "name", EncodeNode.encode JE.string name )
        , ( "typeAnnotation", EncodeNode.encode encode ref )
        ]


decodeModuleNameAndName : Decoder ( ModuleName, String )
decodeModuleNameAndName =
    JD.map2 Tuple.pair
        (JD.field "moduleName" <| EncodeModuleName.decoder)
        (JD.field "name" <| JD.string)


{-| JSON decoder for a `TypeAnnotation` syntax element.
-}
decoder : Decoder TypeAnnotation
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "generic", JD.map GenericType (JD.field "value" JD.string) )
                , ( "typed"
                  , JD.map2 Typed
                        (JD.field "moduleNameAndName" <| EncodeNode.decoder decodeModuleNameAndName)
                        (JD.field "args" (JD.list nestedDecoder))
                  )
                , ( "unit", JD.succeed Unit )
                , ( "tupled", JD.map Tupled (JD.field "values" (JD.list nestedDecoder)) )
                , ( "function"
                  , JD.map2 FunctionTypeAnnotation
                        (JD.field "left" nestedDecoder)
                        (JD.field "right" nestedDecoder)
                  )
                , ( "record", JD.map Record (JD.field "value" recordDefinitionDecoder) )
                , ( "genericRecord"
                  , JD.map2 GenericRecord
                        (JD.field "name" <| EncodeNode.decoder JD.string)
                        (JD.field "values" <| EncodeNode.decoder recordDefinitionDecoder)
                  )
                ]
        )


nestedDecoder : Decoder (Node TypeAnnotation)
nestedDecoder =
    JD.lazy (\() -> EncodeNode.decoder decoder)


recordDefinitionDecoder : Decoder RecordDefinition
recordDefinitionDecoder =
    JD.lazy (\() -> JD.list <| EncodeNode.decoder recordFieldDecoder)


recordFieldDecoder : Decoder RecordField
recordFieldDecoder =
    JD.lazy
        (\() ->
            JD.map2 Tuple.pair
                (JD.field "name" <| EncodeNode.decoder JD.string)
                (JD.field "typeAnnotation" nestedDecoder)
        )
