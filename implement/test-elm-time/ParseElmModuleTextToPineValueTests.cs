using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ParseElmModuleTextToPineValueTests
{
    [TestMethod]
    public void Parse_Elm_module_with_varied_syntax()
    {
        var elmModuleText =
            """
            module Namespace.Beta exposing (..)

            import Delta exposing (..)
            import Dict
            import Elm.Dependency exposing (Dependency(..), Version, epsilon)
            import Gamma.Interface as Interface


            type alias MaybeInt =
                Maybe Int


            type alias RecordType =
                { alfa : Int }


            type ChoiceType
                = Choice_Alfa
                | Choice_Beta Int


            greet : String -> String
            greet param_name =
                "Hello, " ++ param_name ++ " 👋"


            type String
                = String (List Char.Char)
                  -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                | AnyOtherKind


            {-| Replicates the given appendable.
            -}
            replicate : appendable -> appendable
            replicate a =
                case a of
                    String stringA ->
                        String (Pine_kernel.concat [ stringA, stringA ])

                    _ ->
                        Pine_kernel.concat [ a, a ]


            toIntFromList : List Char -> Maybe Int
            toIntFromList stringAsList =
                case stringAsList of
                    [] ->
                        Nothing

                    firstChar :: lessFirstChar ->
                        let
                            ( valueString, signMultiplier ) =
                                case firstChar of
                                    '-' ->
                                        ( lessFirstChar, -1 )

                                    '+' ->
                                        ( lessFirstChar, 1 )

                                    _ ->
                                        ( stringAsList, 1 )
                        in
                        if valueString == [] then
                            Nothing

                        else
                            case toUnsignedIntFromList 0 valueString of
                                Just unsigned ->
                                    Just (Pine_kernel.int_mul [ signMultiplier, unsigned ])

                                Nothing ->
                                    Nothing


            moveRedLeft : Dict k v -> Dict k v
            moveRedLeft dict =
                case dict of
                    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV ((RBNode_elm_builtin Red rlK rlV rlL rlR) as rLeft) rRight) ->
                        RBNode_elm_builtin
                            Red
                            rlK
                            rlV
                            (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
                            (RBNode_elm_builtin Black rK rV rlR rRight)

                    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
                        case clr of
                            Black ->
                                RBNode_elm_builtin
                                    Black
                                    k
                                    v
                                    (RBNode_elm_builtin Red lK lV lLeft lRight)
                                    (RBNode_elm_builtin Red rK rV rLeft rRight)

                            Red ->
                                RBNode_elm_builtin
                                    Black
                                    k
                                    v
                                    (RBNode_elm_builtin Red lK lV lLeft lRight)
                                    (RBNode_elm_builtin Red rK rV rLeft rRight)

                    _ ->
                        dict


            map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
            map3 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) =
                Decoder
                    (\bites offset ->
                        let
                            ( offsetA, a ) =
                                decodeA bites offset

                            ( offsetB, b ) =
                                decodeB bites offsetA

                            ( offsetC, c ) =
                                decodeC bites offsetB
                        in
                        ( offsetC, func a b c )
                    )


            type Boolean
                = MyTrue
                | MyFalse
                | MyOr Boolean Boolean


            booleanParser : Parser.Parser Boolean
            booleanParser =
                Parser.oneOf
                    [ Parser.succeed MyTrue
                        |. Parser.keyword "true"
                    , Parser.succeed MyFalse
                        |. Parser.keyword "false"
                    , Parser.succeed MyOr
                        |. Parser.symbol "("
                        |. Parser.spaces
                        |= Parser.lazy (\_ -> boolean)
                        |. Parser.spaces
                        |. Parser.symbol "||"
                        |. Parser.spaces
                        |= Parser.lazy (\_ -> boolean)
                        |. Parser.spaces
                        |. Parser.symbol ")"
                    ]


            mixedOperators_Alfa : Int -> Int -> Int -> Int -> Int
            mixedOperators_Alfa a b c d =
                a + b * c - d


            mixedOperators_Beta : Int -> Int -> Int -> Int -> Int
            mixedOperators_Beta a b c d =
                a + b // c + d
            
                        
            recordAccess : RecordType -> Int
            recordAccess record =
                record.alfa


            recordAccessFunction : RecordType -> Int
            recordAccessFunction record =
                .beta record


            recordUpdate : RecordType -> Int -> RecordType
            recordUpdate record newValue =
                { record | gamma = newValue }
            
            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = [Node { end = { column = 119, row = 29 }, start = { column = 7, row = 29 } } \"-- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.\"]",
            ", declarations = [",
            "Node { end = { column = 14, row = 10 }, start = { column = 1, row = 9 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 9 }, start = { column = 12, row = 9 } } \"MaybeInt\", typeAnnotation = Node { end = { column = 14, row = 10 }, start = { column = 5, row = 10 } } (Typed (Node { end = { column = 10, row = 10 }, start = { column = 5, row = 10 } } ([],\"Maybe\")) [Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } (Typed (Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } ([],\"Int\")) [])]) })",
            ",Node { end = { column = 19, row = 14 }, start = { column = 1, row = 13 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 13 }, start = { column = 12, row = 13 } } \"RecordType\", typeAnnotation = Node { end = { column = 19, row = 14 }, start = { column = 5, row = 14 } } (Record [Node { end = { column = 17, row = 14 }, start = { column = 7, row = 14 } } [Node { end = { column = 11, row = 14 }, start = { column = 7, row = 14 } } \"alfa\",Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } (Typed (Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } ([],\"Int\")) [])]]) })",
            ",Node { end = { column = 22, row = 19 }, start = { column = 1, row = 17 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } { arguments = [], name = Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } \"Choice_Alfa\" },Node { end = { column = 22, row = 19 }, start = { column = 7, row = 19 } } { arguments = [Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } ([],\"Int\")) [])], name = Node { end = { column = 18, row = 19 }, start = { column = 7, row = 19 } } \"Choice_Beta\" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 17 }, start = { column = 6, row = 17 } } \"ChoiceType\" })",
            ",Node { end = { column = 36, row = 24 }, start = { column = 1, row = 22 } } (FunctionDeclaration { declaration = Node { end = { column = 36, row = 24 }, start = { column = 1, row = 23 } } { arguments = [Node { end = { column = 17, row = 23 }, start = { column = 7, row = 23 } } (VarPattern \"param_name\")], expression = Node { end = { column = 36, row = 24 }, start = { column = 5, row = 24 } } (OperatorApplication \"++\" Right (Node { end = { column = 14, row = 24 }, start = { column = 5, row = 24 } } (Literal \"Hello, \")) (Node { end = { column = 36, row = 24 }, start = { column = 18, row = 24 } } (OperatorApplication \"++\" Right (Node { end = { column = 28, row = 24 }, start = { column = 18, row = 24 } } (FunctionOrValue [] \"param_name\")) (Node { end = { column = 36, row = 24 }, start = { column = 32, row = 24 } } (Literal \" 👋\"))))), name = Node { end = { column = 6, row = 23 }, start = { column = 1, row = 23 } } \"greet\" }, documentation = Nothing, signature = Just (Node { end = { column = 25, row = 22 }, start = { column = 1, row = 22 } } { name = Node { end = { column = 6, row = 22 }, start = { column = 1, row = 22 } } \"greet\", typeAnnotation = Node { end = { column = 25, row = 22 }, start = { column = 9, row = 22 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } (Typed (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } ([],\"String\")) [])) (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } (Typed (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } ([],\"String\")) []))) }) })",
            ",Node { end = { column = 19, row = 30 }, start = { column = 1, row = 27 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 30, row = 28 }, start = { column = 7, row = 28 } } { arguments = [Node { end = { column = 30, row = 28 }, start = { column = 14, row = 28 } } (Typed (Node { end = { column = 19, row = 28 }, start = { column = 15, row = 28 } } ([],\"List\")) [Node { end = { column = 29, row = 28 }, start = { column = 20, row = 28 } } (Typed (Node { end = { column = 29, row = 28 }, start = { column = 20, row = 28 } } ([\"Char\"],\"Char\")) [])])], name = Node { end = { column = 13, row = 28 }, start = { column = 7, row = 28 } } \"String\" },Node { end = { column = 19, row = 30 }, start = { column = 7, row = 30 } } { arguments = [], name = Node { end = { column = 19, row = 30 }, start = { column = 7, row = 30 } } \"AnyOtherKind\" }], documentation = Nothing, generics = [], name = Node { end = { column = 12, row = 27 }, start = { column = 6, row = 27 } } \"String\" })",
            ",Node { end = { column = 40, row = 42 }, start = { column = 1, row = 33 } } (FunctionDeclaration { declaration = Node { end = { column = 40, row = 42 }, start = { column = 1, row = 36 } } { arguments = [Node { end = { column = 12, row = 36 }, start = { column = 11, row = 36 } } (VarPattern \"a\")], expression = Node { end = { column = 40, row = 42 }, start = { column = 5, row = 37 } } (CaseExpression { cases = [[Node { end = { column = 23, row = 38 }, start = { column = 9, row = 38 } } (NamedPattern { moduleName = [], name = \"String\" } [Node { end = { column = 23, row = 38 }, start = { column = 16, row = 38 } } (VarPattern \"stringA\")]),Node { end = { column = 61, row = 39 }, start = { column = 13, row = 39 } } (Application [Node { end = { column = 19, row = 39 }, start = { column = 13, row = 39 } } (FunctionOrValue [] \"String\"),Node { end = { column = 61, row = 39 }, start = { column = 20, row = 39 } } (ParenthesizedExpression (Node { end = { column = 60, row = 39 }, start = { column = 21, row = 39 } } (Application [Node { end = { column = 39, row = 39 }, start = { column = 21, row = 39 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 60, row = 39 }, start = { column = 40, row = 39 } } (ListExpr [Node { end = { column = 49, row = 39 }, start = { column = 42, row = 39 } } (FunctionOrValue [] \"stringA\"),Node { end = { column = 58, row = 39 }, start = { column = 51, row = 39 } } (FunctionOrValue [] \"stringA\")])])))])],[Node { end = { column = 10, row = 41 }, start = { column = 9, row = 41 } } AllPattern,Node { end = { column = 40, row = 42 }, start = { column = 13, row = 42 } } (Application [Node { end = { column = 31, row = 42 }, start = { column = 13, row = 42 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 40, row = 42 }, start = { column = 32, row = 42 } } (ListExpr [Node { end = { column = 35, row = 42 }, start = { column = 34, row = 42 } } (FunctionOrValue [] \"a\"),Node { end = { column = 38, row = 42 }, start = { column = 37, row = 42 } } (FunctionOrValue [] \"a\")])])]], expression = Node { end = { column = 11, row = 37 }, start = { column = 10, row = 37 } } (FunctionOrValue [] \"a\") }), name = Node { end = { column = 10, row = 36 }, start = { column = 1, row = 36 } } \"replicate\" }, documentation = Just (Node { end = { column = 3, row = 34 }, start = { column = 1, row = 33 } } \"{-| Replicates the given appendable.\n-}\"), signature = Just (Node { end = { column = 37, row = 35 }, start = { column = 1, row = 35 } } { name = Node { end = { column = 10, row = 35 }, start = { column = 1, row = 35 } } \"replicate\", typeAnnotation = Node { end = { column = 37, row = 35 }, start = { column = 13, row = 35 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 35 }, start = { column = 13, row = 35 } } (GenericType \"appendable\")) (Node { end = { column = 37, row = 35 }, start = { column = 27, row = 35 } } (GenericType \"appendable\"))) }) })",
            ",Node { end = { column = 32, row = 73 }, start = { column = 1, row = 45 } } (FunctionDeclaration { declaration = Node { end = { column = 32, row = 73 }, start = { column = 1, row = 46 } } { arguments = [Node { end = { column = 27, row = 46 }, start = { column = 15, row = 46 } } (VarPattern \"stringAsList\")], expression = Node { end = { column = 32, row = 73 }, start = { column = 5, row = 47 } } (CaseExpression { cases = [[Node { end = { column = 11, row = 48 }, start = { column = 9, row = 48 } } (ListPattern []),Node { end = { column = 20, row = 49 }, start = { column = 13, row = 49 } } (FunctionOrValue [] \"Nothing\")],[Node { end = { column = 35, row = 51 }, start = { column = 9, row = 51 } } (UnConsPattern (Node { end = { column = 18, row = 51 }, start = { column = 9, row = 51 } } (VarPattern \"firstChar\")) (Node { end = { column = 35, row = 51 }, start = { column = 22, row = 51 } } (VarPattern \"lessFirstChar\"))),Node { end = { column = 32, row = 73 }, start = { column = 13, row = 52 } } (LetExpression { declarations = [Node { end = { column = 48, row = 62 }, start = { column = 17, row = 53 } } (LetDestructuring (Node { end = { column = 48, row = 53 }, start = { column = 17, row = 53 } } (TuplePattern [Node { end = { column = 30, row = 53 }, start = { column = 19, row = 53 } } (VarPattern \"valueString\"),Node { end = { column = 46, row = 53 }, start = { column = 32, row = 53 } } (VarPattern \"signMultiplier\")])) (Node { end = { column = 48, row = 62 }, start = { column = 21, row = 54 } } (CaseExpression { cases = [[Node { end = { column = 28, row = 55 }, start = { column = 25, row = 55 } } (CharPattern '-'),Node { end = { column = 50, row = 56 }, start = { column = 29, row = 56 } } (TupledExpression [Node { end = { column = 44, row = 56 }, start = { column = 31, row = 56 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 48, row = 56 }, start = { column = 46, row = 56 } } (Negation (Node { end = { column = 48, row = 56 }, start = { column = 47, row = 56 } } (Integer 1)))])],[Node { end = { column = 28, row = 58 }, start = { column = 25, row = 58 } } (CharPattern '+'),Node { end = { column = 49, row = 59 }, start = { column = 29, row = 59 } } (TupledExpression [Node { end = { column = 44, row = 59 }, start = { column = 31, row = 59 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 47, row = 59 }, start = { column = 46, row = 59 } } (Integer 1)])],[Node { end = { column = 26, row = 61 }, start = { column = 25, row = 61 } } AllPattern,Node { end = { column = 48, row = 62 }, start = { column = 29, row = 62 } } (TupledExpression [Node { end = { column = 43, row = 62 }, start = { column = 31, row = 62 } } (FunctionOrValue [] \"stringAsList\"),Node { end = { column = 46, row = 62 }, start = { column = 45, row = 62 } } (Integer 1)])]], expression = Node { end = { column = 35, row = 54 }, start = { column = 26, row = 54 } } (FunctionOrValue [] \"firstChar\") })))], expression = Node { end = { column = 32, row = 73 }, start = { column = 13, row = 64 } } (IfBlock (Node { end = { column = 33, row = 64 }, start = { column = 16, row = 64 } } (OperatorApplication \"==\" Non (Node { end = { column = 27, row = 64 }, start = { column = 16, row = 64 } } (FunctionOrValue [] \"valueString\")) (Node { end = { column = 33, row = 64 }, start = { column = 31, row = 64 } } (ListExpr [])))) (Node { end = { column = 24, row = 65 }, start = { column = 17, row = 65 } } (FunctionOrValue [] \"Nothing\")) (Node { end = { column = 32, row = 73 }, start = { column = 17, row = 68 } } (CaseExpression { cases = [[Node { end = { column = 34, row = 69 }, start = { column = 21, row = 69 } } (NamedPattern { moduleName = [], name = \"Just\" } [Node { end = { column = 34, row = 69 }, start = { column = 26, row = 69 } } (VarPattern \"unsigned\")]),Node { end = { column = 80, row = 70 }, start = { column = 25, row = 70 } } (Application [Node { end = { column = 29, row = 70 }, start = { column = 25, row = 70 } } (FunctionOrValue [] \"Just\"),Node { end = { column = 80, row = 70 }, start = { column = 30, row = 70 } } (ParenthesizedExpression (Node { end = { column = 79, row = 70 }, start = { column = 31, row = 70 } } (Application [Node { end = { column = 50, row = 70 }, start = { column = 31, row = 70 } } (FunctionOrValue [\"Pine_kernel\"] \"int_mul\"),Node { end = { column = 79, row = 70 }, start = { column = 51, row = 70 } } (ListExpr [Node { end = { column = 67, row = 70 }, start = { column = 53, row = 70 } } (FunctionOrValue [] \"signMultiplier\"),Node { end = { column = 77, row = 70 }, start = { column = 69, row = 70 } } (FunctionOrValue [] \"unsigned\")])])))])],[Node { end = { column = 28, row = 72 }, start = { column = 21, row = 72 } } (NamedPattern { moduleName = [], name = \"Nothing\" } []),Node { end = { column = 32, row = 73 }, start = { column = 25, row = 73 } } (FunctionOrValue [] \"Nothing\")]], expression = Node { end = { column = 57, row = 68 }, start = { column = 22, row = 68 } } (Application [Node { end = { column = 43, row = 68 }, start = { column = 22, row = 68 } } (FunctionOrValue [] \"toUnsignedIntFromList\"),Node { end = { column = 45, row = 68 }, start = { column = 44, row = 68 } } (Integer 0),Node { end = { column = 57, row = 68 }, start = { column = 46, row = 68 } } (FunctionOrValue [] \"valueString\")]) }))) })]], expression = Node { end = { column = 22, row = 47 }, start = { column = 10, row = 47 } } (FunctionOrValue [] \"stringAsList\") }), name = Node { end = { column = 14, row = 46 }, start = { column = 1, row = 46 } } \"toIntFromList\" }, documentation = Nothing, signature = Just (Node { end = { column = 39, row = 45 }, start = { column = 1, row = 45 } } { name = Node { end = { column = 14, row = 45 }, start = { column = 1, row = 45 } } \"toIntFromList\", typeAnnotation = Node { end = { column = 39, row = 45 }, start = { column = 17, row = 45 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 45 }, start = { column = 17, row = 45 } } (Typed (Node { end = { column = 21, row = 45 }, start = { column = 17, row = 45 } } ([],\"List\")) [Node { end = { column = 26, row = 45 }, start = { column = 22, row = 45 } } (Typed (Node { end = { column = 26, row = 45 }, start = { column = 22, row = 45 } } ([],\"Char\")) [])])) (Node { end = { column = 39, row = 45 }, start = { column = 30, row = 45 } } (Typed (Node { end = { column = 35, row = 45 }, start = { column = 30, row = 45 } } ([],\"Maybe\")) [Node { end = { column = 39, row = 45 }, start = { column = 36, row = 45 } } (Typed (Node { end = { column = 39, row = 45 }, start = { column = 36, row = 45 } } ([],\"Int\")) [])]))) }) })",
            ",Node { end = { column = 17, row = 106 }, start = { column = 1, row = 76 } } (FunctionDeclaration { declaration = Node { end = { column = 17, row = 106 }, start = { column = 1, row = 77 } } { arguments = [Node { end = { column = 17, row = 77 }, start = { column = 13, row = 77 } } (VarPattern \"dict\")], expression = Node { end = { column = 17, row = 106 }, start = { column = 5, row = 78 } } (CaseExpression { cases = [[Node { end = { column = 171, row = 79 }, start = { column = 9, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 31, row = 79 }, start = { column = 28, row = 79 } } (VarPattern \"clr\"),Node { end = { column = 33, row = 79 }, start = { column = 32, row = 79 } } (VarPattern \"k\"),Node { end = { column = 35, row = 79 }, start = { column = 34, row = 79 } } (VarPattern \"v\"),Node { end = { column = 80, row = 79 }, start = { column = 36, row = 79 } } (ParenthesizedPattern (Node { end = { column = 79, row = 79 }, start = { column = 37, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 60, row = 79 }, start = { column = 56, row = 79 } } (VarPattern \"lClr\"),Node { end = { column = 63, row = 79 }, start = { column = 61, row = 79 } } (VarPattern \"lK\"),Node { end = { column = 66, row = 79 }, start = { column = 64, row = 79 } } (VarPattern \"lV\"),Node { end = { column = 72, row = 79 }, start = { column = 67, row = 79 } } (VarPattern \"lLeft\"),Node { end = { column = 79, row = 79 }, start = { column = 73, row = 79 } } (VarPattern \"lRight\")]))),Node { end = { column = 171, row = 79 }, start = { column = 81, row = 79 } } (ParenthesizedPattern (Node { end = { column = 170, row = 79 }, start = { column = 82, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 105, row = 79 }, start = { column = 101, row = 79 } } (VarPattern \"rClr\"),Node { end = { column = 108, row = 79 }, start = { column = 106, row = 79 } } (VarPattern \"rK\"),Node { end = { column = 111, row = 79 }, start = { column = 109, row = 79 } } (VarPattern \"rV\"),Node { end = { column = 163, row = 79 }, start = { column = 112, row = 79 } } (ParenthesizedPattern (Node { end = { column = 162, row = 79 }, start = { column = 113, row = 79 } } (AsPattern (Node { end = { column = 153, row = 79 }, start = { column = 113, row = 79 } } (ParenthesizedPattern (Node { end = { column = 152, row = 79 }, start = { column = 114, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 136, row = 79 }, start = { column = 133, row = 79 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 140, row = 79 }, start = { column = 137, row = 79 } } (VarPattern \"rlK\"),Node { end = { column = 144, row = 79 }, start = { column = 141, row = 79 } } (VarPattern \"rlV\"),Node { end = { column = 148, row = 79 }, start = { column = 145, row = 79 } } (VarPattern \"rlL\"),Node { end = { column = 152, row = 79 }, start = { column = 149, row = 79 } } (VarPattern \"rlR\")])))) (Node { end = { column = 162, row = 79 }, start = { column = 157, row = 79 } } \"rLeft\")))),Node { end = { column = 170, row = 79 }, start = { column = 164, row = 79 } } (VarPattern \"rRight\")])))]),Node { end = { column = 60, row = 85 }, start = { column = 13, row = 80 } } (Application [Node { end = { column = 31, row = 80 }, start = { column = 13, row = 80 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 20, row = 81 }, start = { column = 17, row = 81 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 20, row = 82 }, start = { column = 17, row = 82 } } (FunctionOrValue [] \"rlK\"),Node { end = { column = 20, row = 83 }, start = { column = 17, row = 83 } } (FunctionOrValue [] \"rlV\"),Node { end = { column = 95, row = 84 }, start = { column = 17, row = 84 } } (ParenthesizedExpression (Node { end = { column = 94, row = 84 }, start = { column = 18, row = 84 } } (Application [Node { end = { column = 36, row = 84 }, start = { column = 18, row = 84 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 42, row = 84 }, start = { column = 37, row = 84 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 44, row = 84 }, start = { column = 43, row = 84 } } (FunctionOrValue [] \"k\"),Node { end = { column = 46, row = 84 }, start = { column = 45, row = 84 } } (FunctionOrValue [] \"v\"),Node { end = { column = 90, row = 84 }, start = { column = 47, row = 84 } } (ParenthesizedExpression (Node { end = { column = 89, row = 84 }, start = { column = 48, row = 84 } } (Application [Node { end = { column = 66, row = 84 }, start = { column = 48, row = 84 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 70, row = 84 }, start = { column = 67, row = 84 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 73, row = 84 }, start = { column = 71, row = 84 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 76, row = 84 }, start = { column = 74, row = 84 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 82, row = 84 }, start = { column = 77, row = 84 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 89, row = 84 }, start = { column = 83, row = 84 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 94, row = 84 }, start = { column = 91, row = 84 } } (FunctionOrValue [] \"rlL\")]))),Node { end = { column = 60, row = 85 }, start = { column = 17, row = 85 } } (ParenthesizedExpression (Node { end = { column = 59, row = 85 }, start = { column = 18, row = 85 } } (Application [Node { end = { column = 36, row = 85 }, start = { column = 18, row = 85 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 42, row = 85 }, start = { column = 37, row = 85 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 45, row = 85 }, start = { column = 43, row = 85 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 48, row = 85 }, start = { column = 46, row = 85 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 52, row = 85 }, start = { column = 49, row = 85 } } (FunctionOrValue [] \"rlR\"),Node { end = { column = 59, row = 85 }, start = { column = 53, row = 85 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 125, row = 87 }, start = { column = 9, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 31, row = 87 }, start = { column = 28, row = 87 } } (VarPattern \"clr\"),Node { end = { column = 33, row = 87 }, start = { column = 32, row = 87 } } (VarPattern \"k\"),Node { end = { column = 35, row = 87 }, start = { column = 34, row = 87 } } (VarPattern \"v\"),Node { end = { column = 80, row = 87 }, start = { column = 36, row = 87 } } (ParenthesizedPattern (Node { end = { column = 79, row = 87 }, start = { column = 37, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 60, row = 87 }, start = { column = 56, row = 87 } } (VarPattern \"lClr\"),Node { end = { column = 63, row = 87 }, start = { column = 61, row = 87 } } (VarPattern \"lK\"),Node { end = { column = 66, row = 87 }, start = { column = 64, row = 87 } } (VarPattern \"lV\"),Node { end = { column = 72, row = 87 }, start = { column = 67, row = 87 } } (VarPattern \"lLeft\"),Node { end = { column = 79, row = 87 }, start = { column = 73, row = 87 } } (VarPattern \"lRight\")]))),Node { end = { column = 125, row = 87 }, start = { column = 81, row = 87 } } (ParenthesizedPattern (Node { end = { column = 124, row = 87 }, start = { column = 82, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 105, row = 87 }, start = { column = 101, row = 87 } } (VarPattern \"rClr\"),Node { end = { column = 108, row = 87 }, start = { column = 106, row = 87 } } (VarPattern \"rK\"),Node { end = { column = 111, row = 87 }, start = { column = 109, row = 87 } } (VarPattern \"rV\"),Node { end = { column = 117, row = 87 }, start = { column = 112, row = 87 } } (VarPattern \"rLeft\"),Node { end = { column = 124, row = 87 }, start = { column = 118, row = 87 } } (VarPattern \"rRight\")])))]),Node { end = { column = 68, row = 103 }, start = { column = 13, row = 88 } } (CaseExpression { cases = [[Node { end = { column = 22, row = 89 }, start = { column = 17, row = 89 } } (NamedPattern { moduleName = [], name = \"Black\" } []),Node { end = { column = 68, row = 95 }, start = { column = 21, row = 90 } } (Application [Node { end = { column = 39, row = 90 }, start = { column = 21, row = 90 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 30, row = 91 }, start = { column = 25, row = 91 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 26, row = 92 }, start = { column = 25, row = 92 } } (FunctionOrValue [] \"k\"),Node { end = { column = 26, row = 93 }, start = { column = 25, row = 93 } } (FunctionOrValue [] \"v\"),Node { end = { column = 68, row = 94 }, start = { column = 25, row = 94 } } (ParenthesizedExpression (Node { end = { column = 67, row = 94 }, start = { column = 26, row = 94 } } (Application [Node { end = { column = 44, row = 94 }, start = { column = 26, row = 94 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 94 }, start = { column = 45, row = 94 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 94 }, start = { column = 49, row = 94 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 54, row = 94 }, start = { column = 52, row = 94 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 60, row = 94 }, start = { column = 55, row = 94 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 67, row = 94 }, start = { column = 61, row = 94 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 68, row = 95 }, start = { column = 25, row = 95 } } (ParenthesizedExpression (Node { end = { column = 67, row = 95 }, start = { column = 26, row = 95 } } (Application [Node { end = { column = 44, row = 95 }, start = { column = 26, row = 95 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 95 }, start = { column = 45, row = 95 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 95 }, start = { column = 49, row = 95 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 54, row = 95 }, start = { column = 52, row = 95 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 60, row = 95 }, start = { column = 55, row = 95 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 67, row = 95 }, start = { column = 61, row = 95 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 20, row = 97 }, start = { column = 17, row = 97 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 68, row = 103 }, start = { column = 21, row = 98 } } (Application [Node { end = { column = 39, row = 98 }, start = { column = 21, row = 98 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 30, row = 99 }, start = { column = 25, row = 99 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 26, row = 100 }, start = { column = 25, row = 100 } } (FunctionOrValue [] \"k\"),Node { end = { column = 26, row = 101 }, start = { column = 25, row = 101 } } (FunctionOrValue [] \"v\"),Node { end = { column = 68, row = 102 }, start = { column = 25, row = 102 } } (ParenthesizedExpression (Node { end = { column = 67, row = 102 }, start = { column = 26, row = 102 } } (Application [Node { end = { column = 44, row = 102 }, start = { column = 26, row = 102 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 102 }, start = { column = 45, row = 102 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 102 }, start = { column = 49, row = 102 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 54, row = 102 }, start = { column = 52, row = 102 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 60, row = 102 }, start = { column = 55, row = 102 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 67, row = 102 }, start = { column = 61, row = 102 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 68, row = 103 }, start = { column = 25, row = 103 } } (ParenthesizedExpression (Node { end = { column = 67, row = 103 }, start = { column = 26, row = 103 } } (Application [Node { end = { column = 44, row = 103 }, start = { column = 26, row = 103 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 103 }, start = { column = 45, row = 103 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 103 }, start = { column = 49, row = 103 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 54, row = 103 }, start = { column = 52, row = 103 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 60, row = 103 }, start = { column = 55, row = 103 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 67, row = 103 }, start = { column = 61, row = 103 } } (FunctionOrValue [] \"rRight\")])))])]], expression = Node { end = { column = 21, row = 88 }, start = { column = 18, row = 88 } } (FunctionOrValue [] \"clr\") })],[Node { end = { column = 10, row = 105 }, start = { column = 9, row = 105 } } AllPattern,Node { end = { column = 17, row = 106 }, start = { column = 13, row = 106 } } (FunctionOrValue [] \"dict\")]], expression = Node { end = { column = 14, row = 78 }, start = { column = 10, row = 78 } } (FunctionOrValue [] \"dict\") }), name = Node { end = { column = 12, row = 77 }, start = { column = 1, row = 77 } } \"moveRedLeft\" }, documentation = Nothing, signature = Just (Node { end = { column = 35, row = 76 }, start = { column = 1, row = 76 } } { name = Node { end = { column = 12, row = 76 }, start = { column = 1, row = 76 } } \"moveRedLeft\", typeAnnotation = Node { end = { column = 35, row = 76 }, start = { column = 15, row = 76 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 76 }, start = { column = 15, row = 76 } } (Typed (Node { end = { column = 19, row = 76 }, start = { column = 15, row = 76 } } ([],\"Dict\")) [Node { end = { column = 21, row = 76 }, start = { column = 20, row = 76 } } (GenericType \"k\"),Node { end = { column = 23, row = 76 }, start = { column = 22, row = 76 } } (GenericType \"v\")])) (Node { end = { column = 35, row = 76 }, start = { column = 27, row = 76 } } (Typed (Node { end = { column = 31, row = 76 }, start = { column = 27, row = 76 } } ([],\"Dict\")) [Node { end = { column = 33, row = 76 }, start = { column = 32, row = 76 } } (GenericType \"k\"),Node { end = { column = 35, row = 76 }, start = { column = 34, row = 76 } } (GenericType \"v\")]))) }) })",
            ",Node { end = { column = 10, row = 124 }, start = { column = 1, row = 109 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 124 }, start = { column = 1, row = 110 } } { arguments = [Node { end = { column = 10, row = 110 }, start = { column = 6, row = 110 } } (VarPattern \"func\"),Node { end = { column = 28, row = 110 }, start = { column = 11, row = 110 } } (ParenthesizedPattern (Node { end = { column = 27, row = 110 }, start = { column = 12, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 27, row = 110 }, start = { column = 20, row = 110 } } (VarPattern \"decodeA\")]))),Node { end = { column = 46, row = 110 }, start = { column = 29, row = 110 } } (ParenthesizedPattern (Node { end = { column = 45, row = 110 }, start = { column = 30, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 45, row = 110 }, start = { column = 38, row = 110 } } (VarPattern \"decodeB\")]))),Node { end = { column = 64, row = 110 }, start = { column = 47, row = 110 } } (ParenthesizedPattern (Node { end = { column = 63, row = 110 }, start = { column = 48, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 63, row = 110 }, start = { column = 56, row = 110 } } (VarPattern \"decodeC\")])))], expression = Node { end = { column = 10, row = 124 }, start = { column = 5, row = 111 } } (Application [Node { end = { column = 12, row = 111 }, start = { column = 5, row = 111 } } (FunctionOrValue [] \"Decoder\"),Node { end = { column = 10, row = 124 }, start = { column = 9, row = 112 } } (ParenthesizedExpression (Node { end = { column = 36, row = 123 }, start = { column = 10, row = 112 } } (LambdaExpression { args = [Node { end = { column = 16, row = 112 }, start = { column = 11, row = 112 } } (VarPattern \"bites\"),Node { end = { column = 23, row = 112 }, start = { column = 17, row = 112 } } (VarPattern \"offset\")], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 113 } } (LetExpression { declarations = [Node { end = { column = 41, row = 115 }, start = { column = 17, row = 114 } } (LetDestructuring (Node { end = { column = 31, row = 114 }, start = { column = 17, row = 114 } } (TuplePattern [Node { end = { column = 26, row = 114 }, start = { column = 19, row = 114 } } (VarPattern \"offsetA\"),Node { end = { column = 29, row = 114 }, start = { column = 28, row = 114 } } (VarPattern \"a\")])) (Node { end = { column = 41, row = 115 }, start = { column = 21, row = 115 } } (Application [Node { end = { column = 28, row = 115 }, start = { column = 21, row = 115 } } (FunctionOrValue [] \"decodeA\"),Node { end = { column = 34, row = 115 }, start = { column = 29, row = 115 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 41, row = 115 }, start = { column = 35, row = 115 } } (FunctionOrValue [] \"offset\")]))),Node { end = { column = 42, row = 118 }, start = { column = 17, row = 117 } } (LetDestructuring (Node { end = { column = 31, row = 117 }, start = { column = 17, row = 117 } } (TuplePattern [Node { end = { column = 26, row = 117 }, start = { column = 19, row = 117 } } (VarPattern \"offsetB\"),Node { end = { column = 29, row = 117 }, start = { column = 28, row = 117 } } (VarPattern \"b\")])) (Node { end = { column = 42, row = 118 }, start = { column = 21, row = 118 } } (Application [Node { end = { column = 28, row = 118 }, start = { column = 21, row = 118 } } (FunctionOrValue [] \"decodeB\"),Node { end = { column = 34, row = 118 }, start = { column = 29, row = 118 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 118 }, start = { column = 35, row = 118 } } (FunctionOrValue [] \"offsetA\")]))),Node { end = { column = 42, row = 121 }, start = { column = 17, row = 120 } } (LetDestructuring (Node { end = { column = 31, row = 120 }, start = { column = 17, row = 120 } } (TuplePattern [Node { end = { column = 26, row = 120 }, start = { column = 19, row = 120 } } (VarPattern \"offsetC\"),Node { end = { column = 29, row = 120 }, start = { column = 28, row = 120 } } (VarPattern \"c\")])) (Node { end = { column = 42, row = 121 }, start = { column = 21, row = 121 } } (Application [Node { end = { column = 28, row = 121 }, start = { column = 21, row = 121 } } (FunctionOrValue [] \"decodeC\"),Node { end = { column = 34, row = 121 }, start = { column = 29, row = 121 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 121 }, start = { column = 35, row = 121 } } (FunctionOrValue [] \"offsetB\")])))], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 123 } } (TupledExpression [Node { end = { column = 22, row = 123 }, start = { column = 15, row = 123 } } (FunctionOrValue [] \"offsetC\"),Node { end = { column = 34, row = 123 }, start = { column = 24, row = 123 } } (Application [Node { end = { column = 28, row = 123 }, start = { column = 24, row = 123 } } (FunctionOrValue [] \"func\"),Node { end = { column = 30, row = 123 }, start = { column = 29, row = 123 } } (FunctionOrValue [] \"a\"),Node { end = { column = 32, row = 123 }, start = { column = 31, row = 123 } } (FunctionOrValue [] \"b\")",
            ",Node { end = { column = 34, row = 123 }, start = { column = 33, row = 123 } } (FunctionOrValue [] \"c\")])]) }) })))]), name = Node { end = { column = 5, row = 110 }, start = { column = 1, row = 110 } } \"map3\" }, documentation = Nothing, signature = Just (Node { end = { column = 86, row = 109 }, start = { column = 1, row = 109 } } { name = Node { end = { column = 5, row = 109 }, start = { column = 1, row = 109 } } \"map3\", typeAnnotation = Node { end = { column = 86, row = 109 }, start = { column = 8, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 30, row = 109 }, start = { column = 8, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 10, row = 109 }, start = { column = 9, row = 109 } } (GenericType \"a\")) (Node { end = { column = 29, row = 109 }, start = { column = 14, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 109 }, start = { column = 14, row = 109 } } (GenericType \"b\")) (Node { end = { column = 29, row = 109 }, start = { column = 19, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 20, row = 109 }, start = { column = 19, row = 109 } } (GenericType \"c\")) (Node { end = { column = 29, row = 109 }, start = { column = 24, row = 109 } } (GenericType \"value\")))))))) (Node { end = { column = 86, row = 109 }, start = { column = 34, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 43, row = 109 }, start = { column = 34, row = 109 } } (Typed (Node { end = { column = 41, row = 109 }, start = { column = 34, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 43, row = 109 }, start = { column = 42, row = 109 } } (GenericType \"a\")])) (Node { end = { column = 86, row = 109 }, start = { column = 47, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 56, row = 109 }, start = { column = 47, row = 109 } } (Typed (Node { end = { column = 54, row = 109 }, start = { column = 47, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 56, row = 109 }, start = { column = 55, row = 109 } } (GenericType \"b\")])) (Node { end = { column = 86, row = 109 }, start = { column = 60, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 69, row = 109 }, start = { column = 60, row = 109 } } (Typed (Node { end = { column = 67, row = 109 }, start = { column = 60, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 69, row = 109 }, start = { column = 68, row = 109 } } (GenericType \"c\")])) (Node { end = { column = 86, row = 109 }, start = { column = 73, row = 109 } } (Typed (Node { end = { column = 80, row = 109 }, start = { column = 73, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 86, row = 109 }, start = { column = 81, row = 109 } } (GenericType \"value\")]))))))))) }) })",
            ",Node { end = { column = 27, row = 130 }, start = { column = 1, row = 127 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 13, row = 128 }, start = { column = 7, row = 128 } } { arguments = [], name = Node { end = { column = 13, row = 128 }, start = { column = 7, row = 128 } } \"MyTrue\" },Node { end = { column = 14, row = 129 }, start = { column = 7, row = 129 } } { arguments = [], name = Node { end = { column = 14, row = 129 }, start = { column = 7, row = 129 } } \"MyFalse\" },Node { end = { column = 27, row = 130 }, start = { column = 7, row = 130 } } { arguments = [Node { end = { column = 19, row = 130 }, start = { column = 12, row = 130 } } (Typed (Node { end = { column = 19, row = 130 }, start = { column = 12, row = 130 } } ([],\"Boolean\")) []),Node { end = { column = 27, row = 130 }, start = { column = 20, row = 130 } } (Typed (Node { end = { column = 27, row = 130 }, start = { column = 20, row = 130 } } ([],\"Boolean\")) [])], name = Node { end = { column = 11, row = 130 }, start = { column = 7, row = 130 } } \"MyOr\" }], documentation = Nothing, generics = [], name = Node { end = { column = 13, row = 127 }, start = { column = 6, row = 127 } } \"Boolean\" })",
            ",Node { end = { column = 10, row = 150 }, start = { column = 1, row = 133 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 150 }, start = { column = 1, row = 134 } } { arguments = [], expression = Node { end = { column = 10, row = 150 }, start = { column = 5, row = 135 } } (Application [Node { end = { column = 17, row = 135 }, start = { column = 5, row = 135 } } (FunctionOrValue [\"Parser\"] \"oneOf\"),Node { end = { column = 10, row = 150 }, start = { column = 9, row = 136 } } (ListExpr [Node { end = { column = 37, row = 137 }, start = { column = 11, row = 136 } } (OperatorApplication \"|.\" Left (Node { end = { column = 32, row = 136 }, start = { column = 11, row = 136 } } (Application [Node { end = { column = 25, row = 136 }, start = { column = 11, row = 136 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 32, row = 136 }, start = { column = 26, row = 136 } } (FunctionOrValue [] \"MyTrue\")])) (Node { end = { column = 37, row = 137 }, start = { column = 16, row = 137 } } (Application [Node { end = { column = 30, row = 137 }, start = { column = 16, row = 137 } } (FunctionOrValue [\"Parser\"] \"keyword\"),Node { end = { column = 37, row = 137 }, start = { column = 31, row = 137 } } (Literal \"true\")]))),Node { end = { column = 38, row = 139 }, start = { column = 11, row = 138 } } (OperatorApplication \"|.\" Left (Node { end = { column = 33, row = 138 }, start = { column = 11, row = 138 } } (Application [Node { end = { column = 25, row = 138 }, start = { column = 11, row = 138 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 33, row = 138 }, start = { column = 26, row = 138 } } (FunctionOrValue [] \"MyFalse\")])) (Node { end = { column = 38, row = 139 }, start = { column = 16, row = 139 } } (Application [Node { end = { column = 30, row = 139 }, start = { column = 16, row = 139 } } (FunctionOrValue [\"Parser\"] \"keyword\"),Node { end = { column = 38, row = 139 }, start = { column = 31, row = 139 } } (Literal \"false\")]))),Node { end = { column = 33, row = 149 }, start = { column = 11, row = 140 } } (OperatorApplication \"|=\" Left (Node { end = { column = 29, row = 146 }, start = { column = 11, row = 140 } } (OperatorApplication \"|=\" Left (Node { end = { column = 29, row = 142 }, start = { column = 11, row = 140 } } (OperatorApplication \"|.\" Left (Node { end = { column = 33, row = 141 }, start = { column = 11, row = 140 } } (OperatorApplication \"|.\" Left (Node { end = { column = 30, row = 140 }, start = { column = 11, row = 140 } } (Application [Node { end = { column = 25, row = 140 }, start = { column = 11, row = 140 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 30, row = 140 }, start = { column = 26, row = 140 } } (FunctionOrValue [] \"MyOr\")])) (Node { end = { column = 33, row = 141 }, start = { column = 16, row = 141 } } (Application [Node { end = { column = 29, row = 141 }, start = { column = 16, row = 141 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 33, row = 141 }, start = { column = 30, row = 141 } } (Literal \"(\")])))) (Node { end = { column = 29, row = 142 }, start = { column = 16, row = 142 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 29, row = 146 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 34, row = 145 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 29, row = 144 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 43, row = 143 }, start = { column = 16, row = 143 } } (Application [Node { end = { column = 27, row = 143 }, start = { column = 16, row = 143 } } (FunctionOrValue [\"Parser\"] \"lazy\"),Node { end = { column = 43, row = 143 }, start = { column = 28, row = 143 } } (ParenthesizedExpression (Node { end = { column = 42, row = 143 }, start = { column = 29, row = 143 } } (LambdaExpression { args = [Node { end = { column = 31, row = 143 }, start = { column = 30, row = 143 } } AllPattern], expression = Node { end = { column = 42, row = 143 }, start = { column = 35, row = 143 } } (FunctionOrValue [] \"boolean\") })))])) (Node { end = { column = 29, row = 144 }, start = { column = 16, row = 144 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 34, row = 145 }, start = { column = 16, row = 145 } } (Application [Node { end = { column = 29, row = 145 }, start = { column = 16, row = 145 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 34, row = 145 }, start = { column = 30, row = 145 } } (Literal \"||\")])))) (Node { end = { column = 29, row = 146 }, start = { column = 16, row = 146 } } (FunctionOrValue [\"Parser\"] \"spaces\")))))) (Node { end = { column = 33, row = 149 }, start = { column = 16, row = 147 } } (OperatorApplication \"|.\" Left (Node { end = { column = 29, row = 148 }, start = { column = 16, row = 147 } } (OperatorApplication \"|.\" Left (Node { end = { column = 43, row = 147 }, start = { column = 16, row = 147 } } (Application [Node { end = { column = 27, row = 147 }, start = { column = 16, row = 147 } } (FunctionOrValue [\"Parser\"] \"lazy\"),Node { end = { column = 43, row = 147 }, start = { column = 28, row = 147 } } (ParenthesizedExpression (Node { end = { column = 42, row = 147 }, start = { column = 29, row = 147 } } (LambdaExpression { args = [Node { end = { column = 31, row = 147 }, start = { column = 30, row = 147 } } AllPattern], expression = Node { end = { column = 42, row = 147 }, start = { column = 35, row = 147 } } (FunctionOrValue [] \"boolean\") })))])) (Node { end = { column = 29, row = 148 }, start = { column = 16, row = 148 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 33, row = 149 }, start = { column = 16, row = 149 } } (Application [Node { end = { column = 29, row = 149 }, start = { column = 16, row = 149 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 33, row = 149 }, start = { column = 30, row = 149 } } (Literal \")\")])))))])]), name = Node { end = { column = 14, row = 134 }, start = { column = 1, row = 134 } } \"booleanParser\" }, documentation = Nothing, signature = Just (Node { end = { column = 38, row = 133 }, start = { column = 1, row = 133 } } { name = Node { end = { column = 14, row = 133 }, start = { column = 1, row = 133 } } \"booleanParser\", typeAnnotation = Node { end = { column = 38, row = 133 }, start = { column = 17, row = 133 } } (Typed (Node { end = { column = 30, row = 133 }, start = { column = 17, row = 133 } } ([\"Parser\"],\"Parser\")) [Node { end = { column = 38, row = 133 }, start = { column = 31, row = 133 } } (Typed (Node { end = { column = 38, row = 133 }, start = { column = 31, row = 133 } } ([],\"Boolean\")) [])]) }) })",
            ",Node { end = { column = 18, row = 155 }, start = { column = 1, row = 153 } } (FunctionDeclaration { declaration = Node { end = { column = 18, row = 155 }, start = { column = 1, row = 154 } } { arguments = [Node { end = { column = 22, row = 154 }, start = { column = 21, row = 154 } } (VarPattern \"a\"),Node { end = { column = 24, row = 154 }, start = { column = 23, row = 154 } } (VarPattern \"b\"),Node { end = { column = 26, row = 154 }, start = { column = 25, row = 154 } } (VarPattern \"c\"),Node { end = { column = 28, row = 154 }, start = { column = 27, row = 154 } } (VarPattern \"d\")], expression = Node { end = { column = 18, row = 155 }, start = { column = 5, row = 155 } } (OperatorApplication \"-\" Left (Node { end = { column = 14, row = 155 }, start = { column = 5, row = 155 } } (OperatorApplication \"+\" Left (Node { end = { column = 6, row = 155 }, start = { column = 5, row = 155 } } (FunctionOrValue [] \"a\")) (Node { end = { column = 14, row = 155 }, start = { column = 9, row = 155 } } (OperatorApplication \"*\" Left (Node { end = { column = 10, row = 155 }, start = { column = 9, row = 155 } } (FunctionOrValue [] \"b\")) (Node { end = { column = 14, row = 155 }, start = { column = 13, row = 155 } } (FunctionOrValue [] \"c\")))))) (Node { end = { column = 18, row = 155 }, start = { column = 17, row = 155 } } (FunctionOrValue [] \"d\"))), name = Node { end = { column = 20, row = 154 }, start = { column = 1, row = 154 } } \"mixedOperators_Alfa\" }, documentation = Nothing, signature = Just (Node { end = { column = 54, row = 153 }, start = { column = 1, row = 153 } } { name = Node { end = { column = 20, row = 153 }, start = { column = 1, row = 153 } } \"mixedOperators_Alfa\", typeAnnotation = Node { end = { column = 54, row = 153 }, start = { column = 23, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 153 }, start = { column = 23, row = 153 } } (Typed (Node { end = { column = 26, row = 153 }, start = { column = 23, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 30, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 153 }, start = { column = 30, row = 153 } } (Typed (Node { end = { column = 33, row = 153 }, start = { column = 30, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 37, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 40, row = 153 }, start = { column = 37, row = 153 } } (Typed (Node { end = { column = 40, row = 153 }, start = { column = 37, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 44, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 47, row = 153 }, start = { column = 44, row = 153 } } (Typed (Node { end = { column = 47, row = 153 }, start = { column = 44, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 51, row = 153 } } (Typed (Node { end = { column = 54, row = 153 }, start = { column = 51, row = 153 } } ([],\"Int\")) []))))))))) }) })",
            ",Node { end = { column = 19, row = 160 }, start = { column = 1, row = 158 } } (FunctionDeclaration { declaration = Node { end = { column = 19, row = 160 }, start = { column = 1, row = 159 } } { arguments = [Node { end = { column = 22, row = 159 }, start = { column = 21, row = 159 } } (VarPattern \"a\"),Node { end = { column = 24, row = 159 }, start = { column = 23, row = 159 } } (VarPattern \"b\"),Node { end = { column = 26, row = 159 }, start = { column = 25, row = 159 } } (VarPattern \"c\"),Node { end = { column = 28, row = 159 }, start = { column = 27, row = 159 } } (VarPattern \"d\")], expression = Node { end = { column = 19, row = 160 }, start = { column = 5, row = 160 } } (OperatorApplication \"+\" Left (Node { end = { column = 15, row = 160 }, start = { column = 5, row = 160 } } (OperatorApplication \"+\" Left (Node { end = { column = 6, row = 160 }, start = { column = 5, row = 160 } } (FunctionOrValue [] \"a\")) (Node { end = { column = 15, row = 160 }, start = { column = 9, row = 160 } } (OperatorApplication \"//\" Left (Node { end = { column = 10, row = 160 }, start = { column = 9, row = 160 } } (FunctionOrValue [] \"b\")) (Node { end = { column = 15, row = 160 }, start = { column = 14, row = 160 } } (FunctionOrValue [] \"c\")))))) (Node { end = { column = 19, row = 160 }, start = { column = 18, row = 160 } } (FunctionOrValue [] \"d\"))), name = Node { end = { column = 20, row = 159 }, start = { column = 1, row = 159 } } \"mixedOperators_Beta\" }, documentation = Nothing, signature = Just (Node { end = { column = 54, row = 158 }, start = { column = 1, row = 158 } } { name = Node { end = { column = 20, row = 158 }, start = { column = 1, row = 158 } } \"mixedOperators_Beta\", typeAnnotation = Node { end = { column = 54, row = 158 }, start = { column = 23, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 158 }, start = { column = 23, row = 158 } } (Typed (Node { end = { column = 26, row = 158 }, start = { column = 23, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 30, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 158 }, start = { column = 30, row = 158 } } (Typed (Node { end = { column = 33, row = 158 }, start = { column = 30, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 37, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 40, row = 158 }, start = { column = 37, row = 158 } } (Typed (Node { end = { column = 40, row = 158 }, start = { column = 37, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 44, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 47, row = 158 }, start = { column = 44, row = 158 } } (Typed (Node { end = { column = 47, row = 158 }, start = { column = 44, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 51, row = 158 } } (Typed (Node { end = { column = 54, row = 158 }, start = { column = 51, row = 158 } } ([],\"Int\")) []))))))))) }) })",
            ",Node { end = { column = 16, row = 165 }, start = { column = 1, row = 163 } } (FunctionDeclaration { declaration = Node { end = { column = 16, row = 165 }, start = { column = 1, row = 164 } } { arguments = [Node { end = { column = 20, row = 164 }, start = { column = 14, row = 164 } } (VarPattern \"record\")], expression = Node { end = { column = 16, row = 165 }, start = { column = 5, row = 165 } } (RecordAccess (Node { end = { column = 11, row = 165 }, start = { column = 5, row = 165 } } (FunctionOrValue [] \"record\")) (Node { end = { column = 16, row = 165 }, start = { column = 12, row = 165 } } \"alfa\")), name = Node { end = { column = 13, row = 164 }, start = { column = 1, row = 164 } } \"recordAccess\" }, documentation = Nothing, signature = Just (Node { end = { column = 33, row = 163 }, start = { column = 1, row = 163 } } { name = Node { end = { column = 13, row = 163 }, start = { column = 1, row = 163 } } \"recordAccess\", typeAnnotation = Node { end = { column = 33, row = 163 }, start = { column = 16, row = 163 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 163 }, start = { column = 16, row = 163 } } (Typed (Node { end = { column = 26, row = 163 }, start = { column = 16, row = 163 } } ([],\"RecordType\")) [])) (Node { end = { column = 33, row = 163 }, start = { column = 30, row = 163 } } (Typed (Node { end = { column = 33, row = 163 }, start = { column = 30, row = 163 } } ([],\"Int\")) []))) }) })",
            ",Node { end = { column = 17, row = 170 }, start = { column = 1, row = 168 } } (FunctionDeclaration { declaration = Node { end = { column = 17, row = 170 }, start = { column = 1, row = 169 } } { arguments = [Node { end = { column = 28, row = 169 }, start = { column = 22, row = 169 } } (VarPattern \"record\")], expression = Node { end = { column = 17, row = 170 }, start = { column = 5, row = 170 } } (Application [Node { end = { column = 10, row = 170 }, start = { column = 5, row = 170 } } (RecordAccessFunction \".beta\"),Node { end = { column = 17, row = 170 }, start = { column = 11, row = 170 } } (FunctionOrValue [] \"record\")]), name = Node { end = { column = 21, row = 169 }, start = { column = 1, row = 169 } } \"recordAccessFunction\" }, documentation = Nothing, signature = Just (Node { end = { column = 41, row = 168 }, start = { column = 1, row = 168 } } { name = Node { end = { column = 21, row = 168 }, start = { column = 1, row = 168 } } \"recordAccessFunction\", typeAnnotation = Node { end = { column = 41, row = 168 }, start = { column = 24, row = 168 } } (FunctionTypeAnnotation (Node { end = { column = 34, row = 168 }, start = { column = 24, row = 168 } } (Typed (Node { end = { column = 34, row = 168 }, start = { column = 24, row = 168 } } ([],\"RecordType\")) [])) (Node { end = { column = 41, row = 168 }, start = { column = 38, row = 168 } } (Typed (Node { end = { column = 41, row = 168 }, start = { column = 38, row = 168 } } ([],\"Int\")) []))) }) })",
            ",Node { end = { column = 34, row = 175 }, start = { column = 1, row = 173 } } (FunctionDeclaration { declaration = Node { end = { column = 34, row = 175 }, start = { column = 1, row = 174 } } { arguments = [Node { end = { column = 20, row = 174 }, start = { column = 14, row = 174 } } (VarPattern \"record\"),Node { end = { column = 29, row = 174 }, start = { column = 21, row = 174 } } (VarPattern \"newValue\")], expression = Node { end = { column = 34, row = 175 }, start = { column = 5, row = 175 } } (RecordUpdateExpression (Node { end = { column = 13, row = 175 }, start = { column = 7, row = 175 } } \"record\") [Node { end = { column = 33, row = 175 }, start = { column = 16, row = 175 } } [Node { end = { column = 21, row = 175 }, start = { column = 16, row = 175 } } \"gamma\",Node { end = { column = 32, row = 175 }, start = { column = 24, row = 175 } } (FunctionOrValue [] \"newValue\")]]), name = Node { end = { column = 13, row = 174 }, start = { column = 1, row = 174 } } \"recordUpdate\" }, documentation = Nothing, signature = Just (Node { end = { column = 47, row = 173 }, start = { column = 1, row = 173 } } { name = Node { end = { column = 13, row = 173 }, start = { column = 1, row = 173 } } \"recordUpdate\", typeAnnotation = Node { end = { column = 47, row = 173 }, start = { column = 16, row = 173 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 173 }, start = { column = 16, row = 173 } } (Typed (Node { end = { column = 26, row = 173 }, start = { column = 16, row = 173 } } ([],\"RecordType\")) [])) (Node { end = { column = 47, row = 173 }, start = { column = 30, row = 173 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 173 }, start = { column = 30, row = 173 } } (Typed (Node { end = { column = 33, row = 173 }, start = { column = 30, row = 173 } } ([],\"Int\")) [])) (Node { end = { column = 47, row = 173 }, start = { column = 37, row = 173 } } (Typed (Node { end = { column = 47, row = 173 }, start = { column = 37, row = 173 } } ([],\"RecordType\")) []))))) }) })]",
            ", imports = [Node { end = { column = 27, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Just (Node { end = { column = 27, row = 3 }, start = { column = 14, row = 3 } } (All { end = { column = 26, row = 3 }, start = { column = 24, row = 3 } })), moduleAlias = Nothing, moduleName = Node { end = { column = 13, row = 3 }, start = { column = 8, row = 3 } } [\"Delta\"] },Node { end = { column = 12, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 4 }, start = { column = 8, row = 4 } } [\"Dict\"] },Node { end = { column = 66, row = 5 }, start = { column = 1, row = 5 } } { exposingList = Just (Node { end = { column = 66, row = 5 }, start = { column = 23, row = 5 } } (Explicit [Node { end = { column = 47, row = 5 }, start = { column = 33, row = 5 } } (TypeExpose { name = \"Dependency\", open = Just { end = { column = 47, row = 5 }, start = { column = 43, row = 5 } } }),Node { end = { column = 56, row = 5 }, start = { column = 49, row = 5 } } (TypeOrAliasExpose \"Version\"),Node { end = { column = 65, row = 5 }, start = { column = 58, row = 5 } } (FunctionExpose \"epsilon\")])), moduleAlias = Nothing, moduleName = Node { end = { column = 22, row = 5 }, start = { column = 8, row = 5 } } [\"Elm\",\"Dependency\"] },Node { end = { column = 36, row = 6 }, start = { column = 1, row = 6 } } { exposingList = Nothing, moduleAlias = Just (Node { end = { column = 36, row = 6 }, start = { column = 27, row = 6 } } [\"Interface\"]), moduleName = Node { end = { column = 23, row = 6 }, start = { column = 8, row = 6 } } [\"Gamma\",\"Interface\"] }]",
            ", moduleDefinition = Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 36, row = 1 }, start = { column = 23, row = 1 } } (All { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } }), moduleName = Node { end = { column = 22, row = 1 }, start = { column = 8, row = 1 } } [\"Namespace\",\"Beta\"] }) }"
            ];

        var responseAsElmValue =
            TestParsingModuleText(
                elmModuleText,
                expectedExpressionStringChunks,
                alsoTestDotnetParser: false);


        var moduleDefinitionNode =
            ((ElmValue.ElmRecord)responseAsElmValue).Fields.First(f => f.FieldName is "moduleDefinition").Value;

        var moduleDefinitionNodeAsExpression =
            ElmValue.RenderAsElmExpression(moduleDefinitionNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 36, row = 1 }, start = { column = 23, row = 1 } } (All { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } }), moduleName = Node { end = { column = 22, row = 1 }, start = { column = 8, row = 1 } } ["Namespace","Beta"] })""",
            moduleDefinitionNodeAsExpression);

        var importsNode =
            ((ElmValue.ElmRecord)responseAsElmValue).Fields.First(f => f.FieldName is "imports").Value;

        var importsNodeAsExpression =
            ElmValue.RenderAsElmExpression(importsNode).expressionString;

        Assert.AreEqual(
            """[Node { end = { column = 27, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Just (Node { end = { column = 27, row = 3 }, start = { column = 14, row = 3 } } (All { end = { column = 26, row = 3 }, start = { column = 24, row = 3 } })), moduleAlias = Nothing, moduleName = Node { end = { column = 13, row = 3 }, start = { column = 8, row = 3 } } ["Delta"] },Node { end = { column = 12, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 4 }, start = { column = 8, row = 4 } } ["Dict"] },Node { end = { column = 66, row = 5 }, start = { column = 1, row = 5 } } { exposingList = Just (Node { end = { column = 66, row = 5 }, start = { column = 23, row = 5 } } (Explicit [Node { end = { column = 47, row = 5 }, start = { column = 33, row = 5 } } (TypeExpose { name = "Dependency", open = Just { end = { column = 47, row = 5 }, start = { column = 43, row = 5 } } }),Node { end = { column = 56, row = 5 }, start = { column = 49, row = 5 } } (TypeOrAliasExpose "Version"),Node { end = { column = 65, row = 5 }, start = { column = 58, row = 5 } } (FunctionExpose "epsilon")])), moduleAlias = Nothing, moduleName = Node { end = { column = 22, row = 5 }, start = { column = 8, row = 5 } } ["Elm","Dependency"] },Node { end = { column = 36, row = 6 }, start = { column = 1, row = 6 } } { exposingList = Nothing, moduleAlias = Just (Node { end = { column = 36, row = 6 }, start = { column = 27, row = 6 } } ["Interface"]), moduleName = Node { end = { column = 23, row = 6 }, start = { column = 8, row = 6 } } ["Gamma","Interface"] }]""",
            importsNodeAsExpression);

        var declarationsList =
            (ElmValue.ElmList)((ElmValue.ElmRecord)responseAsElmValue)["declarations"]!;

        var declarations =
            declarationsList.Elements.Cast<ElmValue.ElmTag>()
            .OrderBy(declarationNode =>
            ((ElmValue.ElmInteger)
            ((ElmValue.ElmRecord)((ElmValue.ElmRecord)declarationNode.Arguments[0])["start"]!)["row"]!).Value)
            .ToImmutableArray();

        Assert.AreEqual(16, declarations.Length);

        var typeAliasDeclarationNode = declarations.ElementAt(0);
        var recordAliasDeclarationNode = declarations.ElementAt(1);
        var choiceTypeDeclarationNode = declarations.ElementAt(2);

        var typeAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(typeAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 14, row = 10 }, start = { column = 1, row = 9 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 9 }, start = { column = 12, row = 9 } } "MaybeInt", typeAnnotation = Node { end = { column = 14, row = 10 }, start = { column = 5, row = 10 } } (Typed (Node { end = { column = 10, row = 10 }, start = { column = 5, row = 10 } } ([],"Maybe")) [Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } (Typed (Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } ([],"Int")) [])]) })""",
            typeAliasDeclarationNodeAsExpression);

        var recordAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(recordAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 19, row = 14 }, start = { column = 1, row = 13 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 13 }, start = { column = 12, row = 13 } } "RecordType", typeAnnotation = Node { end = { column = 19, row = 14 }, start = { column = 5, row = 14 } } (Record [Node { end = { column = 17, row = 14 }, start = { column = 7, row = 14 } } [Node { end = { column = 11, row = 14 }, start = { column = 7, row = 14 } } "alfa",Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } (Typed (Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } ([],"Int")) [])]]) })""",
            recordAliasDeclarationNodeAsExpression);

        var choiceTypeDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(choiceTypeDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 22, row = 19 }, start = { column = 1, row = 17 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } { arguments = [], name = Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } "Choice_Alfa" },Node { end = { column = 22, row = 19 }, start = { column = 7, row = 19 } } { arguments = [Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } ([],"Int")) [])], name = Node { end = { column = 18, row = 19 }, start = { column = 7, row = 19 } } "Choice_Beta" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 17 }, start = { column = 6, row = 17 } } "ChoiceType" })""",
            choiceTypeDeclarationNodeAsExpression);

        {
            var functionDeclarationNode = declarations.ElementAt(3);

            var declarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(functionDeclarationNode).expressionString;

            var declaration =
                (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

            Assert.AreEqual("FunctionDeclaration", declaration.TagName);

            var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

            var declarationSignatureNode =
                declarationRecord.Fields.First(f => f.FieldName is "signature").Value;

            var declarationSignatureNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationSignatureNode).expressionString;

            Assert.AreEqual(
                """Just (Node { end = { column = 25, row = 22 }, start = { column = 1, row = 22 } } { name = Node { end = { column = 6, row = 22 }, start = { column = 1, row = 22 } } "greet", typeAnnotation = Node { end = { column = 25, row = 22 }, start = { column = 9, row = 22 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } (Typed (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } ([],"String")) [])) (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } (Typed (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } ([],"String")) []))) })""",
                declarationSignatureNodeAsExpression);

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 36, row = 24 }, start = { column = 1, row = 23 } } { arguments = [Node { end = { column = 17, row = 23 }, start = { column = 7, row = 23 } } (VarPattern "param_name")], expression = Node { end = { column = 36, row = 24 }, start = { column = 5, row = 24 } } (OperatorApplication "++" Right (Node { end = { column = 14, row = 24 }, start = { column = 5, row = 24 } } (Literal "Hello, ")) (Node { end = { column = 36, row = 24 }, start = { column = 18, row = 24 } } (OperatorApplication "++" Right (Node { end = { column = 28, row = 24 }, start = { column = 18, row = 24 } } (FunctionOrValue [] "param_name")) (Node { end = { column = 36, row = 24 }, start = { column = 32, row = 24 } } (Literal " 👋"))))), name = Node { end = { column = 6, row = 23 }, start = { column = 1, row = 23 } } "greet" }""",
                declarationDeclarationNodeAsExpression);
        }

        {
            var functionDeclarationNode = declarations.ElementAt(5);

            var declarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(functionDeclarationNode).expressionString;

            var declaration =
                (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

            Assert.AreEqual("FunctionDeclaration", declaration.TagName);

            var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 40, row = 42 }, start = { column = 1, row = 36 } } { arguments = [Node { end = { column = 12, row = 36 }, start = { column = 11, row = 36 } } (VarPattern "a")], expression = Node { end = { column = 40, row = 42 }, start = { column = 5, row = 37 } } (CaseExpression { cases = [[Node { end = { column = 23, row = 38 }, start = { column = 9, row = 38 } } (NamedPattern { moduleName = [], name = "String" } [Node { end = { column = 23, row = 38 }, start = { column = 16, row = 38 } } (VarPattern "stringA")]),Node { end = { column = 61, row = 39 }, start = { column = 13, row = 39 } } (Application [Node { end = { column = 19, row = 39 }, start = { column = 13, row = 39 } } (FunctionOrValue [] "String"),Node { end = { column = 61, row = 39 }, start = { column = 20, row = 39 } } (ParenthesizedExpression (Node { end = { column = 60, row = 39 }, start = { column = 21, row = 39 } } (Application [Node { end = { column = 39, row = 39 }, start = { column = 21, row = 39 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 60, row = 39 }, start = { column = 40, row = 39 } } (ListExpr [Node { end = { column = 49, row = 39 }, start = { column = 42, row = 39 } } (FunctionOrValue [] "stringA"),Node { end = { column = 58, row = 39 }, start = { column = 51, row = 39 } } (FunctionOrValue [] "stringA")])])))])],[Node { end = { column = 10, row = 41 }, start = { column = 9, row = 41 } } AllPattern,Node { end = { column = 40, row = 42 }, start = { column = 13, row = 42 } } (Application [Node { end = { column = 31, row = 42 }, start = { column = 13, row = 42 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 40, row = 42 }, start = { column = 32, row = 42 } } (ListExpr [Node { end = { column = 35, row = 42 }, start = { column = 34, row = 42 } } (FunctionOrValue [] "a"),Node { end = { column = 38, row = 42 }, start = { column = 37, row = 42 } } (FunctionOrValue [] "a")])])]], expression = Node { end = { column = 11, row = 37 }, start = { column = 10, row = 37 } } (FunctionOrValue [] "a") }), name = Node { end = { column = 10, row = 36 }, start = { column = 1, row = 36 } } "replicate" }""",
                declarationDeclarationNodeAsExpression);
        }

        {
            var functionDeclarationNode = declarations.ElementAt(8);

            var declarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(functionDeclarationNode).expressionString;

            var declaration =
                (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

            Assert.AreEqual("FunctionDeclaration", declaration.TagName);

            var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 10, row = 124 }, start = { column = 1, row = 110 } } { arguments = [Node { end = { column = 10, row = 110 }, start = { column = 6, row = 110 } } (VarPattern "func"),Node { end = { column = 28, row = 110 }, start = { column = 11, row = 110 } } (ParenthesizedPattern (Node { end = { column = 27, row = 110 }, start = { column = 12, row = 110 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 27, row = 110 }, start = { column = 20, row = 110 } } (VarPattern "decodeA")]))),Node { end = { column = 46, row = 110 }, start = { column = 29, row = 110 } } (ParenthesizedPattern (Node { end = { column = 45, row = 110 }, start = { column = 30, row = 110 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 45, row = 110 }, start = { column = 38, row = 110 } } (VarPattern "decodeB")]))),Node { end = { column = 64, row = 110 }, start = { column = 47, row = 110 } } (ParenthesizedPattern (Node { end = { column = 63, row = 110 }, start = { column = 48, row = 110 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 63, row = 110 }, start = { column = 56, row = 110 } } (VarPattern "decodeC")])))], expression = Node { end = { column = 10, row = 124 }, start = { column = 5, row = 111 } } (Application [Node { end = { column = 12, row = 111 }, start = { column = 5, row = 111 } } (FunctionOrValue [] "Decoder"),Node { end = { column = 10, row = 124 }, start = { column = 9, row = 112 } } (ParenthesizedExpression (Node { end = { column = 36, row = 123 }, start = { column = 10, row = 112 } } (LambdaExpression { args = [Node { end = { column = 16, row = 112 }, start = { column = 11, row = 112 } } (VarPattern "bites"),Node { end = { column = 23, row = 112 }, start = { column = 17, row = 112 } } (VarPattern "offset")], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 113 } } (LetExpression { declarations = [Node { end = { column = 41, row = 115 }, start = { column = 17, row = 114 } } (LetDestructuring (Node { end = { column = 31, row = 114 }, start = { column = 17, row = 114 } } (TuplePattern [Node { end = { column = 26, row = 114 }, start = { column = 19, row = 114 } } (VarPattern "offsetA"),Node { end = { column = 29, row = 114 }, start = { column = 28, row = 114 } } (VarPattern "a")])) (Node { end = { column = 41, row = 115 }, start = { column = 21, row = 115 } } (Application [Node { end = { column = 28, row = 115 }, start = { column = 21, row = 115 } } (FunctionOrValue [] "decodeA"),Node { end = { column = 34, row = 115 }, start = { column = 29, row = 115 } } (FunctionOrValue [] "bites"),Node { end = { column = 41, row = 115 }, start = { column = 35, row = 115 } } (FunctionOrValue [] "offset")]))),Node { end = { column = 42, row = 118 }, start = { column = 17, row = 117 } } (LetDestructuring (Node { end = { column = 31, row = 117 }, start = { column = 17, row = 117 } } (TuplePattern [Node { end = { column = 26, row = 117 }, start = { column = 19, row = 117 } } (VarPattern "offsetB"),Node { end = { column = 29, row = 117 }, start = { column = 28, row = 117 } } (VarPattern "b")])) (Node { end = { column = 42, row = 118 }, start = { column = 21, row = 118 } } (Application [Node { end = { column = 28, row = 118 }, start = { column = 21, row = 118 } } (FunctionOrValue [] "decodeB"),Node { end = { column = 34, row = 118 }, start = { column = 29, row = 118 } } (FunctionOrValue [] "bites"),Node { end = { column = 42, row = 118 }, start = { column = 35, row = 118 } } (FunctionOrValue [] "offsetA")]))),Node { end = { column = 42, row = 121 }, start = { column = 17, row = 120 } } (LetDestructuring (Node { end = { column = 31, row = 120 }, start = { column = 17, row = 120 } } (TuplePattern [Node { end = { column = 26, row = 120 }, start = { column = 19, row = 120 } } (VarPattern "offsetC"),Node { end = { column = 29, row = 120 }, start = { column = 28, row = 120 } } (VarPattern "c")])) (Node { end = { column = 42, row = 121 }, start = { column = 21, row = 121 } } (Application [Node { end = { column = 28, row = 121 }, start = { column = 21, row = 121 } } (FunctionOrValue [] "decodeC"),Node { end = { column = 34, row = 121 }, start = { column = 29, row = 121 } } (FunctionOrValue [] "bites"),Node { end = { column = 42, row = 121 }, start = { column = 35, row = 121 } } (FunctionOrValue [] "offsetB")])))], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 123 } } (TupledExpression [Node { end = { column = 22, row = 123 }, start = { column = 15, row = 123 } } (FunctionOrValue [] "offsetC"),Node { end = { column = 34, row = 123 }, start = { column = 24, row = 123 } } (Application [Node { end = { column = 28, row = 123 }, start = { column = 24, row = 123 } } (FunctionOrValue [] "func"),Node { end = { column = 30, row = 123 }, start = { column = 29, row = 123 } } (FunctionOrValue [] "a"),Node { end = { column = 32, row = 123 }, start = { column = 31, row = 123 } } (FunctionOrValue [] "b"),Node { end = { column = 34, row = 123 }, start = { column = 33, row = 123 } } (FunctionOrValue [] "c")])]) }) })))]), name = Node { end = { column = 5, row = 110 }, start = { column = 1, row = 110 } } "map3" }""",
                declarationDeclarationNodeAsExpression);
        }
    }

    [TestMethod]
    public void Parse_Elm_module_Bytes_Decode_decodeBlobAsCharsRec()
    {
        var elmModuleText =
            """
            module Bytes.Decode exposing (..)


            decodeBlobAsCharsRec : Int -> Int -> List Char -> String
            decodeBlobAsCharsRec offset blob chars =
                if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length blob, offset ] then
                    String.fromList (List.reverse chars)

                else
                    let
                        ( char, bytesConsumed ) =
                            decodeUtf8Char blob offset
                    in
                    decodeBlobAsCharsRec
                        (offset + bytesConsumed)
                        blob
                        (char :: chars)


            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = [], declarations = ",
            "[Node { end = { column = 28, row = 17 }, start = { column = 1, row = 4 } } (FunctionDeclaration { declaration = Node { end = { column = 28, row = 17 }, start = { column = 1, row = 5 } } { arguments = [Node { end = { column = 28, row = 5 }, start = { column = 22, row = 5 } } (VarPattern \"offset\"),Node { end = { column = 33, row = 5 }, start = { column = 29, row = 5 } } (VarPattern \"blob\"),Node { end = { column = 39, row = 5 }, start = { column = 34, row = 5 } } (VarPattern \"chars\")], expression = Node { end = { column = 28, row = 17 }, start = { column = 5, row = 6 } } (IfBlock (Node { end = { column = 73, row = 6 }, start = { column = 8, row = 6 } } (Application [Node { end = { column = 37, row = 6 }, start = { column = 8, row = 6 } } (FunctionOrValue [\"Pine_kernel\"] \"int_is_sorted_asc\"),Node { end = { column = 73, row = 6 }, start = { column = 38, row = 6 } } (ListExpr [Node { end = { column = 63, row = 6 }, start = { column = 40, row = 6 } } (Application [Node { end = { column = 58, row = 6 }, start = { column = 40, row = 6 } } (FunctionOrValue [\"Pine_kernel\"] \"length\"),Node { end = { column = 63, row = 6 }, start = { column = 59, row = 6 } } (FunctionOrValue [] \"blob\")]),Node { end = { column = 71, row = 6 }, start = { column = 65, row = 6 } } (FunctionOrValue [] \"offset\")])])) (Node { end = { column = 45, row = 7 }, start = { column = 9, row = 7 } } (Application [Node { end = { column = 24, row = 7 }, start = { column = 9, row = 7 } } (FunctionOrValue [\"String\"] \"fromList\"),Node { end = { column = 45, row = 7 }, start = { column = 25, row = 7 } } (ParenthesizedExpression (Node { end = { column = 44, row = 7 }, start = { column = 26, row = 7 } } (Application [Node { end = { column = 38, row = 7 }, start = { column = 26, row = 7 } } (FunctionOrValue [\"List\"] \"reverse\")",
            ",Node { end = { column = 44, row = 7 }, start = { column = 39, row = 7 } } (FunctionOrValue [] \"chars\")])))])) (Node { end = { column = 28, row = 17 }, start = { column = 9, row = 10 } } (LetExpression { declarations = [Node { end = { column = 43, row = 12 }, start = { column = 13, row = 11 } } (LetDestructuring (Node { end = { column = 36, row = 11 }, start = { column = 13, row = 11 } } (TuplePattern [Node { end = { column = 19, row = 11 }, start = { column = 15, row = 11 } } (VarPattern \"char\")",
            ",Node { end = { column = 34, row = 11 }, start = { column = 21, row = 11 } } (VarPattern \"bytesConsumed\")])) (Node { end = { column = 43, row = 12 }, start = { column = 17, row = 12 } } (Application [Node { end = { column = 31, row = 12 }, start = { column = 17, row = 12 } } (FunctionOrValue [] \"decodeUtf8Char\"),Node { end = { column = 36, row = 12 }, start = { column = 32, row = 12 } } (FunctionOrValue [] \"blob\"),Node { end = { column = 43, row = 12 }, start = { column = 37, row = 12 } } (FunctionOrValue [] \"offset\")])))], expression = Node { end = { column = 28, row = 17 }, start = { column = 9, row = 14 } } (Application [Node { end = { column = 29, row = 14 }, start = { column = 9, row = 14 } } (FunctionOrValue [] \"decodeBlobAsCharsRec\"),Node { end = { column = 37, row = 15 }, start = { column = 13, row = 15 } } (ParenthesizedExpression (Node { end = { column = 36, row = 15 }, start = { column = 14, row = 15 } } (OperatorApplication \"+\" Left (Node { end = { column = 20, row = 15 }, start = { column = 14, row = 15 } } (FunctionOrValue [] \"offset\")) (Node { end = { column = 36, row = 15 }, start = { column = 23, row = 15 } } (FunctionOrValue [] \"bytesConsumed\"))))),Node { end = { column = 17, row = 16 }, start = { column = 13, row = 16 } } (FunctionOrValue [] \"blob\"),Node { end = { column = 28, row = 17 }, start = { column = 13, row = 17 } } (ParenthesizedExpression (Node { end = { column = 27, row = 17 }, start = { column = 14, row = 17 } } (OperatorApplication \"::\" Right (Node { end = { column = 18, row = 17 }, start = { column = 14, row = 17 } } (FunctionOrValue [] \"char\")) (Node { end = { column = 27, row = 17 }, start = { column = 22, row = 17 } } (FunctionOrValue [] \"chars\")))))]) }))), name = Node { end = { column = 21, row = 5 }, start = { column = 1, row = 5 } } \"decodeBlobAsCharsRec\" }, documentation = Nothing, signature = Just (Node { end = { column = 57, row = 4 }, start = { column = 1, row = 4 } } { name = Node { end = { column = 21, row = 4 }, start = { column = 1, row = 4 } } \"decodeBlobAsCharsRec\", typeAnnotation = Node { end = { column = 57, row = 4 }, start = { column = 24, row = 4 } } (FunctionTypeAnnotation (Node { end = { column = 27, row = 4 }, start = { column = 24, row = 4 } } (Typed (Node { end = { column = 27, row = 4 }, start = { column = 24, row = 4 } } ([],\"Int\")) [])) (Node { end = { column = 57, row = 4 }, start = { column = 31, row = 4 } } (FunctionTypeAnnotation (Node { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } } (Typed (Node { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } } ([],\"Int\")) [])) (Node { end = { column = 57, row = 4 }, start = { column = 38, row = 4 } } (FunctionTypeAnnotation (Node { end = { column = 47, row = 4 }, start = { column = 38, row = 4 } } (Typed (Node { end = { column = 42, row = 4 }, start = { column = 38, row = 4 } } ([],\"List\")) [Node { end = { column = 47, row = 4 }, start = { column = 43, row = 4 } } (Typed (Node { end = { column = 47, row = 4 }, start = { column = 43, row = 4 } } ([],\"Char\")) [])])) (Node { end = { column = 57, row = 4 }, start = { column = 51, row = 4 } } (Typed (Node { end = { column = 57, row = 4 }, start = { column = 51, row = 4 } } ([],\"String\")) []))))))) }) })]",
            ", imports = []",
            ", moduleDefinition = Node { end = { column = 34, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 34, row = 1 }, start = { column = 21, row = 1 } } (All { end = { column = 33, row = 1 }, start = { column = 31, row = 1 } }), moduleName = Node { end = { column = 20, row = 1 }, start = { column = 8, row = 1 } } [\"Bytes\",\"Decode\"] }) }"
            ];

        TestParsingModuleText(
            elmModuleText,
            expectedExpressionStringChunks,
            alsoTestDotnetParser: false);
    }

    [TestMethod]
    public void Dotnet_parser_results_in_same_expression_as_Elm_parser_source_files()
    {
        /*
         * Drawing inspiration from:
         * https://github.com/pine-vm/pine/blob/73fc8872476836f9e5a596d1d9278a289412be1f/implement/example-apps/elm-editor/src/CompilationInterface/SourceFiles/Generated_SourceFiles.elm
         * */

        var elmModuleText =
            """
            module CompilationInterface.SourceFiles.Generated_SourceFiles exposing (..)


            type FileTreeNode blobStructure
                = BlobNode blobStructure
                | TreeNode (List ( String, FileTreeNode blobStructure ))


            file__src_Backend_VolatileProcess_csx =
                { utf8 = "#r \"netstandard\"\n#r \"System\"\n#r \"System.Collections.Immutable\"\n#r \"System.Net\"----truncated" }

            file_tree_node_elm_core_modules_explicit_import =
                TreeNode
                    [( "Array.elm"
                    , BlobNode ({ utf8 = "module Array\n    exposing\n        ----truncated" })
                    )
                    ,( "Bitwise.elm"
                    , BlobNode ({ utf8 = "module Bitwise exposing\n  ----truncated" })
                    )]
            
            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = [], declarations = [Node { end = { column = 61, row = 6 }, start = { column = 1, row = 4 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 29, row = 5 }, start = { column = 7, row = 5 } } { arguments = [Node { end = { column = 29, row = 5 }, start = { column = 16, row = 5 } } (GenericType \"blobStructure\")], name = Node { end = { column = 15, row = 5 }, start = { column = 7, row = 5 } } \"BlobNode\" },Node { end = { column = 61, row = 6 }, start = { column = 7, row = 6 } } { arguments = [Node { end = { column = 61, row = 6 }, start = { column = 16, row = 6 } } (Typed (Node { end = { column = 21, row = 6 }, start = { column = 17, row = 6 } } ([],\"List\")) [Node { end = { column = 60, row = 6 }, start = { column = 22, row = 6 } } (Tupled [Node { end = { column = 30, row = 6 }, start = { column = 24, row = 6 } } (Typed (Node { end = { column = 30, row = 6 }, start = { column = 24, row = 6 } } ([],\"String\")) []),Node { end = { column = 58, row = 6 }, start = { column = 32, row = 6 } } (Typed (Node { end = { column = 44, row = 6 }, start = { column = 32, row = 6 } } ([],\"FileTreeNode\")) [Node { end = { column = 58, row = 6 }, start = { column = 45, row = 6 } } (GenericType \"blobStructure\")])])])], name = Node { end = { column = 15, row = 6 }, start = { column = 7, row = 6 } } \"TreeNode\" }], documentation = Nothing, generics = [Node { end = { column = 32, row = 4 }, start = { column = 19, row = 4 } } \"blobStructure\"], name = Node { end = { column = 18, row = 4 }, start = { column = 6, row = 4 } } \"FileTreeNode\" }),Node { end = { column = 120, row = 10 }, start = { column = 1, row = 9 } } (FunctionDeclaration { declaration = Node { end = { column = 120, row = 10 }, start = { column = 1, row = 9 } } { arguments = [], expression = Node { end = { column = 120, row = 10 }, start = { column = 5, row = 10 } } (RecordExpr [Node { end = { column = 118, row = 10 }, start = { column = 7, row = 10 } } [Node { end = { column = 11, row = 10 }, start = { column = 7, row = 10 } } \"utf8\",Node { end = { column = 118, row = 10 }, start = { column = 14, row = 10 } } (Literal \"#r \\\"netstandard\\\"\n#r \\\"System\\\"\n#r \\\"System.Collections.Immutable\\\"\n#r \\\"System.Net\\\"----truncated\")]]), name = Node { end = { column = 38, row = 9 }, start = { column = 1, row = 9 } } \"file__src_Backend_VolatileProcess_csx\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 11, row = 19 }, start = { column = 1, row = 12 } } (FunctionDeclaration { declaration = Node { end = { column = 11, row = 19 }, start = { column = 1, row = 12 } } { arguments = [], expression = Node { end = { column = 11, row = 19 }, start = { column = 5, row = 13 } } (Application [Node { end = { column = 13, row = 13 }, start = { column = 5, row = 13 } } (FunctionOrValue [] \"TreeNode\"),Node { end = { column = 11, row = 19 }, start = { column = 9, row = 14 } } (ListExpr [Node { end = { column = 10, row = 16 }, start = { column = 10, row = 14 } } (TupledExpression [Node { end = { column = 23, row = 14 }, start = { column = 12, row = 14 } } (Literal \"Array.elm\"),Node { end = { column = 84, row = 15 }, start = { column = 11, row = 15 } } (Application [Node { end = { column = 19, row = 15 }, start = { column = 11, row = 15 } } (FunctionOrValue [] \"BlobNode\"),Node { end = { column = 84, row = 15 }, start = { column = 20, row = 15 } } (ParenthesizedExpression (Node { end = { column = 83, row = 15 }, start = { column = 21, row = 15 } } (RecordExpr [Node { end = { column = 81, row = 15 }, start = { column = 23, row = 15 } } [Node { end = { column = 27, row = 15 }, start = { column = 23, row = 15 } } \"utf8\",Node { end = { column = 81, row = 15 }, start = { column = 30, row = 15 } } (Literal \"module Array\n    exposing\n        ----truncated\")]])))])]),Node { end = { column = 10, row = 19 }, start = { column = 10, row = 17 } } (TupledExpression [Node { end = { column = 25, row = 17 }, start = { column = 12, row = 17 } } (Literal \"Bitwise.elm\"),Node { end = { column = 75, row = 18 }, start = { column = 11, row = 18 } } (Application [Node { end = { column = 19, row = 18 }, start = { column = 11, row = 18 } } (FunctionOrValue [] \"BlobNode\"),Node { end = { column = 75, row = 18 }, start = { column = 20, row = 18 } } (ParenthesizedExpression (Node { end = { column = 74, row = 18 }, start = { column = 21, row = 18 } } (RecordExpr [Node { end = { column = 72, row = 18 }, start = { column = 23, row = 18 } } [Node { end = { column = 27, row = 18 }, start = { column = 23, row = 18 } } \"utf8\",Node { end = { column = 72, row = 18 }, start = { column = 30, row = 18 } } (Literal \"module Bitwise exposing\n  ----truncated\")]])))])])])]), name = Node { end = { column = 48, row = 12 }, start = { column = 1, row = 12 } } \"file_tree_node_elm_core_modules_explicit_import\" }, documentation = Nothing, signature = Nothing })], imports = [], moduleDefinition = Node { end = { column = 76, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 76, row = 1 }, start = { column = 63, row = 1 } } (All { end = { column = 75, row = 1 }, start = { column = 73, row = 1 } }), moduleName = Node { end = { column = 62, row = 1 }, start = { column = 8, row = 1 } } [\"CompilationInterface\",\"SourceFiles\",\"Generated_SourceFiles\"] }) }"
            ];

        TestParsingModuleText(
            elmModuleText,
            expectedExpressionStringChunks,
            alsoTestDotnetParser: true);
    }

    [TestMethod]
    public void Dotnet_parser_results_in_same_expression_as_Elm_parser_json_converters()
    {
        /*
         * Drawing inspiration from:
         * https://github.com/pine-vm/pine/blob/73fc8872476836f9e5a596d1d9278a289412be1f/implement/example-apps/elm-editor/src/CompilationInterface/GenerateJsonConverters/Generated_JsonConverters.elm
         * */

        var elmModuleText =
            """
            module CompilationInterface.GenerateJsonConverters.Generated_JsonConverters exposing (..)


            import Bytes.Encode
            import CompilerGenerated.Base64 as Base64
            import WorkspaceState_2021_01
            
            jsonEncode_2548601546 valueToEncode =
                jsonEncode_FrontendBackendInterface_RequestStructure valueToEncode


            jsonEncode_2146051484 valueToEncode =
                jsonEncode_FileTree_FileTreeNode (\type_arg -> json_encode_Bytes type_arg) valueToEncode


            jsonEncode__generic_Maybe encodeJust valueToEncode =
                case valueToEncode of
                    Nothing ->
                        [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

                    Just just ->
                        [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object


            jsonDecode__generic_Maybe decoder =
                Json.Decode.oneOf
                    -- single-line comment
                    [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
                    , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
                    ]

            jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
            jsonDecodeSucceedWhenNotNull valueIfNotNull =
                Json.Decode.value
                    |> Json.Decode.andThen
                        (\asValue ->
                            if asValue == Json.Encode.null then
                                Json.Decode.fail "Is null."

                            else
                                Json.Decode.succeed valueIfNotNull
                        )
            
            jsonEncode_119717524 valueToEncode =
                jsonEncode__generic_Dict (\type_arg -> Json.Encode.int type_arg) (\type_arg -> Json.Encode.object
                    [ ( "length"
                        , Json.Encode.int type_arg.length
                        )
                    ]) valueToEncode


            jsonDecode_119717524 =
                jsonDecode__generic_Dict Json.Decode.int (Json.Decode.succeed (\length -> { length = length })
                |> jsonDecode_andMap
                    ( jsonDecode_field_withAlternateNames "length"
                        [ "Length" ]
                        Json.Decode.int
                    ))


            jsonEncode__tuple_2 encodeA encodeB ( a, b ) =
                [ a |> encodeA, b |> encodeB ]
                |> Json.Encode.list identity


            jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
            jsonDecode_andMap =
                Json.Decode.map2 (|>)
            

            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = [Node { end = { column = 31, row = 27 }, start = { column = 9, row = 27 } } \"-- single-line comment\"], declarations = [Node { end = { column = 71, row = 9 }, start = { column = 1, row = 8 } } (FunctionDeclaration { declaration = Node { end = { column = 71, row = 9 }, start = { column = 1, row = 8 } } { arguments = [Node { end = { column = 36, row = 8 }, start = { column = 23, row = 8 } } (VarPattern \"valueToEncode\")], expression = Node { end = { column = 71, row = 9 }, start = { column = 5, row = 9 } } (Application [Node { end = { column = 57, row = 9 }, start = { column = 5, row = 9 } } (FunctionOrValue [] \"jsonEncode_FrontendBackendInterface_RequestStructure\"),Node { end = { column = 71, row = 9 }, start = { column = 58, row = 9 } } (FunctionOrValue [] \"valueToEncode\")]), name = Node { end = { column = 22, row = 8 }, start = { column = 1, row = 8 } } \"jsonEncode_2548601546\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 93, row = 13 }, start = { column = 1, row = 12 } } (FunctionDeclaration { declaration = Node { end = { column = 93, row = 13 }, start = { column = 1, row = 12 } } { arguments = [Node { end = { column = 36, row = 12 }, start = { column = 23, row = 12 } } (VarPattern \"valueToEncode\")], expression = Node { end = { column = 93, row = 13 }, start = { column = 5, row = 13 } } (Application [Node { end = { column = 37, row = 13 }, start = { column = 5, row = 13 } } (FunctionOrValue [] \"jsonEncode_FileTree_FileTreeNode\"),Node { end = { column = 79, row = 13 }, start = { column = 38, row = 13 } } (ParenthesizedExpression (Node { end = { column = 78, row = 13 }, start = { column = 39, row = 13 } } (LambdaExpression { args = [Node { end = { column = 48, row = 13 }, start = { column = 40, row = 13 } } (VarPattern \"type_arg\")], expression = Node { end = { column = 78, row = 13 }, start = { column = 52, row = 13 } } (Application [Node { end = { column = 69, row = 13 }, start = { column = 52, row = 13 } } (FunctionOrValue [] \"json_encode_Bytes\"),Node { end = { column = 78, row = 13 }, start = { column = 70, row = 13 } } (FunctionOrValue [] \"type_arg\")]) }))),Node { end = { column = 93, row = 13 }, start = { column = 80, row = 13 } } (FunctionOrValue [] \"valueToEncode\")]), name = Node { end = { column = 22, row = 12 }, start = { column = 1, row = 12 } } \"jsonEncode_2146051484\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 90, row = 22 }, start = { column = 1, row = 16 } } (FunctionDeclaration { declaration = Node { end = { column = 90, row = 22 }, start = { column = 1, row = 16 } } { arguments = [Node { end = { column = 37, row = 16 }, start = { column = 27, row = 16 } } (VarPattern \"encodeJust\"),Node { end = { column = 51, row = 16 }, start = { column = 38, row = 16 } } (VarPattern \"valueToEncode\")], expression = Node { end = { column = 90, row = 22 }, start = { column = 5, row = 17 } } (CaseExpression { cases = [[Node { end = { column = 16, row = 18 }, start = { column = 9, row = 18 } } (NamedPattern { moduleName = [], name = \"Nothing\" } []),Node { end = { column = 85, row = 19 }, start = { column = 13, row = 19 } } (OperatorApplication \"|>\" Left (Node { end = { column = 63, row = 19 }, start = { column = 13, row = 19 } } (ListExpr [Node { end = { column = 61, row = 19 }, start = { column = 15, row = 19 } } (TupledExpression [Node { end = { column = 26, row = 19 }, start = { column = 17, row = 19 } } (Literal \"Nothing\"),Node { end = { column = 59, row = 19 }, start = { column = 28, row = 19 } } (OperatorApplication \"|>\" Left (Node { end = { column = 30, row = 19 }, start = { column = 28, row = 19 } } (ListExpr [])) (Node { end = { column = 59, row = 19 }, start = { column = 34, row = 19 } } (Application [Node { end = { column = 50, row = 19 }, start = { column = 34, row = 19 } } (FunctionOrValue [\"Json\",\"Encode\"] \"list\"),Node { end = { column = 59, row = 19 }, start = { column = 51, row = 19 } } (FunctionOrValue [] \"identity\")])))])])) (Node { end = { column = 85, row = 19 }, start = { column = 67, row = 19 } } (FunctionOrValue [\"Json\",\"Encode\"] \"object\")))],[Node { end = { column = 18, row = 21 }, start = { column = 9, row = 21 } } (NamedPattern { moduleName = [], name = \"Just\" } [Node { end = { column = 18, row = 21 }, start = { column = 14, row = 21 } } (VarPattern \"just\")]),Node { end = { column = 90, row = 22 }, start = { column = 13, row = 22 } } (OperatorApplication \"|>\" Left (Node { end = { column = 68, row = 22 }, start = { column = 13, row = 22 } } (ListExpr [Node { end = { column = 66, row = 22 }, start = { column = 15, row = 22 } } (TupledExpression [Node { end = { column = 23, row = 22 }, start = { column = 17, row = 22 } } (Literal \"Just\"),Node { end = { column = 64, row = 22 }, start = { column = 25, row = 22 } } (OperatorApplication \"|>\" Left (Node { end = { column = 33, row = 22 }, start = { column = 25, row = 22 } } (ListExpr [Node { end = { column = 31, row = 22 }, start = { column = 27, row = 22 } } (FunctionOrValue [] \"just\")])) (Node { end = { column = 64, row = 22 }, start = { column = 37, row = 22 } } (Application [Node { end = { column = 53, row = 22 }, start = { column = 37, row = 22 } } (FunctionOrValue [\"Json\",\"Encode\"] \"list\"),Node { end = { column = 64, row = 22 }, start = { column = 54, row = 22 } } (FunctionOrValue [] \"encodeJust\")])))])])) (Node { end = { column = 90, row = 22 }, start = { column = 72, row = 22 } } (FunctionOrValue [\"Json\",\"Encode\"] \"object\")))]], expression = Node { end = { column = 23, row = 17 }, start = { column = 10, row = 17 } } (FunctionOrValue [] \"valueToEncode\") }), name = Node { end = { column = 26, row = 16 }, start = { column = 1, row = 16 } } \"jsonEncode__generic_Maybe\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 10, row = 30 }, start = { column = 1, row = 25 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 30 }, start = { column = 1, row = 25 } } { arguments = [Node { end = { column = 34, row = 25 }, start = { column = 27, row = 25 } } (VarPattern \"decoder\")], expression = Node { end = { column = 10, row = 30 }, start = { column = 5, row = 26 } } (Application [Node { end = { column = 22, row = 26 }, start = { column = 5, row = 26 } } (FunctionOrValue [\"Json\",\"Decode\"] \"oneOf\"),Node { end = { column = 10, row = 30 }, start = { column = 9, row = 28 } } (ListExpr [Node { end = { column = 68, row = 28 }, start = { column = 11, row = 28 } } (Application [Node { end = { column = 28, row = 28 }, start = { column = 11, row = 28 } } (FunctionOrValue [\"Json\",\"Decode\"] \"field\"),Node { end = { column = 38, row = 28 }, start = { column = 29, row = 28 } } (Literal \"Nothing\"),Node { end = { column = 68, row = 28 }, start = { column = 39, row = 28 } } (ParenthesizedExpression (Node { end = { column = 67, row = 28 }, start = { column = 40, row = 28 } } (Application [Node { end = { column = 59, row = 28 }, start = { column = 40, row = 28 } } (FunctionOrValue [\"Json\",\"Decode\"] \"succeed\"),Node { end = { column = 67, row = 28 }, start = { column = 60, row = 28 } } (FunctionOrValue [] \"Nothing\")])))]),Node { end = { column = 91, row = 29 }, start = { column = 11, row = 29 } } (Application [Node { end = { column = 28, row = 29 }, start = { column = 11, row = 29 } } (FunctionOrValue [\"Json\",\"Decode\"] \"field\"),Node { end = { column = 35, row = 29 }, start = { column = 29, row = 29 } } (Literal \"Just\"),Node { end = { column = 91, row = 29 }, start = { column = 36, row = 29 } } (ParenthesizedExpression (Node { end = { column = 90, row = 29 }, start = { column = 37, row = 29 } } (OperatorApplication \"|>\" Left (Node { end = { column = 66, row = 29 }, start = { column = 37, row = 29 } } (ParenthesizedExpression (Node { end = { column = 65, row = 29 }, start = { column = 38, row = 29 } } (Application [Node { end = { column = 55, row = 29 }, start = { column = 38, row = 29 } } (FunctionOrValue [\"Json\",\"Decode\"] \"index\"),Node { end = { column = 57, row = 29 }, start = { column = 56, row = 29 } } (Integer 0),Node { end = { column = 65, row = 29 }, start = { column = 58, row = 29 } } (FunctionOrValue [] \"decoder\")])))) (Node { end = { column = 90, row = 29 }, start = { column = 70, row = 29 } } (Application [Node { end = { column = 85, row = 29 }, start = { column = 70, row = 29 } } (FunctionOrValue [\"Json\",\"Decode\"] \"map\"),Node { end = { column = 90, row = 29 }, start = { column = 86, row = 29 } } (FunctionOrValue [] \"Just\")])))))])])]), name = Node { end = { column = 26, row = 25 }, start = { column = 1, row = 25 } } \"jsonDecode__generic_Maybe\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 14, row = 42 }, start = { column = 1, row = 32 } } (FunctionDeclaration { declaration = Node { end = { column = 14, row = 42 }, start = { column = 1, row = 33 } } { arguments = [Node { end = { column = 44, row = 33 }, start = { column = 30, row = 33 } } (VarPattern \"valueIfNotNull\")], expression = Node { end = { column = 14, row = 42 }, start = { column = 5, row = 34 } } (OperatorApplication \"|>\" Left (Node { end = { column = 22, row = 34 }, start = { column = 5, row = 34 } } (FunctionOrValue [\"Json\",\"Decode\"] \"value\")) (Node { end = { column = 14, row = 42 }, start = { column = 12, row = 35 } } (Application [Node { end = { column = 31, row = 35 }, start = { column = 12, row = 35 } } (FunctionOrValue [\"Json\",\"Decode\"] \"andThen\"),Node { end = { column = 14, row = 42 }, start = { column = 13, row = 36 } } (ParenthesizedExpression (Node { end = { column = 55, row = 41 }, start = { column = 14, row = 36 } } (LambdaExpression { args = [Node { end = { column = 22, row = 36 }, start = { column = 15, row = 36 } } (VarPattern \"asValue\")], expression = Node { end = { column = 55, row = 41 }, start = { column = 17, row = 37 } } (IfBlock (Node { end = { column = 47, row = 37 }, start = { column = 20, row = 37 } } (OperatorApplication \"==\" Non (Node { end = { column = 27, row = 37 }, start = { column = 20, row = 37 } } (FunctionOrValue [] \"asValue\")) (Node { end = { column = 47, row = 37 }, start = { column = 31, row = 37 } } (FunctionOrValue [\"Json\",\"Encode\"] \"null\")))) (Node { end = { column = 48, row = 38 }, start = { column = 21, row = 38 } } (Application [Node { end = { column = 37, row = 38 }, start = { column = 21, row = 38 } } (FunctionOrValue [\"Json\",\"Decode\"] \"fail\"),Node { end = { column = 48, row = 38 }, start = { column = 38, row = 38 } } (Literal \"Is null.\")])) (Node { end = { column = 55, row = 41 }, start = { column = 21, row = 41 } } (Application [Node { end = { column = 40, row = 41 }, start = { column = 21, row = 41 } } (FunctionOrValue [\"Json\",\"Decode\"] \"succeed\"),Node { end = { column = 55, row = 41 }, start = { column = 41, row = 41 } } (FunctionOrValue [] \"valueIfNotNull\")]))) })))]))), name = Node { end = { column = 29, row = 33 }, start = { column = 1, row = 33 } } \"jsonDecodeSucceedWhenNotNull\" }, documentation = Nothing, signature = Just (Node { end = { column = 58, row = 32 }, start = { column = 1, row = 32 } } { name = Node { end = { column = 29, row = 32 }, start = { column = 1, row = 32 } } \"jsonDecodeSucceedWhenNotNull\", typeAnnotation = Node { end = { column = 58, row = 32 }, start = { column = 32, row = 32 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 32 }, start = { column = 32, row = 32 } } (GenericType \"a\")) (Node { end = { column = 58, row = 32 }, start = { column = 37, row = 32 } } (Typed (Node { end = { column = 56, row = 32 }, start = { column = 37, row = 32 } } ([\"Json\",\"Decode\"],\"Decoder\")) [Node { end = { column = 58, row = 32 }, start = { column = 57, row = 32 } } (GenericType \"a\")]))) }) }),Node { end = { column = 25, row = 49 }, start = { column = 1, row = 44 } } (FunctionDeclaration { declaration = Node { end = { column = 25, row = 49 }, start = { column = 1, row = 44 } } { arguments = [Node { end = { column = 35, row = 44 }, start = { column = 22, row = 44 } } (VarPattern \"valueToEncode\")], expression = Node { end = { column = 25, row = 49 }, start = { column = 5, row = 45 } } (Application [Node { end = { column = 29, row = 45 }, start = { column = 5, row = 45 } } (FunctionOrValue [] \"jsonEncode__generic_Dict\"),Node { end = { column = 69, row = 45 }, start = { column = 30, row = 45 } } (ParenthesizedExpression (Node { end = { column = 68, row = 45 }, start = { column = 31, row = 45 } } (LambdaExpression { args = [Node { end = { column = 40, row = 45 }, start = { column = 32, row = 45 } } (VarPattern \"type_arg\")], expression = Node { end = { column = 68, row = 45 }, start = { column = 44, row = 45 } } (Application [Node { end = { column = 59, row = 45 }, start = { column = 44, row = 45 } } (FunctionOrValue [\"Json\",\"Encode\"] \"int\"),Node { end = { column = 68, row = 45 }, start = { column = 60, row = 45 } } (FunctionOrValue [] \"type_arg\")]) }))),Node { end = { column = 11, row = 49 }, start = { column = 70, row = 45 } } (ParenthesizedExpression (Node { end = { column = 10, row = 49 }, start = { column = 71, row = 45 } } (LambdaExpression { args = [Node { end = { column = 80, row = 45 }, start = { column = 72, row = 45 } } (VarPattern \"type_arg\")], expression = Node { end = { column = 10, row = 49 }, start = { column = 84, row = 45 } } (Application [Node { end = { column = 102, row = 45 }, start = { column = 84, row = 45 } } (FunctionOrValue [\"Json\",\"Encode\"] \"object\"),Node { end = { column = 10, row = 49 }, start = { column = 9, row = 46 } } (ListExpr [Node { end = { column = 14, row = 48 }, start = { column = 11, row = 46 } } (TupledExpression [Node { end = { column = 21, row = 46 }, start = { column = 13, row = 46 } } (Literal \"length\"),Node { end = { column = 46, row = 47 }, start = { column = 15, row = 47 } } (Application [Node { end = { column = 30, row = 47 }, start = { column = 15, row = 47 } } (FunctionOrValue [\"Json\",\"Encode\"] \"int\"),Node { end = { column = 46, row = 47 }, start = { column = 31, row = 47 } } (RecordAccess (Node { end = { column = 39, row = 47 }, start = { column = 31, row = 47 } } (FunctionOrValue [] \"type_arg\")) (Node { end = { column = 46, row = 47 }, start = { column = 40, row = 47 } } \"length\"))])])])]) }))),Node { end = { column = 25, row = 49 }, start = { column = 12, row = 49 } } (FunctionOrValue [] \"valueToEncode\")]), name = Node { end = { column = 21, row = 44 }, start = { column = 1, row = 44 } } \"jsonEncode_119717524\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 11, row = 58 }, start = { column = 1, row = 52 } } (FunctionDeclaration { declaration = Node { end = { column = 11, row = 58 }, start = { column = 1, row = 52 } } { arguments = [], expression = Node { end = { column = 11, row = 58 }, start = { column = 5, row = 53 } } (Application [Node { end = { column = 29, row = 53 }, start = { column = 5, row = 53 } } (FunctionOrValue [] \"jsonDecode__generic_Dict\"),Node { end = { column = 45, row = 53 }, start = { column = 30, row = 53 } } (FunctionOrValue [\"Json\",\"Decode\"] \"int\"),Node { end = { column = 11, row = 58 }, start = { column = 46, row = 53 } } (ParenthesizedExpression (Node { end = { column = 10, row = 58 }, start = { column = 47, row = 53 } } (OperatorApplication \"|>\" Left (Node { end = { column = 99, row = 53 }, start = { column = 47, row = 53 } } (Application [Node { end = { column = 66, row = 53 }, start = { column = 47, row = 53 } } (FunctionOrValue [\"Json\",\"Decode\"] \"succeed\"),Node { end = { column = 99, row = 53 }, start = { column = 67, row = 53 } } (ParenthesizedExpression (Node { end = { column = 98, row = 53 }, start = { column = 68, row = 53 } } (LambdaExpression { args = [Node { end = { column = 75, row = 53 }, start = { column = 69, row = 53 } } (VarPattern \"length\")], expression = Node { end = { column = 98, row = 53 }, start = { column = 79, row = 53 } } (RecordExpr [Node { end = { column = 96, row = 53 }, start = { column = 81, row = 53 } } [Node { end = { column = 87, row = 53 }, start = { column = 81, row = 53 } } \"length\",Node { end = { column = 96, row = 53 }, start = { column = 90, row = 53 } } (FunctionOrValue [] \"length\")]]) })))])) (Node { end = { column = 10, row = 58 }, start = { column = 8, row = 54 } } (Application [Node { end = { column = 25, row = 54 }, start = { column = 8, row = 54 } } (FunctionOrValue [] \"jsonDecode_andMap\"),Node { end = { column = 10, row = 58 }, start = { column = 9, row = 55 } } (ParenthesizedExpression (Node { end = { column = 28, row = 57 }, start = { column = 11, row = 55 } } (Application [Node { end = { column = 46, row = 55 }, start = { column = 11, row = 55 } } (FunctionOrValue [] \"jsonDecode_field_withAlternateNames\"),Node { end = { column = 55, row = 55 }, start = { column = 47, row = 55 } } (Literal \"length\"),Node { end = { column = 25, row = 56 }, start = { column = 13, row = 56 } } (ListExpr [Node { end = { column = 23, row = 56 }, start = { column = 15, row = 56 } } (Literal \"Length\")]),Node { end = { column = 28, row = 57 }, start = { column = 13, row = 57 } } (FunctionOrValue [\"Json\",\"Decode\"] \"int\")])))])))))]), name = Node { end = { column = 21, row = 52 }, start = { column = 1, row = 52 } } \"jsonDecode_119717524\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 33, row = 63 }, start = { column = 1, row = 61 } } (FunctionDeclaration { declaration = Node { end = { column = 33, row = 63 }, start = { column = 1, row = 61 } } { arguments = [Node { end = { column = 28, row = 61 }, start = { column = 21, row = 61 } } (VarPattern \"encodeA\"),Node { end = { column = 36, row = 61 }, start = { column = 29, row = 61 } } (VarPattern \"encodeB\"),Node { end = { column = 45, row = 61 }, start = { column = 37, row = 61 } } (TuplePattern [Node { end = { column = 40, row = 61 }, start = { column = 39, row = 61 } } (VarPattern \"a\"),Node { end = { column = 43, row = 61 }, start = { column = 42, row = 61 } } (VarPattern \"b\")])], expression = Node { end = { column = 33, row = 63 }, start = { column = 5, row = 62 } } (OperatorApplication \"|>\" Left (Node { end = { column = 35, row = 62 }, start = { column = 5, row = 62 } } (ListExpr [Node { end = { column = 19, row = 62 }, start = { column = 7, row = 62 } } (OperatorApplication \"|>\" Left (Node { end = { column = 8, row = 62 }, start = { column = 7, row = 62 } } (FunctionOrValue [] \"a\")) (Node { end = { column = 19, row = 62 }, start = { column = 12, row = 62 } } (FunctionOrValue [] \"encodeA\"))),Node { end = { column = 33, row = 62 }, start = { column = 21, row = 62 } } (OperatorApplication \"|>\" Left (Node { end = { column = 22, row = 62 }, start = { column = 21, row = 62 } } (FunctionOrValue [] \"b\")) (Node { end = { column = 33, row = 62 }, start = { column = 26, row = 62 } } (FunctionOrValue [] \"encodeB\")))])) (Node { end = { column = 33, row = 63 }, start = { column = 8, row = 63 } } (Application [Node { end = { column = 24, row = 63 }, start = { column = 8, row = 63 } } (FunctionOrValue [\"Json\",\"Encode\"] \"list\"),Node { end = { column = 33, row = 63 }, start = { column = 25, row = 63 } } (FunctionOrValue [] \"identity\")]))), name = Node { end = { column = 20, row = 61 }, start = { column = 1, row = 61 } } \"jsonEncode__tuple_2\" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 26, row = 68 }, start = { column = 1, row = 66 } } (FunctionDeclaration { declaration = Node { end = { column = 26, row = 68 }, start = { column = 1, row = 67 } } { arguments = [], expression = Node { end = { column = 26, row = 68 }, start = { column = 5, row = 68 } } (Application [Node { end = { column = 21, row = 68 }, start = { column = 5, row = 68 } } (FunctionOrValue [\"Json\",\"Decode\"] \"map2\"),Node { end = { column = 26, row = 68 }, start = { column = 22, row = 68 } } (PrefixOperator \"|>\")]), name = Node { end = { column = 18, row = 67 }, start = { column = 1, row = 67 } } \"jsonDecode_andMap\" }, documentation = Nothing, signature = Just (Node { end = { column = 99, row = 66 }, start = { column = 1, row = 66 } } { name = Node { end = { column = 18, row = 66 }, start = { column = 1, row = 66 } } \"jsonDecode_andMap\", typeAnnotation = Node { end = { column = 99, row = 66 }, start = { column = 21, row = 66 } } (FunctionTypeAnnotation (Node { end = { column = 42, row = 66 }, start = { column = 21, row = 66 } } (Typed (Node { end = { column = 40, row = 66 }, start = { column = 21, row = 66 } } ([\"Json\",\"Decode\"],\"Decoder\")) [Node { end = { column = 42, row = 66 }, start = { column = 41, row = 66 } } (GenericType \"a\")])) (Node { end = { column = 99, row = 66 }, start = { column = 46, row = 66 } } (FunctionTypeAnnotation (Node { end = { column = 74, row = 66 }, start = { column = 46, row = 66 } } (Typed (Node { end = { column = 65, row = 66 }, start = { column = 46, row = 66 } } ([\"Json\",\"Decode\"],\"Decoder\")) [Node { end = { column = 74, row = 66 }, start = { column = 66, row = 66 } } (FunctionTypeAnnotation (Node { end = { column = 68, row = 66 }, start = { column = 67, row = 66 } } (GenericType \"a\")) (Node { end = { column = 73, row = 66 }, start = { column = 72, row = 66 } } (GenericType \"b\")))])) (Node { end = { column = 99, row = 66 }, start = { column = 78, row = 66 } } (Typed (Node { end = { column = 97, row = 66 }, start = { column = 78, row = 66 } } ([\"Json\",\"Decode\"],\"Decoder\")) [Node { end = { column = 99, row = 66 }, start = { column = 98, row = 66 } } (GenericType \"b\")]))))) }) })], imports = [Node { end = { column = 20, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 20, row = 4 }, start = { column = 8, row = 4 } } [\"Bytes\",\"Encode\"] },Node { end = { column = 42, row = 5 }, start = { column = 1, row = 5 } } { exposingList = Nothing, moduleAlias = Just (Node { end = { column = 42, row = 5 }, start = { column = 36, row = 5 } } [\"Base64\"]), moduleName = Node { end = { column = 32, row = 5 }, start = { column = 8, row = 5 } } [\"CompilerGenerated\",\"Base64\"] },Node { end = { column = 30, row = 6 }, start = { column = 1, row = 6 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 30, row = 6 }, start = { column = 8, row = 6 } } [\"WorkspaceState_2021_01\"] }], moduleDefinition = Node { end = { column = 90, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 90, row = 1 }, start = { column = 77, row = 1 } } (All { end = { column = 89, row = 1 }, start = { column = 87, row = 1 } }), moduleName = Node { end = { column = 76, row = 1 }, start = { column = 8, row = 1 } } [\"CompilationInterface\",\"GenerateJsonConverters\",\"Generated_JsonConverters\"] }) }"
            ];

        TestParsingModuleText(
            elmModuleText,
            expectedExpressionStringChunks,
            alsoTestDotnetParser: true);
    }


    [TestMethod]
    public void Dotnet_parser_results_in_same_expression_as_Elm_parser()
    {
        IReadOnlyList<string> testCases =
            [
                """""
                module Operations exposing (..)


                operations_0 a b =
                    a * (b + 1)

                operations_1 a b =
                    a * (b - 1)
                
                operations_2 a b =
                    a + (b // 1)


                operations_3 a b c f =
                    (a + b) * (c + d)


                operations_4 a b c f =
                    a + b * c + d
                                 
                """"",

                """"
                module Pizza exposing (..)


                test_0 a =
                    a
                    |> f1
                    |> f2
                    |> f3


                test_1 func =
                    func
                    |. a
                    |. b
                    |= c
                    |. d
                    |. e

                """",

                """"
                module TypeDecls exposing (..)


                type alias ResultInstance =
                    Result String Int

                
                {-| Comment
                -}
                type alias MaybeInstance =
                    Maybe Int
                
                
                """",

                """"
                module Basics exposing
                    ( (&&)
                    , (*)
                    , (+)
                    , (++)
                    , (-)
                    , (/)
                    , (//)
                    , (/=)
                    , (<)
                    , (<<)
                    , (<=)
                    , (<|)
                    , (==)
                    , (>)
                    , (>=)
                    , (>>)
                    , (^)
                    , (|>)
                    , (||)
                    , Bool(..)
                    , Float
                    , Int
                    , Never
                    , Order(..)
                    , abs
                    , acos
                    , always
                    , asin
                    , atan
                    , atan2
                    , ceiling
                    , clamp
                    , compare
                    , cos
                    , degrees
                    , e
                    , floor
                    , fromPolar
                    , identity
                    , isInfinite
                    , isNaN
                    , logBase
                    , max
                    , min
                    , modBy
                    , negate
                    , never
                    , not
                    , pi
                    , radians
                    , remainderBy
                    , round
                    , sin
                    , sqrt
                    , tan
                    , toFloat
                    , toPolar
                    , truncate
                    , turns
                    , xor
                    )


                """",

                """"
                module Basics exposing ( .. )


                infix right 0 (<|) = apL
                infix left  0 (|>) = apR
                infix right 2 (||) = or
                infix right 3 (&&) = and
                infix non   4 (==) = eq
                infix non   4 (/=) = neq
                infix non   4 (<) = lt
                infix non   4 (>) = gt
                infix non   4 (<=) = le
                infix non   4 (>=) = ge
                infix right 5 (++) = append
                infix left  6 (+) = add
                infix left  6 (-) = sub
                infix left  7 (*) = mul
                infix left  7 (//) = idiv
                infix right 8 (^) = pow
                infix left  9 (<<) = composeL
                infix right 9 (>>) = composeR


                """",

                """"
                module Basics exposing ( .. )

                {-| Testing comment

                -}

                type Bool
                    = True
                    | False


                type String
                    = String (List Char.Char)
                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                    | AnyOtherKind_String


                type Elm_Float
                    = Elm_Float Int Int
                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                    | AnyOtherKind_Float


                {-| Represents the relative ordering of two things.
                The relations are less than, equal to, and greater than.
                -}
                type Order
                    = LT
                    | EQ
                    | GT


                """",

                """"
                module Basics exposing (..)


                eq : a -> a -> Bool
                eq a b =
                    if Pine_kernel.equal [ a, b ] then
                        True

                    else
                        case ( a, b ) of
                            ( Elm_Float numA denomA, intB ) ->
                                if Pine_kernel.equal [ numA, intB ] then
                                    Pine_kernel.equal [ denomA, 1 ]

                                else
                                    False

                            ( intA, Elm_Float numB denomB ) ->
                                if Pine_kernel.equal [ intA, numB ] then
                                    Pine_kernel.equal [ denomB, 1 ]

                                else
                                    False

                            _ ->
                                if isPineBlob a then
                                    False

                                else if Pine_kernel.equal [ Pine_kernel.length a, Pine_kernel.length b ] then
                                    case a of
                                        String _ ->
                                            False

                                        RBNode_elm_builtin _ _ _ _ _ ->
                                            Pine_kernel.equal [ dictToList a, dictToList b ]

                                        Set_elm_builtin dictA ->
                                            let
                                                (Set_elm_builtin dictB) =
                                                    b
                                            in
                                            Pine_kernel.equal [ dictKeys dictA, dictKeys dictB ]

                                        _ ->
                                            listsEqualRecursive a b

                                else
                                    False


                listsEqualRecursive : List comparable -> List comparable -> Bool
                listsEqualRecursive listA listB =
                    if Pine_kernel.equal [ listA, [] ] then
                        True

                    else if eq (Pine_kernel.head listA) (Pine_kernel.head listB) then
                        listsEqualRecursive
                            (Pine_kernel.skip [ 1, listA ])
                            (Pine_kernel.skip [ 1, listB ])

                    else
                        False


                dictToList : Dict k v -> List ( k, v )
                dictToList dict =
                    case dict of
                        RBEmpty_elm_builtin ->
                            []

                        RBNode_elm_builtin _ key value left right ->
                            Pine_kernel.concat [ dictToList left, [ ( key, value ) ], dictToList right ]


                dictKeys : Dict k v -> List k
                dictKeys dict =
                    case dict of
                        RBEmpty_elm_builtin ->
                            []

                        RBNode_elm_builtin _ key value left right ->
                            Pine_kernel.concat [ dictKeys left, [ key ], dictKeys right ]


                neq : a -> a -> Bool
                neq a b =
                    not (eq a b)


                add : number -> number -> number
                add a b =
                    Pine_kernel.int_add [ a, b ]


                """",

                """"
                module Basics exposing (..)


                mul : number -> number -> number
                mul a b =
                    case ( a, b ) of
                        ( Elm_Float numA denomA, Elm_Float numB denomB ) ->
                            let
                                newNumerator =
                                    Pine_kernel.int_mul [ numA, numB ]

                                newDenominator =
                                    Pine_kernel.int_mul [ denomA, denomB ]
                            in
                            simplifyFraction (Elm_Float newNumerator newDenominator)

                        ( Elm_Float numA denomA, intB ) ->
                            let
                                newNumerator =
                                    Pine_kernel.int_mul [ numA, intB ]
                            in
                            simplifyFraction (Elm_Float newNumerator denomA)

                        ( intA, Elm_Float numB denomB ) ->
                            let
                                newNumerator =
                                    Pine_kernel.int_mul [ intA, numB ]
                            in
                            simplifyFraction (Elm_Float newNumerator denomB)

                        _ ->
                            Pine_kernel.int_mul [ a, b ]

                """",

                """"
                module Basics exposing (..)


                type alias Buffer = Int


                {-| Find the smaller of two comparables.

                    min 42 12345678 == 42

                    min "abc" "xyz" == "abc"

                -}
                min : comparable -> comparable -> comparable
                min x y =
                    if lt x y then
                        x

                    else
                        y
                

                """",

                """"
                module Basics exposing (..)


                type alias WithGenerics a beta =
                    Int


                param_0 () = 17

                """",

                """"
                module Test exposing (..)


                decl_alfa : Int -> (Int, Int)
                decl_alfa a =
                    ( a, a + 1 )

                decl_beta : Int -> Int -> ( Int, Int, Int )
                decl_beta a b =
                    ( a, a + b, a * b )

                """",

                """"
                module Basics exposing (..)


                type alias MinType = {}

                {- Beginning comment, {- nested comment -}
                -- test
                Continuing comment -}

                """",

                """"
                module Basics exposing (..)


                decl = """ test """

                decl2 = """
                
                test2 """

                """",

                """""
                module Encodings exposing (..)

                
                escapeChar : Char -> String
                escapeChar char =
                    case char of
                        '\u{0008}' ->
                            "\\b"

                        '\t' ->
                            "\\t"

                        '\n' ->
                            "\\n"

                        '\u{000C}' ->
                            "\\f"

                        '\u{000D}' ->
                            "\\r"

                        '"' ->
                            "\\\""

                        '\\' ->
                            "\\\\"

                        _ ->
                            String.fromChar char


                string_alfa =
                    "\u{0008}\t\n\u{000C}\u{000D}\"\\"
                
                string_beta =
                    """\u{0008}\t\n\u{000C}\u{000D}\"\\"""


                decl_10 = '\u{0000}'
                
                decl_11 = '\u{0001}'
                
                                                
                """"",

                """""
                module Encodings exposing (..)


                intAlfa =
                    0x4

                intBeta =
                    0x03E8

                intGamma =
                    -0x13

                intDelta =
                    0x0000000100000000
                
                intDelta =
                    0x0000000100000000

                intEpsilon =
                    0x1713000100000041

                """"",

                """""
                module OperatorSpecialCase exposing (..)


                negate a =
                    -a

                combine alfa beta =
                    beta * -alfa

                negate_tag alfa = Just -alfa

                """"",

                """""
                module Records exposing (..)


                simple: { alfa : String }
                simple =
                    { alfa = "alfa" }


                twoFields : { alfa : String, beta : Int }
                twoFields =
                    { alfa = "alfa", beta = 17 }


                varyingSpacing : { alfa :   String  ,  beta   : Int     }
                varyingSpacing =
                    {  alfa = "alfa" ,   beta  =   17  }
                
                """"",

                """""
                module Records exposing (..)


                simpleGeneric : { a | name : String} -> String
                simpleGeneric =
                    .name


                recordAccessApplied =
                    Tuple.mapSecond .description

                
                """"",

                """""
                module Records exposing (..)


                simpleNested : { alfa : { beta : String }, gamma : Int   } -> String
                simpleNested record =
                    record.alfa.beta

                nested_3 : { alfa : { beta_1 : { delta_3 : String } }, gamma : Int   } -> String
                nested_3 record =
                    record.alfa.beta_1.delta_3
                
                """"",

                """"
                module Records exposing (..)


                updateRecord_0 record =
                    { record
                        | field1 = 13
                    }
                

                updateRecord_1 record =
                    { record
                        | field1 = 17
                        , field2 = 21
                    }
                
                """",

                """"
                module Frontend.ContainerHtml exposing (..)
                

                type Message
                    = ClickedLinkInPreview { href : String }

                """",

                """"
                module Basics exposing (..)


                idiv : Int -> Int -> Int
                idiv dividend divisor =
                    if Pine_kernel.equal [ divisor, 0 ] then
                        0

                    else
                        let
                            ( dividendNegative, absDividend ) =
                                if Pine_kernel.int_is_sorted_asc [ 0, dividend ] then
                                    ( False, dividend )

                                else
                                    ( True, -dividend )

                            ( divisorNegative, absDivisor ) =
                                if Pine_kernel.int_is_sorted_asc [ 0, divisor ] then
                                    ( False, divisor )

                                else
                                    ( True, -divisor )

                            absQuotient =
                                idivHelper absDividend absDivisor 0
                        in
                        if Pine_kernel.equal [ dividendNegative, divisorNegative ] then
                            absQuotient

                        else
                            -absQuotient


                """",

                """""
                module List exposing (..)


                any : (a -> Bool) -> List a -> Bool
                any isOkay list =
                    case list of
                        [] ->
                            False

                        x :: xs ->
                            if isOkay x then
                                True

                            else
                                any isOkay xs
                

                repeatHelp : List a -> Int -> a -> List a
                repeatHelp result n value =
                    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
                        result

                    else
                        repeatHelp (cons value result) (n - 1) value
                

                """"",

                """""
                module String exposing (..)


                fromIntAsList : Int -> List Char
                fromIntAsList int =
                    if Pine_kernel.int_is_sorted_asc [ 0, int ] then
                        fromUnsignedIntAsList int

                    else
                        Pine_kernel.concat [ [ '-' ], fromUnsignedIntAsList -int ]
                

                right : Int -> String -> String
                right n string =
                    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
                        ""

                    else
                        slice -n (length string) string

                """"",

                """"
                module Dict exposing (..)

                import Basics
                import List exposing (..)
                import Maybe exposing (..)


                type NColor
                    = Red
                    | Black


                {-| A dictionary of keys and values. So a `Dict String User` is a dictionary
                that lets you look up a `String` (such as user names) and find the associated
                `User`.

                    import Dict exposing (Dict)

                    users : Dict String User
                    users =
                        Dict.fromList
                            [ ( "Alice", User "Alice" 28 1.65 )
                            , ( "Bob", User "Bob" 19 1.82 )
                            , ( "Chuck", User "Chuck" 33 1.75 )
                            ]

                    type alias User =
                        { name : String
                        , age : Int
                        , height : Float
                        }

                -}
                type Dict k v
                    = RBNode_elm_builtin NColor k v (Dict k v) (Dict k v)
                    | RBEmpty_elm_builtin


                {-| Insert a key-value pair into a dictionary. Replaces value when there is
                a collision.
                -}
                insert : comparable -> v -> Dict comparable v -> Dict comparable v
                insert key value dict =
                    -- Root node is always Black
                    case insertHelp key value dict of
                        RBNode_elm_builtin Red k v l r ->
                            RBNode_elm_builtin Black k v l r

                        x ->
                            x
                
                
                """",


                """"
                module Set exposing                
                    ( Set
                    , empty, singleton, insert, remove
                    , isEmpty, member, size
                    , union, intersect, diff
                    , toList, fromList
                    , map, foldl, foldr, filter, partition
                    )

                {-| A set of unique values. The values can be any comparable type. This
                includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
                of comparable types.

                Insert, remove, and query operations all take _O(log n)_ time.


                # Sets

                @docs Set


                # Build

                @docs empty, singleton, insert, remove


                # Query

                @docs isEmpty, member, size


                # Combine

                @docs union, intersect, diff


                # Lists

                @docs toList, fromList


                # Transform

                @docs map, foldl, foldr, filter, partition

                -}

                import Basics exposing (Bool, Int)
                import Dict
                import List exposing ((::))
                import Maybe exposing (Maybe(..))


                {-| Represents a set of unique values. So `(Set Int)` is a set of integers and
                `(Set String)` is a set of strings.
                -}
                type Set t
                    = Set_elm_builtin (Dict.Dict t ())

                                
                {-| Create a set with one value.
                -}
                singleton : comparable -> Set comparable
                singleton key =
                    Set_elm_builtin (Dict.singleton key ())

                
                """",

                """""
                module Parser.Advanced exposing
                    ( Parser, run, DeadEnd, inContext, Token(..)
                    , int, float, number, symbol, keyword, variable, end
                    , succeed, (|=), (|.), lazy, andThen, problem
                    , oneOf, map, backtrackable, commit, token
                    , sequence, Trailing(..), loop, Step(..)
                    , spaces, lineComment, multiComment, Nestable(..)
                    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
                    , withIndent, getIndent
                    , getPosition, getRow, getCol, getOffset, getSource
                    , changeIndent, ignorer, keeper
                    )


                oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> PStep c x a
                oneOfHelp s0 bag parsers =
                    case parsers of
                        [] ->
                            Bad False bag

                        (Parser parse) :: remainingParsers ->
                            case parse s0 of
                                (Good _ _ _) as step ->
                                    step

                                (Bad p x) as step ->
                                    if p then
                                        step

                                    else
                                        oneOfHelp s0 (Append bag x) remainingParsers
                
                
                """"",

                """""
                module Json.Decode exposing
                    ( Decoder
                    , Error(..)
                    , Value
                    , andThen
                    , array
                    , at
                    , bool
                    , decodeString
                    , decodeValue
                    , dict
                    , errorToString
                    , fail
                    , field
                    , float
                    , index
                    , int
                    , keyValuePairs
                    , lazy
                    , list
                    , map
                    , map2
                    , maybe
                    , null
                    , nullable
                    , oneOf
                    , oneOrMore
                    , string
                    , succeed
                    , value
                    )

                import Array
                import Json.Encode exposing (Value(..))
                

                parseInt : List Char -> Int -> ( Result String Int, Int )
                parseInt src offset0 =
                    let
                        nextChar =
                            List.take 1 (List.drop offset0 src)
                    in
                    case nextChar of
                        [ '-' ] ->
                            -- If we see a minus sign, parse the rest as an unsigned integer
                            let
                                ( unsignedResult, offset1 ) =
                                    parseUnsignedInt src (offset0 + 1)
                            in
                            case unsignedResult of
                                Ok unsignedVal ->
                                    ( Ok -unsignedVal, offset1 )

                                Err err ->
                                    ( Err err, offset1 )

                        _ ->
                            -- If no minus sign, parse the rest as an unsigned integer
                            parseUnsignedInt src offset0
                
                
                
                """"",

                """"
                module Bytes.Decode exposing (..)

                unsignedInt8 : Decoder Int
                unsignedInt8 =
                    Decoder
                        (\(Bytes.Elm_Bytes blob) offset ->
                            let
                                byte =
                                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]
                            in
                            ( Pine_kernel.int_add [ offset, 1 ]
                            , Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte ]
                            )
                        )
                
                """",

                """"
                module Url.Parser exposing
                    ( (</>)
                    , (<?>)
                    , Parser
                    , custom
                    , fragment
                    , int
                    , map
                    , oneOf
                    , parse
                    , query
                    , s
                    , string
                    , top
                    )

                import Dict exposing (Dict)
                import Url exposing (Url)
                import Url.Parser.Internal as Q
                import Url.Parser.Query as Query


                infix right 7 (</>) = slash
                infix left  8 (<?>) = questionMark


                type Parser a b
                    = Parser (State a -> List (State b))


                s : String -> Parser a a
                s str =
                    Parser <|
                        \{ visited, unvisited, params, frag, value } ->
                            case unvisited of
                                [] ->
                                    []

                                next :: rest ->
                                    if next == str then
                                        [ State (next :: visited) rest params frag value ]

                                    else
                                        []
                
                
                """",

                """"
                module Encode exposing (encoder, toBytes)
                

                import Bitwise
                import Bytes exposing (Bytes, Endianness(..))
                import Bytes.Encode as Encode exposing (Encoder)


                encodeChunks : String -> List Encoder -> Maybe (List Encoder)
                encodeChunks input accum =
                    case String.toList (String.left 4 input) of
                        [] ->
                            Just accum

                        [ a, b, c, d ] ->
                            case encodeCharacters a b c d of
                                Just enc ->
                                    encodeChunks (String.dropLeft 4 input) (enc :: accum)

                                Nothing ->
                                    Nothing

                        [ a, b, c ] ->
                            case encodeCharacters a b c '=' of
                                Nothing ->
                                    Nothing

                                Just enc ->
                                    Just (enc :: accum)

                        [ a, b ] ->
                            case encodeCharacters a b '=' '=' of
                                Nothing ->
                                    Nothing

                                Just enc ->
                                    Just (enc :: accum)

                        _ ->
                            Nothing

                
                """",

                """""
                module ElmCompiler exposing (..)
                

                typeCannotContainSetOrDict :
                    List ( String, Elm.Syntax.TypeAnnotation.TypeAnnotation )
                    -> Elm.Syntax.TypeAnnotation.TypeAnnotation
                    -> Bool
                typeCannotContainSetOrDict knownTypes typeAnnotation =
                    case typeAnnotation of
                        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( [], typeName )) typeArgs ->
                            case ( typeName, typeArgs ) of
                                ( "Int", [] ) ->
                                    True

                                ( "String", [] ) ->
                                    True

                                ( "Char", [] ) ->
                                    True

                                ( "List", [ Elm.Syntax.Node.Node _ itemType ] ) ->
                                    typeCannotContainSetOrDict knownTypes itemType

                                _ ->
                                    True

                        _ ->
                            False
                
                
                """"",

                """""
                module Backend.Generated.StateShimTypes exposing (..)

                import Json.Encode


                type alias ResponseOverSerialInterface =
                    Result String StateShimResponse


                type StateShimRequest
                    = ListExposedFunctionsShimRequest


                type StateShimResponse
                    = ListExposedFunctionsShimResponse (List { functionName : String, functionDescription : ExposedFunctionDescription })


                type alias ApplyFunctionArguments state =
                    { stateArgument : state
                    , serializedArgumentsJson : List Json.Encode.Value
                    }


                type alias ExposedFunctionDescription =
                    { returnType : ExposedFunctionReturnTypeDescription
                    , parameters : List ExposedFunctionParameterDescription
                    }


                type alias ExposedFunctionReturnTypeDescription =
                    { sourceCodeText : String
                    , containsAppStateType : Bool
                    }


                type alias ExposedFunctionParameterDescription =
                    { patternSourceCodeText : String
                    , typeSourceCodeText : String
                    , typeIsAppStateType : Bool
                    }


                type alias FunctionApplicationResult =
                    { resultLessStateJson : Maybe Json.Encode.Value
                    , producedStateDifferentFromStateArgument : Bool
                    }
                
                
                """"",

                """"
                module CompilationInterface.SourceFiles.Generated_SourceFiles exposing (..)


                type FileTreeNode blobStructure
                    = BlobNode blobStructure
                    | TreeNode (List ( String, FileTreeNode blobStructure ))


                file__src_Backend_VolatileProcess_csx =
                    { utf8 = "#r \"netstandard\"\n#r \"System\"\n#r \"System.Collections.Immutable\"\n#r \"System.Net\"----truncated" }

                file_tree_node_elm_core_modules_explicit_import =
                    TreeNode
                        [( "Array.elm"
                        , BlobNode ({ utf8 = "module Array\n    exposing\n        ----truncated" })
                        )
                        ,( "Bitwise.elm"
                        , BlobNode ({ utf8 = "module Bitwise exposing\n  ----truncated" })
                        )]

                
                """",

                """""
                {- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module> -}


                module CompilationInterface.SourceFiles exposing (..)
                import CompilerGenerated.EncodeBytes as EncodeBytes
                import CompilationInterface.SourceFiles.Generated_SourceFiles


                type FileTreeNode blobStructure
                    = BlobNode blobStructure
                    | TreeNode (List ( String, FileTreeNode blobStructure ))


                file_tree____static : FileTreeNode { base64 : String }
                file_tree____static =
                    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_static
                    |>mapFileTreeNodeFromGenerated
                    |>mapBlobs(\blobValue -> { base64 = blobValue.base64
                    })


                file____src_Backend_VolatileProcess_csx : { utf8 : String }
                file____src_Backend_VolatileProcess_csx =
                    { utf8 = CompilationInterface.SourceFiles.Generated_SourceFiles.file__src_Backend_VolatileProcess_csx.utf8
                    }


                file_tree____elm_core_modules_implicit_import : FileTreeNode { utf8 : String }
                file_tree____elm_core_modules_implicit_import =
                    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_core_modules_implicit_import
                    |>mapFileTreeNodeFromGenerated
                    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
                    })


                file_tree____elm_core_modules_explicit_import : FileTreeNode { utf8 : String }
                file_tree____elm_core_modules_explicit_import =
                    CompilationInterface.SourceFiles.Generated_SourceFiles.file_tree_node_elm_core_modules_explicit_import
                    |>mapFileTreeNodeFromGenerated
                    |>mapBlobs(\blobValue -> { utf8 = blobValue.utf8
                    })
                
                """"",


                """""
                module CompilationInterface.SourceFiles exposing (..)

                import CompilerGenerated.EncodeBytes as EncodeBytes
                import CompilationInterface.SourceFiles.Generated_SourceFiles

                {-| For documentation
                -}


                type FileTreeNode blobStructure
                    = BlobNode blobStructure
                    | TreeNode (List ( String, FileTreeNode blobStructure ))


                """"",
            ];

        for (int i = 0; i < testCases.Count; ++i)
        {
            var testCase = testCases[i].TrimStart();

            try
            {
                var parsedModulePineValue =
                    ParseElmModuleTextToPineValue(testCase)
                    .Extract(err => throw new Exception(err));

                var responseAsElmValue =
                    elmCompilerCache.PineValueDecodedAsElmValue(parsedModulePineValue)
                    .Extract(err => throw new Exception(err));

                var responseAsExpression =
                    ElmValue.RenderAsElmExpression(responseAsElmValue).expressionString;

                var fromDotnetResult =
                    Pine.ElmSyntax.ElmSyntaxParser.ParseModuleTextAsElmSyntaxElmValue(testCase);

                if (fromDotnetResult.IsErrOrNull() is { } err)
                {
                    Assert.Fail("Failed to parse Elm module text as Elm syntax: " + err);
                }

                if (fromDotnetResult.IsOkOrNull() is not { } fromDotnetOk)
                {
                    throw new Exception("Unexpected result type: " + fromDotnetResult);
                }

                var fromDotnetExpression =
                    ElmValue.RenderAsElmExpression(fromDotnetOk).expressionString;

                if (Testing.CompareStringsChunkwiseAndReportFirstDifference(
                    expectedChunks: [responseAsExpression],
                    actual: fromDotnetExpression) is { } firstDifference)
                {
                    Console.WriteLine(firstDifference);

                    Assert.Fail(firstDifference);
                }
            }
            catch (Exception e)
            {
                throw new Exception("Failed in test case " + i + ":\n" + testCase, e);
            }
        }
    }

    static readonly ElmCompilerCache elmCompilerCache = new();

    public static ElmValue TestParsingModuleText(
        string elmModuleText,
        IReadOnlyList<string> expectedExpressionStringChunks,
        bool alsoTestDotnetParser)
    {
        var expectedExpressionString =
            string.Concat(expectedExpressionStringChunks);

        var parseClock = System.Diagnostics.Stopwatch.StartNew();

        var parsedModulePineValue =
            ParseElmModuleTextToPineValue(elmModuleText)
            .Extract(err => throw new Exception(err));

        Console.WriteLine(
            "Parsed Elm module text in " +
            CommandLineInterface.FormatIntegerForDisplay(parseClock.ElapsedMilliseconds) +
            " milliseconds");

        var responseAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(parsedModulePineValue)
            .Extract(err => throw new Exception(err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseAsElmValue).expressionString;

        {
            /*
             * Compare chunkwise to make it easier to maintain and adapt tests.
             * */

            if (Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedExpressionStringChunks,
                responseAsExpression) is { } firstDifference)
            {
                Console.WriteLine(firstDifference);

                Assert.Fail(firstDifference);
            }
        }

        Assert.AreEqual(
            expectedExpressionString,
            responseAsExpression,
            "Module parsed as expression syntax");

        if (alsoTestDotnetParser)
        {
            var fromDotnetResult =
                Pine.ElmSyntax.ElmSyntaxParser.ParseModuleTextAsElmSyntaxElmValue(elmModuleText);

            if (fromDotnetResult.IsErrOrNull() is { } err)
            {
                Assert.Fail("Failed to parse Elm module text as Elm syntax: " + err);
            }

            if (fromDotnetResult.IsOkOrNull() is not { } fromDotnetOk)
            {
                throw new Exception("Unexpected result type: " + fromDotnetResult);
            }

            var fromDotnetExpression =
                ElmValue.RenderAsElmExpression(fromDotnetOk).expressionString;

            if (Testing.CompareStringsChunkwiseAndReportFirstDifference(
                expectedExpressionStringChunks,
                fromDotnetExpression) is { } firstDifference)
            {
                Console.WriteLine(firstDifference);

                Assert.Fail(
                    "Dotnet parser produced different expression syntax: " +
                    firstDifference);
            }

        }

        return responseAsElmValue;
    }

    public static Result<string, PineValue> ParseElmModuleTextToPineValue(string elmModuleText)
    {
        var pineVMCache = new Pine.PineVM.PineVMCache();

        var pineVM =
            new Pine.PineVM.PineVM(evalCache: pineVMCache.EvalCache);

        return bundledElmCompiler.Value.ParseElmModuleText(elmModuleText, pineVM);
    }

    private static readonly Lazy<ElmCompiler> bundledElmCompiler =
        new(() =>
        {
            var elmCompilerFromBundle =
                BundledElmEnvironments.BundledElmCompilerCompiledEnvValue();

            Assert.IsNotNull(
                elmCompilerFromBundle,
                message: "Elm compiler environment not found in bundled environments");

            return
            ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundle)
            .Extract(err => throw new Exception(err));
        });
}
