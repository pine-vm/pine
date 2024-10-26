using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Elm;
using Pine.ElmInteractive;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ParseElmModuleTextToPineValueTests
{
    [TestMethod]
    public void Parse_Elm_module_and_encode_as_Pine_value()
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
            
            
            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = []",
            ", declarations = [",
            "Node { end = { column = 14, row = 10 }, start = { column = 1, row = 9 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 9 }, start = { column = 12, row = 9 } } \"MaybeInt\", typeAnnotation = Node { end = { column = 14, row = 10 }, start = { column = 5, row = 10 } } (Typed (Node { end = { column = 10, row = 10 }, start = { column = 5, row = 10 } } ([],\"Maybe\")) [Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } (Typed (Node { end = { column = 14, row = 10 }, start = { column = 11, row = 10 } } ([],\"Int\")) [])]) })",
            ",Node { end = { column = 19, row = 14 }, start = { column = 1, row = 13 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 13 }, start = { column = 12, row = 13 } } \"RecordType\", typeAnnotation = Node { end = { column = 19, row = 14 }, start = { column = 5, row = 14 } } (Record [Node { end = { column = 17, row = 14 }, start = { column = 7, row = 14 } } [Node { end = { column = 11, row = 14 }, start = { column = 7, row = 14 } } \"alfa\",Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } (Typed (Node { end = { column = 17, row = 14 }, start = { column = 14, row = 14 } } ([],\"Int\")) [])]]) })",
            ",Node { end = { column = 22, row = 19 }, start = { column = 1, row = 17 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } { arguments = [], name = Node { end = { column = 18, row = 18 }, start = { column = 7, row = 18 } } \"Choice_Alfa\" },Node { end = { column = 22, row = 19 }, start = { column = 7, row = 19 } } { arguments = [Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 22, row = 19 }, start = { column = 19, row = 19 } } ([],\"Int\")) [])], name = Node { end = { column = 18, row = 19 }, start = { column = 7, row = 19 } } \"Choice_Beta\" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 17 }, start = { column = 6, row = 17 } } \"ChoiceType\" })",
            ",Node { end = { column = 36, row = 24 }, start = { column = 1, row = 22 } } (FunctionDeclaration { declaration = Node { end = { column = 36, row = 24 }, start = { column = 1, row = 23 } } { arguments = [Node { end = { column = 17, row = 23 }, start = { column = 7, row = 23 } } (VarPattern \"param_name\")], expression = Node { end = { column = 36, row = 24 }, start = { column = 5, row = 24 } } (OperatorApplication \"++\" Right (Node { end = { column = 14, row = 24 }, start = { column = 5, row = 24 } } (Literal \"Hello, \")) (Node { end = { column = 36, row = 24 }, start = { column = 18, row = 24 } } (OperatorApplication \"++\" Right (Node { end = { column = 28, row = 24 }, start = { column = 18, row = 24 } } (FunctionOrValue [] \"param_name\")) (Node { end = { column = 36, row = 24 }, start = { column = 32, row = 24 } } (Literal \" 👋\"))))), name = Node { end = { column = 6, row = 23 }, start = { column = 1, row = 23 } } \"greet\" }, documentation = Nothing, signature = Just (Node { end = { column = 25, row = 22 }, start = { column = 1, row = 22 } } { name = Node { end = { column = 6, row = 22 }, start = { column = 1, row = 22 } } \"greet\", typeAnnotation = Node { end = { column = 25, row = 22 }, start = { column = 9, row = 22 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } (Typed (Node { end = { column = 15, row = 22 }, start = { column = 9, row = 22 } } ([],\"String\")) [])) (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } (Typed (Node { end = { column = 25, row = 22 }, start = { column = 19, row = 22 } } ([],\"String\")) []))) }) })",
            ",Node { end = { column = 19, row = 30 }, start = { column = 1, row = 27 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 30, row = 28 }, start = { column = 7, row = 28 } } { arguments = [Node { end = { column = 30, row = 28 }, start = { column = 14, row = 28 } } (Typed (Node { end = { column = 19, row = 28 }, start = { column = 15, row = 28 } } ([],\"List\")) [Node { end = { column = 29, row = 28 }, start = { column = 20, row = 28 } } (Typed (Node { end = { column = 29, row = 28 }, start = { column = 20, row = 28 } } ([\"Char\"],\"Char\")) [])])], name = Node { end = { column = 13, row = 28 }, start = { column = 7, row = 28 } } \"String\" },Node { end = { column = 19, row = 30 }, start = { column = 7, row = 30 } } { arguments = [], name = Node { end = { column = 19, row = 30 }, start = { column = 7, row = 30 } } \"AnyOtherKind\" }], documentation = Nothing, generics = [], name = Node { end = { column = 12, row = 27 }, start = { column = 6, row = 27 } } \"String\" })",
            ",Node { end = { column = 40, row = 42 }, start = { column = 1, row = 33 } } (FunctionDeclaration { declaration = Node { end = { column = 40, row = 42 }, start = { column = 1, row = 36 } } { arguments = [Node { end = { column = 12, row = 36 }, start = { column = 11, row = 36 } } (VarPattern \"a\")], expression = Node { end = { column = 40, row = 42 }, start = { column = 5, row = 37 } } (CaseExpression { cases = [[Node { end = { column = 23, row = 38 }, start = { column = 9, row = 38 } } (NamedPattern { moduleName = [], name = \"String\" } [Node { end = { column = 23, row = 38 }, start = { column = 16, row = 38 } } (VarPattern \"stringA\")]),Node { end = { column = 61, row = 39 }, start = { column = 13, row = 39 } } (Application [Node { end = { column = 19, row = 39 }, start = { column = 13, row = 39 } } (FunctionOrValue [] \"String\"),Node { end = { column = 61, row = 39 }, start = { column = 20, row = 39 } } (ParenthesizedExpression (Node { end = { column = 60, row = 39 }, start = { column = 21, row = 39 } } (Application [Node { end = { column = 39, row = 39 }, start = { column = 21, row = 39 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 60, row = 39 }, start = { column = 40, row = 39 } } (ListExpr [Node { end = { column = 49, row = 39 }, start = { column = 42, row = 39 } } (FunctionOrValue [] \"stringA\"),Node { end = { column = 58, row = 39 }, start = { column = 51, row = 39 } } (FunctionOrValue [] \"stringA\")])])))])],[Node { end = { column = 10, row = 41 }, start = { column = 9, row = 41 } } AllPattern,Node { end = { column = 40, row = 42 }, start = { column = 13, row = 42 } } (Application [Node { end = { column = 31, row = 42 }, start = { column = 13, row = 42 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 40, row = 42 }, start = { column = 32, row = 42 } } (ListExpr [Node { end = { column = 35, row = 42 }, start = { column = 34, row = 42 } } (FunctionOrValue [] \"a\"),Node { end = { column = 38, row = 42 }, start = { column = 37, row = 42 } } (FunctionOrValue [] \"a\")])])]], expression = Node { end = { column = 11, row = 37 }, start = { column = 10, row = 37 } } (FunctionOrValue [] \"a\") }), name = Node { end = { column = 10, row = 36 }, start = { column = 1, row = 36 } } \"replicate\" }, documentation = Just (Node { end = { column = 3, row = 34 }, start = { column = 1, row = 33 } } \"{-| Replicates the given appendable.\n-}\"), signature = Just (Node { end = { column = 37, row = 35 }, start = { column = 1, row = 35 } } { name = Node { end = { column = 10, row = 35 }, start = { column = 1, row = 35 } } \"replicate\", typeAnnotation = Node { end = { column = 37, row = 35 }, start = { column = 13, row = 35 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 35 }, start = { column = 13, row = 35 } } (GenericType \"appendable\")) (Node { end = { column = 37, row = 35 }, start = { column = 27, row = 35 } } (GenericType \"appendable\"))) }) })",
            ",Node { end = { column = 32, row = 73 }, start = { column = 1, row = 45 } } (FunctionDeclaration { declaration = Node { end = { column = 32, row = 73 }, start = { column = 1, row = 46 } } { arguments = [Node { end = { column = 27, row = 46 }, start = { column = 15, row = 46 } } (VarPattern \"stringAsList\")], expression = Node { end = { column = 32, row = 73 }, start = { column = 5, row = 47 } } (CaseExpression { cases = [[Node { end = { column = 11, row = 48 }, start = { column = 9, row = 48 } } (ListPattern []),Node { end = { column = 20, row = 49 }, start = { column = 13, row = 49 } } (FunctionOrValue [] \"Nothing\")],[Node { end = { column = 35, row = 51 }, start = { column = 9, row = 51 } } (UnConsPattern (Node { end = { column = 18, row = 51 }, start = { column = 9, row = 51 } } (VarPattern \"firstChar\")) (Node { end = { column = 35, row = 51 }, start = { column = 22, row = 51 } } (VarPattern \"lessFirstChar\"))),Node { end = { column = 32, row = 73 }, start = { column = 13, row = 52 } } (LetExpression { declarations = [Node { end = { column = 48, row = 62 }, start = { column = 17, row = 53 } } (LetDestructuring (Node { end = { column = 48, row = 53 }, start = { column = 17, row = 53 } } (TuplePattern [Node { end = { column = 30, row = 53 }, start = { column = 19, row = 53 } } (VarPattern \"valueString\"),Node { end = { column = 46, row = 53 }, start = { column = 32, row = 53 } } (VarPattern \"signMultiplier\")])) (Node { end = { column = 48, row = 62 }, start = { column = 21, row = 54 } } (CaseExpression { cases = [[Node { end = { column = 28, row = 55 }, start = { column = 25, row = 55 } } (CharPattern '-'),Node { end = { column = 50, row = 56 }, start = { column = 29, row = 56 } } (TupledExpression [Node { end = { column = 44, row = 56 }, start = { column = 31, row = 56 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 48, row = 56 }, start = { column = 46, row = 56 } } (Negation (Node { end = { column = 48, row = 56 }, start = { column = 47, row = 56 } } (Integer 1)))])],[Node { end = { column = 28, row = 58 }, start = { column = 25, row = 58 } } (CharPattern '+'),Node { end = { column = 49, row = 59 }, start = { column = 29, row = 59 } } (TupledExpression [Node { end = { column = 44, row = 59 }, start = { column = 31, row = 59 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 47, row = 59 }, start = { column = 46, row = 59 } } (Integer 1)])],[Node { end = { column = 26, row = 61 }, start = { column = 25, row = 61 } } AllPattern,Node { end = { column = 48, row = 62 }, start = { column = 29, row = 62 } } (TupledExpression [Node { end = { column = 43, row = 62 }, start = { column = 31, row = 62 } } (FunctionOrValue [] \"stringAsList\"),Node { end = { column = 46, row = 62 }, start = { column = 45, row = 62 } } (Integer 1)])]], expression = Node { end = { column = 35, row = 54 }, start = { column = 26, row = 54 } } (FunctionOrValue [] \"firstChar\") })))], expression = Node { end = { column = 32, row = 73 }, start = { column = 13, row = 64 } } (IfBlock (Node { end = { column = 33, row = 64 }, start = { column = 16, row = 64 } } (OperatorApplication \"==\" Non (Node { end = { column = 27, row = 64 }, start = { column = 16, row = 64 } } (FunctionOrValue [] \"valueString\")) (Node { end = { column = 33, row = 64 }, start = { column = 31, row = 64 } } (ListExpr [])))) (Node { end = { column = 24, row = 65 }, start = { column = 17, row = 65 } } (FunctionOrValue [] \"Nothing\")) (Node { end = { column = 32, row = 73 }, start = { column = 17, row = 68 } } (CaseExpression { cases = [[Node { end = { column = 34, row = 69 }, start = { column = 21, row = 69 } } (NamedPattern { moduleName = [], name = \"Just\" } [Node { end = { column = 34, row = 69 }, start = { column = 26, row = 69 } } (VarPattern \"unsigned\")]),Node { end = { column = 80, row = 70 }, start = { column = 25, row = 70 } } (Application [Node { end = { column = 29, row = 70 }, start = { column = 25, row = 70 } } (FunctionOrValue [] \"Just\"),Node { end = { column = 80, row = 70 }, start = { column = 30, row = 70 } } (ParenthesizedExpression (Node { end = { column = 79, row = 70 }, start = { column = 31, row = 70 } } (Application [Node { end = { column = 50, row = 70 }, start = { column = 31, row = 70 } } (FunctionOrValue [\"Pine_kernel\"] \"int_mul\"),Node { end = { column = 79, row = 70 }, start = { column = 51, row = 70 } } (ListExpr [Node { end = { column = 67, row = 70 }, start = { column = 53, row = 70 } } (FunctionOrValue [] \"signMultiplier\"),Node { end = { column = 77, row = 70 }, start = { column = 69, row = 70 } } (FunctionOrValue [] \"unsigned\")])])))])],[Node { end = { column = 28, row = 72 }, start = { column = 21, row = 72 } } (NamedPattern { moduleName = [], name = \"Nothing\" } []),Node { end = { column = 32, row = 73 }, start = { column = 25, row = 73 } } (FunctionOrValue [] \"Nothing\")]], expression = Node { end = { column = 57, row = 68 }, start = { column = 22, row = 68 } } (Application [Node { end = { column = 43, row = 68 }, start = { column = 22, row = 68 } } (FunctionOrValue [] \"toUnsignedIntFromList\"),Node { end = { column = 45, row = 68 }, start = { column = 44, row = 68 } } (Integer 0),Node { end = { column = 57, row = 68 }, start = { column = 46, row = 68 } } (FunctionOrValue [] \"valueString\")]) }))) })]], expression = Node { end = { column = 22, row = 47 }, start = { column = 10, row = 47 } } (FunctionOrValue [] \"stringAsList\") }), name = Node { end = { column = 14, row = 46 }, start = { column = 1, row = 46 } } \"toIntFromList\" }, documentation = Nothing, signature = Just (Node { end = { column = 39, row = 45 }, start = { column = 1, row = 45 } } { name = Node { end = { column = 14, row = 45 }, start = { column = 1, row = 45 } } \"toIntFromList\", typeAnnotation = Node { end = { column = 39, row = 45 }, start = { column = 17, row = 45 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 45 }, start = { column = 17, row = 45 } } (Typed (Node { end = { column = 21, row = 45 }, start = { column = 17, row = 45 } } ([],\"List\")) [Node { end = { column = 26, row = 45 }, start = { column = 22, row = 45 } } (Typed (Node { end = { column = 26, row = 45 }, start = { column = 22, row = 45 } } ([],\"Char\")) [])])) (Node { end = { column = 39, row = 45 }, start = { column = 30, row = 45 } } (Typed (Node { end = { column = 35, row = 45 }, start = { column = 30, row = 45 } } ([],\"Maybe\")) [Node { end = { column = 39, row = 45 }, start = { column = 36, row = 45 } } (Typed (Node { end = { column = 39, row = 45 }, start = { column = 36, row = 45 } } ([],\"Int\")) [])]))) }) })",
            ",Node { end = { column = 17, row = 106 }, start = { column = 1, row = 76 } } (FunctionDeclaration { declaration = Node { end = { column = 17, row = 106 }, start = { column = 1, row = 77 } } { arguments = [Node { end = { column = 17, row = 77 }, start = { column = 13, row = 77 } } (VarPattern \"dict\")], expression = Node { end = { column = 17, row = 106 }, start = { column = 5, row = 78 } } (CaseExpression { cases = [[Node { end = { column = 171, row = 79 }, start = { column = 9, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 31, row = 79 }, start = { column = 28, row = 79 } } (VarPattern \"clr\"),Node { end = { column = 33, row = 79 }, start = { column = 32, row = 79 } } (VarPattern \"k\"),Node { end = { column = 35, row = 79 }, start = { column = 34, row = 79 } } (VarPattern \"v\"),Node { end = { column = 80, row = 79 }, start = { column = 36, row = 79 } } (ParenthesizedPattern (Node { end = { column = 79, row = 79 }, start = { column = 37, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 60, row = 79 }, start = { column = 56, row = 79 } } (VarPattern \"lClr\"),Node { end = { column = 63, row = 79 }, start = { column = 61, row = 79 } } (VarPattern \"lK\"),Node { end = { column = 66, row = 79 }, start = { column = 64, row = 79 } } (VarPattern \"lV\"),Node { end = { column = 72, row = 79 }, start = { column = 67, row = 79 } } (VarPattern \"lLeft\"),Node { end = { column = 79, row = 79 }, start = { column = 73, row = 79 } } (VarPattern \"lRight\")]))),Node { end = { column = 171, row = 79 }, start = { column = 81, row = 79 } } (ParenthesizedPattern (Node { end = { column = 170, row = 79 }, start = { column = 82, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 105, row = 79 }, start = { column = 101, row = 79 } } (VarPattern \"rClr\"),Node { end = { column = 108, row = 79 }, start = { column = 106, row = 79 } } (VarPattern \"rK\"),Node { end = { column = 111, row = 79 }, start = { column = 109, row = 79 } } (VarPattern \"rV\"),Node { end = { column = 163, row = 79 }, start = { column = 112, row = 79 } } (ParenthesizedPattern (Node { end = { column = 162, row = 79 }, start = { column = 113, row = 79 } } (AsPattern (Node { end = { column = 153, row = 79 }, start = { column = 113, row = 79 } } (ParenthesizedPattern (Node { end = { column = 152, row = 79 }, start = { column = 114, row = 79 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 136, row = 79 }, start = { column = 133, row = 79 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 140, row = 79 }, start = { column = 137, row = 79 } } (VarPattern \"rlK\"),Node { end = { column = 144, row = 79 }, start = { column = 141, row = 79 } } (VarPattern \"rlV\"),Node { end = { column = 148, row = 79 }, start = { column = 145, row = 79 } } (VarPattern \"rlL\"),Node { end = { column = 152, row = 79 }, start = { column = 149, row = 79 } } (VarPattern \"rlR\")])))) (Node { end = { column = 162, row = 79 }, start = { column = 157, row = 79 } } \"rLeft\")))),Node { end = { column = 170, row = 79 }, start = { column = 164, row = 79 } } (VarPattern \"rRight\")])))]),Node { end = { column = 60, row = 85 }, start = { column = 13, row = 80 } } (Application [Node { end = { column = 31, row = 80 }, start = { column = 13, row = 80 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 20, row = 81 }, start = { column = 17, row = 81 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 20, row = 82 }, start = { column = 17, row = 82 } } (FunctionOrValue [] \"rlK\"),Node { end = { column = 20, row = 83 }, start = { column = 17, row = 83 } } (FunctionOrValue [] \"rlV\"),Node { end = { column = 95, row = 84 }, start = { column = 17, row = 84 } } (ParenthesizedExpression (Node { end = { column = 94, row = 84 }, start = { column = 18, row = 84 } } (Application [Node { end = { column = 36, row = 84 }, start = { column = 18, row = 84 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 42, row = 84 }, start = { column = 37, row = 84 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 44, row = 84 }, start = { column = 43, row = 84 } } (FunctionOrValue [] \"k\"),Node { end = { column = 46, row = 84 }, start = { column = 45, row = 84 } } (FunctionOrValue [] \"v\"),Node { end = { column = 90, row = 84 }, start = { column = 47, row = 84 } } (ParenthesizedExpression (Node { end = { column = 89, row = 84 }, start = { column = 48, row = 84 } } (Application [Node { end = { column = 66, row = 84 }, start = { column = 48, row = 84 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 70, row = 84 }, start = { column = 67, row = 84 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 73, row = 84 }, start = { column = 71, row = 84 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 76, row = 84 }, start = { column = 74, row = 84 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 82, row = 84 }, start = { column = 77, row = 84 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 89, row = 84 }, start = { column = 83, row = 84 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 94, row = 84 }, start = { column = 91, row = 84 } } (FunctionOrValue [] \"rlL\")]))),Node { end = { column = 60, row = 85 }, start = { column = 17, row = 85 } } (ParenthesizedExpression (Node { end = { column = 59, row = 85 }, start = { column = 18, row = 85 } } (Application [Node { end = { column = 36, row = 85 }, start = { column = 18, row = 85 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 42, row = 85 }, start = { column = 37, row = 85 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 45, row = 85 }, start = { column = 43, row = 85 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 48, row = 85 }, start = { column = 46, row = 85 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 52, row = 85 }, start = { column = 49, row = 85 } } (FunctionOrValue [] \"rlR\"),Node { end = { column = 59, row = 85 }, start = { column = 53, row = 85 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 125, row = 87 }, start = { column = 9, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 31, row = 87 }, start = { column = 28, row = 87 } } (VarPattern \"clr\"),Node { end = { column = 33, row = 87 }, start = { column = 32, row = 87 } } (VarPattern \"k\"),Node { end = { column = 35, row = 87 }, start = { column = 34, row = 87 } } (VarPattern \"v\"),Node { end = { column = 80, row = 87 }, start = { column = 36, row = 87 } } (ParenthesizedPattern (Node { end = { column = 79, row = 87 }, start = { column = 37, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 60, row = 87 }, start = { column = 56, row = 87 } } (VarPattern \"lClr\"),Node { end = { column = 63, row = 87 }, start = { column = 61, row = 87 } } (VarPattern \"lK\"),Node { end = { column = 66, row = 87 }, start = { column = 64, row = 87 } } (VarPattern \"lV\"),Node { end = { column = 72, row = 87 }, start = { column = 67, row = 87 } } (VarPattern \"lLeft\"),Node { end = { column = 79, row = 87 }, start = { column = 73, row = 87 } } (VarPattern \"lRight\")]))),Node { end = { column = 125, row = 87 }, start = { column = 81, row = 87 } } (ParenthesizedPattern (Node { end = { column = 124, row = 87 }, start = { column = 82, row = 87 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 105, row = 87 }, start = { column = 101, row = 87 } } (VarPattern \"rClr\"),Node { end = { column = 108, row = 87 }, start = { column = 106, row = 87 } } (VarPattern \"rK\"),Node { end = { column = 111, row = 87 }, start = { column = 109, row = 87 } } (VarPattern \"rV\"),Node { end = { column = 117, row = 87 }, start = { column = 112, row = 87 } } (VarPattern \"rLeft\"),Node { end = { column = 124, row = 87 }, start = { column = 118, row = 87 } } (VarPattern \"rRight\")])))]),Node { end = { column = 68, row = 103 }, start = { column = 13, row = 88 } } (CaseExpression { cases = [[Node { end = { column = 22, row = 89 }, start = { column = 17, row = 89 } } (NamedPattern { moduleName = [], name = \"Black\" } []),Node { end = { column = 68, row = 95 }, start = { column = 21, row = 90 } } (Application [Node { end = { column = 39, row = 90 }, start = { column = 21, row = 90 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 30, row = 91 }, start = { column = 25, row = 91 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 26, row = 92 }, start = { column = 25, row = 92 } } (FunctionOrValue [] \"k\"),Node { end = { column = 26, row = 93 }, start = { column = 25, row = 93 } } (FunctionOrValue [] \"v\"),Node { end = { column = 68, row = 94 }, start = { column = 25, row = 94 } } (ParenthesizedExpression (Node { end = { column = 67, row = 94 }, start = { column = 26, row = 94 } } (Application [Node { end = { column = 44, row = 94 }, start = { column = 26, row = 94 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 94 }, start = { column = 45, row = 94 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 94 }, start = { column = 49, row = 94 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 54, row = 94 }, start = { column = 52, row = 94 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 60, row = 94 }, start = { column = 55, row = 94 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 67, row = 94 }, start = { column = 61, row = 94 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 68, row = 95 }, start = { column = 25, row = 95 } } (ParenthesizedExpression (Node { end = { column = 67, row = 95 }, start = { column = 26, row = 95 } } (Application [Node { end = { column = 44, row = 95 }, start = { column = 26, row = 95 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 95 }, start = { column = 45, row = 95 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 95 }, start = { column = 49, row = 95 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 54, row = 95 }, start = { column = 52, row = 95 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 60, row = 95 }, start = { column = 55, row = 95 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 67, row = 95 }, start = { column = 61, row = 95 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 20, row = 97 }, start = { column = 17, row = 97 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 68, row = 103 }, start = { column = 21, row = 98 } } (Application [Node { end = { column = 39, row = 98 }, start = { column = 21, row = 98 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 30, row = 99 }, start = { column = 25, row = 99 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 26, row = 100 }, start = { column = 25, row = 100 } } (FunctionOrValue [] \"k\"),Node { end = { column = 26, row = 101 }, start = { column = 25, row = 101 } } (FunctionOrValue [] \"v\"),Node { end = { column = 68, row = 102 }, start = { column = 25, row = 102 } } (ParenthesizedExpression (Node { end = { column = 67, row = 102 }, start = { column = 26, row = 102 } } (Application [Node { end = { column = 44, row = 102 }, start = { column = 26, row = 102 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 102 }, start = { column = 45, row = 102 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 102 }, start = { column = 49, row = 102 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 54, row = 102 }, start = { column = 52, row = 102 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 60, row = 102 }, start = { column = 55, row = 102 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 67, row = 102 }, start = { column = 61, row = 102 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 68, row = 103 }, start = { column = 25, row = 103 } } (ParenthesizedExpression (Node { end = { column = 67, row = 103 }, start = { column = 26, row = 103 } } (Application [Node { end = { column = 44, row = 103 }, start = { column = 26, row = 103 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 48, row = 103 }, start = { column = 45, row = 103 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 51, row = 103 }, start = { column = 49, row = 103 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 54, row = 103 }, start = { column = 52, row = 103 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 60, row = 103 }, start = { column = 55, row = 103 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 67, row = 103 }, start = { column = 61, row = 103 } } (FunctionOrValue [] \"rRight\")])))])]], expression = Node { end = { column = 21, row = 88 }, start = { column = 18, row = 88 } } (FunctionOrValue [] \"clr\") })],[Node { end = { column = 10, row = 105 }, start = { column = 9, row = 105 } } AllPattern,Node { end = { column = 17, row = 106 }, start = { column = 13, row = 106 } } (FunctionOrValue [] \"dict\")]], expression = Node { end = { column = 14, row = 78 }, start = { column = 10, row = 78 } } (FunctionOrValue [] \"dict\") }), name = Node { end = { column = 12, row = 77 }, start = { column = 1, row = 77 } } \"moveRedLeft\" }, documentation = Nothing, signature = Just (Node { end = { column = 35, row = 76 }, start = { column = 1, row = 76 } } { name = Node { end = { column = 12, row = 76 }, start = { column = 1, row = 76 } } \"moveRedLeft\", typeAnnotation = Node { end = { column = 35, row = 76 }, start = { column = 15, row = 76 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 76 }, start = { column = 15, row = 76 } } (Typed (Node { end = { column = 19, row = 76 }, start = { column = 15, row = 76 } } ([],\"Dict\")) [Node { end = { column = 21, row = 76 }, start = { column = 20, row = 76 } } (GenericType \"k\"),Node { end = { column = 23, row = 76 }, start = { column = 22, row = 76 } } (GenericType \"v\")])) (Node { end = { column = 35, row = 76 }, start = { column = 27, row = 76 } } (Typed (Node { end = { column = 31, row = 76 }, start = { column = 27, row = 76 } } ([],\"Dict\")) [Node { end = { column = 33, row = 76 }, start = { column = 32, row = 76 } } (GenericType \"k\"),Node { end = { column = 35, row = 76 }, start = { column = 34, row = 76 } } (GenericType \"v\")]))) }) })",
            ",Node { end = { column = 10, row = 124 }, start = { column = 1, row = 109 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 124 }, start = { column = 1, row = 110 } } { arguments = [Node { end = { column = 10, row = 110 }, start = { column = 6, row = 110 } } (VarPattern \"func\"),Node { end = { column = 28, row = 110 }, start = { column = 11, row = 110 } } (ParenthesizedPattern (Node { end = { column = 27, row = 110 }, start = { column = 12, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 27, row = 110 }, start = { column = 20, row = 110 } } (VarPattern \"decodeA\")]))),Node { end = { column = 46, row = 110 }, start = { column = 29, row = 110 } } (ParenthesizedPattern (Node { end = { column = 45, row = 110 }, start = { column = 30, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 45, row = 110 }, start = { column = 38, row = 110 } } (VarPattern \"decodeB\")]))),Node { end = { column = 64, row = 110 }, start = { column = 47, row = 110 } } (ParenthesizedPattern (Node { end = { column = 63, row = 110 }, start = { column = 48, row = 110 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 63, row = 110 }, start = { column = 56, row = 110 } } (VarPattern \"decodeC\")])))], expression = Node { end = { column = 10, row = 124 }, start = { column = 5, row = 111 } } (Application [Node { end = { column = 12, row = 111 }, start = { column = 5, row = 111 } } (FunctionOrValue [] \"Decoder\"),Node { end = { column = 10, row = 124 }, start = { column = 9, row = 112 } } (ParenthesizedExpression (Node { end = { column = 36, row = 123 }, start = { column = 10, row = 112 } } (LambdaExpression { args = [Node { end = { column = 16, row = 112 }, start = { column = 11, row = 112 } } (VarPattern \"bites\"),Node { end = { column = 23, row = 112 }, start = { column = 17, row = 112 } } (VarPattern \"offset\")], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 113 } } (LetExpression { declarations = [Node { end = { column = 41, row = 115 }, start = { column = 17, row = 114 } } (LetDestructuring (Node { end = { column = 31, row = 114 }, start = { column = 17, row = 114 } } (TuplePattern [Node { end = { column = 26, row = 114 }, start = { column = 19, row = 114 } } (VarPattern \"offsetA\"),Node { end = { column = 29, row = 114 }, start = { column = 28, row = 114 } } (VarPattern \"a\")])) (Node { end = { column = 41, row = 115 }, start = { column = 21, row = 115 } } (Application [Node { end = { column = 28, row = 115 }, start = { column = 21, row = 115 } } (FunctionOrValue [] \"decodeA\"),Node { end = { column = 34, row = 115 }, start = { column = 29, row = 115 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 41, row = 115 }, start = { column = 35, row = 115 } } (FunctionOrValue [] \"offset\")]))),Node { end = { column = 42, row = 118 }, start = { column = 17, row = 117 } } (LetDestructuring (Node { end = { column = 31, row = 117 }, start = { column = 17, row = 117 } } (TuplePattern [Node { end = { column = 26, row = 117 }, start = { column = 19, row = 117 } } (VarPattern \"offsetB\"),Node { end = { column = 29, row = 117 }, start = { column = 28, row = 117 } } (VarPattern \"b\")])) (Node { end = { column = 42, row = 118 }, start = { column = 21, row = 118 } } (Application [Node { end = { column = 28, row = 118 }, start = { column = 21, row = 118 } } (FunctionOrValue [] \"decodeB\"),Node { end = { column = 34, row = 118 }, start = { column = 29, row = 118 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 118 }, start = { column = 35, row = 118 } } (FunctionOrValue [] \"offsetA\")]))),Node { end = { column = 42, row = 121 }, start = { column = 17, row = 120 } } (LetDestructuring (Node { end = { column = 31, row = 120 }, start = { column = 17, row = 120 } } (TuplePattern [Node { end = { column = 26, row = 120 }, start = { column = 19, row = 120 } } (VarPattern \"offsetC\"),Node { end = { column = 29, row = 120 }, start = { column = 28, row = 120 } } (VarPattern \"c\")])) (Node { end = { column = 42, row = 121 }, start = { column = 21, row = 121 } } (Application [Node { end = { column = 28, row = 121 }, start = { column = 21, row = 121 } } (FunctionOrValue [] \"decodeC\"),Node { end = { column = 34, row = 121 }, start = { column = 29, row = 121 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 121 }, start = { column = 35, row = 121 } } (FunctionOrValue [] \"offsetB\")])))], expression = Node { end = { column = 36, row = 123 }, start = { column = 13, row = 123 } } (TupledExpression [Node { end = { column = 22, row = 123 }, start = { column = 15, row = 123 } } (FunctionOrValue [] \"offsetC\"),Node { end = { column = 34, row = 123 }, start = { column = 24, row = 123 } } (Application [Node { end = { column = 28, row = 123 }, start = { column = 24, row = 123 } } (FunctionOrValue [] \"func\"),Node { end = { column = 30, row = 123 }, start = { column = 29, row = 123 } } (FunctionOrValue [] \"a\"),Node { end = { column = 32, row = 123 }, start = { column = 31, row = 123 } } (FunctionOrValue [] \"b\"),Node { end = { column = 34, row = 123 }, start = { column = 33, row = 123 } } (FunctionOrValue [] \"c\")])]) }) })))]), name = Node { end = { column = 5, row = 110 }, start = { column = 1, row = 110 } } \"map3\" }, documentation = Nothing, signature = Just (Node { end = { column = 86, row = 109 }, start = { column = 1, row = 109 } } { name = Node { end = { column = 5, row = 109 }, start = { column = 1, row = 109 } } \"map3\", typeAnnotation = Node { end = { column = 86, row = 109 }, start = { column = 8, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 30, row = 109 }, start = { column = 8, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 10, row = 109 }, start = { column = 9, row = 109 } } (GenericType \"a\")) (Node { end = { column = 29, row = 109 }, start = { column = 14, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 109 }, start = { column = 14, row = 109 } } (GenericType \"b\")) (Node { end = { column = 29, row = 109 }, start = { column = 19, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 20, row = 109 }, start = { column = 19, row = 109 } } (GenericType \"c\")) (Node { end = { column = 29, row = 109 }, start = { column = 24, row = 109 } } (GenericType \"value\")))))))) (Node { end = { column = 86, row = 109 }, start = { column = 34, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 43, row = 109 }, start = { column = 34, row = 109 } } (Typed (Node { end = { column = 41, row = 109 }, start = { column = 34, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 43, row = 109 }, start = { column = 42, row = 109 } } (GenericType \"a\")])) (Node { end = { column = 86, row = 109 }, start = { column = 47, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 56, row = 109 }, start = { column = 47, row = 109 } } (Typed (Node { end = { column = 54, row = 109 }, start = { column = 47, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 56, row = 109 }, start = { column = 55, row = 109 } } (GenericType \"b\")])) (Node { end = { column = 86, row = 109 }, start = { column = 60, row = 109 } } (FunctionTypeAnnotation (Node { end = { column = 69, row = 109 }, start = { column = 60, row = 109 } } (Typed (Node { end = { column = 67, row = 109 }, start = { column = 60, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 69, row = 109 }, start = { column = 68, row = 109 } } (GenericType \"c\")])) (Node { end = { column = 86, row = 109 }, start = { column = 73, row = 109 } } (Typed (Node { end = { column = 80, row = 109 }, start = { column = 73, row = 109 } } ([],\"Decoder\")) [Node { end = { column = 86, row = 109 }, start = { column = 81, row = 109 } } (GenericType \"value\")]))))))))) }) })",
            ",Node { end = { column = 27, row = 130 }, start = { column = 1, row = 127 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 13, row = 128 }, start = { column = 7, row = 128 } } { arguments = [], name = Node { end = { column = 13, row = 128 }, start = { column = 7, row = 128 } } \"MyTrue\" },Node { end = { column = 14, row = 129 }, start = { column = 7, row = 129 } } { arguments = [], name = Node { end = { column = 14, row = 129 }, start = { column = 7, row = 129 } } \"MyFalse\" },Node { end = { column = 27, row = 130 }, start = { column = 7, row = 130 } } { arguments = [Node { end = { column = 19, row = 130 }, start = { column = 12, row = 130 } } (Typed (Node { end = { column = 19, row = 130 }, start = { column = 12, row = 130 } } ([],\"Boolean\")) []),Node { end = { column = 27, row = 130 }, start = { column = 20, row = 130 } } (Typed (Node { end = { column = 27, row = 130 }, start = { column = 20, row = 130 } } ([],\"Boolean\")) [])], name = Node { end = { column = 11, row = 130 }, start = { column = 7, row = 130 } } \"MyOr\" }], documentation = Nothing, generics = [], name = Node { end = { column = 13, row = 127 }, start = { column = 6, row = 127 } } \"Boolean\" })",
            ",Node { end = { column = 10, row = 150 }, start = { column = 1, row = 133 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 150 }, start = { column = 1, row = 134 } } { arguments = [], expression = Node { end = { column = 10, row = 150 }, start = { column = 5, row = 135 } } (Application [Node { end = { column = 17, row = 135 }, start = { column = 5, row = 135 } } (FunctionOrValue [\"Parser\"] \"oneOf\"),Node { end = { column = 10, row = 150 }, start = { column = 9, row = 136 } } (ListExpr [Node { end = { column = 37, row = 137 }, start = { column = 11, row = 136 } } (OperatorApplication \"|.\" Left (Node { end = { column = 32, row = 136 }, start = { column = 11, row = 136 } } (Application [Node { end = { column = 25, row = 136 }, start = { column = 11, row = 136 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 32, row = 136 }, start = { column = 26, row = 136 } } (FunctionOrValue [] \"MyTrue\")])) (Node { end = { column = 37, row = 137 }, start = { column = 16, row = 137 } } (Application [Node { end = { column = 30, row = 137 }, start = { column = 16, row = 137 } } (FunctionOrValue [\"Parser\"] \"keyword\"),Node { end = { column = 37, row = 137 }, start = { column = 31, row = 137 } } (Literal \"true\")]))),Node { end = { column = 38, row = 139 }, start = { column = 11, row = 138 } } (OperatorApplication \"|.\" Left (Node { end = { column = 33, row = 138 }, start = { column = 11, row = 138 } } (Application [Node { end = { column = 25, row = 138 }, start = { column = 11, row = 138 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 33, row = 138 }, start = { column = 26, row = 138 } } (FunctionOrValue [] \"MyFalse\")])) (Node { end = { column = 38, row = 139 }, start = { column = 16, row = 139 } } (Application [Node { end = { column = 30, row = 139 }, start = { column = 16, row = 139 } } (FunctionOrValue [\"Parser\"] \"keyword\"),Node { end = { column = 38, row = 139 }, start = { column = 31, row = 139 } } (Literal \"false\")]))),Node { end = { column = 33, row = 149 }, start = { column = 11, row = 140 } } (OperatorApplication \"|=\" Left (Node { end = { column = 29, row = 146 }, start = { column = 11, row = 140 } } (OperatorApplication \"|=\" Left (Node { end = { column = 29, row = 142 }, start = { column = 11, row = 140 } } (OperatorApplication \"|.\" Left (Node { end = { column = 33, row = 141 }, start = { column = 11, row = 140 } } (OperatorApplication \"|.\" Left (Node { end = { column = 30, row = 140 }, start = { column = 11, row = 140 } } (Application [Node { end = { column = 25, row = 140 }, start = { column = 11, row = 140 } } (FunctionOrValue [\"Parser\"] \"succeed\"),Node { end = { column = 30, row = 140 }, start = { column = 26, row = 140 } } (FunctionOrValue [] \"MyOr\")])) (Node { end = { column = 33, row = 141 }, start = { column = 16, row = 141 } } (Application [Node { end = { column = 29, row = 141 }, start = { column = 16, row = 141 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 33, row = 141 }, start = { column = 30, row = 141 } } (Literal \"(\")])))) (Node { end = { column = 29, row = 142 }, start = { column = 16, row = 142 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 29, row = 146 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 34, row = 145 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 29, row = 144 }, start = { column = 16, row = 143 } } (OperatorApplication \"|.\" Left (Node { end = { column = 43, row = 143 }, start = { column = 16, row = 143 } } (Application [Node { end = { column = 27, row = 143 }, start = { column = 16, row = 143 } } (FunctionOrValue [\"Parser\"] \"lazy\"),Node { end = { column = 43, row = 143 }, start = { column = 28, row = 143 } } (ParenthesizedExpression (Node { end = { column = 42, row = 143 }, start = { column = 29, row = 143 } } (LambdaExpression { args = [Node { end = { column = 31, row = 143 }, start = { column = 30, row = 143 } } AllPattern], expression = Node { end = { column = 42, row = 143 }, start = { column = 35, row = 143 } } (FunctionOrValue [] \"boolean\") })))])) (Node { end = { column = 29, row = 144 }, start = { column = 16, row = 144 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 34, row = 145 }, start = { column = 16, row = 145 } } (Application [Node { end = { column = 29, row = 145 }, start = { column = 16, row = 145 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 34, row = 145 }, start = { column = 30, row = 145 } } (Literal \"||\")])))) (Node { end = { column = 29, row = 146 }, start = { column = 16, row = 146 } } (FunctionOrValue [\"Parser\"] \"spaces\")))))) (Node { end = { column = 33, row = 149 }, start = { column = 16, row = 147 } } (OperatorApplication \"|.\" Left (Node { end = { column = 29, row = 148 }, start = { column = 16, row = 147 } } (OperatorApplication \"|.\" Left (Node { end = { column = 43, row = 147 }, start = { column = 16, row = 147 } } (Application [Node { end = { column = 27, row = 147 }, start = { column = 16, row = 147 } } (FunctionOrValue [\"Parser\"] \"lazy\"),Node { end = { column = 43, row = 147 }, start = { column = 28, row = 147 } } (ParenthesizedExpression (Node { end = { column = 42, row = 147 }, start = { column = 29, row = 147 } } (LambdaExpression { args = [Node { end = { column = 31, row = 147 }, start = { column = 30, row = 147 } } AllPattern], expression = Node { end = { column = 42, row = 147 }, start = { column = 35, row = 147 } } (FunctionOrValue [] \"boolean\") })))])) (Node { end = { column = 29, row = 148 }, start = { column = 16, row = 148 } } (FunctionOrValue [\"Parser\"] \"spaces\")))) (Node { end = { column = 33, row = 149 }, start = { column = 16, row = 149 } } (Application [Node { end = { column = 29, row = 149 }, start = { column = 16, row = 149 } } (FunctionOrValue [\"Parser\"] \"symbol\"),Node { end = { column = 33, row = 149 }, start = { column = 30, row = 149 } } (Literal \")\")])))))])]), name = Node { end = { column = 14, row = 134 }, start = { column = 1, row = 134 } } \"booleanParser\" }, documentation = Nothing, signature = Just (Node { end = { column = 38, row = 133 }, start = { column = 1, row = 133 } } { name = Node { end = { column = 14, row = 133 }, start = { column = 1, row = 133 } } \"booleanParser\", typeAnnotation = Node { end = { column = 38, row = 133 }, start = { column = 17, row = 133 } } (Typed (Node { end = { column = 30, row = 133 }, start = { column = 17, row = 133 } } ([\"Parser\"],\"Parser\")) [Node { end = { column = 38, row = 133 }, start = { column = 31, row = 133 } } (Typed (Node { end = { column = 38, row = 133 }, start = { column = 31, row = 133 } } ([],\"Boolean\")) [])]) }) }),Node { end = { column = 18, row = 155 }, start = { column = 1, row = 153 } } (FunctionDeclaration { declaration = Node { end = { column = 18, row = 155 }, start = { column = 1, row = 154 } } { arguments = [Node { end = { column = 22, row = 154 }, start = { column = 21, row = 154 } } (VarPattern \"a\"),Node { end = { column = 24, row = 154 }, start = { column = 23, row = 154 } } (VarPattern \"b\"),Node { end = { column = 26, row = 154 }, start = { column = 25, row = 154 } } (VarPattern \"c\"),Node { end = { column = 28, row = 154 }, start = { column = 27, row = 154 } } (VarPattern \"d\")], expression = Node { end = { column = 18, row = 155 }, start = { column = 5, row = 155 } } (OperatorApplication \"-\" Left (Node { end = { column = 14, row = 155 }, start = { column = 5, row = 155 } } (OperatorApplication \"+\" Left (Node { end = { column = 6, row = 155 }, start = { column = 5, row = 155 } } (FunctionOrValue [] \"a\")) (Node { end = { column = 14, row = 155 }, start = { column = 9, row = 155 } } (OperatorApplication \"*\" Left (Node { end = { column = 10, row = 155 }, start = { column = 9, row = 155 } } (FunctionOrValue [] \"b\")) (Node { end = { column = 14, row = 155 }, start = { column = 13, row = 155 } } (FunctionOrValue [] \"c\")))))) (Node { end = { column = 18, row = 155 }, start = { column = 17, row = 155 } } (FunctionOrValue [] \"d\"))), name = Node { end = { column = 20, row = 154 }, start = { column = 1, row = 154 } } \"mixedOperators_Alfa\" }, documentation = Nothing, signature = Just (Node { end = { column = 54, row = 153 }, start = { column = 1, row = 153 } } { name = Node { end = { column = 20, row = 153 }, start = { column = 1, row = 153 } } \"mixedOperators_Alfa\", typeAnnotation = Node { end = { column = 54, row = 153 }, start = { column = 23, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 153 }, start = { column = 23, row = 153 } } (Typed (Node { end = { column = 26, row = 153 }, start = { column = 23, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 30, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 153 }, start = { column = 30, row = 153 } } (Typed (Node { end = { column = 33, row = 153 }, start = { column = 30, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 37, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 40, row = 153 }, start = { column = 37, row = 153 } } (Typed (Node { end = { column = 40, row = 153 }, start = { column = 37, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 44, row = 153 } } (FunctionTypeAnnotation (Node { end = { column = 47, row = 153 }, start = { column = 44, row = 153 } } (Typed (Node { end = { column = 47, row = 153 }, start = { column = 44, row = 153 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 153 }, start = { column = 51, row = 153 } } (Typed (Node { end = { column = 54, row = 153 }, start = { column = 51, row = 153 } } ([],\"Int\")) []))))))))) }) })",
            ",Node { end = { column = 19, row = 160 }, start = { column = 1, row = 158 } } (FunctionDeclaration { declaration = Node { end = { column = 19, row = 160 }, start = { column = 1, row = 159 } } { arguments = [Node { end = { column = 22, row = 159 }, start = { column = 21, row = 159 } } (VarPattern \"a\"),Node { end = { column = 24, row = 159 }, start = { column = 23, row = 159 } } (VarPattern \"b\"),Node { end = { column = 26, row = 159 }, start = { column = 25, row = 159 } } (VarPattern \"c\"),Node { end = { column = 28, row = 159 }, start = { column = 27, row = 159 } } (VarPattern \"d\")], expression = Node { end = { column = 19, row = 160 }, start = { column = 5, row = 160 } } (OperatorApplication \"+\" Left (Node { end = { column = 15, row = 160 }, start = { column = 5, row = 160 } } (OperatorApplication \"+\" Left (Node { end = { column = 6, row = 160 }, start = { column = 5, row = 160 } } (FunctionOrValue [] \"a\")) (Node { end = { column = 15, row = 160 }, start = { column = 9, row = 160 } } (OperatorApplication \"//\" Left (Node { end = { column = 10, row = 160 }, start = { column = 9, row = 160 } } (FunctionOrValue [] \"b\")) (Node { end = { column = 15, row = 160 }, start = { column = 14, row = 160 } } (FunctionOrValue [] \"c\")))))) (Node { end = { column = 19, row = 160 }, start = { column = 18, row = 160 } } (FunctionOrValue [] \"d\"))), name = Node { end = { column = 20, row = 159 }, start = { column = 1, row = 159 } } \"mixedOperators_Beta\" }, documentation = Nothing, signature = Just (Node { end = { column = 54, row = 158 }, start = { column = 1, row = 158 } } { name = Node { end = { column = 20, row = 158 }, start = { column = 1, row = 158 } } \"mixedOperators_Beta\", typeAnnotation = Node { end = { column = 54, row = 158 }, start = { column = 23, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 158 }, start = { column = 23, row = 158 } } (Typed (Node { end = { column = 26, row = 158 }, start = { column = 23, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 30, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 33, row = 158 }, start = { column = 30, row = 158 } } (Typed (Node { end = { column = 33, row = 158 }, start = { column = 30, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 37, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 40, row = 158 }, start = { column = 37, row = 158 } } (Typed (Node { end = { column = 40, row = 158 }, start = { column = 37, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 44, row = 158 } } (FunctionTypeAnnotation (Node { end = { column = 47, row = 158 }, start = { column = 44, row = 158 } } (Typed (Node { end = { column = 47, row = 158 }, start = { column = 44, row = 158 } } ([],\"Int\")) [])) (Node { end = { column = 54, row = 158 }, start = { column = 51, row = 158 } } (Typed (Node { end = { column = 54, row = 158 }, start = { column = 51, row = 158 } } ([],\"Int\")) []))))))))) }) })]",
            ", imports = [Node { end = { column = 27, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Just (Node { end = { column = 27, row = 3 }, start = { column = 14, row = 3 } } (All { end = { column = 26, row = 3 }, start = { column = 24, row = 3 } })), moduleAlias = Nothing, moduleName = Node { end = { column = 13, row = 3 }, start = { column = 8, row = 3 } } [\"Delta\"] },Node { end = { column = 12, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 4 }, start = { column = 8, row = 4 } } [\"Dict\"] },Node { end = { column = 66, row = 5 }, start = { column = 1, row = 5 } } { exposingList = Just (Node { end = { column = 66, row = 5 }, start = { column = 23, row = 5 } } (Explicit [Node { end = { column = 47, row = 5 }, start = { column = 33, row = 5 } } (TypeExpose { name = \"Dependency\", open = Just { end = { column = 47, row = 5 }, start = { column = 43, row = 5 } } }),Node { end = { column = 56, row = 5 }, start = { column = 49, row = 5 } } (TypeOrAliasExpose \"Version\"),Node { end = { column = 65, row = 5 }, start = { column = 58, row = 5 } } (FunctionExpose \"epsilon\")])), moduleAlias = Nothing, moduleName = Node { end = { column = 22, row = 5 }, start = { column = 8, row = 5 } } [\"Elm\",\"Dependency\"] },Node { end = { column = 36, row = 6 }, start = { column = 1, row = 6 } } { exposingList = Nothing, moduleAlias = Just (Node { end = { column = 36, row = 6 }, start = { column = 27, row = 6 } } [\"Interface\"]), moduleName = Node { end = { column = 23, row = 6 }, start = { column = 8, row = 6 } } [\"Gamma\",\"Interface\"] }]",
            ", moduleDefinition = Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 36, row = 1 }, start = { column = 23, row = 1 } } (All { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } }), moduleName = Node { end = { column = 22, row = 1 }, start = { column = 8, row = 1 } } [\"Namespace\",\"Beta\"] }) }"
            ];

        var expectedExpressionString =
            string.Concat(expectedExpressionStringChunks);

        var compilerProgram =
            ElmCompiler.CompilerSourceContainerFilesDefault.Value;

        using var compilerJavaScript =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var parsedModulePineValue =
            ElmInteractive.ParseElmModuleTextToPineValue(elmModuleText, compilerJavaScript)
            .Extract(err => throw new Exception(err));

        var responseAsElmValue =
            ElmValueEncoding.PineValueAsElmValue(parsedModulePineValue, null, null)
            .Extract(err => throw new Exception(err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseAsElmValue).expressionString;

        {
            /*
             * Compare chunkwise to make it easier to maintain and adapt tests.
             * */

            int previousChunkEnd = 0;

            for (var chunkIndex = 0; chunkIndex < expectedExpressionStringChunks.Count; chunkIndex++)
            {
                try
                {
                    var expectedChunk = expectedExpressionStringChunks[chunkIndex];

                    var responseChunk =
                        responseAsExpression.Substring(previousChunkEnd, expectedChunk.Length);

                    Assert.AreEqual(expectedChunk, responseChunk);

                    previousChunkEnd += expectedChunk.Length;
                }
                catch (Exception e)
                {
                    var remainingText =
                        responseAsExpression[previousChunkEnd..];

                    throw new Exception(
                        "Failed in chunk " + chunkIndex + " of " + expectedExpressionStringChunks.Count +
                        "\nText following previous checked chunk is:\n" + remainingText,
                        e);
                }
            }
        }

        Assert.AreEqual(
            expectedExpressionString,
            responseAsExpression,
            "Module parsed as expression syntax");

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

        Assert.AreEqual(13, declarations.Length);

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
}
