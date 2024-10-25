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

            import Dict


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
                        if valueString == []
                        then
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
            
            """;

        IReadOnlyList<string> expectedExpressionStringChunks =
            [
            "{ comments = []",
            ", declarations = [",
            "Node { end = { column = 14, row = 7 }, start = { column = 1, row = 6 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 6 }, start = { column = 12, row = 6 } } \"MaybeInt\", typeAnnotation = Node { end = { column = 14, row = 7 }, start = { column = 5, row = 7 } } (Typed (Node { end = { column = 10, row = 7 }, start = { column = 5, row = 7 } } ([],\"Maybe\")) [Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } (Typed (Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } ([],\"Int\")) [])]) })",
            ",Node { end = { column = 19, row = 11 }, start = { column = 1, row = 10 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 10 }, start = { column = 12, row = 10 } } \"RecordType\", typeAnnotation = Node { end = { column = 19, row = 11 }, start = { column = 5, row = 11 } } (Record [Node { end = { column = 17, row = 11 }, start = { column = 7, row = 11 } } [Node { end = { column = 11, row = 11 }, start = { column = 7, row = 11 } } \"alfa\",Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } (Typed (Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } ([],\"Int\")) [])]]) })",
            ",Node { end = { column = 22, row = 16 }, start = { column = 1, row = 14 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } { arguments = [], name = Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } \"Choice_Alfa\" },Node { end = { column = 22, row = 16 }, start = { column = 7, row = 16 } } { arguments = [Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } (Typed (Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } ([],\"Int\")) [])], name = Node { end = { column = 18, row = 16 }, start = { column = 7, row = 16 } } \"Choice_Beta\" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 14 }, start = { column = 6, row = 14 } } \"ChoiceType\" })",
            ",Node { end = { column = 36, row = 21 }, start = { column = 1, row = 19 } } (FunctionDeclaration { declaration = Node { end = { column = 36, row = 21 }, start = { column = 1, row = 20 } } { arguments = [Node { end = { column = 17, row = 20 }, start = { column = 7, row = 20 } } (VarPattern \"param_name\")], expression = Node { end = { column = 36, row = 21 }, start = { column = 5, row = 21 } } (OperatorApplication \"++\" Right (Node { end = { column = 14, row = 21 }, start = { column = 5, row = 21 } } (Literal \"Hello, \")) (Node { end = { column = 36, row = 21 }, start = { column = 18, row = 21 } } (OperatorApplication \"++\" Right (Node { end = { column = 28, row = 21 }, start = { column = 18, row = 21 } } (FunctionOrValue [] \"param_name\")) (Node { end = { column = 36, row = 21 }, start = { column = 32, row = 21 } } (Literal \" 👋\"))))), name = Node { end = { column = 6, row = 20 }, start = { column = 1, row = 20 } } \"greet\" }, documentation = Nothing, signature = Just (Node { end = { column = 25, row = 19 }, start = { column = 1, row = 19 } } { name = Node { end = { column = 6, row = 19 }, start = { column = 1, row = 19 } } \"greet\", typeAnnotation = Node { end = { column = 25, row = 19 }, start = { column = 9, row = 19 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } (Typed (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } ([],\"String\")) [])) (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } ([],\"String\")) []))) }) })",
            ",Node { end = { column = 19, row = 28 }, start = { column = 1, row = 24 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 30, row = 25 }, start = { column = 7, row = 25 } } { arguments = [Node { end = { column = 30, row = 25 }, start = { column = 14, row = 25 } } (Typed (Node { end = { column = 19, row = 25 }, start = { column = 15, row = 25 } } ([],\"List\")) [Node { end = { column = 29, row = 25 }, start = { column = 20, row = 25 } } (Typed (Node { end = { column = 29, row = 25 }, start = { column = 20, row = 25 } } ([\"Char\"],\"Char\")) [])])], name = Node { end = { column = 13, row = 25 }, start = { column = 7, row = 25 } } \"String\" },Node { end = { column = 19, row = 28 }, start = { column = 7, row = 28 } } { arguments = [], name = Node { end = { column = 19, row = 28 }, start = { column = 7, row = 28 } } \"AnyOtherKind\" }], documentation = Nothing, generics = [], name = Node { end = { column = 12, row = 24 }, start = { column = 6, row = 24 } } \"String\" })",
            ",Node { end = { column = 36, row = 40 }, start = { column = 1, row = 31 } } (FunctionDeclaration { declaration = Node { end = { column = 36, row = 40 }, start = { column = 1, row = 34 } } { arguments = [Node { end = { column = 12, row = 34 }, start = { column = 11, row = 34 } } (VarPattern \"a\")], expression = Node { end = { column = 36, row = 40 }, start = { column = 5, row = 35 } } (CaseExpression { cases = [[Node { end = { column = 19, row = 36 }, start = { column = 5, row = 36 } } (NamedPattern { moduleName = [], name = \"String\" } [Node { end = { column = 19, row = 36 }, start = { column = 12, row = 36 } } (VarPattern \"stringA\")]),Node { end = { column = 57, row = 37 }, start = { column = 9, row = 37 } } (Application [Node { end = { column = 15, row = 37 }, start = { column = 9, row = 37 } } (FunctionOrValue [] \"String\"),Node { end = { column = 57, row = 37 }, start = { column = 16, row = 37 } } (ParenthesizedExpression (Node { end = { column = 56, row = 37 }, start = { column = 17, row = 37 } } (Application [Node { end = { column = 35, row = 37 }, start = { column = 17, row = 37 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 56, row = 37 }, start = { column = 36, row = 37 } } (ListExpr [Node { end = { column = 45, row = 37 }, start = { column = 38, row = 37 } } (FunctionOrValue [] \"stringA\"),Node { end = { column = 54, row = 37 }, start = { column = 47, row = 37 } } (FunctionOrValue [] \"stringA\")])])))])],[Node { end = { column = 6, row = 39 }, start = { column = 5, row = 39 } } AllPattern,Node { end = { column = 36, row = 40 }, start = { column = 9, row = 40 } } (Application [Node { end = { column = 27, row = 40 }, start = { column = 9, row = 40 } } (FunctionOrValue [\"Pine_kernel\"] \"concat\"),Node { end = { column = 36, row = 40 }, start = { column = 28, row = 40 } } (ListExpr [Node { end = { column = 31, row = 40 }, start = { column = 30, row = 40 } } (FunctionOrValue [] \"a\"),Node { end = { column = 34, row = 40 }, start = { column = 33, row = 40 } } (FunctionOrValue [] \"a\")])])]], expression = Node { end = { column = 11, row = 35 }, start = { column = 10, row = 35 } } (FunctionOrValue [] \"a\") }), name = Node { end = { column = 10, row = 34 }, start = { column = 1, row = 34 } } \"replicate\" }, documentation = Just (Node { end = { column = 3, row = 32 }, start = { column = 1, row = 31 } } \"{-| Replicates the given appendable.\n-}\"), signature = Just (Node { end = { column = 37, row = 33 }, start = { column = 1, row = 33 } } { name = Node { end = { column = 10, row = 33 }, start = { column = 1, row = 33 } } \"replicate\", typeAnnotation = Node { end = { column = 37, row = 33 }, start = { column = 13, row = 33 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 33 }, start = { column = 13, row = 33 } } (GenericType \"appendable\")) (Node { end = { column = 37, row = 33 }, start = { column = 27, row = 33 } } (GenericType \"appendable\"))) }) })",
            ",Node { end = { column = 32, row = 71 }, start = { column = 1, row = 43 } } (FunctionDeclaration { declaration = Node { end = { column = 32, row = 71 }, start = { column = 1, row = 44 } } { arguments = [Node { end = { column = 27, row = 44 }, start = { column = 15, row = 44 } } (VarPattern \"stringAsList\")], expression = Node { end = { column = 32, row = 71 }, start = { column = 5, row = 45 } } (CaseExpression { cases = [[Node { end = { column = 11, row = 46 }, start = { column = 9, row = 46 } } (ListPattern []),Node { end = { column = 20, row = 47 }, start = { column = 13, row = 47 } } (FunctionOrValue [] \"Nothing\")],[Node { end = { column = 35, row = 49 }, start = { column = 9, row = 49 } } (UnConsPattern (Node { end = { column = 18, row = 49 }, start = { column = 9, row = 49 } } (VarPattern \"firstChar\")) (Node { end = { column = 35, row = 49 }, start = { column = 22, row = 49 } } (VarPattern \"lessFirstChar\"))),Node { end = { column = 32, row = 71 }, start = { column = 13, row = 50 } } (LetExpression { declarations = [Node { end = { column = 48, row = 60 }, start = { column = 17, row = 51 } } (LetDestructuring (Node { end = { column = 48, row = 51 }, start = { column = 17, row = 51 } } (TuplePattern [Node { end = { column = 30, row = 51 }, start = { column = 19, row = 51 } } (VarPattern \"valueString\"),Node { end = { column = 46, row = 51 }, start = { column = 32, row = 51 } } (VarPattern \"signMultiplier\")])) (Node { end = { column = 48, row = 60 }, start = { column = 21, row = 52 } } (CaseExpression { cases = [[Node { end = { column = 28, row = 53 }, start = { column = 25, row = 53 } } (CharPattern '-'),Node { end = { column = 50, row = 54 }, start = { column = 29, row = 54 } } (TupledExpression [Node { end = { column = 44, row = 54 }, start = { column = 31, row = 54 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 48, row = 54 }, start = { column = 46, row = 54 } } (Negation (Node { end = { column = 48, row = 54 }, start = { column = 47, row = 54 } } (Integer 1)))])],[Node { end = { column = 28, row = 56 }, start = { column = 25, row = 56 } } (CharPattern '+'),Node { end = { column = 49, row = 57 }, start = { column = 29, row = 57 } } (TupledExpression [Node { end = { column = 44, row = 57 }, start = { column = 31, row = 57 } } (FunctionOrValue [] \"lessFirstChar\"),Node { end = { column = 47, row = 57 }, start = { column = 46, row = 57 } } (Integer 1)])],[Node { end = { column = 26, row = 59 }, start = { column = 25, row = 59 } } AllPattern,Node { end = { column = 48, row = 60 }, start = { column = 29, row = 60 } } (TupledExpression [Node { end = { column = 43, row = 60 }, start = { column = 31, row = 60 } } (FunctionOrValue [] \"stringAsList\"),Node { end = { column = 46, row = 60 }, start = { column = 45, row = 60 } } (Integer 1)])]], expression = Node { end = { column = 35, row = 52 }, start = { column = 26, row = 52 } } (FunctionOrValue [] \"firstChar\") })))], expression = Node { end = { column = 32, row = 71 }, start = { column = 13, row = 62 } } (IfBlock (Node { end = { column = 33, row = 62 }, start = { column = 16, row = 62 } } (OperatorApplication \"==\" Non (Node { end = { column = 27, row = 62 }, start = { column = 16, row = 62 } } (FunctionOrValue [] \"valueString\")) (Node { end = { column = 33, row = 62 }, start = { column = 31, row = 62 } } (ListExpr [])))) (Node { end = { column = 24, row = 64 }, start = { column = 17, row = 64 } } (FunctionOrValue [] \"Nothing\")) (Node { end = { column = 32, row = 71 }, start = { column = 17, row = 66 } } (CaseExpression { cases = [[Node { end = { column = 34, row = 67 }, start = { column = 21, row = 67 } } (NamedPattern { moduleName = [], name = \"Just\" } [Node { end = { column = 34, row = 67 }, start = { column = 26, row = 67 } } (VarPattern \"unsigned\")]),Node { end = { column = 80, row = 68 }, start = { column = 25, row = 68 } } (Application [Node { end = { column = 29, row = 68 }, start = { column = 25, row = 68 } } (FunctionOrValue [] \"Just\"),Node { end = { column = 80, row = 68 }, start = { column = 30, row = 68 } } (ParenthesizedExpression (Node { end = { column = 79, row = 68 }, start = { column = 31, row = 68 } } (Application [Node { end = { column = 50, row = 68 }, start = { column = 31, row = 68 } } (FunctionOrValue [\"Pine_kernel\"] \"int_mul\"),Node { end = { column = 79, row = 68 }, start = { column = 51, row = 68 } } (ListExpr [Node { end = { column = 67, row = 68 }, start = { column = 53, row = 68 } } (FunctionOrValue [] \"signMultiplier\"),Node { end = { column = 77, row = 68 }, start = { column = 69, row = 68 } } (FunctionOrValue [] \"unsigned\")])])))])],[Node { end = { column = 28, row = 70 }, start = { column = 21, row = 70 } } (NamedPattern { moduleName = [], name = \"Nothing\" } []),Node { end = { column = 32, row = 71 }, start = { column = 25, row = 71 } } (FunctionOrValue [] \"Nothing\")]], expression = Node { end = { column = 57, row = 66 }, start = { column = 22, row = 66 } } (Application [Node { end = { column = 43, row = 66 }, start = { column = 22, row = 66 } } (FunctionOrValue [] \"toUnsignedIntFromList\"),Node { end = { column = 45, row = 66 }, start = { column = 44, row = 66 } } (Integer 0),Node { end = { column = 57, row = 66 }, start = { column = 46, row = 66 } } (FunctionOrValue [] \"valueString\")]) }))) })]], expression = Node { end = { column = 22, row = 45 }, start = { column = 10, row = 45 } } (FunctionOrValue [] \"stringAsList\") }), name = Node { end = { column = 14, row = 44 }, start = { column = 1, row = 44 } } \"toIntFromList\" }, documentation = Nothing, signature = Just (Node { end = { column = 39, row = 43 }, start = { column = 1, row = 43 } } { name = Node { end = { column = 14, row = 43 }, start = { column = 1, row = 43 } } \"toIntFromList\", typeAnnotation = Node { end = { column = 39, row = 43 }, start = { column = 17, row = 43 } } (FunctionTypeAnnotation (Node { end = { column = 26, row = 43 }, start = { column = 17, row = 43 } } (Typed (Node { end = { column = 21, row = 43 }, start = { column = 17, row = 43 } } ([],\"List\")) [Node { end = { column = 26, row = 43 }, start = { column = 22, row = 43 } } (Typed (Node { end = { column = 26, row = 43 }, start = { column = 22, row = 43 } } ([],\"Char\")) [])])) (Node { end = { column = 39, row = 43 }, start = { column = 30, row = 43 } } (Typed (Node { end = { column = 35, row = 43 }, start = { column = 30, row = 43 } } ([],\"Maybe\")) [Node { end = { column = 39, row = 43 }, start = { column = 36, row = 43 } } (Typed (Node { end = { column = 39, row = 43 }, start = { column = 36, row = 43 } } ([],\"Int\")) [])]))) }) })",
            ",Node { end = { column = 11, row = 104 }, start = { column = 1, row = 74 } } (FunctionDeclaration { declaration = Node { end = { column = 11, row = 104 }, start = { column = 1, row = 75 } } { arguments = [Node { end = { column = 17, row = 75 }, start = { column = 13, row = 75 } } (VarPattern \"dict\")], expression = Node { end = { column = 11, row = 104 }, start = { column = 3, row = 76 } } (CaseExpression { cases = [[Node { end = { column = 167, row = 77 }, start = { column = 5, row = 77 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 27, row = 77 }, start = { column = 24, row = 77 } } (VarPattern \"clr\"),Node { end = { column = 29, row = 77 }, start = { column = 28, row = 77 } } (VarPattern \"k\"),Node { end = { column = 31, row = 77 }, start = { column = 30, row = 77 } } (VarPattern \"v\"),Node { end = { column = 76, row = 77 }, start = { column = 32, row = 77 } } (ParenthesizedPattern (Node { end = { column = 75, row = 77 }, start = { column = 33, row = 77 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 56, row = 77 }, start = { column = 52, row = 77 } } (VarPattern \"lClr\"),Node { end = { column = 59, row = 77 }, start = { column = 57, row = 77 } } (VarPattern \"lK\"),Node { end = { column = 62, row = 77 }, start = { column = 60, row = 77 } } (VarPattern \"lV\"),Node { end = { column = 68, row = 77 }, start = { column = 63, row = 77 } } (VarPattern \"lLeft\"),Node { end = { column = 75, row = 77 }, start = { column = 69, row = 77 } } (VarPattern \"lRight\")]))),Node { end = { column = 167, row = 77 }, start = { column = 77, row = 77 } } (ParenthesizedPattern (Node { end = { column = 166, row = 77 }, start = { column = 78, row = 77 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 101, row = 77 }, start = { column = 97, row = 77 } } (VarPattern \"rClr\"),Node { end = { column = 104, row = 77 }, start = { column = 102, row = 77 } } (VarPattern \"rK\"),Node { end = { column = 107, row = 77 }, start = { column = 105, row = 77 } } (VarPattern \"rV\"),Node { end = { column = 159, row = 77 }, start = { column = 108, row = 77 } } (ParenthesizedPattern (Node { end = { column = 158, row = 77 }, start = { column = 109, row = 77 } } (AsPattern (Node { end = { column = 149, row = 77 }, start = { column = 109, row = 77 } } (ParenthesizedPattern (Node { end = { column = 148, row = 77 }, start = { column = 110, row = 77 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 132, row = 77 }, start = { column = 129, row = 77 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 136, row = 77 }, start = { column = 133, row = 77 } } (VarPattern \"rlK\"),Node { end = { column = 140, row = 77 }, start = { column = 137, row = 77 } } (VarPattern \"rlV\"),Node { end = { column = 144, row = 77 }, start = { column = 141, row = 77 } } (VarPattern \"rlL\"),Node { end = { column = 148, row = 77 }, start = { column = 145, row = 77 } } (VarPattern \"rlR\")])))) (Node { end = { column = 158, row = 77 }, start = { column = 153, row = 77 } } \"rLeft\")))),Node { end = { column = 166, row = 77 }, start = { column = 160, row = 77 } } (VarPattern \"rRight\")])))]),Node { end = { column = 52, row = 83 }, start = { column = 7, row = 78 } } (Application [Node { end = { column = 25, row = 78 }, start = { column = 7, row = 78 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 12, row = 79 }, start = { column = 9, row = 79 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 12, row = 80 }, start = { column = 9, row = 80 } } (FunctionOrValue [] \"rlK\"),Node { end = { column = 12, row = 81 }, start = { column = 9, row = 81 } } (FunctionOrValue [] \"rlV\"),Node { end = { column = 87, row = 82 }, start = { column = 9, row = 82 } } (ParenthesizedExpression (Node { end = { column = 86, row = 82 }, start = { column = 10, row = 82 } } (Application [Node { end = { column = 28, row = 82 }, start = { column = 10, row = 82 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 34, row = 82 }, start = { column = 29, row = 82 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 36, row = 82 }, start = { column = 35, row = 82 } } (FunctionOrValue [] \"k\"),Node { end = { column = 38, row = 82 }, start = { column = 37, row = 82 } } (FunctionOrValue [] \"v\"),Node { end = { column = 82, row = 82 }, start = { column = 39, row = 82 } } (ParenthesizedExpression (Node { end = { column = 81, row = 82 }, start = { column = 40, row = 82 } } (Application [Node { end = { column = 58, row = 82 }, start = { column = 40, row = 82 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 62, row = 82 }, start = { column = 59, row = 82 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 65, row = 82 }, start = { column = 63, row = 82 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 68, row = 82 }, start = { column = 66, row = 82 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 74, row = 82 }, start = { column = 69, row = 82 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 81, row = 82 }, start = { column = 75, row = 82 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 86, row = 82 }, start = { column = 83, row = 82 } } (FunctionOrValue [] \"rlL\")]))),Node { end = { column = 52, row = 83 }, start = { column = 9, row = 83 } } (ParenthesizedExpression (Node { end = { column = 51, row = 83 }, start = { column = 10, row = 83 } } (Application [Node { end = { column = 28, row = 83 }, start = { column = 10, row = 83 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 34, row = 83 }, start = { column = 29, row = 83 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 37, row = 83 }, start = { column = 35, row = 83 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 40, row = 83 }, start = { column = 38, row = 83 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 44, row = 83 }, start = { column = 41, row = 83 } } (FunctionOrValue [] \"rlR\"),Node { end = { column = 51, row = 83 }, start = { column = 45, row = 83 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 121, row = 85 }, start = { column = 5, row = 85 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 27, row = 85 }, start = { column = 24, row = 85 } } (VarPattern \"clr\"),Node { end = { column = 29, row = 85 }, start = { column = 28, row = 85 } } (VarPattern \"k\"),Node { end = { column = 31, row = 85 }, start = { column = 30, row = 85 } } (VarPattern \"v\"),Node { end = { column = 76, row = 85 }, start = { column = 32, row = 85 } } (ParenthesizedPattern (Node { end = { column = 75, row = 85 }, start = { column = 33, row = 85 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 56, row = 85 }, start = { column = 52, row = 85 } } (VarPattern \"lClr\"),Node { end = { column = 59, row = 85 }, start = { column = 57, row = 85 } } (VarPattern \"lK\"),Node { end = { column = 62, row = 85 }, start = { column = 60, row = 85 } } (VarPattern \"lV\"),Node { end = { column = 68, row = 85 }, start = { column = 63, row = 85 } } (VarPattern \"lLeft\"),Node { end = { column = 75, row = 85 }, start = { column = 69, row = 85 } } (VarPattern \"lRight\")]))),Node { end = { column = 121, row = 85 }, start = { column = 77, row = 85 } } (ParenthesizedPattern (Node { end = { column = 120, row = 85 }, start = { column = 78, row = 85 } } (NamedPattern { moduleName = [], name = \"RBNode_elm_builtin\" } [Node { end = { column = 101, row = 85 }, start = { column = 97, row = 85 } } (VarPattern \"rClr\"),Node { end = { column = 104, row = 85 }, start = { column = 102, row = 85 } } (VarPattern \"rK\"),Node { end = { column = 107, row = 85 }, start = { column = 105, row = 85 } } (VarPattern \"rV\"),Node { end = { column = 113, row = 85 }, start = { column = 108, row = 85 } } (VarPattern \"rLeft\"),Node { end = { column = 120, row = 85 }, start = { column = 114, row = 85 } } (VarPattern \"rRight\")])))]),Node { end = { column = 56, row = 101 }, start = { column = 7, row = 86 } } (CaseExpression { cases = [[Node { end = { column = 14, row = 87 }, start = { column = 9, row = 87 } } (NamedPattern { moduleName = [], name = \"Black\" } []),Node { end = { column = 56, row = 93 }, start = { column = 11, row = 88 } } (Application [Node { end = { column = 29, row = 88 }, start = { column = 11, row = 88 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 18, row = 89 }, start = { column = 13, row = 89 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 14, row = 90 }, start = { column = 13, row = 90 } } (FunctionOrValue [] \"k\"),Node { end = { column = 14, row = 91 }, start = { column = 13, row = 91 } } (FunctionOrValue [] \"v\"),Node { end = { column = 56, row = 92 }, start = { column = 13, row = 92 } } (ParenthesizedExpression (Node { end = { column = 55, row = 92 }, start = { column = 14, row = 92 } } (Application [Node { end = { column = 32, row = 92 }, start = { column = 14, row = 92 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 36, row = 92 }, start = { column = 33, row = 92 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 39, row = 92 }, start = { column = 37, row = 92 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 42, row = 92 }, start = { column = 40, row = 92 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 48, row = 92 }, start = { column = 43, row = 92 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 55, row = 92 }, start = { column = 49, row = 92 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 56, row = 93 }, start = { column = 13, row = 93 } } (ParenthesizedExpression (Node { end = { column = 55, row = 93 }, start = { column = 14, row = 93 } } (Application [Node { end = { column = 32, row = 93 }, start = { column = 14, row = 93 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 36, row = 93 }, start = { column = 33, row = 93 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 39, row = 93 }, start = { column = 37, row = 93 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 42, row = 93 }, start = { column = 40, row = 93 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 48, row = 93 }, start = { column = 43, row = 93 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 55, row = 93 }, start = { column = 49, row = 93 } } (FunctionOrValue [] \"rRight\")])))])],[Node { end = { column = 12, row = 95 }, start = { column = 9, row = 95 } } (NamedPattern { moduleName = [], name = \"Red\" } []),Node { end = { column = 56, row = 101 }, start = { column = 11, row = 96 } } (Application [Node { end = { column = 29, row = 96 }, start = { column = 11, row = 96 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 18, row = 97 }, start = { column = 13, row = 97 } } (FunctionOrValue [] \"Black\"),Node { end = { column = 14, row = 98 }, start = { column = 13, row = 98 } } (FunctionOrValue [] \"k\"),Node { end = { column = 14, row = 99 }, start = { column = 13, row = 99 } } (FunctionOrValue [] \"v\"),Node { end = { column = 56, row = 100 }, start = { column = 13, row = 100 } } (ParenthesizedExpression (Node { end = { column = 55, row = 100 }, start = { column = 14, row = 100 } } (Application [Node { end = { column = 32, row = 100 }, start = { column = 14, row = 100 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 36, row = 100 }, start = { column = 33, row = 100 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 39, row = 100 }, start = { column = 37, row = 100 } } (FunctionOrValue [] \"lK\"),Node { end = { column = 42, row = 100 }, start = { column = 40, row = 100 } } (FunctionOrValue [] \"lV\"),Node { end = { column = 48, row = 100 }, start = { column = 43, row = 100 } } (FunctionOrValue [] \"lLeft\"),Node { end = { column = 55, row = 100 }, start = { column = 49, row = 100 } } (FunctionOrValue [] \"lRight\")]))),Node { end = { column = 56, row = 101 }, start = { column = 13, row = 101 } } (ParenthesizedExpression (Node { end = { column = 55, row = 101 }, start = { column = 14, row = 101 } } (Application [Node { end = { column = 32, row = 101 }, start = { column = 14, row = 101 } } (FunctionOrValue [] \"RBNode_elm_builtin\"),Node { end = { column = 36, row = 101 }, start = { column = 33, row = 101 } } (FunctionOrValue [] \"Red\"),Node { end = { column = 39, row = 101 }, start = { column = 37, row = 101 } } (FunctionOrValue [] \"rK\"),Node { end = { column = 42, row = 101 }, start = { column = 40, row = 101 } } (FunctionOrValue [] \"rV\"),Node { end = { column = 48, row = 101 }, start = { column = 43, row = 101 } } (FunctionOrValue [] \"rLeft\"),Node { end = { column = 55, row = 101 }, start = { column = 49, row = 101 } } (FunctionOrValue [] \"rRight\")])))])]], expression = Node { end = { column = 15, row = 86 }, start = { column = 12, row = 86 } } (FunctionOrValue [] \"clr\") })],[Node { end = { column = 6, row = 103 }, start = { column = 5, row = 103 } } AllPattern,Node { end = { column = 11, row = 104 }, start = { column = 7, row = 104 } } (FunctionOrValue [] \"dict\")]], expression = Node { end = { column = 12, row = 76 }, start = { column = 8, row = 76 } } (FunctionOrValue [] \"dict\") }), name = Node { end = { column = 12, row = 75 }, start = { column = 1, row = 75 } } \"moveRedLeft\" }, documentation = Nothing, signature = Just (Node { end = { column = 35, row = 74 }, start = { column = 1, row = 74 } } { name = Node { end = { column = 12, row = 74 }, start = { column = 1, row = 74 } } \"moveRedLeft\", typeAnnotation = Node { end = { column = 35, row = 74 }, start = { column = 15, row = 74 } } (FunctionTypeAnnotation (Node { end = { column = 23, row = 74 }, start = { column = 15, row = 74 } } (Typed (Node { end = { column = 19, row = 74 }, start = { column = 15, row = 74 } } ([],\"Dict\")) [Node { end = { column = 21, row = 74 }, start = { column = 20, row = 74 } } (GenericType \"k\"),Node { end = { column = 23, row = 74 }, start = { column = 22, row = 74 } } (GenericType \"v\")])) (Node { end = { column = 35, row = 74 }, start = { column = 27, row = 74 } } (Typed (Node { end = { column = 31, row = 74 }, start = { column = 27, row = 74 } } ([],\"Dict\")) [Node { end = { column = 33, row = 74 }, start = { column = 32, row = 74 } } (GenericType \"k\"),Node { end = { column = 35, row = 74 }, start = { column = 34, row = 74 } } (GenericType \"v\")]))) }) })",
            ",Node { end = { column = 10, row = 122 }, start = { column = 1, row = 107 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 122 }, start = { column = 1, row = 108 } } { arguments = [Node { end = { column = 10, row = 108 }, start = { column = 6, row = 108 } } (VarPattern \"func\"),Node { end = { column = 28, row = 108 }, start = { column = 11, row = 108 } } (ParenthesizedPattern (Node { end = { column = 27, row = 108 }, start = { column = 12, row = 108 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 27, row = 108 }, start = { column = 20, row = 108 } } (VarPattern \"decodeA\")]))),Node { end = { column = 46, row = 108 }, start = { column = 29, row = 108 } } (ParenthesizedPattern (Node { end = { column = 45, row = 108 }, start = { column = 30, row = 108 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 45, row = 108 }, start = { column = 38, row = 108 } } (VarPattern \"decodeB\")]))),Node { end = { column = 64, row = 108 }, start = { column = 47, row = 108 } } (ParenthesizedPattern (Node { end = { column = 63, row = 108 }, start = { column = 48, row = 108 } } (NamedPattern { moduleName = [], name = \"Decoder\" } [Node { end = { column = 63, row = 108 }, start = { column = 56, row = 108 } } (VarPattern \"decodeC\")])))], expression = Node { end = { column = 10, row = 122 }, start = { column = 5, row = 109 } } (Application [Node { end = { column = 12, row = 109 }, start = { column = 5, row = 109 } } (FunctionOrValue [] \"Decoder\"),Node { end = { column = 10, row = 122 }, start = { column = 9, row = 110 } } (ParenthesizedExpression (Node { end = { column = 36, row = 121 }, start = { column = 10, row = 110 } } (LambdaExpression { args = [Node { end = { column = 16, row = 110 }, start = { column = 11, row = 110 } } (VarPattern \"bites\"),Node { end = { column = 23, row = 110 }, start = { column = 17, row = 110 } } (VarPattern \"offset\")], expression = Node { end = { column = 36, row = 121 }, start = { column = 13, row = 111 } } (LetExpression { declarations = [Node { end = { column = 41, row = 113 }, start = { column = 17, row = 112 } } (LetDestructuring (Node { end = { column = 31, row = 112 }, start = { column = 17, row = 112 } } (TuplePattern [Node { end = { column = 26, row = 112 }, start = { column = 19, row = 112 } } (VarPattern \"offsetA\"),Node { end = { column = 29, row = 112 }, start = { column = 28, row = 112 } } (VarPattern \"a\")])) (Node { end = { column = 41, row = 113 }, start = { column = 21, row = 113 } } (Application [Node { end = { column = 28, row = 113 }, start = { column = 21, row = 113 } } (FunctionOrValue [] \"decodeA\"),Node { end = { column = 34, row = 113 }, start = { column = 29, row = 113 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 41, row = 113 }, start = { column = 35, row = 113 } } (FunctionOrValue [] \"offset\")]))),Node { end = { column = 42, row = 116 }, start = { column = 17, row = 115 } } (LetDestructuring (Node { end = { column = 31, row = 115 }, start = { column = 17, row = 115 } } (TuplePattern [Node { end = { column = 26, row = 115 }, start = { column = 19, row = 115 } } (VarPattern \"offsetB\"),Node { end = { column = 29, row = 115 }, start = { column = 28, row = 115 } } (VarPattern \"b\")])) (Node { end = { column = 42, row = 116 }, start = { column = 21, row = 116 } } (Application [Node { end = { column = 28, row = 116 }, start = { column = 21, row = 116 } } (FunctionOrValue [] \"decodeB\"),Node { end = { column = 34, row = 116 }, start = { column = 29, row = 116 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 116 }, start = { column = 35, row = 116 } } (FunctionOrValue [] \"offsetA\")]))),Node { end = { column = 42, row = 119 }, start = { column = 17, row = 118 } } (LetDestructuring (Node { end = { column = 31, row = 118 }, start = { column = 17, row = 118 } } (TuplePattern [Node { end = { column = 26, row = 118 }, start = { column = 19, row = 118 } } (VarPattern \"offsetC\"),Node { end = { column = 29, row = 118 }, start = { column = 28, row = 118 } } (VarPattern \"c\")])) (Node { end = { column = 42, row = 119 }, start = { column = 21, row = 119 } } (Application [Node { end = { column = 28, row = 119 }, start = { column = 21, row = 119 } } (FunctionOrValue [] \"decodeC\"),Node { end = { column = 34, row = 119 }, start = { column = 29, row = 119 } } (FunctionOrValue [] \"bites\"),Node { end = { column = 42, row = 119 }, start = { column = 35, row = 119 } } (FunctionOrValue [] \"offsetB\")])))], expression = Node { end = { column = 36, row = 121 }, start = { column = 13, row = 121 } } (TupledExpression [Node { end = { column = 22, row = 121 }, start = { column = 15, row = 121 } } (FunctionOrValue [] \"offsetC\"),Node { end = { column = 34, row = 121 }, start = { column = 24, row = 121 } } (Application [Node { end = { column = 28, row = 121 }, start = { column = 24, row = 121 } } (FunctionOrValue [] \"func\"),Node { end = { column = 30, row = 121 }, start = { column = 29, row = 121 } } (FunctionOrValue [] \"a\"),Node { end = { column = 32, row = 121 }, start = { column = 31, row = 121 } } (FunctionOrValue [] \"b\"),Node { end = { column = 34, row = 121 }, start = { column = 33, row = 121 } } (FunctionOrValue [] \"c\")])]) }) })))]), name = Node { end = { column = 5, row = 108 }, start = { column = 1, row = 108 } } \"map3\" }, documentation = Nothing, signature = Just (Node { end = { column = 86, row = 107 }, start = { column = 1, row = 107 } } { name = Node { end = { column = 5, row = 107 }, start = { column = 1, row = 107 } } \"map3\", typeAnnotation = Node { end = { column = 86, row = 107 }, start = { column = 8, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 30, row = 107 }, start = { column = 8, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 10, row = 107 }, start = { column = 9, row = 107 } } (GenericType \"a\")) (Node { end = { column = 29, row = 107 }, start = { column = 14, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 107 }, start = { column = 14, row = 107 } } (GenericType \"b\")) (Node { end = { column = 29, row = 107 }, start = { column = 19, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 20, row = 107 }, start = { column = 19, row = 107 } } (GenericType \"c\")) (Node { end = { column = 29, row = 107 }, start = { column = 24, row = 107 } } (GenericType \"value\")))))))) (Node { end = { column = 86, row = 107 }, start = { column = 34, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 43, row = 107 }, start = { column = 34, row = 107 } } (Typed (Node { end = { column = 41, row = 107 }, start = { column = 34, row = 107 } } ([],\"Decoder\")) [Node { end = { column = 43, row = 107 }, start = { column = 42, row = 107 } } (GenericType \"a\")])) (Node { end = { column = 86, row = 107 }, start = { column = 47, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 56, row = 107 }, start = { column = 47, row = 107 } } (Typed (Node { end = { column = 54, row = 107 }, start = { column = 47, row = 107 } } ([],\"Decoder\")) [Node { end = { column = 56, row = 107 }, start = { column = 55, row = 107 } } (GenericType \"b\")])) (Node { end = { column = 86, row = 107 }, start = { column = 60, row = 107 } } (FunctionTypeAnnotation (Node { end = { column = 69, row = 107 }, start = { column = 60, row = 107 } } (Typed (Node { end = { column = 67, row = 107 }, start = { column = 60, row = 107 } } ([],\"Decoder\")) [Node { end = { column = 69, row = 107 }, start = { column = 68, row = 107 } } (GenericType \"c\")])) (Node { end = { column = 86, row = 107 }, start = { column = 73, row = 107 } } (Typed (Node { end = { column = 80, row = 107 }, start = { column = 73, row = 107 } } ([],\"Decoder\")) [Node { end = { column = 86, row = 107 }, start = { column = 81, row = 107 } } (GenericType \"value\")]))))))))) }) })]",
            ", imports = [Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } [\"Dict\"] }]",
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
            """[Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } ["Dict"] }]""",
            importsNodeAsExpression);

        var declarationsList =
            (ElmValue.ElmList)((ElmValue.ElmRecord)responseAsElmValue)["declarations"]!;

        var declarations =
            declarationsList.Elements.Cast<ElmValue.ElmTag>()
            .OrderBy(declarationNode =>
            ((ElmValue.ElmInteger)
            ((ElmValue.ElmRecord)((ElmValue.ElmRecord)declarationNode.Arguments[0])["start"]!)["row"]!).Value)
            .ToImmutableArray();

        Assert.AreEqual(9, declarations.Length);

        var typeAliasDeclarationNode = declarations.ElementAt(0);
        var recordAliasDeclarationNode = declarations.ElementAt(1);
        var choiceTypeDeclarationNode = declarations.ElementAt(2);

        var typeAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(typeAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 14, row = 7 }, start = { column = 1, row = 6 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 6 }, start = { column = 12, row = 6 } } "MaybeInt", typeAnnotation = Node { end = { column = 14, row = 7 }, start = { column = 5, row = 7 } } (Typed (Node { end = { column = 10, row = 7 }, start = { column = 5, row = 7 } } ([],"Maybe")) [Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } (Typed (Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } ([],"Int")) [])]) })""",
            typeAliasDeclarationNodeAsExpression);

        var recordAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(recordAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 19, row = 11 }, start = { column = 1, row = 10 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 10 }, start = { column = 12, row = 10 } } "RecordType", typeAnnotation = Node { end = { column = 19, row = 11 }, start = { column = 5, row = 11 } } (Record [Node { end = { column = 17, row = 11 }, start = { column = 7, row = 11 } } [Node { end = { column = 11, row = 11 }, start = { column = 7, row = 11 } } "alfa",Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } (Typed (Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } ([],"Int")) [])]]) })""",
            recordAliasDeclarationNodeAsExpression);

        var choiceTypeDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(choiceTypeDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 22, row = 16 }, start = { column = 1, row = 14 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } { arguments = [], name = Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } "Choice_Alfa" },Node { end = { column = 22, row = 16 }, start = { column = 7, row = 16 } } { arguments = [Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } (Typed (Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } ([],"Int")) [])], name = Node { end = { column = 18, row = 16 }, start = { column = 7, row = 16 } } "Choice_Beta" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 14 }, start = { column = 6, row = 14 } } "ChoiceType" })""",
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
                """Just (Node { end = { column = 25, row = 19 }, start = { column = 1, row = 19 } } { name = Node { end = { column = 6, row = 19 }, start = { column = 1, row = 19 } } "greet", typeAnnotation = Node { end = { column = 25, row = 19 }, start = { column = 9, row = 19 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } (Typed (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } ([],"String")) [])) (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } ([],"String")) []))) })""",
                declarationSignatureNodeAsExpression);

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 36, row = 21 }, start = { column = 1, row = 20 } } { arguments = [Node { end = { column = 17, row = 20 }, start = { column = 7, row = 20 } } (VarPattern "param_name")], expression = Node { end = { column = 36, row = 21 }, start = { column = 5, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 14, row = 21 }, start = { column = 5, row = 21 } } (Literal "Hello, ")) (Node { end = { column = 36, row = 21 }, start = { column = 18, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 28, row = 21 }, start = { column = 18, row = 21 } } (FunctionOrValue [] "param_name")) (Node { end = { column = 36, row = 21 }, start = { column = 32, row = 21 } } (Literal " 👋"))))), name = Node { end = { column = 6, row = 20 }, start = { column = 1, row = 20 } } "greet" }""",
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
                """Node { end = { column = 36, row = 40 }, start = { column = 1, row = 34 } } { arguments = [Node { end = { column = 12, row = 34 }, start = { column = 11, row = 34 } } (VarPattern "a")], expression = Node { end = { column = 36, row = 40 }, start = { column = 5, row = 35 } } (CaseExpression { cases = [[Node { end = { column = 19, row = 36 }, start = { column = 5, row = 36 } } (NamedPattern { moduleName = [], name = "String" } [Node { end = { column = 19, row = 36 }, start = { column = 12, row = 36 } } (VarPattern "stringA")]),Node { end = { column = 57, row = 37 }, start = { column = 9, row = 37 } } (Application [Node { end = { column = 15, row = 37 }, start = { column = 9, row = 37 } } (FunctionOrValue [] "String"),Node { end = { column = 57, row = 37 }, start = { column = 16, row = 37 } } (ParenthesizedExpression (Node { end = { column = 56, row = 37 }, start = { column = 17, row = 37 } } (Application [Node { end = { column = 35, row = 37 }, start = { column = 17, row = 37 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 56, row = 37 }, start = { column = 36, row = 37 } } (ListExpr [Node { end = { column = 45, row = 37 }, start = { column = 38, row = 37 } } (FunctionOrValue [] "stringA"),Node { end = { column = 54, row = 37 }, start = { column = 47, row = 37 } } (FunctionOrValue [] "stringA")])])))])],[Node { end = { column = 6, row = 39 }, start = { column = 5, row = 39 } } AllPattern,Node { end = { column = 36, row = 40 }, start = { column = 9, row = 40 } } (Application [Node { end = { column = 27, row = 40 }, start = { column = 9, row = 40 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 36, row = 40 }, start = { column = 28, row = 40 } } (ListExpr [Node { end = { column = 31, row = 40 }, start = { column = 30, row = 40 } } (FunctionOrValue [] "a"),Node { end = { column = 34, row = 40 }, start = { column = 33, row = 40 } } (FunctionOrValue [] "a")])])]], expression = Node { end = { column = 11, row = 35 }, start = { column = 10, row = 35 } } (FunctionOrValue [] "a") }), name = Node { end = { column = 10, row = 34 }, start = { column = 1, row = 34 } } "replicate" }""",
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
                """Node { end = { column = 10, row = 122 }, start = { column = 1, row = 108 } } { arguments = [Node { end = { column = 10, row = 108 }, start = { column = 6, row = 108 } } (VarPattern "func"),Node { end = { column = 28, row = 108 }, start = { column = 11, row = 108 } } (ParenthesizedPattern (Node { end = { column = 27, row = 108 }, start = { column = 12, row = 108 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 27, row = 108 }, start = { column = 20, row = 108 } } (VarPattern "decodeA")]))),Node { end = { column = 46, row = 108 }, start = { column = 29, row = 108 } } (ParenthesizedPattern (Node { end = { column = 45, row = 108 }, start = { column = 30, row = 108 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 45, row = 108 }, start = { column = 38, row = 108 } } (VarPattern "decodeB")]))),Node { end = { column = 64, row = 108 }, start = { column = 47, row = 108 } } (ParenthesizedPattern (Node { end = { column = 63, row = 108 }, start = { column = 48, row = 108 } } (NamedPattern { moduleName = [], name = "Decoder" } [Node { end = { column = 63, row = 108 }, start = { column = 56, row = 108 } } (VarPattern "decodeC")])))], expression = Node { end = { column = 10, row = 122 }, start = { column = 5, row = 109 } } (Application [Node { end = { column = 12, row = 109 }, start = { column = 5, row = 109 } } (FunctionOrValue [] "Decoder"),Node { end = { column = 10, row = 122 }, start = { column = 9, row = 110 } } (ParenthesizedExpression (Node { end = { column = 36, row = 121 }, start = { column = 10, row = 110 } } (LambdaExpression { args = [Node { end = { column = 16, row = 110 }, start = { column = 11, row = 110 } } (VarPattern "bites"),Node { end = { column = 23, row = 110 }, start = { column = 17, row = 110 } } (VarPattern "offset")], expression = Node { end = { column = 36, row = 121 }, start = { column = 13, row = 111 } } (LetExpression { declarations = [Node { end = { column = 41, row = 113 }, start = { column = 17, row = 112 } } (LetDestructuring (Node { end = { column = 31, row = 112 }, start = { column = 17, row = 112 } } (TuplePattern [Node { end = { column = 26, row = 112 }, start = { column = 19, row = 112 } } (VarPattern "offsetA"),Node { end = { column = 29, row = 112 }, start = { column = 28, row = 112 } } (VarPattern "a")])) (Node { end = { column = 41, row = 113 }, start = { column = 21, row = 113 } } (Application [Node { end = { column = 28, row = 113 }, start = { column = 21, row = 113 } } (FunctionOrValue [] "decodeA"),Node { end = { column = 34, row = 113 }, start = { column = 29, row = 113 } } (FunctionOrValue [] "bites"),Node { end = { column = 41, row = 113 }, start = { column = 35, row = 113 } } (FunctionOrValue [] "offset")]))),Node { end = { column = 42, row = 116 }, start = { column = 17, row = 115 } } (LetDestructuring (Node { end = { column = 31, row = 115 }, start = { column = 17, row = 115 } } (TuplePattern [Node { end = { column = 26, row = 115 }, start = { column = 19, row = 115 } } (VarPattern "offsetB"),Node { end = { column = 29, row = 115 }, start = { column = 28, row = 115 } } (VarPattern "b")])) (Node { end = { column = 42, row = 116 }, start = { column = 21, row = 116 } } (Application [Node { end = { column = 28, row = 116 }, start = { column = 21, row = 116 } } (FunctionOrValue [] "decodeB"),Node { end = { column = 34, row = 116 }, start = { column = 29, row = 116 } } (FunctionOrValue [] "bites"),Node { end = { column = 42, row = 116 }, start = { column = 35, row = 116 } } (FunctionOrValue [] "offsetA")]))),Node { end = { column = 42, row = 119 }, start = { column = 17, row = 118 } } (LetDestructuring (Node { end = { column = 31, row = 118 }, start = { column = 17, row = 118 } } (TuplePattern [Node { end = { column = 26, row = 118 }, start = { column = 19, row = 118 } } (VarPattern "offsetC"),Node { end = { column = 29, row = 118 }, start = { column = 28, row = 118 } } (VarPattern "c")])) (Node { end = { column = 42, row = 119 }, start = { column = 21, row = 119 } } (Application [Node { end = { column = 28, row = 119 }, start = { column = 21, row = 119 } } (FunctionOrValue [] "decodeC"),Node { end = { column = 34, row = 119 }, start = { column = 29, row = 119 } } (FunctionOrValue [] "bites"),Node { end = { column = 42, row = 119 }, start = { column = 35, row = 119 } } (FunctionOrValue [] "offsetB")])))], expression = Node { end = { column = 36, row = 121 }, start = { column = 13, row = 121 } } (TupledExpression [Node { end = { column = 22, row = 121 }, start = { column = 15, row = 121 } } (FunctionOrValue [] "offsetC"),Node { end = { column = 34, row = 121 }, start = { column = 24, row = 121 } } (Application [Node { end = { column = 28, row = 121 }, start = { column = 24, row = 121 } } (FunctionOrValue [] "func"),Node { end = { column = 30, row = 121 }, start = { column = 29, row = 121 } } (FunctionOrValue [] "a"),Node { end = { column = 32, row = 121 }, start = { column = 31, row = 121 } } (FunctionOrValue [] "b"),Node { end = { column = 34, row = 121 }, start = { column = 33, row = 121 } } (FunctionOrValue [] "c")])]) }) })))]), name = Node { end = { column = 5, row = 108 }, start = { column = 1, row = 108 } } "map3" }""",
                declarationDeclarationNodeAsExpression);
        }
    }
}
