module Elm.Interface exposing
    ( Interface, Exposed(..)
    , build, exposesAlias, exposesFunction, operators
    )

{-| A type that represents the interface for an Elm module.
You can see this as a trimmed down version of a file that only contains the header (`module X exposing (..)`) and some small set of additional data.


## Types

@docs Interface, Exposed


## Functions

@docs build, exposesAlias, exposesFunction, operators

-}

import Dict exposing (Dict)
import Elm.Internal.RawFile as InternalRawFile
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))


{-| An interface is just a list of 'things' that are exposed by a module.

    [ Type "Color" [ "Red", "Blue" ], Function "asRgb" ]

-}
type alias Interface =
    List Exposed


{-| Union type for the things that a module can expose. These are `Function`s, `CustomType`s, and `Alias`es.

Elm core packages can also define `Operator`s, and thus we take that into account as well.
The `Infix` type alias will contain all the information regarding the operator

-}
type Exposed
    = Function String
    | CustomType ( String, List String )
    | Alias String
    | Operator Infix


{-| A function to check whether an `Interface` exposes an certain type alias.
-}
exposesAlias : String -> Interface -> Bool
exposesAlias k interface =
    List.any
        (\x ->
            case x of
                Alias l ->
                    k == l

                _ ->
                    False
        )
        interface


{-| Check whether an `Interface` exposes an function.

    exposesFunction "A" [ Function "A", CustomType "B", [ "C" ] ] == True
    exposesFunction "B" [ Function "A", CustomType "B", [ "C" ] ] == False
    exposesFunction "<" [ Infix { operator = "<" , ... } ] == True
    exposesFunction "A" [ Alias "A" ] == False

-}
exposesFunction : String -> Interface -> Bool
exposesFunction k interface =
    List.any
        (\x ->
            case x of
                Function l ->
                    k == l

                CustomType ( _, constructors ) ->
                    List.member k constructors

                Operator inf ->
                    Node.value inf.operator == k

                Alias _ ->
                    False
        )
        interface


{-| Retrieve all operators exposed by the `Interface`
-}
operators : Interface -> List Infix
operators =
    List.filterMap
        (\interface ->
            case interface of
                Operator operator ->
                    Just operator

                _ ->
                    Nothing
        )


{-| Build an interface from a file
-}
build : RawFile.RawFile -> Interface
build (InternalRawFile.Raw file) =
    let
        fileDefinitionList : List ( String, Exposed )
        fileDefinitionList =
            fileToDefinitions file
    in
    case Module.exposingList (Node.value file.moduleDefinition) of
        Explicit x ->
            buildInterfaceFromExplicit x (Dict.fromList fileDefinitionList)

        All _ ->
            List.map Tuple.second fileDefinitionList


buildInterfaceFromExplicit : List (Node TopLevelExpose) -> Dict String Exposed -> Interface
buildInterfaceFromExplicit x exposedDict =
    List.filterMap
        (\(Node _ expose) ->
            case expose of
                InfixExpose k ->
                    Dict.get k exposedDict

                TypeOrAliasExpose s ->
                    Dict.get s exposedDict
                        |> Maybe.map (ifCustomType (\( name, _ ) -> CustomType ( name, [] )))

                FunctionExpose s ->
                    Just <| Function s

                TypeExpose exposedType ->
                    case exposedType.open of
                        Nothing ->
                            Just <| CustomType ( exposedType.name, [] )

                        Just _ ->
                            Dict.get exposedType.name exposedDict
        )
        x


ifCustomType : (( String, List String ) -> Exposed) -> Exposed -> Exposed
ifCustomType f i =
    case i of
        CustomType t ->
            f t

        _ ->
            i


fileToDefinitions : File -> List ( String, Exposed )
fileToDefinitions file =
    let
        allDeclarations : List ( String, Exposed )
        allDeclarations =
            List.filterMap
                (\(Node _ decl) ->
                    case decl of
                        CustomTypeDeclaration t ->
                            Just ( Node.value t.name, CustomType ( Node.value t.name, t.constructors |> List.map (Node.value >> .name >> Node.value) ) )

                        AliasDeclaration a ->
                            Just ( Node.value a.name, Alias <| Node.value a.name )

                        PortDeclaration p ->
                            Just ( Node.value p.name, Function (Node.value p.name) )

                        FunctionDeclaration f ->
                            let
                                declaration : FunctionImplementation
                                declaration =
                                    Node.value f.declaration

                                name : String
                                name =
                                    Node.value declaration.name
                            in
                            Just ( name, Function <| name )

                        InfixDeclaration i ->
                            Just ( Node.value i.operator, Operator i )

                        Destructuring _ _ ->
                            Nothing
                )
                file.declarations
    in
    -- I really don't understand what this part of the function does.
    allDeclarations
        |> List.map
            (\( name, _ ) ->
                -- AFAIK, it's not possible to have two declarations with the same name
                -- in the same file, so this seems pointless.
                allDeclarations
                    |> List.filter (\( otherDeclarationName, _ ) -> otherDeclarationName == name)
            )
        |> List.filterMap resolveGroup


resolveGroup : List ( String, Exposed ) -> Maybe ( String, Exposed )
resolveGroup g =
    case g of
        [] ->
            Nothing

        [ x ] ->
            Just x

        [ ( n1, t1 ), ( _, t2 ) ] ->
            getValidOperatorInterface t1 t2
                |> Maybe.map (\a -> ( n1, a ))

        _ ->
            Nothing


getValidOperatorInterface : Exposed -> Exposed -> Maybe Exposed
getValidOperatorInterface t1 t2 =
    case ( t1, t2 ) of
        ( Operator x, Operator y ) ->
            if Node.value x.precedence == 5 && Node.value x.direction == Left then
                Just <| Operator y

            else
                Just <| Operator x

        _ ->
            Nothing
