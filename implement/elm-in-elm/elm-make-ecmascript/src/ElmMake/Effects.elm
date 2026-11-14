module ElmMake.Effects exposing (compile, isLeaf, isTrusted, managerName)

import ElmMake.Errors as Errors
import ElmMake.JavaScriptLiterals as Literals
import ElmMake.JavaScriptSyntax as JS
import ElmMake.Model exposing (..)
import ElmSyntax.Abstract.Module as SyntaxModule


managerName : String
managerName =
    "$fx$"


isTrusted : ElmMake.Model.Module -> Bool
isTrusted module_ =
    String.startsWith "elm/" module_.package || String.startsWith "elm-explorations/" module_.package


isLeaf : ElmMake.Model.Module -> String -> Bool
isLeaf module_ name =
    case module_.syntax.moduleDefinition of
        SyntaxModule.EffectModule data ->
            (name == "command" && data.command /= Nothing) || (name == "subscription" && data.subscription /= Nothing)

        _ ->
            False


{-| A manager is one graph node shared by its command/subscription leaves.
Its initialization dependencies have semantic order, unlike ordinary globals.
-}
compile : ElmMake.Model.Module -> Result MakeError { text : String, dependencies : List Global }
compile module_ =
    case module_.syntax.moduleDefinition of
        SyntaxModule.EffectModule data ->
            let
                global name =
                    ( module_.package, module_.name, name )

                ref name =
                    JS.Ref (globalName (global name))

                core =
                    [ "init", "onEffects", "onSelfMsg" ]

                leaf name =
                    JS.Var (globalName (global name)) (JS.Call (JS.Ref "_Platform_leaf") [ JS.Literal (Literals.string module_.name) ])

                configuration =
                    case ( data.command, data.subscription ) of
                        ( Just _, Nothing ) ->
                            Ok ( core ++ [ "cmdMap" ], List.map ref (core ++ [ "cmdMap" ]), [ leaf "command" ] )

                        ( Nothing, Just _ ) ->
                            Ok ( core ++ [ "subMap" ], List.map ref core ++ [ JS.Literal "0", ref "subMap" ], [ leaf "subscription" ] )

                        ( Just _, Just _ ) ->
                            Ok ( core ++ [ "cmdMap", "subMap" ], List.map ref (core ++ [ "cmdMap", "subMap" ]), [ leaf "command", leaf "subscription" ] )

                        ( Nothing, Nothing ) ->
                            Err (Errors.internal InvalidPackageSources ("Effect module has no effect kind: " ++ module_.name))
            in
            configuration
                |> Result.map
                    (\( dependencies, arguments, leaves ) ->
                        { text =
                            JS.Assign ("_Platform_effectManagers[" ++ Literals.string module_.name ++ "]")
                                (JS.Call (JS.Ref "_Platform_createManager") arguments)
                                :: leaves
                                |> List.map JS.renderStatement
                                |> String.concat
                        , dependencies = ( "elm/core", "Elm.Kernel.Platform", "$" ) :: List.map global dependencies
                        }
                    )

        _ ->
            Err (Errors.internal CompilerInvariant ("Not an effect manager: " ++ module_.name))
