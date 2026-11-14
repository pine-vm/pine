module ElmMake.EntryPoint exposing (compile)

import ElmMake.JavaScriptSyntax as JS
import ElmMake.MainType exposing (Main(..))
import ElmMake.Model exposing (..)


{-| Initialization contributes graph dependencies in addition to the main body.
Keep this outside expression lowering: static Html and Program values have
different runtime calling conventions.
-}
compile : Mode -> String -> Main -> Global -> { initializer : String, dependencies : List Global }
compile mode metadata main global =
    let
        name =
            JS.Ref (globalName global)
    in
    case main of
        StaticHtml ->
            { initializer = "_VirtualDom_init(" ++ globalName global ++ ")(0)(0)"
            , dependencies = [ ( "elm/virtual-dom", "Elm.Kernel.VirtualDom", "$" ) ]
            }

        ProgramWithUnitFlags ->
            let
                succeed =
                    ( "elm/json", "Json.Decode", "succeed" )

                decoder =
                    JS.Call (JS.Ref (globalName succeed))
                        [ if mode.flavor == Optimize then
                            JS.Literal "0"

                          else
                            JS.Ref "_Utils_Tuple0"
                        ]
            in
            { initializer = JS.renderExpression (JS.Call (JS.Call name [ decoder ]) [ JS.Literal metadata ])
            , dependencies = [ succeed ]
            }
