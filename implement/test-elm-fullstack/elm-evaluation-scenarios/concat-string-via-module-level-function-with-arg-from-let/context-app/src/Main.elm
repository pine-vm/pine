module Main exposing (evaluation_root)


module_level_binding : String -> String
module_level_binding param0 =
    let
        local_binding =
            "unused value"
    in
    "literal from module " ++ param0


evaluation_root : String
evaluation_root =
    let
        local_binding =
            " second literal ✔️"
    in
    module_level_binding local_binding
