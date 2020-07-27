module Main exposing (evaluation_root)


module_level_binding : String -> String
module_level_binding param0 =
    "literal from module " ++ param0


evaluation_root : String
evaluation_root =
    module_level_binding " second literal ✔️"
