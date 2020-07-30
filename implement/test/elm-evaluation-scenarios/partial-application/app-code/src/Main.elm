module Main exposing (evaluation_root)


evaluation_root : String
evaluation_root =
    partially_applied_b "c"


partially_applied_a : String -> String -> String
partially_applied_a =
    module_level_binding "a"


partially_applied_b : String -> String
partially_applied_b =
    partially_applied_a "b"


module_level_binding : String -> String -> String -> String
module_level_binding param0 param1 param2 =
    "literal from module " ++ param0 ++ param2 ++ param1
