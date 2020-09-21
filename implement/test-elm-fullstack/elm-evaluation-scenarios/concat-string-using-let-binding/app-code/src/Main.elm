module Main exposing (evaluation_root)


evaluation_root : String
evaluation_root =
    let
        binding_from_let =
            "literal from let "
    in
    binding_from_let ++ " second literal ✔️"
