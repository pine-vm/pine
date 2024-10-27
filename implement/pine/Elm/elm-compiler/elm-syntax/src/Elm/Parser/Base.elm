module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Combine exposing (Parser, sepBy1, string)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)


moduleName : Parser s ModuleName
moduleName =
    sepBy1 (string ".") Tokens.typeName


typeIndicator : Parser s ( ModuleName, String )
typeIndicator =
    let
        helper : ModuleName -> String -> Parser s ( ModuleName, String )
        helper moduleNameSoFar typeOrSegment =
            Combine.oneOf
                [ Combine.succeed identity
                    |> Combine.ignore (string ".")
                    |> Combine.keep Tokens.typeName
                    |> Combine.andThen (\t -> helper (typeOrSegment :: moduleNameSoFar) t)
                , Combine.succeed ()
                    |> Combine.map (\() -> ( List.reverse moduleNameSoFar, typeOrSegment ))
                ]
    in
    Tokens.typeName
        |> Combine.andThen (\typeOrSegment -> helper [] typeOrSegment)
