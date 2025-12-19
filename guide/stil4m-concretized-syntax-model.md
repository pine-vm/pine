# stil4m Concretized Syntax Model

For working with Elm syntax, we use a concretized variant of the syntax model from [stil4m/elm-syntax](https://github.com/stil4m/elm-syntax/tree/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax)

What does this mean? This syntax model enables the simple derivation of the same syntax description we would get by running the parser from stil4m/elm-syntax directly.

But in contrast to the model from `stil4m/elm-syntax`, it contains more information about the original representation, to enable round-tripping back to a string.

Initially, we used parsed Elm syntax solely for compilation. We used the parser and syntax model from `stil4m/elm-syntax` version 7 for a long time, and it has provided all the information we needed for the Elm compiler. In 2025, we began rendering generated Elm syntax for snapshot tests and formatting Elm module files.

Now we need a more concrete syntax model, since we do not just compile Elm code but also format Elm modules.

For more background on the challenges with the simpler syntax model, see:

+ [2025-12-12-elm-formatter-and-syntax-model-design-challenge.md](./../explore/2025-12-12-elm-formatter-and-syntax-model-design-challenge.md)
+ <https://discourse.elm-lang.org/t/elm-syntax-the-rough-edges/10507>
