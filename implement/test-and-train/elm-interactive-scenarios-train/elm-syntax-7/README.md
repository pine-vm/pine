Tests using elm-syntax version 65641dac584b468b66c1eff8a09bba0df1be039c with the following changes:

+ Removed the dependencies on `Json.Encode` and `Json.Decode`
+ Changed import statements merging multiple modules into the same alias.
+ Replaced applications from `Unicode` with `Char`
+ Removed expositions of function declarations from import statements to avoid a bug in shadowing of imported function names. (One example triggering the shadowing bug is at <https://github.com/stil4m/elm-syntax/blob/65641da/src/Elm/Parser/Typings.elm#L8-L23>)
