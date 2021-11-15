module CompilationInterface.SourceFiles exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-fullstack/elm-fullstack/blob/main/guide/how-to-configure-and-deploy-an-elm-fullstack-app.md#compilationinterfacesourcefiles-elm-module>
-}


type alias Base64AndUtf8 =
    { utf8 : String, base64 : String }


file__utf8____a_file_txt : String
file__utf8____a_file_txt =
    "The compiler replaces this value."


file____a_file_txt : { utf8 : String }
file____a_file_txt =
    { utf8 = "The compiler replaces this value." }


file____directory_file_alpha_txt : Base64AndUtf8
file____directory_file_alpha_txt =
    { utf8 = "The compiler replaces this value."
    , base64 = "The compiler replaces this value."
    }
