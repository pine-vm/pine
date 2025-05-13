module CompilationInterface.SourceFiles exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module>
-}

import Basics


file____README_md : { utf8 : String }
file____README_md =
    { utf8 = "The compiler replaces this value." }
