module CompilationInterface.ElmMake exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfaceelmmake-elm-module>
-}

import Basics


elm_make____src_Frontend_Main_elm : { debug : { base64 : String } }
elm_make____src_Frontend_Main_elm =
    { debug = { base64 = "The compiler replaces this value." } }
