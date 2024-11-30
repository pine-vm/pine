module CompilationInterface.ElmMake exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfaceelmmake-elm-module>
-}

import Basics


elm_make____src_Frontend_ElmSilentTeacher_elm : { javascript : { utf8 : String } }
elm_make____src_Frontend_ElmSilentTeacher_elm =
    { javascript = { utf8 = "The compiler replaces this declaration." }
    }
