module CompilationInterface.ElmMake exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfaceelmmake-elm-module>
-}

import Basics
import CompilerGenerated.EncodeBytes as EncodeBytes
import CompilationInterface.ElmMake.Generated_ElmMake


elm_make____src_Frontend_Main_elm : { debug : { base64 : String } }
elm_make____src_Frontend_Main_elm =
    { debug = { base64 = CompilationInterface.ElmMake.Generated_ElmMake.elm_make_output_src_Frontend_Main_elm_debug_html.base64
    }
    }
