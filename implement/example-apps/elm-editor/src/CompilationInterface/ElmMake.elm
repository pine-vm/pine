{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfaceelmmake-elm-module> -}


module CompilationInterface.ElmMake exposing (..)
import CompilerGenerated.EncodeBytes as EncodeBytes
import CompilationInterface.ElmMake.Generated_ElmMake


elm_make____src_Frontend_Main_elm : { javascript : { base64 : String } }
elm_make____src_Frontend_Main_elm =
    { javascript = { base64 = CompilationInterface.ElmMake.Generated_ElmMake.elm_make_output_src_Frontend_Main_elm_javascript.base64
    }
    }


elm_make____src_LanguageServiceWorker_elm : { javascript : { base64 : String } }
elm_make____src_LanguageServiceWorker_elm =
    { javascript = { base64 = CompilationInterface.ElmMake.Generated_ElmMake.elm_make_output_src_LanguageServiceWorker_elm_javascript.base64
    }
    }
