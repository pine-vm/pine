{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfaceelmmake-elm-module> -}


module CompilationInterface.ElmMake exposing (..)


elm_make____src_Frontend_Main_elm : { javascript : { base64 : String } }
elm_make____src_Frontend_Main_elm =
    { javascript = { base64 = "The compiler replaces this declaration." }
    }


elm_make____src_LanguageServiceWorker_elm : { javascript : { base64 : String } }
elm_make____src_LanguageServiceWorker_elm =
    { javascript = { base64 = "The compiler replaces this declaration." }
    }
