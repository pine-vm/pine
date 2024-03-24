module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import HttpApi
import Json.Decode


jsonDecodeClientRequest : Json.Decode.Decoder HttpApi.ClientRequest
jsonDecodeClientRequest =
    Json.Decode.fail "The compiler replaces this declaration."
