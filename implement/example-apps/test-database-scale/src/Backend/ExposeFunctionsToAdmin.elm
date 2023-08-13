module Backend.ExposeFunctionsToAdmin exposing (..)

import Backend.State
import Dict


setStoreEntryBase64 : { entryId : Int, entryBase64 : String } -> Backend.State.State -> Backend.State.State
setStoreEntryBase64 { entryId, entryBase64 } state =
    { state
        | store = state.store |> Dict.insert entryId { entryBase64 = entryBase64 }
    }
