module Backend.ExposeFunctionsToAdmin exposing (..)

import Backend.Main


addToCounter : Int -> Backend.Main.State -> Backend.Main.State
addToCounter addition stateBefore =
    stateBefore + addition
