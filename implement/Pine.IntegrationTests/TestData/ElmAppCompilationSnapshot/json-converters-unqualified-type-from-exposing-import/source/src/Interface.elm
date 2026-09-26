module Interface exposing (..)

import Common.EffectOnWindow exposing (MouseButton(..), VirtualKeyCode(..))


type Request
    = MouseClick MouseButton
    | KeyDown VirtualKeyCode
