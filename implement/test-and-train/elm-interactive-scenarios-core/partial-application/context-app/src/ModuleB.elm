module ModuleB exposing (partially_applied_b)

import ModuleA exposing (..)


partially_applied_b =
    ModuleA.partially_applied_a named_literal


named_literal =
    13
