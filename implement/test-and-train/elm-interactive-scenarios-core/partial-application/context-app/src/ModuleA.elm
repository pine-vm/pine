module ModuleA exposing (..)


partially_applied_a =
    function_with_three_parameters 11


function_with_three_parameters param0 param1 param2 =
    [ param0, param1, param0, param2, param1 ]


same_module_partially_applied_b =
    partially_applied_a named_literal


named_literal =
    71
