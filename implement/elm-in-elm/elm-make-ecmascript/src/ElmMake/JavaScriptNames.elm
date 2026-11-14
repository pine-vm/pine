module ElmMake.JavaScriptNames exposing (field, local)

import Char


local : String -> String
local name =
    if List.member name (String.words "do if in NaN int for new try var let null true eval byte char goto long case else this void with enum false final float short break catch throw while class const super yield double native throws delete return switch typeof export import public static boolean default finally extends package private Infinity abstract volatile function continue debugger undefined arguments transient interface protected instanceof implements synchronized F2 F3 F4 F5 F6 F7 F8 F9 A2 A3 A4 A5 A6 A7 A8 A9") then
        "_" ++ name

    else
        name


field : Int -> String
field index =
    String.fromChar (Char.fromCode (97 + index))
