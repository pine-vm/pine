module Json.Encode exposing (..)


type Value
    = NullValue
    | BoolValue Bool
    | IntValue Int
    | StringValue String
    | ArrayValue (List Value)
    | ObjectValue (List ( String, Value ))


null : Value
null =
    NullValue


bool : Bool -> Value
bool =
    BoolValue


int : Int -> Value
int =
    IntValue


string : String -> Value
string =
    StringValue


list : (item -> Value) -> List item -> Value
list encodeItem items =
    ArrayValue (List.map encodeItem items)


object : List ( String, Value ) -> Value
object =
    ObjectValue


encode : Int -> Value -> String
encode indent value =
    case value of
        NullValue ->
            "null"

        BoolValue boolVal ->
            if boolVal then
                "true"

            else
                "false"

        IntValue intVal ->
            String.fromInt intVal

        StringValue stringVal ->
            "\"" ++ escapeString stringVal ++ "\""

        ArrayValue values ->
            "[" ++ String.join "," (List.map (encode indent) values) ++ "]"

        ObjectValue fields ->
            "{" ++ String.join "," (List.map (encodeField indent) fields) ++ "}"


encodeField : Int -> ( String, Value ) -> String
encodeField indent ( key, value ) =
    "\"" ++ escapeString key ++ "\":" ++ encode indent value


escapeString : String -> String
escapeString stringVal =
    String.join "" (List.map escapeChar (String.toList stringVal))


escapeChar : Char -> String
escapeChar char =
    case Char.toCode char of
        8 ->
            "\\b"

        9 ->
            "\\t"

        10 ->
            "\\n"

        12 ->
            "\\f"

        13 ->
            "\\r"

        34 ->
            "\\\""

        92 ->
            "\\\\"

        _ ->
            String.fromChar char