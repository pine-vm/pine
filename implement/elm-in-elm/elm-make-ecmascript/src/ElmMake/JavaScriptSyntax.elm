module ElmMake.JavaScriptSyntax exposing (Expr(..), Stmt(..), asExpression, asStatements, call, curryFunction, renderExpression, renderStatement)


{-| A small ECMAScript IR keeps layout, grouping, and control flow independent
of Elm name resolution and lowering. Layout matches Elm 0.19.1's development
output: structural expressions force multiline parent calls, regardless of width.
-}
type Expr
    = Ref String
    | Literal String
    | Array (List Expr)
    | Object (List ( String, Expr ))
    | Access Expr String
    | Unary String Expr
    | Binary String Expr Expr
    | Conditional Expr Expr Expr
    | Call Expr (List Expr)
    | Function (List String) (List Stmt)


type Stmt
    = Return Expr
    | Var String Expr
    | Vars (List ( String, Expr ))
    | Assign String Expr
    | If Expr (List Stmt) (List Stmt)
    | Switch Expr (List ( Maybe Expr, List Stmt ))
    | Loop String (List Stmt)
    | Continue String
    | Break String
    | Throw String
    | FunctionDeclaration String (List String) (List Stmt)
    | TryCatch (List Stmt) String (List Stmt)
    | ThrowValue Expr


call : Expr -> List Expr -> Expr
call function arguments =
    case List.length arguments of
        0 ->
            function

        1 ->
            Call function arguments

        count ->
            if count <= 9 then
                Call (Ref ("A" ++ String.fromInt count)) (function :: arguments)

            else
                List.foldl (\argument target -> Call target [ argument ]) function arguments


asExpression : List Stmt -> Expr
asExpression statements =
    case statements of
        [ Return expr ] ->
            expr

        _ ->
            Call (Function [] statements) []


asStatements : Expr -> List Stmt
asStatements expression =
    case expression of
        Call (Function [] statements) [] ->
            statements

        _ ->
            [ Return expression ]


curryFunction : List String -> List Stmt -> Expr
curryFunction parameters body =
    let
        count =
            List.length parameters
    in
    if count >= 2 && count <= 9 then
        Call (Ref ("F" ++ String.fromInt count)) [ Function parameters body ]

    else
        List.foldr (\parameter inner -> Function [ parameter ] (asStatements inner)) (asExpression body) parameters


renderExpression : Expr -> String
renderExpression expression =
    (render 0 False expression).text


renderStatement : Stmt -> String
renderStatement =
    statement 0


type alias Layout =
    { many : Bool, text : String }


tabs : Int -> String
tabs level =
    String.repeat level "\t"


render : Int -> Bool -> Expr -> Layout
render level atomic expression =
    let
        one text =
            { many = False, text = text }

        grouped text =
            if atomic then
                "(" ++ text ++ ")"

            else
                text

        argumentsLayout prefix suffix values =
            if List.any .many values then
                prefix
                    ++ "\n"
                    ++ tabs (level + 1)
                    ++ String.join (",\n" ++ tabs (level + 1)) (List.map .text values)
                    ++ suffix

            else
                prefix ++ String.join ", " (List.map .text values) ++ suffix
    in
    case expression of
        Ref name ->
            one name

        Literal value ->
            one value

        Array entries ->
            let
                values =
                    List.map (render level False) entries
            in
            { many = True
            , text =
                argumentsLayout "["
                    (if List.any .many values then
                        "\n" ++ tabs level ++ "]"

                     else
                        "]"
                    )
                    values
            }

        Object fields ->
            let
                values =
                    List.map
                        (\( name, value ) ->
                            let
                                layout =
                                    render (level + 1) False value
                            in
                            { layout | text = name ++ ": " ++ layout.text }
                        )
                        fields
            in
            { many = True
            , text =
                argumentsLayout "{"
                    (if List.any .many values then
                        "\n" ++ tabs level ++ "}"

                     else
                        "}"
                    )
                    values
            }

        Access target field ->
            let
                layout =
                    render level True target
            in
            { layout | text = layout.text ++ "." ++ field }

        Unary operator value ->
            let
                layout =
                    render level True value
            in
            { layout | text = grouped (operator ++ layout.text) }

        Binary operator left right ->
            let
                lhs =
                    render level True left

                rhs =
                    render level True right
            in
            { many = lhs.many || rhs.many, text = grouped (lhs.text ++ " " ++ operator ++ " " ++ rhs.text) }

        Conditional condition yes no ->
            { many = True
            , text = grouped ((render level True condition).text ++ " ? " ++ (render level True yes).text ++ " : " ++ (render level True no).text)
            }

        Call function arguments ->
            { many = True
            , text =
                argumentsLayout ((render level True function).text ++ "(")
                    ")"
                    (List.map (render (level + 1) False) arguments)
            }

        Function parameters body ->
            { many = True
            , text =
                "function ("
                    ++ String.join ", " parameters
                    ++ ") {\n"
                    ++ String.concat (List.map (statement (level + 1)) body)
                    ++ tabs level
                    ++ "}"
            }


statement : Int -> Stmt -> String
statement level stmt =
    let
        indent =
            tabs level

        expr value =
            (render level False value).text

        block body =
            String.concat (List.map (statement (level + 1)) body)
    in
    case stmt of
        Return value ->
            indent ++ "return " ++ expr value ++ ";\n"

        Var name value ->
            indent ++ "var " ++ name ++ " = " ++ expr value ++ ";\n"

        Vars bindings ->
            if List.isEmpty bindings then
                ""

            else
                indent
                    ++ "var "
                    ++ String.join (",\n" ++ tabs (level + 1)) (List.map (\( name, value ) -> name ++ " = " ++ expr value) bindings)
                    ++ ";\n"

        Assign name value ->
            indent ++ name ++ " = " ++ expr value ++ ";\n"

        If condition yes no ->
            indent
                ++ "if ("
                ++ expr condition
                ++ ") {\n"
                ++ block yes
                ++ indent
                ++ "} else {\n"
                ++ block no
                ++ indent
                ++ "}\n"

        Switch value clauses ->
            indent
                ++ "switch ("
                ++ expr value
                ++ ") {\n"
                ++ String.concat
                    (List.map
                        (\( match, body ) ->
                            tabs (level + 1)
                                ++ (case match of
                                        Just key ->
                                            "case " ++ expr key

                                        Nothing ->
                                            "default"
                                   )
                                ++ ":\n"
                                ++ String.concat (List.map (statement (level + 2)) body)
                        )
                        clauses
                    )
                ++ indent
                ++ "}\n"

        Loop name body ->
            indent ++ name ++ ":\n" ++ indent ++ "while (true) {\n" ++ block body ++ indent ++ "}\n"

        Continue name ->
            indent ++ "continue " ++ name ++ ";\n"

        Break name ->
            indent ++ "break " ++ name ++ ";\n"

        Throw message ->
            indent ++ "throw new Error(" ++ message ++ ");\n"

        FunctionDeclaration name parameters body ->
            indent ++ "function " ++ name ++ "(" ++ String.join ", " parameters ++ ") {\n" ++ block body ++ indent ++ "}\n"

        TryCatch body name handler ->
            -- Classic cyclic initialization closes the catch immediately after its final throw.
            indent ++ "try {\n" ++ block body ++ indent ++ "} catch (" ++ name ++ ") {\n" ++ String.dropRight 1 (block handler) ++ "}\n"

        ThrowValue value ->
            indent ++ "throw " ++ expr value ++ ";\n"
