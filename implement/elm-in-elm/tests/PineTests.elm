module PineTests exposing (tests)

import BigInt
import Expect
import Pine exposing (Expression(..), Value(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Pine 2026"
        [ encodingTests
        , decodingTests
        , evaluationTests
        , builtinTests
        , valueEncodingTests
        ]


encodingTests : Test
encodingTests =
    let
        literal =
            LitralExpression (BlobValue [ 1, 2, 255 ])

        encodedLiteral =
            tagged "Litral" [ BlobValue [ 1, 2, 255 ] ]

        encodedEnvironment =
            tagged "Environment" []

        label =
            ListValue [ BlobValue [ 0, 255 ], ListValue [] ]
    in
    describe "Encoding"
        [ describe "Exact wire shapes"
            (List.map
                (\( name, expression, expected ) ->
                    test name <|
                        \_ ->
                            Expect.all
                                [ \_ -> Expect.equal expected (Pine.encodeExpressionAsValue expression)
                                , \_ -> Expect.equal (Ok expression) (Pine.parseExpressionFromValue expected)
                                ]
                                ()
                )
                [ ( "Litral embeds raw data", literal, encodedLiteral )
                , ( "empty List has no payload wrapper", ListExpression [], tagged "List" [] )
                , ( "List is flat"
                  , ListExpression [ literal, EnvironmentExpression ]
                  , tagged "List" [ encodedLiteral, encodedEnvironment ]
                  )
                , ( "Builtin"
                  , BuiltinExpression "length" literal
                  , tagged "Builtin" [ Pine.valueFromString "length", encodedLiteral ]
                  )
                , ( "Conditional stores false before true"
                  , ConditionalExpression EnvironmentExpression literal (ListExpression [])
                  , tagged "Conditional" [ encodedEnvironment, encodedLiteral, tagged "List" [] ]
                  )
                , ( "Environment", EnvironmentExpression, encodedEnvironment )
                , ( "Eval stores code before environment"
                  , EvalExpression literal EnvironmentExpression
                  , tagged "Eval" [ encodedLiteral, encodedEnvironment ]
                  )
                , ( "Label preserves an arbitrary raw value"
                  , LabelExpression label literal
                  , tagged "Label" [ label, encodedLiteral ]
                  )
                ]
            )
        , describe "Expression and canonical wire roundtrips"
            (List.map
                (\( name, expression ) ->
                    test name <|
                        \_ ->
                            let
                                encoded =
                                    Pine.encodeExpressionAsValue expression

                                parsed =
                                    Pine.parseExpressionFromValue encoded
                            in
                            Expect.all
                                [ \_ -> Expect.equal (Ok expression) parsed
                                , \_ -> Expect.equal (Ok encoded) (Result.map Pine.encodeExpressionAsValue parsed)
                                ]
                                ()
                )
                expressionExamples
            )
        , describe "Encoding preserves evaluation, including errors"
            (List.map
                (\( name, expression ) ->
                    describe name
                        (List.map
                            (\( environmentName, environment ) ->
                                test ("environment " ++ environmentName) <|
                                    \_ ->
                                        Expect.equal (Pine.evalExpression expression environment)
                                            (Pine.parseExpressionFromValue (Pine.encodeExpressionAsValue expression)
                                                |> Result.andThen (\parsed -> Pine.evalExpression parsed environment)
                                            )
                            )
                            valueExamples
                        )
                )
                expressionExamples
            )
        ]


decodingTests : Test
decodingTests =
    let
        environment =
            tagged "Environment" []

        literal =
            tagged "Litral" [ ListValue [] ]

        malformed =
            BlobValue []

        fixedShapes =
            [ ( "Litral", [ ListValue [] ] )
            , ( "Builtin", [ Pine.valueFromString "head", environment ] )
            , ( "Conditional", [ literal, environment, environment ] )
            , ( "Condition", [ literal, environment, environment ] )
            , ( "Environment", [] )
            , ( "Eval", [ literal, environment ] )
            , ( "Label", [ malformed, environment ] )
            ]

        invalidChildren =
            [ ( "List item", tagged "List" [ literal, malformed ] )
            , ( "Builtin input", tagged "Builtin" [ Pine.valueFromString "head", malformed ] )
            , ( "condition", tagged "Conditional" [ malformed, literal, environment ] )
            , ( "false branch", tagged "Conditional" [ literal, malformed, environment ] )
            , ( "true branch", tagged "Conditional" [ literal, environment, malformed ] )
            , ( "Eval code", tagged "Eval" [ malformed, environment ] )
            , ( "Eval environment", tagged "Eval" [ literal, malformed ] )
            , ( "Label expression", tagged "Label" [ malformed, malformed ] )
            ]
    in
    describe "Decoding"
        [ describe "Reject malformed roots and legacy tags"
            (List.indexedMap
                (\index value ->
                    test (String.fromInt index)
                        (\_ -> expectError (Pine.parseExpressionFromValue value))
                )
                [ BlobValue []
                , ListValue []
                , ListValue [ BlobValue [ 0 ] ]
                , tagged "Unknown" []
                , tagged "Literal" [ ListValue [] ]
                , tagged "KernelApplication" [ Pine.valueFromString "head", environment ]
                , tagged "ParseAndEval" [ literal, environment ]
                , tagged "StringTag" [ Pine.valueFromString "label", environment ]
                , tagged "List" [ ListValue [ literal ] ]
                , tagged "Builtin" [ ListValue [], environment ]
                ]
            )
        , describe "Exact arity for every fixed-size variant"
            (List.concatMap
                (\( name, arguments ) ->
                    [ test (name ++ " rejects extra arguments")
                        (\_ -> expectError (Pine.parseExpressionFromValue (tagged name (arguments ++ [ malformed ]))))
                    ]
                        ++ (if List.isEmpty arguments then
                                []

                            else
                                [ test (name ++ " rejects missing arguments")
                                    (\_ -> expectError (Pine.parseExpressionFromValue (tagged name (List.drop 1 arguments))))
                                ]
                           )
                )
                fixedShapes
            )
        , describe "Recursively validate every operand"
            (List.map
                (\( name, value ) ->
                    test name (\_ -> expectError (Pine.parseExpressionFromValue value))
                )
                invalidChildren
            )
        , test "decoder validates even an unselected conditional branch" <|
            \_ ->
                tagged "Conditional"
                    [ tagged "Litral" [ Pine.trueValue ], malformed, literal ]
                    |> Pine.parseExpressionFromValue
                    |> expectError
        , test "literal data is not recursively decoded" <|
            \_ ->
                Expect.equal (Ok (LitralExpression malformed))
                    (Pine.parseExpressionFromValue (tagged "Litral" [ malformed ]))
        , describe "Label data is not decoded as an expression or string"
            (List.map
                (\( name, label ) ->
                    test name <|
                        \_ ->
                            Expect.equal (Ok (LabelExpression label EnvironmentExpression))
                                (Pine.parseExpressionFromValue (tagged "Label" [ label, environment ]))
                )
                valueExamples
            )
        , test "Condition alias decodes and re-encodes canonically as Conditional" <|
            \_ ->
                Expect.equal (Ok (tagged "Conditional" [ literal, environment, environment ]))
                    (Pine.parseExpressionFromValue (tagged "Condition" [ literal, environment, environment ])
                        |> Result.map Pine.encodeExpressionAsValue
                    )
        , test "unknown builtin names decode but fail when evaluated" <|
            \_ ->
                Pine.parseExpressionFromValue (tagged "Builtin" [ Pine.valueFromString "unknown", literal ])
                    |> Result.andThen (\expression -> Pine.evalExpression expression (ListValue []))
                    |> expectErrorContaining "Unknown builtin function"
        , test "nested decoding errors identify the operand" <|
            \_ ->
                Pine.parseExpressionFromValue (tagged "List" [ literal, malformed ])
                    |> expectErrorContaining "List item 1"
        ]


evaluationTests : Test
evaluationTests =
    let
        environment =
            ListValue [ BlobValue [ 7, 8 ], BlobValue [ 9 ] ]

        run expression =
            Pine.evalExpression expression environment

        crash =
            EvalExpression (LitralExpression (BlobValue [])) EnvironmentExpression

        head =
            BuiltinExpression "head" EnvironmentExpression

        number =
            BuiltinExpression "head"
                (BuiltinExpression "skip"
                    (ListExpression [ LitralExpression (Pine.valueFromInt 1), EnvironmentExpression ])
                )

        recursiveBody =
            ConditionalExpression
                (BuiltinExpression "equal"
                    (ListExpression [ number, LitralExpression (Pine.valueFromInt 0) ])
                )
                (EvalExpression head
                    (ListExpression
                        [ head
                        , BuiltinExpression "int_add"
                            (ListExpression [ number, LitralExpression (Pine.valueFromInt -1) ])
                        ]
                    )
                )
                number
    in
    describe "Evaluation"
        [ describe "Literal is independent of the environment"
            (List.map
                (\( name, literal ) ->
                    describe name
                        (List.map
                            (\( environmentName, env ) ->
                                test ("environment " ++ environmentName) <|
                                    \_ ->
                                        Expect.equal (Ok literal) (Pine.evalExpression (LitralExpression literal) env)
                            )
                            valueExamples
                        )
                )
                valueExamples
            )
        , describe "Environment returns the entire value, including blobs"
            (List.map
                (\( name, env ) ->
                    test name <|
                        \_ ->
                            Expect.equal (Ok env) (Pine.evalExpression EnvironmentExpression env)
                )
                valueExamples
            )
        , test "empty list" <|
            \_ -> Expect.equal (Ok (ListValue [])) (run (ListExpression []))
        , test "wide lists encode, decode and evaluate without consuming the call stack" <|
            \_ ->
                ListExpression (List.repeat 10000 EnvironmentExpression)
                    |> Pine.encodeExpressionAsValue
                    |> Pine.parseExpressionFromValue
                    |> Result.andThen run
                    |> Expect.equal (Ok (ListValue (List.repeat 10000 environment)))
        , test "list evaluation preserves order and environment" <|
            \_ ->
                Expect.equal (Ok (ListValue [ BlobValue [ 3 ], environment, BlobValue [ 7, 8 ] ]))
                    (run (ListExpression [ LitralExpression (BlobValue [ 3 ]), EnvironmentExpression, head ]))
        , test "Builtin evaluates its input expression" <|
            \_ ->
                Expect.equal (Ok (Pine.valueFromInt 2)) (run (BuiltinExpression "length" EnvironmentExpression))
        , describe "Label leaves evaluation unchanged"
            (List.map
                (\( name, label ) ->
                    test name <|
                        \_ ->
                            Expect.equal (Ok (BlobValue [ 7, 8 ])) (run (LabelExpression label head))
                )
                valueExamples
            )
        , test "Label does not swallow errors" <|
            \_ -> Expect.equal (run crash) (run (LabelExpression (BlobValue []) crash))
        , test "true selects only the true branch" <|
            \_ ->
                Expect.equal (Ok environment)
                    (run (ConditionalExpression (LitralExpression Pine.trueValue) crash EnvironmentExpression))
        , describe "Anything other than exactly blob [4] selects the false branch"
            (List.indexedMap
                (\index condition ->
                    test (String.fromInt index) <|
                        \_ ->
                            Expect.equal (Ok environment)
                                (run (ConditionalExpression (LitralExpression condition) EnvironmentExpression crash))
                )
                [ Pine.falseValue, BlobValue [], BlobValue [ 4, 0 ], ListValue [], Pine.valueFromInt 1 ]
            )
        , test "condition errors propagate" <|
            \_ ->
                run (ConditionalExpression crash EnvironmentExpression EnvironmentExpression)
                    |> expectErrorContaining "Conditional condition"
        , test "list element errors propagate with an index" <|
            \_ ->
                run (ListExpression [ EnvironmentExpression, crash ])
                    |> expectErrorContaining "List item 1"
        , test "builtin input errors propagate" <|
            \_ ->
                run (BuiltinExpression "head" crash)
                    |> expectErrorContaining "Builtin head input"
        , test "Eval decodes 2026 code and rebinds the environment" <|
            \_ ->
                Expect.equal (Ok (BlobValue [ 42 ]))
                    (run
                        (EvalExpression
                            (LitralExpression (Pine.encodeExpressionAsValue EnvironmentExpression))
                            (LitralExpression (BlobValue [ 42 ]))
                        )
                    )
        , test "both Eval operands are evaluated in the outer environment" <|
            \_ ->
                let
                    encodedHead =
                        Pine.encodeExpressionAsValue head

                    expression =
                        EvalExpression
                            (BuiltinExpression "head" EnvironmentExpression)
                            (BuiltinExpression "head"
                                (BuiltinExpression "skip"
                                    (ListExpression [ LitralExpression (Pine.valueFromInt 1), EnvironmentExpression ])
                                )
                            )
                in
                Expect.equal (Ok (BlobValue [ 99 ]))
                    (Pine.evalExpression expression
                        (ListValue [ encodedHead, ListValue [ BlobValue [ 99 ] ] ])
                    )
        , test "nested Eval has its own environment" <|
            \_ ->
                let
                    inner =
                        EvalExpression
                            (LitralExpression (Pine.encodeExpressionAsValue EnvironmentExpression))
                            EnvironmentExpression
                in
                Expect.equal (Ok (BlobValue [ 10 ]))
                    (run
                        (EvalExpression (LitralExpression (Pine.encodeExpressionAsValue inner))
                            (LitralExpression (BlobValue [ 10 ]))
                        )
                    )
        , test "Eval rejects invalid encoded values" <|
            \_ -> run crash |> expectErrorContaining "Eval decode"
        , test "Eval environment errors propagate" <|
            \_ ->
                run (EvalExpression EnvironmentExpression crash)
                    |> expectErrorContaining "Eval environment"
        , test "Eval code operand errors propagate" <|
            \_ ->
                run (EvalExpression crash EnvironmentExpression)
                    |> expectErrorContaining "Eval encoded expression"
        , test "Eval supports recursion through finite environment values" <|
            \_ ->
                Expect.equal (Ok (Pine.valueFromInt 0))
                    (Pine.evalExpression recursiveBody
                        (ListValue [ Pine.encodeExpressionAsValue recursiveBody, Pine.valueFromInt 20 ])
                    )
        , test "literal-wrapped invalid code in an unselected branch stays lazy" <|
            \_ ->
                ConditionalExpression (LitralExpression Pine.trueValue) crash EnvironmentExpression
                    |> Pine.encodeExpressionAsValue
                    |> Pine.parseExpressionFromValue
                    |> Result.andThen run
                    |> Expect.equal (Ok environment)
        ]


builtinTests : Test
builtinTests =
    let
        int =
            Pine.valueFromInt

        ints values =
            ListValue (List.map int values)

        empty =
            ListValue []

        examples =
            [ ( "equal", BlobValue [ 5, 5 ], Pine.trueValue )
            , ( "equal", BlobValue [ 5, 6 ], Pine.falseValue )
            , ( "equal", empty, Pine.trueValue )
            , ( "equal", ListValue [ empty, empty ], Pine.trueValue )
            , ( "equal", ListValue [ empty, BlobValue [] ], Pine.falseValue )
            , ( "negate", BlobValue [ 4 ], Pine.falseValue )
            , ( "negate", int -258, int 258 )
            , ( "negate", BlobValue [ 0 ], empty )
            , ( "length", BlobValue [ 1, 2, 3 ], int 3 )
            , ( "length", ListValue [ empty, empty ], int 2 )
            , ( "head", BlobValue [ 1, 2 ], BlobValue [ 1 ] )
            , ( "head", BlobValue [], BlobValue [] )
            , ( "head", ints [ 4, 5 ], int 4 )
            , ( "head", empty, empty )
            , ( "skip", ListValue [ int 1, BlobValue [ 8, 9 ] ], BlobValue [ 9 ] )
            , ( "skip", ListValue [ int -1, BlobValue [ 8, 9 ] ], BlobValue [ 8, 9 ] )
            , ( "skip", ListValue [ int 8, BlobValue [ 8, 9 ] ], BlobValue [] )
            , ( "skip", ListValue [ int 1, ints [ 8, 9 ] ], ints [ 9 ] )
            , ( "skip", empty, empty )
            , ( "take", ListValue [ int 1, BlobValue [ 8, 9 ] ], BlobValue [ 8 ] )
            , ( "take", ListValue [ int -1, BlobValue [ 8, 9 ] ], BlobValue [] )
            , ( "take", ListValue [ int 8, ints [ 8, 9 ] ], ints [ 8, 9 ] )
            , ( "take", ListValue [ int 1, ints [ 8, 9 ] ], ints [ 8 ] )
            , ( "take", ListValue [ Pine.trueValue, empty ], empty )
            , ( "reverse", BlobValue [ 1, 2, 3 ], BlobValue [ 3, 2, 1 ] )
            , ( "reverse", ints [ 1, 2 ], ints [ 2, 1 ] )
            , ( "concat", ListValue [ BlobValue [ 1 ], BlobValue [ 2, 3 ] ], BlobValue [ 1, 2, 3 ] )
            , ( "concat", ListValue [ empty, BlobValue [ 1 ], empty, BlobValue [ 2 ] ], BlobValue [ 1, 2 ] )
            , ( "concat", ListValue [ ints [ 1 ], ints [ 2, 3 ] ], ints [ 1, 2, 3 ] )
            , ( "concat", ListValue [ ints [ 1 ], BlobValue [ 2 ] ], empty )
            , ( "concat", ListValue [ BlobValue [], ints [ 1 ] ], empty )
            , ( "concat", empty, empty )
            , ( "int_add", ints [ 257, -2, 1 ], int 256 )
            , ( "int_add", empty, int 0 )
            , ( "int_add", ListValue [ Pine.trueValue ], empty )
            , ( "int_add", ListValue [ BlobValue [ 4, 0, 1 ], BlobValue [ 2, 0, 1 ] ], int 0 )
            , ( "int_mul", ints [ -3, 257 ], int -771 )
            , ( "int_mul", empty, int 1 )
            , ( "int_mul", ListValue [ BlobValue [ 9, 1 ] ], empty )
            , ( "int_is_sorted_asc", ints [ -3, -3, 0, 2 ], Pine.trueValue )
            , ( "int_is_sorted_asc", ints [ 2, 1 ], Pine.falseValue )
            , ( "int_is_sorted_asc", empty, Pine.trueValue )
            , ( "int_is_sorted_asc", ListValue [ Pine.trueValue ], empty )
            , ( "bit_and", ListValue [ BlobValue [ 255, 170 ], BlobValue [ 15 ] ], BlobValue [ 10 ] )
            , ( "bit_and", ListValue [ BlobValue [], BlobValue [ 1 ] ], BlobValue [] )
            , ( "bit_or", ListValue [ BlobValue [ 16, 128 ], BlobValue [ 1 ] ], BlobValue [ 16, 129 ] )
            , ( "bit_xor", ListValue [ BlobValue [ 16, 128 ], BlobValue [ 255 ] ], BlobValue [ 16, 127 ] )
            , ( "bit_not", BlobValue [ 0, 15, 255 ], BlobValue [ 255, 240, 0 ] )
            , ( "bit_not", empty, empty )
            , ( "bit_shift_left", ListValue [ int 4, BlobValue [ 18, 52 ] ], BlobValue [ 35, 64 ] )
            , ( "bit_shift_left", ListValue [ int 8, BlobValue [ 18, 52 ] ], BlobValue [ 52, 0 ] )
            , ( "bit_shift_right", ListValue [ int 4, BlobValue [ 18, 52 ] ], BlobValue [ 1, 35 ] )
            , ( "bit_shift_right", ListValue [ int 8, BlobValue [ 18, 52 ] ], BlobValue [ 0, 18 ] )
            , ( "bit_shift_right", ListValue [ int 16, BlobValue [ 18, 52 ] ], BlobValue [ 0, 0 ] )
            ]
    in
    describe "Builtins"
        [ describe "Reference examples for all 17 functions"
            (List.indexedMap
                (\index ( name, input, expected ) ->
                    test (name ++ " " ++ String.fromInt index) <|
                        \_ ->
                            Expect.equal (Ok expected)
                                (Pine.evalExpression (BuiltinExpression name (LitralExpression input)) empty)
                )
                examples
            )
        , describe "Malformed inputs use the specified empty-list result"
            (List.map
                (\name ->
                    test name <|
                        \_ -> Expect.equal (Ok empty) (Pine.applyBuiltinFunction name (BlobValue []))
                )
                [ "skip", "take", "concat", "int_add", "int_mul", "int_is_sorted_asc", "bit_and", "bit_or", "bit_xor", "bit_shift_left", "bit_shift_right" ]
            )
        , describe "Bitwise folds validate all operands"
            (List.map
                (\name ->
                    test name <|
                        \_ ->
                            Expect.all
                                [ \_ -> Expect.equal (Ok empty) (Pine.applyBuiltinFunction name empty)
                                , \_ -> Expect.equal (Ok empty) (Pine.applyBuiltinFunction name (ListValue [ BlobValue [ 1 ], ints [ 2 ] ]))
                                , \_ -> Expect.equal (Ok (BlobValue [ 3 ])) (Pine.applyBuiltinFunction name (ListValue [ BlobValue [ 3 ] ]))
                                ]
                                ()
                )
                [ "bit_and", "bit_or", "bit_xor" ]
            )
        , describe "Reverse is an involution for both value kinds"
            (List.map
                (\( name, value ) ->
                    test name <|
                        \_ ->
                            Expect.equal (Ok value)
                                (Pine.applyBuiltinFunction "reverse" value
                                    |> Result.andThen (Pine.applyBuiltinFunction "reverse")
                                )
                )
                valueExamples
            )
        , describe "Take and skip partition either sequence kind"
            (List.concatMap
                (\( name, value ) ->
                    List.map
                        (\count ->
                            test (name ++ ", count " ++ String.fromInt count) <|
                                \_ ->
                                    Result.map2
                                        (\prefix suffix -> ListValue [ prefix, suffix ])
                                        (Pine.applyBuiltinFunction "take" (ListValue [ int count, value ]))
                                        (Pine.applyBuiltinFunction "skip" (ListValue [ int count, value ]))
                                        |> Result.andThen (Pine.applyBuiltinFunction "concat")
                                        |> Expect.equal (Ok value)
                        )
                        [ -10, -1, 0, 1, 2, 3, 4, 100 ]
                )
                valueExamples
            )
        , describe "Bit shifts preserve width at bit, byte and blob boundaries"
            (List.map
                (\( count, left, right ) ->
                    test ("count " ++ String.fromInt count) <|
                        \_ ->
                            Expect.all
                                [ \_ ->
                                    Expect.equal (Ok (BlobValue left))
                                        (Pine.applyBuiltinFunction "bit_shift_left" (ListValue [ int count, BlobValue [ 129, 165 ] ]))
                                , \_ ->
                                    Expect.equal (Ok (BlobValue right))
                                        (Pine.applyBuiltinFunction "bit_shift_right" (ListValue [ int count, BlobValue [ 129, 165 ] ]))
                                ]
                                ()
                )
                [ ( 0, [ 129, 165 ], [ 129, 165 ] )
                , ( 1, [ 3, 74 ], [ 64, 210 ] )
                , ( 7, [ 210, 128 ], [ 1, 3 ] )
                , ( 8, [ 165, 0 ], [ 0, 129 ] )
                , ( 9, [ 74, 0 ], [ 0, 64 ] )
                , ( 15, [ 128, 0 ], [ 0, 1 ] )
                , ( 16, [ 0, 0 ], [ 0, 0 ] )
                , ( 17, [ 0, 0 ], [ 0, 0 ] )
                , ( 24, [ 0, 0 ], [ 0, 0 ] )
                ]
            )
        , describe "Bit shifts of zero and all-set blobs"
            (List.concatMap
                (\bytes ->
                    List.map
                        (\count ->
                            test (Debug.toString bytes ++ ", count " ++ String.fromInt count) <|
                                \_ ->
                                    let
                                        number =
                                            List.foldl (\byte aggregate -> aggregate * 256 + byte) 0 bytes

                                        fromNumber n =
                                            BlobValue [ n // 256, modBy 256 n ]
                                    in
                                    Expect.all
                                        [ \_ ->
                                            Expect.equal (Ok (fromNumber (modBy 65536 (number * 2 ^ min 16 count))))
                                                (Pine.applyBuiltinFunction "bit_shift_left" (ListValue [ int count, BlobValue bytes ]))
                                        , \_ ->
                                            Expect.equal (Ok (fromNumber (number // 2 ^ min 16 count)))
                                                (Pine.applyBuiltinFunction "bit_shift_right" (ListValue [ int count, BlobValue bytes ]))
                                        ]
                                        ()
                        )
                        [ 0, 1, 8, 15, 16, 24 ]
                )
                [ [ 0, 0 ], [ 255, 255 ] ]
            )
        , test "unknown builtin is an explicit error" <|
            \_ -> Pine.applyBuiltinFunction "not_a_builtin" empty |> expectErrorContaining "not_a_builtin"
        , test "integer arithmetic and comparisons preserve precision beyond native integers" <|
            \_ ->
                let
                    positive =
                        BlobValue (4 :: 1 :: List.repeat 16 0)

                    negative =
                        BlobValue (2 :: 1 :: List.repeat 16 0)

                    doubled =
                        BlobValue (4 :: 2 :: List.repeat 16 0)
                in
                Expect.all
                    [ \_ ->
                        Expect.equal (Ok (int 0))
                            (Pine.applyBuiltinFunction "int_add" (ListValue [ positive, negative ]))
                    , \_ ->
                        Expect.equal (Ok doubled)
                            (Pine.applyBuiltinFunction "int_mul" (ListValue [ positive, int 2 ]))
                    , \_ ->
                        Expect.equal (Ok Pine.trueValue)
                            (Pine.applyBuiltinFunction "int_is_sorted_asc" (ListValue [ negative, int 0, positive ]))
                    ]
                    ()
        , test "arbitrarily large slice and shift counts are bounded before conversion" <|
            \_ ->
                let
                    huge =
                        BlobValue (4 :: List.repeat 40 255)
                in
                Expect.all
                    [ \_ ->
                        Expect.equal (Ok (BlobValue []))
                            (Pine.applyBuiltinFunction "skip" (ListValue [ huge, BlobValue [ 1, 2 ] ]))
                    , \_ ->
                        Expect.equal (Ok (BlobValue [ 1, 2 ]))
                            (Pine.applyBuiltinFunction "take" (ListValue [ huge, BlobValue [ 1, 2 ] ]))
                    , \_ ->
                        Expect.equal (Ok (BlobValue [ 0, 0 ]))
                            (Pine.applyBuiltinFunction "bit_shift_left" (ListValue [ huge, BlobValue [ 1, 2 ] ]))
                    ]
                    ()
        ]


valueEncodingTests : Test
valueEncodingTests =
    describe "Value encodings"
        [ describe "UTF-32 reference encodings and roundtrips"
            (List.map
                (\( name, text, bytes ) ->
                    test name <|
                        \_ ->
                            let
                                encoded =
                                    BlobValue bytes
                            in
                            Expect.all
                                [ \_ -> Expect.equal encoded (Pine.valueFromString text)
                                , \_ -> Expect.equal (Ok text) (Pine.stringFromValue encoded)
                                ]
                                ()
                )
                [ ( "empty", "", [] )
                , ( "ASCII", "Az", [ 0, 0, 0, 65, 0, 0, 0, 122 ] )
                , ( "control characters", "\u{0000}\n\t", [ 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 9 ] )
                , ( "ASCII boundary", "\u{007F}\u{0080}", [ 0, 0, 0, 127, 0, 0, 0, 128 ] )
                , ( "surrogate boundaries", "\u{D7FF}\u{E000}", [ 0, 0, 215, 255, 0, 0, 224, 0 ] )
                , ( "plane boundary", String.fromList [ Char.fromCode 65535, Char.fromCode 65536 ], [ 0, 0, 255, 255, 0, 1, 0, 0 ] )
                , ( "maximum scalar", "\u{10FFFF}", [ 0, 16, 255, 255 ] )
                ]
            )
        , test "strings use four big-endian bytes per Unicode scalar" <|
            \_ ->
                Expect.equal (BlobValue [ 0, 0, 0, 65, 0, 1, 246, 0 ])
                    (Pine.valueFromString "A😀")
        , describe "Invalid string encodings are errors"
            (List.indexedMap
                (\index value ->
                    test (String.fromInt index) (\_ -> expectError (Pine.stringFromValue value))
                )
                [ ListValue []
                , BlobValue [ 0 ]
                , BlobValue [ 0, 0, 0 ]
                , BlobValue [ 0, 0, 216, 0 ]
                , BlobValue [ 0, 17, 0, 0 ]
                , BlobValue [ 256, 0, 0, 0 ]
                , BlobValue [ 0, 0, 0, -1 ]
                ]
            )
        , describe "Signed integer reference encodings and roundtrips"
            (List.map
                (\( integer, bytes ) ->
                    test (String.fromInt integer) <|
                        \_ ->
                            let
                                encoded =
                                    BlobValue bytes
                            in
                            Expect.all
                                [ \_ -> Expect.equal encoded (Pine.valueFromInt integer)
                                , \_ ->
                                    Expect.equal (Ok (String.fromInt integer))
                                        (Pine.bigIntFromValue encoded |> Result.map BigInt.toString)
                                ]
                                ()
                )
                [ ( -2147483648, [ 2, 128, 0, 0, 0 ] )
                , ( -65536, [ 2, 1, 0, 0 ] )
                , ( -256, [ 2, 1, 0 ] )
                , ( -255, [ 2, 255 ] )
                , ( -1, [ 2, 1 ] )
                , ( 0, [ 4, 0 ] )
                , ( 1, [ 4, 1 ] )
                , ( 255, [ 4, 255 ] )
                , ( 256, [ 4, 1, 0 ] )
                , ( 65535, [ 4, 255, 255 ] )
                , ( 65536, [ 4, 1, 0, 0 ] )
                , ( 2147483647, [ 4, 127, 255, 255, 255 ] )
                ]
            )
        , describe "Arbitrary-precision integer reference encodings and roundtrips"
            (List.map
                (\( decimal, bytes ) ->
                    test decimal <|
                        \_ ->
                            let
                                encoded =
                                    BlobValue bytes

                                decoded =
                                    Pine.bigIntFromValue encoded
                            in
                            Expect.all
                                [ \_ -> Expect.equal (Ok decimal) (Result.map BigInt.toString decoded)
                                , \_ -> Expect.equal (Ok encoded) (Result.map Pine.valueFromBigInt decoded)
                                ]
                                ()
                )
                [ ( "18446744073709551615", 4 :: List.repeat 8 255 )
                , ( "-18446744073709551615", 2 :: List.repeat 8 255 )
                , ( "18446744073709551616", 4 :: 1 :: List.repeat 8 0 )
                , ( "-18446744073709551616", 2 :: 1 :: List.repeat 8 0 )
                , ( "340282366920938463463374607431768211456", 4 :: 1 :: List.repeat 16 0 )
                , ( "-340282366920938463463374607431768211456", 2 :: 1 :: List.repeat 16 0 )
                ]
            )
        , test "zero and signed multibyte integers have canonical encodings" <|
            \_ ->
                Expect.equal
                    [ BlobValue [ 4, 0 ], BlobValue [ 4, 1, 0 ], BlobValue [ 2, 1, 0 ] ]
                    (List.map Pine.valueFromInt [ 0, 256, -256 ])
        , describe "Invalid integer encodings are errors"
            (List.indexedMap
                (\index value ->
                    test (String.fromInt index) (\_ -> expectError (Pine.bigIntFromValue value))
                )
                [ ListValue [], BlobValue [], Pine.trueValue, Pine.falseValue, BlobValue [ 0, 1 ], BlobValue [ 4, 256 ] ]
            )
        ]


tagged : String -> List Value -> Value
tagged name arguments =
    ListValue (Pine.valueFromString name :: arguments)


valueExamples : List ( String, Value )
valueExamples =
    [ ( "empty blob", BlobValue [] )
    , ( "zero byte", BlobValue [ 0 ] )
    , ( "byte boundaries", BlobValue [ 0, 1, 127, 128, 255 ] )
    , ( "true", Pine.trueValue )
    , ( "false", Pine.falseValue )
    , ( "empty list", ListValue [] )
    , ( "singleton list", ListValue [ BlobValue [ 255 ] ] )
    , ( "mixed nested list", ListValue [ BlobValue [], ListValue [ ListValue [], BlobValue [ 0, 255 ] ], BlobValue [ 42 ] ] )
    , ( "encoded expression", tagged "Environment" [] )
    , ( "malformed expression", tagged "Eval" [ BlobValue [] ] )
    ]


expressionExamples : List ( String, Expression )
expressionExamples =
    let
        literal =
            LitralExpression (BlobValue [ 0, 255 ])

        invalidEval =
            EvalExpression (LitralExpression (BlobValue [])) EnvironmentExpression
    in
    List.map (\( name, value ) -> ( "literal " ++ name, LitralExpression value )) valueExamples
        ++ [ ( "environment", EnvironmentExpression )
           , ( "empty list", ListExpression [] )
           , ( "nested list", ListExpression [ literal, ListExpression [ EnvironmentExpression, ListExpression [] ] ] )
           , ( "builtin", BuiltinExpression "length" EnvironmentExpression )
           , ( "unknown builtin", BuiltinExpression "unknown" literal )
           , ( "empty builtin name", BuiltinExpression "" literal )
           , ( "true conditional with an invalid unselected branch", ConditionalExpression (LitralExpression Pine.trueValue) invalidEval literal )
           , ( "false conditional with an invalid unselected branch", ConditionalExpression (LitralExpression Pine.falseValue) literal invalidEval )
           , ( "conditional depending on the environment", ConditionalExpression EnvironmentExpression literal (ListExpression []) )
           , ( "eval with a rebound environment", EvalExpression (LitralExpression (tagged "Environment" [])) literal )
           , ( "invalid eval", invalidEval )
           , ( "builtin input error", BuiltinExpression "head" invalidEval )
           , ( "list item error", ListExpression [ literal, invalidEval ] )
           , ( "label with raw data", LabelExpression (tagged "Unknown" [ BlobValue [ 0, 255 ] ]) EnvironmentExpression )
           , ( "nested labels", LabelExpression (ListValue []) (LabelExpression (BlobValue []) literal) )
           ]


expectError : Result String a -> Expect.Expectation
expectError result =
    case result of
        Err message ->
            Expect.notEqual "" message

        Ok _ ->
            Expect.fail "Expected an error, but decoding or evaluation succeeded."


expectErrorContaining : String -> Result String a -> Expect.Expectation
expectErrorContaining context result =
    case result of
        Err message ->
            Expect.equal True (String.contains context message)
                |> Expect.onFail ("Error should contain " ++ context ++ ": " ++ message)

        Ok _ ->
            Expect.fail ("Expected an error containing " ++ context)
