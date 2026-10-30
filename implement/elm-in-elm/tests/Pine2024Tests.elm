module Pine2024Tests exposing (..)

import Expect
import Pine2024
import Test


pineExpressionEncodeDecodeRoundtrip : Test.Test
pineExpressionEncodeDecodeRoundtrip =
    [ ( "literal empty list", Pine2024.LiteralExpression (Pine2024.ListValue []) )
    , ( "literal blob", Pine2024.LiteralExpression (Pine2024.BlobValue [ 1, 3, 4 ]) )
    , ( "list empty", Pine2024.ListExpression [] )
    , ( "application one arg"
      , Pine2024.ParseAndEvalExpression
            (Pine2024.LiteralExpression (Pine2024.BlobValue [ 1, 3, 4 ]))
            (Pine2024.LiteralExpression (Pine2024.ListValue []))
      )
    , ( "kernel application one arg"
      , Pine2024.KernelApplicationExpression
            "equal"
            (Pine2024.LiteralExpression (Pine2024.BlobValue [ 6, 7, 8 ]))
      )
    , ( "if block"
      , Pine2024.ConditionalExpression
            (Pine2024.LiteralExpression (Pine2024.ListValue []))
            (Pine2024.LiteralExpression (Pine2024.ListValue []))
            (Pine2024.LiteralExpression (Pine2024.ListValue []))
      )
    , ( "string tag"
      , Pine2024.StringTagExpression "the tag string" (Pine2024.ListExpression [])
      )
    ]
        |> List.map
            (\( testName, expression ) ->
                Test.test testName <|
                    \_ ->
                        let
                            encoded =
                                Pine2024.encodeExpressionAsValue expression
                        in
                        Expect.equal (Ok expression) (Pine2024.parseExpressionFromValue encoded)
            )
        |> Test.describe "Pine expression encode decode roundtrip"


countValueContentTests : Test.Test
countValueContentTests =
    [ ( "empty blob"
      , ( Pine2024.BlobValue []
        , ( 0, 0 )
        )
      )
    , ( "blob with one byte"
      , ( Pine2024.BlobValue [ 4 ]
        , ( 0, 1 )
        )
      )
    , ( "empty list"
      , ( Pine2024.ListValue []
        , ( 0, 0 )
        )
      )
    , ( "list with one empty list"
      , ( Pine2024.ListValue [ Pine2024.ListValue [] ]
        , ( 1, 0 )
        )
      )
    , ( "list with one list containing empty list"
      , ( Pine2024.ListValue [ Pine2024.ListValue [ Pine2024.ListValue [] ] ]
        , ( 2, 0 )
        )
      )
    , ( "list [ blob(1) ]"
      , ( Pine2024.ListValue [ Pine2024.BlobValue [ 123 ] ]
        , ( 1, 1 )
        )
      )
    , ( "list [ blob(1), blob(3) ]"
      , ( Pine2024.ListValue [ Pine2024.BlobValue [ 123 ], Pine2024.BlobValue [ 1, 2, 3 ] ]
        , ( 2, 4 )
        )
      )
    , ( "list [list [ blob(1) ], blob(3) ]"
      , ( Pine2024.ListValue [ Pine2024.ListValue [ Pine2024.BlobValue [ 123 ] ], Pine2024.BlobValue [ 1, 2, 3 ] ]
        , ( 3, 4 )
        )
      )
    ]
        |> List.map
            (\( testName, ( pineValue, expectedCounts ) ) ->
                Test.test testName <|
                    \_ ->
                        let
                            counts =
                                Pine2024.countValueContent pineValue
                        in
                        Expect.equal expectedCounts counts
            )
        |> Test.describe "Count Pine value contents"
