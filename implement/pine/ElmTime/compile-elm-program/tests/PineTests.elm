module PineTests exposing (..)

import Expect
import Pine
import Test


pineExpressionEncodeDecodeRoundtrip : Test.Test
pineExpressionEncodeDecodeRoundtrip =
    [ ( "literal empty list", Pine.LiteralExpression (Pine.ListValue []) )
    , ( "literal blob", Pine.LiteralExpression (Pine.BlobValue [ 1, 3, 4 ]) )
    , ( "list empty", Pine.ListExpression [] )
    , ( "application one arg"
      , Pine.ParseAndEvalExpression
            (Pine.LiteralExpression (Pine.BlobValue [ 1, 3, 4 ]))
            (Pine.LiteralExpression (Pine.ListValue []))
      )
    , ( "kernel application one arg"
      , Pine.KernelApplicationExpression
            "equal"
            (Pine.LiteralExpression (Pine.BlobValue [ 6, 7, 8 ]))
      )
    , ( "if block"
      , Pine.ConditionalExpression
            (Pine.LiteralExpression (Pine.ListValue []))
            (Pine.LiteralExpression (Pine.ListValue []))
            (Pine.LiteralExpression (Pine.ListValue []))
      )
    , ( "string tag"
      , Pine.StringTagExpression "the tag string" (Pine.ListExpression [])
      )
    ]
        |> List.map
            (\( testName, expression ) ->
                Test.test testName <|
                    \_ ->
                        let
                            encoded =
                                Pine.encodeExpressionAsValue expression
                        in
                        Expect.equal (Ok expression) (Pine.parseExpressionFromValue encoded)
            )
        |> Test.describe "Pine expression encode decode roundtrip"


countValueContentTests : Test.Test
countValueContentTests =
    [ ( "empty blob"
      , ( Pine.BlobValue []
        , ( 0, 0 )
        )
      )
    , ( "blob with one byte"
      , ( Pine.BlobValue [ 4 ]
        , ( 0, 1 )
        )
      )
    , ( "empty list"
      , ( Pine.ListValue []
        , ( 0, 0 )
        )
      )
    , ( "list with one empty list"
      , ( Pine.ListValue [ Pine.ListValue [] ]
        , ( 1, 0 )
        )
      )
    , ( "list with one list containing empty list"
      , ( Pine.ListValue [ Pine.ListValue [ Pine.ListValue [] ] ]
        , ( 2, 0 )
        )
      )
    , ( "list [ blob(1) ]"
      , ( Pine.ListValue [ Pine.BlobValue [ 123 ] ]
        , ( 1, 1 )
        )
      )
    , ( "list [ blob(1), blob(3) ]"
      , ( Pine.ListValue [ Pine.BlobValue [ 123 ], Pine.BlobValue [ 1, 2, 3 ] ]
        , ( 2, 4 )
        )
      )
    , ( "list [list [ blob(1) ], blob(3) ]"
      , ( Pine.ListValue [ Pine.ListValue [ Pine.BlobValue [ 123 ] ], Pine.BlobValue [ 1, 2, 3 ] ]
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
                                Pine.countValueContent pineValue
                        in
                        Expect.equal expectedCounts counts
            )
        |> Test.describe "Count Pine value contents"
