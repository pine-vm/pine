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
