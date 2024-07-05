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
            { expression = Pine.LiteralExpression (Pine.ListValue [])
            , environment = Pine.LiteralExpression (Pine.BlobValue [ 1, 3, 4 ])
            }
      )
    , ( "kernel application one arg"
      , Pine.KernelApplicationExpression
            (Pine.LiteralExpression (Pine.BlobValue [ 6, 7, 8 ]))
            "equal"
      )
    , ( "if block"
      , Pine.ConditionalExpression
            { condition = Pine.LiteralExpression (Pine.ListValue [])
            , ifTrue = Pine.LiteralExpression (Pine.ListValue [])
            , ifFalse = Pine.LiteralExpression (Pine.ListValue [])
            }
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
