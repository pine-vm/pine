module DemoSyntax exposing (..)

import Syntax


type alias NodeContent =
    { alfa : Int
    , beta : String
    }


demoRange : Syntax.Range
demoRange =
    { start = { row = 11, column = 13 }
    , end = { row = 17, column = 19 }
    }


sumNodeContentAlfa : List (Syntax.Node NodeContent) -> Int
sumNodeContentAlfa nodes =
    case nodes of
        [] ->
            0

        node :: rest ->
            let
                (Syntax.Node _ content) =
                    node
            in
            content.alfa + sumNodeContentAlfa rest


concatNodeContentBeta : List (Syntax.Node NodeContent) -> String
concatNodeContentBeta nodes =
    case nodes of
        [] ->
            ""

        node :: rest ->
            let
                (Syntax.Node _ content) =
                    node
            in
            content.beta ++ "-" ++ concatNodeContentBeta rest


nodeListRangeComponents : Syntax.Node a -> List Int
nodeListRangeComponents (Syntax.Node { start, end } _) =
    [ start.row
    , start.column
    , end.row
    , end.column
    ]
