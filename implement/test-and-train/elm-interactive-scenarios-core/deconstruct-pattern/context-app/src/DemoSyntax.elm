module DemoSyntax exposing (..)

import Syntax


type alias NodeContent =
    { alfa : Int
    , beta : String
    }


demoRange : Syntax.Range
demoRange =
    { start = { row = 1, column = 1 }
    , end = { row = 1, column = 2 }
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
