module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)


singleLineComment : ParserFast.Parser (Node String)
singleLineComment =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.whileMapWithRange
            (\c -> c /= '\u{000D}' && c /= '\n')
            (\range content ->
                Node
                    { start = { row = range.start.row, column = range.start.column - 2 }
                    , end =
                        { row = range.start.row
                        , column = range.end.column
                        }
                    }
                    ("--" ++ content)
            )
        )


multilineComment : ParserFast.Parser (Node String)
multilineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case List.take 1 (List.drop (offset + 2) source) of
                [ '|' ] ->
                    problemUnexpectedDocumentation

                _ ->
                    multiLineCommentNoCheck
        )


problemUnexpectedDocumentation : Parser a
problemUnexpectedDocumentation =
    ParserFast.problem "unexpected documentation comment"


multiLineCommentNoCheck : Parser (Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange Node
        ( '{', "-" )
        ( '-', "}" )


moduleDocumentation : Parser (Node String)
moduleDocumentation =
    declarationDocumentation


declarationDocumentation : ParserFast.Parser (Node Documentation)
declarationDocumentation =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    multiLineCommentNoCheck
