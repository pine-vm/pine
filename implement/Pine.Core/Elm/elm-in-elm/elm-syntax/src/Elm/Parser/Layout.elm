module Elm.Parser.Layout exposing
    ( layoutStrict
    , layoutStrictFollowedBy
    , layoutStrictFollowedByComments
    , layoutStrictFollowedByWithComments
    , maybeAroundBothSides
    , maybeLayout
    , moduleLevelIndentationFollowedBy
    , onTopIndentationFollowedBy
    , optimisticLayout
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlusFollowedBy
    )

import Elm.Parser.Comments as Comments
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments(..))
import Rope


whitespaceAndCommentsOrEmpty : Parser Comments
whitespaceAndCommentsOrEmpty =
    ParserFast.skipWhileWhitespaceFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.Parser
            (\state ->
                let
                    (ParserFast.PState sourceBytes offsetBytes _ _ _) =
                        state

                    nextTwoChars =
                        Pine_kernel.take
                            [ 8
                            , Pine_kernel.skip [ offsetBytes, sourceBytes ]
                            ]
                in
                if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '-', '-' ] ] then
                    -- this will always succeed from here, so no need to fall back to Rope.empty
                    let
                        (ParserFast.Parser parse) =
                            fromSingleLineCommentNode
                    in
                    parse state

                else if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '{', '-' ] ] then
                    let
                        (ParserFast.Parser parse) =
                            fromMultilineCommentNodeOrEmptyOnProblem
                    in
                    parse state

                else
                    ParserFast.Good Rope.empty state
            )
        )


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        (Comments.multilineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        Rope.empty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            Rope.one content |> Rope.filledPrependTo commentsAfter
        )
        (Comments.singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : Parser Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            Comments.singleLineComment
            Comments.multilineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        Rope.empty
        (\right soFar -> soFar |> Rope.prependToFilled (Rope.one right))
        identity


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty |> endsPositivelyIndented


endsPositivelyIndented : Parser a -> Parser a
endsPositivelyIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent ->
            Pine_kernel.int_is_sorted_asc [ Pine_kernel.int_add [ indent, 1 ], column ]
        )
        "must be positively indented"
        parser


{-| Check that the indentation of an already parsed token
would be valid after [`maybeLayout`](#maybeLayout)
-}
positivelyIndentedPlusFollowedBy : Int -> Parser a -> Parser a
positivelyIndentedPlusFollowedBy extraIndent nextParser =
    ParserFast.Parser
        (\state ->
            let
                (ParserFast.PState _ _ indent row column) =
                    state
            in
            if Pine_kernel.int_is_sorted_asc [ Pine_kernel.int_add [ indent, extraIndent, 1 ], column ] then
                let
                    (ParserFast.Parser nextParser_unwrapped) =
                        nextParser
                in
                nextParser_unwrapped state

            else
                ParserFast.Bad False (ParserFast.ExpectingCustom row column "must be positively indented")
        )


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.Parser
        (\state ->
            let
                (ParserFast.PState _ _ indent row column) =
                    state
            in
            if Pine_kernel.int_is_sorted_asc [ Pine_kernel.int_add [ indent, 1 ], column ] then
                let
                    (ParserFast.Parser nextParser_unwrapped) =
                        nextParser
                in
                nextParser_unwrapped state

            else
                ParserFast.Bad False (ParserFast.ExpectingCustom row column "must be positively indented")
        )


optimisticLayout : Parser Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrictFollowedByComments : Parser Comments -> Parser Comments
layoutStrictFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> Rope.prependTo afterComments
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedByWithComments : Parser (WithComments syntax) -> Parser (WithComments syntax)
layoutStrictFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore (WithComments comments syntax) ->
            WithComments
                (commentsBefore |> Rope.prependTo comments)
                syntax
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedBy : Parser syntax -> Parser (WithComments syntax)
layoutStrictFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            WithComments
                commentsBefore
                after
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout |> endsTopIndented


moduleLevelIndentationFollowedBy : Parser a -> Parser a
moduleLevelIndentationFollowedBy nextParser =
    ParserFast.Parser
        (\state ->
            let
                (ParserFast.PState _ _ _ row column) =
                    state
            in
            if Pine_kernel.equal [ column, 1 ] then
                let
                    (ParserFast.Parser nextParser_unwrapped) =
                        nextParser
                in
                nextParser_unwrapped state

            else
                ParserFast.Bad False (ParserFast.ExpectingCustom row column "must be on module-level indentation")
        )


endsTopIndented : Parser a -> Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> Pine_kernel.equal [ column, indent ])
        "must be on top indentation"
        parser


onTopIndentationFollowedBy : Parser a -> Parser a
onTopIndentationFollowedBy nextParser =
    ParserFast.Parser
        (\state ->
            let
                (ParserFast.PState _ _ indent row column) =
                    state
            in
            if Pine_kernel.equal [ column, indent ] then
                let
                    (ParserFast.Parser nextParser_unwrapped) =
                        nextParser
                in
                nextParser_unwrapped state

            else
                ParserFast.Bad False (ParserFast.ExpectingCustom row column "must be on top indentation")
        )


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    ParserFast.map3
        (\before (WithComments comments syntax) after ->
            WithComments
                (before
                    |> Rope.prependTo comments
                    |> Rope.prependTo after
                )
                syntax
        )
        maybeLayout
        x
        maybeLayout
