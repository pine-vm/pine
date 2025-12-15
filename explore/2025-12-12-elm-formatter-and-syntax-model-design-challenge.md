# 2025-12-12 Elm Formatter And Syntax Model Design Challenge

The formatter for Elm module files aims to emulate the style of avh4/elm-format.

Currently, the formatter reuses the [syntax model found in the stil4m/elm-syntax project](https://github.com/stil4m/elm-syntax/blob/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax/Expression.elm#L113-L137). The formatting process first parses into said syntax model, then applies the formatting function with the same input and output types, and then renders those syntax nodes back to a string.

One challenge emerging from these choices is the preservation of tokens. For some syntax tokens, like “if”, “then”, “else”, and comma separators in nodes like lists and tuples, there is no location in that syntax model.

For example, in a list expression, the possible locations to render a comma between items are constrained by the end of the last item's range and the start of the following item's range. Similarly, for the “then” token, the range of possible placements is constrained by the end of the condition expression node and the start of the “then” expression.

The renderer then has to choose one out of this set of legal placements. (Because of constraints on indentation, the set of legal placements is not a contiguous range)

The point is, if we want to emulate a certain style of formatting, the logic for placing syntax elements cannot reside entirely in a formatter that uses only that syntax model as input and return type.

For now, we went with the approach of having the render function place these locationless tokens in the style of avh4/elm-format, since we currently do not have an application for other renderings.

## Interaction With Comments

The challenges of making the renderer in part responsible for these placements increase with the introduction of comments.

### Example: Comments After `then` Keyword

**Input (already correctly formatted):**

```elm
decl a b =
    if
        -- Comment before condition
        a < b
    then
        -- Comment after then
        [ 13 ]

    else
        [ 17 ]
```

For the roundtrip via parsing as outlined above, one of the versions of the render observed in testing produced this incorrect version:

```elm
decl a b =
    if
        -- Comment before condition
        a < b

        -- Comment after then
    then[ 13 ]

    else
        [ 17 ]
```

To work around the problem of figuring out placement between these location-less tokens on one side and comments on the other, we used an approach that places the tokens where the gaps between comments are.

The following is an example from our test suite that we succesfully round-trip using the original stil4m/elm-syntax version 7 model:

```Elm
module Test exposing (..)


decl a b =
    let
        alfa =
            42
    in
    if a == 71 then
        [ 13 ]

    else if
        -- A comment before condition
        (a == 73)
            || (a == 77)
        -- Comment between conditions
    then
        [ 17 ]

    else if
        a == 79
        -- Another simple comment
    then
        -- And another simple comment
        [ 21 ]

    else
        -- Yet another simple comment
        [ 23 ]
```

While this might be sufficient to achieve proper round-tripping aligned with avh4/elm-format, the implementation is somewhat hacky here: Since our architecture has a formatter separate from the renderer, the renderer has duplicate implementations of this placement logic.


tags:elm,parsing,formatting