# Explore Early Instantiation Stage

We want to explore expanding the compilation of Elm programs to include an additional early stage that instantiates (partially) applied functions to improve runtime performance.
Currently, the Elm compiler does only very limited inlining. The current module-wise compilation structure makes inlining more challenging when the application occurs in another module than the function definition.
The idea explored here is to offer a quick workaround for challenges in implementing inlining by adding it as an early/first stage of compilation.

The idea is to make that new stage output Elm syntax using the same syntax model we already use for parsing. This output model should have the following advantages:

+ Testing this stage becomes very simple, as snapshot tests are easy to read, and we can reuse existing tools to write expectations.
+ We can insert that stage before the already existing compiler implementation without having to adapt the following stage.

## Scope

While the substitution of parameter references by arguments enables subsequent reduction by compile-time evaluation, that is not in scope here, as the existing implementation already performs such transformations.

The most important goal is to replace uses of higher-order functions with first-order functions by substituting the arguments given by the application.

Following is an example of a frequently used function in an app, where we want to improve the runtime efficiency:

```Elm
module Elm.Parser.Tokens exposing (..)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.isUpper
        isAlphaNumOrUnderscore

```

```Elm
module ParserFast exposing (..)


ifFollowedByWhileWithoutLinebreak :
    (Char -> Bool)
    -> (Char -> Bool)
    -> Parser String
ifFollowedByWhileWithoutLinebreak firstIsOkay afterFirstIsOkay =
    Parser
        (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
            let
                sColInt : Int
                sColInt =
                    sCol

                nextCharBytes : Int
                nextCharBytes =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ sOffset, sSrcBytes ]
                        ]
            in
            if firstIsOkay nextCharBytes then
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            (Pine_kernel.int_add [ sOffset, 4 ])
                            sRow
                            (sColInt + 1)
                            sSrcBytes
                            sIndent

                    (PState _ s1Offset _ _ _) =
                        s1

                    nameSliceBytesLength : Int
                    nameSliceBytesLength =
                        Pine_kernel.int_add
                            [ s1Offset
                            , Pine_kernel.int_mul [ -1, sOffset ]
                            ]

                    nameSliceBytes : Int
                    nameSliceBytes =
                        Pine_kernel.take
                            [ nameSliceBytesLength
                            , Pine_kernel.skip [ sOffset, sSrcBytes ]
                            ]
                in
                Good (String nameSliceBytes) s1

            else
                Bad False (ExpectingCharSatisfyingPredicate sRow sCol)
        )

```

In this example, the new stage is to replace `typeName` as follows:

```Elm
module Elm.Parser.Tokens exposing (..)


typeName : ParserFast.Parser String
typeName =
    ParserFast.Parser
        (\(ParserFast.PState sSrcBytes sOffset sIndent sRow sCol) ->
            let
                sColInt : Int
                sColInt =
                    sCol

                nextCharBytes : Int
                nextCharBytes =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip
                            [ sOffset
                            , sSrcBytes
                            ]
                        ]
            in
            if Char.isUpper nextCharBytes then
                let
                    s1 : ParserFast.State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            isAlphaNumOrUnderscore
                            (Pine_kernel.int_add
                                [ sOffset
                                , 4
                                ]
                            )
                            sRow
                            (sColInt + 1)
                            sSrcBytes
                            sIndent

                    (ParserFast.PState _ s1Offset _ _ _) =
                        s1

                    nameSliceBytesLength : Int
                    nameSliceBytesLength =
                        Pine_kernel.int_add
                            [ s1Offset
                            , Pine_kernel.int_mul
                                [ -1
                                , sOffset
                                ]
                            ]

                    nameSliceBytes : Int
                    nameSliceBytes =
                        Pine_kernel.take
                            [ nameSliceBytesLength
                            , Pine_kernel.skip
                                [ sOffset
                                , sSrcBytes
                                ]
                            ]
                in
                ParserFast.Good
                    (String nameSliceBytes)
                    s1

            else
                ParserFast.Bad
                    False
                    (ParserFast.ExpectingCharSatisfyingPredicate sRow sCol)
        )

```

## Design Details

By inserting the new stage after canonicalization, we can create references using the canonical module names, without considering import statements at all.

### Placement of Added Declarations

In cases that require additional declarations, they are added in the calling module/function.

### Dependencies of the Inlined Function

The inlined function may reference other functions from its original module. In these cases, the references in the calling module use the canonical names of function and type declarations.
Since the canonicalization stage happens before, we don't care if the declaring module initially exposed that declaration.

### Recursive Functions

In the first implementation, we do not inline recursive function calls. If the applied function depends on itself either directly or indirectly, we skip inlining of that application.

### Locations And Ranges For Generated Syntax Nodes

For the first implementation, we do not attempt to produce valid locations in the generated syntax nodes; instead, we use the value 0 for all locations (row, column) and ranges for newly created syntax nodes.

