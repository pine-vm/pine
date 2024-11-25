module Elm.Parser.Expression exposing (..)

import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpression

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccess

                "[" ->
                    listOrGlslExpression

                "{" ->
                    recordExpressionFollowedByRecordAccess

                "c" ->
                    caseOrUnqualifiedReferenceExpression

                "\\" ->
                    lambdaExpression

                "l" ->
                    letOrUnqualifiedReferenceExpression

                "i" ->
                    ifOrUnqualifiedReferenceExpression

                "." ->
                    recordAccessFunctionExpression

                "-" ->
                    negationOperation

                "'" ->
                    charLiteralExpression

                _ ->
                    referenceOrNumberExpression
        )


caseOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
caseOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        caseExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


letOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
letOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        letExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


ifOrUnqualifiedReferenceExpression : Parser (WithComments (Node Expression))
ifOrUnqualifiedReferenceExpression =
    ParserFast.oneOf2
        ifBlockExpression
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess


referenceOrNumberExpression : Parser (WithComments (Node Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
        numberExpression


multiRecordAccess : ParserFast.Parser (List (Node String))
multiRecordAccess =
    ParserFast.loopWhileSucceeds
        (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
        []
        (::)
        List.reverse


multiRecordAccessMap : (List (Node String) -> res) -> ParserFast.Parser res
multiRecordAccessMap fieldsToRes =
    ParserFast.loopWhileSucceeds
        (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
        []
        (::)
        (\reversed -> fieldsToRes (List.reverse reversed))


precedence1ApR : InfixOperatorInfo
precedence1ApR =
    infixLeft 1 "|>"


precedence1ApL : InfixOperatorInfo
precedence1ApL =
    infixRight 1 "<|"


precedence2Or : InfixOperatorInfo
precedence2Or =
    infixRight 2 "||"


precedence3And : InfixOperatorInfo
precedence3And =
    infixRight 3 "&&"


precedence4Eq : InfixOperatorInfo
precedence4Eq =
    infixNonAssociative 4 "=="


precedence4Neq : InfixOperatorInfo
precedence4Neq =
    infixNonAssociative 4 "/="


precedence4Le : InfixOperatorInfo
precedence4Le =
    infixNonAssociative 4 "<="


precedence4Ge : InfixOperatorInfo
precedence4Ge =
    infixNonAssociative 4 ">="


precedence4Gt : InfixOperatorInfo
precedence4Gt =
    infixNonAssociative 4 ">"


precedence4Lt : InfixOperatorInfo
precedence4Lt =
    infixNonAssociative 4 "<"


precedence5append : InfixOperatorInfo
precedence5append =
    infixRight 5 "++"


precedence5Cons : InfixOperatorInfo
precedence5Cons =
    infixRight 5 "::"


precedence5Keep : InfixOperatorInfo
precedence5Keep =
    infixLeft 5 "|="


precedence6Add : InfixOperatorInfo
precedence6Add =
    infixLeft 6 "+"


precedence6Sub : InfixOperatorInfo
precedence6Sub =
    infixLeft 6 "-"


precedence6Ignore : InfixOperatorInfo
precedence6Ignore =
    infixLeft 6 "|."


precedence7Idiv : InfixOperatorInfo
precedence7Idiv =
    infixLeft 7 "//"


precedence7Mul : InfixOperatorInfo
precedence7Mul =
    infixLeft 7 "*"


precedence7Fdiv : InfixOperatorInfo
precedence7Fdiv =
    infixLeft 7 "/"


precedence7Slash : InfixOperatorInfo
precedence7Slash =
    infixRight 7 "</>"


precedence8QuestionMark : InfixOperatorInfo
precedence8QuestionMark =
    infixLeft 8 "<?>"


precedence8Pow : InfixOperatorInfo
precedence8Pow =
    infixRight 8 "^"


precedence9ComposeR : InfixOperatorInfo
precedence9ComposeR =
    infixRight 9 ">>"


precedence9ComposeL : InfixOperatorInfo
precedence9ComposeL =
    infixLeft 9 "<<"


expression : Parser (WithComments (Node Expression))
expression =
    extendedSubExpressionOptimisticLayout Ok .extensionRight


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = Rope.empty
                , syntax =
                    Node
                        -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                        { start = { row = range.start.row, column = range.start.column - 6 }
                        , end = { row = range.end.row, column = range.end.column + 2 }
                        }
                        (GLSLExpression s)
                }
            )
            (ParserFast.loopUntil
                (ParserFast.symbol "|]" ())
                (ParserFast.oneOf2
                    (ParserFast.symbol "|" "|")
                    (ParserFast.while (\c -> c /= '|'))
                )
                ""
                (\extension soFar ->
                    soFar ++ extension ++ ""
                )
                identity
            )
        )


listOrGlslExpression : Parser (WithComments (Node Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> Rope.prependTo elements.comments
                , syntax =
                    Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
                        }
                        elements.syntax
                }
            )
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = Rope.empty, syntax = ListExpr [] })
                (ParserFast.map3
                    (\head commentsAfterHead tail ->
                        { comments =
                            head.comments
                                |> Rope.prependTo commentsAfterHead
                                |> Rope.prependTo tail.comments
                        , syntax = ListExpr (head.syntax :: tail.syntax)
                        }
                    )
                    expression
                    Layout.maybeLayout
                    (ParserWithComments.many
                        (ParserFast.symbolFollowedBy ","
                            (Layout.maybeAroundBothSides expression)
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )



-- recordExpression


recordExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
recordExpressionFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2
            (\leftestResult recordAccesses ->
                case recordAccesses of
                    [] ->
                        leftestResult

                    _ :: _ ->
                        { comments = leftestResult.comments
                        , syntax =
                            recordAccesses
                                |> List.foldl
                                    (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                        Node { start = leftRange.start, end = fieldRange.end }
                                            (Expression.RecordAccess leftNode fieldNode)
                                    )
                                    leftestResult.syntax
                        }
            )
            (ParserFast.map2WithRange
                (\range commentsBefore afterCurly ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo afterCurly.comments
                    , syntax = Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                    }
                )
                Layout.maybeLayout
                recordContentsCurlyEnd
            )
            multiRecordAccess
        )


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf2
        (ParserFast.map5
            (\nameNode commentsAfterFunctionName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                { comments =
                    commentsAfterFunctionName
                        |> Rope.prependTo afterNameBeforeFields.comments
                        |> Rope.prependTo tailFields.comments
                        |> Rope.prependTo commentsBeforeClosingCurly
                , syntax =
                    case afterNameBeforeFields.syntax of
                        RecordUpdateFirstSetter firstField ->
                            RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                        FieldsFirstValue firstFieldValue ->
                            RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                }
            )
            Tokens.functionNameNode
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> Rope.prependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        Layout.maybeLayout
                        recordSetterNodeWithLayout
                    )
                )
                (ParserFast.symbolFollowedBy "="
                    (ParserFast.map3
                        (\commentsBefore expressionResult commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo expressionResult.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax = FieldsFirstValue expressionResult.syntax
                            }
                        )
                        Layout.maybeLayout
                        expression
                        Layout.maybeLayout
                    )
                )
            )
            recordFields
            (Layout.maybeLayout |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" { comments = Rope.empty, syntax = RecordExpr [] })


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map2
                (\commentsBefore setterResult ->
                    { comments = commentsBefore |> Rope.prependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                Layout.maybeLayout
                recordSetterNodeWithLayout
            )
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map5WithRange
        (\range name commentsAfterFunctionName commentsAfterEquals expressionResult commentsAfterExpression ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
                    |> Rope.prependTo commentsAfterExpression
            , syntax = Node range ( name, expressionResult.syntax )
            }
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expression
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        Layout.maybeLayout


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Node range (Literal string)
            }
        )


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty
            , syntax = Node range (CharLiteral char)
            }
        )



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    ParserFast.symbolFollowedBy "\\"
        (ParserFast.map6WithStartLocation
            (\start commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs commentsAfterArrow expressionResult ->
                let
                    (Node expressionRange _) =
                        expressionResult.syntax
                in
                { comments =
                    commentsAfterBackslash
                        |> Rope.prependTo firstArg.comments
                        |> Rope.prependTo commentsAfterFirstArg
                        |> Rope.prependTo secondUpArgs.comments
                        |> Rope.prependTo commentsAfterArrow
                        |> Rope.prependTo expressionResult.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 1 }
                        , end = expressionRange.end
                        }
                        (LambdaExpression
                            { args = firstArg.syntax :: secondUpArgs.syntax
                            , expression = expressionResult.syntax
                            }
                        )
                }
            )
            Layout.maybeLayout
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
            (ParserWithComments.until
                (ParserFast.symbol "->" ())
                (ParserFast.map2
                    (\patternResult commentsAfter ->
                        { comments =
                            patternResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = patternResult.syntax
                        }
                    )
                    Patterns.patternNotDirectlyComposing
                    Layout.maybeLayout
                )
            )
            Layout.maybeLayout
            expression
        )



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    ParserFast.keywordFollowedBy "case"
        (ParserFast.map5WithStartLocation
            (\start commentsAfterCase casedExpressionResult commentsBeforeOf commentsAfterOf casesResult ->
                let
                    ( firstCase, lastToSecondCase ) =
                        casesResult.syntax
                in
                { comments =
                    commentsAfterCase
                        |> Rope.prependTo casedExpressionResult.comments
                        |> Rope.prependTo commentsBeforeOf
                        |> Rope.prependTo commentsAfterOf
                        |> Rope.prependTo casesResult.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 4 }
                        , end =
                            case lastToSecondCase of
                                ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                    lastCaseExpressionRange.end

                                [] ->
                                    let
                                        ( _, Node firstCaseExpressionRange _ ) =
                                            firstCase
                                    in
                                    firstCaseExpressionRange.end
                        }
                        (CaseExpression
                            { expression = casedExpressionResult.syntax
                            , cases = firstCase :: List.reverse lastToSecondCase
                            }
                        )
                }
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "of" Layout.maybeLayout)
            (ParserFast.withIndentSetToColumn caseStatements)
        )


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> Rope.prependTo commentsAfterFirstCasePattern
                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                    |> Rope.prependTo firstCaseExpressionResult.comments
                    |> Rope.prependTo lastToSecondCase.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        Patterns.pattern
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
        expression
        (ParserWithComments.manyWithoutReverse caseStatement)


caseStatement : Parser (WithComments Case)
caseStatement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map4
            (\pattern commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    pattern.comments
                        |> Rope.prependTo commentsBeforeArrowRight
                        |> Rope.prependTo commentsAfterArrowRight
                        |> Rope.prependTo expr.comments
                , syntax = ( pattern.syntax, expr.syntax )
                }
            )
            Patterns.pattern
            Layout.maybeLayout
            (ParserFast.symbolFollowedBy "->" Layout.maybeLayout)
            expression
        )



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    ParserFast.keywordFollowedBy "let"
        (ParserFast.map3WithStartLocation
            (\start declarations commentsAfterIn expressionResult ->
                let
                    (Node expressionRange _) =
                        expressionResult.syntax
                in
                { comments =
                    declarations.comments
                        |> Rope.prependTo commentsAfterIn
                        |> Rope.prependTo expressionResult.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 3 }
                        , end = expressionRange.end
                        }
                        (LetExpression
                            { declarations = declarations.declarations
                            , expression = expressionResult.syntax
                            }
                        )
                }
            )
            (ParserFast.withIndentSetToColumnMinus 3
                (ParserFast.map2
                    (\commentsAfterLet declarations ->
                        { comments =
                            commentsAfterLet
                                |> Rope.prependTo declarations.comments
                        , declarations = declarations.syntax
                        }
                    )
                    Layout.maybeLayout
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
            -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
            (Layout.positivelyIndentedPlusFollowedBy 2
                Layout.maybeLayout
            )
            expression
        )


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> Rope.prependTo commentsAfter
                        |> Rope.prependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
            (ParserWithComments.until Tokens.inToken blockElement)
        )


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map2
            (\letDeclarationResult commentsAfter ->
                { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
                , syntax = letDeclarationResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
        )


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    ParserFast.map4
        (\pattern commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (Node { start } _) =
                    pattern.syntax

                (Node { end } _) =
                    expressionResult.syntax
            in
            { comments =
                pattern.comments
                    |> Rope.prependTo commentsAfterPattern
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node { start = start, end = end }
                    (LetDestructuring pattern.syntax expressionResult.syntax)
            }
        )
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
            let
                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
                    )
                        |> Rope.prependTo arguments.comments
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo expressionResult.comments
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (LetFunction
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = arguments.syntax
                                        , expression = expressionResult.syntax
                                        }
                                }
                            )
                    }

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (LetFunction
                                { documentation = Nothing
                                , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                , declaration =
                                    Node { start = implementationNameRange.start, end = expressionRange.end }
                                        { name = signature.implementationName
                                        , arguments = arguments.syntax
                                        , expression = expressionResult.syntax
                                        }
                                }
                            )
                    }
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ letDeclaration) =
                        result.syntax
                in
                case letDeclaration of
                    LetDestructuring _ _ ->
                        True

                    LetFunction letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""
            )
            "Expected to find the same name for declaration and signature"


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


numberExpression : Parser (WithComments (Node Expression))
numberExpression =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Floatable n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Integer n)
            }
        )
        (\range n ->
            { comments = Rope.empty
            , syntax = Node range (Hex n)
            }
        )


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    ParserFast.keywordFollowedBy "if"
        (ParserFast.map8WithStartLocation
            (\start commentsAfterIf condition commentsBeforeThen commentsAfterThen ifTrue commentsBeforeElse commentsAfterElse ifFalse ->
                let
                    (Node ifFalseRange _) =
                        ifFalse.syntax
                in
                { comments =
                    commentsAfterIf
                        |> Rope.prependTo condition.comments
                        |> Rope.prependTo commentsBeforeThen
                        |> Rope.prependTo commentsAfterThen
                        |> Rope.prependTo ifTrue.comments
                        |> Rope.prependTo commentsBeforeElse
                        |> Rope.prependTo commentsAfterElse
                        |> Rope.prependTo ifFalse.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 2 }
                        , end = ifFalseRange.end
                        }
                        (IfBlock
                            condition.syntax
                            ifTrue.syntax
                            ifFalse.syntax
                        )
                }
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "then" Layout.maybeLayout)
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "else" Layout.maybeLayout)
            expression
        )


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- TODO only for tests
                    "" ->
                        negationAfterMinus

                    _ ->
                        negationWhitespaceProblem
            )
        )


negationWhitespaceProblem : Parser a
negationWhitespaceProblem =
    ParserFast.problem "if a negation sign is not preceded by whitespace, it's considered subtraction"


negationAfterMinus : Parser (WithComments (Node Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            let
                (Node subExpressionRange _) =
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                Node
                    { start =
                        { row = subExpressionRange.start.row
                        , column = subExpressionRange.start.column - 1
                        }
                    , end = subExpressionRange.end
                    }
                    (Negation subExpressionResult.syntax)
            }
        )
        (ParserFast.lazy (\() -> subExpression))


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
            , syntax =
                case after of
                    Nothing ->
                        Node range (FunctionOrValue [] firstName)

                    Just ( qualificationAfter, unqualified, recordAccesses ) ->
                        case recordAccesses of
                            [] ->
                                Node range (FunctionOrValue (firstName :: qualificationAfter) unqualified)

                            (Node firstRecordAccessRange _) :: _ ->
                                let
                                    referenceNode : Node Expression
                                    referenceNode =
                                        Node
                                            { start = range.start
                                            , end =
                                                { row = firstRecordAccessRange.start.row
                                                , column = firstRecordAccessRange.start.column - 1
                                                }
                                            }
                                            (FunctionOrValue (firstName :: qualificationAfter) unqualified)
                                in
                                recordAccesses
                                    |> List.foldl
                                        (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                            Node { start = leftRange.start, end = fieldRange.end }
                                                (Expression.RecordAccess leftNode fieldNode)
                                        )
                                        referenceNode
            }
        )
        Tokens.typeName
        maybeDotReferenceExpressionTuple


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String, List (Node String) ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName, [] )

                            Just ( qualificationAfter, unqualified, recordAccess ) ->
                                ( firstName :: qualificationAfter, unqualified, recordAccess )
                    )
                    Tokens.typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                Basics.identity
                (ParserFast.map2
                    (\name recordAccesses ->
                        Just ( [], name, recordAccesses )
                    )
                    Tokens.functionName
                    multiRecordAccess
                )
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2
        (\leftestResult recordAccesses ->
            case recordAccesses of
                [] ->
                    leftestResult

                _ :: _ ->
                    { comments = leftestResult.comments
                    , syntax =
                        recordAccesses
                            |> List.foldl
                                (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                    Node { start = leftRange.start, end = fieldRange.end }
                                        (Expression.RecordAccess leftNode fieldNode)
                                )
                                leftestResult.syntax
                    }
        )
        (Tokens.functionNameMapWithRange
            (\range unqualified ->
                { comments = Rope.empty
                , syntax =
                    Node range (FunctionOrValue [] unqualified)
                }
            )
        )
        multiRecordAccess


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (Tokens.functionNameMapWithRange
            (\range field ->
                { comments = Rope.empty
                , syntax =
                    Node (range |> rangeMoveStartLeftByOneColumn)
                        (RecordAccessFunction ("." ++ field))
                }
            )
        )


rangeMoveStartLeftByOneColumn : Range -> Range
rangeMoveStartLeftByOneColumn range =
    { start = { row = range.start.row, column = range.start.column - 1 }
    , end = range.end
    }


tupledExpressionIfNecessaryFollowedByRecordAccess : Parser (WithComments (Node Expression))
tupledExpressionIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            UnitExpr
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            tupledExpressionInnerAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (Node Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = Rope.empty
            , syntax =
                Node
                    { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                    , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                    }
                    (PrefixOperator operator)
            }
        )
        Tokens.isOperatorSymbolChar
        Tokens.isAllowedOperatorToken
        ")"


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map4WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> Rope.prependTo firstPart.comments
                    |> Rope.prependTo commentsAfterFirstPart
                    |> Rope.prependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesizedFollowedByRecordAccesses recordAccesses ->
                        case recordAccesses of
                            [] ->
                                Node
                                    { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                                    , end = rangeAfterOpeningParens.end
                                    }
                                    (ParenthesizedExpression firstPart.syntax)

                            (Node firstRecordAccessRange _) :: _ ->
                                let
                                    range : Range
                                    range =
                                        { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                                        , end =
                                            { row = firstRecordAccessRange.start.row
                                            , column = firstRecordAccessRange.start.column - 1
                                            }
                                        }

                                    parenthesizedNode : Node Expression
                                    parenthesizedNode =
                                        Node range (ParenthesizedExpression firstPart.syntax)
                                in
                                recordAccesses
                                    |> List.foldl
                                        (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                            Node { start = leftRange.start, end = fieldRange.end }
                                                (Expression.RecordAccess leftNode fieldNode)
                                        )
                                        parenthesizedNode

                    TupledTwoOrThree ( secondPart, maybeThirdPart ) ->
                        Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case maybeThirdPart of
                                Nothing ->
                                    TupledExpression [ firstPart.syntax, secondPart ]

                                Just thirdPart ->
                                    TupledExpression [ firstPart.syntax, secondPart, thirdPart ]
                            )
            }
        )
        Layout.maybeLayout
        expression
        Layout.maybeLayout
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy ")"
                (multiRecordAccessMap
                    (\recordAccesses -> { comments = Rope.empty, syntax = TupledParenthesizedFollowedByRecordAccesses recordAccesses })
                )
            )
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore partResult commentsAfter maybeThirdPart ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo partResult.comments
                                |> Rope.prependTo commentsAfter
                                |> Rope.prependTo maybeThirdPart.comments
                        , syntax = TupledTwoOrThree ( partResult.syntax, maybeThirdPart.syntax )
                        }
                    )
                    Layout.maybeLayout
                    expression
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map3
                                (\commentsBefore partResult commentsAfter ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo partResult.comments
                                            |> Rope.prependTo commentsAfter
                                    , syntax = Just partResult.syntax
                                    }
                                )
                                Layout.maybeLayout
                                expression
                                Layout.maybeLayout
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
        )


type Tupled
    = TupledParenthesizedFollowedByRecordAccesses (List (Node String))
    | TupledTwoOrThree ( Node Expression, Maybe (Node Expression) )



---


extendedSubExpressionOptimisticLayout :
    (InfixOperatorInfo -> Result String intermediate)
    -> (intermediate -> Parser (WithComments ExtensionRight))
    -> Parser (WithComments (Node Expression))
extendedSubExpressionOptimisticLayout toResult afterCommitting =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (Layout.positivelyIndentedFollowedBy
            (infixOperatorAndThen toResult afterCommitting)
        )
        subExpressionMaybeAppliedOptimisticLayout
        (\extensionRightResult leftNodeWithComments ->
            { comments =
                leftNodeWithComments.comments
                    |> Rope.prependTo extensionRightResult.comments
            , syntax =
                leftNodeWithComments.syntax
                    |> applyExtensionRight extensionRightResult.syntax
            }
        )
        Basics.identity


infixOperatorAndThen : (InfixOperatorInfo -> Result String intermediate) -> (intermediate -> Parser res) -> Parser res
infixOperatorAndThen toResult afterCommitting =
    ParserFast.whileWithoutLinebreakAnd2PartUtf16ToResultAndThen
        Tokens.isOperatorSymbolChar
        (\operator ->
            case operator of
                "|>" ->
                    toResult precedence1ApR

                "++" ->
                    toResult precedence5append

                "<|" ->
                    toResult precedence1ApL

                ">>" ->
                    toResult precedence9ComposeR

                "==" ->
                    toResult precedence4Eq

                "*" ->
                    toResult precedence7Mul

                "::" ->
                    toResult precedence5Cons

                "+" ->
                    toResult precedence6Add

                "-" ->
                    toResult precedence6Sub

                "|." ->
                    toResult precedence6Ignore

                "&&" ->
                    toResult precedence3And

                "|=" ->
                    toResult precedence5Keep

                "<<" ->
                    toResult precedence9ComposeL

                "/=" ->
                    toResult precedence4Neq

                "//" ->
                    toResult precedence7Idiv

                "/" ->
                    toResult precedence7Fdiv

                "</>" ->
                    toResult precedence7Slash

                "||" ->
                    toResult precedence2Or

                "<=" ->
                    toResult precedence4Le

                ">=" ->
                    toResult precedence4Ge

                ">" ->
                    toResult precedence4Gt

                "<?>" ->
                    toResult precedence8QuestionMark

                "<" ->
                    toResult precedence4Lt

                "^" ->
                    toResult precedence8Pow

                _ ->
                    errUnknownInfixOperator
        )
        afterCommitting


subExpressionMaybeAppliedOptimisticLayout : Parser (WithComments (Node Expression))
subExpressionMaybeAppliedOptimisticLayout =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> Rope.prependTo commentsBeforeExtension
                    |> Rope.prependTo maybeArgsReverse.comments
            , syntax =
                case maybeArgsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    ((Node lastArgRange _) :: _) as argsReverse ->
                        let
                            ((Node leftRange _) as leftNode) =
                                leftExpressionResult.syntax
                        in
                        Node { start = leftRange.start, end = lastArgRange.end }
                            (Expression.Application
                                (leftNode :: List.reverse argsReverse)
                            )
            }
        )
        (ParserFast.lazy (\() -> subExpression))
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.map2
                (\arg commentsAfter ->
                    { comments = arg.comments |> Rope.prependTo commentsAfter
                    , syntax = arg.syntax
                    }
                )
                (Layout.positivelyIndentedFollowedBy
                    (ParserFast.lazy (\() -> subExpression))
                )
                Layout.optimisticLayout
            )
        )


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight (ExtendRightByOperation operation) ((Node leftRange _) as leftNode) =
    let
        ((Node rightExpressionRange _) as rightExpressionNode) =
            operation.expression
    in
    Node { start = leftRange.start, end = rightExpressionRange.end }
        (OperatorApplication operation.symbol
            operation.direction
            leftNode
            rightExpressionNode
        )


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRight : Parser (WithComments ExtensionRight)
    }


errUnknownInfixOperator : Result String a
errUnknownInfixOperator =
    Err "unknown infix operator"


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBeforeFirst first ->
                { comments =
                    commentsBeforeFirst
                        |> Rope.prependTo first.comments
                , syntax =
                    ExtendRightByOperation
                        { symbol = symbol
                        , direction = Infix.Left
                        , expression = first.syntax
                        }
                }
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence > leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                .extensionRight
            )
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBefore right ->
                { comments = commentsBefore |> Rope.prependTo right.comments
                , syntax =
                    ExtendRightByOperation
                        { symbol = symbol
                        , direction = Infix.Non
                        , expression = right.syntax
                        }
                }
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence >= leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                (\info ->
                    if info.leftPrecedence == leftPrecedence then
                        problemCannotMixNonAssociativeInfixOperators

                    else
                        -- info.leftPrecedence > leftPrecedence
                        info.extensionRight
                )
            )
    }


problemCannotMixNonAssociativeInfixOperators : Parser a
problemCannotMixNonAssociativeInfixOperators =
    ParserFast.problem "cannot mix non-associative infix operators without parenthesis"


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRight =
        ParserFast.map2
            (\commentsBeforeFirst first ->
                { comments =
                    commentsBeforeFirst
                        |> Rope.prependTo first.comments
                , syntax =
                    ExtendRightByOperation
                        { symbol = symbol
                        , direction = Infix.Right
                        , expression = first.syntax
                        }
                }
            )
            Layout.maybeLayout
            (extendedSubExpressionOptimisticLayout
                (\info ->
                    if info.leftPrecedence >= leftPrecedence then
                        Ok info

                    else
                        temporaryErrPrecedenceTooHigh
                )
                .extensionRight
            )
    }


temporaryErrPrecedenceTooHigh : Result String a
temporaryErrPrecedenceTooHigh =
    Err "infix operator precedence too high"


type ExtensionRight
    = ExtendRightByOperation
        { symbol : String
        , direction : Infix.InfixDirection
        , expression : Node Expression
        }
