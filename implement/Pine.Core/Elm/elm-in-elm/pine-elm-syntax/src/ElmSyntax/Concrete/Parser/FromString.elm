module ElmSyntax.Concrete.Parser.FromString exposing (..)

import Char
import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.Infix as Infix
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.Token as Token
import ElmSyntax.Concrete.Parser.TokensFromString as TokensFromString
import ElmSyntax.Concrete.Pattern as Pattern
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList


parseExpression : String -> Result String Expression.Expression
parseExpression input =
    TokensFromString.parseExpression input
        |> Result.andThen parseExpressionTokens


parseExpressionTokens : List Token.Token -> Result String Expression.Expression
parseExpressionTokens tokens =
    parseExpressionNode tokens
        |> Result.andThen
            (\( expressionNode, remaining ) ->
                case dropTrivia remaining of
                    [] ->
                        Ok (Node.value expressionNode)

                    nextToken :: _ ->
                        Err ("Unexpected token '" ++ nextToken.lexeme ++ "' after parsing expression.")
            )


parseExpressionNode : List Token.Token -> Result String ( Node Expression.Expression, List Token.Token )
parseExpressionNode tokens =
    parseExpressionNodeAt 0 0 tokens


parseExpressionNodeAt :
    Int
    -> Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseExpressionNodeAt indentMin minPrecedence tokens =
    parseApplication indentMin tokens
        |> Result.andThen (parseOperators indentMin minPrecedence)


parseOperators :
    Int
    -> Int
    -> ( Node Expression.Expression, List Token.Token )
    -> Result String ( Node Expression.Expression, List Token.Token )
parseOperators indentMin minPrecedence ( left, tokens ) =
    case dropTrivia tokens of
        operatorToken :: afterOperator ->
            case operatorInfo operatorToken of
                Just ( precedence, direction ) ->
                    if precedence < minPrecedence then
                        Ok ( left, tokens )

                    else
                        let
                            nextMinPrecedence =
                                if direction == Infix.Left || direction == Infix.Non then
                                    precedence + 1

                                else
                                    precedence
                        in
                        parseExpressionNodeAt indentMin nextMinPrecedence afterOperator
                            |> Result.andThen
                                (\( right, remaining ) ->
                                    parseOperators indentMin
                                        minPrecedence
                                        ( Node
                                            { start = (Node.range left).start
                                            , end = (Node.range right).end
                                            }
                                            (Expression.OperatorApplication
                                                (Node (tokenRange operatorToken) operatorToken.lexeme)
                                                direction
                                                left
                                                right
                                            )
                                        , remaining
                                        )
                                )

                Nothing ->
                    Ok ( left, tokens )

        [] ->
            Ok ( left, [] )


parseApplication :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseApplication indentMin tokens =
    parseBasicExpression indentMin tokens
        |> Result.andThen
            (\( function, remaining ) ->
                parseApplicationArguments indentMin function [] remaining
            )


parseApplicationArguments :
    Int
    -> Node Expression.Expression
    -> List (Node Expression.Expression)
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseApplicationArguments indentMin function argumentsRev tokens =
    case dropTrivia tokens of
        next :: _ ->
            if next.start.column > indentMin && canStartArgumentExpression next then
                parseBasicExpression indentMin tokens
                    |> Result.andThen
                        (\( argument, remaining ) ->
                            parseApplicationArguments indentMin function (argument :: argumentsRev) remaining
                        )

            else
                finishApplication function argumentsRev tokens

        [] ->
            finishApplication function argumentsRev []


finishApplication :
    Node Expression.Expression
    -> List (Node Expression.Expression)
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
finishApplication function argumentsRev remaining =
    case List.reverse argumentsRev of
        [] ->
            Ok ( function, remaining )

        arguments ->
            case List.reverse arguments of
                lastArgument :: _ ->
                    Ok
                        ( Node
                            { start = (Node.range function).start
                            , end = (Node.range lastArgument).end
                            }
                            (Expression.Application function arguments)
                        , remaining
                        )

                [] ->
                    Ok ( function, remaining )


parseBasicExpression :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseBasicExpression indentMin tokens =
    parseAtomicExpression indentMin tokens
        |> Result.andThen (parseRecordAccesses indentMin)


parseRecordAccesses :
    Int
    -> ( Node Expression.Expression, List Token.Token )
    -> Result String ( Node Expression.Expression, List Token.Token )
parseRecordAccesses indentMin ( record, tokens ) =
    case dropTrivia tokens of
        dotToken :: fieldToken :: rest ->
            if
                dotToken.tokenType
                    == Token.Dot
                    && fieldToken.tokenType
                    == Token.Identifier
                    && (Node.range record).end
                    == dotToken.start
                    && dotToken.end
                    == fieldToken.start
            then
                let
                    access =
                        Node
                            { start = (Node.range record).start, end = fieldToken.end }
                            (Expression.RecordAccess
                                record
                                (Node (tokenRange fieldToken) fieldToken.lexeme)
                            )
                in
                parseRecordAccesses indentMin ( access, rest )

            else
                Ok ( record, tokens )

        _ ->
            Ok ( record, tokens )


parseAtomicExpression :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseAtomicExpression indentMin tokens =
    case dropTrivia tokens of
        [] ->
            Err "No tokens to parse as an expression."

        token :: rest ->
            case token.tokenType of
                Token.StringLiteral ->
                    Ok
                        ( Node (tokenRange token)
                            (Expression.StringLiteral token.lexeme token.rawText)
                        , rest
                        )

                Token.TripleQuotedStringLiteral ->
                    Ok
                        ( Node (tokenRange token)
                            (Expression.MultilineStringLiteral token.lexeme
                                (Maybe.map (String.split "\n") token.rawText)
                            )
                        , rest
                        )

                Token.CharLiteral ->
                    case String.uncons token.lexeme of
                        Just ( char, "" ) ->
                            Ok ( Node (tokenRange token) (Expression.CharLiteral (Char.toCode char)), rest )

                        _ ->
                            Err ("Invalid character literal at " ++ locationString token.start ++ ".")

                Token.NumberLiteral ->
                    Ok ( Node (tokenRange token) (parseNumber token.lexeme), rest )

                Token.Identifier ->
                    if token.lexeme == "let" then
                        parseLetBlock indentMin token rest

                    else if token.lexeme == "if" then
                        parseIfBlock indentMin token rest

                    else if token.lexeme == "case" then
                        parseCaseBlock indentMin token rest

                    else
                        parseIdentifier token rest

                Token.OpenBracket ->
                    parseList indentMin token rest

                Token.OpenParen ->
                    parseParenthesizedOrTuple indentMin token rest

                Token.OpenBrace ->
                    parseRecord indentMin token rest

                Token.Negation ->
                    parseBasicExpression indentMin rest
                        |> Result.map
                            (\( negated, remaining ) ->
                                ( Node
                                    { start = token.start, end = (Node.range negated).end }
                                    (Expression.Negation negated)
                                , remaining
                                )
                            )

                Token.Lambda ->
                    parseLambda indentMin token rest

                Token.Dot ->
                    case dropTrivia rest of
                        fieldToken :: remaining ->
                            if fieldToken.tokenType == Token.Identifier then
                                Ok
                                    ( Node
                                        { start = token.start, end = fieldToken.end }
                                        (Expression.RecordAccessFunction ("." ++ fieldToken.lexeme))
                                    , remaining
                                    )

                            else
                                Err ("Expected a record field name after '.', but found '" ++ fieldToken.lexeme ++ "'.")

                        [] ->
                            Err "Expected a record field name after '.'."

                _ ->
                    Err ("Failed to parse expression: Unexpected token '" ++ token.lexeme ++ "'.")


parseIdentifier :
    Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseIdentifier firstToken tokens =
    parseQualifiedName [ firstToken ] tokens
        |> (\( nameTokens, remaining ) ->
                case List.reverse nameTokens of
                    nameToken :: reversedModuleTokens ->
                        Ok
                            ( Node
                                { start = firstToken.start, end = nameToken.end }
                                (Expression.Identifier
                                    (List.reverse reversedModuleTokens |> List.map .lexeme)
                                    nameToken.lexeme
                                )
                            , remaining
                            )

                    [] ->
                        Err "Expected an identifier."
           )


parseQualifiedName :
    List Token.Token
    -> List Token.Token
    -> ( List Token.Token, List Token.Token )
parseQualifiedName nameTokens tokens =
    case ( List.reverse nameTokens, dropTrivia tokens ) of
        ( lastName :: _, dotToken :: nextName :: rest ) ->
            if startsWithUpper lastName.lexeme && dotToken.tokenType == Token.Dot && nextName.tokenType == Token.Identifier then
                parseQualifiedName (nameTokens ++ [ nextName ]) rest

            else
                ( nameTokens, tokens )

        _ ->
            ( nameTokens, tokens )


parseList :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseList indentMin openToken tokens =
    parseSeparatedExpressions indentMin Token.CloseBracket tokens
        |> Result.map
            (\( elements, closeToken, remaining ) ->
                ( Node
                    { start = openToken.start, end = closeToken.end }
                    (Expression.ListExpr elements)
                , remaining
                )
            )


parseParenthesizedOrTuple :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseParenthesizedOrTuple indentMin openToken tokens =
    case dropTrivia tokens of
        firstToken :: rest ->
            if firstToken.tokenType == Token.CloseParen then
                Ok
                    ( Node
                        { start = openToken.start, end = firstToken.end }
                        Expression.UnitExpr
                    , rest
                    )

            else
                case dropTrivia rest of
                    closeToken :: afterClose ->
                        if firstToken.tokenType == Token.Operator && closeToken.tokenType == Token.CloseParen then
                            Ok
                                ( Node
                                    { start = openToken.start, end = closeToken.end }
                                    (Expression.PrefixOperator firstToken.lexeme)
                                , afterClose
                                )

                        else
                            parseNonEmptyParenthesized indentMin openToken tokens

                    [] ->
                        parseNonEmptyParenthesized indentMin openToken tokens

        [] ->
            Err "Expected an expression or ')' after '('."


parseNonEmptyParenthesized :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseNonEmptyParenthesized indentMin openToken tokens =
    parseExpressionNodeAt indentMin 0 tokens
        |> Result.andThen
            (\( first, afterFirst ) ->
                parseFurtherSeparatedExpressions indentMin Token.CloseParen afterFirst []
                    |> Result.map
                        (\( further, closeToken, remaining ) ->
                            let
                                expression =
                                    case further of
                                        [] ->
                                            Expression.Parenthesized first

                                        _ ->
                                            Expression.TupledExpression
                                                (SeparatedSyntaxList.NonEmpty first further)
                            in
                            ( Node
                                { start = openToken.start, end = closeToken.end }
                                expression
                            , remaining
                            )
                        )
            )


parseSeparatedExpressions :
    Int
    -> Token.TokenType
    -> List Token.Token
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node Expression.Expression), Token.Token, List Token.Token )
parseSeparatedExpressions indentMin closingType tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == closingType then
                Ok ( SeparatedSyntaxList.Empty, closeToken, rest )

            else
                parseNonEmptySeparatedExpressions indentMin closingType tokens

        [] ->
            Err "Expected a closing delimiter."


parseNonEmptySeparatedExpressions :
    Int
    -> Token.TokenType
    -> List Token.Token
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node Expression.Expression), Token.Token, List Token.Token )
parseNonEmptySeparatedExpressions indentMin closingType tokens =
    parseExpressionNodeAt indentMin 0 tokens
        |> Result.andThen
            (\( first, afterFirst ) ->
                parseFurtherSeparatedExpressions indentMin closingType afterFirst []
                    |> Result.map
                        (\( further, closeToken, remaining ) ->
                            ( SeparatedSyntaxList.NonEmpty first further, closeToken, remaining )
                        )
            )


parseFurtherSeparatedExpressions :
    Int
    -> Token.TokenType
    -> List Token.Token
    -> List ( Location, Node Expression.Expression )
    -> Result String ( List ( Location, Node Expression.Expression ), Token.Token, List Token.Token )
parseFurtherSeparatedExpressions indentMin closingType tokens furtherRev =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == closingType then
                Ok ( List.reverse furtherRev, token, rest )

            else if token.tokenType == Token.Comma then
                parseExpressionNodeAt indentMin 0 rest
                    |> Result.andThen
                        (\( expression, remaining ) ->
                            parseFurtherSeparatedExpressions indentMin
                                closingType
                                remaining
                                (( token.start, expression ) :: furtherRev)
                        )

            else
                Err ("Expected ',' or a closing delimiter, but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err "Expected a closing delimiter."


parseIfBlock :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseIfBlock indentMin ifToken tokens =
    let
        branchIndentMin =
            min indentMin ifToken.start.column
    in
    parseExpressionNodeAt branchIndentMin 0 tokens
        |> Result.andThen
            (\( condition, afterCondition ) ->
                consumeKeyword "then" afterCondition
                    |> Result.andThen
                        (\( thenToken, afterThen ) ->
                            parseExpressionNodeAt branchIndentMin 0 afterThen
                                |> Result.andThen
                                    (\( thenBranch, afterThenBranch ) ->
                                        consumeKeyword "else" afterThenBranch
                                            |> Result.andThen
                                                (\( elseToken, afterElse ) ->
                                                    parseExpressionNodeAt branchIndentMin 0 afterElse
                                                        |> Result.map
                                                            (\( elseBranch, remaining ) ->
                                                                ( Node
                                                                    { start = ifToken.start
                                                                    , end = (Node.range elseBranch).end
                                                                    }
                                                                    (Expression.IfBlock
                                                                        ifToken.start
                                                                        condition
                                                                        thenToken.start
                                                                        thenBranch
                                                                        elseToken.start
                                                                        elseBranch
                                                                    )
                                                                , remaining
                                                                )
                                                            )
                                                )
                                    )
                        )
            )


parseLambda :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseLambda indentMin lambdaToken tokens =
    parseLambdaArguments indentMin tokens []
        |> Result.andThen
            (\( arguments, arrowToken, afterArrow ) ->
                if List.isEmpty arguments then
                    Err "Expected at least one argument in lambda expression."

                else
                    parseExpressionNodeAt indentMin 0 afterArrow
                        |> Result.map
                            (\( body, remaining ) ->
                                ( Node
                                    { start = lambdaToken.start, end = (Node.range body).end }
                                    (Expression.LambdaExpression
                                        { backslashLocation = lambdaToken.start
                                        , arguments = arguments
                                        , arrowLocation = arrowToken.start
                                        , expression = body
                                        }
                                    )
                                , remaining
                                )
                            )
            )


parseLambdaArguments :
    Int
    -> List Token.Token
    -> List (Node Pattern.Pattern)
    -> Result String ( List (Node Pattern.Pattern), Token.Token, List Token.Token )
parseLambdaArguments indentMin tokens argumentsRev =
    case dropTrivia tokens of
        [] ->
            Err "Expected '->' in lambda expression."

        token :: rest ->
            if token.tokenType == Token.Arrow then
                Ok ( List.reverse argumentsRev, token, rest )

            else
                parsePatternNodeAt indentMin (token :: rest)
                    |> Result.andThen
                        (\( argument, remaining ) ->
                            parseLambdaArguments indentMin remaining (argument :: argumentsRev)
                        )


parseLetBlock :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseLetBlock indentMin letToken tokens =
    parseLetDeclarations (min indentMin letToken.start.column) tokens []
        |> Result.andThen
            (\( declarations, inToken, afterIn ) ->
                if List.isEmpty declarations then
                    Err "Expected at least one declaration in let expression."

                else
                    parseExpressionNodeAt indentMin 0 afterIn
                        |> Result.map
                            (\( body, remaining ) ->
                                ( Node
                                    { start = letToken.start, end = (Node.range body).end }
                                    (Expression.LetExpression
                                        { letTokenLocation = letToken.start
                                        , declarations = declarations
                                        , inTokenLocation = inToken.start
                                        , expression = body
                                        }
                                    )
                                , remaining
                                )
                            )
            )


parseLetDeclarations :
    Int
    -> List Token.Token
    -> List (Node Expression.LetDeclaration)
    -> Result String ( List (Node Expression.LetDeclaration), Token.Token, List Token.Token )
parseLetDeclarations indentMin tokens declarationsRev =
    case dropTrivia tokens of
        [] ->
            Err "Expected 'in' in let expression."

        token :: rest ->
            if token.tokenType == Token.Identifier && token.lexeme == "in" then
                Ok ( List.reverse declarationsRev, token, rest )

            else if token.start.column <= indentMin then
                Err ("Expected 'in' in let expression, but found '" ++ token.lexeme ++ "'.")

            else
                parseLetDeclaration token.start.column (token :: rest)
                    |> Result.andThen
                        (\( declaration, remaining ) ->
                            parseLetDeclarations indentMin remaining (declaration :: declarationsRev)
                        )


parseLetDeclaration :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.LetDeclaration, List Token.Token )
parseLetDeclaration declarationIndent tokens =
    case dropTrivia tokens of
        nameToken :: rest ->
            if nameToken.tokenType == Token.Identifier then
                parsePatternsUntilEqual declarationIndent rest []
                    |> Result.andThen
                        (\( arguments, equalToken, afterEqual ) ->
                            parseExpressionNodeAt declarationIndent 0 afterEqual
                                |> Result.map
                                    (\( body, remaining ) ->
                                        let
                                            declarationRange =
                                                { start = nameToken.start, end = (Node.range body).end }
                                        in
                                        ( Node declarationRange
                                            (Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Node declarationRange
                                                        { name = Node (tokenRange nameToken) nameToken.lexeme
                                                        , arguments = arguments
                                                        , equalsTokenLocation = equalToken.start
                                                        , expression = body
                                                        }
                                                }
                                            )
                                        , remaining
                                        )
                                    )
                        )

            else
                parsePatternNodeAt declarationIndent (nameToken :: rest)
                    |> Result.andThen
                        (\( pattern, afterPattern ) ->
                            consumeToken Token.Equal "'='" afterPattern
                                |> Result.andThen
                                    (\( equalToken, afterEqual ) ->
                                        parseExpressionNodeAt declarationIndent 0 afterEqual
                                            |> Result.map
                                                (\( body, remaining ) ->
                                                    ( Node
                                                        { start = (Node.range pattern).start
                                                        , end = (Node.range body).end
                                                        }
                                                        (Expression.LetDestructuring pattern equalToken.start body)
                                                    , remaining
                                                    )
                                                )
                                    )
                        )

        [] ->
            Err "Expected a declaration in let expression."


parsePatternsUntilEqual :
    Int
    -> List Token.Token
    -> List (Node Pattern.Pattern)
    -> Result String ( List (Node Pattern.Pattern), Token.Token, List Token.Token )
parsePatternsUntilEqual indentMin tokens patternsRev =
    case dropTrivia tokens of
        [] ->
            Err "Expected '=' in let declaration."

        token :: rest ->
            if token.tokenType == Token.Equal then
                Ok ( List.reverse patternsRev, token, rest )

            else
                parsePatternNodeAt indentMin (token :: rest)
                    |> Result.andThen
                        (\( pattern, remaining ) ->
                            parsePatternsUntilEqual indentMin remaining (pattern :: patternsRev)
                        )


parseCaseBlock :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseCaseBlock indentMin caseToken tokens =
    parseExpressionNodeAt caseToken.start.column 0 tokens
        |> Result.andThen
            (\( subject, afterSubject ) ->
                consumeKeyword "of" afterSubject
                    |> Result.andThen
                        (\( ofToken, afterOf ) ->
                            case dropTrivia afterOf of
                                firstBranchToken :: _ ->
                                    parseCaseBranches
                                        (min firstBranchToken.start.column (indentMin + 1))
                                        firstBranchToken.start.column
                                        afterOf
                                        []
                                        |> Result.andThen
                                            (\( branches, remaining ) ->
                                                case List.reverse branches of
                                                    [] ->
                                                        Err "Expected at least one case branch after 'of'."

                                                    lastBranch :: _ ->
                                                        Ok
                                                            ( Node
                                                                { start = caseToken.start
                                                                , end = (Node.range lastBranch.expression).end
                                                                }
                                                                (Expression.CaseExpression
                                                                    { caseTokenLocation = caseToken.start
                                                                    , expression = subject
                                                                    , ofTokenLocation = ofToken.start
                                                                    , cases = branches
                                                                    }
                                                                )
                                                            , remaining
                                                            )
                                            )

                                [] ->
                                    Err "Expected at least one case branch after 'of'."
                        )
            )


parseCaseBranches :
    Int
    -> Int
    -> List Token.Token
    -> List Expression.Case
    -> Result String ( List Expression.Case, List Token.Token )
parseCaseBranches lowerBound branchIndent tokens branchesRev =
    case dropTrivia tokens of
        [] ->
            Ok ( List.reverse branchesRev, [] )

        token :: _ ->
            if token.start.column < lowerBound || isClosingToken token then
                Ok ( List.reverse branchesRev, tokens )

            else
                parseCaseBranch branchIndent tokens
                    |> Result.andThen
                        (\( branch, remaining ) ->
                            parseCaseBranches lowerBound branchIndent remaining (branch :: branchesRev)
                        )


parseCaseBranch :
    Int
    -> List Token.Token
    -> Result String ( Expression.Case, List Token.Token )
parseCaseBranch branchIndent tokens =
    parsePatternNodeAt branchIndent tokens
        |> Result.andThen
            (\( pattern, afterPattern ) ->
                consumeToken Token.Arrow "'->'" afterPattern
                    |> Result.andThen
                        (\( arrowToken, afterArrow ) ->
                            parseExpressionNodeAt branchIndent 0 afterArrow
                                |> Result.map
                                    (\( body, remaining ) ->
                                        ( { pattern = pattern
                                          , arrowLocation = arrowToken.start
                                          , expression = body
                                          }
                                        , remaining
                                        )
                                    )
                        )
            )


parseRecord :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseRecord indentMin openToken tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseBrace then
                Ok
                    ( Node
                        { start = openToken.start, end = closeToken.end }
                        (Expression.RecordExpr SeparatedSyntaxList.Empty)
                    , rest
                    )

            else
                parseNonEmptyRecord indentMin openToken tokens

        [] ->
            Err "Expected '}' in record expression."


parseNonEmptyRecord :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseNonEmptyRecord indentMin openToken tokens =
    case dropTrivia tokens of
        nameToken :: afterName ->
            if nameToken.tokenType /= Token.Identifier then
                Err ("Expected a record field name, but found '" ++ nameToken.lexeme ++ "'.")

            else
                case dropTrivia afterName of
                    pipeToken :: afterPipe ->
                        if pipeToken.tokenType == Token.Pipe then
                            parseRecordFields indentMin afterPipe
                                |> Result.map
                                    (\( fields, closeToken, remaining ) ->
                                        ( Node
                                            { start = openToken.start, end = closeToken.end }
                                            (Expression.RecordUpdateExpression
                                                (Node (tokenRange nameToken) nameToken.lexeme)
                                                pipeToken.start
                                                fields
                                            )
                                        , remaining
                                        )
                                    )

                        else
                            parseRecordFieldsWithFirst indentMin nameToken afterName
                                |> Result.map
                                    (\( fields, closeToken, remaining ) ->
                                        ( Node
                                            { start = openToken.start, end = closeToken.end }
                                            (Expression.RecordExpr fields)
                                        , remaining
                                        )
                                    )

                    [] ->
                        Err "Expected '=' or '|' in record expression."

        [] ->
            Err "Expected a record field."


parseRecordFields :
    Int
    -> List Token.Token
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField, Token.Token, List Token.Token )
parseRecordFields indentMin tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseBrace then
                Ok ( SeparatedSyntaxList.Empty, closeToken, rest )

            else
                parseRecordFieldsWithFirst indentMin closeToken rest

        [] ->
            Err "Expected a record field or '}'."


parseRecordFieldsWithFirst :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField, Token.Token, List Token.Token )
parseRecordFieldsWithFirst indentMin fieldName tokens =
    parseRecordField indentMin fieldName tokens
        |> Result.andThen
            (\( firstField, remaining ) ->
                parseFurtherRecordFields indentMin remaining []
                    |> Result.map
                        (\( furtherFields, closeToken, afterClose ) ->
                            ( SeparatedSyntaxList.NonEmpty firstField furtherFields
                            , closeToken
                            , afterClose
                            )
                        )
            )


parseRecordField :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Expression.RecordExprField, List Token.Token )
parseRecordField indentMin fieldName tokens =
    if fieldName.tokenType /= Token.Identifier then
        Err ("Expected a record field name, but found '" ++ fieldName.lexeme ++ "'.")

    else
        consumeToken Token.Equal "'='" tokens
            |> Result.andThen
                (\( equalToken, afterEqual ) ->
                    parseExpressionNodeAt indentMin 0 afterEqual
                        |> Result.map
                            (\( valueExpression, remaining ) ->
                                ( { fieldName = Node (tokenRange fieldName) fieldName.lexeme
                                  , equalsLocation = equalToken.start
                                  , valueExpr = valueExpression
                                  }
                                , remaining
                                )
                            )
                )


parseFurtherRecordFields :
    Int
    -> List Token.Token
    -> List ( Location, Expression.RecordExprField )
    -> Result String ( List ( Location, Expression.RecordExprField ), Token.Token, List Token.Token )
parseFurtherRecordFields indentMin tokens fieldsRev =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.CloseBrace then
                Ok ( List.reverse fieldsRev, token, rest )

            else if token.tokenType == Token.Comma then
                case dropTrivia rest of
                    fieldName :: afterName ->
                        parseRecordField indentMin fieldName afterName
                            |> Result.andThen
                                (\( field, remaining ) ->
                                    parseFurtherRecordFields indentMin
                                        remaining
                                        (( token.start, field ) :: fieldsRev)
                                )

                    [] ->
                        Err "Expected a record field after ','."

            else
                Err ("Expected ',' or '}', but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err "Expected '}' in record expression."


parsePatternNode : List Token.Token -> Result String ( Node Pattern.Pattern, List Token.Token )
parsePatternNode tokens =
    parsePatternNodeAt 0 tokens


parsePatternNodeAt :
    Int
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parsePatternNodeAt indentMin tokens =
    parsePatternAtomic indentMin tokens
        |> Result.andThen
            (\( pattern, remaining ) ->
                parseNamedPatternArguments indentMin pattern remaining
                    |> Result.andThen (parsePatternSuffix indentMin)
            )


parseNamedPatternArguments :
    Int
    -> Node Pattern.Pattern
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseNamedPatternArguments indentMin pattern tokens =
    case Node.value pattern of
        Pattern.NamedPattern name [] ->
            parsePatternArguments indentMin name pattern [] tokens

        _ ->
            Ok ( pattern, tokens )


parsePatternArguments :
    Int
    -> Pattern.QualifiedNameRef
    -> Node Pattern.Pattern
    -> List (Node Pattern.Pattern)
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parsePatternArguments indentMin name original argumentsRev tokens =
    case dropTrivia tokens of
        next :: _ ->
            if next.start.column >= indentMin && canStartNamedPatternArgument next then
                parsePatternAtomic indentMin tokens
                    |> Result.andThen
                        (\( argument, remaining ) ->
                            parsePatternArguments indentMin name original (argument :: argumentsRev) remaining
                        )

            else
                finishNamedPattern name original argumentsRev tokens

        [] ->
            finishNamedPattern name original argumentsRev []


finishNamedPattern :
    Pattern.QualifiedNameRef
    -> Node Pattern.Pattern
    -> List (Node Pattern.Pattern)
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
finishNamedPattern name original argumentsRev tokens =
    case List.reverse argumentsRev of
        [] ->
            Ok ( original, tokens )

        arguments ->
            case List.reverse arguments of
                lastArgument :: _ ->
                    Ok
                        ( Node
                            { start = (Node.range original).start
                            , end = (Node.range lastArgument).end
                            }
                            (Pattern.NamedPattern name arguments)
                        , tokens
                        )

                [] ->
                    Ok ( original, tokens )


parsePatternSuffix :
    Int
    -> ( Node Pattern.Pattern, List Token.Token )
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parsePatternSuffix indentMin ( pattern, tokens ) =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.Operator && token.lexeme == "::" then
                parsePatternNodeAt indentMin rest
                    |> Result.map
                        (\( tailPattern, remaining ) ->
                            ( Node
                                { start = (Node.range pattern).start
                                , end = (Node.range tailPattern).end
                                }
                                (Pattern.UnConsPattern pattern token.start tailPattern)
                            , remaining
                            )
                        )

            else if token.tokenType == Token.Identifier && token.lexeme == "as" then
                case dropTrivia rest of
                    nameToken :: remaining ->
                        if nameToken.tokenType == Token.Identifier then
                            Ok
                                ( Node
                                    { start = (Node.range pattern).start, end = nameToken.end }
                                    (Pattern.AsPattern
                                        pattern
                                        token.start
                                        (Node (tokenRange nameToken) nameToken.lexeme)
                                    )
                                , remaining
                                )

                        else
                            Err ("Expected a pattern name after 'as', but found '" ++ nameToken.lexeme ++ "'.")

                    [] ->
                        Err "Expected a pattern name after 'as'."

            else
                Ok ( pattern, tokens )

        [] ->
            Ok ( pattern, [] )


parsePatternAtomic :
    Int
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parsePatternAtomic indentMin tokens =
    case dropTrivia tokens of
        [] ->
            Err "Expected a pattern."

        token :: rest ->
            case token.tokenType of
                Token.Identifier ->
                    if token.lexeme == "_" then
                        Ok ( Node (tokenRange token) Pattern.AllPattern, rest )

                    else if startsWithUpper token.lexeme then
                        let
                            ( nameTokens, remaining ) =
                                parseQualifiedName [ token ] rest
                        in
                        case List.reverse nameTokens of
                            nameToken :: reversedModuleTokens ->
                                Ok
                                    ( Node
                                        { start = token.start, end = nameToken.end }
                                        (Pattern.NamedPattern
                                            { moduleName = List.reverse reversedModuleTokens |> List.map .lexeme
                                            , name = nameToken.lexeme
                                            }
                                            []
                                        )
                                    , remaining
                                    )

                            [] ->
                                Err "Expected a named pattern."

                    else
                        Ok ( Node (tokenRange token) (Pattern.VarPattern token.lexeme), rest )

                Token.StringLiteral ->
                    Ok ( Node (tokenRange token) (Pattern.StringPattern token.lexeme), rest )

                Token.TripleQuotedStringLiteral ->
                    Ok ( Node (tokenRange token) (Pattern.StringPattern token.lexeme), rest )

                Token.CharLiteral ->
                    case String.uncons token.lexeme of
                        Just ( char, "" ) ->
                            Ok ( Node (tokenRange token) (Pattern.CharPattern (Char.toCode char)), rest )

                        _ ->
                            Err "Invalid character pattern."

                Token.NumberLiteral ->
                    if String.startsWith "0x" token.lexeme then
                        case hexStringToInt (String.dropLeft 2 token.lexeme) of
                            Just value ->
                                Ok ( Node (tokenRange token) (Pattern.HexPattern value), rest )

                            Nothing ->
                                Err ("Invalid hexadecimal pattern '" ++ token.lexeme ++ "'.")

                    else if String.contains "." token.lexeme || String.contains "e" token.lexeme || String.contains "E" token.lexeme then
                        case String.toFloat token.lexeme of
                            Just value ->
                                Ok ( Node (tokenRange token) (Pattern.FloatPattern value), rest )

                            Nothing ->
                                Err ("Invalid float pattern '" ++ token.lexeme ++ "'.")

                    else
                        case String.toInt token.lexeme of
                            Just value ->
                                Ok ( Node (tokenRange token) (Pattern.IntPattern value), rest )

                            Nothing ->
                                Err ("Invalid integer pattern '" ++ token.lexeme ++ "'.")

                Token.OpenParen ->
                    parseTuplePattern indentMin token rest

                Token.OpenBracket ->
                    parseListPattern indentMin token rest

                Token.OpenBrace ->
                    parseRecordPattern token rest

                _ ->
                    Err ("Expected a pattern, but found '" ++ token.lexeme ++ "'.")


parseTuplePattern :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseTuplePattern indentMin openToken tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseParen then
                Ok
                    ( Node { start = openToken.start, end = closeToken.end } Pattern.UnitPattern
                    , rest
                    )

            else
                parseNonEmptyTuplePattern indentMin openToken tokens

        [] ->
            Err "Expected ')' in pattern."


parseNonEmptyTuplePattern :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseNonEmptyTuplePattern indentMin openToken tokens =
    parsePatternNodeAt indentMin tokens
        |> Result.andThen
            (\( first, afterFirst ) ->
                parseFurtherPatterns indentMin Token.CloseParen afterFirst []
                    |> Result.map
                        (\( further, closeToken, remaining ) ->
                            let
                                pattern =
                                    case further of
                                        [] ->
                                            Pattern.ParenthesizedPattern first

                                        _ ->
                                            Pattern.TuplePattern (SeparatedSyntaxList.NonEmpty first further)
                            in
                            ( Node { start = openToken.start, end = closeToken.end } pattern
                            , remaining
                            )
                        )
            )


parseListPattern :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseListPattern indentMin openToken tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseBracket then
                Ok
                    ( Node
                        { start = openToken.start, end = closeToken.end }
                        (Pattern.ListPattern SeparatedSyntaxList.Empty)
                    , rest
                    )

            else
                parsePatternNodeAt indentMin tokens
                    |> Result.andThen
                        (\( first, afterFirst ) ->
                            parseFurtherPatterns indentMin Token.CloseBracket afterFirst []
                                |> Result.map
                                    (\( further, closing, remaining ) ->
                                        ( Node
                                            { start = openToken.start, end = closing.end }
                                            (Pattern.ListPattern (SeparatedSyntaxList.NonEmpty first further))
                                        , remaining
                                        )
                                    )
                        )

        [] ->
            Err "Expected ']' in pattern."


parseFurtherPatterns :
    Int
    -> Token.TokenType
    -> List Token.Token
    -> List ( Location, Node Pattern.Pattern )
    -> Result String ( List ( Location, Node Pattern.Pattern ), Token.Token, List Token.Token )
parseFurtherPatterns indentMin closingType tokens furtherRev =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == closingType then
                Ok ( List.reverse furtherRev, token, rest )

            else if token.tokenType == Token.Comma then
                parsePatternNodeAt indentMin rest
                    |> Result.andThen
                        (\( pattern, remaining ) ->
                            parseFurtherPatterns indentMin
                                closingType
                                remaining
                                (( token.start, pattern ) :: furtherRev)
                        )

            else
                Err ("Expected ',' or a closing delimiter in pattern, but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err "Expected a closing delimiter in pattern."


parseRecordPattern :
    Token.Token
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseRecordPattern openToken tokens =
    parseRecordPatternFields tokens Nothing []
        |> Result.map
            (\( fields, closeToken, remaining ) ->
                ( Node
                    { start = openToken.start, end = closeToken.end }
                    (Pattern.RecordPattern fields)
                , remaining
                )
            )


parseRecordPatternFields :
    List Token.Token
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), Token.Token, List Token.Token )
parseRecordPatternFields tokens firstField furtherRev =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.CloseBrace then
                case firstField of
                    Nothing ->
                        Ok ( SeparatedSyntaxList.Empty, token, rest )

                    Just first ->
                        Ok ( SeparatedSyntaxList.NonEmpty first (List.reverse furtherRev), token, rest )

            else if token.tokenType == Token.Identifier then
                let
                    field =
                        Node (tokenRange token) token.lexeme
                in
                case firstField of
                    Nothing ->
                        parseRecordPatternFieldsAfterField rest (Just field) furtherRev

                    Just _ ->
                        Err "Expected ',' before record pattern field."

            else
                Err ("Expected a record pattern field or '}', but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err "Expected '}' in record pattern."


parseRecordPatternFieldsAfterField :
    List Token.Token
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), Token.Token, List Token.Token )
parseRecordPatternFieldsAfterField tokens firstField furtherRev =
    case dropTrivia tokens of
        commaToken :: rest ->
            if commaToken.tokenType == Token.Comma then
                case dropTrivia rest of
                    fieldToken :: afterField ->
                        if fieldToken.tokenType == Token.Identifier then
                            parseRecordPatternFieldsAfterField afterField
                                firstField
                                (( commaToken.start, Node (tokenRange fieldToken) fieldToken.lexeme ) :: furtherRev)

                        else
                            Err ("Expected a record pattern field after ',', but found '" ++ fieldToken.lexeme ++ "'.")

                    [] ->
                        Err "Expected a record pattern field after ','."

            else
                parseRecordPatternFields tokens firstField furtherRev

        _ ->
            parseRecordPatternFields tokens firstField furtherRev


consumeKeyword : String -> List Token.Token -> Result String ( Token.Token, List Token.Token )
consumeKeyword keyword tokens =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.Identifier && token.lexeme == keyword then
                Ok ( token, rest )

            else
                Err ("Expected '" ++ keyword ++ "', but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err ("Expected '" ++ keyword ++ "'.")


consumeToken :
    Token.TokenType
    -> String
    -> List Token.Token
    -> Result String ( Token.Token, List Token.Token )
consumeToken tokenType description tokens =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == tokenType then
                Ok ( token, rest )

            else
                Err ("Expected " ++ description ++ ", but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err ("Expected " ++ description ++ ".")


operatorInfo : Token.Token -> Maybe ( Int, Infix.InfixDirection )
operatorInfo token =
    if token.tokenType /= Token.Operator then
        Nothing

    else
        case token.lexeme of
            "<|" ->
                Just ( 0, Infix.Right )

            "|>" ->
                Just ( 0, Infix.Left )

            "||" ->
                Just ( 2, Infix.Right )

            "&&" ->
                Just ( 3, Infix.Right )

            "==" ->
                Just ( 4, Infix.Non )

            "/=" ->
                Just ( 4, Infix.Non )

            "<" ->
                Just ( 4, Infix.Non )

            ">" ->
                Just ( 4, Infix.Non )

            "<=" ->
                Just ( 4, Infix.Non )

            ">=" ->
                Just ( 4, Infix.Non )

            "++" ->
                Just ( 5, Infix.Right )

            "::" ->
                Just ( 5, Infix.Right )

            "+" ->
                Just ( 6, Infix.Left )

            "-" ->
                Just ( 6, Infix.Left )

            "*" ->
                Just ( 7, Infix.Left )

            "//" ->
                Just ( 7, Infix.Left )

            "/" ->
                Just ( 7, Infix.Left )

            "^" ->
                Just ( 8, Infix.Right )

            "<<" ->
                Just ( 9, Infix.Left )

            ">>" ->
                Just ( 9, Infix.Right )

            "|=" ->
                Just ( 5, Infix.Left )

            "|." ->
                Just ( 6, Infix.Left )

            "</>" ->
                Just ( 7, Infix.Right )

            "<?>" ->
                Just ( 8, Infix.Left )

            _ ->
                Nothing


canStartArgumentExpression : Token.Token -> Bool
canStartArgumentExpression token =
    not (isKeyword token)
        && List.member token.tokenType
            [ Token.StringLiteral
            , Token.TripleQuotedStringLiteral
            , Token.CharLiteral
            , Token.NumberLiteral
            , Token.Identifier
            , Token.OpenParen
            , Token.OpenBrace
            , Token.OpenBracket
            , Token.Negation
            , Token.Dot
            ]


canStartNamedPatternArgument : Token.Token -> Bool
canStartNamedPatternArgument token =
    canStartPattern token
        && not
            (token.tokenType
                == Token.Identifier
                && List.member token.lexeme [ "as", "of", "then", "else", "in", "let" ]
            )


canStartPattern : Token.Token -> Bool
canStartPattern token =
    List.member token.tokenType
        [ Token.StringLiteral
        , Token.TripleQuotedStringLiteral
        , Token.CharLiteral
        , Token.NumberLiteral
        , Token.Identifier
        , Token.OpenParen
        , Token.OpenBrace
        , Token.OpenBracket
        ]


isKeyword : Token.Token -> Bool
isKeyword token =
    token.tokenType
        == Token.Identifier
        && List.member token.lexeme [ "if", "then", "else", "let", "in", "case", "of" ]


isClosingToken : Token.Token -> Bool
isClosingToken token =
    List.member token.tokenType
        [ Token.Comma
        , Token.CloseParen
        , Token.CloseBracket
        , Token.CloseBrace
        ]


isTrivia : Token.Token -> Bool
isTrivia token =
    token.tokenType == Token.Whitespace
        || token.tokenType == Token.Newline
        || token.tokenType == Token.Comment


dropTrivia : List Token.Token -> List Token.Token
dropTrivia tokens =
    case tokens of
        token :: rest ->
            if isTrivia token then
                dropTrivia rest

            else
                tokens

        [] ->
            []


tokenRange : Token.Token -> Range
tokenRange token =
    { start = token.start, end = token.end }


startsWithUpper : String -> Bool
startsWithUpper name =
    case String.uncons name of
        Just ( 'A', _ ) ->
            True

        Just ( 'B', _ ) ->
            True

        Just ( 'C', _ ) ->
            True

        Just ( 'D', _ ) ->
            True

        Just ( 'E', _ ) ->
            True

        Just ( 'F', _ ) ->
            True

        Just ( 'G', _ ) ->
            True

        Just ( 'H', _ ) ->
            True

        Just ( 'I', _ ) ->
            True

        Just ( 'J', _ ) ->
            True

        Just ( 'K', _ ) ->
            True

        Just ( 'L', _ ) ->
            True

        Just ( 'M', _ ) ->
            True

        Just ( 'N', _ ) ->
            True

        Just ( 'O', _ ) ->
            True

        Just ( 'P', _ ) ->
            True

        Just ( 'Q', _ ) ->
            True

        Just ( 'R', _ ) ->
            True

        Just ( 'S', _ ) ->
            True

        Just ( 'T', _ ) ->
            True

        Just ( 'U', _ ) ->
            True

        Just ( 'V', _ ) ->
            True

        Just ( 'W', _ ) ->
            True

        Just ( 'X', _ ) ->
            True

        Just ( 'Y', _ ) ->
            True

        Just ( 'Z', _ ) ->
            True

        _ ->
            False


parseNumber : String -> Expression.Expression
parseNumber literal =
    if String.startsWith "0x" literal then
        Expression.IntegerLiteral literal

    else if String.contains "." literal || String.contains "e" literal || String.contains "E" literal then
        Expression.FloatLiteral literal

    else
        Expression.IntegerLiteral literal


hexStringToInt : String -> Maybe Int
hexStringToInt string =
    case String.toList string of
        [] ->
            Nothing

        chars ->
            hexCharsToInt 0 chars


hexCharsToInt : Int -> List Char -> Maybe Int
hexCharsToInt accumulated chars =
    case chars of
        [] ->
            Just accumulated

        char :: remaining ->
            case char of
                '0' ->
                    hexCharsToInt (accumulated * 16) remaining

                '1' ->
                    hexCharsToInt (accumulated * 16 + 1) remaining

                '2' ->
                    hexCharsToInt (accumulated * 16 + 2) remaining

                '3' ->
                    hexCharsToInt (accumulated * 16 + 3) remaining

                '4' ->
                    hexCharsToInt (accumulated * 16 + 4) remaining

                '5' ->
                    hexCharsToInt (accumulated * 16 + 5) remaining

                '6' ->
                    hexCharsToInt (accumulated * 16 + 6) remaining

                '7' ->
                    hexCharsToInt (accumulated * 16 + 7) remaining

                '8' ->
                    hexCharsToInt (accumulated * 16 + 8) remaining

                '9' ->
                    hexCharsToInt (accumulated * 16 + 9) remaining

                'a' ->
                    hexCharsToInt (accumulated * 16 + 10) remaining

                'A' ->
                    hexCharsToInt (accumulated * 16 + 10) remaining

                'b' ->
                    hexCharsToInt (accumulated * 16 + 11) remaining

                'B' ->
                    hexCharsToInt (accumulated * 16 + 11) remaining

                'c' ->
                    hexCharsToInt (accumulated * 16 + 12) remaining

                'C' ->
                    hexCharsToInt (accumulated * 16 + 12) remaining

                'd' ->
                    hexCharsToInt (accumulated * 16 + 13) remaining

                'D' ->
                    hexCharsToInt (accumulated * 16 + 13) remaining

                'e' ->
                    hexCharsToInt (accumulated * 16 + 14) remaining

                'E' ->
                    hexCharsToInt (accumulated * 16 + 14) remaining

                'f' ->
                    hexCharsToInt (accumulated * 16 + 15) remaining

                'F' ->
                    hexCharsToInt (accumulated * 16 + 15) remaining

                _ ->
                    Nothing


locationString : Location -> String
locationString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column
