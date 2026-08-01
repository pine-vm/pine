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
    case TokensFromString.parseExpression input of
        Ok tokens ->
            parseExpressionTokens tokens

        Err error ->
            Err error


parseExpressionTokens : List Token.Token -> Result String Expression.Expression
parseExpressionTokens tokens =
    case parseExpressionNode tokens of
        Ok ( expressionNode, remaining ) ->
            case dropTrivia remaining of
                [] ->
                    Ok (Node.value expressionNode)

                nextToken :: _ ->
                    Err ("Unexpected token '" ++ nextToken.lexeme ++ "' after parsing expression.")

        Err error ->
            Err error


parseExpressionNode : List Token.Token -> Result String ( Node Expression.Expression, List Token.Token )
parseExpressionNode tokens =
    parseExpressionNodeAt 0 0 tokens


parseExpressionNodeAt :
    Int
    -> Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseExpressionNodeAt indentMin minPrecedence tokens =
    case parseApplication indentMin tokens of
        Ok parsedApplication ->
            parseOperators indentMin minPrecedence parsedApplication

        Err error ->
            Err error


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
                        case parseExpressionNodeAt indentMin nextMinPrecedence afterOperator of
                            Ok ( right, remaining ) ->
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

                            Err error ->
                                Err error

                Nothing ->
                    Ok ( left, tokens )

        [] ->
            Ok ( left, [] )


parseApplication :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseApplication indentMin tokens =
    case parseBasicExpression indentMin tokens of
        Ok ( function, remaining ) ->
            parseApplicationArguments indentMin function [] remaining

        Err error ->
            Err error


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
                case parseBasicExpression indentMin tokens of
                    Ok ( argument, remaining ) ->
                        parseApplicationArguments indentMin function (argument :: argumentsRev) remaining

                    Err error ->
                        Err error

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
    case parseAtomicExpression indentMin tokens of
        Ok parsedAtomicExpression ->
            parseRecordAccesses indentMin parsedAtomicExpression

        Err error ->
            Err error


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
                    case token.rawText of
                        Just rawText ->
                            Ok
                                ( Node (tokenRange token)
                                    (Expression.MultilineStringLiteral token.lexeme
                                        (Just (String.split "\n" rawText))
                                    )
                                , rest
                                )

                        Nothing ->
                            Ok
                                ( Node (tokenRange token)
                                    (Expression.MultilineStringLiteral token.lexeme Nothing)
                                , rest
                                )

                Token.CharLiteral ->
                    case String.toList token.lexeme of
                        [ char ] ->
                            Ok ( Node (tokenRange token) (Expression.CharLiteral (Char.toCode char)), rest )

                        _ ->
                            Err ("Invalid character literal '" ++ token.lexeme ++ "'.")

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
                    case parseBasicExpression indentMin rest of
                        Ok ( negated, remaining ) ->
                            Ok
                                ( Node
                                    { start = token.start, end = (Node.range negated).end }
                                    (Expression.Negation negated)
                                , remaining
                                )

                        Err error ->
                            Err error

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
    case parseSeparatedExpressions indentMin Token.CloseBracket tokens of
        Ok ( elements, closeToken, remaining ) ->
            Ok
                ( Node
                    { start = openToken.start, end = closeToken.end }
                    (Expression.ListExpr elements)
                , remaining
                )

        Err error ->
            Err error


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
    case parseExpressionNodeAt indentMin 0 tokens of
        Ok ( first, afterFirst ) ->
            case parseFurtherSeparatedExpressions indentMin Token.CloseParen afterFirst [] of
                Ok ( further, closeToken, remaining ) ->
                    let
                        expression =
                            case further of
                                [] ->
                                    Expression.Parenthesized first

                                _ ->
                                    Expression.TupledExpression
                                        (SeparatedSyntaxList.NonEmpty first further)
                    in
                    Ok
                        ( Node
                            { start = openToken.start, end = closeToken.end }
                            expression
                        , remaining
                        )

                Err error ->
                    Err error

        Err error ->
            Err error


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
    case parseExpressionNodeAt indentMin 0 tokens of
        Ok ( first, afterFirst ) ->
            case parseFurtherSeparatedExpressions indentMin closingType afterFirst [] of
                Ok ( further, closeToken, remaining ) ->
                    Ok ( SeparatedSyntaxList.NonEmpty first further, closeToken, remaining )

                Err error ->
                    Err error

        Err error ->
            Err error


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
                case parseExpressionNodeAt indentMin 0 rest of
                    Ok ( expression, remaining ) ->
                        parseFurtherSeparatedExpressions indentMin
                            closingType
                            remaining
                            (( token.start, expression ) :: furtherRev)

                    Err error ->
                        Err error

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
    case parseExpressionNodeAt branchIndentMin 0 tokens of
        Ok ( condition, afterCondition ) ->
            case consumeKeyword "then" afterCondition of
                Ok ( thenToken, afterThen ) ->
                    case parseExpressionNodeAt branchIndentMin 0 afterThen of
                        Ok ( thenBranch, afterThenBranch ) ->
                            case consumeKeyword "else" afterThenBranch of
                                Ok ( elseToken, afterElse ) ->
                                    case parseExpressionNodeAt branchIndentMin 0 afterElse of
                                        Ok ( elseBranch, remaining ) ->
                                            Ok
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

                                        Err error ->
                                            Err error

                                Err error ->
                                    Err error

                        Err error ->
                            Err error

                Err error ->
                    Err error

        Err error ->
            Err error


parseLambda :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseLambda indentMin lambdaToken tokens =
    case parseLambdaArguments indentMin tokens [] of
        Ok ( arguments, arrowToken, afterArrow ) ->
            if List.isEmpty arguments then
                Err "Expected at least one argument in lambda expression."

            else
                case parseExpressionNodeAt indentMin 0 afterArrow of
                    Ok ( body, remaining ) ->
                        Ok
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

                    Err error ->
                        Err error

        Err error ->
            Err error


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
                case parsePatternNodeAt indentMin (token :: rest) of
                    Ok ( argument, remaining ) ->
                        parseLambdaArguments indentMin remaining (argument :: argumentsRev)

                    Err error ->
                        Err error


parseLetBlock :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseLetBlock indentMin letToken tokens =
    case parseLetDeclarations (min indentMin letToken.start.column) tokens [] of
        Ok ( declarations, inToken, afterIn ) ->
            if List.isEmpty declarations then
                Err "Expected at least one declaration in let expression."

            else
                case parseExpressionNodeAt indentMin 0 afterIn of
                    Ok ( body, remaining ) ->
                        Ok
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

                    Err error ->
                        Err error

        Err error ->
            Err error


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
                case parseLetDeclaration token.start.column (token :: rest) of
                    Ok ( declaration, remaining ) ->
                        parseLetDeclarations indentMin remaining (declaration :: declarationsRev)

                    Err error ->
                        Err error


parseLetDeclaration :
    Int
    -> List Token.Token
    -> Result String ( Node Expression.LetDeclaration, List Token.Token )
parseLetDeclaration declarationIndent tokens =
    case dropTrivia tokens of
        nameToken :: rest ->
            if nameToken.tokenType == Token.Identifier then
                case parsePatternsUntilEqual declarationIndent rest [] of
                    Ok ( arguments, equalToken, afterEqual ) ->
                        case parseExpressionNodeAt declarationIndent 0 afterEqual of
                            Ok ( body, remaining ) ->
                                let
                                    declarationRange =
                                        { start = nameToken.start, end = (Node.range body).end }
                                in
                                Ok
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

                            Err error ->
                                Err error

                    Err error ->
                        Err error

            else
                case parsePatternNodeAt declarationIndent (nameToken :: rest) of
                    Ok ( pattern, afterPattern ) ->
                        case consumeToken Token.Equal "'='" afterPattern of
                            Ok ( equalToken, afterEqual ) ->
                                case parseExpressionNodeAt declarationIndent 0 afterEqual of
                                    Ok ( body, remaining ) ->
                                        Ok
                                            ( Node
                                                { start = (Node.range pattern).start
                                                , end = (Node.range body).end
                                                }
                                                (Expression.LetDestructuring pattern equalToken.start body)
                                            , remaining
                                            )

                                    Err error ->
                                        Err error

                            Err error ->
                                Err error

                    Err error ->
                        Err error

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
                case parsePatternNodeAt indentMin (token :: rest) of
                    Ok ( pattern, remaining ) ->
                        parsePatternsUntilEqual indentMin remaining (pattern :: patternsRev)

                    Err error ->
                        Err error


parseCaseBlock :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node Expression.Expression, List Token.Token )
parseCaseBlock indentMin caseToken tokens =
    case parseExpressionNodeAt caseToken.start.column 0 tokens of
        Ok ( subject, afterSubject ) ->
            case consumeKeyword "of" afterSubject of
                Ok ( ofToken, afterOf ) ->
                    case dropTrivia afterOf of
                        firstBranchToken :: _ ->
                            case
                                parseCaseBranches
                                    (min firstBranchToken.start.column (indentMin + 1))
                                    firstBranchToken.start.column
                                    afterOf
                                    []
                            of
                                Ok ( branches, remaining ) ->
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

                                Err error ->
                                    Err error

                        [] ->
                            Err "Expected at least one case branch after 'of'."

                Err error ->
                    Err error

        Err error ->
            Err error


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
                case parseCaseBranch branchIndent tokens of
                    Ok ( branch, remaining ) ->
                        parseCaseBranches lowerBound branchIndent remaining (branch :: branchesRev)

                    Err error ->
                        Err error


parseCaseBranch :
    Int
    -> List Token.Token
    -> Result String ( Expression.Case, List Token.Token )
parseCaseBranch branchIndent tokens =
    case parsePatternNodeAt branchIndent tokens of
        Ok ( pattern, afterPattern ) ->
            case consumeToken Token.Arrow "'->'" afterPattern of
                Ok ( arrowToken, afterArrow ) ->
                    case parseExpressionNodeAt branchIndent 0 afterArrow of
                        Ok ( body, remaining ) ->
                            Ok
                                ( { pattern = pattern
                                  , arrowLocation = arrowToken.start
                                  , expression = body
                                  }
                                , remaining
                                )

                        Err error ->
                            Err error

                Err error ->
                    Err error

        Err error ->
            Err error


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
                            case parseRecordFields indentMin afterPipe of
                                Ok ( fields, closeToken, remaining ) ->
                                    Ok
                                        ( Node
                                            { start = openToken.start, end = closeToken.end }
                                            (Expression.RecordUpdateExpression
                                                (Node (tokenRange nameToken) nameToken.lexeme)
                                                pipeToken.start
                                                fields
                                            )
                                        , remaining
                                        )

                                Err error ->
                                    Err error

                        else
                            case parseRecordFieldsWithFirst indentMin nameToken afterName of
                                Ok ( fields, closeToken, remaining ) ->
                                    Ok
                                        ( Node
                                            { start = openToken.start, end = closeToken.end }
                                            (Expression.RecordExpr fields)
                                        , remaining
                                        )

                                Err error ->
                                    Err error

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
    case parseRecordField indentMin fieldName tokens of
        Ok ( firstField, remaining ) ->
            case parseFurtherRecordFields indentMin remaining [] of
                Ok ( furtherFields, closeToken, afterClose ) ->
                    Ok
                        ( SeparatedSyntaxList.NonEmpty firstField furtherFields
                        , closeToken
                        , afterClose
                        )

                Err error ->
                    Err error

        Err error ->
            Err error


parseRecordField :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Expression.RecordExprField, List Token.Token )
parseRecordField indentMin fieldName tokens =
    if fieldName.tokenType /= Token.Identifier then
        Err ("Expected a record field name, but found '" ++ fieldName.lexeme ++ "'.")

    else
        case consumeToken Token.Equal "'='" tokens of
            Ok ( equalToken, afterEqual ) ->
                case parseExpressionNodeAt indentMin 0 afterEqual of
                    Ok ( valueExpression, remaining ) ->
                        Ok
                            ( { fieldName = Node (tokenRange fieldName) fieldName.lexeme
                              , equalsLocation = equalToken.start
                              , valueExpr = valueExpression
                              }
                            , remaining
                            )

                    Err error ->
                        Err error

            Err error ->
                Err error


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
                        case parseRecordField indentMin fieldName afterName of
                            Ok ( field, remaining ) ->
                                parseFurtherRecordFields indentMin
                                    remaining
                                    (( token.start, field ) :: fieldsRev)

                            Err error ->
                                Err error

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
    case parsePatternAtomic indentMin tokens of
        Ok ( pattern, remaining ) ->
            case parseNamedPatternArguments indentMin pattern remaining of
                Ok parsedNamedPattern ->
                    parsePatternSuffix indentMin parsedNamedPattern

                Err error ->
                    Err error

        Err error ->
            Err error


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
                case parsePatternAtomic indentMin tokens of
                    Ok ( argument, remaining ) ->
                        parsePatternArguments indentMin name original (argument :: argumentsRev) remaining

                    Err error ->
                        Err error

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
                case parsePatternNodeAt indentMin rest of
                    Ok ( tailPattern, remaining ) ->
                        Ok
                            ( Node
                                { start = (Node.range pattern).start
                                , end = (Node.range tailPattern).end
                                }
                                (Pattern.UnConsPattern pattern token.start tailPattern)
                            , remaining
                            )

                    Err error ->
                        Err error

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
                    case String.toList token.lexeme of
                        [ char ] ->
                            Ok ( Node (tokenRange token) (Pattern.CharPattern (Char.toCode char)), rest )

                        _ ->
                            Err ("Invalid character pattern '" ++ token.lexeme ++ "'.")

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
    case parsePatternNodeAt indentMin tokens of
        Ok ( first, afterFirst ) ->
            case parseFurtherPatterns indentMin Token.CloseParen afterFirst [] of
                Ok ( further, closeToken, remaining ) ->
                    let
                        pattern =
                            case further of
                                [] ->
                                    Pattern.ParenthesizedPattern first

                                _ ->
                                    Pattern.TuplePattern (SeparatedSyntaxList.NonEmpty first further)
                    in
                    Ok
                        ( Node { start = openToken.start, end = closeToken.end } pattern
                        , remaining
                        )

                Err error ->
                    Err error

        Err error ->
            Err error


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
                case parsePatternNodeAt indentMin tokens of
                    Ok ( first, afterFirst ) ->
                        case parseFurtherPatterns indentMin Token.CloseBracket afterFirst [] of
                            Ok ( further, closing, remaining ) ->
                                Ok
                                    ( Node
                                        { start = openToken.start, end = closing.end }
                                        (Pattern.ListPattern (SeparatedSyntaxList.NonEmpty first further))
                                    , remaining
                                    )

                            Err error ->
                                Err error

                    Err error ->
                        Err error

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
                case parsePatternNodeAt indentMin rest of
                    Ok ( pattern, remaining ) ->
                        parseFurtherPatterns indentMin
                            closingType
                            remaining
                            (( token.start, pattern ) :: furtherRev)

                    Err error ->
                        Err error

            else
                Err ("Expected ',' or a closing delimiter in pattern, but found '" ++ token.lexeme ++ "'.")

        [] ->
            Err "Expected a closing delimiter in pattern."


parseRecordPattern :
    Token.Token
    -> List Token.Token
    -> Result String ( Node Pattern.Pattern, List Token.Token )
parseRecordPattern openToken tokens =
    case parseRecordPatternFields tokens Nothing [] of
        Ok ( fields, closeToken, remaining ) ->
            Ok
                ( Node
                    { start = openToken.start, end = closeToken.end }
                    (Pattern.RecordPattern fields)
                , remaining
                )

        Err error ->
            Err error


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
        && (case token.tokenType of
                Token.StringLiteral ->
                    True

                Token.TripleQuotedStringLiteral ->
                    True

                Token.CharLiteral ->
                    True

                Token.NumberLiteral ->
                    True

                Token.Identifier ->
                    True

                Token.OpenParen ->
                    True

                Token.OpenBrace ->
                    True

                Token.OpenBracket ->
                    True

                Token.Negation ->
                    True

                Token.Dot ->
                    True

                _ ->
                    False
           )


canStartNamedPatternArgument : Token.Token -> Bool
canStartNamedPatternArgument token =
    canStartPattern token
        && not
            (token.tokenType
                == Token.Identifier
                && (case token.lexeme of
                        "as" ->
                            True

                        "of" ->
                            True

                        "then" ->
                            True

                        "else" ->
                            True

                        "in" ->
                            True

                        "let" ->
                            True

                        _ ->
                            False
                   )
            )


canStartPattern : Token.Token -> Bool
canStartPattern token =
    case token.tokenType of
        Token.StringLiteral ->
            True

        Token.TripleQuotedStringLiteral ->
            True

        Token.CharLiteral ->
            True

        Token.NumberLiteral ->
            True

        Token.Identifier ->
            True

        Token.OpenParen ->
            True

        Token.OpenBrace ->
            True

        Token.OpenBracket ->
            True

        _ ->
            False


isKeyword : Token.Token -> Bool
isKeyword token =
    token.tokenType
        == Token.Identifier
        && (case token.lexeme of
                "if" ->
                    True

                "then" ->
                    True

                "else" ->
                    True

                "let" ->
                    True

                "in" ->
                    True

                "case" ->
                    True

                "of" ->
                    True

                _ ->
                    False
           )


isClosingToken : Token.Token -> Bool
isClosingToken token =
    case token.tokenType of
        Token.Comma ->
            True

        Token.CloseParen ->
            True

        Token.CloseBracket ->
            True

        Token.CloseBrace ->
            True

        _ ->
            False


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
    case String.slice 0 1 name of
        "A" ->
            True

        "B" ->
            True

        "C" ->
            True

        "D" ->
            True

        "E" ->
            True

        "F" ->
            True

        "G" ->
            True

        "H" ->
            True

        "I" ->
            True

        "J" ->
            True

        "K" ->
            True

        "L" ->
            True

        "M" ->
            True

        "N" ->
            True

        "O" ->
            True

        "P" ->
            True

        "Q" ->
            True

        "R" ->
            True

        "S" ->
            True

        "T" ->
            True

        "U" ->
            True

        "V" ->
            True

        "W" ->
            True

        "X" ->
            True

        "Y" ->
            True

        "Z" ->
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


{-| Delegates to `TokensFromString.hexStringToInt`, which parses a string of ASCII hex digits
using specialized first-order recursion. Sharing this implementation keeps hexadecimal
integer/pattern literals here and `\u{...}` escapes in the tokenizer consistent.
-}
hexStringToInt : String -> Maybe Int
hexStringToInt string =
    TokensFromString.hexStringToInt string


locationString : Location -> String
locationString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column
