module ElmSyntax.Concrete.Parser.FromString exposing (..)

import Char
import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Exposing as Exposing
import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.File as File
import ElmSyntax.Concrete.Import as Import
import ElmSyntax.Concrete.Infix as Infix
import ElmSyntax.Concrete.Module as Module
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.DeclarationOrExpression as DeclarationOrExpression exposing (DeclarationOrExpression)
import ElmSyntax.Concrete.Parser.Token as Token
import ElmSyntax.Concrete.Parser.TokensFromString as TokensFromString
import ElmSyntax.Concrete.Pattern as Pattern
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
import ElmSyntax.Concrete.TypeAnnotation as TypeAnnotation


parseExpression : String -> Result String Expression.Expression
parseExpression input =
    case TokensFromString.parseExpression input of
        Ok tokens ->
            parseExpressionTokens tokens

        Err error ->
            Err error


parseFile : String -> Result String File.File
parseFile input =
    case TokensFromString.parseFile input of
        Ok tokens ->
            parseFileTokens tokens

        Err error ->
            Err error


parseFileTokens : List Token.Token -> Result String File.File
parseFileTokens tokens =
    case parseModuleTokens tokens of
        Err error ->
            Err error

        Ok ( moduleDefinition, afterModule ) ->
            case parseImports [] afterModule of
                Err error ->
                    Err error

                Ok ( imports, afterImports ) ->
                    let
                        previousRangeEndRow =
                            imports
                                |> List.reverse
                                |> List.head
                                |> Maybe.map (Node.range >> .end >> .row)
                    in
                    case parseFileDeclarations [] [] previousRangeEndRow afterImports of
                        Err error ->
                            Err error

                        Ok ( declarations, documentationComments ) ->
                            let
                                isDocumentationComment token =
                                    List.any
                                        (\documentation ->
                                            tokenRange token == Node.range documentation
                                        )
                                        documentationComments

                                comments =
                                    tokens
                                        |> List.filter
                                            (\token ->
                                                token.tokenType == Token.Comment
                                                    && not (isDocumentationComment token)
                                            )
                                        |> List.map
                                            (\token ->
                                                Node (tokenRange token) token.lexeme
                                            )
                            in
                            Ok
                                { moduleDefinition = moduleDefinition
                                , imports = imports
                                , declarations = declarations
                                , comments = comments
                                , incompleteDeclarations = []
                                }


parseImports :
    List (Node Import.Import)
    -> List Token.Token
    -> Result String ( List (Node Import.Import), List Token.Token )
parseImports importsRev tokens =
    case dropTrivia tokens of
        next :: _ ->
            if next.tokenType == Token.Identifier && next.lexeme == "import" then
                case parseImportTokens tokens of
                    Ok ( importNode, remaining ) ->
                        parseImports (importNode :: importsRev) remaining

                    Err error ->
                        Err error

            else
                Ok ( List.reverse importsRev, tokens )

        [] ->
            Ok ( List.reverse importsRev, [] )


parseFileDeclarations :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Maybe Int
    -> List Token.Token
    -> Result String ( List (Node Declaration.Declaration), List (Node String) )
parseFileDeclarations declarationsRev documentationCommentsRev previousRangeEndRow tokens =
    case dropTrivia tokens of
        [] ->
            Ok ( List.reverse declarationsRev, List.reverse documentationCommentsRev )

        firstToken :: _ ->
            if firstToken.start.column /= 1 then
                Err
                    ("Unexpected token '"
                        ++ firstToken.lexeme
                        ++ "' after parsing "
                        ++ String.fromInt (List.length declarationsRev)
                        ++ " declarations."
                    )

            else
                case parseDeclarationTokens tokens of
                    Err error ->
                        Err error

                    Ok ( declaration, remaining ) ->
                        let
                            documentation =
                                documentationCommentBefore
                                    previousRangeEndRow
                                    firstToken.start.row
                                    tokens

                            declarationWithDocumentation =
                                case documentation of
                                    Nothing ->
                                        declaration

                                    Just documentationNode ->
                                        setDeclarationDocumentation documentationNode declaration

                            nextDocumentationCommentsRev =
                                case documentation of
                                    Nothing ->
                                        documentationCommentsRev

                                    Just documentationNode ->
                                        documentationNode :: documentationCommentsRev

                            declarationNode =
                                Node (rangeOfDeclaration declarationWithDocumentation) declarationWithDocumentation
                        in
                        parseFileDeclarations
                            (declarationNode :: declarationsRev)
                            nextDocumentationCommentsRev
                            (Just (Node.range declarationNode).end.row)
                            remaining


documentationCommentBefore : Maybe Int -> Int -> List Token.Token -> Maybe (Node String)
documentationCommentBefore previousRangeEndRow declarationRow tokens =
    let
        leadingComments =
            tokens
                |> takeLeadingTrivia
                |> List.filter (\token -> token.tokenType == Token.Comment)

        canAttach token =
            let
                hasInterveningComment =
                    List.any
                        (\other ->
                            not (String.startsWith "{-|" other.lexeme)
                                && other.start.row
                                > token.end.row
                        )
                        leadingComments

                isAfterPreviousRange =
                    case previousRangeEndRow of
                        Nothing ->
                            token.end.row + 1 == declarationRow

                        Just previousEndRow ->
                            previousEndRow < token.end.row
            in
            String.startsWith "{-|" token.lexeme
                && token.end.row
                < declarationRow
                && isAfterPreviousRange
                && not hasInterveningComment
    in
    leadingComments
        |> List.filter canAttach
        |> List.reverse
        |> List.head
        |> Maybe.map (\token -> Node (tokenRange token) token.lexeme)


takeLeadingTrivia : List Token.Token -> List Token.Token
takeLeadingTrivia tokens =
    case tokens of
        token :: rest ->
            if isTrivia token then
                token :: takeLeadingTrivia rest

            else
                []

        [] ->
            []


setDeclarationDocumentation : Node String -> Declaration.Declaration -> Declaration.Declaration
setDeclarationDocumentation documentation declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            Declaration.FunctionDeclaration
                { function | documentation = Just documentation }

        Declaration.ChoiceTypeDeclaration choiceType ->
            Declaration.ChoiceTypeDeclaration
                { choiceType | documentation = Just documentation }

        Declaration.AliasDeclaration typeAlias ->
            Declaration.AliasDeclaration
                { typeAlias | documentation = Just documentation }

        Declaration.PortDeclaration _ _ ->
            declaration

        Declaration.InfixDeclaration _ ->
            declaration


rangeOfDeclaration : Declaration.Declaration -> Range
rangeOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            { start =
                function.signature
                    |> Maybe.map (Node.range >> .start)
                    |> Maybe.withDefault (Node.range function.declaration).start
            , end = (Node.range function.declaration).end
            }

        Declaration.ChoiceTypeDeclaration choiceType ->
            { start = choiceType.typeTokenLocation
            , end =
                case choiceType.constructors of
                    SeparatedSyntaxList.NonEmpty first rest ->
                        case List.reverse rest of
                            ( _, last ) :: _ ->
                                (Node.range last).end

                            [] ->
                                (Node.range first).end

                    SeparatedSyntaxList.Empty ->
                        (Node.range choiceType.name).end
            }

        Declaration.AliasDeclaration typeAlias ->
            { start = typeAlias.typeTokenLocation
            , end = (Node.range typeAlias.typeAnnotation).end
            }

        Declaration.PortDeclaration portTokenLocation signature ->
            { start = portTokenLocation
            , end = (Node.range signature.typeAnnotation).end
            }

        Declaration.InfixDeclaration infix ->
            { start = infix.infixTokenLocation
            , end = (Node.range infix.function).end
            }


parseModuleTokens : List Token.Token -> Result String ( Node Module.Module, List Token.Token )
parseModuleTokens tokens =
    case dropTrivia tokens of
        firstToken :: _ ->
            if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "effect" then
                parseEffectModule tokens

            else if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "port" then
                parseDefaultModule Module.PortModule "port" tokens

            else
                parseDefaultModule Module.NormalModule "module" tokens

        [] ->
            Err "Expected a module declaration."


parseDefaultModule :
    (Module.DefaultModuleData -> Module.Module)
    -> String
    -> List Token.Token
    -> Result String ( Node Module.Module, List Token.Token )
parseDefaultModule moduleConstructor firstKeyword tokens =
    case consumeKeyword firstKeyword tokens of
        Err error ->
            Err error

        Ok ( firstToken, afterFirstKeyword ) ->
            let
                consumeModuleKeyword =
                    if firstKeyword == "module" then
                        Ok afterFirstKeyword

                    else
                        consumeKeyword "module" afterFirstKeyword
                            |> Result.map Tuple.second
            in
            case consumeModuleKeyword of
                Err error ->
                    Err error

                Ok afterModuleKeyword ->
                    case parseModuleName afterModuleKeyword of
                        Err error ->
                            Err error

                        Ok ( moduleName, afterModuleName ) ->
                            case parseExposingTokens afterModuleName of
                                Err error ->
                                    Err error

                                Ok ( exposingList, remaining ) ->
                                    Ok
                                        ( Node
                                            { start = firstToken.start
                                            , end = (Node.range exposingList).end
                                            }
                                            (moduleConstructor
                                                { moduleName = moduleName
                                                , exposingList = exposingList
                                                }
                                            )
                                        , remaining
                                        )


parseEffectModule : List Token.Token -> Result String ( Node Module.Module, List Token.Token )
parseEffectModule tokens =
    case consumeKeyword "effect" tokens of
        Err error ->
            Err error

        Ok ( effectToken, afterEffect ) ->
            case consumeKeyword "module" afterEffect of
                Err error ->
                    Err error

                Ok ( _, afterModule ) ->
                    case parseModuleName afterModule of
                        Err error ->
                            Err error

                        Ok ( moduleName, afterModuleName ) ->
                            case parseEffectWhere afterModuleName of
                                Err error ->
                                    Err error

                                Ok ( command, subscription, afterWhere ) ->
                                    case parseExposingTokens afterWhere of
                                        Err error ->
                                            Err error

                                        Ok ( exposingList, remaining ) ->
                                            Ok
                                                ( Node
                                                    { start = effectToken.start
                                                    , end = (Node.range exposingList).end
                                                    }
                                                    (Module.EffectModule
                                                        { moduleName = moduleName
                                                        , exposingList = exposingList
                                                        , command = command
                                                        , subscription = subscription
                                                        }
                                                    )
                                                , remaining
                                                )


parseEffectWhere :
    List Token.Token
    -> Result String ( Maybe (Node String), Maybe (Node String), List Token.Token )
parseEffectWhere tokens =
    case dropTrivia tokens of
        whereToken :: _ ->
            if whereToken.tokenType == Token.Identifier && whereToken.lexeme == "where" then
                case consumeKeyword "where" tokens of
                    Err error ->
                        Err error

                    Ok ( _, afterWhere ) ->
                        case consumeToken Token.OpenBrace "'{'" (dropTrivia afterWhere) of
                            Err error ->
                                Err error

                            Ok ( _, afterOpenBrace ) ->
                                parseEffectWhereFields Nothing Nothing afterOpenBrace

            else
                Ok ( Nothing, Nothing, tokens )

        [] ->
            Ok ( Nothing, Nothing, [] )


parseEffectWhereFields :
    Maybe (Node String)
    -> Maybe (Node String)
    -> List Token.Token
    -> Result String ( Maybe (Node String), Maybe (Node String), List Token.Token )
parseEffectWhereFields command subscription tokens =
    case dropTrivia tokens of
        closeBrace :: rest ->
            if closeBrace.tokenType == Token.CloseBrace then
                Ok ( command, subscription, rest )

            else if closeBrace.tokenType == Token.Identifier then
                case consumeToken Token.Equal "'='" (dropTrivia rest) of
                    Err error ->
                        Err error

                    Ok ( _, afterEqual ) ->
                        case parseModuleName afterEqual of
                            Err error ->
                                Err error

                            Ok ( valueNode, afterValue ) ->
                                let
                                    valueName =
                                        case List.reverse (Node.value valueNode) of
                                            name :: _ ->
                                                name

                                            [] ->
                                                ""

                                    value =
                                        Node (Node.range valueNode) valueName

                                    nextCommand =
                                        if closeBrace.lexeme == "command" then
                                            Just value

                                        else
                                            command

                                    nextSubscription =
                                        if closeBrace.lexeme == "subscription" then
                                            Just value

                                        else
                                            subscription
                                in
                                case dropTrivia afterValue of
                                    comma :: afterComma ->
                                        if comma.tokenType == Token.Comma then
                                            parseEffectWhereFields nextCommand nextSubscription afterComma

                                        else
                                            parseEffectWhereFields nextCommand nextSubscription afterValue

                                    [] ->
                                        Err "Expected '}' after effect module fields."

            else
                Err ("Expected an effect module field or '}', but found '" ++ closeBrace.lexeme ++ "'.")

        [] ->
            Err "Expected '}' after effect module fields."


parseImportTokens : List Token.Token -> Result String ( Node Import.Import, List Token.Token )
parseImportTokens tokens =
    case consumeKeyword "import" tokens of
        Err error ->
            Err error

        Ok ( importToken, afterImport ) ->
            case parseModuleName afterImport of
                Err error ->
                    Err error

                Ok ( moduleName, afterModuleName ) ->
                    case parseImportAlias afterModuleName of
                        Err error ->
                            Err error

                        Ok ( moduleAlias, afterAlias ) ->
                            case parseOptionalExposing afterAlias of
                                Err error ->
                                    Err error

                                Ok ( exposingList, remaining ) ->
                                    let
                                        importEnd =
                                            case exposingList of
                                                Just ( _, exposingNode ) ->
                                                    (Node.range exposingNode).end

                                                Nothing ->
                                                    case moduleAlias of
                                                        Just ( _, aliasNode ) ->
                                                            (Node.range aliasNode).end

                                                        Nothing ->
                                                            (Node.range moduleName).end
                                    in
                                    Ok
                                        ( Node
                                            { start = importToken.start, end = importEnd }
                                            { importTokenLocation = importToken.start
                                            , moduleName = moduleName
                                            , moduleAlias = moduleAlias
                                            , exposingList = exposingList
                                            }
                                        , remaining
                                        )


parseImportAlias :
    List Token.Token
    -> Result String ( Maybe ( Location, Node Module.ModuleName ), List Token.Token )
parseImportAlias tokens =
    case dropTrivia tokens of
        asToken :: _ ->
            if asToken.tokenType == Token.Identifier && asToken.lexeme == "as" then
                case consumeKeyword "as" tokens of
                    Err error ->
                        Err error

                    Ok ( consumedAs, afterAs ) ->
                        case dropTrivia afterAs of
                            aliasToken :: rest ->
                                if aliasToken.tokenType == Token.Identifier then
                                    Ok
                                        ( Just
                                            ( consumedAs.start
                                            , Node (tokenRange aliasToken) [ aliasToken.lexeme ]
                                            )
                                        , rest
                                        )

                                else
                                    Err ("Expected module alias, but found '" ++ aliasToken.lexeme ++ "'.")

                            [] ->
                                Err "Expected module alias."

            else
                Ok ( Nothing, tokens )

        [] ->
            Ok ( Nothing, [] )


parseOptionalExposing :
    List Token.Token
    -> Result String ( Maybe ( Location, Node Exposing.Exposing ), List Token.Token )
parseOptionalExposing tokens =
    case dropTrivia tokens of
        exposingToken :: _ ->
            if exposingToken.tokenType == Token.Identifier && exposingToken.lexeme == "exposing" then
                parseExposingTokens tokens
                    |> Result.map
                        (\( exposingNode, remaining ) ->
                            ( Just ( exposingToken.start, exposingNode ), remaining )
                        )

            else
                Ok ( Nothing, tokens )

        [] ->
            Ok ( Nothing, [] )


parseModuleName :
    List Token.Token
    -> Result String ( Node Module.ModuleName, List Token.Token )
parseModuleName tokens =
    case dropTrivia tokens of
        firstToken :: rest ->
            if firstToken.tokenType == Token.Identifier then
                parseModuleNameRest
                    firstToken.start
                    firstToken.end
                    [ firstToken.lexeme ]
                    rest

            else
                Err ("Expected module name, but found '" ++ firstToken.lexeme ++ "'.")

        [] ->
            Err "Expected module name."


parseModuleNameRest :
    Location
    -> Location
    -> List String
    -> List Token.Token
    -> Result String ( Node Module.ModuleName, List Token.Token )
parseModuleNameRest start end partsRev tokens =
    case tokens of
        dotToken :: nameToken :: rest ->
            if dotToken.tokenType == Token.Dot && nameToken.tokenType == Token.Identifier then
                parseModuleNameRest start nameToken.end (nameToken.lexeme :: partsRev) rest

            else if dotToken.tokenType == Token.Dot then
                Err ("Expected module name part, but found '" ++ nameToken.lexeme ++ "'.")

            else
                Ok ( Node { start = start, end = end } (List.reverse partsRev), tokens )

        dotToken :: [] ->
            if dotToken.tokenType == Token.Dot then
                Err "Expected module name part."

            else
                Ok ( Node { start = start, end = end } (List.reverse partsRev), tokens )

        [] ->
            Ok ( Node { start = start, end = end } (List.reverse partsRev), [] )


parseExposingTokens :
    List Token.Token
    -> Result String ( Node Exposing.Exposing, List Token.Token )
parseExposingTokens tokens =
    case consumeKeyword "exposing" tokens of
        Err error ->
            Err error

        Ok ( exposingToken, afterExposing ) ->
            case consumeToken Token.OpenParen "'('" (dropTrivia afterExposing) of
                Err error ->
                    Err error

                Ok ( openParen, afterOpenParen ) ->
                    case dropTrivia afterOpenParen of
                        dotDotToken :: afterDotDot ->
                            if dotDotToken.tokenType == Token.DotDot then
                                case consumeToken Token.CloseParen "')'" (dropTrivia afterDotDot) of
                                    Err error ->
                                        Err error

                                    Ok ( closeParen, remaining ) ->
                                        Ok
                                            ( Node
                                                { start = exposingToken.start, end = closeParen.end }
                                                (Exposing.All (tokenRange dotDotToken))
                                            , remaining
                                            )

                            else
                                parseExplicitExposing
                                    exposingToken
                                    openParen
                                    Nothing
                                    []
                                    afterOpenParen

                        [] ->
                            Err "Expected ')' to close exposing list."


parseExplicitExposing :
    Token.Token
    -> Token.Token
    -> Maybe (Node Exposing.TopLevelExpose)
    -> List ( Location, Node Exposing.TopLevelExpose )
    -> List Token.Token
    -> Result String ( Node Exposing.Exposing, List Token.Token )
parseExplicitExposing exposingToken openParen first restRev tokens =
    case dropTrivia tokens of
        token :: remaining ->
            if token.tokenType == Token.CloseParen then
                let
                    nodes =
                        case first of
                            Nothing ->
                                SeparatedSyntaxList.Empty

                            Just firstNode ->
                                SeparatedSyntaxList.NonEmpty firstNode (List.reverse restRev)
                in
                Ok
                    ( Node
                        { start = exposingToken.start, end = token.end }
                        (Exposing.Explicit openParen.start nodes token.start)
                    , remaining
                    )

            else
                case first of
                    Nothing ->
                        case parseTopLevelExpose tokens of
                            Err error ->
                                Err error

                            Ok ( exposeNode, afterExpose ) ->
                                parseExplicitExposing
                                    exposingToken
                                    openParen
                                    (Just exposeNode)
                                    restRev
                                    afterExpose

                    Just _ ->
                        if token.tokenType == Token.Comma then
                            case parseTopLevelExpose remaining of
                                Err error ->
                                    Err error

                                Ok ( exposeNode, afterExpose ) ->
                                    parseExplicitExposing
                                        exposingToken
                                        openParen
                                        first
                                        (( token.start, exposeNode ) :: restRev)
                                        afterExpose

                        else
                            Err "Expected ',' before exposing list item."

        [] ->
            Err "Expected ')' to close exposing list."


parseTopLevelExpose :
    List Token.Token
    -> Result String ( Node Exposing.TopLevelExpose, List Token.Token )
parseTopLevelExpose tokens =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.OpenParen then
                case rest of
                    operatorToken :: closeParen :: remaining ->
                        if
                            operatorToken.tokenType
                                == Token.Operator
                                && closeParen.tokenType
                                == Token.CloseParen
                        then
                            Ok
                                ( Node
                                    { start = token.start, end = closeParen.end }
                                    (Exposing.InfixExpose operatorToken.lexeme)
                                , remaining
                                )

                        else
                            Err "Expected an operator followed by ')' in exposing list."

                    _ ->
                        Err "Expected an operator followed by ')' in exposing list."

            else if token.tokenType == Token.Identifier then
                if startsWithUpper token.lexeme then
                    parseUpperExpose token rest

                else
                    Ok
                        ( Node (tokenRange token) (Exposing.FunctionExpose token.lexeme)
                        , rest
                        )

            else
                Err ("Unexpected token '" ++ token.lexeme ++ "' in exposing list.")

        [] ->
            Err "Expected exposing list item."


parseUpperExpose :
    Token.Token
    -> List Token.Token
    -> Result String ( Node Exposing.TopLevelExpose, List Token.Token )
parseUpperExpose nameToken tokens =
    case dropTrivia tokens of
        openParen :: afterOpenParen ->
            if openParen.tokenType == Token.OpenParen then
                case dropTrivia afterOpenParen of
                    dotDot :: afterDotDot ->
                        if dotDot.tokenType == Token.DotDot then
                            case consumeToken Token.CloseParen "')'" (dropTrivia afterDotDot) of
                                Err error ->
                                    Err error

                                Ok ( closeParen, remaining ) ->
                                    Ok
                                        ( Node
                                            { start = nameToken.start, end = closeParen.end }
                                            (Exposing.TypeExpose
                                                { name = nameToken.lexeme
                                                , open =
                                                    Just
                                                        { start = openParen.start
                                                        , end = closeParen.end
                                                        }
                                                }
                                            )
                                        , remaining
                                        )

                        else if dotDot.tokenType == Token.CloseParen then
                            Ok
                                ( Node
                                    { start = nameToken.start, end = dotDot.end }
                                    (Exposing.TypeExpose
                                        { name = nameToken.lexeme, open = Nothing }
                                    )
                                , afterDotDot
                                )

                        else
                            Err "Expected '..' or ')' after exposed type name."

                    [] ->
                        Err "Expected ')' after exposed type name."

            else
                Ok
                    ( Node
                        (tokenRange nameToken)
                        (Exposing.TypeOrAliasExpose nameToken.lexeme)
                    , tokens
                    )

        [] ->
            Ok
                ( Node
                    (tokenRange nameToken)
                    (Exposing.TypeOrAliasExpose nameToken.lexeme)
                , []
                )


parseDeclarationOrExpression : String -> Result String DeclarationOrExpression
parseDeclarationOrExpression input =
    case TokensFromString.parseExpression input of
        Ok tokens ->
            parseDeclarationOrExpressionTokens tokens

        Err error ->
            Err error


parseDeclarationOrExpressionTokens : List Token.Token -> Result String DeclarationOrExpression
parseDeclarationOrExpressionTokens tokens =
    case dropTrivia tokens of
        [] ->
            Err "No tokens to parse as a declaration or expression."

        firstToken :: _ ->
            let
                startsWithDeclarationKeyword =
                    firstToken.tokenType
                        == Token.Identifier
                        && (firstToken.lexeme
                                == "type"
                                || firstToken.lexeme
                                == "port"
                                || firstToken.lexeme
                                == "infix"
                           )
            in
            if startsWithDeclarationKeyword then
                case parseDeclarationTokens tokens of
                    Ok ( declaration, remaining ) ->
                        case dropTrivia remaining of
                            [] ->
                                Ok (DeclarationOrExpression.Declaration declaration)

                            nextToken :: _ ->
                                Err
                                    ("Unexpected token '"
                                        ++ nextToken.lexeme
                                        ++ "' after parsing declaration."
                                    )

                    Err error ->
                        Err ("Failed to parse declaration or expression: " ++ error)

            else
                case parseDeclarationTokens tokens of
                    Ok ( declaration, remaining ) ->
                        case dropTrivia remaining of
                            [] ->
                                Ok (DeclarationOrExpression.Declaration declaration)

                            _ ->
                                parseAsExpressionFallback tokens

                    Err _ ->
                        parseAsExpressionFallback tokens


parseAsExpressionFallback : List Token.Token -> Result String DeclarationOrExpression
parseAsExpressionFallback tokens =
    case parseExpressionNode tokens of
        Ok ( expressionNode, remaining ) ->
            case dropTrivia remaining of
                [] ->
                    Ok (DeclarationOrExpression.Expression (Node.value expressionNode))

                nextToken :: _ ->
                    Err
                        ("Unexpected token '"
                            ++ nextToken.lexeme
                            ++ "' after parsing expression."
                        )

        Err error ->
            Err ("Failed to parse declaration or expression: " ++ error)


parseDeclarationTokens : List Token.Token -> Result String ( Declaration.Declaration, List Token.Token )
parseDeclarationTokens tokens =
    case dropTrivia tokens of
        firstToken :: _ ->
            if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "infix" then
                parseInfixDeclaration tokens

            else if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "type" then
                parseTypeDeclarationTokens tokens

            else if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "port" then
                parsePortDeclarationTokens tokens

            else
                parseFunctionDeclarationTokens tokens

        [] ->
            Err "Expected a declaration."


parseInfixDeclaration : List Token.Token -> Result String ( Declaration.Declaration, List Token.Token )
parseInfixDeclaration tokens =
    case consumeKeyword "infix" tokens of
        Err e ->
            Err e

        Ok ( infixToken, afterInfix ) ->
            case dropTrivia afterInfix of
                directionToken :: afterDirection ->
                    if directionToken.tokenType /= Token.Identifier then
                        Err
                            ("Expected infix direction, but found '"
                                ++ directionToken.lexeme
                                ++ "'."
                            )

                    else
                        case parseInfixDirection directionToken.lexeme of
                            Nothing ->
                                Err
                                    ("Infix direction is not a valid value: "
                                        ++ directionToken.lexeme
                                    )

                            Just direction ->
                                case dropTrivia afterDirection of
                                    precedenceToken :: afterPrecedence ->
                                        if precedenceToken.tokenType /= Token.NumberLiteral then
                                            Err
                                                ("Expected infix precedence, but found '"
                                                    ++ precedenceToken.lexeme
                                                    ++ "'."
                                                )

                                        else
                                            case String.toInt precedenceToken.lexeme of
                                                Nothing ->
                                                    Err
                                                        ("Infix precedence is not a number: "
                                                            ++ precedenceToken.lexeme
                                                        )

                                                Just precedence ->
                                                    case consumeToken Token.OpenParen "'('" (dropTrivia afterPrecedence) of
                                                        Err e ->
                                                            Err e

                                                        Ok ( openParen, afterOpen ) ->
                                                            case dropTrivia afterOpen of
                                                                operatorToken :: afterOperator ->
                                                                    if operatorToken.tokenType /= Token.Operator then
                                                                        Err
                                                                            ("Expected operator symbol, but found '"
                                                                                ++ operatorToken.lexeme
                                                                                ++ "'."
                                                                            )

                                                                    else
                                                                        case consumeToken Token.CloseParen "')'" (dropTrivia afterOperator) of
                                                                            Err e ->
                                                                                Err e

                                                                            Ok ( closeParen, afterClose ) ->
                                                                                case consumeToken Token.Equal "'='" (dropTrivia afterClose) of
                                                                                    Err e ->
                                                                                        Err e

                                                                                    Ok ( equalToken, afterEqual ) ->
                                                                                        case dropTrivia afterEqual of
                                                                                            funcNameToken :: afterFuncName ->
                                                                                                if funcNameToken.tokenType /= Token.Identifier then
                                                                                                    Err
                                                                                                        ("Expected function name, but found '"
                                                                                                            ++ funcNameToken.lexeme
                                                                                                            ++ "'."
                                                                                                        )

                                                                                                else
                                                                                                    let
                                                                                                        infixValue =
                                                                                                            { infixTokenLocation = infixToken.start
                                                                                                            , direction =
                                                                                                                Node
                                                                                                                    (tokenRange directionToken)
                                                                                                                    direction
                                                                                                            , precedence =
                                                                                                                Node
                                                                                                                    (tokenRange precedenceToken)
                                                                                                                    precedence
                                                                                                            , operator =
                                                                                                                Node
                                                                                                                    { start = openParen.start
                                                                                                                    , end = closeParen.end
                                                                                                                    }
                                                                                                                    operatorToken.lexeme
                                                                                                            , equalsTokenLocation = equalToken.start
                                                                                                            , function =
                                                                                                                Node
                                                                                                                    (tokenRange funcNameToken)
                                                                                                                    funcNameToken.lexeme
                                                                                                            }
                                                                                                    in
                                                                                                    Ok
                                                                                                        ( Declaration.InfixDeclaration infixValue
                                                                                                        , afterFuncName
                                                                                                        )

                                                                                            [] ->
                                                                                                Err "Expected function name after '='."

                                                                _ ->
                                                                    Err "Expected operator symbol after '('."

                                    [] ->
                                        Err "Expected infix precedence."

                [] ->
                    Err "Expected infix direction."


parseInfixDirection : String -> Maybe Infix.InfixDirection
parseInfixDirection s =
    case s of
        "left" ->
            Just Infix.Left

        "right" ->
            Just Infix.Right

        "non" ->
            Just Infix.Non

        _ ->
            Nothing


parseTypeDeclarationTokens : List Token.Token -> Result String ( Declaration.Declaration, List Token.Token )
parseTypeDeclarationTokens tokens =
    case consumeKeyword "type" tokens of
        Err e ->
            Err e

        Ok ( typeToken, afterType ) ->
            let
                remaining =
                    dropTrivia afterType
            in
            case remaining of
                firstToken :: _ ->
                    if firstToken.tokenType == Token.Identifier && firstToken.lexeme == "alias" then
                        parseAliasDeclaration typeToken remaining

                    else
                        parseChoiceTypeDeclaration typeToken remaining

                [] ->
                    Err "Expected type name or 'alias' keyword."


parseAliasDeclaration :
    Token.Token
    -> List Token.Token
    -> Result String ( Declaration.Declaration, List Token.Token )
parseAliasDeclaration typeToken tokens =
    case consumeKeyword "alias" tokens of
        Err e ->
            Err e

        Ok ( aliasToken, afterAlias ) ->
            case dropTrivia afterAlias of
                nameToken :: afterName ->
                    if nameToken.tokenType /= Token.Identifier then
                        Err "Expected type alias name."

                    else
                        let
                            ( generics, afterGenerics ) =
                                collectTypeGenerics afterName []
                        in
                        case consumeToken Token.Equal "'='" afterGenerics of
                            Err e ->
                                Err e

                            Ok ( equalToken, afterEqual ) ->
                                case parseTypeAnnotation 0 (dropTrivia afterEqual) of
                                    Err e ->
                                        Err e

                                    Ok ( typeAnnotationNode, remaining ) ->
                                        let
                                            typeAlias =
                                                { documentation = Nothing
                                                , typeTokenLocation = typeToken.start
                                                , aliasTokenLocation = aliasToken.start
                                                , name =
                                                    Node (tokenRange nameToken) nameToken.lexeme
                                                , generics = generics
                                                , equalsTokenLocation = equalToken.start
                                                , typeAnnotation = typeAnnotationNode
                                                }
                                        in
                                        Ok
                                            ( Declaration.AliasDeclaration typeAlias
                                            , remaining
                                            )

                [] ->
                    Err "Expected type alias name."


parseChoiceTypeDeclaration :
    Token.Token
    -> List Token.Token
    -> Result String ( Declaration.Declaration, List Token.Token )
parseChoiceTypeDeclaration typeToken tokens =
    case dropTrivia tokens of
        nameToken :: afterName ->
            if nameToken.tokenType /= Token.Identifier then
                Err "Expected type name."

            else
                let
                    ( generics, afterGenerics ) =
                        collectTypeGenerics afterName []
                in
                case consumeToken Token.Equal "'='" afterGenerics of
                    Err e ->
                        Err e

                    Ok ( equalToken, afterEqual ) ->
                        case dropTrivia afterEqual of
                            firstConstructorNameToken :: afterFirstConstructorName ->
                                if firstConstructorNameToken.tokenType /= Token.Identifier then
                                    Err
                                        ("Expected constructor name, but found '"
                                            ++ firstConstructorNameToken.lexeme
                                            ++ "'."
                                        )

                                else
                                    case
                                        parseChoiceTypeConstructorArgs
                                            firstConstructorNameToken.start.column
                                            []
                                            afterFirstConstructorName
                                    of
                                        Err e ->
                                            Err e

                                        Ok ( firstArgs, afterFirstArgs ) ->
                                            let
                                                firstConstructorEnd =
                                                    case List.reverse firstArgs of
                                                        lastArg :: _ ->
                                                            (Node.range lastArg).end

                                                        [] ->
                                                            firstConstructorNameToken.end

                                                firstConstructor =
                                                    Node
                                                        { start = firstConstructorNameToken.start
                                                        , end = firstConstructorEnd
                                                        }
                                                        { name =
                                                            Node
                                                                (tokenRange firstConstructorNameToken)
                                                                firstConstructorNameToken.lexeme
                                                        , arguments = firstArgs
                                                        }
                                            in
                                            case parseMoreChoiceConstructors firstConstructor [] afterFirstArgs of
                                                Err e ->
                                                    Err e

                                                Ok ( constructors, remaining ) ->
                                                    let
                                                        lastConstructorEnd =
                                                            case constructors of
                                                                SeparatedSyntaxList.NonEmpty first rest ->
                                                                    case List.reverse rest of
                                                                        ( _, lastNode ) :: _ ->
                                                                            (Node.range lastNode).end

                                                                        [] ->
                                                                            (Node.range first).end

                                                                SeparatedSyntaxList.Empty ->
                                                                    firstConstructorEnd

                                                        choiceStruct =
                                                            { documentation = Nothing
                                                            , typeTokenLocation = typeToken.start
                                                            , name =
                                                                Node (tokenRange nameToken) nameToken.lexeme
                                                            , generics = generics
                                                            , equalsTokenLocation = equalToken.start
                                                            , constructors = constructors
                                                            }
                                                    in
                                                    Ok
                                                        ( Declaration.ChoiceTypeDeclaration choiceStruct
                                                        , remaining
                                                        )

                            [] ->
                                Err "Expected constructor name."

        [] ->
            Err "Expected type name."


parseMoreChoiceConstructors :
    Node Declaration.ValueConstructor
    -> List ( Location, Node Declaration.ValueConstructor )
    -> List Token.Token
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node Declaration.ValueConstructor), List Token.Token )
parseMoreChoiceConstructors firstConstructor restRev tokens =
    case dropTrivia tokens of
        pipeToken :: afterPipe ->
            if pipeToken.tokenType == Token.Pipe then
                case dropTrivia afterPipe of
                    nameToken :: afterName ->
                        if nameToken.tokenType /= Token.Identifier then
                            Err
                                ("Expected constructor name after '|', but found '"
                                    ++ nameToken.lexeme
                                    ++ "'."
                                )

                        else
                            case parseChoiceTypeConstructorArgs nameToken.start.column [] afterName of
                                Err e ->
                                    Err e

                                Ok ( args, remaining ) ->
                                    let
                                        constructorEnd =
                                            case List.reverse args of
                                                lastArg :: _ ->
                                                    (Node.range lastArg).end

                                                [] ->
                                                    nameToken.end

                                        constructorNode =
                                            Node
                                                { start = nameToken.start, end = constructorEnd }
                                                { name =
                                                    Node (tokenRange nameToken) nameToken.lexeme
                                                , arguments = args
                                                }
                                    in
                                    parseMoreChoiceConstructors
                                        firstConstructor
                                        (( pipeToken.start, constructorNode ) :: restRev)
                                        remaining

                    [] ->
                        Err "Expected constructor name after '|'."

            else
                Ok
                    ( SeparatedSyntaxList.NonEmpty firstConstructor (List.reverse restRev)
                    , tokens
                    )

        [] ->
            Ok ( SeparatedSyntaxList.NonEmpty firstConstructor (List.reverse restRev), [] )


parseChoiceTypeConstructorArgs :
    Int
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> List Token.Token
    -> Result String ( List (Node TypeAnnotation.TypeAnnotation), List Token.Token )
parseChoiceTypeConstructorArgs constructorCol argsRev tokens =
    case dropTrivia tokens of
        nextToken :: _ ->
            if nextToken.start.column >= constructorCol && canStartTypeAnnotation nextToken then
                case parseTypeAnnotationTypedArg constructorCol tokens of
                    Err e ->
                        Err e

                    Ok ( arg, remaining ) ->
                        parseChoiceTypeConstructorArgs
                            constructorCol
                            (arg :: argsRev)
                            remaining

            else
                Ok ( List.reverse argsRev, tokens )

        [] ->
            Ok ( List.reverse argsRev, [] )


collectTypeGenerics :
    List Token.Token
    -> List (Node String)
    -> ( List (Node String), List Token.Token )
collectTypeGenerics tokens genericsRev =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.Identifier then
                collectTypeGenerics rest (Node (tokenRange token) token.lexeme :: genericsRev)

            else
                ( List.reverse genericsRev, tokens )

        [] ->
            ( List.reverse genericsRev, [] )


parsePortDeclarationTokens : List Token.Token -> Result String ( Declaration.Declaration, List Token.Token )
parsePortDeclarationTokens tokens =
    case consumeKeyword "port" tokens of
        Err e ->
            Err e

        Ok ( portToken, afterPort ) ->
            case dropTrivia afterPort of
                portNameToken :: afterPortName ->
                    if portNameToken.tokenType /= Token.Identifier then
                        Err "Expected port name."

                    else
                        case consumeToken Token.Colon "':'" (dropTrivia afterPortName) of
                            Err e ->
                                Err e

                            Ok ( colonToken, afterColon ) ->
                                case parseTypeAnnotation 0 (dropTrivia afterColon) of
                                    Err e ->
                                        Err e

                                    Ok ( typeAnnotationNode, remaining ) ->
                                        let
                                            signature =
                                                { name =
                                                    Node (tokenRange portNameToken) portNameToken.lexeme
                                                , colonLocation = colonToken.start
                                                , typeAnnotation = typeAnnotationNode
                                                }
                                        in
                                        Ok
                                            ( Declaration.PortDeclaration portToken.start signature
                                            , remaining
                                            )

                [] ->
                    Err "Expected port name."


parseFunctionDeclarationTokens : List Token.Token -> Result String ( Declaration.Declaration, List Token.Token )
parseFunctionDeclarationTokens tokens =
    case dropTrivia tokens of
        firstNameToken :: afterFirstName ->
            if firstNameToken.tokenType /= Token.Identifier then
                Err
                    ("Expected function name, but found '"
                        ++ firstNameToken.lexeme
                        ++ "'."
                    )

            else
                case dropTrivia afterFirstName of
                    colonToken :: afterColon ->
                        if colonToken.tokenType == Token.Colon then
                            case
                                parseTypeAnnotation
                                    firstNameToken.start.column
                                    (dropTrivia afterColon)
                            of
                                Err e ->
                                    Err e

                                Ok ( sigTypeAnnotation, afterSigAnnotation ) ->
                                    case dropTrivia afterSigAnnotation of
                                        secondNameToken :: afterSecondName ->
                                            if secondNameToken.tokenType /= Token.Identifier then
                                                Err
                                                    ("Expected function name after signature, but found '"
                                                        ++ secondNameToken.lexeme
                                                        ++ "'."
                                                    )

                                            else if secondNameToken.lexeme /= firstNameToken.lexeme then
                                                Err
                                                    ("Function name does not match signature: "
                                                        ++ secondNameToken.lexeme
                                                        ++ " != "
                                                        ++ firstNameToken.lexeme
                                                    )

                                            else
                                                let
                                                    signatureNode =
                                                        Node
                                                            { start = firstNameToken.start
                                                            , end = (Node.range sigTypeAnnotation).end
                                                            }
                                                            { name =
                                                                Node
                                                                    (tokenRange firstNameToken)
                                                                    firstNameToken.lexeme
                                                            , colonLocation = colonToken.start
                                                            , typeAnnotation = sigTypeAnnotation
                                                            }
                                                in
                                                finishFunctionDeclaration
                                                    firstNameToken
                                                    secondNameToken
                                                    (Just signatureNode)
                                                    afterSecondName

                                        [] ->
                                            Err "Expected function name after signature."

                        else
                            finishFunctionDeclaration
                                firstNameToken
                                firstNameToken
                                Nothing
                                afterFirstName

                    [] ->
                        finishFunctionDeclaration firstNameToken firstNameToken Nothing []

        [] ->
            Err "Expected function name."


finishFunctionDeclaration :
    Token.Token
    -> Token.Token
    -> Maybe (Node Expression.Signature)
    -> List Token.Token
    -> Result String ( Declaration.Declaration, List Token.Token )
finishFunctionDeclaration firstNameToken implNameToken maybeSignature tokens =
    case collectFunctionArguments firstNameToken.start.column [] tokens of
        Err e ->
            Err e

        Ok ( arguments, afterArguments ) ->
            case consumeToken Token.Equal "'='" afterArguments of
                Err e ->
                    Err e

                Ok ( equalToken, afterEqual ) ->
                    case
                        parseExpressionNodeAt
                            (firstNameToken.start.column + 1)
                            0
                            (dropTrivia afterEqual)
                    of
                        Err e ->
                            Err e

                        Ok ( bodyExpr, remaining ) ->
                            let
                                implRange =
                                    { start = implNameToken.start
                                    , end = (Node.range bodyExpr).end
                                    }

                                functionImpl =
                                    { name =
                                        Node (tokenRange implNameToken) implNameToken.lexeme
                                    , arguments = arguments
                                    , equalsTokenLocation = equalToken.start
                                    , expression = bodyExpr
                                    }

                                functionStruct =
                                    { documentation = Nothing
                                    , signature = maybeSignature
                                    , declaration = Node implRange functionImpl
                                    }
                            in
                            Ok
                                ( Declaration.FunctionDeclaration functionStruct
                                , remaining
                                )


collectFunctionArguments :
    Int
    -> List (Node Pattern.Pattern)
    -> List Token.Token
    -> Result String ( List (Node Pattern.Pattern), List Token.Token )
collectFunctionArguments indentMin argsRev tokens =
    case dropTrivia tokens of
        nextToken :: _ ->
            if canStartArgumentPattern nextToken then
                case parsePatternAtomic indentMin tokens of
                    Err e ->
                        Err e

                    Ok ( arg, remaining ) ->
                        collectFunctionArguments indentMin (arg :: argsRev) remaining

            else
                Ok ( List.reverse argsRev, tokens )

        [] ->
            Ok ( List.reverse argsRev, [] )


canStartArgumentPattern : Token.Token -> Bool
canStartArgumentPattern token =
    case token.tokenType of
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


canStartTypeAnnotation : Token.Token -> Bool
canStartTypeAnnotation token =
    case token.tokenType of
        Token.Identifier ->
            True

        Token.OpenParen ->
            True

        Token.OpenBrace ->
            True

        _ ->
            False


parseTypeAnnotation :
    Int
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseTypeAnnotation indentMin tokens =
    case parseTypeAnnotationFunctionParam indentMin tokens of
        Err e ->
            Err e

        Ok ( paramType, remaining ) ->
            case dropTrivia remaining of
                arrowToken :: afterArrow ->
                    if arrowToken.tokenType == Token.Arrow then
                        case
                            parseTypeAnnotation
                                (Node.range paramType).start.column
                                (dropTrivia afterArrow)
                        of
                            Err e ->
                                Err e

                            Ok ( returnType, afterReturn ) ->
                                let
                                    range =
                                        { start = (Node.range paramType).start
                                        , end = (Node.range returnType).end
                                        }
                                in
                                Ok
                                    ( Node range
                                        (TypeAnnotation.FunctionTypeAnnotation
                                            paramType
                                            arrowToken.start
                                            returnType
                                        )
                                    , afterReturn
                                    )

                    else
                        Ok ( paramType, remaining )

                [] ->
                    Ok ( paramType, [] )


parseTypeAnnotationFunctionParam :
    Int
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseTypeAnnotationFunctionParam indentMin tokens =
    case parseTypeAnnotationTypedArg indentMin tokens of
        Err e ->
            Err e

        Ok ( lessApp, remaining ) ->
            case Node.value lessApp of
                TypeAnnotation.Typed typedName [] ->
                    let
                        lessAppStartCol =
                            (Node.range lessApp).start.column
                    in
                    collectTypeApplicationArgs
                        indentMin
                        lessAppStartCol
                        typedName
                        lessApp
                        []
                        (dropTrivia remaining)

                _ ->
                    Ok ( lessApp, remaining )


collectTypeApplicationArgs :
    Int
    -> Int
    -> Node ( List String, String )
    -> Node TypeAnnotation.TypeAnnotation
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
collectTypeApplicationArgs indentMin lessAppStartCol typedName lessApp argsRev tokens =
    case tokens of
        nextToken :: _ ->
            if
                nextToken.start.column
                    > lessAppStartCol
                    && nextToken.start.column
                    > indentMin
                    && canStartTypeAnnotation nextToken
            then
                case parseTypeAnnotationTypedArg indentMin tokens of
                    Err e ->
                        Err e

                    Ok ( arg, remaining ) ->
                        collectTypeApplicationArgs
                            indentMin
                            lessAppStartCol
                            typedName
                            lessApp
                            (arg :: argsRev)
                            (dropTrivia remaining)

            else
                buildTypedResult lessApp typedName argsRev tokens

        [] ->
            buildTypedResult lessApp typedName argsRev []


buildTypedResult :
    Node TypeAnnotation.TypeAnnotation
    -> Node ( List String, String )
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
buildTypedResult lessApp typedName argsRev remaining =
    let
        args =
            List.reverse argsRev

        range =
            case List.reverse argsRev of
                lastArg :: _ ->
                    { start = (Node.range lessApp).start
                    , end = (Node.range lastArg).end
                    }

                [] ->
                    Node.range lessApp
    in
    Ok ( Node range (TypeAnnotation.Typed typedName args), remaining )


parseTypeAnnotationTypedArg :
    Int
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseTypeAnnotationTypedArg indentMin tokens =
    case dropTrivia tokens of
        [] ->
            Err "Expected a type annotation."

        firstToken :: rest ->
            case firstToken.tokenType of
                Token.OpenParen ->
                    parseParenthesizedTypeAnnotation indentMin firstToken rest

                Token.OpenBrace ->
                    parseRecordTypeAnnotation firstToken rest

                Token.Identifier ->
                    if startsWithUpper firstToken.lexeme then
                        let
                            ( nameTokens, remaining ) =
                                parseQualifiedName [ firstToken ] rest
                        in
                        case List.reverse nameTokens of
                            typeNameToken :: reversedModuleTokens ->
                                let
                                    moduleNames =
                                        List.reverse reversedModuleTokens
                                            |> List.map .lexeme

                                    typeRange =
                                        { start = firstToken.start
                                        , end = typeNameToken.end
                                        }
                                in
                                Ok
                                    ( Node typeRange
                                        (TypeAnnotation.Typed
                                            (Node typeRange ( moduleNames, typeNameToken.lexeme ))
                                            []
                                        )
                                    , remaining
                                    )

                            [] ->
                                Err "Expected a type name."

                    else
                        Ok
                            ( Node (tokenRange firstToken) (TypeAnnotation.GenericType firstToken.lexeme)
                            , rest
                            )

                _ ->
                    Err
                        ("Unsupported type annotation start: '"
                            ++ firstToken.lexeme
                            ++ "'."
                        )


parseParenthesizedTypeAnnotation :
    Int
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseParenthesizedTypeAnnotation indentMin openParen tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseParen then
                Ok
                    ( Node { start = openParen.start, end = closeToken.end } TypeAnnotation.Unit
                    , rest
                    )

            else
                case parseTypeAnnotation indentMin tokens of
                    Err e ->
                        Err e

                    Ok ( firstAnnotation, afterFirst ) ->
                        parseFurtherTypeAnnotations indentMin openParen firstAnnotation [] afterFirst

        [] ->
            Err "Expected ')' or a type annotation after '('."


parseFurtherTypeAnnotations :
    Int
    -> Token.Token
    -> Node TypeAnnotation.TypeAnnotation
    -> List ( Location, Node TypeAnnotation.TypeAnnotation )
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseFurtherTypeAnnotations indentMin openParen first restRev tokens =
    case dropTrivia tokens of
        token :: rest ->
            if token.tokenType == Token.CloseParen then
                let
                    range =
                        { start = openParen.start, end = token.end }

                    annotation =
                        TypeAnnotation.Tupled
                            (SeparatedSyntaxList.NonEmpty first (List.reverse restRev))
                in
                Ok ( Node range annotation, rest )

            else if token.tokenType == Token.Comma then
                case parseTypeAnnotation indentMin (dropTrivia rest) of
                    Err e ->
                        Err e

                    Ok ( nextAnnotation, remaining ) ->
                        parseFurtherTypeAnnotations
                            indentMin
                            openParen
                            first
                            (( token.start, nextAnnotation ) :: restRev)
                            remaining

            else
                Err
                    ("Expected ',' or ')' in type annotation, but found '"
                        ++ token.lexeme
                        ++ "'."
                    )

        [] ->
            Err "Expected ')' in type annotation."


parseRecordTypeAnnotation :
    Token.Token
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseRecordTypeAnnotation openBrace tokens =
    case dropTrivia tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseBrace then
                Ok
                    ( Node { start = openBrace.start, end = closeToken.end }
                        (TypeAnnotation.Record SeparatedSyntaxList.Empty)
                    , rest
                    )

            else
                case dropTrivia tokens of
                    firstIdToken :: afterFirstId ->
                        if firstIdToken.tokenType /= Token.Identifier then
                            Err
                                ("Expected record field name, but found '"
                                    ++ firstIdToken.lexeme
                                    ++ "'."
                                )

                        else
                            case dropTrivia afterFirstId of
                                pipeToken :: afterPipe ->
                                    if pipeToken.tokenType == Token.Pipe then
                                        parseGenericRecordBody
                                            openBrace
                                            firstIdToken
                                            pipeToken
                                            (dropTrivia afterPipe)

                                    else
                                        parseRecordTypeFields
                                            openBrace
                                            firstIdToken
                                            (dropTrivia afterFirstId)

                                [] ->
                                    Err "Expected '|' or ':' in record type annotation."

                    [] ->
                        Err "Expected record field name."

        [] ->
            Err "Expected '}' in record type annotation."


parseGenericRecordBody :
    Token.Token
    -> Token.Token
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseGenericRecordBody openBrace genericName pipeToken tokens =
    case tokens of
        closeToken :: rest ->
            if closeToken.tokenType == Token.CloseBrace then
                let
                    nodeRecordDefRange =
                        { start = pipeToken.end, end = pipeToken.end }

                    range =
                        { start = openBrace.start, end = closeToken.end }
                in
                Ok
                    ( Node range
                        (TypeAnnotation.GenericRecord
                            (Node (tokenRange genericName) genericName.lexeme)
                            pipeToken.start
                            (Node nodeRecordDefRange SeparatedSyntaxList.Empty)
                        )
                    , rest
                    )

            else
                parseGenericRecordFields openBrace genericName pipeToken pipeToken.end Nothing [] tokens

        [] ->
            Err "Expected '}' in generic record type annotation."


parseGenericRecordFields :
    Token.Token
    -> Token.Token
    -> Token.Token
    -> Location
    -> Maybe (Node TypeAnnotation.RecordField)
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseGenericRecordFields openBrace genericName pipeToken nodeRecordDefStart maybeFirst restRev tokens =
    case parseTypeRecordFieldFromName tokens of
        Err e ->
            Err e

        Ok ( fieldNode, fieldEnd, afterField ) ->
            case maybeFirst of
                Nothing ->
                    finishOrContinueGenericRecord
                        openBrace
                        genericName
                        pipeToken
                        nodeRecordDefStart
                        fieldNode
                        fieldEnd
                        []
                        (dropTrivia afterField)

                Just firstField ->
                    Err "Internal error: expected to parse first field in generic record."


finishOrContinueGenericRecord :
    Token.Token
    -> Token.Token
    -> Token.Token
    -> Location
    -> Node TypeAnnotation.RecordField
    -> Location
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
finishOrContinueGenericRecord openBrace genericName pipeToken nodeRecordDefStart firstField lastEnd restRev tokens =
    case tokens of
        token :: rest ->
            if token.tokenType == Token.CloseBrace then
                let
                    fieldsList =
                        SeparatedSyntaxList.NonEmpty firstField (List.reverse restRev)

                    nodeRecordDefRange =
                        { start = nodeRecordDefStart, end = lastEnd }

                    range =
                        { start = openBrace.start, end = token.end }
                in
                Ok
                    ( Node range
                        (TypeAnnotation.GenericRecord
                            (Node (tokenRange genericName) genericName.lexeme)
                            pipeToken.start
                            (Node nodeRecordDefRange fieldsList)
                        )
                    , rest
                    )

            else if token.tokenType == Token.Comma then
                case parseTypeRecordFieldFromName (dropTrivia rest) of
                    Err e ->
                        Err e

                    Ok ( nextField, nextEnd, afterNext ) ->
                        finishOrContinueGenericRecord
                            openBrace
                            genericName
                            pipeToken
                            nodeRecordDefStart
                            firstField
                            nextEnd
                            (( token.start, nextField ) :: restRev)
                            (dropTrivia afterNext)

            else
                Err
                    ("Expected ',' or '}' in generic record type annotation, but found '"
                        ++ token.lexeme
                        ++ "'."
                    )

        [] ->
            Err "Expected '}' in generic record type annotation."


parseRecordTypeFields :
    Token.Token
    -> Token.Token
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
parseRecordTypeFields openBrace firstFieldName tokens =
    case parseTypeRecordFieldFromName (firstFieldName :: tokens) of
        Err e ->
            Err e

        Ok ( firstField, _, afterFirst ) ->
            finishOrContinueRecord openBrace firstField [] (dropTrivia afterFirst)


finishOrContinueRecord :
    Token.Token
    -> Node TypeAnnotation.RecordField
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> List Token.Token
    -> Result String ( Node TypeAnnotation.TypeAnnotation, List Token.Token )
finishOrContinueRecord openBrace firstField restRev tokens =
    case tokens of
        token :: rest ->
            if token.tokenType == Token.CloseBrace then
                let
                    fieldsList =
                        SeparatedSyntaxList.NonEmpty firstField (List.reverse restRev)

                    range =
                        { start = openBrace.start, end = token.end }
                in
                Ok
                    ( Node range (TypeAnnotation.Record fieldsList)
                    , rest
                    )

            else if token.tokenType == Token.Comma then
                case parseTypeRecordFieldFromName (dropTrivia rest) of
                    Err e ->
                        Err e

                    Ok ( nextField, _, afterNext ) ->
                        finishOrContinueRecord
                            openBrace
                            firstField
                            (( token.start, nextField ) :: restRev)
                            (dropTrivia afterNext)

            else
                Err
                    ("Expected ',' or '}' in record type annotation, but found '"
                        ++ token.lexeme
                        ++ "'."
                    )

        [] ->
            Err "Expected '}' in record type annotation."


parseTypeRecordFieldFromName :
    List Token.Token
    -> Result String ( Node TypeAnnotation.RecordField, Location, List Token.Token )
parseTypeRecordFieldFromName tokens =
    case dropTrivia tokens of
        fieldNameToken :: afterFieldName ->
            if fieldNameToken.tokenType /= Token.Identifier then
                Err
                    ("Expected record field name, but found '"
                        ++ fieldNameToken.lexeme
                        ++ "'."
                    )

            else
                case consumeToken Token.Colon "':'" (dropTrivia afterFieldName) of
                    Err e ->
                        Err e

                    Ok ( colonToken, afterColon ) ->
                        case parseTypeAnnotation fieldNameToken.start.column (dropTrivia afterColon) of
                            Err e ->
                                Err e

                            Ok ( fieldTypeNode, remaining ) ->
                                let
                                    fieldEnd =
                                        (Node.range fieldTypeNode).end

                                    fieldRecord =
                                        { fieldName =
                                            Node (tokenRange fieldNameToken) fieldNameToken.lexeme
                                        , colonLocation = colonToken.start
                                        , fieldType = fieldTypeNode
                                        }

                                    fieldNode =
                                        Node
                                            { start = fieldNameToken.start, end = fieldEnd }
                                            fieldRecord
                                in
                                Ok ( fieldNode, fieldEnd, remaining )

        [] ->
            Err "Expected record field name."


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
                case dropTrivia rest of
                    colonToken :: afterColon ->
                        if colonToken.tokenType == Token.Colon then
                            case parseTypeAnnotation declarationIndent (dropTrivia afterColon) of
                                Ok ( typeAnnotation, afterTypeAnnotation ) ->
                                    case dropTrivia afterTypeAnnotation of
                                        implementationNameToken :: afterImplementationName ->
                                            if implementationNameToken.tokenType /= Token.Identifier then
                                                Err
                                                    ("Expected function name after signature, but found '"
                                                        ++ implementationNameToken.lexeme
                                                        ++ "'."
                                                    )

                                            else if implementationNameToken.lexeme /= nameToken.lexeme then
                                                Err
                                                    ("Function name does not match signature: "
                                                        ++ implementationNameToken.lexeme
                                                        ++ " != "
                                                        ++ nameToken.lexeme
                                                    )

                                            else
                                                finishLetFunctionDeclaration declarationIndent
                                                    nameToken
                                                    implementationNameToken
                                                    (Just
                                                        (Node
                                                            { start = nameToken.start
                                                            , end = (Node.range typeAnnotation).end
                                                            }
                                                            { name = Node (tokenRange nameToken) nameToken.lexeme
                                                            , colonLocation = colonToken.start
                                                            , typeAnnotation = typeAnnotation
                                                            }
                                                        )
                                                    )
                                                    afterImplementationName

                                        [] ->
                                            Err "Expected function name after signature."

                                Err error ->
                                    Err error

                        else
                            finishLetFunctionDeclaration declarationIndent nameToken nameToken Nothing rest

                    [] ->
                        finishLetFunctionDeclaration declarationIndent nameToken nameToken Nothing []

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


finishLetFunctionDeclaration :
    Int
    -> Token.Token
    -> Token.Token
    -> Maybe (Node Expression.Signature)
    -> List Token.Token
    -> Result String ( Node Expression.LetDeclaration, List Token.Token )
finishLetFunctionDeclaration declarationIndent firstNameToken implementationNameToken maybeSignature tokens =
    case parsePatternsUntilEqual declarationIndent tokens [] of
        Ok ( arguments, equalToken, afterEqual ) ->
            case parseExpressionNodeAt declarationIndent 0 afterEqual of
                Ok ( body, remaining ) ->
                    let
                        implementationRange =
                            { start = implementationNameToken.start, end = (Node.range body).end }

                        declarationRange =
                            { start = firstNameToken.start, end = (Node.range body).end }
                    in
                    Ok
                        ( Node declarationRange
                            (Expression.LetFunction
                                { documentation = Nothing
                                , signature = maybeSignature
                                , declaration =
                                    Node implementationRange
                                        { name =
                                            Node
                                                (tokenRange implementationNameToken)
                                                implementationNameToken.lexeme
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
    case token.tokenType of
        Token.Whitespace ->
            True

        Token.Newline ->
            True

        Token.Comment ->
            True

        _ ->
            False


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
