module ElmSyntax.Concrete.Parser.FromString exposing (..)

import Char
import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Exposing as Exposing
import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.File as File
import ElmSyntax.Concrete.Import as Import
import ElmSyntax.Concrete.Infix as Infix
import ElmSyntax.Concrete.Module as Module
import ElmSyntax.Concrete.Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.DeclarationOrExpression as DeclarationOrExpression exposing (DeclarationOrExpression)
import ElmSyntax.Concrete.Parser.StringParsing as StringParsing
    exposing
        ( LiteralRunBoundary(..)
        , LiteralTermination(..)
        , MultilineCommentRunEnd(..)
        , concatenateChunksRev
        , findLiteralRunEnd
        , hexStringToInt
        , isDigit
        , isFloatLiteral
        , isIdentifierChar
        , isIdentifierStart
        , isLowerCharacter
        , isOperatorChar
        , isUpperCharacter
        , isWhitespace
        , lineCommentEnd
        , literalTerminationLength
        , locationString
        , multilineCommentRunEnd
        , numberEnd
        , prependNonEmptyChunk
        , scanUnicodeEscapeDigits
        , skipOperatorChars
        , skipToIdentifierEnd
        , startsWithUpper
        )
import ElmSyntax.Concrete.Pattern as Pattern
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
import ElmSyntax.Concrete.TypeAnnotation as TypeAnnotation


{-| The complete state threaded through every recursive parsing function: the original source
string, the current offset into that string, the row and column matching that offset, and the
comments collected so far.

Row and column are stored directly instead of in a nested location record, so advancing the
state neither reads through nor rebuilds an inner record.

`commentsRev` collects every line and block comment in the order they are consumed, most recent
first. `skipTrivia` is the only place that recognizes a comment, therefore the state returned by
a successful parse already carries every comment of the parsed source and no separate scan over
the source is needed.

Parsing functions never skip trivia (whitespace and comments) before returning: the state a
function returns points at the exact end of the syntax it consumed. Callers that accept trivia
apply `skipTrivia` themselves, which keeps the adjacency checks (for example for record access)
exact. Because a caller that rejects what follows the trivia continues from the state before the
trivia, the comments of a rejected branch are dropped together with that state and collected
again by whichever `skipTrivia` result the parser keeps.

-}
type alias ParserState =
    { source : String
    , offset : Int
    , row : Int
    , column : Int
    , commentsRev : List (Node String)
    }


parseExpression : String -> Result String Expression.Expression
parseExpression input =
    finishParseExpression
        (parseExpressionNodeAt
            0
            0
            { source = input
            , offset = 0
            , row = 1
            , column = 1
            , commentsRev = []
            }
        )


finishParseExpression :
    Result String ( Node Expression.Expression, ParserState )
    -> Result String Expression.Expression
finishParseExpression expressionResult =
    case expressionResult of
        Err error ->
            Err error

        Ok ( expressionNode, stateAfterExpression ) ->
            finishParseExpressionAtEnd expressionNode (skipTrivia stateAfterExpression)


finishParseExpressionAtEnd :
    Node Expression.Expression
    -> ParserState
    -> Result String Expression.Expression
finishParseExpressionAtEnd expressionNode stateAfterTrivia =
    case String.left 1 (String.dropLeft stateAfterTrivia.offset stateAfterTrivia.source) of
        "" ->
            case expressionNode of
                Node _ expression ->
                    Ok expression

        _ ->
            Err
                ("Unexpected token '"
                    ++ snippetAt stateAfterTrivia
                    ++ "' after parsing expression."
                )


parseFile : String -> Result String File.File
parseFile input =
    case
        parseModuleDefinition
            { source = input
            , offset = 0
            , row = 1
            , column = 1
            , commentsRev = []
            }
    of
        Err error ->
            Err error

        Ok ( moduleDefinition, afterModule ) ->
            parseFileImports moduleDefinition afterModule


{-| Each stage of the file grammar hands its result to the next stage as arguments instead of
binding it in an enclosing `case` branch that later stages read from again.

A value destructured from a `case` is recomputed for every place the branch mentions it, so
holding on to an earlier stage's result while parsing the following stages would re-run the
earlier parse once per mention. Passing the result on as an argument evaluates each stage once.

-}
parseFileImports : Node Module.Module -> ParserState -> Result String File.File
parseFileImports moduleDefinition state =
    case parseImports [] state of
        Err error ->
            Err error

        Ok ( imports, afterImports ) ->
            parseFileBody moduleDefinition imports afterImports


parseFileBody :
    Node Module.Module
    -> List (Node Import.Import)
    -> ParserState
    -> Result String File.File
parseFileBody moduleDefinition imports state =
    case parseFileDeclarations [] [] (lastImportEndRow imports Nothing) state of
        Err error ->
            Err error

        Ok ( declarations, documentationComments, finalState ) ->
            finishFile moduleDefinition imports declarations documentationComments finalState


finishFile :
    Node Module.Module
    -> List (Node Import.Import)
    -> List (Node Declaration.Declaration)
    -> List (Node String)
    -> ParserState
    -> Result String File.File
finishFile moduleDefinition imports declarations documentationComments finalState =
    Ok
        { moduleDefinition = moduleDefinition
        , imports = imports
        , declarations = declarations
        , comments =
            commentsExcludingDocumentation
                documentationComments
                (List.reverse finalState.commentsRev)
        , incompleteDeclarations = []
        }


lastImportEndRow : List (Node Import.Import) -> Maybe Int -> Maybe Int
lastImportEndRow imports latest =
    case imports of
        [] ->
            latest

        (Node importRange _) :: rest ->
            lastImportEndRow rest (Just importRange.end.row)


parseDeclarationOrExpression : String -> Result String DeclarationOrExpression
parseDeclarationOrExpression input =
    let
        initialState =
            { source = input, offset = 0, row = 1, column = 1, commentsRev = [] }

        stateAtFirst =
            skipTrivia initialState
    in
    case String.left 1 (String.dropLeft stateAtFirst.offset input) of
        first ->
            if isIdentifierStart first && startsWithDeclarationKeyword input stateAtFirst.offset then
                case parseDeclaration stateAtFirst of
                    Err error ->
                        Err ("Failed to parse declaration or expression: " ++ error)

                    Ok ( declaration, afterDeclaration ) ->
                        let
                            stateAfterTrivia =
                                skipTrivia afterDeclaration
                        in
                        case String.left 1 (String.dropLeft stateAfterTrivia.offset input) of
                            "" ->
                                Ok (DeclarationOrExpression.Declaration declaration)

                            _ ->
                                Err
                                    ("Unexpected token '"
                                        ++ snippetAt stateAfterTrivia
                                        ++ "' after parsing declaration."
                                    )

            else if startsFunctionDeclaration input stateAtFirst.offset then
                case parseDeclaration stateAtFirst of
                    Err _ ->
                        parseAsExpressionFallback stateAtFirst

                    Ok ( declaration, afterDeclaration ) ->
                        let
                            stateAfterTrivia =
                                skipTrivia afterDeclaration
                        in
                        case String.left 1 (String.dropLeft stateAfterTrivia.offset input) of
                            "" ->
                                Ok (DeclarationOrExpression.Declaration declaration)

                            _ ->
                                parseAsExpressionFallback stateAtFirst

            else
                parseAsExpressionFallback stateAtFirst


parseAsExpressionFallback : ParserState -> Result String DeclarationOrExpression
parseAsExpressionFallback state =
    case parseExpressionNodeAt 0 0 state of
        Err error ->
            Err ("Failed to parse declaration or expression: " ++ error)

        Ok ( Node _ expression, stateAfterExpression ) ->
            let
                stateAfterTrivia =
                    skipTrivia stateAfterExpression
            in
            case String.left 1 (String.dropLeft stateAfterTrivia.offset stateAfterTrivia.source) of
                "" ->
                    Ok (DeclarationOrExpression.Expression expression)

                _ ->
                    Err
                        ("Unexpected token '"
                            ++ snippetAt stateAfterTrivia
                            ++ "' after parsing expression."
                        )


{-| True when the identifier starting at `offset` is one of the keywords that can only start a
declaration, never an expression.
-}
startsWithDeclarationKeyword : String -> Int -> Bool
startsWithDeclarationKeyword source offset =
    case String.left (skipToIdentifierEnd source (offset + 1) - offset) (String.dropLeft offset source) of
        "type" ->
            True

        "port" ->
            True

        "infix" ->
            True

        _ ->
            False


{-| Mirrors the lookahead that decides whether the input starts a function declaration rather
than an expression: an identifier that is not a block keyword, followed either by a type
annotation colon or by an equals sign outside of any bracket.
-}
startsFunctionDeclaration : String -> Int -> Bool
startsFunctionDeclaration source offset =
    case String.left 1 (String.dropLeft offset source) of
        first ->
            if not (isIdentifierStart first) then
                False

            else
                let
                    nameEnd =
                        skipToIdentifierEnd source (offset + 1)
                in
                case String.left (nameEnd - offset) (String.dropLeft offset source) of
                    "let" ->
                        False

                    "case" ->
                        False

                    "if" ->
                        False

                    _ ->
                        let
                            afterName =
                                skipTriviaOffset source nameEnd
                        in
                        case String.left 1 (String.dropLeft afterName source) of
                            ":" ->
                                if isOperatorChar (String.left 1 (String.dropLeft (afterName + 1) source)) then
                                    containsTopLevelEqual source nameEnd 0

                                else
                                    True

                            _ ->
                                containsTopLevelEqual source nameEnd 0


{-| Scans forward looking for an equals sign that is not nested in parentheses, braces or
brackets, skipping over comments, literals and multi-character operators (so `==` and `/=` never
count as an equals sign).
-}
containsTopLevelEqual : String -> Int -> Int -> Bool
containsTopLevelEqual source offset delimiterDepth =
    let
        nextThreeChars =
            String.left 3 (String.dropLeft offset source)
    in
    case nextThreeChars of
        "\"\"\"" ->
            containsTopLevelEqual source (literalEndOffset TripleQuoteTermination source (offset + 3)) delimiterDepth

        _ ->
            case String.left 2 nextThreeChars of
                "{-" ->
                    containsTopLevelEqual source (blockCommentEndOffset source (offset + 2) 1) delimiterDepth

                "--" ->
                    containsTopLevelEqual source (lineCommentEnd source (offset + 2)) delimiterDepth

                nextTwoChars ->
                    case String.left 1 nextTwoChars of
                        "" ->
                            False

                        "(" ->
                            containsTopLevelEqual source (offset + 1) (delimiterDepth + 1)

                        "[" ->
                            containsTopLevelEqual source (offset + 1) (delimiterDepth + 1)

                        ")" ->
                            containsTopLevelEqual source (offset + 1) (max 0 (delimiterDepth - 1))

                        "]" ->
                            containsTopLevelEqual source (offset + 1) (max 0 (delimiterDepth - 1))

                        "}" ->
                            containsTopLevelEqual source (offset + 1) (max 0 (delimiterDepth - 1))

                        "{" ->
                            containsTopLevelEqual source (offset + 1) (delimiterDepth + 1)

                        "-" ->
                            containsTopLevelEqual source (offset + 1) delimiterDepth

                        "=" ->
                            if isOperatorChar (String.left 1 (String.dropLeft 1 nextTwoChars)) then
                                containsTopLevelEqual source (offset + 2) delimiterDepth

                            else
                                delimiterDepth == 0

                        "\"" ->
                            containsTopLevelEqual source (literalEndOffset DoubleQuoteTermination source (offset + 1)) delimiterDepth

                        "'" ->
                            containsTopLevelEqual source (literalEndOffset SingleQuoteTermination source (offset + 1)) delimiterDepth

                        first ->
                            if isIdentifierStart first then
                                containsTopLevelEqual source (skipToIdentifierEnd source (offset + 1)) delimiterDepth

                            else if isDigit first then
                                containsTopLevelEqual source (numberEnd source first offset) delimiterDepth

                            else
                                containsTopLevelEqual source (offset + 1) delimiterDepth



-- MODULE DEFINITION


parseModuleDefinition : ParserState -> Result String ( Node Module.Module, ParserState )
parseModuleDefinition state =
    parseModuleDefinitionAt (skipTrivia state)


parseModuleDefinitionAt : ParserState -> Result String ( Node Module.Module, ParserState )
parseModuleDefinitionAt stateAtFirst =
    case String.left 1 (String.dropLeft stateAtFirst.offset stateAtFirst.source) of
        first ->
            if isIdentifierStart first then
                case String.left (skipToIdentifierEnd stateAtFirst.source (stateAtFirst.offset + 1) - stateAtFirst.offset) (String.dropLeft stateAtFirst.offset stateAtFirst.source) of
                    "effect" ->
                        parseEffectModule stateAtFirst

                    "port" ->
                        parsePortModule stateAtFirst

                    _ ->
                        parseNormalModule stateAtFirst

            else
                parseNormalModule stateAtFirst


parseNormalModule : ParserState -> Result String ( Node Module.Module, ParserState )
parseNormalModule state =
    finishDefaultModuleOnKeyword NormalDefaultModule (consumeKeyword "module" 6 state)


parsePortModule : ParserState -> Result String ( Node Module.Module, ParserState )
parsePortModule state =
    parsePortModuleOnKeyword (consumeKeyword "port" 4 state)


parsePortModuleOnKeyword :
    Result String ( Location, ParserState )
    -> Result String ( Node Module.Module, ParserState )
parsePortModuleOnKeyword portKeywordResult =
    case portKeywordResult of
        Err error ->
            Err error

        Ok ( portTokenLocation, afterPortKeyword ) ->
            parsePortModuleOnModuleKeyword
                portTokenLocation
                (consumeKeyword "module" 6 afterPortKeyword)


parsePortModuleOnModuleKeyword :
    Location
    -> Result String ( Location, ParserState )
    -> Result String ( Node Module.Module, ParserState )
parsePortModuleOnModuleKeyword portTokenLocation moduleKeywordResult =
    case moduleKeywordResult of
        Err error ->
            Err error

        Ok ( _, afterModuleKeyword ) ->
            finishDefaultModule PortDefaultModule portTokenLocation afterModuleKeyword


type DefaultModuleKind
    = NormalDefaultModule
    | PortDefaultModule


finishDefaultModuleOnKeyword :
    DefaultModuleKind
    -> Result String ( Location, ParserState )
    -> Result String ( Node Module.Module, ParserState )
finishDefaultModuleOnKeyword moduleKind keywordResult =
    case keywordResult of
        Err error ->
            Err error

        Ok ( moduleTokenLocation, afterModuleKeyword ) ->
            finishDefaultModule moduleKind moduleTokenLocation afterModuleKeyword


finishDefaultModule :
    DefaultModuleKind
    -> Location
    -> ParserState
    -> Result String ( Node Module.Module, ParserState )
finishDefaultModule moduleKind startLocation state =
    finishDefaultModuleExposing moduleKind startLocation (parseModuleName state)


finishDefaultModuleExposing :
    DefaultModuleKind
    -> Location
    -> Result String ( Node Module.ModuleName, ParserState )
    -> Result String ( Node Module.Module, ParserState )
finishDefaultModuleExposing moduleKind startLocation moduleNameResult =
    case moduleNameResult of
        Err error ->
            Err error

        Ok ( moduleName, afterModuleName ) ->
            finishDefaultModuleOnExposing
                moduleKind
                startLocation
                moduleName
                (parseExposing afterModuleName)


finishDefaultModuleOnExposing :
    DefaultModuleKind
    -> Location
    -> Node Module.ModuleName
    -> Result String ( Node Exposing.Exposing, ParserState )
    -> Result String ( Node Module.Module, ParserState )
finishDefaultModuleOnExposing moduleKind startLocation moduleName exposingResult =
    case exposingResult of
        Err error ->
            Err error

        Ok ( exposingList, remaining ) ->
            finishDefaultModuleNode moduleKind startLocation moduleName exposingList remaining


finishDefaultModuleNode :
    DefaultModuleKind
    -> Location
    -> Node Module.ModuleName
    -> Node Exposing.Exposing
    -> ParserState
    -> Result String ( Node Module.Module, ParserState )
finishDefaultModuleNode moduleKind startLocation moduleName exposingList remaining =
    let
        (Node exposingRange _) =
            exposingList

        moduleData =
            { moduleName = moduleName
            , exposingList = exposingList
            }

        parsedModule =
            case moduleKind of
                NormalDefaultModule ->
                    Module.NormalModule moduleData

                PortDefaultModule ->
                    Module.PortModule moduleData
    in
    Ok
        ( Node
            { start = startLocation
            , end = exposingRange.end
            }
            parsedModule
        , remaining
        )


parseEffectModule : ParserState -> Result String ( Node Module.Module, ParserState )
parseEffectModule state =
    case consumeKeyword "effect" 6 state of
        Err error ->
            Err error

        Ok ( effectTokenLocation, afterEffect ) ->
            case consumeKeyword "module" 6 afterEffect of
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
                                    case parseExposing afterWhere of
                                        Err error ->
                                            Err error

                                        Ok ( exposingList, remaining ) ->
                                            let
                                                (Node exposingRange _) =
                                                    exposingList
                                            in
                                            Ok
                                                ( Node
                                                    { start = effectTokenLocation
                                                    , end = exposingRange.end
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
    ParserState
    -> Result String ( Maybe (Node String), Maybe (Node String), ParserState )
parseEffectWhere state =
    parseEffectWhereAt state (skipTrivia state)


parseEffectWhereAt :
    ParserState
    -> ParserState
    -> Result String ( Maybe (Node String), Maybe (Node String), ParserState )
parseEffectWhereAt state stateAtWhere =
    case String.left 1 (String.dropLeft stateAtWhere.offset stateAtWhere.source) of
        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtWhere.source (stateAtWhere.offset + 1) - stateAtWhere.offset) (String.dropLeft stateAtWhere.offset stateAtWhere.source)
                            == "where"
                       )
            then
                case consumeKeyword "where" 5 stateAtWhere of
                    Err error ->
                        Err error

                    Ok ( _, afterWhere ) ->
                        let
                            stateAtOpenBrace =
                                skipTrivia afterWhere
                        in
                        case String.left 1 (String.dropLeft stateAtOpenBrace.offset stateAtOpenBrace.source) of
                            "{" ->
                                parseEffectWhereFields
                                    Nothing
                                    Nothing
                                    { source = stateAtOpenBrace.source
                                    , offset = stateAtOpenBrace.offset + 1
                                    , row = stateAtOpenBrace.row
                                    , column = stateAtOpenBrace.column + 1
                                    , commentsRev = stateAtOpenBrace.commentsRev
                                    }

                            _ ->
                                Err ("Expected '{', but found '" ++ snippetAt stateAtOpenBrace ++ "'.")

            else
                Ok ( Nothing, Nothing, state )


parseEffectWhereFields :
    Maybe (Node String)
    -> Maybe (Node String)
    -> ParserState
    -> Result String ( Maybe (Node String), Maybe (Node String), ParserState )
parseEffectWhereFields command subscription state =
    parseEffectWhereFieldsAt command subscription (skipTrivia state)


parseEffectWhereFieldsAt :
    Maybe (Node String)
    -> Maybe (Node String)
    -> ParserState
    -> Result String ( Maybe (Node String), Maybe (Node String), ParserState )
parseEffectWhereFieldsAt command subscription stateAtField =
    case String.left 1 (String.dropLeft stateAtField.offset stateAtField.source) of
        "}" ->
            Ok
                ( command
                , subscription
                , { source = stateAtField.source
                  , offset = stateAtField.offset + 1
                  , row = stateAtField.row
                  , column = stateAtField.column + 1
                  , commentsRev = stateAtField.commentsRev
                  }
                )

        first ->
            if isIdentifierStart first then
                let
                    fieldNameEnd =
                        skipToIdentifierEnd stateAtField.source (stateAtField.offset + 1)

                    fieldName =
                        String.left (fieldNameEnd - stateAtField.offset) (String.dropLeft stateAtField.offset stateAtField.source)

                    stateAtEquals =
                        skipTrivia
                            { source = stateAtField.source
                            , offset = fieldNameEnd
                            , row = stateAtField.row
                            , column = stateAtField.column + (fieldNameEnd - stateAtField.offset)
                            , commentsRev = stateAtField.commentsRev
                            }
                in
                case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                    "=" ->
                        case
                            parseModuleName
                                { source = stateAtEquals.source
                                , offset = stateAtEquals.offset + 1
                                , row = stateAtEquals.row
                                , column = stateAtEquals.column + 1
                                , commentsRev = stateAtEquals.commentsRev
                                }
                        of
                            Err error ->
                                Err error

                            Ok ( Node valueRange valueNames, afterValue ) ->
                                let
                                    valueName =
                                        case List.reverse valueNames of
                                            name :: _ ->
                                                name

                                            [] ->
                                                ""

                                    value =
                                        Node valueRange valueName

                                    nextCommand =
                                        if fieldName == "command" then
                                            Just value

                                        else
                                            command

                                    nextSubscription =
                                        if fieldName == "subscription" then
                                            Just value

                                        else
                                            subscription

                                    stateAtSeparator =
                                        skipTrivia afterValue
                                in
                                case String.left 1 (String.dropLeft stateAtSeparator.offset stateAtSeparator.source) of
                                    "," ->
                                        parseEffectWhereFields
                                            nextCommand
                                            nextSubscription
                                            { source = stateAtSeparator.source
                                            , offset = stateAtSeparator.offset + 1
                                            , row = stateAtSeparator.row
                                            , column = stateAtSeparator.column + 1
                                            , commentsRev = stateAtSeparator.commentsRev
                                            }

                                    _ ->
                                        parseEffectWhereFields nextCommand nextSubscription afterValue

                    _ ->
                        Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")

            else
                Err
                    ("Expected an effect module field or '}', but found '"
                        ++ snippetAt stateAtField
                        ++ "'."
                    )



-- IMPORTS


parseImports :
    List (Node Import.Import)
    -> ParserState
    -> Result String ( List (Node Import.Import), ParserState )
parseImports importsRev state =
    parseImportsAt importsRev state (skipTrivia state)


parseImportsAt :
    List (Node Import.Import)
    -> ParserState
    -> ParserState
    -> Result String ( List (Node Import.Import), ParserState )
parseImportsAt importsRev state stateAtImport =
    case String.left 1 (String.dropLeft stateAtImport.offset stateAtImport.source) of
        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtImport.source (stateAtImport.offset + 1) - stateAtImport.offset) (String.dropLeft stateAtImport.offset stateAtImport.source)
                            == "import"
                       )
            then
                parseImportsOnImport importsRev (parseImport stateAtImport)

            else
                Ok ( List.reverse importsRev, state )


parseImportsOnImport :
    List (Node Import.Import)
    -> Result String ( Node Import.Import, ParserState )
    -> Result String ( List (Node Import.Import), ParserState )
parseImportsOnImport importsRev importResult =
    case importResult of
        Err error ->
            Err error

        Ok ( importNode, remaining ) ->
            parseImports (importNode :: importsRev) remaining


parseImport : ParserState -> Result String ( Node Import.Import, ParserState )
parseImport state =
    parseImportOnKeyword (consumeKeyword "import" 6 state)


parseImportOnKeyword :
    Result String ( Location, ParserState )
    -> Result String ( Node Import.Import, ParserState )
parseImportOnKeyword keywordResult =
    case keywordResult of
        Err error ->
            Err error

        Ok ( importTokenLocation, afterImport ) ->
            parseImportOnModuleName importTokenLocation (parseModuleName afterImport)


parseImportOnModuleName :
    Location
    -> Result String ( Node Module.ModuleName, ParserState )
    -> Result String ( Node Import.Import, ParserState )
parseImportOnModuleName importTokenLocation moduleNameResult =
    case moduleNameResult of
        Err error ->
            Err error

        Ok ( moduleName, afterModuleName ) ->
            parseImportOnAlias importTokenLocation moduleName (parseImportAlias afterModuleName)


parseImportOnAlias :
    Location
    -> Node Module.ModuleName
    -> Result String ( Maybe ( Location, Node Module.ModuleName ), ParserState )
    -> Result String ( Node Import.Import, ParserState )
parseImportOnAlias importTokenLocation moduleName aliasResult =
    case aliasResult of
        Err error ->
            Err error

        Ok ( moduleAlias, afterAlias ) ->
            parseImportOnExposing
                importTokenLocation
                moduleName
                moduleAlias
                (parseOptionalExposing afterAlias)


parseImportOnExposing :
    Location
    -> Node Module.ModuleName
    -> Maybe ( Location, Node Module.ModuleName )
    -> Result String ( Maybe ( Location, Node Exposing.Exposing ), ParserState )
    -> Result String ( Node Import.Import, ParserState )
parseImportOnExposing importTokenLocation moduleName moduleAlias exposingResult =
    case exposingResult of
        Err error ->
            Err error

        Ok ( exposingList, remaining ) ->
            Ok
                ( Node
                    { start = importTokenLocation
                    , end = importEndLocation moduleName moduleAlias exposingList
                    }
                    { importTokenLocation = importTokenLocation
                    , moduleName = moduleName
                    , moduleAlias = moduleAlias
                    , exposingList = exposingList
                    }
                , remaining
                )


importEndLocation :
    Node Module.ModuleName
    -> Maybe ( Location, Node Module.ModuleName )
    -> Maybe ( Location, Node Exposing.Exposing )
    -> Location
importEndLocation moduleName moduleAlias exposingList =
    case exposingList of
        Just ( _, Node exposingRange _ ) ->
            exposingRange.end

        Nothing ->
            case moduleAlias of
                Just ( _, Node aliasRange _ ) ->
                    aliasRange.end

                Nothing ->
                    let
                        (Node moduleNameRange _) =
                            moduleName
                    in
                    moduleNameRange.end


parseImportAlias :
    ParserState
    -> Result String ( Maybe ( Location, Node Module.ModuleName ), ParserState )
parseImportAlias state =
    parseImportAliasAt state (skipTrivia state)


parseImportAliasAt :
    ParserState
    -> ParserState
    -> Result String ( Maybe ( Location, Node Module.ModuleName ), ParserState )
parseImportAliasAt state stateAtAs =
    case String.left 1 (String.dropLeft stateAtAs.offset stateAtAs.source) of
        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtAs.source (stateAtAs.offset + 1) - stateAtAs.offset) (String.dropLeft stateAtAs.offset stateAtAs.source)
                            == "as"
                       )
            then
                let
                    asTokenLocation =
                        { row = stateAtAs.row, column = stateAtAs.column }

                    afterAs =
                        { source = stateAtAs.source
                        , offset = stateAtAs.offset + 2
                        , row = stateAtAs.row
                        , column = stateAtAs.column + 2
                        , commentsRev = stateAtAs.commentsRev
                        }

                    stateAtAlias =
                        skipTrivia afterAs
                in
                case String.left 1 (String.dropLeft stateAtAlias.offset stateAtAlias.source) of
                    aliasFirst ->
                        if isIdentifierStart aliasFirst then
                            let
                                aliasEnd =
                                    skipToIdentifierEnd stateAtAlias.source (stateAtAlias.offset + 1)

                                aliasLength =
                                    aliasEnd - stateAtAlias.offset
                            in
                            Ok
                                ( Just
                                    ( asTokenLocation
                                    , Node
                                        { start = { row = stateAtAlias.row, column = stateAtAlias.column }
                                        , end = { row = stateAtAlias.row, column = stateAtAlias.column + aliasLength }
                                        }
                                        [ String.left (aliasEnd - stateAtAlias.offset) (String.dropLeft stateAtAlias.offset stateAtAlias.source) ]
                                    )
                                , { source = stateAtAlias.source
                                  , offset = aliasEnd
                                  , row = stateAtAlias.row
                                  , column = stateAtAlias.column + aliasLength
                                  , commentsRev = stateAtAlias.commentsRev
                                  }
                                )

                        else
                            Err
                                ("Expected module alias, but found '"
                                    ++ snippetAt stateAtAlias
                                    ++ "'."
                                )

            else
                Ok ( Nothing, state )


parseOptionalExposing :
    ParserState
    -> Result String ( Maybe ( Location, Node Exposing.Exposing ), ParserState )
parseOptionalExposing state =
    parseOptionalExposingAt state (skipTrivia state)


parseOptionalExposingAt :
    ParserState
    -> ParserState
    -> Result String ( Maybe ( Location, Node Exposing.Exposing ), ParserState )
parseOptionalExposingAt state stateAtExposing =
    case String.left 1 (String.dropLeft stateAtExposing.offset stateAtExposing.source) of
        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtExposing.source (stateAtExposing.offset + 1) - stateAtExposing.offset) (String.dropLeft stateAtExposing.offset stateAtExposing.source)
                            == "exposing"
                       )
            then
                case parseExposing stateAtExposing of
                    Err error ->
                        Err error

                    Ok ( exposingNode, remaining ) ->
                        Ok
                            ( Just
                                ( { row = stateAtExposing.row, column = stateAtExposing.column }
                                , exposingNode
                                )
                            , remaining
                            )

            else
                Ok ( Nothing, state )


parseModuleName : ParserState -> Result String ( Node Module.ModuleName, ParserState )
parseModuleName state =
    parseModuleNameAt (skipTrivia state)


parseModuleNameAt : ParserState -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNameAt stateAtName =
    case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
        first ->
            if isIdentifierStart first then
                parseModuleNameFromEnd
                    stateAtName
                    (skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1))

            else
                Err ("Expected module name, but found '" ++ snippetAt stateAtName ++ "'.")


parseModuleNameFromEnd :
    ParserState
    -> Int
    -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNameFromEnd stateAtName nameEnd =
    parseModuleNameRest
        { row = stateAtName.row, column = stateAtName.column }
        stateAtName.row
        (stateAtName.column + (nameEnd - stateAtName.offset))
        [ String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source) ]
        { source = stateAtName.source
        , offset = nameEnd
        , row = stateAtName.row
        , column = stateAtName.column + (nameEnd - stateAtName.offset)
        , commentsRev = stateAtName.commentsRev
        }


parseModuleNameRest :
    Location
    -> Int
    -> Int
    -> List String
    -> ParserState
    -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNameRest start endRow endColumn partsRev state =
    parseModuleNameRestAt start endRow endColumn partsRev state state


parseModuleNameRestAt :
    Location
    -> Int
    -> Int
    -> List String
    -> ParserState
    -> ParserState
    -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNameRestAt start endRow endColumn partsRev state stateAtDot =
    if isDotToken stateAtDot.source stateAtDot.offset then
        parseModuleNamePartAt
            start
            partsRev
            { source = stateAtDot.source
            , offset = stateAtDot.offset + 1
            , row = stateAtDot.row
            , column = stateAtDot.column + 1
            , commentsRev = stateAtDot.commentsRev
            }

    else
        Ok
            ( Node
                { start = start, end = { row = endRow, column = endColumn } }
                (List.reverse partsRev)
            , state
            )


parseModuleNamePartAt :
    Location
    -> List String
    -> ParserState
    -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNamePartAt start partsRev stateAtPart =
    case String.left 1 (String.dropLeft stateAtPart.offset stateAtPart.source) of
        first ->
            if isIdentifierStart first then
                parseModuleNamePartFromEnd
                    start
                    partsRev
                    stateAtPart
                    (skipToIdentifierEnd stateAtPart.source (stateAtPart.offset + 1))

            else
                Err ("Expected module name part, but found '" ++ snippetAt stateAtPart ++ "'.")


parseModuleNamePartFromEnd :
    Location
    -> List String
    -> ParserState
    -> Int
    -> Result String ( Node Module.ModuleName, ParserState )
parseModuleNamePartFromEnd start partsRev stateAtPart partEnd =
    parseModuleNameRest
        start
        stateAtPart.row
        (stateAtPart.column + (partEnd - stateAtPart.offset))
        (String.left (partEnd - stateAtPart.offset) (String.dropLeft stateAtPart.offset stateAtPart.source) :: partsRev)
        { source = stateAtPart.source
        , offset = partEnd
        , row = stateAtPart.row
        , column = stateAtPart.column + (partEnd - stateAtPart.offset)
        , commentsRev = stateAtPart.commentsRev
        }


parseExposing : ParserState -> Result String ( Node Exposing.Exposing, ParserState )
parseExposing state =
    parseExposingOnKeyword (consumeKeyword "exposing" 8 state)


parseExposingOnKeyword :
    Result String ( Location, ParserState )
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExposingOnKeyword keywordResult =
    case keywordResult of
        Err error ->
            Err error

        Ok ( exposingTokenLocation, afterExposing ) ->
            let
                stateAtOpenParen =
                    skipTrivia afterExposing
            in
            case String.left 1 (String.dropLeft stateAtOpenParen.offset stateAtOpenParen.source) of
                "(" ->
                    let
                        afterOpenParen =
                            { source = stateAtOpenParen.source
                            , offset = stateAtOpenParen.offset + 1
                            , row = stateAtOpenParen.row
                            , column = stateAtOpenParen.column + 1
                            , commentsRev = stateAtOpenParen.commentsRev
                            }
                    in
                    parseExposingListAt
                        exposingTokenLocation
                        { row = stateAtOpenParen.row, column = stateAtOpenParen.column }
                        afterOpenParen
                        (skipTrivia afterOpenParen)

                _ ->
                    Err ("Expected '(', but found '" ++ snippetAt stateAtOpenParen ++ "'.")


parseExposingListAt :
    Location
    -> Location
    -> ParserState
    -> ParserState
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExposingListAt exposingTokenLocation openParenLocation afterOpenParen stateAtFirst =
    case String.left 2 (String.dropLeft stateAtFirst.offset stateAtFirst.source) of
        ".." ->
            let
                stateAtCloseParen =
                    skipTrivia
                        { source = stateAtFirst.source
                        , offset = stateAtFirst.offset + 2
                        , row = stateAtFirst.row
                        , column = stateAtFirst.column + 2
                        , commentsRev = stateAtFirst.commentsRev
                        }
            in
            case String.left 1 (String.dropLeft stateAtCloseParen.offset stateAtCloseParen.source) of
                ")" ->
                    let
                        remaining =
                            { source = stateAtCloseParen.source
                            , offset = stateAtCloseParen.offset + 1
                            , row = stateAtCloseParen.row
                            , column = stateAtCloseParen.column + 1
                            , commentsRev = stateAtCloseParen.commentsRev
                            }
                    in
                    Ok
                        ( Node
                            { start = exposingTokenLocation
                            , end = { row = remaining.row, column = remaining.column }
                            }
                            (Exposing.All
                                { start = { row = stateAtFirst.row, column = stateAtFirst.column }
                                , end = { row = stateAtFirst.row, column = stateAtFirst.column + 2 }
                                }
                            )
                        , remaining
                        )

                _ ->
                    Err ("Expected ')', but found '" ++ snippetAt stateAtCloseParen ++ "'.")

        _ ->
            parseExplicitExposing
                exposingTokenLocation
                openParenLocation
                Nothing
                []
                afterOpenParen


parseExplicitExposing :
    Location
    -> Location
    -> Maybe (Node Exposing.TopLevelExpose)
    -> List ( Location, Node Exposing.TopLevelExpose )
    -> ParserState
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExplicitExposing exposingTokenLocation openParenLocation first restRev state =
    parseExplicitExposingAt exposingTokenLocation openParenLocation first restRev (skipTrivia state)


parseExplicitExposingAt :
    Location
    -> Location
    -> Maybe (Node Exposing.TopLevelExpose)
    -> List ( Location, Node Exposing.TopLevelExpose )
    -> ParserState
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExplicitExposingAt exposingTokenLocation openParenLocation first restRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
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
                    { start = exposingTokenLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (Exposing.Explicit
                        openParenLocation
                        nodes
                        { row = stateAtToken.row, column = stateAtToken.column }
                    )
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        separator ->
            case first of
                Nothing ->
                    parseExplicitExposingOnFirst
                        exposingTokenLocation
                        openParenLocation
                        restRev
                        (parseTopLevelExpose stateAtToken)

                Just _ ->
                    if separator == "," then
                        parseExplicitExposingOnFurther
                            exposingTokenLocation
                            openParenLocation
                            first
                            restRev
                            { row = stateAtToken.row, column = stateAtToken.column }
                            (parseTopLevelExpose
                                { source = stateAtToken.source
                                , offset = stateAtToken.offset + 1
                                , row = stateAtToken.row
                                , column = stateAtToken.column + 1
                                , commentsRev = stateAtToken.commentsRev
                                }
                            )

                    else
                        Err "Expected ',' before exposing list item."


parseExplicitExposingOnFirst :
    Location
    -> Location
    -> List ( Location, Node Exposing.TopLevelExpose )
    -> Result String ( Node Exposing.TopLevelExpose, ParserState )
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExplicitExposingOnFirst exposingTokenLocation openParenLocation restRev exposeResult =
    case exposeResult of
        Err error ->
            Err error

        Ok ( exposeNode, afterExpose ) ->
            parseExplicitExposing
                exposingTokenLocation
                openParenLocation
                (Just exposeNode)
                restRev
                afterExpose


parseExplicitExposingOnFurther :
    Location
    -> Location
    -> Maybe (Node Exposing.TopLevelExpose)
    -> List ( Location, Node Exposing.TopLevelExpose )
    -> Location
    -> Result String ( Node Exposing.TopLevelExpose, ParserState )
    -> Result String ( Node Exposing.Exposing, ParserState )
parseExplicitExposingOnFurther exposingTokenLocation openParenLocation first restRev separatorLocation exposeResult =
    case exposeResult of
        Err error ->
            Err error

        Ok ( exposeNode, afterExpose ) ->
            parseExplicitExposing
                exposingTokenLocation
                openParenLocation
                first
                (( separatorLocation, exposeNode ) :: restRev)
                afterExpose


parseTopLevelExpose : ParserState -> Result String ( Node Exposing.TopLevelExpose, ParserState )
parseTopLevelExpose state =
    parseTopLevelExposeAt (skipTrivia state)


parseTopLevelExposeAt : ParserState -> Result String ( Node Exposing.TopLevelExpose, ParserState )
parseTopLevelExposeAt stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "(" ->
            let
                stateAtOperator =
                    skipTrivia
                        { source = stateAtToken.source
                        , offset = stateAtToken.offset + 1
                        , row = stateAtToken.row
                        , column = stateAtToken.column + 1
                        , commentsRev = stateAtToken.commentsRev
                        }

                operatorLength =
                    operatorTokenLength stateAtOperator.source stateAtOperator.offset
            in
            if operatorLength == 0 then
                Err "Expected an operator followed by ')' in exposing list."

            else
                let
                    stateAtClose =
                        skipTrivia
                            { source = stateAtOperator.source
                            , offset = stateAtOperator.offset + operatorLength
                            , row = stateAtOperator.row
                            , column = stateAtOperator.column + operatorLength
                            , commentsRev = stateAtOperator.commentsRev
                            }
                in
                if String.left 1 (String.dropLeft stateAtClose.offset stateAtClose.source) == ")" then
                    Ok
                        ( Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtClose.row, column = stateAtClose.column + 1 }
                            }
                            (Exposing.InfixExpose
                                (String.left operatorLength (String.dropLeft stateAtOperator.offset stateAtOperator.source))
                            )
                        , { source = stateAtClose.source
                          , offset = stateAtClose.offset + 1
                          , row = stateAtClose.row
                          , column = stateAtClose.column + 1
                          , commentsRev = stateAtClose.commentsRev
                          }
                        )

                else
                    Err "Expected an operator followed by ')' in exposing list."

        first ->
            if isIdentifierStart first then
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    name =
                        String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)

                    afterName =
                        { source = stateAtToken.source
                        , offset = nameEnd
                        , row = stateAtToken.row
                        , column = stateAtToken.column + nameLength
                        , commentsRev = stateAtToken.commentsRev
                        }
                in
                if startsWithUpper name then
                    parseUpperExpose
                        { row = stateAtToken.row, column = stateAtToken.column }
                        stateAtToken.row
                        (stateAtToken.column + nameLength)
                        name
                        afterName

                else
                    Ok
                        ( Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                            }
                            (Exposing.FunctionExpose name)
                        , afterName
                        )

            else
                Err ("Unexpected token '" ++ snippetAt stateAtToken ++ "' in exposing list.")


parseUpperExpose :
    Location
    -> Int
    -> Int
    -> String
    -> ParserState
    -> Result String ( Node Exposing.TopLevelExpose, ParserState )
parseUpperExpose nameStart nameEndRow nameEndColumn name state =
    parseUpperExposeAt nameStart nameEndRow nameEndColumn name state (skipTrivia state)


parseUpperExposeAt :
    Location
    -> Int
    -> Int
    -> String
    -> ParserState
    -> ParserState
    -> Result String ( Node Exposing.TopLevelExpose, ParserState )
parseUpperExposeAt nameStart nameEndRow nameEndColumn name state stateAtOpen =
    if String.left 1 (String.dropLeft stateAtOpen.offset stateAtOpen.source) == "(" then
        let
            stateAtInner =
                skipTrivia
                    { source = stateAtOpen.source
                    , offset = stateAtOpen.offset + 1
                    , row = stateAtOpen.row
                    , column = stateAtOpen.column + 1
                    , commentsRev = stateAtOpen.commentsRev
                    }
        in
        case String.left 1 (String.dropLeft stateAtInner.offset stateAtInner.source) of
            ")" ->
                Ok
                    ( Node
                        { start = nameStart
                        , end = { row = stateAtInner.row, column = stateAtInner.column + 1 }
                        }
                        (Exposing.TypeExpose { name = name, open = Nothing })
                    , { source = stateAtInner.source
                      , offset = stateAtInner.offset + 1
                      , row = stateAtInner.row
                      , column = stateAtInner.column + 1
                      , commentsRev = stateAtInner.commentsRev
                      }
                    )

            _ ->
                if String.left 2 (String.dropLeft stateAtInner.offset stateAtInner.source) == ".." then
                    let
                        stateAtCloseParen =
                            skipTrivia
                                { source = stateAtInner.source
                                , offset = stateAtInner.offset + 2
                                , row = stateAtInner.row
                                , column = stateAtInner.column + 2
                                , commentsRev = stateAtInner.commentsRev
                                }
                    in
                    case String.left 1 (String.dropLeft stateAtCloseParen.offset stateAtCloseParen.source) of
                        ")" ->
                            let
                                remaining =
                                    { source = stateAtCloseParen.source
                                    , offset = stateAtCloseParen.offset + 1
                                    , row = stateAtCloseParen.row
                                    , column = stateAtCloseParen.column + 1
                                    , commentsRev = stateAtCloseParen.commentsRev
                                    }
                            in
                            Ok
                                ( Node
                                    { start = nameStart
                                    , end = { row = remaining.row, column = remaining.column }
                                    }
                                    (Exposing.TypeExpose
                                        { name = name
                                        , open =
                                            Just
                                                { start = { row = stateAtOpen.row, column = stateAtOpen.column }
                                                , end = { row = remaining.row, column = remaining.column }
                                                }
                                        }
                                    )
                                , remaining
                                )

                        _ ->
                            Err ("Expected ')', but found '" ++ snippetAt stateAtCloseParen ++ "'.")

                else
                    Err "Expected '..' or ')' after exposed type name."

    else
        Ok
            ( Node
                { start = nameStart, end = { row = nameEndRow, column = nameEndColumn } }
                (Exposing.TypeOrAliasExpose name)
            , state
            )



-- FILE DECLARATIONS


parseFileDeclarations :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Maybe Int
    -> ParserState
    -> Result String ( List (Node Declaration.Declaration), List (Node String), ParserState )
parseFileDeclarations declarationsRev documentationCommentsRev previousRangeEndRow state =
    parseFileDeclarationsAt declarationsRev documentationCommentsRev previousRangeEndRow state (skipTrivia state)


parseFileDeclarationsAt :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Maybe Int
    -> ParserState
    -> ParserState
    -> Result String ( List (Node Declaration.Declaration), List (Node String), ParserState )
parseFileDeclarationsAt declarationsRev documentationCommentsRev previousRangeEndRow state stateAtDeclaration =
    case String.left 1 (String.dropLeft stateAtDeclaration.offset stateAtDeclaration.source) of
        "" ->
            Ok
                ( List.reverse declarationsRev
                , List.reverse documentationCommentsRev
                , stateAtDeclaration
                )

        _ ->
            if stateAtDeclaration.column /= 1 then
                Err
                    ("Unexpected token '"
                        ++ snippetAt stateAtDeclaration
                        ++ "' after parsing "
                        ++ String.fromInt (List.length declarationsRev)
                        ++ " declarations."
                    )

            else
                parseFileDeclarationsOnDeclaration
                    declarationsRev
                    documentationCommentsRev
                    (attachableDocumentationComment
                        previousRangeEndRow
                        stateAtDeclaration.row
                        state.row
                        state.column
                        0
                        stateAtDeclaration.commentsRev
                    )
                    (parseDeclaration stateAtDeclaration)


parseFileDeclarationsOnDeclaration :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Maybe (Node String)
    -> Result String ( Declaration.Declaration, ParserState )
    -> Result String ( List (Node Declaration.Declaration), List (Node String), ParserState )
parseFileDeclarationsOnDeclaration declarationsRev documentationCommentsRev documentation declarationResult =
    case declarationResult of
        Err error ->
            Err error

        Ok ( declaration, remaining ) ->
            case documentation of
                Nothing ->
                    parseFileDeclarationsWithRange
                        declarationsRev
                        documentationCommentsRev
                        declaration
                        (rangeOfDeclaration declaration)
                        remaining

                Just documentationNode ->
                    parseFileDeclarationsWithDocumentation
                        declarationsRev
                        (documentationNode :: documentationCommentsRev)
                        (setDeclarationDocumentation documentationNode declaration)
                        remaining


parseFileDeclarationsWithDocumentation :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Declaration.Declaration
    -> ParserState
    -> Result String ( List (Node Declaration.Declaration), List (Node String), ParserState )
parseFileDeclarationsWithDocumentation declarationsRev documentationCommentsRev declaration remaining =
    parseFileDeclarationsWithRange
        declarationsRev
        documentationCommentsRev
        declaration
        (rangeOfDeclaration declaration)
        remaining


parseFileDeclarationsWithRange :
    List (Node Declaration.Declaration)
    -> List (Node String)
    -> Declaration.Declaration
    -> Range
    -> ParserState
    -> Result String ( List (Node Declaration.Declaration), List (Node String), ParserState )
parseFileDeclarationsWithRange declarationsRev documentationCommentsRev declaration declarationRange remaining =
    parseFileDeclarations
        (Node declarationRange declaration :: declarationsRev)
        documentationCommentsRev
        (Just declarationRange.end.row)
        remaining


{-| Returns the documentation comment attaching to a declaration, taken from the comments that
`skipTrivia` already collected in the trivia directly in front of that declaration.

`commentsRev` is the parser state's comment list, most recent first. The walk ends at the first
comment starting before `fromRow`/`fromColumn`, the position where that trivia begins, which is
the end of the syntax preceding the declaration.

`plainCommentRow` is the start row of the plain (non-documentation) comment following the
candidate under consideration, or `0` while no plain comment was passed: a plain comment on a row
after the candidate's last row cancels the attachment. Because the walk runs from the declaration
backwards, the first plain comment it passes is the last one in the source and therefore the one
with the highest start row.

-}
attachableDocumentationComment :
    Maybe Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List (Node String)
    -> Maybe (Node String)
attachableDocumentationComment previousRangeEndRow declarationRow fromRow fromColumn plainCommentRow commentsRev =
    case commentsRev of
        [] ->
            Nothing

        (Node commentRange commentText) :: remainingCommentsRev ->
            if
                (commentRange.start.row < fromRow)
                    || (commentRange.start.row == fromRow && commentRange.start.column < fromColumn)
            then
                Nothing

            else if String.startsWith "{-|" commentText then
                let
                    isAfterPreviousRange =
                        case previousRangeEndRow of
                            Nothing ->
                                commentRange.end.row + 1 == declarationRow

                            Just previousEndRow ->
                                previousEndRow < commentRange.end.row
                in
                if commentRange.end.row < declarationRow && isAfterPreviousRange then
                    if plainCommentRow > commentRange.end.row then
                        Nothing

                    else
                        Just (Node commentRange commentText)

                else
                    attachableDocumentationComment
                        previousRangeEndRow
                        declarationRow
                        fromRow
                        fromColumn
                        plainCommentRow
                        remainingCommentsRev

            else if plainCommentRow == 0 then
                attachableDocumentationComment
                    previousRangeEndRow
                    declarationRow
                    fromRow
                    fromColumn
                    commentRange.start.row
                    remainingCommentsRev

            else
                attachableDocumentationComment
                    previousRangeEndRow
                    declarationRow
                    fromRow
                    fromColumn
                    plainCommentRow
                    remainingCommentsRev


setDeclarationDocumentation : Node String -> Declaration.Declaration -> Declaration.Declaration
setDeclarationDocumentation documentation declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            Declaration.FunctionDeclaration
                { documentation = Just documentation
                , signature = function.signature
                , declaration = function.declaration
                }

        Declaration.ChoiceTypeDeclaration choiceType ->
            Declaration.ChoiceTypeDeclaration
                { documentation = Just documentation
                , typeTokenLocation = choiceType.typeTokenLocation
                , name = choiceType.name
                , generics = choiceType.generics
                , equalsTokenLocation = choiceType.equalsTokenLocation
                , constructors = choiceType.constructors
                }

        Declaration.AliasDeclaration typeAlias ->
            Declaration.AliasDeclaration
                { documentation = Just documentation
                , typeTokenLocation = typeAlias.typeTokenLocation
                , aliasTokenLocation = typeAlias.aliasTokenLocation
                , name = typeAlias.name
                , generics = typeAlias.generics
                , equalsTokenLocation = typeAlias.equalsTokenLocation
                , typeAnnotation = typeAlias.typeAnnotation
                }

        Declaration.PortDeclaration _ _ ->
            declaration

        Declaration.InfixDeclaration _ ->
            declaration


rangeOfDeclaration : Declaration.Declaration -> Range
rangeOfDeclaration declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                (Node functionRange _) =
                    function.declaration
            in
            { start =
                case function.signature of
                    Nothing ->
                        functionRange.start

                    Just (Node signatureRange _) ->
                        signatureRange.start
            , end = functionRange.end
            }

        Declaration.ChoiceTypeDeclaration choiceType ->
            { start = choiceType.typeTokenLocation
            , end =
                case choiceType.constructors of
                    SeparatedSyntaxList.NonEmpty first rest ->
                        case List.reverse rest of
                            ( _, Node lastRange _ ) :: _ ->
                                lastRange.end

                            [] ->
                                let
                                    (Node firstRange _) =
                                        first
                                in
                                firstRange.end

                    SeparatedSyntaxList.Empty ->
                        let
                            (Node nameRange _) =
                                choiceType.name
                        in
                        nameRange.end
            }

        Declaration.AliasDeclaration typeAlias ->
            let
                (Node typeAnnotationRange _) =
                    typeAlias.typeAnnotation
            in
            { start = typeAlias.typeTokenLocation
            , end = typeAnnotationRange.end
            }

        Declaration.PortDeclaration portTokenLocation signature ->
            let
                (Node typeAnnotationRange _) =
                    signature.typeAnnotation
            in
            { start = portTokenLocation
            , end = typeAnnotationRange.end
            }

        Declaration.InfixDeclaration infix ->
            let
                (Node functionRange _) =
                    infix.function
            in
            { start = infix.infixTokenLocation
            , end = functionRange.end
            }


commentsExcludingDocumentation : List (Node String) -> List (Node String) -> List (Node String)
commentsExcludingDocumentation documentationComments comments =
    case comments of
        comment :: rest ->
            commentsExcludingDocumentationAtComment documentationComments comment rest

        [] ->
            []


commentsExcludingDocumentationAtComment :
    List (Node String)
    -> Node String
    -> List (Node String)
    -> List (Node String)
commentsExcludingDocumentationAtComment documentationComments comment remainingComments =
    let
        (Node commentRange _) =
            comment
    in
    case documentationComments of
        (Node documentationRange _) :: remainingDocumentationComments ->
            if
                commentRange.start == documentationRange.start
                    && commentRange.end == documentationRange.end
            then
                commentsExcludingDocumentation remainingDocumentationComments remainingComments

            else if locationBefore documentationRange.start commentRange.start then
                commentsExcludingDocumentationAtComment remainingDocumentationComments comment remainingComments

            else
                comment :: commentsExcludingDocumentation documentationComments remainingComments

        [] ->
            comment :: commentsExcludingDocumentation [] remainingComments


locationBefore : Location -> Location -> Bool
locationBefore left right =
    left.row
        < right.row
        || (left.row == right.row && left.column < right.column)



-- DECLARATIONS


parseDeclaration : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseDeclaration state =
    parseDeclarationAt (skipTrivia state)


parseDeclarationAt : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseDeclarationAt stateAtDeclaration =
    case String.left 1 (String.dropLeft stateAtDeclaration.offset stateAtDeclaration.source) of
        first ->
            if isIdentifierStart first then
                case String.left (skipToIdentifierEnd stateAtDeclaration.source (stateAtDeclaration.offset + 1) - stateAtDeclaration.offset) (String.dropLeft stateAtDeclaration.offset stateAtDeclaration.source) of
                    "infix" ->
                        parseInfixDeclaration stateAtDeclaration

                    "type" ->
                        parseTypeDeclaration stateAtDeclaration

                    "port" ->
                        parsePortDeclaration stateAtDeclaration

                    _ ->
                        parseFunctionDeclaration stateAtDeclaration

            else
                parseFunctionDeclaration stateAtDeclaration


parseInfixDeclaration : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseInfixDeclaration state =
    case consumeKeyword "infix" 5 state of
        Err error ->
            Err error

        Ok ( infixTokenLocation, afterInfix ) ->
            let
                stateAtDirection =
                    skipTrivia afterInfix
            in
            case String.left 1 (String.dropLeft stateAtDirection.offset stateAtDirection.source) of
                directionFirst ->
                    if not (isIdentifierStart directionFirst) then
                        Err
                            ("Expected infix direction, but found '"
                                ++ snippetAt stateAtDirection
                                ++ "'."
                            )

                    else
                        let
                            directionEnd =
                                skipToIdentifierEnd stateAtDirection.source (stateAtDirection.offset + 1)

                            directionLexeme =
                                String.left (directionEnd - stateAtDirection.offset) (String.dropLeft stateAtDirection.offset stateAtDirection.source)

                            directionRange =
                                { start = { row = stateAtDirection.row, column = stateAtDirection.column }
                                , end = { row = stateAtDirection.row, column = stateAtDirection.column + (directionEnd - stateAtDirection.offset) }
                                }

                            afterDirection =
                                { source = stateAtDirection.source
                                , offset = directionEnd
                                , row = stateAtDirection.row
                                , column = stateAtDirection.column + (directionEnd - stateAtDirection.offset)
                                , commentsRev = stateAtDirection.commentsRev
                                }
                        in
                        case parseInfixDirection directionLexeme of
                            Nothing ->
                                Err ("Infix direction is not a valid value: " ++ directionLexeme)

                            Just direction ->
                                parseInfixDeclarationFromDirection
                                    infixTokenLocation
                                    (Node directionRange direction)
                                    afterDirection


parseInfixDeclarationFromDirection :
    Location
    -> Node Infix.InfixDirection
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseInfixDeclarationFromDirection infixTokenLocation direction state =
    parseInfixDeclarationFromDirectionAt infixTokenLocation direction (skipTrivia state)


parseInfixDeclarationFromDirectionAt :
    Location
    -> Node Infix.InfixDirection
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseInfixDeclarationFromDirectionAt infixTokenLocation direction stateAtPrecedence =
    case String.left 1 (String.dropLeft stateAtPrecedence.offset stateAtPrecedence.source) of
        precedenceFirst ->
            if not (isDigit precedenceFirst) then
                Err
                    ("Expected infix precedence, but found '"
                        ++ snippetAt stateAtPrecedence
                        ++ "'."
                    )

            else
                let
                    precedenceEnd =
                        numberEnd stateAtPrecedence.source precedenceFirst stateAtPrecedence.offset

                    precedenceLexeme =
                        String.left (precedenceEnd - stateAtPrecedence.offset) (String.dropLeft stateAtPrecedence.offset stateAtPrecedence.source)

                    precedenceRange =
                        { start = { row = stateAtPrecedence.row, column = stateAtPrecedence.column }
                        , end = { row = stateAtPrecedence.row, column = stateAtPrecedence.column + (precedenceEnd - stateAtPrecedence.offset) }
                        }

                    afterPrecedence =
                        { source = stateAtPrecedence.source
                        , offset = precedenceEnd
                        , row = stateAtPrecedence.row
                        , column = stateAtPrecedence.column + (precedenceEnd - stateAtPrecedence.offset)
                        , commentsRev = stateAtPrecedence.commentsRev
                        }
                in
                case String.toInt precedenceLexeme of
                    Nothing ->
                        Err ("Infix precedence is not a number: " ++ precedenceLexeme)

                    Just precedence ->
                        let
                            stateAtOpenParen =
                                skipTrivia afterPrecedence
                        in
                        case String.left 1 (String.dropLeft stateAtOpenParen.offset stateAtOpenParen.source) of
                            "(" ->
                                let
                                    openParenLocation =
                                        { row = stateAtOpenParen.row, column = stateAtOpenParen.column }

                                    stateAtOperator =
                                        skipTrivia
                                            { source = stateAtOpenParen.source
                                            , offset = stateAtOpenParen.offset + 1
                                            , row = stateAtOpenParen.row
                                            , column = stateAtOpenParen.column + 1
                                            , commentsRev = stateAtOpenParen.commentsRev
                                            }

                                    operatorLength =
                                        operatorTokenLength stateAtOperator.source stateAtOperator.offset
                                in
                                if operatorLength == 0 then
                                    Err
                                        ("Expected operator symbol, but found '"
                                            ++ snippetAt stateAtOperator
                                            ++ "'."
                                        )

                                else
                                    let
                                        operatorLexeme =
                                            String.left operatorLength (String.dropLeft stateAtOperator.offset stateAtOperator.source)

                                        afterOperator =
                                            { source = stateAtOperator.source
                                            , offset = stateAtOperator.offset + operatorLength
                                            , row = stateAtOperator.row
                                            , column = stateAtOperator.column + operatorLength
                                            , commentsRev = stateAtOperator.commentsRev
                                            }

                                        stateAtCloseParen =
                                            skipTrivia afterOperator
                                    in
                                    case String.left 1 (String.dropLeft stateAtCloseParen.offset stateAtCloseParen.source) of
                                        ")" ->
                                            let
                                                afterClose =
                                                    { source = stateAtCloseParen.source
                                                    , offset = stateAtCloseParen.offset + 1
                                                    , row = stateAtCloseParen.row
                                                    , column = stateAtCloseParen.column + 1
                                                    , commentsRev = stateAtCloseParen.commentsRev
                                                    }

                                                stateAtEquals =
                                                    skipTrivia afterClose
                                            in
                                            case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                                                "=" ->
                                                    let
                                                        equalsLocation =
                                                            { row = stateAtEquals.row, column = stateAtEquals.column }

                                                        stateAtFunction =
                                                            skipTrivia
                                                                { source = stateAtEquals.source
                                                                , offset = stateAtEquals.offset + 1
                                                                , row = stateAtEquals.row
                                                                , column = stateAtEquals.column + 1
                                                                , commentsRev = stateAtEquals.commentsRev
                                                                }
                                                    in
                                                    case String.left 1 (String.dropLeft stateAtFunction.offset stateAtFunction.source) of
                                                        functionFirst ->
                                                            if not (isIdentifierStart functionFirst) then
                                                                Err
                                                                    ("Expected function name, but found '"
                                                                        ++ snippetAt stateAtFunction
                                                                        ++ "'."
                                                                    )

                                                            else
                                                                let
                                                                    functionNameEnd =
                                                                        skipToIdentifierEnd stateAtFunction.source (stateAtFunction.offset + 1)

                                                                    functionNameLength =
                                                                        functionNameEnd - stateAtFunction.offset
                                                                in
                                                                Ok
                                                                    ( Declaration.InfixDeclaration
                                                                        { infixTokenLocation = infixTokenLocation
                                                                        , direction = direction
                                                                        , precedence = Node precedenceRange precedence
                                                                        , operator =
                                                                            Node
                                                                                { start = openParenLocation
                                                                                , end = { row = afterClose.row, column = afterClose.column }
                                                                                }
                                                                                operatorLexeme
                                                                        , equalsTokenLocation = equalsLocation
                                                                        , function =
                                                                            Node
                                                                                { start = { row = stateAtFunction.row, column = stateAtFunction.column }
                                                                                , end = { row = stateAtFunction.row, column = stateAtFunction.column + functionNameLength }
                                                                                }
                                                                                (String.left (functionNameEnd - stateAtFunction.offset) (String.dropLeft stateAtFunction.offset stateAtFunction.source))
                                                                        }
                                                                    , { source = stateAtFunction.source
                                                                      , offset = functionNameEnd
                                                                      , row = stateAtFunction.row
                                                                      , column = stateAtFunction.column + functionNameLength
                                                                      , commentsRev = stateAtFunction.commentsRev
                                                                      }
                                                                    )

                                                _ ->
                                                    Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")

                                        _ ->
                                            Err ("Expected ')', but found '" ++ snippetAt stateAtCloseParen ++ "'.")

                            _ ->
                                Err ("Expected '(', but found '" ++ snippetAt stateAtOpenParen ++ "'.")


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


parseTypeDeclaration : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseTypeDeclaration state =
    case consumeKeyword "type" 4 state of
        Err error ->
            Err error

        Ok ( typeTokenLocation, afterType ) ->
            let
                stateAtNext =
                    skipTrivia afterType
            in
            case String.left 1 (String.dropLeft stateAtNext.offset stateAtNext.source) of
                first ->
                    if
                        isIdentifierStart first
                            && (String.left (skipToIdentifierEnd stateAtNext.source (stateAtNext.offset + 1) - stateAtNext.offset) (String.dropLeft stateAtNext.offset stateAtNext.source)
                                    == "alias"
                               )
                    then
                        parseAliasDeclaration typeTokenLocation stateAtNext

                    else
                        parseChoiceTypeDeclaration typeTokenLocation stateAtNext


parseAliasDeclaration :
    Location
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseAliasDeclaration typeTokenLocation state =
    case consumeKeyword "alias" 5 state of
        Err error ->
            Err error

        Ok ( aliasTokenLocation, afterAlias ) ->
            let
                stateAtName =
                    skipTrivia afterAlias
            in
            case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
                first ->
                    if not (isIdentifierStart first) then
                        Err "Expected type alias name."

                    else
                        let
                            nameEnd =
                                skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                            nameLength =
                                nameEnd - stateAtName.offset

                            nameNode =
                                Node
                                    { start = { row = stateAtName.row, column = stateAtName.column }
                                    , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                                    }
                                    (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))

                            afterName =
                                { source = stateAtName.source
                                , offset = nameEnd
                                , row = stateAtName.row
                                , column = stateAtName.column + nameLength
                                , commentsRev = stateAtName.commentsRev
                                }

                            ( generics, afterGenerics ) =
                                collectTypeGenerics afterName []

                            stateAtEquals =
                                skipTrivia afterGenerics
                        in
                        case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                            "=" ->
                                case
                                    parseTypeAnnotation
                                        0
                                        { source = stateAtEquals.source
                                        , offset = stateAtEquals.offset + 1
                                        , row = stateAtEquals.row
                                        , column = stateAtEquals.column + 1
                                        , commentsRev = stateAtEquals.commentsRev
                                        }
                                of
                                    Err error ->
                                        Err error

                                    Ok ( typeAnnotationNode, remaining ) ->
                                        Ok
                                            ( Declaration.AliasDeclaration
                                                { documentation = Nothing
                                                , typeTokenLocation = typeTokenLocation
                                                , aliasTokenLocation = aliasTokenLocation
                                                , name = nameNode
                                                , generics = generics
                                                , equalsTokenLocation = { row = stateAtEquals.row, column = stateAtEquals.column }
                                                , typeAnnotation = typeAnnotationNode
                                                }
                                            , remaining
                                            )

                            _ ->
                                Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")


parseChoiceTypeDeclaration :
    Location
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseChoiceTypeDeclaration typeTokenLocation state =
    parseChoiceTypeDeclarationAt typeTokenLocation (skipTrivia state)


parseChoiceTypeDeclarationAt :
    Location
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseChoiceTypeDeclarationAt typeTokenLocation stateAtName =
    case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
        first ->
            if not (isIdentifierStart first) then
                Err "Expected type name."

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                    nameLength =
                        nameEnd - stateAtName.offset

                    nameNode =
                        Node
                            { start = { row = stateAtName.row, column = stateAtName.column }
                            , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                            }
                            (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))

                    afterName =
                        { source = stateAtName.source
                        , offset = nameEnd
                        , row = stateAtName.row
                        , column = stateAtName.column + nameLength
                        , commentsRev = stateAtName.commentsRev
                        }

                    ( generics, afterGenerics ) =
                        collectTypeGenerics afterName []

                    stateAtEquals =
                        skipTrivia afterGenerics
                in
                case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                    "=" ->
                        case
                            parseChoiceTypeConstructor
                                { source = stateAtEquals.source
                                , offset = stateAtEquals.offset + 1
                                , row = stateAtEquals.row
                                , column = stateAtEquals.column + 1
                                , commentsRev = stateAtEquals.commentsRev
                                }
                        of
                            Err error ->
                                Err error

                            Ok ( firstConstructor, afterFirstConstructor ) ->
                                case parseMoreChoiceConstructors firstConstructor [] afterFirstConstructor of
                                    Err error ->
                                        Err error

                                    Ok ( constructors, remaining ) ->
                                        Ok
                                            ( Declaration.ChoiceTypeDeclaration
                                                { documentation = Nothing
                                                , typeTokenLocation = typeTokenLocation
                                                , name = nameNode
                                                , generics = generics
                                                , equalsTokenLocation = { row = stateAtEquals.row, column = stateAtEquals.column }
                                                , constructors = constructors
                                                }
                                            , remaining
                                            )

                    _ ->
                        Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")


parseChoiceTypeConstructor :
    ParserState
    -> Result String ( Node Declaration.ValueConstructor, ParserState )
parseChoiceTypeConstructor state =
    parseChoiceTypeConstructorAt (skipTrivia state)


parseChoiceTypeConstructorAt :
    ParserState
    -> Result String ( Node Declaration.ValueConstructor, ParserState )
parseChoiceTypeConstructorAt stateAtName =
    case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
        first ->
            if not (isIdentifierStart first) then
                Err
                    ("Expected constructor name, but found '"
                        ++ snippetAt stateAtName
                        ++ "'."
                    )

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                    nameLength =
                        nameEnd - stateAtName.offset

                    nameRange =
                        { start = { row = stateAtName.row, column = stateAtName.column }
                        , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                        }

                    afterName =
                        { source = stateAtName.source
                        , offset = nameEnd
                        , row = stateAtName.row
                        , column = stateAtName.column + nameLength
                        , commentsRev = stateAtName.commentsRev
                        }
                in
                case parseChoiceTypeConstructorArgs stateAtName.column [] afterName of
                    Err error ->
                        Err error

                    Ok ( arguments, afterArguments ) ->
                        let
                            constructorEnd =
                                case List.reverse arguments of
                                    (Node lastArgumentRange _) :: _ ->
                                        lastArgumentRange.end

                                    [] ->
                                        nameRange.end
                        in
                        Ok
                            ( Node
                                { start = nameRange.start, end = constructorEnd }
                                { name =
                                    Node nameRange (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))
                                , arguments = arguments
                                }
                            , afterArguments
                            )


parseMoreChoiceConstructors :
    Node Declaration.ValueConstructor
    -> List ( Location, Node Declaration.ValueConstructor )
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node Declaration.ValueConstructor), ParserState )
parseMoreChoiceConstructors firstConstructor restRev state =
    parseMoreChoiceConstructorsAt firstConstructor restRev state (skipTrivia state)


parseMoreChoiceConstructorsAt :
    Node Declaration.ValueConstructor
    -> List ( Location, Node Declaration.ValueConstructor )
    -> ParserState
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node Declaration.ValueConstructor), ParserState )
parseMoreChoiceConstructorsAt firstConstructor restRev state stateAtPipe =
    if isPipeToken stateAtPipe.source stateAtPipe.offset then
        let
            pipeLocation =
                { row = stateAtPipe.row, column = stateAtPipe.column }

            afterPipe =
                { source = stateAtPipe.source
                , offset = stateAtPipe.offset + 1
                , row = stateAtPipe.row
                , column = stateAtPipe.column + 1
                , commentsRev = stateAtPipe.commentsRev
                }
        in
        case parseChoiceTypeConstructor afterPipe of
            Err error ->
                Err error

            Ok ( constructorNode, remaining ) ->
                parseMoreChoiceConstructors
                    firstConstructor
                    (( pipeLocation, constructorNode ) :: restRev)
                    remaining

    else
        Ok
            ( SeparatedSyntaxList.NonEmpty firstConstructor (List.reverse restRev)
            , state
            )


parseChoiceTypeConstructorArgs :
    Int
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> ParserState
    -> Result String ( List (Node TypeAnnotation.TypeAnnotation), ParserState )
parseChoiceTypeConstructorArgs constructorColumn argumentsRev state =
    parseChoiceTypeConstructorArgsAt constructorColumn argumentsRev state (skipTrivia state)


parseChoiceTypeConstructorArgsAt :
    Int
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> ParserState
    -> ParserState
    -> Result String ( List (Node TypeAnnotation.TypeAnnotation), ParserState )
parseChoiceTypeConstructorArgsAt constructorColumn argumentsRev state stateAtArgument =
    if
        stateAtArgument.column
            >= constructorColumn
            && canStartTypeAnnotationAt stateAtArgument.source stateAtArgument.offset
    then
        case parseTypeAnnotationTypedArg constructorColumn stateAtArgument of
            Err error ->
                Err error

            Ok ( argument, remaining ) ->
                parseChoiceTypeConstructorArgs constructorColumn (argument :: argumentsRev) remaining

    else
        Ok ( List.reverse argumentsRev, state )


collectTypeGenerics :
    ParserState
    -> List (Node String)
    -> ( List (Node String), ParserState )
collectTypeGenerics state genericsRev =
    collectTypeGenericsAt state genericsRev (skipTrivia state)


collectTypeGenericsAt :
    ParserState
    -> List (Node String)
    -> ParserState
    -> ( List (Node String), ParserState )
collectTypeGenericsAt state genericsRev stateAtGeneric =
    case String.left 1 (String.dropLeft stateAtGeneric.offset stateAtGeneric.source) of
        first ->
            if isIdentifierStart first then
                let
                    genericEnd =
                        skipToIdentifierEnd stateAtGeneric.source (stateAtGeneric.offset + 1)

                    genericLength =
                        genericEnd - stateAtGeneric.offset
                in
                collectTypeGenerics
                    { source = stateAtGeneric.source
                    , offset = genericEnd
                    , row = stateAtGeneric.row
                    , column = stateAtGeneric.column + genericLength
                    , commentsRev = stateAtGeneric.commentsRev
                    }
                    (Node
                        { start = { row = stateAtGeneric.row, column = stateAtGeneric.column }
                        , end = { row = stateAtGeneric.row, column = stateAtGeneric.column + genericLength }
                        }
                        (String.left (genericEnd - stateAtGeneric.offset) (String.dropLeft stateAtGeneric.offset stateAtGeneric.source))
                        :: genericsRev
                    )

            else
                ( List.reverse genericsRev, state )


parsePortDeclaration : ParserState -> Result String ( Declaration.Declaration, ParserState )
parsePortDeclaration state =
    case consumeKeyword "port" 4 state of
        Err error ->
            Err error

        Ok ( portTokenLocation, afterPort ) ->
            let
                stateAtName =
                    skipTrivia afterPort
            in
            case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
                first ->
                    if not (isIdentifierStart first) then
                        Err "Expected port name."

                    else
                        let
                            nameEnd =
                                skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                            nameLength =
                                nameEnd - stateAtName.offset

                            nameNode =
                                Node
                                    { start = { row = stateAtName.row, column = stateAtName.column }
                                    , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                                    }
                                    (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))

                            stateAtColon =
                                skipTrivia
                                    { source = stateAtName.source
                                    , offset = nameEnd
                                    , row = stateAtName.row
                                    , column = stateAtName.column + nameLength
                                    , commentsRev = stateAtName.commentsRev
                                    }
                        in
                        case String.left 1 (String.dropLeft stateAtColon.offset stateAtColon.source) of
                            ":" ->
                                case
                                    parseTypeAnnotation
                                        0
                                        { source = stateAtColon.source
                                        , offset = stateAtColon.offset + 1
                                        , row = stateAtColon.row
                                        , column = stateAtColon.column + 1
                                        , commentsRev = stateAtColon.commentsRev
                                        }
                                of
                                    Err error ->
                                        Err error

                                    Ok ( typeAnnotationNode, remaining ) ->
                                        Ok
                                            ( Declaration.PortDeclaration
                                                portTokenLocation
                                                { name = nameNode
                                                , colonLocation = { row = stateAtColon.row, column = stateAtColon.column }
                                                , typeAnnotation = typeAnnotationNode
                                                }
                                            , remaining
                                            )

                            _ ->
                                Err ("Expected ':', but found '" ++ snippetAt stateAtColon ++ "'.")


parseFunctionDeclaration : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclaration state =
    parseFunctionDeclarationAt (skipTrivia state)


parseFunctionDeclarationAt : ParserState -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationAt stateAtName =
    case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
        first ->
            if not (isIdentifierStart first) then
                Err ("Expected function name, but found '" ++ snippetAt stateAtName ++ "'.")

            else
                parseFunctionDeclarationWithName
                    stateAtName
                    (skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1))


parseFunctionDeclarationWithName :
    ParserState
    -> Int
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationWithName stateAtName nameEnd =
    parseFunctionDeclarationAfterName
        { start = { row = stateAtName.row, column = stateAtName.column }
        , end = { row = stateAtName.row, column = stateAtName.column + (nameEnd - stateAtName.offset) }
        }
        (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))
        { source = stateAtName.source
        , offset = nameEnd
        , row = stateAtName.row
        , column = stateAtName.column + (nameEnd - stateAtName.offset)
        , commentsRev = stateAtName.commentsRev
        }


parseFunctionDeclarationAfterName :
    Range
    -> String
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationAfterName nameRange name afterName =
    parseFunctionDeclarationAtColon nameRange name afterName (skipTrivia afterName)


parseFunctionDeclarationAtColon :
    Range
    -> String
    -> ParserState
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationAtColon nameRange name afterName stateAtColon =
    if isColonToken stateAtColon.source stateAtColon.offset then
        parseFunctionDeclarationOnSignature
            nameRange
            name
            { row = stateAtColon.row, column = stateAtColon.column }
            (parseTypeAnnotation
                nameRange.start.column
                { source = stateAtColon.source
                , offset = stateAtColon.offset + 1
                , row = stateAtColon.row
                , column = stateAtColon.column + 1
                , commentsRev = stateAtColon.commentsRev
                }
            )

    else
        finishFunctionDeclaration nameRange.start nameRange name Nothing afterName


parseFunctionDeclarationOnSignature :
    Range
    -> String
    -> Location
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationOnSignature nameRange name colonLocation signatureResult =
    case signatureResult of
        Err error ->
            Err error

        Ok ( signatureTypeNode, afterSignatureType ) ->
            parseFunctionDeclarationAtSecondName
                nameRange
                name
                colonLocation
                signatureTypeNode
                (skipTrivia afterSignatureType)


parseFunctionDeclarationAtSecondName :
    Range
    -> String
    -> Location
    -> Node TypeAnnotation.TypeAnnotation
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationAtSecondName nameRange name colonLocation signatureTypeNode stateAtSecondName =
    case String.left 1 (String.dropLeft stateAtSecondName.offset stateAtSecondName.source) of
        secondFirst ->
            if not (isIdentifierStart secondFirst) then
                Err
                    ("Expected function name after signature, but found '"
                        ++ snippetAt stateAtSecondName
                        ++ "'."
                    )

            else
                parseFunctionDeclarationWithSecondName
                    nameRange
                    name
                    colonLocation
                    signatureTypeNode
                    stateAtSecondName
                    (skipToIdentifierEnd stateAtSecondName.source (stateAtSecondName.offset + 1))


parseFunctionDeclarationWithSecondName :
    Range
    -> String
    -> Location
    -> Node TypeAnnotation.TypeAnnotation
    -> ParserState
    -> Int
    -> Result String ( Declaration.Declaration, ParserState )
parseFunctionDeclarationWithSecondName nameRange name colonLocation signatureTypeNode stateAtSecondName secondNameEnd =
    let
        secondName =
            String.left (secondNameEnd - stateAtSecondName.offset) (String.dropLeft stateAtSecondName.offset stateAtSecondName.source)

        (Node signatureTypeRange _) =
            signatureTypeNode
    in
    if secondName /= name then
        Err
            ("Function name does not match signature: "
                ++ secondName
                ++ " != "
                ++ name
            )

    else
        finishFunctionDeclaration
            nameRange.start
            { start = { row = stateAtSecondName.row, column = stateAtSecondName.column }
            , end = { row = stateAtSecondName.row, column = stateAtSecondName.column + (secondNameEnd - stateAtSecondName.offset) }
            }
            secondName
            (Just
                (Node
                    { start = nameRange.start
                    , end = signatureTypeRange.end
                    }
                    { name = Node nameRange name
                    , colonLocation = colonLocation
                    , typeAnnotation = signatureTypeNode
                    }
                )
            )
            { source = stateAtSecondName.source
            , offset = secondNameEnd
            , row = stateAtSecondName.row
            , column = stateAtSecondName.column + (secondNameEnd - stateAtSecondName.offset)
            , commentsRev = stateAtSecondName.commentsRev
            }


finishFunctionDeclaration :
    Location
    -> Range
    -> String
    -> Maybe (Node Expression.Signature)
    -> ParserState
    -> Result String ( Declaration.Declaration, ParserState )
finishFunctionDeclaration declarationStart implementationNameRange implementationName maybeSignature state =
    finishFunctionDeclarationOnArguments
        declarationStart
        implementationNameRange
        implementationName
        maybeSignature
        (collectFunctionArguments declarationStart.column [] state)


finishFunctionDeclarationOnArguments :
    Location
    -> Range
    -> String
    -> Maybe (Node Expression.Signature)
    -> Result String ( List (Node Pattern.Pattern), ParserState )
    -> Result String ( Declaration.Declaration, ParserState )
finishFunctionDeclarationOnArguments declarationStart implementationNameRange implementationName maybeSignature argumentsResult =
    case argumentsResult of
        Err error ->
            Err error

        Ok ( arguments, afterArguments ) ->
            let
                stateAtEquals =
                    skipTrivia afterArguments
            in
            case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                "=" ->
                    finishFunctionDeclarationOnBody
                        implementationNameRange
                        implementationName
                        maybeSignature
                        arguments
                        { row = stateAtEquals.row, column = stateAtEquals.column }
                        (parseExpressionNodeAt
                            (declarationStart.column + 1)
                            0
                            { source = stateAtEquals.source
                            , offset = stateAtEquals.offset + 1
                            , row = stateAtEquals.row
                            , column = stateAtEquals.column + 1
                            , commentsRev = stateAtEquals.commentsRev
                            }
                        )

                _ ->
                    Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")


finishFunctionDeclarationOnBody :
    Range
    -> String
    -> Maybe (Node Expression.Signature)
    -> List (Node Pattern.Pattern)
    -> Location
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Declaration.Declaration, ParserState )
finishFunctionDeclarationOnBody implementationNameRange implementationName maybeSignature arguments equalsTokenLocation bodyResult =
    case bodyResult of
        Err error ->
            Err error

        Ok ( Node bodyRange body, remaining ) ->
            Ok
                ( Declaration.FunctionDeclaration
                    { documentation = Nothing
                    , signature = maybeSignature
                    , declaration =
                        Node
                            { start = implementationNameRange.start
                            , end = bodyRange.end
                            }
                            { name = Node implementationNameRange implementationName
                            , arguments = arguments
                            , equalsTokenLocation = equalsTokenLocation
                            , expression = Node bodyRange body
                            }
                    }
                , remaining
                )


collectFunctionArguments :
    Int
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> Result String ( List (Node Pattern.Pattern), ParserState )
collectFunctionArguments indentMin argumentsRev state =
    collectFunctionArgumentsAt indentMin argumentsRev state (skipTrivia state)


collectFunctionArgumentsAt :
    Int
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> ParserState
    -> Result String ( List (Node Pattern.Pattern), ParserState )
collectFunctionArgumentsAt indentMin argumentsRev state stateAtArgument =
    if canStartArgumentPatternAt stateAtArgument.source stateAtArgument.offset then
        collectFunctionArgumentsOnArgument
            indentMin
            argumentsRev
            (parsePatternAtomic indentMin stateAtArgument)

    else
        Ok ( List.reverse argumentsRev, state )


collectFunctionArgumentsOnArgument :
    Int
    -> List (Node Pattern.Pattern)
    -> Result String ( Node Pattern.Pattern, ParserState )
    -> Result String ( List (Node Pattern.Pattern), ParserState )
collectFunctionArgumentsOnArgument indentMin argumentsRev argumentResult =
    case argumentResult of
        Err error ->
            Err error

        Ok ( argument, remaining ) ->
            collectFunctionArguments indentMin (argument :: argumentsRev) remaining



-- TYPE ANNOTATIONS


parseTypeAnnotation :
    Int
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotation indentMin state =
    parseTypeAnnotationOnParam (parseTypeAnnotationFunctionParam indentMin state)


parseTypeAnnotationOnParam :
    Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationOnParam paramResult =
    case paramResult of
        Err error ->
            Err error

        Ok ( paramTypeNode, afterParamType ) ->
            parseTypeAnnotationAtArrow paramTypeNode afterParamType (skipTrivia afterParamType)


parseTypeAnnotationAtArrow :
    Node TypeAnnotation.TypeAnnotation
    -> ParserState
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationAtArrow paramTypeNode afterParamType stateAtArrow =
    if String.left 2 (String.dropLeft stateAtArrow.offset stateAtArrow.source) == "->" then
        parseTypeAnnotationOnReturn
            paramTypeNode
            { row = stateAtArrow.row, column = stateAtArrow.column }
            (parseTypeAnnotation
                (typeAnnotationStartColumn paramTypeNode)
                { source = stateAtArrow.source
                , offset = stateAtArrow.offset + 2
                , row = stateAtArrow.row
                , column = stateAtArrow.column + 2
                , commentsRev = stateAtArrow.commentsRev
                }
            )

    else
        Ok ( paramTypeNode, afterParamType )


typeAnnotationStartColumn : Node TypeAnnotation.TypeAnnotation -> Int
typeAnnotationStartColumn typeAnnotationNode =
    case typeAnnotationNode of
        Node typeAnnotationRange _ ->
            typeAnnotationRange.start.column


parseTypeAnnotationOnReturn :
    Node TypeAnnotation.TypeAnnotation
    -> Location
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationOnReturn paramTypeNode arrowLocation returnResult =
    case returnResult of
        Err error ->
            Err error

        Ok ( returnTypeNode, afterReturn ) ->
            finishFunctionTypeAnnotation paramTypeNode arrowLocation returnTypeNode afterReturn


finishFunctionTypeAnnotation :
    Node TypeAnnotation.TypeAnnotation
    -> Location
    -> Node TypeAnnotation.TypeAnnotation
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
finishFunctionTypeAnnotation paramTypeNode arrowLocation returnTypeNode afterReturn =
    let
        (Node paramTypeRange _) =
            paramTypeNode

        (Node returnTypeRange _) =
            returnTypeNode
    in
    Ok
        ( Node
            { start = paramTypeRange.start
            , end = returnTypeRange.end
            }
            (TypeAnnotation.FunctionTypeAnnotation
                paramTypeNode
                arrowLocation
                returnTypeNode
            )
        , afterReturn
        )


parseTypeAnnotationFunctionParam :
    Int
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationFunctionParam indentMin state =
    parseTypeAnnotationOnTypedArg indentMin (parseTypeAnnotationTypedArg indentMin state)


parseTypeAnnotationOnTypedArg :
    Int
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationOnTypedArg indentMin typedArgResult =
    case typedArgResult of
        Err error ->
            Err error

        Ok ( Node lessAppRange lessAppValue, remaining ) ->
            case lessAppValue of
                TypeAnnotation.Typed typedName [] ->
                    collectTypeApplicationArgs
                        indentMin
                        lessAppRange.start.column
                        typedName
                        (Node lessAppRange lessAppValue)
                        []
                        remaining

                _ ->
                    Ok ( Node lessAppRange lessAppValue, remaining )


collectTypeApplicationArgs :
    Int
    -> Int
    -> Node ( List String, String )
    -> Node TypeAnnotation.TypeAnnotation
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
collectTypeApplicationArgs indentMin lessAppStartColumn typedName lessApp argumentsRev state =
    collectTypeApplicationArgsAt indentMin lessAppStartColumn typedName lessApp argumentsRev state (skipTrivia state)


collectTypeApplicationArgsAt :
    Int
    -> Int
    -> Node ( List String, String )
    -> Node TypeAnnotation.TypeAnnotation
    -> List (Node TypeAnnotation.TypeAnnotation)
    -> ParserState
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
collectTypeApplicationArgsAt indentMin lessAppStartColumn typedName lessApp argumentsRev state stateAtArgument =
    if
        stateAtArgument.column
            > lessAppStartColumn
            && stateAtArgument.column
            > indentMin
            && canStartTypeAnnotationAt stateAtArgument.source stateAtArgument.offset
    then
        case parseTypeAnnotationTypedArg indentMin stateAtArgument of
            Err error ->
                Err error

            Ok ( argument, remaining ) ->
                collectTypeApplicationArgs
                    indentMin
                    lessAppStartColumn
                    typedName
                    lessApp
                    (argument :: argumentsRev)
                    remaining

    else
        let
            (Node lessAppRange _) =
                lessApp

            range =
                case argumentsRev of
                    (Node lastArgumentRange _) :: _ ->
                        { start = lessAppRange.start
                        , end = lastArgumentRange.end
                        }

                    [] ->
                        lessAppRange
        in
        Ok
            ( Node range (TypeAnnotation.Typed typedName (List.reverse argumentsRev))
            , state
            )


parseTypeAnnotationTypedArg :
    Int
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationTypedArg indentMin state =
    parseTypeAnnotationTypedArgAt indentMin (skipTrivia state)


parseTypeAnnotationTypedArgAt :
    Int
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseTypeAnnotationTypedArgAt indentMin stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "(" ->
            parseParenthesizedTypeAnnotation
                indentMin
                { row = stateAtToken.row, column = stateAtToken.column }
                { source = stateAtToken.source
                , offset = stateAtToken.offset + 1
                , row = stateAtToken.row
                , column = stateAtToken.column + 1
                , commentsRev = stateAtToken.commentsRev
                }

        "{" ->
            parseRecordTypeAnnotation
                { row = stateAtToken.row, column = stateAtToken.column }
                { source = stateAtToken.source
                , offset = stateAtToken.offset + 1
                , row = stateAtToken.row
                , column = stateAtToken.column + 1
                , commentsRev = stateAtToken.commentsRev
                }

        first ->
            if isIdentifierStart first then
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    name =
                        String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)
                in
                if isUpperCharacter first then
                    let
                        ( Node qualifiedRange qualifiedName, remaining ) =
                            parseQualifiedNameNode name nameEnd stateAtToken
                    in
                    Ok
                        ( Node qualifiedRange
                            (TypeAnnotation.Typed (Node qualifiedRange qualifiedName) [])
                        , remaining
                        )

                else
                    Ok
                        ( Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                            }
                            (TypeAnnotation.GenericType name)
                        , { source = stateAtToken.source
                          , offset = nameEnd
                          , row = stateAtToken.row
                          , column = stateAtToken.column + nameLength
                          , commentsRev = stateAtToken.commentsRev
                          }
                        )

            else
                Err
                    ("Unsupported type annotation start: '"
                        ++ snippetAt stateAtToken
                        ++ "'."
                    )


parseParenthesizedTypeAnnotation :
    Int
    -> Location
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseParenthesizedTypeAnnotation indentMin openParenLocation state =
    parseParenthesizedTypeAnnotationAt indentMin openParenLocation (skipTrivia state)


parseParenthesizedTypeAnnotationAt :
    Int
    -> Location
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseParenthesizedTypeAnnotationAt indentMin openParenLocation stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( Node
                    { start = openParenLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    TypeAnnotation.Unit
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            case parseTypeAnnotation indentMin stateAtToken of
                Err error ->
                    Err error

                Ok ( firstAnnotation, afterFirst ) ->
                    parseFurtherTypeAnnotations indentMin openParenLocation firstAnnotation [] afterFirst


parseFurtherTypeAnnotations :
    Int
    -> Location
    -> Node TypeAnnotation.TypeAnnotation
    -> List ( Location, Node TypeAnnotation.TypeAnnotation )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseFurtherTypeAnnotations indentMin openParenLocation first restRev state =
    parseFurtherTypeAnnotationsAt indentMin openParenLocation first restRev (skipTrivia state)


parseFurtherTypeAnnotationsAt :
    Int
    -> Location
    -> Node TypeAnnotation.TypeAnnotation
    -> List ( Location, Node TypeAnnotation.TypeAnnotation )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseFurtherTypeAnnotationsAt indentMin openParenLocation first restRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( Node
                    { start = openParenLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (TypeAnnotation.Tupled
                        (SeparatedSyntaxList.NonEmpty first (List.reverse restRev))
                    )
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parseTypeAnnotation
                    indentMin
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( nextAnnotation, remaining ) ->
                    parseFurtherTypeAnnotations
                        indentMin
                        openParenLocation
                        first
                        (( { row = stateAtToken.row, column = stateAtToken.column }, nextAnnotation ) :: restRev)
                        remaining

        _ ->
            Err
                ("Expected ',' or ')' in type annotation, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseRecordTypeAnnotation :
    Location
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseRecordTypeAnnotation openBraceLocation state =
    parseRecordTypeAnnotationAt openBraceLocation (skipTrivia state)


parseRecordTypeAnnotationAt :
    Location
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseRecordTypeAnnotationAt openBraceLocation stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( Node
                    { start = openBraceLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (TypeAnnotation.Record SeparatedSyntaxList.Empty)
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        first ->
            if not (isIdentifierStart first) then
                Err
                    ("Expected record field name, but found '"
                        ++ snippetAt stateAtToken
                        ++ "'."
                    )

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    afterName =
                        { source = stateAtToken.source
                        , offset = nameEnd
                        , row = stateAtToken.row
                        , column = stateAtToken.column + nameLength
                        , commentsRev = stateAtToken.commentsRev
                        }

                    stateAtPipe =
                        skipTrivia afterName
                in
                if isPipeToken stateAtPipe.source stateAtPipe.offset then
                    parseGenericRecordBody
                        openBraceLocation
                        (Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                            }
                            (String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source))
                        )
                        { row = stateAtPipe.row, column = stateAtPipe.column }
                        { source = stateAtPipe.source
                        , offset = stateAtPipe.offset + 1
                        , row = stateAtPipe.row
                        , column = stateAtPipe.column + 1
                        , commentsRev = stateAtPipe.commentsRev
                        }

                else
                    case parseTypeRecordFieldFromName stateAtToken of
                        Err error ->
                            Err error

                        Ok ( firstField, _, afterFirst ) ->
                            finishOrContinueRecord openBraceLocation firstField [] afterFirst


parseGenericRecordBody :
    Location
    -> Node String
    -> Location
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
parseGenericRecordBody openBraceLocation genericName pipeLocation state =
    let
        nodeRecordDefStart =
            { row = pipeLocation.row, column = pipeLocation.column + 1 }

        stateAtToken =
            skipTrivia state
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( Node
                    { start = openBraceLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (TypeAnnotation.GenericRecord
                        genericName
                        pipeLocation
                        (Node
                            { start = nodeRecordDefStart, end = nodeRecordDefStart }
                            SeparatedSyntaxList.Empty
                        )
                    )
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            case parseTypeRecordFieldFromName stateAtToken of
                Err error ->
                    Err error

                Ok ( firstField, fieldEnd, afterField ) ->
                    finishOrContinueGenericRecord
                        openBraceLocation
                        genericName
                        pipeLocation
                        nodeRecordDefStart
                        firstField
                        fieldEnd
                        []
                        afterField


finishOrContinueGenericRecord :
    Location
    -> Node String
    -> Location
    -> Location
    -> Node TypeAnnotation.RecordField
    -> Location
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
finishOrContinueGenericRecord openBraceLocation genericName pipeLocation nodeRecordDefStart firstField lastEnd restRev state =
    finishOrContinueGenericRecordAt openBraceLocation genericName pipeLocation nodeRecordDefStart firstField lastEnd restRev (skipTrivia state)


finishOrContinueGenericRecordAt :
    Location
    -> Node String
    -> Location
    -> Location
    -> Node TypeAnnotation.RecordField
    -> Location
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
finishOrContinueGenericRecordAt openBraceLocation genericName pipeLocation nodeRecordDefStart firstField lastEnd restRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( Node
                    { start = openBraceLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (TypeAnnotation.GenericRecord
                        genericName
                        pipeLocation
                        (Node
                            { start = nodeRecordDefStart, end = lastEnd }
                            (SeparatedSyntaxList.NonEmpty firstField (List.reverse restRev))
                        )
                    )
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parseTypeRecordFieldFromName
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( nextField, nextEnd, afterNext ) ->
                    finishOrContinueGenericRecord
                        openBraceLocation
                        genericName
                        pipeLocation
                        nodeRecordDefStart
                        firstField
                        nextEnd
                        (( { row = stateAtToken.row, column = stateAtToken.column }, nextField ) :: restRev)
                        afterNext

        _ ->
            Err
                ("Expected ',' or '}' in generic record type annotation, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


finishOrContinueRecord :
    Location
    -> Node TypeAnnotation.RecordField
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
finishOrContinueRecord openBraceLocation firstField restRev state =
    finishOrContinueRecordAt openBraceLocation firstField restRev (skipTrivia state)


finishOrContinueRecordAt :
    Location
    -> Node TypeAnnotation.RecordField
    -> List ( Location, Node TypeAnnotation.RecordField )
    -> ParserState
    -> Result String ( Node TypeAnnotation.TypeAnnotation, ParserState )
finishOrContinueRecordAt openBraceLocation firstField restRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( Node
                    { start = openBraceLocation
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (TypeAnnotation.Record
                        (SeparatedSyntaxList.NonEmpty firstField (List.reverse restRev))
                    )
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parseTypeRecordFieldFromName
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( nextField, _, afterNext ) ->
                    finishOrContinueRecord
                        openBraceLocation
                        firstField
                        (( { row = stateAtToken.row, column = stateAtToken.column }, nextField ) :: restRev)
                        afterNext

        _ ->
            Err
                ("Expected ',' or '}' in record type annotation, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseTypeRecordFieldFromName :
    ParserState
    -> Result String ( Node TypeAnnotation.RecordField, Location, ParserState )
parseTypeRecordFieldFromName state =
    parseTypeRecordFieldFromNameAt (skipTrivia state)


parseTypeRecordFieldFromNameAt :
    ParserState
    -> Result String ( Node TypeAnnotation.RecordField, Location, ParserState )
parseTypeRecordFieldFromNameAt stateAtName =
    case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
        first ->
            if not (isIdentifierStart first) then
                Err
                    ("Expected record field name, but found '"
                        ++ snippetAt stateAtName
                        ++ "'."
                    )

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                    nameLength =
                        nameEnd - stateAtName.offset

                    nameRange =
                        { start = { row = stateAtName.row, column = stateAtName.column }
                        , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                        }

                    stateAtColon =
                        skipTrivia
                            { source = stateAtName.source
                            , offset = nameEnd
                            , row = stateAtName.row
                            , column = stateAtName.column + nameLength
                            , commentsRev = stateAtName.commentsRev
                            }
                in
                case String.left 1 (String.dropLeft stateAtColon.offset stateAtColon.source) of
                    ":" ->
                        case
                            parseTypeAnnotation
                                stateAtName.column
                                { source = stateAtColon.source
                                , offset = stateAtColon.offset + 1
                                , row = stateAtColon.row
                                , column = stateAtColon.column + 1
                                , commentsRev = stateAtColon.commentsRev
                                }
                        of
                            Err error ->
                                Err error

                            Ok ( Node fieldTypeRange fieldType, remaining ) ->
                                Ok
                                    ( Node
                                        { start = nameRange.start, end = fieldTypeRange.end }
                                        { fieldName =
                                            Node nameRange (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))
                                        , colonLocation = { row = stateAtColon.row, column = stateAtColon.column }
                                        , fieldType = Node fieldTypeRange fieldType
                                        }
                                    , fieldTypeRange.end
                                    , remaining
                                    )

                    _ ->
                        Err ("Expected ':', but found '" ++ snippetAt stateAtColon ++ "'.")



-- EXPRESSIONS


parseExpressionNodeAt :
    Int
    -> Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseExpressionNodeAt indentMin minPrecedence state =
    parseOperatorsOnApplication indentMin minPrecedence (parseApplication indentMin state)


{-| Continues with the operator parser on an already parsed application.

The application result arrives as an argument instead of being matched directly on the call
that produced it: a `case` on a call evaluates that call once for the branch test and once more
for the branch that reads the matched value, which would double the cost of every nesting level
of the grammar. Matching on a parameter costs two environment lookups instead.

-}
parseOperatorsOnApplication :
    Int
    -> Int
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsOnApplication indentMin minPrecedence applicationResult =
    case applicationResult of
        Err error ->
            Err error

        Ok ( application, afterApplication ) ->
            parseOperators indentMin minPrecedence application afterApplication


parseOperators :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseOperators indentMin minPrecedence left state =
    parseOperatorsAt indentMin minPrecedence left state (skipTrivia state)


parseOperatorsAt :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsAt indentMin minPrecedence left state stateAtOperator =
    parseOperatorsWithLength
        indentMin
        minPrecedence
        left
        state
        stateAtOperator
        (operatorTokenLength stateAtOperator.source stateAtOperator.offset)


parseOperatorsWithLength :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> ParserState
    -> Int
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsWithLength indentMin minPrecedence left state stateAtOperator operatorLength =
    if operatorLength == 0 then
        Ok ( left, state )

    else
        parseOperatorsWithLexeme
            indentMin
            minPrecedence
            left
            state
            stateAtOperator
            operatorLength
            (String.left operatorLength (String.dropLeft stateAtOperator.offset stateAtOperator.source))


parseOperatorsWithLexeme :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> ParserState
    -> Int
    -> String
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsWithLexeme indentMin minPrecedence left state stateAtOperator operatorLength operatorLexeme =
    parseOperatorsWithPrecedence
        indentMin
        minPrecedence
        left
        state
        stateAtOperator
        operatorLength
        operatorLexeme
        (operatorPrecedence operatorLexeme)


parseOperatorsWithPrecedence :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> ParserState
    -> Int
    -> String
    -> Int
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsWithPrecedence indentMin minPrecedence left state stateAtOperator operatorLength operatorLexeme precedence =
    if precedence < 0 || precedence < minPrecedence then
        Ok ( left, state )

    else
        parseOperatorsWithDirection
            indentMin
            minPrecedence
            left
            stateAtOperator
            operatorLength
            operatorLexeme
            precedence
            (operatorDirection operatorLexeme)


parseOperatorsWithDirection :
    Int
    -> Int
    -> Node Expression.Expression
    -> ParserState
    -> Int
    -> String
    -> Int
    -> Infix.InfixDirection
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsWithDirection indentMin minPrecedence left stateAtOperator operatorLength operatorLexeme precedence direction =
    parseOperatorsOnRight
        indentMin
        minPrecedence
        left
        (Node
            { start = { row = stateAtOperator.row, column = stateAtOperator.column }
            , end = { row = stateAtOperator.row, column = stateAtOperator.column + operatorLength }
            }
            operatorLexeme
        )
        direction
        (parseExpressionNodeAt
            indentMin
            (case direction of
                Infix.Left ->
                    precedence + 1

                Infix.Non ->
                    precedence + 1

                Infix.Right ->
                    precedence
            )
            { source = stateAtOperator.source
            , offset = stateAtOperator.offset + operatorLength
            , row = stateAtOperator.row
            , column = stateAtOperator.column + operatorLength
            , commentsRev = stateAtOperator.commentsRev
            }
        )


parseOperatorsOnRight :
    Int
    -> Int
    -> Node Expression.Expression
    -> Node String
    -> Infix.InfixDirection
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
parseOperatorsOnRight indentMin minPrecedence left operatorNode direction rightResult =
    case rightResult of
        Err error ->
            Err error

        Ok ( rightNode, remaining ) ->
            parseOperators
                indentMin
                minPrecedence
                (operatorApplicationNode left operatorNode direction rightNode)
                remaining


operatorApplicationNode :
    Node Expression.Expression
    -> Node String
    -> Infix.InfixDirection
    -> Node Expression.Expression
    -> Node Expression.Expression
operatorApplicationNode left operatorNode direction rightNode =
    let
        (Node leftRange _) =
            left

        (Node rightRange _) =
            rightNode
    in
    Node
        { start = leftRange.start
        , end = rightRange.end
        }
        (Expression.OperatorApplication
            operatorNode
            direction
            left
            rightNode
        )


parseApplication :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseApplication indentMin state =
    parseApplicationArgumentsOnFunction indentMin (parseBasicExpression indentMin state)


parseApplicationArgumentsOnFunction :
    Int
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
parseApplicationArgumentsOnFunction indentMin functionResult =
    case functionResult of
        Err error ->
            Err error

        Ok ( function, afterFunction ) ->
            parseApplicationArguments indentMin function [] afterFunction


parseApplicationArguments :
    Int
    -> Node Expression.Expression
    -> List (Node Expression.Expression)
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseApplicationArguments indentMin function argumentsRev state =
    parseApplicationArgumentsAt indentMin function argumentsRev state (skipTrivia state)


parseApplicationArgumentsAt :
    Int
    -> Node Expression.Expression
    -> List (Node Expression.Expression)
    -> ParserState
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseApplicationArgumentsAt indentMin function argumentsRev state stateAtArgument =
    if
        stateAtArgument.column
            > indentMin
            && canStartArgumentExpressionAt stateAtArgument.source stateAtArgument.offset
    then
        parseApplicationArgumentsOnArgument
            indentMin
            function
            argumentsRev
            (parseBasicExpression indentMin stateAtArgument)

    else
        case argumentsRev of
            [] ->
                Ok ( function, state )

            (Node lastArgumentRange _) :: _ ->
                let
                    (Node functionRange _) =
                        function
                in
                Ok
                    ( Node
                        { start = functionRange.start
                        , end = lastArgumentRange.end
                        }
                        (Expression.Application function (List.reverse argumentsRev))
                    , state
                    )


parseApplicationArgumentsOnArgument :
    Int
    -> Node Expression.Expression
    -> List (Node Expression.Expression)
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
parseApplicationArgumentsOnArgument indentMin function argumentsRev argumentResult =
    case argumentResult of
        Err error ->
            Err error

        Ok ( argument, remaining ) ->
            parseApplicationArguments indentMin function (argument :: argumentsRev) remaining


parseBasicExpression :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseBasicExpression indentMin state =
    parseRecordAccessesOnAtomic indentMin (parseAtomicExpression indentMin state)


parseRecordAccessesOnAtomic :
    Int
    -> Result String ( Node Expression.Expression, ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
parseRecordAccessesOnAtomic indentMin atomicResult =
    case atomicResult of
        Err error ->
            Err error

        Ok ( atomic, afterAtomic ) ->
            parseRecordAccesses indentMin atomic afterAtomic


{-| Consumes any number of `.field` suffixes directly attached to the expression parsed so far.
The dot has to follow the expression without any separating trivia, therefore this inspects the
source at the exact end of that expression instead of skipping trivia first.
-}
parseRecordAccesses :
    Int
    -> Node Expression.Expression
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseRecordAccesses indentMin record state =
    if String.left 1 (String.dropLeft state.offset state.source) == "." then
        case String.left 1 (String.dropLeft (state.offset + 1) state.source) of
            fieldFirst ->
                if isLowerCharacter fieldFirst then
                    let
                        fieldEnd =
                            skipToIdentifierEnd state.source (state.offset + 2)

                        fieldLength =
                            fieldEnd - (state.offset + 1)

                        (Node recordRange _) =
                            record

                        fieldEndColumn =
                            state.column + 1 + fieldLength
                    in
                    parseRecordAccesses
                        indentMin
                        (Node
                            { start = recordRange.start
                            , end = { row = state.row, column = fieldEndColumn }
                            }
                            (Expression.RecordAccess
                                record
                                (Node
                                    { start = { row = state.row, column = state.column + 1 }
                                    , end = { row = state.row, column = fieldEndColumn }
                                    }
                                    (String.left (fieldEnd - (state.offset + 1)) (String.dropLeft (state.offset + 1) state.source))
                                )
                            )
                        )
                        { source = state.source
                        , offset = fieldEnd
                        , row = state.row
                        , column = fieldEndColumn
                        , commentsRev = state.commentsRev
                        }

                else
                    Ok ( record, state )

    else
        Ok ( record, state )


parseAtomicExpression :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseAtomicExpression indentMin state =
    parseAtomicExpressionAt indentMin (skipTrivia state)


parseAtomicExpressionAt :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseAtomicExpressionAt indentMin stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "\"" ->
            if String.left 3 (String.dropLeft stateAtToken.offset stateAtToken.source) == "\"\"\"" then
                parseTripleQuotedStringExpression stateAtToken

            else
                parseStringExpression stateAtToken

        "'" ->
            parseCharExpression stateAtToken

        "[" ->
            parseList indentMin stateAtToken

        "(" ->
            parseParenthesizedOrTuple indentMin stateAtToken

        "{" ->
            parseRecord indentMin stateAtToken

        "\\" ->
            parseLambda indentMin stateAtToken

        "-" ->
            case String.left 1 (String.dropLeft (stateAtToken.offset + 1) stateAtToken.source) of
                ">" ->
                    Err "Failed to parse expression: Unexpected token '->'."

                _ ->
                    if minusIsOperatorAt stateAtToken.source stateAtToken.offset then
                        Err "Failed to parse expression: Unexpected token '-'."

                    else
                        parseNegation indentMin stateAtToken

        "." ->
            case String.left 1 (String.dropLeft (stateAtToken.offset + 1) stateAtToken.source) of
                "." ->
                    Err "Failed to parse expression: Unexpected token '..'."

                next ->
                    if isOperatorChar next then
                        Err ("Failed to parse expression: Unexpected token '" ++ snippetAt stateAtToken ++ "'.")

                    else
                        parseRecordAccessFunction stateAtToken

        first ->
            if isDigit first then
                parseNumberExpressionAt
                    stateAtToken
                    (numberEnd stateAtToken.source first stateAtToken.offset)

            else if isIdentifierStart first then
                parseNameExpressionAt
                    indentMin
                    stateAtToken
                    first
                    (skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1))

            else
                Err ("Failed to parse expression: Unexpected token '" ++ snippetAt stateAtToken ++ "'.")


parseNumberExpressionAt :
    ParserState
    -> Int
    -> Result String ( Node Expression.Expression, ParserState )
parseNumberExpressionAt stateAtToken literalEnd =
    Ok
        ( Node
            { start = { row = stateAtToken.row, column = stateAtToken.column }
            , end = { row = stateAtToken.row, column = stateAtToken.column + (literalEnd - stateAtToken.offset) }
            }
            (parseNumber (String.left (literalEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)))
        , { source = stateAtToken.source
          , offset = literalEnd
          , row = stateAtToken.row
          , column = stateAtToken.column + (literalEnd - stateAtToken.offset)
          , commentsRev = stateAtToken.commentsRev
          }
        )


parseNameExpressionAt :
    Int
    -> ParserState
    -> String
    -> Int
    -> Result String ( Node Expression.Expression, ParserState )
parseNameExpressionAt indentMin stateAtToken first nameEnd =
    case String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "let" ->
            parseLetBlock indentMin stateAtToken

        "if" ->
            parseIfBlock indentMin stateAtToken

        "case" ->
            parseCaseBlock indentMin stateAtToken

        name ->
            if isUpperCharacter first then
                finishQualifiedNameExpression (parseQualifiedNameNode name nameEnd stateAtToken)

            else
                Ok
                    ( Node
                        { start = { row = stateAtToken.row, column = stateAtToken.column }
                        , end = { row = stateAtToken.row, column = stateAtToken.column + (nameEnd - stateAtToken.offset) }
                        }
                        (Expression.Identifier [] name)
                    , { source = stateAtToken.source
                      , offset = nameEnd
                      , row = stateAtToken.row
                      , column = stateAtToken.column + (nameEnd - stateAtToken.offset)
                      , commentsRev = stateAtToken.commentsRev
                      }
                    )


finishQualifiedNameExpression :
    ( Node ( List String, String ), ParserState )
    -> Result String ( Node Expression.Expression, ParserState )
finishQualifiedNameExpression qualifiedNameResult =
    case qualifiedNameResult of
        ( Node qualifiedRange ( moduleNames, qualifiedName ), remaining ) ->
            Ok
                ( Node qualifiedRange (Expression.Identifier moduleNames qualifiedName)
                , remaining
                )


parseNegation :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseNegation indentMin state =
    case
        parseBasicExpression
            indentMin
            { source = state.source
            , offset = state.offset + 1
            , row = state.row
            , column = state.column + 1
            , commentsRev = state.commentsRev
            }
    of
        Err error ->
            Err error

        Ok ( Node negatedRange negated, remaining ) ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = negatedRange.end
                    }
                    (Expression.Negation (Node negatedRange negated))
                , remaining
                )


parseRecordAccessFunction : ParserState -> Result String ( Node Expression.Expression, ParserState )
parseRecordAccessFunction state =
    let
        stateAtField =
            { source = state.source
            , offset = state.offset + 1
            , row = state.row
            , column = state.column + 1
            , commentsRev = state.commentsRev
            }
    in
    case String.left 1 (String.dropLeft stateAtField.offset stateAtField.source) of
        first ->
            if isLowerCharacter first then
                let
                    fieldEnd =
                        skipToIdentifierEnd stateAtField.source (stateAtField.offset + 1)

                    fieldLength =
                        fieldEnd - stateAtField.offset
                in
                Ok
                    ( Node
                        { start = { row = state.row, column = state.column }
                        , end = { row = stateAtField.row, column = stateAtField.column + fieldLength }
                        }
                        (Expression.RecordAccessFunction
                            ("." ++ String.left (fieldEnd - stateAtField.offset) (String.dropLeft stateAtField.offset stateAtField.source))
                        )
                    , { source = stateAtField.source
                      , offset = fieldEnd
                      , row = stateAtField.row
                      , column = stateAtField.column + fieldLength
                      , commentsRev = stateAtField.commentsRev
                      }
                    )

            else
                Err
                    ("Expected a record field name after '.', but found '"
                        ++ snippetAt stateAtField
                        ++ "'."
                    )


parseStringExpression : ParserState -> Result String ( Node Expression.Expression, ParserState )
parseStringExpression state =
    parseStringExpressionOnLiteral
        state
        (consumeLiteral
            DoubleQuoteTermination
            state.source
            state.row
            state.column
            (state.offset + 1)
            state.row
            (state.column + 1)
            []
            []
        )


parseStringExpressionOnLiteral :
    ParserState
    -> Result String ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
parseStringExpressionOnLiteral state literalResult =
    case literalResult of
        Err error ->
            Err error

        Ok consumed ->
            finishStringExpression state consumed


finishStringExpression :
    ParserState
    -> ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
finishStringExpression state consumed =
    Ok
        ( Node
            { start = { row = state.row, column = state.column }
            , end = { row = consumed.endRow, column = consumed.endColumn }
            }
            (Expression.StringLiteral consumed.decoded (Just consumed.raw))
        , { source = state.source
          , offset = consumed.endOffset
          , row = consumed.endRow
          , column = consumed.endColumn
          , commentsRev = state.commentsRev
          }
        )


parseTripleQuotedStringExpression : ParserState -> Result String ( Node Expression.Expression, ParserState )
parseTripleQuotedStringExpression state =
    parseTripleQuotedStringExpressionOnLiteral
        state
        (consumeLiteral
            TripleQuoteTermination
            state.source
            state.row
            state.column
            (state.offset + 3)
            state.row
            (state.column + 3)
            []
            []
        )


parseTripleQuotedStringExpressionOnLiteral :
    ParserState
    -> Result String ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
parseTripleQuotedStringExpressionOnLiteral state literalResult =
    case literalResult of
        Err error ->
            Err error

        Ok consumed ->
            finishTripleQuotedStringExpression state consumed


finishTripleQuotedStringExpression :
    ParserState
    -> ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
finishTripleQuotedStringExpression state consumed =
    Ok
        ( Node
            { start = { row = state.row, column = state.column }
            , end = { row = consumed.endRow, column = consumed.endColumn }
            }
            (Expression.MultilineStringLiteral
                consumed.decoded
                (Just (String.split "\n" consumed.raw))
            )
        , { source = state.source
          , offset = consumed.endOffset
          , row = consumed.endRow
          , column = consumed.endColumn
          , commentsRev = state.commentsRev
          }
        )


parseCharExpression : ParserState -> Result String ( Node Expression.Expression, ParserState )
parseCharExpression state =
    parseCharExpressionOnLiteral
        state
        (consumeLiteral
            SingleQuoteTermination
            state.source
            state.row
            state.column
            (state.offset + 1)
            state.row
            (state.column + 1)
            []
            []
        )


parseCharExpressionOnLiteral :
    ParserState
    -> Result String ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
parseCharExpressionOnLiteral state literalResult =
    case literalResult of
        Err error ->
            Err error

        Ok consumed ->
            finishCharExpression state consumed


finishCharExpression :
    ParserState
    -> ConsumedLiteral
    -> Result String ( Node Expression.Expression, ParserState )
finishCharExpression state consumed =
    case String.toList consumed.decoded of
        [ char ] ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = consumed.endRow, column = consumed.endColumn }
                    }
                    (Expression.CharLiteral (Char.toCode char))
                , { source = state.source
                  , offset = consumed.endOffset
                  , row = consumed.endRow
                  , column = consumed.endColumn
                  , commentsRev = state.commentsRev
                  }
                )

        _ ->
            Err ("Invalid character literal '" ++ consumed.decoded ++ "'.")


parseQualifiedNameNode : String -> Int -> ParserState -> ( Node ( List String, String ), ParserState )
parseQualifiedNameNode name nameEnd state =
    let
        nameLength =
            nameEnd - state.offset
    in
    parseQualifiedNameRest
        { row = state.row, column = state.column }
        []
        name
        state.row
        (state.column + nameLength)
        { source = state.source
        , offset = nameEnd
        , row = state.row
        , column = state.column + nameLength
        , commentsRev = state.commentsRev
        }


parseQualifiedNameRest :
    Location
    -> List String
    -> String
    -> Int
    -> Int
    -> ParserState
    -> ( Node ( List String, String ), ParserState )
parseQualifiedNameRest start moduleNamesRev currentName endRow endColumn state =
    if startsWithUpper currentName then
        if isDotToken state.source state.offset then
            let
                stateAtName =
                    { source = state.source
                    , offset = state.offset + 1
                    , row = state.row
                    , column = state.column + 1
                    , commentsRev = state.commentsRev
                    }
            in
            case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
                first ->
                    if isIdentifierStart first then
                        let
                            nextNameEnd =
                                skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                            nextNameLength =
                                nextNameEnd - stateAtName.offset
                        in
                        parseQualifiedNameRest
                            start
                            (currentName :: moduleNamesRev)
                            (String.left (nextNameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))
                            stateAtName.row
                            (stateAtName.column + nextNameLength)
                            { source = stateAtName.source
                            , offset = nextNameEnd
                            , row = stateAtName.row
                            , column = stateAtName.column + nextNameLength
                            , commentsRev = stateAtName.commentsRev
                            }

                    else
                        finishQualifiedName start moduleNamesRev currentName endRow endColumn state

        else
            finishQualifiedName start moduleNamesRev currentName endRow endColumn state

    else
        finishQualifiedName start moduleNamesRev currentName endRow endColumn state


finishQualifiedName :
    Location
    -> List String
    -> String
    -> Int
    -> Int
    -> ParserState
    -> ( Node ( List String, String ), ParserState )
finishQualifiedName start moduleNamesRev currentName endRow endColumn state =
    ( Node
        { start = start, end = { row = endRow, column = endColumn } }
        ( List.reverse moduleNamesRev, currentName )
    , state
    )


parseList : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseList indentMin state =
    let
        stateAtToken =
            skipTrivia
                { source = state.source
                , offset = state.offset + 1
                , row = state.row
                , column = state.column + 1
                , commentsRev = state.commentsRev
                }
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "]" ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (Expression.ListExpr SeparatedSyntaxList.Empty)
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            case parseExpressionNodeAt indentMin 0 stateAtToken of
                Err error ->
                    Err error

                Ok ( first, afterFirst ) ->
                    case parseFurtherListElements indentMin afterFirst [] of
                        Err error ->
                            Err error

                        Ok ( further, afterClose ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = { row = afterClose.row, column = afterClose.column }
                                    }
                                    (Expression.ListExpr (SeparatedSyntaxList.NonEmpty first further))
                                , afterClose
                                )


parseFurtherListElements :
    Int
    -> ParserState
    -> List ( Location, Node Expression.Expression )
    -> Result String ( List ( Location, Node Expression.Expression ), ParserState )
parseFurtherListElements indentMin state furtherRev =
    parseFurtherListElementsAt indentMin state furtherRev (skipTrivia state)


parseFurtherListElementsAt :
    Int
    -> ParserState
    -> List ( Location, Node Expression.Expression )
    -> ParserState
    -> Result String ( List ( Location, Node Expression.Expression ), ParserState )
parseFurtherListElementsAt indentMin state furtherRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "]" ->
            Ok
                ( List.reverse furtherRev
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parseExpressionNodeAt
                    indentMin
                    0
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( expression, remaining ) ->
                    parseFurtherListElements
                        indentMin
                        remaining
                        (( { row = stateAtToken.row, column = stateAtToken.column }, expression ) :: furtherRev)

        _ ->
            Err
                ("Expected ',' or a closing delimiter, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseParenthesizedOrTuple :
    Int
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseParenthesizedOrTuple indentMin state =
    let
        stateAtToken =
            skipTrivia
                { source = state.source
                , offset = state.offset + 1
                , row = state.row
                , column = state.column + 1
                , commentsRev = state.commentsRev
                }
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    Expression.UnitExpr
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            let
                operatorLength =
                    operatorTokenLength stateAtToken.source stateAtToken.offset
            in
            if operatorLength > 0 then
                let
                    stateAtClose =
                        skipTrivia
                            { source = stateAtToken.source
                            , offset = stateAtToken.offset + operatorLength
                            , row = stateAtToken.row
                            , column = stateAtToken.column + operatorLength
                            , commentsRev = stateAtToken.commentsRev
                            }
                in
                if String.left 1 (String.dropLeft stateAtClose.offset stateAtClose.source) == ")" then
                    Ok
                        ( Node
                            { start = { row = state.row, column = state.column }
                            , end = { row = stateAtClose.row, column = stateAtClose.column + 1 }
                            }
                            (Expression.PrefixOperator
                                (String.left operatorLength (String.dropLeft stateAtToken.offset stateAtToken.source))
                            )
                        , { source = stateAtClose.source
                          , offset = stateAtClose.offset + 1
                          , row = stateAtClose.row
                          , column = stateAtClose.column + 1
                          , commentsRev = stateAtClose.commentsRev
                          }
                        )

                else
                    parseNonEmptyParenthesized indentMin state stateAtToken

            else
                parseNonEmptyParenthesized indentMin state stateAtToken


parseNonEmptyParenthesized :
    Int
    -> ParserState
    -> ParserState
    -> Result String ( Node Expression.Expression, ParserState )
parseNonEmptyParenthesized indentMin openState state =
    case parseExpressionNodeAt indentMin 0 state of
        Err error ->
            Err error

        Ok ( first, afterFirst ) ->
            case parseFurtherTupleElements indentMin afterFirst [] of
                Err error ->
                    Err error

                Ok ( further, afterClose ) ->
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
                            { start = { row = openState.row, column = openState.column }
                            , end = { row = afterClose.row, column = afterClose.column }
                            }
                            expression
                        , afterClose
                        )


parseFurtherTupleElements :
    Int
    -> ParserState
    -> List ( Location, Node Expression.Expression )
    -> Result String ( List ( Location, Node Expression.Expression ), ParserState )
parseFurtherTupleElements indentMin state furtherRev =
    parseFurtherTupleElementsAt indentMin state furtherRev (skipTrivia state)


parseFurtherTupleElementsAt :
    Int
    -> ParserState
    -> List ( Location, Node Expression.Expression )
    -> ParserState
    -> Result String ( List ( Location, Node Expression.Expression ), ParserState )
parseFurtherTupleElementsAt indentMin state furtherRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( List.reverse furtherRev
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parseExpressionNodeAt
                    indentMin
                    0
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( expression, remaining ) ->
                    parseFurtherTupleElements
                        indentMin
                        remaining
                        (( { row = stateAtToken.row, column = stateAtToken.column }, expression ) :: furtherRev)

        _ ->
            Err
                ("Expected ',' or a closing delimiter, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseIfBlock : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseIfBlock indentMin state =
    let
        ifTokenLocation =
            { row = state.row, column = state.column }

        branchIndentMin =
            min indentMin state.column

        afterIf =
            { source = state.source
            , offset = state.offset + 2
            , row = state.row
            , column = state.column + 2
            , commentsRev = state.commentsRev
            }
    in
    case parseExpressionNodeAt branchIndentMin 0 afterIf of
        Err error ->
            Err error

        Ok ( condition, afterCondition ) ->
            case consumeKeyword "then" 4 afterCondition of
                Err error ->
                    Err error

                Ok ( thenTokenLocation, afterThen ) ->
                    case parseExpressionNodeAt branchIndentMin 0 afterThen of
                        Err error ->
                            Err error

                        Ok ( thenBranch, afterThenBranch ) ->
                            case consumeKeyword "else" 4 afterThenBranch of
                                Err error ->
                                    Err error

                                Ok ( elseTokenLocation, afterElse ) ->
                                    case parseExpressionNodeAt branchIndentMin 0 afterElse of
                                        Err error ->
                                            Err error

                                        Ok ( Node elseBranchRange elseBranch, remaining ) ->
                                            Ok
                                                ( Node
                                                    { start = ifTokenLocation
                                                    , end = elseBranchRange.end
                                                    }
                                                    (Expression.IfBlock
                                                        ifTokenLocation
                                                        condition
                                                        thenTokenLocation
                                                        thenBranch
                                                        elseTokenLocation
                                                        (Node elseBranchRange elseBranch)
                                                    )
                                                , remaining
                                                )


parseLambda : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseLambda indentMin state =
    case
        parseLambdaArguments
            indentMin
            { source = state.source
            , offset = state.offset + 1
            , row = state.row
            , column = state.column + 1
            , commentsRev = state.commentsRev
            }
            []
    of
        Err error ->
            Err error

        Ok ( arguments, arrowLocation, afterArrow ) ->
            case arguments of
                [] ->
                    Err "Expected at least one argument in lambda expression."

                _ ->
                    case parseExpressionNodeAt indentMin 0 afterArrow of
                        Err error ->
                            Err error

                        Ok ( Node bodyRange body, remaining ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = bodyRange.end
                                    }
                                    (Expression.LambdaExpression
                                        { backslashLocation = { row = state.row, column = state.column }
                                        , arguments = arguments
                                        , arrowLocation = arrowLocation
                                        , expression = Node bodyRange body
                                        }
                                    )
                                , remaining
                                )


parseLambdaArguments :
    Int
    -> ParserState
    -> List (Node Pattern.Pattern)
    -> Result String ( List (Node Pattern.Pattern), Location, ParserState )
parseLambdaArguments indentMin state argumentsRev =
    parseLambdaArgumentsAt indentMin state argumentsRev (skipTrivia state)


parseLambdaArgumentsAt :
    Int
    -> ParserState
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> Result String ( List (Node Pattern.Pattern), Location, ParserState )
parseLambdaArgumentsAt indentMin state argumentsRev stateAtToken =
    if String.left 2 (String.dropLeft stateAtToken.offset stateAtToken.source) == "->" then
        Ok
            ( List.reverse argumentsRev
            , { row = stateAtToken.row, column = stateAtToken.column }
            , { source = stateAtToken.source
              , offset = stateAtToken.offset + 2
              , row = stateAtToken.row
              , column = stateAtToken.column + 2
              , commentsRev = stateAtToken.commentsRev
              }
            )

    else
        case parsePatternNodeAt indentMin stateAtToken of
            Err error ->
                Err error

            Ok ( argument, remaining ) ->
                parseLambdaArguments indentMin remaining (argument :: argumentsRev)


parseLetBlock : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseLetBlock indentMin state =
    case
        parseLetDeclarations
            (min indentMin state.column)
            { source = state.source
            , offset = state.offset + 3
            , row = state.row
            , column = state.column + 3
            , commentsRev = state.commentsRev
            }
            []
    of
        Err error ->
            Err error

        Ok ( declarations, inTokenLocation, afterIn ) ->
            case declarations of
                [] ->
                    Err "Expected at least one declaration in let expression."

                _ ->
                    case parseExpressionNodeAt indentMin 0 afterIn of
                        Err error ->
                            Err error

                        Ok ( Node bodyRange body, remaining ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = bodyRange.end
                                    }
                                    (Expression.LetExpression
                                        { letTokenLocation = { row = state.row, column = state.column }
                                        , declarations = declarations
                                        , inTokenLocation = inTokenLocation
                                        , expression = Node bodyRange body
                                        }
                                    )
                                , remaining
                                )


parseLetDeclarations :
    Int
    -> ParserState
    -> List (Node Expression.LetDeclaration)
    -> Result String ( List (Node Expression.LetDeclaration), Location, ParserState )
parseLetDeclarations indentMin state declarationsRev =
    parseLetDeclarationsAt indentMin state declarationsRev (skipTrivia state)


parseLetDeclarationsAt :
    Int
    -> ParserState
    -> List (Node Expression.LetDeclaration)
    -> ParserState
    -> Result String ( List (Node Expression.LetDeclaration), Location, ParserState )
parseLetDeclarationsAt indentMin state declarationsRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1) - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)
                            == "in"
                       )
            then
                Ok
                    ( List.reverse declarationsRev
                    , { row = stateAtToken.row, column = stateAtToken.column }
                    , { source = stateAtToken.source
                      , offset = stateAtToken.offset + 2
                      , row = stateAtToken.row
                      , column = stateAtToken.column + 2
                      , commentsRev = stateAtToken.commentsRev
                      }
                    )

            else if stateAtToken.column <= indentMin then
                Err
                    ("Expected 'in' in let expression, but found '"
                        ++ snippetAt stateAtToken
                        ++ "'."
                    )

            else
                case parseLetDeclaration stateAtToken.column stateAtToken of
                    Err error ->
                        Err error

                    Ok ( declaration, remaining ) ->
                        parseLetDeclarations indentMin remaining (declaration :: declarationsRev)


parseLetDeclaration :
    Int
    -> ParserState
    -> Result String ( Node Expression.LetDeclaration, ParserState )
parseLetDeclaration declarationIndent state =
    parseLetDeclarationAt declarationIndent (skipTrivia state)


parseLetDeclarationAt :
    Int
    -> ParserState
    -> Result String ( Node Expression.LetDeclaration, ParserState )
parseLetDeclarationAt declarationIndent stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        first ->
            if isIdentifierStart first then
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    name =
                        String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)

                    nameRange =
                        { start = { row = stateAtToken.row, column = stateAtToken.column }
                        , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                        }

                    afterName =
                        { source = stateAtToken.source
                        , offset = nameEnd
                        , row = stateAtToken.row
                        , column = stateAtToken.column + nameLength
                        , commentsRev = stateAtToken.commentsRev
                        }

                    stateAtColon =
                        skipTrivia afterName
                in
                if isColonToken stateAtColon.source stateAtColon.offset then
                    case
                        parseTypeAnnotation
                            declarationIndent
                            { source = stateAtColon.source
                            , offset = stateAtColon.offset + 1
                            , row = stateAtColon.row
                            , column = stateAtColon.column + 1
                            , commentsRev = stateAtColon.commentsRev
                            }
                    of
                        Err error ->
                            Err error

                        Ok ( Node typeAnnotationRange typeAnnotation, afterTypeAnnotation ) ->
                            let
                                stateAtImplementationName =
                                    skipTrivia afterTypeAnnotation
                            in
                            case String.left 1 (String.dropLeft stateAtImplementationName.offset stateAtImplementationName.source) of
                                implementationFirst ->
                                    if not (isIdentifierStart implementationFirst) then
                                        Err
                                            ("Expected function name after signature, but found '"
                                                ++ snippetAt stateAtImplementationName
                                                ++ "'."
                                            )

                                    else
                                        let
                                            implementationNameEnd =
                                                skipToIdentifierEnd stateAtImplementationName.source (stateAtImplementationName.offset + 1)

                                            implementationNameLength =
                                                implementationNameEnd - stateAtImplementationName.offset

                                            implementationName =
                                                String.left (implementationNameEnd - stateAtImplementationName.offset) (String.dropLeft stateAtImplementationName.offset stateAtImplementationName.source)
                                        in
                                        if implementationName /= name then
                                            Err
                                                ("Function name does not match signature: "
                                                    ++ implementationName
                                                    ++ " != "
                                                    ++ name
                                                )

                                        else
                                            finishLetFunctionDeclaration
                                                declarationIndent
                                                nameRange.start
                                                { start = { row = stateAtImplementationName.row, column = stateAtImplementationName.column }
                                                , end = { row = stateAtImplementationName.row, column = stateAtImplementationName.column + implementationNameLength }
                                                }
                                                implementationName
                                                (Just
                                                    (Node
                                                        { start = nameRange.start
                                                        , end = typeAnnotationRange.end
                                                        }
                                                        { name = Node nameRange name
                                                        , colonLocation = { row = stateAtColon.row, column = stateAtColon.column }
                                                        , typeAnnotation = Node typeAnnotationRange typeAnnotation
                                                        }
                                                    )
                                                )
                                                { source = stateAtImplementationName.source
                                                , offset = implementationNameEnd
                                                , row = stateAtImplementationName.row
                                                , column = stateAtImplementationName.column + implementationNameLength
                                                , commentsRev = stateAtImplementationName.commentsRev
                                                }

                else
                    finishLetFunctionDeclaration declarationIndent nameRange.start nameRange name Nothing afterName

            else
                case parsePatternNodeAt declarationIndent stateAtToken of
                    Err error ->
                        Err error

                    Ok ( Node patternRange pattern, afterPattern ) ->
                        let
                            stateAtEquals =
                                skipTrivia afterPattern
                        in
                        case String.left 1 (String.dropLeft stateAtEquals.offset stateAtEquals.source) of
                            "=" ->
                                case
                                    parseExpressionNodeAt
                                        declarationIndent
                                        0
                                        { source = stateAtEquals.source
                                        , offset = stateAtEquals.offset + 1
                                        , row = stateAtEquals.row
                                        , column = stateAtEquals.column + 1
                                        , commentsRev = stateAtEquals.commentsRev
                                        }
                                of
                                    Err error ->
                                        Err error

                                    Ok ( Node bodyRange body, remaining ) ->
                                        Ok
                                            ( Node
                                                { start = patternRange.start
                                                , end = bodyRange.end
                                                }
                                                (Expression.LetDestructuring
                                                    (Node patternRange pattern)
                                                    { row = stateAtEquals.row, column = stateAtEquals.column }
                                                    (Node bodyRange body)
                                                )
                                            , remaining
                                            )

                            _ ->
                                Err ("Expected '=', but found '" ++ snippetAt stateAtEquals ++ "'.")


finishLetFunctionDeclaration :
    Int
    -> Location
    -> Range
    -> String
    -> Maybe (Node Expression.Signature)
    -> ParserState
    -> Result String ( Node Expression.LetDeclaration, ParserState )
finishLetFunctionDeclaration declarationIndent declarationStart implementationNameRange implementationName maybeSignature state =
    case parsePatternsUntilEqual declarationIndent state [] of
        Err error ->
            Err error

        Ok ( arguments, equalsLocation, afterEqual ) ->
            case parseExpressionNodeAt declarationIndent 0 afterEqual of
                Err error ->
                    Err error

                Ok ( Node bodyRange body, remaining ) ->
                    Ok
                        ( Node
                            { start = declarationStart, end = bodyRange.end }
                            (Expression.LetFunction
                                { documentation = Nothing
                                , signature = maybeSignature
                                , declaration =
                                    Node
                                        { start = implementationNameRange.start, end = bodyRange.end }
                                        { name = Node implementationNameRange implementationName
                                        , arguments = arguments
                                        , equalsTokenLocation = equalsLocation
                                        , expression = Node bodyRange body
                                        }
                                }
                            )
                        , remaining
                        )


parsePatternsUntilEqual :
    Int
    -> ParserState
    -> List (Node Pattern.Pattern)
    -> Result String ( List (Node Pattern.Pattern), Location, ParserState )
parsePatternsUntilEqual indentMin state patternsRev =
    parsePatternsUntilEqualAt indentMin state patternsRev (skipTrivia state)


parsePatternsUntilEqualAt :
    Int
    -> ParserState
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> Result String ( List (Node Pattern.Pattern), Location, ParserState )
parsePatternsUntilEqualAt indentMin state patternsRev stateAtToken =
    if isEqualsToken stateAtToken.source stateAtToken.offset then
        Ok
            ( List.reverse patternsRev
            , { row = stateAtToken.row, column = stateAtToken.column }
            , { source = stateAtToken.source
              , offset = stateAtToken.offset + 1
              , row = stateAtToken.row
              , column = stateAtToken.column + 1
              , commentsRev = stateAtToken.commentsRev
              }
            )

    else
        case parsePatternNodeAt indentMin stateAtToken of
            Err error ->
                Err error

            Ok ( pattern, remaining ) ->
                parsePatternsUntilEqual indentMin remaining (pattern :: patternsRev)


parseCaseBlock : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseCaseBlock indentMin state =
    let
        caseTokenLocation =
            { row = state.row, column = state.column }
    in
    case
        parseExpressionNodeAt
            state.column
            0
            { source = state.source
            , offset = state.offset + 4
            , row = state.row
            , column = state.column + 4
            , commentsRev = state.commentsRev
            }
    of
        Err error ->
            Err error

        Ok ( subject, afterSubject ) ->
            case consumeKeyword "of" 2 afterSubject of
                Err error ->
                    Err error

                Ok ( ofTokenLocation, afterOf ) ->
                    let
                        stateAtFirstBranch =
                            skipTrivia afterOf
                    in
                    case
                        parseCaseBranches
                            (min stateAtFirstBranch.column (state.column + 1))
                            stateAtFirstBranch.column
                            afterOf
                            []
                    of
                        Err error ->
                            Err error

                        Ok ( branchesRev, remaining ) ->
                            case branchesRev of
                                [] ->
                                    Err "Expected at least one case branch after 'of'."

                                lastBranch :: _ ->
                                    let
                                        (Node lastExpressionRange _) =
                                            lastBranch.expression
                                    in
                                    Ok
                                        ( Node
                                            { start = caseTokenLocation
                                            , end = lastExpressionRange.end
                                            }
                                            (Expression.CaseExpression
                                                { caseTokenLocation = caseTokenLocation
                                                , expression = subject
                                                , ofTokenLocation = ofTokenLocation
                                                , cases = List.reverse branchesRev
                                                }
                                            )
                                        , remaining
                                        )


parseCaseBranches :
    Int
    -> Int
    -> ParserState
    -> List Expression.Case
    -> Result String ( List Expression.Case, ParserState )
parseCaseBranches lowerBound branchIndent state branchesRev =
    parseCaseBranchesAt lowerBound branchIndent state branchesRev (skipTrivia state)


parseCaseBranchesAt :
    Int
    -> Int
    -> ParserState
    -> List Expression.Case
    -> ParserState
    -> Result String ( List Expression.Case, ParserState )
parseCaseBranchesAt lowerBound branchIndent state branchesRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "" ->
            Ok ( branchesRev, state )

        _ ->
            if
                stateAtToken.column
                    < lowerBound
                    || isClosingAt stateAtToken.source stateAtToken.offset
            then
                Ok ( branchesRev, state )

            else
                case parseCaseBranch branchIndent stateAtToken of
                    Err error ->
                        Err error

                    Ok ( branch, remaining ) ->
                        parseCaseBranches lowerBound branchIndent remaining (branch :: branchesRev)


parseCaseBranch : Int -> ParserState -> Result String ( Expression.Case, ParserState )
parseCaseBranch branchIndent state =
    case parsePatternNodeAt branchIndent state of
        Err error ->
            Err error

        Ok ( pattern, afterPattern ) ->
            let
                stateAtArrow =
                    skipTrivia afterPattern
            in
            case String.left 2 (String.dropLeft stateAtArrow.offset stateAtArrow.source) of
                "->" ->
                    case
                        parseExpressionNodeAt
                            branchIndent
                            0
                            { source = stateAtArrow.source
                            , offset = stateAtArrow.offset + 2
                            , row = stateAtArrow.row
                            , column = stateAtArrow.column + 2
                            , commentsRev = stateAtArrow.commentsRev
                            }
                    of
                        Err error ->
                            Err error

                        Ok ( body, remaining ) ->
                            Ok
                                ( { pattern = pattern
                                  , arrowLocation = { row = stateAtArrow.row, column = stateAtArrow.column }
                                  , expression = body
                                  }
                                , remaining
                                )

                _ ->
                    Err ("Expected '->', but found '" ++ snippetAt stateAtArrow ++ "'.")


parseRecord : Int -> ParserState -> Result String ( Node Expression.Expression, ParserState )
parseRecord indentMin state =
    let
        stateAtToken =
            skipTrivia
                { source = state.source
                , offset = state.offset + 1
                , row = state.row
                , column = state.column + 1
                , commentsRev = state.commentsRev
                }
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (Expression.RecordExpr SeparatedSyntaxList.Empty)
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        first ->
            if not (isIdentifierStart first) then
                Err ("Expected a record field name, but found '" ++ snippetAt stateAtToken ++ "'.")

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    nameRange =
                        { start = { row = stateAtToken.row, column = stateAtToken.column }
                        , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                        }

                    name =
                        String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)

                    afterName =
                        { source = stateAtToken.source
                        , offset = nameEnd
                        , row = stateAtToken.row
                        , column = stateAtToken.column + nameLength
                        , commentsRev = stateAtToken.commentsRev
                        }

                    stateAtPipe =
                        skipTrivia afterName
                in
                if isPipeToken stateAtPipe.source stateAtPipe.offset then
                    case
                        parseRecordUpdateFields
                            indentMin
                            { source = stateAtPipe.source
                            , offset = stateAtPipe.offset + 1
                            , row = stateAtPipe.row
                            , column = stateAtPipe.column + 1
                            , commentsRev = stateAtPipe.commentsRev
                            }
                    of
                        Err error ->
                            Err error

                        Ok ( fields, afterClose ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = { row = afterClose.row, column = afterClose.column }
                                    }
                                    (Expression.RecordUpdateExpression
                                        (Node nameRange name)
                                        { row = stateAtPipe.row, column = stateAtPipe.column }
                                        fields
                                    )
                                , afterClose
                                )

                else
                    case parseRecordFieldsWithFirst indentMin nameRange name afterName of
                        Err error ->
                            Err error

                        Ok ( fields, afterClose ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = { row = afterClose.row, column = afterClose.column }
                                    }
                                    (Expression.RecordExpr fields)
                                , afterClose
                                )


parseRecordUpdateFields :
    Int
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField, ParserState )
parseRecordUpdateFields indentMin state =
    parseRecordUpdateFieldsAt indentMin (skipTrivia state)


parseRecordUpdateFieldsAt :
    Int
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField, ParserState )
parseRecordUpdateFieldsAt indentMin stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( SeparatedSyntaxList.Empty
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        first ->
            if not (isIdentifierStart first) then
                Err ("Expected a record field name, but found '" ++ snippetAt stateAtToken ++ "'.")

            else
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset
                in
                parseRecordFieldsWithFirst
                    indentMin
                    { start = { row = stateAtToken.row, column = stateAtToken.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                    }
                    (String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source))
                    { source = stateAtToken.source
                    , offset = nameEnd
                    , row = stateAtToken.row
                    , column = stateAtToken.column + nameLength
                    , commentsRev = stateAtToken.commentsRev
                    }


parseRecordFieldsWithFirst :
    Int
    -> Range
    -> String
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField, ParserState )
parseRecordFieldsWithFirst indentMin fieldNameRange fieldName state =
    case parseRecordField indentMin fieldNameRange fieldName state of
        Err error ->
            Err error

        Ok ( firstField, afterFirstField ) ->
            case parseFurtherRecordFields indentMin afterFirstField [] of
                Err error ->
                    Err error

                Ok ( furtherFields, afterClose ) ->
                    Ok
                        ( SeparatedSyntaxList.NonEmpty firstField furtherFields
                        , afterClose
                        )


parseRecordField :
    Int
    -> Range
    -> String
    -> ParserState
    -> Result String ( Expression.RecordExprField, ParserState )
parseRecordField indentMin fieldNameRange fieldName state =
    let
        stateAtSeparator =
            skipTrivia state

        separatorText =
            String.left 2 (String.dropLeft stateAtSeparator.offset stateAtSeparator.source)

        separator =
            String.left 1 separatorText
    in
    if separator == "=" || (separator == ":" && separatorText /= "::") then
        case
            parseExpressionNodeAt
                indentMin
                0
                { source = stateAtSeparator.source
                , offset = stateAtSeparator.offset + 1
                , row = stateAtSeparator.row
                , column = stateAtSeparator.column + 1
                , commentsRev = stateAtSeparator.commentsRev
                }
        of
            Err error ->
                Err error

            Ok ( valueExpression, remaining ) ->
                Ok
                    ( { fieldName = Node fieldNameRange fieldName
                      , equalsLocation = { row = stateAtSeparator.row, column = stateAtSeparator.column }
                      , valueExpr = valueExpression
                      }
                    , remaining
                    )

    else
        Err ("Expected '=' or ':', but found '" ++ snippetAt stateAtSeparator ++ "'.")


parseFurtherRecordFields :
    Int
    -> ParserState
    -> List ( Location, Expression.RecordExprField )
    -> Result String ( List ( Location, Expression.RecordExprField ), ParserState )
parseFurtherRecordFields indentMin state fieldsRev =
    parseFurtherRecordFieldsAt indentMin state fieldsRev (skipTrivia state)


parseFurtherRecordFieldsAt :
    Int
    -> ParserState
    -> List ( Location, Expression.RecordExprField )
    -> ParserState
    -> Result String ( List ( Location, Expression.RecordExprField ), ParserState )
parseFurtherRecordFieldsAt indentMin state fieldsRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            Ok
                ( List.reverse fieldsRev
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            let
                stateAtFieldName =
                    skipTrivia
                        { source = stateAtToken.source
                        , offset = stateAtToken.offset + 1
                        , row = stateAtToken.row
                        , column = stateAtToken.column + 1
                        , commentsRev = stateAtToken.commentsRev
                        }
            in
            case String.left 1 (String.dropLeft stateAtFieldName.offset stateAtFieldName.source) of
                fieldFirst ->
                    if not (isIdentifierStart fieldFirst) then
                        Err
                            ("Expected a record field name, but found '"
                                ++ snippetAt stateAtFieldName
                                ++ "'."
                            )

                    else
                        let
                            nameEnd =
                                skipToIdentifierEnd stateAtFieldName.source (stateAtFieldName.offset + 1)

                            nameLength =
                                nameEnd - stateAtFieldName.offset
                        in
                        case
                            parseRecordField
                                indentMin
                                { start = { row = stateAtFieldName.row, column = stateAtFieldName.column }
                                , end = { row = stateAtFieldName.row, column = stateAtFieldName.column + nameLength }
                                }
                                (String.left (nameEnd - stateAtFieldName.offset) (String.dropLeft stateAtFieldName.offset stateAtFieldName.source))
                                { source = stateAtFieldName.source
                                , offset = nameEnd
                                , row = stateAtFieldName.row
                                , column = stateAtFieldName.column + nameLength
                                , commentsRev = stateAtFieldName.commentsRev
                                }
                        of
                            Err error ->
                                Err error

                            Ok ( field, remaining ) ->
                                parseFurtherRecordFields
                                    indentMin
                                    remaining
                                    (( { row = stateAtToken.row, column = stateAtToken.column }, field ) :: fieldsRev)

        _ ->
            Err ("Expected ',' or '}', but found '" ++ snippetAt stateAtToken ++ "'.")



-- PATTERNS


parsePatternNodeAt :
    Int
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternNodeAt indentMin state =
    case parsePatternAtomic indentMin state of
        Err error ->
            Err error

        Ok ( pattern, afterAtomic ) ->
            case parseNamedPatternArguments indentMin pattern afterAtomic of
                Err error ->
                    Err error

                Ok ( namedPattern, afterArguments ) ->
                    parsePatternSuffix indentMin namedPattern afterArguments


parseNamedPatternArguments :
    Int
    -> Node Pattern.Pattern
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parseNamedPatternArguments indentMin pattern state =
    let
        (Node _ patternValue) =
            pattern
    in
    case patternValue of
        Pattern.NamedPattern name [] ->
            parsePatternArguments indentMin name pattern [] state

        _ ->
            Ok ( pattern, state )


parsePatternArguments :
    Int
    -> Pattern.QualifiedNameRef
    -> Node Pattern.Pattern
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternArguments indentMin name original argumentsRev state =
    parsePatternArgumentsAt indentMin name original argumentsRev state (skipTrivia state)


parsePatternArgumentsAt :
    Int
    -> Pattern.QualifiedNameRef
    -> Node Pattern.Pattern
    -> List (Node Pattern.Pattern)
    -> ParserState
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternArgumentsAt indentMin name original argumentsRev state stateAtArgument =
    if
        stateAtArgument.column
            >= indentMin
            && canStartNamedPatternArgumentAt stateAtArgument.source stateAtArgument.offset
    then
        case parsePatternAtomic indentMin stateAtArgument of
            Err error ->
                Err error

            Ok ( argument, remaining ) ->
                parsePatternArguments indentMin name original (argument :: argumentsRev) remaining

    else
        case argumentsRev of
            [] ->
                Ok ( original, state )

            (Node lastArgumentRange _) :: _ ->
                let
                    (Node originalRange _) =
                        original
                in
                Ok
                    ( Node
                        { start = originalRange.start
                        , end = lastArgumentRange.end
                        }
                        (Pattern.NamedPattern name (List.reverse argumentsRev))
                    , state
                    )


parsePatternSuffix :
    Int
    -> Node Pattern.Pattern
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternSuffix indentMin pattern state =
    parsePatternSuffixAt indentMin pattern state (skipTrivia state)


parsePatternSuffixAt :
    Int
    -> Node Pattern.Pattern
    -> ParserState
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternSuffixAt indentMin pattern state stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ":" ->
            if String.left 1 (String.dropLeft (stateAtToken.offset + 1) stateAtToken.source) == ":" then
                case
                    parsePatternNodeAt
                        indentMin
                        { source = stateAtToken.source
                        , offset = stateAtToken.offset + 2
                        , row = stateAtToken.row
                        , column = stateAtToken.column + 2
                        , commentsRev = stateAtToken.commentsRev
                        }
                of
                    Err error ->
                        Err error

                    Ok ( Node tailPatternRange tailPattern, remaining ) ->
                        let
                            (Node patternRange _) =
                                pattern
                        in
                        Ok
                            ( Node
                                { start = patternRange.start
                                , end = tailPatternRange.end
                                }
                                (Pattern.UnConsPattern
                                    pattern
                                    { row = stateAtToken.row, column = stateAtToken.column }
                                    (Node tailPatternRange tailPattern)
                                )
                            , remaining
                            )

            else
                Ok ( pattern, state )

        first ->
            if
                isIdentifierStart first
                    && (String.left (skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1) - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)
                            == "as"
                       )
            then
                let
                    stateAtName =
                        skipTrivia
                            { source = stateAtToken.source
                            , offset = stateAtToken.offset + 2
                            , row = stateAtToken.row
                            , column = stateAtToken.column + 2
                            , commentsRev = stateAtToken.commentsRev
                            }
                in
                case String.left 1 (String.dropLeft stateAtName.offset stateAtName.source) of
                    nameFirst ->
                        if isIdentifierStart nameFirst then
                            let
                                nameEnd =
                                    skipToIdentifierEnd stateAtName.source (stateAtName.offset + 1)

                                nameLength =
                                    nameEnd - stateAtName.offset

                                (Node patternRange _) =
                                    pattern
                            in
                            Ok
                                ( Node
                                    { start = patternRange.start
                                    , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                                    }
                                    (Pattern.AsPattern
                                        pattern
                                        { row = stateAtToken.row, column = stateAtToken.column }
                                        (Node
                                            { start = { row = stateAtName.row, column = stateAtName.column }
                                            , end = { row = stateAtName.row, column = stateAtName.column + nameLength }
                                            }
                                            (String.left (nameEnd - stateAtName.offset) (String.dropLeft stateAtName.offset stateAtName.source))
                                        )
                                    )
                                , { source = stateAtName.source
                                  , offset = nameEnd
                                  , row = stateAtName.row
                                  , column = stateAtName.column + nameLength
                                  , commentsRev = stateAtName.commentsRev
                                  }
                                )

                        else
                            Err
                                ("Expected a pattern name after 'as', but found '"
                                    ++ snippetAt stateAtName
                                    ++ "'."
                                )

            else
                Ok ( pattern, state )


parsePatternAtomic :
    Int
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternAtomic indentMin state =
    parsePatternAtomicAt indentMin (skipTrivia state)


parsePatternAtomicAt :
    Int
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parsePatternAtomicAt indentMin stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "\"" ->
            if String.left 3 (String.dropLeft stateAtToken.offset stateAtToken.source) == "\"\"\"" then
                parseStringPattern TripleQuoteTermination 3 stateAtToken

            else
                parseStringPattern DoubleQuoteTermination 1 stateAtToken

        "'" ->
            parseCharPattern stateAtToken

        "(" ->
            parseTuplePattern indentMin stateAtToken

        "[" ->
            parseListPattern indentMin stateAtToken

        "{" ->
            parseRecordPattern stateAtToken

        first ->
            if isDigit first then
                parseNumberPattern first stateAtToken

            else if isIdentifierStart first then
                let
                    nameEnd =
                        skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                    nameLength =
                        nameEnd - stateAtToken.offset

                    name =
                        String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source)
                in
                if name == "_" then
                    Ok
                        ( Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                            }
                            Pattern.AllPattern
                        , { source = stateAtToken.source
                          , offset = nameEnd
                          , row = stateAtToken.row
                          , column = stateAtToken.column + 1
                          , commentsRev = stateAtToken.commentsRev
                          }
                        )

                else if isUpperCharacter first then
                    let
                        ( Node qualifiedRange ( moduleNames, qualifiedName ), remaining ) =
                            parseQualifiedNameNode name nameEnd stateAtToken
                    in
                    Ok
                        ( Node qualifiedRange
                            (Pattern.NamedPattern
                                { moduleName = moduleNames, name = qualifiedName }
                                []
                            )
                        , remaining
                        )

                else
                    Ok
                        ( Node
                            { start = { row = stateAtToken.row, column = stateAtToken.column }
                            , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                            }
                            (Pattern.VarPattern name)
                        , { source = stateAtToken.source
                          , offset = nameEnd
                          , row = stateAtToken.row
                          , column = stateAtToken.column + nameLength
                          , commentsRev = stateAtToken.commentsRev
                          }
                        )

            else
                Err ("Expected a pattern, but found '" ++ snippetAt stateAtToken ++ "'.")


parseStringPattern :
    LiteralTermination
    -> Int
    -> ParserState
    -> Result String ( Node Pattern.Pattern, ParserState )
parseStringPattern termination openingLength state =
    case
        consumeLiteral
            termination
            state.source
            state.row
            state.column
            (state.offset + openingLength)
            state.row
            (state.column + openingLength)
            []
            []
    of
        Err error ->
            Err error

        Ok consumed ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = consumed.endRow, column = consumed.endColumn }
                    }
                    (Pattern.StringPattern consumed.decoded)
                , { source = state.source
                  , offset = consumed.endOffset
                  , row = consumed.endRow
                  , column = consumed.endColumn
                  , commentsRev = state.commentsRev
                  }
                )


parseCharPattern : ParserState -> Result String ( Node Pattern.Pattern, ParserState )
parseCharPattern state =
    case
        consumeLiteral
            SingleQuoteTermination
            state.source
            state.row
            state.column
            (state.offset + 1)
            state.row
            (state.column + 1)
            []
            []
    of
        Err error ->
            Err error

        Ok consumed ->
            case String.toList consumed.decoded of
                [ char ] ->
                    Ok
                        ( Node
                            { start = { row = state.row, column = state.column }
                            , end = { row = consumed.endRow, column = consumed.endColumn }
                            }
                            (Pattern.CharPattern (Char.toCode char))
                        , { source = state.source
                          , offset = consumed.endOffset
                          , row = consumed.endRow
                          , column = consumed.endColumn
                          , commentsRev = state.commentsRev
                          }
                        )

                _ ->
                    Err ("Invalid character pattern '" ++ consumed.decoded ++ "'.")


parseNumberPattern : String -> ParserState -> Result String ( Node Pattern.Pattern, ParserState )
parseNumberPattern firstCharacter state =
    let
        literalEnd =
            numberEnd state.source firstCharacter state.offset

        literalLength =
            literalEnd - state.offset

        literal =
            String.left (literalEnd - state.offset) (String.dropLeft state.offset state.source)

        range =
            { start = { row = state.row, column = state.column }
            , end = { row = state.row, column = state.column + literalLength }
            }

        remaining =
            { source = state.source
            , offset = literalEnd
            , row = state.row
            , column = state.column + literalLength
            , commentsRev = state.commentsRev
            }
    in
    if String.startsWith "0x" literal then
        case hexStringToInt (String.dropLeft 2 literal) of
            Just value ->
                Ok ( Node range (Pattern.HexPattern value), remaining )

            Nothing ->
                Err ("Invalid hexadecimal pattern '" ++ literal ++ "'.")

    else if String.contains "." literal || String.contains "e" literal || String.contains "E" literal then
        case String.toFloat literal of
            Just value ->
                Ok ( Node range (Pattern.FloatPattern value), remaining )

            Nothing ->
                Err ("Invalid float pattern '" ++ literal ++ "'.")

    else
        case String.toInt literal of
            Just value ->
                Ok ( Node range (Pattern.IntPattern value), remaining )

            Nothing ->
                Err ("Invalid integer pattern '" ++ literal ++ "'.")


parseTuplePattern : Int -> ParserState -> Result String ( Node Pattern.Pattern, ParserState )
parseTuplePattern indentMin state =
    let
        stateAtToken =
            skipTrivia
                { source = state.source
                , offset = state.offset + 1
                , row = state.row
                , column = state.column + 1
                , commentsRev = state.commentsRev
                }
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    Pattern.UnitPattern
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            case parsePatternNodeAt indentMin stateAtToken of
                Err error ->
                    Err error

                Ok ( first, afterFirst ) ->
                    case parseFurtherTuplePatterns indentMin afterFirst [] of
                        Err error ->
                            Err error

                        Ok ( further, afterClose ) ->
                            let
                                pattern =
                                    case further of
                                        [] ->
                                            Pattern.ParenthesizedPattern first

                                        _ ->
                                            Pattern.TuplePattern
                                                (SeparatedSyntaxList.NonEmpty first further)
                            in
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = { row = afterClose.row, column = afterClose.column }
                                    }
                                    pattern
                                , afterClose
                                )


parseFurtherTuplePatterns :
    Int
    -> ParserState
    -> List ( Location, Node Pattern.Pattern )
    -> Result String ( List ( Location, Node Pattern.Pattern ), ParserState )
parseFurtherTuplePatterns indentMin state furtherRev =
    parseFurtherTuplePatternsAt indentMin state furtherRev (skipTrivia state)


parseFurtherTuplePatternsAt :
    Int
    -> ParserState
    -> List ( Location, Node Pattern.Pattern )
    -> ParserState
    -> Result String ( List ( Location, Node Pattern.Pattern ), ParserState )
parseFurtherTuplePatternsAt indentMin state furtherRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        ")" ->
            Ok
                ( List.reverse furtherRev
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parsePatternNodeAt
                    indentMin
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( pattern, remaining ) ->
                    parseFurtherTuplePatterns
                        indentMin
                        remaining
                        (( { row = stateAtToken.row, column = stateAtToken.column }, pattern ) :: furtherRev)

        _ ->
            Err
                ("Expected ',' or a closing delimiter in pattern, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseListPattern : Int -> ParserState -> Result String ( Node Pattern.Pattern, ParserState )
parseListPattern indentMin state =
    let
        stateAtToken =
            skipTrivia
                { source = state.source
                , offset = state.offset + 1
                , row = state.row
                , column = state.column + 1
                , commentsRev = state.commentsRev
                }
    in
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "]" ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = stateAtToken.row, column = stateAtToken.column + 1 }
                    }
                    (Pattern.ListPattern SeparatedSyntaxList.Empty)
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        _ ->
            case parsePatternNodeAt indentMin stateAtToken of
                Err error ->
                    Err error

                Ok ( first, afterFirst ) ->
                    case parseFurtherListPatterns indentMin afterFirst [] of
                        Err error ->
                            Err error

                        Ok ( further, afterClose ) ->
                            Ok
                                ( Node
                                    { start = { row = state.row, column = state.column }
                                    , end = { row = afterClose.row, column = afterClose.column }
                                    }
                                    (Pattern.ListPattern
                                        (SeparatedSyntaxList.NonEmpty first further)
                                    )
                                , afterClose
                                )


parseFurtherListPatterns :
    Int
    -> ParserState
    -> List ( Location, Node Pattern.Pattern )
    -> Result String ( List ( Location, Node Pattern.Pattern ), ParserState )
parseFurtherListPatterns indentMin state furtherRev =
    parseFurtherListPatternsAt indentMin state furtherRev (skipTrivia state)


parseFurtherListPatternsAt :
    Int
    -> ParserState
    -> List ( Location, Node Pattern.Pattern )
    -> ParserState
    -> Result String ( List ( Location, Node Pattern.Pattern ), ParserState )
parseFurtherListPatternsAt indentMin state furtherRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "]" ->
            Ok
                ( List.reverse furtherRev
                , { source = stateAtToken.source
                  , offset = stateAtToken.offset + 1
                  , row = stateAtToken.row
                  , column = stateAtToken.column + 1
                  , commentsRev = stateAtToken.commentsRev
                  }
                )

        "," ->
            case
                parsePatternNodeAt
                    indentMin
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            of
                Err error ->
                    Err error

                Ok ( pattern, remaining ) ->
                    parseFurtherListPatterns
                        indentMin
                        remaining
                        (( { row = stateAtToken.row, column = stateAtToken.column }, pattern ) :: furtherRev)

        _ ->
            Err
                ("Expected ',' or a closing delimiter in pattern, but found '"
                    ++ snippetAt stateAtToken
                    ++ "'."
                )


parseRecordPattern : ParserState -> Result String ( Node Pattern.Pattern, ParserState )
parseRecordPattern state =
    case
        parseRecordPatternFields
            { source = state.source
            , offset = state.offset + 1
            , row = state.row
            , column = state.column + 1
            , commentsRev = state.commentsRev
            }
            Nothing
            []
    of
        Err error ->
            Err error

        Ok ( fields, afterClose ) ->
            Ok
                ( Node
                    { start = { row = state.row, column = state.column }
                    , end = { row = afterClose.row, column = afterClose.column }
                    }
                    (Pattern.RecordPattern fields)
                , afterClose
                )


parseRecordPatternFields :
    ParserState
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), ParserState )
parseRecordPatternFields state firstField furtherRev =
    parseRecordPatternFieldsAt state firstField furtherRev (skipTrivia state)


parseRecordPatternFieldsAt :
    ParserState
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), ParserState )
parseRecordPatternFieldsAt state firstField furtherRev stateAtToken =
    case String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) of
        "}" ->
            let
                afterClose =
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
            in
            case firstField of
                Nothing ->
                    Ok ( SeparatedSyntaxList.Empty, afterClose )

                Just first ->
                    Ok
                        ( SeparatedSyntaxList.NonEmpty first (List.reverse furtherRev)
                        , afterClose
                        )

        first ->
            if isIdentifierStart first then
                case firstField of
                    Nothing ->
                        let
                            nameEnd =
                                skipToIdentifierEnd stateAtToken.source (stateAtToken.offset + 1)

                            nameLength =
                                nameEnd - stateAtToken.offset
                        in
                        parseRecordPatternFieldsAfterField
                            { source = stateAtToken.source
                            , offset = nameEnd
                            , row = stateAtToken.row
                            , column = stateAtToken.column + nameLength
                            , commentsRev = stateAtToken.commentsRev
                            }
                            (Just
                                (Node
                                    { start = { row = stateAtToken.row, column = stateAtToken.column }
                                    , end = { row = stateAtToken.row, column = stateAtToken.column + nameLength }
                                    }
                                    (String.left (nameEnd - stateAtToken.offset) (String.dropLeft stateAtToken.offset stateAtToken.source))
                                )
                            )
                            furtherRev

                    Just _ ->
                        Err "Expected ',' before record pattern field."

            else
                Err
                    ("Expected a record pattern field or '}', but found '"
                        ++ snippetAt stateAtToken
                        ++ "'."
                    )


parseRecordPatternFieldsAfterField :
    ParserState
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), ParserState )
parseRecordPatternFieldsAfterField state firstField furtherRev =
    parseRecordPatternFieldsAfterFieldAt state firstField furtherRev (skipTrivia state)


parseRecordPatternFieldsAfterFieldAt :
    ParserState
    -> Maybe (Node String)
    -> List ( Location, Node String )
    -> ParserState
    -> Result String ( SeparatedSyntaxList.SeparatedSyntaxList (Node String), ParserState )
parseRecordPatternFieldsAfterFieldAt state firstField furtherRev stateAtToken =
    if String.left 1 (String.dropLeft stateAtToken.offset stateAtToken.source) == "," then
        let
            stateAtField =
                skipTrivia
                    { source = stateAtToken.source
                    , offset = stateAtToken.offset + 1
                    , row = stateAtToken.row
                    , column = stateAtToken.column + 1
                    , commentsRev = stateAtToken.commentsRev
                    }
        in
        case String.left 1 (String.dropLeft stateAtField.offset stateAtField.source) of
            fieldFirst ->
                if isIdentifierStart fieldFirst then
                    let
                        nameEnd =
                            skipToIdentifierEnd stateAtField.source (stateAtField.offset + 1)

                        nameLength =
                            nameEnd - stateAtField.offset
                    in
                    parseRecordPatternFieldsAfterField
                        { source = stateAtField.source
                        , offset = nameEnd
                        , row = stateAtField.row
                        , column = stateAtField.column + nameLength
                        , commentsRev = stateAtField.commentsRev
                        }
                        firstField
                        (( { row = stateAtToken.row, column = stateAtToken.column }
                         , Node
                            { start = { row = stateAtField.row, column = stateAtField.column }
                            , end = { row = stateAtField.row, column = stateAtField.column + nameLength }
                            }
                            (String.left (nameEnd - stateAtField.offset) (String.dropLeft stateAtField.offset stateAtField.source))
                         )
                            :: furtherRev
                        )

                else
                    Err
                        ("Expected a record pattern field after ',', but found '"
                            ++ snippetAt stateAtField
                            ++ "'."
                        )

    else
        parseRecordPatternFields state firstField furtherRev



-- TRIVIA


{-| Advances the state over whitespace, line comments and nested block comments, collecting every
comment it consumes with its exact range and lexeme.

This is the only place in the parser that recognizes a comment, so the state reaching the end of a
source carries every comment of that source and no additional scan over the source is needed.

-}
skipTrivia : ParserState -> ParserState
skipTrivia state =
    skipTriviaAt state.source state.offset state.row state.column state.commentsRev


skipWhitespaceAt : String -> Int -> Int -> Int -> List (Node String) -> ParserState
skipWhitespaceAt source offset row column commentsRev =
    let
        nextTwoChars =
            String.left 2 (String.dropLeft offset source)
    in
    if nextTwoChars == "\u{000D}\n" then
        skipWhitespaceAt source (offset + 2) (row + 1) 1 commentsRev

    else
        case String.left 1 nextTwoChars of
            " " ->
                skipWhitespaceAt source (offset + 1) row (column + 1) commentsRev

            "\n" ->
                skipWhitespaceAt source (offset + 1) (row + 1) 1 commentsRev

            "\t" ->
                skipWhitespaceAt source (offset + 1) row (column + 1) commentsRev

            "\u{000D}" ->
                skipWhitespaceAt source (offset + 1) (row + 1) 1 commentsRev

            _ ->
                { source = source
                , offset = offset
                , row = row
                , column = column
                , commentsRev = commentsRev
                }


{-| The trivia scan after at least one character of trivia was consumed: unlike `skipTrivia` it
always builds the resulting state, because the position it starts at already differs from the one
its caller started at.
-}
skipTriviaAt : String -> Int -> Int -> Int -> List (Node String) -> ParserState
skipTriviaAt source offset row column commentsRev =
    skipTriviaAfterWhitespace (skipWhitespaceAt source offset row column commentsRev)


skipTriviaAfterWhitespace : ParserState -> ParserState
skipTriviaAfterWhitespace state =
    case String.left 2 (String.dropLeft state.offset state.source) of
        "--" ->
            skipTriviaLineComment state.source state.offset state.row state.column state.commentsRev

        "{-" ->
            skipTriviaBlockComment
                state.source
                (state.offset + 2)
                state.row
                (state.column + 2)
                state.row
                state.column
                1
                [ "{-" ]
                state.commentsRev

        _ ->
            state


{-| Collects the line comment starting at `offset` and continues the trivia scan after it. The
line break terminating the comment is not part of the comment's lexeme or range.
-}
skipTriviaLineComment : String -> Int -> Int -> Int -> List (Node String) -> ParserState
skipTriviaLineComment source offset row column commentsRev =
    let
        contentEnd =
            lineCommentEnd source (offset + 2)

        endColumn =
            column + (contentEnd - offset)
    in
    skipTriviaAt
        source
        contentEnd
        row
        endColumn
        (Node
            { start = { row = row, column = column }
            , end = { row = row, column = endColumn }
            }
            (String.left (contentEnd - offset) (String.dropLeft offset source))
            :: commentsRev
        )


{-| Collects a (possibly nested, possibly multi-line) block comment with all line breaks
normalized to a single LF, and continues the trivia scan after it.

The comment's chunks are accumulated in `chunksRev` while scanning, so the lexeme is built from
one run per line and nesting delimiter instead of one slice per character.

-}
skipTriviaBlockComment :
    String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List String
    -> List (Node String)
    -> ParserState
skipTriviaBlockComment source offset row column startRow startColumn depth chunksRev commentsRev =
    let
        ( runEndOffset, runEndType ) =
            multilineCommentRunEnd source offset

        run =
            String.left (runEndOffset - offset) (String.dropLeft offset source)

        columnAfterRun =
            column + (runEndOffset - offset)

        chunksAfterRun =
            prependNonEmptyChunk run chunksRev
    in
    case runEndType of
        MultilineCommentRunEnd_EndOfInput ->
            { source = source
            , offset = runEndOffset
            , row = row
            , column = columnAfterRun
            , commentsRev =
                Node
                    { start = { row = startRow, column = startColumn }
                    , end = { row = row, column = columnAfterRun }
                    }
                    (concatenateChunksRev chunksAfterRun)
                    :: commentsRev
            }

        MultilineCommentRunEnd_NewlineLF ->
            skipTriviaBlockComment source (runEndOffset + 1) (row + 1) 1 startRow startColumn depth ("\n" :: chunksAfterRun) commentsRev

        MultilineCommentRunEnd_NewlineCRLF ->
            skipTriviaBlockComment source (runEndOffset + 2) (row + 1) 1 startRow startColumn depth ("\n" :: chunksAfterRun) commentsRev

        MultilineCommentRunEnd_NewlineCR ->
            skipTriviaBlockComment source (runEndOffset + 1) (row + 1) 1 startRow startColumn depth ("\n" :: chunksAfterRun) commentsRev

        MultilineCommentRunEnd_StartComment ->
            skipTriviaBlockComment source (runEndOffset + 2) row (columnAfterRun + 2) startRow startColumn (depth + 1) ("{-" :: chunksAfterRun) commentsRev

        MultilineCommentRunEnd_EndComment ->
            let
                finalChunksRev =
                    "-}" :: chunksAfterRun

                endColumn =
                    columnAfterRun + 2
            in
            if depth == 1 then
                skipTriviaAt
                    source
                    (runEndOffset + 2)
                    row
                    endColumn
                    (Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = row, column = endColumn }
                        }
                        (concatenateChunksRev finalChunksRev)
                        :: commentsRev
                    )

            else
                skipTriviaBlockComment source (runEndOffset + 2) row endColumn startRow startColumn (depth - 1) finalChunksRev commentsRev


{-| Position-only variant of `skipTrivia` for lookahead that never needs row or column.

Lookahead never advances the state that a parse result is built from, therefore this variant does
not collect comments: the trivia it passes over is scanned again by the `skipTrivia` call whose
result the parser keeps.

-}
skipTriviaOffset : String -> Int -> Int
skipTriviaOffset source offset =
    let
        offsetAfterWhitespace =
            skipWhitespaceOffset source offset
    in
    case String.left 2 (String.dropLeft offsetAfterWhitespace source) of
        "--" ->
            skipTriviaOffset source (lineCommentEnd source (offsetAfterWhitespace + 2))

        "{-" ->
            skipTriviaOffset source (blockCommentEndOffset source (offsetAfterWhitespace + 2) 1)

        _ ->
            offsetAfterWhitespace


skipWhitespaceOffset : String -> Int -> Int
skipWhitespaceOffset source offset =
    case String.left 1 (String.dropLeft offset source) of
        " " ->
            skipWhitespaceOffset source (offset + 1)

        "\n" ->
            skipWhitespaceOffset source (offset + 1)

        "\u{000D}" ->
            skipWhitespaceOffset source (offset + 1)

        "\t" ->
            skipWhitespaceOffset source (offset + 1)

        _ ->
            offset


blockCommentEndOffset : String -> Int -> Int -> Int
blockCommentEndOffset source offset depth =
    let
        nextTwoChars =
            String.left 2 (String.dropLeft offset source)
    in
    case nextTwoChars of
        "{-" ->
            blockCommentEndOffset source (offset + 2) (depth + 1)

        "-}" ->
            if depth == 1 then
                offset + 2

            else
                blockCommentEndOffset source (offset + 2) (depth - 1)

        _ ->
            if nextTwoChars == "" then
                offset

            else
                blockCommentEndOffset source (offset + 1) depth



-- CONSUMING FIXED SYNTAX


consumeKeyword : String -> Int -> ParserState -> Result String ( Location, ParserState )
consumeKeyword keyword keywordLength state =
    consumeKeywordAt keyword keywordLength (skipTrivia state)


consumeKeywordAt : String -> Int -> ParserState -> Result String ( Location, ParserState )
consumeKeywordAt keyword keywordLength stateAtKeyword =
    let
        endOffset =
            stateAtKeyword.offset + keywordLength
    in
    if
        String.left (endOffset - stateAtKeyword.offset) (String.dropLeft stateAtKeyword.offset stateAtKeyword.source)
            == keyword
            && not (isIdentifierChar (String.left 1 (String.dropLeft endOffset stateAtKeyword.source)))
    then
        Ok
            ( { row = stateAtKeyword.row, column = stateAtKeyword.column }
            , { source = stateAtKeyword.source
              , offset = endOffset
              , row = stateAtKeyword.row
              , column = stateAtKeyword.column + keywordLength
              , commentsRev = stateAtKeyword.commentsRev
              }
            )

    else
        Err ("Expected '" ++ keyword ++ "', but found '" ++ snippetAt stateAtKeyword ++ "'.")


{-| True when a `.` at the given offset forms a lone dot rather than `..` or a longer operator.
-}
isDotToken : String -> Int -> Bool
isDotToken source offset =
    if String.left 1 (String.dropLeft offset source) == "." then
        not (isOperatorChar (String.left 1 (String.dropLeft (offset + 1) source)))

    else
        False


isPipeToken : String -> Int -> Bool
isPipeToken source offset =
    if String.left 1 (String.dropLeft offset source) == "|" then
        not (isOperatorChar (String.left 1 (String.dropLeft (offset + 1) source)))

    else
        False


isColonToken : String -> Int -> Bool
isColonToken source offset =
    if String.left 1 (String.dropLeft offset source) == ":" then
        not (isOperatorChar (String.left 1 (String.dropLeft (offset + 1) source)))

    else
        False


isEqualsToken : String -> Int -> Bool
isEqualsToken source offset =
    if String.left 1 (String.dropLeft offset source) == "=" then
        not (isOperatorChar (String.left 1 (String.dropLeft (offset + 1) source)))

    else
        False



-- OPERATORS


{-| Length of the operator symbol at the given offset, or `0` when there is no operator there.
A `-` only counts as an operator when it is not a negation sign, matching the surrounding rules
of the language: a `-` directly attached to a preceding expression, or followed by whitespace or
a closing delimiter, is binary; otherwise it negates the expression that follows.
-}
operatorTokenLength : String -> Int -> Int
operatorTokenLength source offset =
    let
        nextTwoChars =
            String.left 2 (String.dropLeft offset source)

        second =
            String.left 1 (String.dropLeft 1 nextTwoChars)
    in
    case nextTwoChars of
        "--" ->
            0

        "->" ->
            0

        ".." ->
            0

        _ ->
            case String.left 1 nextTwoChars of
                "-" ->
                    if minusIsOperatorAt source offset then
                        1

                    else
                        0

                "=" ->
                    if isOperatorChar second then
                        2

                    else
                        0

                "|" ->
                    if isOperatorChar second then
                        2

                    else
                        0

                ":" ->
                    if isOperatorChar second then
                        2

                    else
                        0

                "." ->
                    if isOperatorChar second then
                        2

                    else
                        0

                first ->
                    if isOperatorChar first then
                        skipOperatorChars source (offset + 1) (offset + 3) - offset

                    else
                        0


minusIsOperatorAt : String -> Int -> Bool
minusIsOperatorAt source offset =
    case String.left 1 (String.dropLeft (offset + 1) source) of
        "" ->
            True

        ")" ->
            True

        "]" ->
            True

        "}" ->
            True

        next ->
            if isWhitespace next then
                True

            else
                previousCharacterEndsExpression source offset


{-| True when the character right before the offset is the last character of something that can
end an expression, which makes a directly attached `-` a binary operator.
-}
previousCharacterEndsExpression : String -> Int -> Bool
previousCharacterEndsExpression source offset =
    if offset <= 0 then
        False

    else
        case String.left 1 (String.dropLeft (offset - 1) source) of
            ")" ->
                True

            "]" ->
                True

            "}" ->
                True

            "\"" ->
                True

            "'" ->
                True

            previous ->
                isIdentifierChar previous


{-| Precedence of a known infix operator, or `-1` when the symbol is not a known infix operator.
-}
operatorPrecedence : String -> Int
operatorPrecedence lexeme =
    case lexeme of
        "<|" ->
            0

        "|>" ->
            0

        "||" ->
            2

        "&&" ->
            3

        "==" ->
            4

        "/=" ->
            4

        "<" ->
            4

        ">" ->
            4

        "<=" ->
            4

        ">=" ->
            4

        "++" ->
            5

        "::" ->
            5

        "+" ->
            6

        "-" ->
            6

        "*" ->
            7

        "//" ->
            7

        "/" ->
            7

        "^" ->
            8

        "<<" ->
            9

        ">>" ->
            9

        "|=" ->
            5

        "|." ->
            6

        "</>" ->
            7

        "<?>" ->
            8

        _ ->
            -1


operatorDirection : String -> Infix.InfixDirection
operatorDirection lexeme =
    case lexeme of
        "<|" ->
            Infix.Right

        "|>" ->
            Infix.Left

        "||" ->
            Infix.Right

        "&&" ->
            Infix.Right

        "==" ->
            Infix.Non

        "/=" ->
            Infix.Non

        "<" ->
            Infix.Non

        ">" ->
            Infix.Non

        "<=" ->
            Infix.Non

        ">=" ->
            Infix.Non

        "++" ->
            Infix.Right

        "::" ->
            Infix.Right

        "+" ->
            Infix.Left

        "-" ->
            Infix.Left

        "*" ->
            Infix.Left

        "//" ->
            Infix.Left

        "/" ->
            Infix.Left

        "^" ->
            Infix.Right

        "<<" ->
            Infix.Left

        ">>" ->
            Infix.Right

        "|=" ->
            Infix.Left

        "|." ->
            Infix.Left

        "</>" ->
            Infix.Right

        _ ->
            Infix.Left



-- START-OF-SYNTAX PREDICATES


canStartArgumentExpressionAt : String -> Int -> Bool
canStartArgumentExpressionAt source offset =
    let
        nextTwoChars =
            String.left 2 (String.dropLeft offset source)
    in
    case nextTwoChars of
        ".." ->
            False

        "->" ->
            False

        _ ->
            case String.left 1 nextTwoChars of
                "\"" ->
                    True

                "'" ->
                    True

                "(" ->
                    True

                "{" ->
                    True

                "[" ->
                    True

                "." ->
                    not (isOperatorChar (String.left 1 (String.dropLeft 1 nextTwoChars)))

                "-" ->
                    not (minusIsOperatorAt source offset)

                first ->
                    if isDigit first then
                        True

                    else if isIdentifierStart first then
                        not (isKeywordAt source offset)

                    else
                        False


{-| True when an identifier keyword that cannot start an argument begins at the offset. Longer
keywords are checked first so each branch reads a whole candidate at once.
-}
isKeywordAt : String -> Int -> Bool
isKeywordAt source offset =
    case String.left 4 (String.dropLeft offset source) of
        "case" ->
            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 4) source)))

        "then" ->
            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 4) source)))

        "else" ->
            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 4) source)))

        _ ->
            case String.left 3 (String.dropLeft offset source) of
                "let" ->
                    not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 3) source)))

                _ ->
                    case String.left 2 (String.dropLeft offset source) of
                        "if" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        "in" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        "of" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        _ ->
                            False


{-| True when the identifier at the offset is one of the keywords that terminate a run of
arguments of a named pattern.
-}
isPatternBoundaryKeywordAt : String -> Int -> Bool
isPatternBoundaryKeywordAt source offset =
    case String.left 4 (String.dropLeft offset source) of
        "then" ->
            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 4) source)))

        "else" ->
            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 4) source)))

        _ ->
            case String.left 3 (String.dropLeft offset source) of
                "let" ->
                    not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 3) source)))

                _ ->
                    case String.left 2 (String.dropLeft offset source) of
                        "as" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        "of" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        "in" ->
                            not (isIdentifierChar (String.left 1 (String.dropLeft (offset + 2) source)))

                        _ ->
                            False


canStartNamedPatternArgumentAt : String -> Int -> Bool
canStartNamedPatternArgumentAt source offset =
    case String.left 1 (String.dropLeft offset source) of
        "\"" ->
            True

        "'" ->
            True

        "(" ->
            True

        "{" ->
            True

        "[" ->
            True

        first ->
            if isDigit first then
                True

            else if isIdentifierStart first then
                not (isPatternBoundaryKeywordAt source offset)

            else
                False


canStartArgumentPatternAt : String -> Int -> Bool
canStartArgumentPatternAt source offset =
    case String.left 1 (String.dropLeft offset source) of
        "(" ->
            True

        "{" ->
            True

        "[" ->
            True

        first ->
            isIdentifierStart first


canStartTypeAnnotationAt : String -> Int -> Bool
canStartTypeAnnotationAt source offset =
    case String.left 1 (String.dropLeft offset source) of
        "(" ->
            True

        "{" ->
            True

        first ->
            isIdentifierStart first


isClosingAt : String -> Int -> Bool
isClosingAt source offset =
    case String.left 1 (String.dropLeft offset source) of
        "," ->
            True

        ")" ->
            True

        "]" ->
            True

        "}" ->
            True

        _ ->
            False



-- LITERALS


type alias ConsumedLiteral =
    { decoded : String
    , raw : String
    , endOffset : Int
    , endRow : Int
    , endColumn : Int
    }


consumeLiteral :
    LiteralTermination
    -> String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List String
    -> List String
    -> Result String ConsumedLiteral
consumeLiteral termination source startRow startColumn offset row column decodedChunksRev rawChunksRev =
    let
        ( runEndOffset, boundary ) =
            findLiteralRunEnd termination source offset

        run =
            String.left (runEndOffset - offset) (String.dropLeft offset source)

        columnAfterRun =
            column + (runEndOffset - offset)

        decodedChunksAfterRun =
            prependNonEmptyChunk run decodedChunksRev

        rawChunksAfterRun =
            prependNonEmptyChunk run rawChunksRev
    in
    case boundary of
        LiteralRunTermination ->
            let
                terminationLength =
                    literalTerminationLength termination
            in
            Ok
                { decoded = concatenateChunksRev decodedChunksAfterRun
                , raw = concatenateChunksRev rawChunksAfterRun
                , endOffset = runEndOffset + terminationLength
                , endRow = row
                , endColumn = columnAfterRun + terminationLength
                }

        LiteralRunUnterminated ->
            Err ("Unterminated literal at " ++ locationString { row = startRow, column = startColumn } ++ ".")

        LiteralRunNewlineLF ->
            consumeLiteral termination
                source
                startRow
                startColumn
                (runEndOffset + 1)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunNewlineCRLF ->
            consumeLiteral termination
                source
                startRow
                startColumn
                (runEndOffset + 2)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunNewlineCR ->
            consumeLiteral termination
                source
                startRow
                startColumn
                (runEndOffset + 1)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunBackslash ->
            case String.left 1 (String.dropLeft (runEndOffset + 1) source) of
                "u" ->
                    consumeUnicodeEscape
                        termination
                        source
                        startRow
                        startColumn
                        runEndOffset
                        row
                        columnAfterRun
                        decodedChunksAfterRun
                        rawChunksAfterRun

                "" ->
                    Err ("Unterminated literal at " ++ locationString { row = startRow, column = startColumn } ++ ".")

                escaped ->
                    let
                        decodedCharacter =
                            case escaped of
                                "n" ->
                                    "\n"

                                "r" ->
                                    "\u{000D}"

                                "t" ->
                                    "\t"

                                _ ->
                                    escaped
                    in
                    consumeLiteral termination
                        source
                        startRow
                        startColumn
                        (runEndOffset + 2)
                        row
                        (columnAfterRun + 2)
                        (decodedCharacter :: decodedChunksAfterRun)
                        (("\\" ++ escaped) :: rawChunksAfterRun)


{-| Handles a `\u...` escape beginning at `escapeOffset` (the backslash). Only the `\u{XXXX}`
form is a valid unicode escape; any other character (or no `{`) following `\u` falls back to
treating it the same way an unrecognized single-character escape like `\z` would be treated
elsewhere, i.e. decoding to a literal `u`.
-}
consumeUnicodeEscape :
    LiteralTermination
    -> String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List String
    -> List String
    -> Result String ConsumedLiteral
consumeUnicodeEscape termination source startRow startColumn escapeOffset escapeRow escapeColumn decodedChunksRev rawChunksRev =
    let
        afterPrefixOffset =
            escapeOffset + 2
    in
    if String.left 1 (String.dropLeft afterPrefixOffset source) == "{" then
        case scanUnicodeEscapeDigits source (afterPrefixOffset + 1) of
            Just ( digitsEndOffset, codePoint ) ->
                if
                    String.left 1 (String.dropLeft digitsEndOffset source)
                        == "}"
                        && codePoint
                        <= 0x0010FFFF
                        && not (codePoint >= 0xD800 && codePoint <= 0xDFFF)
                then
                    consumeLiteral termination
                        source
                        startRow
                        startColumn
                        (digitsEndOffset + 1)
                        escapeRow
                        (escapeColumn + ((digitsEndOffset + 1) - escapeOffset))
                        (String.fromChar (Char.fromCode codePoint) :: decodedChunksRev)
                        (String.left (digitsEndOffset + 1 - escapeOffset) (String.dropLeft escapeOffset source) :: rawChunksRev)

                else
                    Err ("Invalid unicode escape at " ++ locationString { row = escapeRow, column = escapeColumn } ++ ".")

            Nothing ->
                Err ("Invalid unicode escape at " ++ locationString { row = escapeRow, column = escapeColumn } ++ ".")

    else
        consumeLiteral termination
            source
            startRow
            startColumn
            afterPrefixOffset
            escapeRow
            (escapeColumn + 2)
            ("u" :: decodedChunksRev)
            ("\\u" :: rawChunksRev)


{-| Offset right after the closing delimiter of a literal whose opening delimiter has already
been consumed. Used by lookahead that only needs to move past a literal.
-}
literalEndOffset : LiteralTermination -> String -> Int -> Int
literalEndOffset termination source offset =
    let
        ( runEndOffset, boundary ) =
            findLiteralRunEnd termination source offset
    in
    case boundary of
        LiteralRunTermination ->
            runEndOffset + literalTerminationLength termination

        LiteralRunUnterminated ->
            runEndOffset

        LiteralRunBackslash ->
            literalEndOffset termination source (runEndOffset + 2)

        LiteralRunNewlineLF ->
            literalEndOffset termination source (runEndOffset + 1)

        LiteralRunNewlineCRLF ->
            literalEndOffset termination source (runEndOffset + 2)

        LiteralRunNewlineCR ->
            literalEndOffset termination source (runEndOffset + 1)



-- SOURCE SCANNING


{-| Short description of the syntax at the given position, for error messages.
-}
snippetAt : ParserState -> String
snippetAt state =
    case String.left 1 (String.dropLeft state.offset state.source) of
        "" ->
            "<end of input>"

        first ->
            if isIdentifierStart first then
                String.left (skipToIdentifierEnd state.source (state.offset + 1) - state.offset) (String.dropLeft state.offset state.source)

            else if isDigit first then
                String.left (numberEnd state.source first state.offset - state.offset) (String.dropLeft state.offset state.source)

            else
                first



-- LITERAL VALUES


parseNumber : String -> Expression.Expression
parseNumber literal =
    if String.left 2 literal == "0x" then
        Expression.IntegerLiteral literal

    else if isFloatLiteral literal then
        Expression.FloatLiteral literal

    else
        Expression.IntegerLiteral literal
