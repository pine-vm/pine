using Pine.Core;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.ElmSyntax;

public class ElmSyntaxParser
{
    private record File(
        Node<Module> ModuleDefinition,
        IReadOnlyList<Node<Declaration>> Declarations);

    private record Node<T>(
        Range Range,
        T Value);

    private record Range(
        Location Start,
        Location End);

    public record Location(
        int Row,
        int Column);

    private abstract record Module
    {
        public sealed record NormalModule(
            DefaultModuleData ModuleData)
            : Module;
    }

    private record DefaultModuleData(
        Node<IReadOnlyList<string>> ModuleName,
        Node<Exposing> ExposingList);

    private abstract record Exposing
    {
        public sealed record All(
            Range Range)
            : Exposing;
    }

    private abstract record Declaration
    {
        /*
        type Declaration
            = FunctionDeclaration Function
            | AliasDeclaration TypeAlias
            | CustomTypeDeclaration Type
            | PortDeclaration Signature
            | InfixDeclaration Infix
            | Destructuring (Node Pattern) (Node Expression)
         * */

        public sealed record FunctionDeclaration(
            FunctionStruct Function)
            : Declaration;

        public sealed record CustomTypeDeclaration(
            TypeStruct TypeDeclaration)
            : Declaration;
    }

    /*
    type alias Type =
        { documentation : Maybe (Node Documentation)
        , name : Node String
        , generics : List (Node String)
        , constructors : List (Node ValueConstructor)
        }


    type alias ValueConstructor =
        { name : Node String
        , arguments : List (Node TypeAnnotation)
        }

     * */

    private record TypeStruct(
        Node<string> TypeName,
        IReadOnlyList<Node<string>> Generics,
        IReadOnlyList<Node<ValueConstructor>> Constructors);

    private record ValueConstructor(
        Node<string> Name,
        IReadOnlyList<Node<TypeAnnotation>> Arguments);

    /*
    type TypeAnnotation
        = GenericType String
        | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
        | Unit
        | Tupled (List (Node TypeAnnotation))
        | Record RecordDefinition
        | GenericRecord (Node String) (Node RecordDefinition)
        | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)

     * */

    private abstract record TypeAnnotation
    {
        /*
        type TypeAnnotation
            = GenericType String
            | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
            | Unit
            | Tupled (List (Node TypeAnnotation))
            | Record RecordDefinition
            | GenericRecord (Node String) (Node RecordDefinition)
            | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
         * */

        public sealed record GenericType(
            string Name)
            : TypeAnnotation;

        public sealed record Typed(
            Node<(IReadOnlyList<string> ModuleName, string Name)> TypeName,
            IReadOnlyList<Node<TypeAnnotation>> TypeArguments)
            : TypeAnnotation;

        public sealed record Unit
            : TypeAnnotation;

        public sealed record Tupled(
            IReadOnlyList<Node<TypeAnnotation>> TypeAnnotations)
            : TypeAnnotation;

        public sealed record Record(
            RecordDefinition RecordDefinition)
            : TypeAnnotation;

        public sealed record GenericRecord(
            Node<string> GenericName,
            Node<RecordDefinition> RecordDefinition)
            : TypeAnnotation;
    }

    /*
    type alias RecordDefinition =
        List (Node RecordField)
     * */

    private record RecordDefinition(
        IReadOnlyList<Node<RecordField>> Fields);

    /*
    type alias RecordField =
        ( Node String, Node TypeAnnotation )
     * */

    private record RecordField(
        Node<string> FieldName,
        Node<TypeAnnotation> FieldType);

    private record FunctionStruct(
        Node<FunctionImplementation> Declaration);

    private record FunctionImplementation(
        Node<string> Name,
        IReadOnlyList<Node<Pattern>> Arguments,
        Node<Expression> Expression);

    private abstract record Pattern
    {
        public sealed record VarPattern(
            string Name)
            : Pattern;
    }

    private abstract record Expression
    {
        /*
        type Expression
            = UnitExpr
            | Application (List (Node Expression))
            | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
            | FunctionOrValue ModuleName String
            | IfBlock (Node Expression) (Node Expression) (Node Expression)
            | PrefixOperator String
            | Operator String
            | Integer Int
            | Hex Int
            | Floatable Float
            | Negation (Node Expression)
            | Literal String
            | CharLiteral Char
            | TupledExpression (List (Node Expression))
            | ParenthesizedExpression (Node Expression)
            | LetExpression LetBlock
            | CaseExpression CaseBlock
            | LambdaExpression Lambda
            | RecordExpr (List (Node RecordSetter))
            | ListExpr (List (Node Expression))
            | RecordAccess (Node Expression) (Node String)
            | RecordAccessFunction String
            | RecordUpdateExpression (Node String) (List (Node RecordSetter))
            | GLSLExpression String

         * */

        public sealed record Literal(
            string Value)
            : Expression;

        public sealed record ListExpr(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        public sealed record FunctionOrValue(
            IReadOnlyList<string> ModuleName,
            string Name)
            : Expression;

        public sealed record ParenthesizedExpression(
            Node<Expression> Expression)
            : Expression;

        public sealed record Application(
            IReadOnlyList<Node<Expression>> Arguments)
            : Expression;

        public sealed record TupledExpression(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        public sealed record RecordExpr(
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
            : Expression;
    }

    public static Result<string, ElmValue> ParseModuleTextAsElmSyntaxElmValue(
        string elmModuleText)
    {
        var parseResult =
            ParseModuleText(elmModuleText);

        if (parseResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (parseResult.IsOkOrNull() is not { } parseOk)
        {
            throw new NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().Name);
        }

        return EncodeAsElmValue(parseOk);
    }

    private static ElmValue EncodeAsElmValue(File file)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("comments", ElmValue.ListInstance([])),

                ("declarations",
                ElmValue.ListInstance(
                    [..file.Declarations.Select(d => EncodeNode(EncodeDeclaration, d))])),

                ("imports",
                ElmValue.ListInstance([])),

                ("moduleDefinition",
                EncodeNode(EncodeModule, file.ModuleDefinition)),
                ]);
    }

    private static ElmValue EncodeModule(Module module)
    {
        return module switch
        {
            Module.NormalModule moduleData =>
            ElmValue.TagInstance(
                "NormalModule",
                [new ElmValue.ElmRecord(
                    [
                    ("exposingList",
                    EncodeNode(EncodeExposing, moduleData.ModuleData.ExposingList)),

                    ("moduleName",
                    EncodeNode(EncodeModuleName, moduleData.ModuleData.ModuleName)),
                    ])
                ]),

            _ =>
            throw new NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };
    }

    private static ElmValue EncodeModuleName(IReadOnlyList<string> moduleName)
    {
        return
            new ElmValue.ElmList(
                [.. moduleName.Select(ElmValue.StringInstance)]);
    }

    private static ElmValue EncodeExposing(Exposing exposing)
    {
        return exposing switch
        {
            Exposing.All range =>
                ElmValue.TagInstance(
                    "All",
                    [EncodeRange(range.Range)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected exposing type: " + exposing.GetType().Name),
        };
    }

    private static ElmValue EncodeDeclaration(Declaration declaration)
    {
        return declaration switch
        {
            Declaration.FunctionDeclaration functionDeclaration =>
                ElmValue.TagInstance(
                    "FunctionDeclaration",
                    [EncodeFunction(functionDeclaration.Function)]),

            Declaration.CustomTypeDeclaration typeDeclaration =>
                ElmValue.TagInstance(
                    "CustomTypeDeclaration",
                    [EncodeTypeStruct(typeDeclaration.TypeDeclaration)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected declaration type: " + declaration.GetType().Name),
        };
    }

    private static ElmValue EncodeTypeStruct(TypeStruct type)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("constructors",
                ElmValue.ListInstance(
                    [..type.Constructors.Select(c => EncodeNode(EncodeValueConstructor, c))])),

                ("documentation",
                ElmValue.TagInstance("Nothing",[])),

                ("generics",
                ElmValue.ListInstance(
                    [..type.Generics.Select(g => EncodeNode(EncodeString, g))])),

                ("name",
                EncodeNode(EncodeString, type.TypeName)),
                ]);
    }

    private static ElmValue EncodeValueConstructor(ValueConstructor constructor)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("arguments",
                ElmValue.ListInstance(
                    [..constructor.Arguments.Select(a => EncodeNode(EncodeTypeAnnotation, a))])),

                ("name",
                EncodeNode(EncodeString, constructor.Name)),
                ]);
    }

    private static ElmValue EncodeTypeAnnotation(TypeAnnotation type)
    {
        return type switch
        {
            // | GenericType String
            TypeAnnotation.GenericType name =>
                ElmValue.TagInstance(
                    "GenericType",
                    [EncodeString(name.Name)]),

            TypeAnnotation.Typed typeName =>
            // | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
                ElmValue.TagInstance(
                    "Typed",
                    [
                        EncodeNode(
                            aggregateName =>
                            ElmValue.ListInstance(
                                [
                                EncodeModuleName(aggregateName.ModuleName),
                                EncodeString(aggregateName.Name),
                                ]),
                            typeName.TypeName),

                        ElmValue.ListInstance(
                            [..typeName.TypeArguments.Select(a => EncodeNode(EncodeTypeAnnotation, a))]),
                    ]),

            TypeAnnotation.Tupled tupled =>
            // | Tupled (List (Node TypeAnnotation))
                ElmValue.TagInstance(
                    "Tupled",
                    [ElmValue.ListInstance(
                        [..tupled.TypeAnnotations.Select(a => EncodeNode(EncodeTypeAnnotation, a))])]),

            _ =>
            throw new NotImplementedException(
                "Unexpected type annotation type: " + type.GetType().Name),
        };
    }

    private static ElmValue EncodeFunction(FunctionStruct function)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("declaration",
                EncodeNode(EncodeFunctionImplementation, function.Declaration)),

                ("documentation",
                ElmValue.TagInstance("Nothing",[])),

                ("signature",
                ElmValue.TagInstance("Nothing",[])),
                ]);
    }

    private static ElmValue EncodeFunctionImplementation(FunctionImplementation function)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("arguments",
                ElmValue.ListInstance(
                    [..function.Arguments.Select(a => EncodeNode(EncodePattern, a))])),

                ("expression",
                EncodeNode(EncodeExpression, function.Expression)),

                ("name",
                EncodeNode(EncodeString, function.Name)),
                ]);
    }

    private static ElmValue EncodePattern(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.VarPattern name =>
                ElmValue.TagInstance(
                    "VarPattern",
                    [EncodeString(name.Name)]),

            _ =>
            throw new NotImplementedException(
                "Unexpected pattern type: " + pattern.GetType().Name),
        };
    }

    private static ElmValue EncodeExpression(Expression expression)
    {
        return expression switch
        {
            // | Literal String
            Expression.Literal value =>
                ElmValue.TagInstance(
                    "Literal",
                    [EncodeString(value.Value)]),

            // | ListExpr (List (Node Expression))
            Expression.ListExpr elements =>
                ElmValue.TagInstance(
                    "ListExpr",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodeExpression, e))])]),

            // | FunctionOrValue ModuleName String
            Expression.FunctionOrValue functionOrValue =>
                ElmValue.TagInstance(
                    "FunctionOrValue",
                    [
                        EncodeModuleName(functionOrValue.ModuleName),
                        EncodeString(functionOrValue.Name),
                    ]),

            // | Application (List (Node Expression))
            Expression.Application arguments =>
                ElmValue.TagInstance(
                    "Application",
                    [ElmValue.ListInstance(
                        [..arguments.Arguments.Select(a => EncodeNode(EncodeExpression, a))])]),

            // | ParenthesizedExpression (Node Expression)
            Expression.ParenthesizedExpression expressionNode =>
                ElmValue.TagInstance(
                    "ParenthesizedExpression",
                    [EncodeNode(EncodeExpression, expressionNode.Expression)]),

            // | TupledExpression (List (Node Expression))
            Expression.TupledExpression elements =>
                ElmValue.TagInstance(
                    "TupledExpression",
                    [ElmValue.ListInstance(
                        [..elements.Elements.Select(e => EncodeNode(EncodeExpression, e))])]),

            // | RecordExpr (List (Node RecordSetter))
            Expression.RecordExpr fields =>
                ElmValue.TagInstance(
                    "RecordExpr",
                    [ElmValue.ListInstance(
                        [..fields.Fields.Select(rs => EncodeNode(EncodeRecordSetter, rs))])]),

            _ =>
                throw new NotImplementedException(
                    "Unexpected expression type: " + expression.GetType().Name),
        };
    }

    private static ElmValue EncodeRecordSetter(
        (Node<string> fieldName, Node<Expression> expr) field)
    {
        return
            ElmValue.ListInstance(
                [
                EncodeNode(EncodeString, field.fieldName),
                EncodeNode(EncodeExpression, field.expr),
                ]);
    }

    private static ElmValue EncodeString(string value)
    {
        return ElmValue.StringInstance(value);
    }

    private static ElmValue EncodeNode<T>(
        Func<T, ElmValue> encodeValue,
        Node<T> node)
    {
        return
            ElmValue.TagInstance(
                "Node",
                [
                EncodeRange(node.Range),
                encodeValue(node.Value),
                ]);
    }

    private static ElmValue EncodeRange(Range range)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("end", EncodeLocation(range.End)),
                ("start", EncodeLocation(range.Start)),
                ]);
    }

    private static ElmValue EncodeLocation(Location location)
    {
        return
            new ElmValue.ElmRecord(
                [
                ("column", ElmValue.Integer(location.Column)),
                ("row", ElmValue.Integer(location.Row)),
            ]);
    }

    public record Token(
        TokenType Type,
        string Lexeme,
        Location Start,
        Location End);

    public enum TokenType
    {
        Identifier,
        StringLiteral,
        NumberLiteral,
        OpenParen,
        CloseParen,
        OpenBrace,
        CloseBrace,
        OpenBracket,
        CloseBracket,
        Comma,
        Dot,
        Equal,
        Arrow,
        Pipe,
        Comment,
        Whitespace,
        Newline,
        Unknown,
    }

    public class Tokenizer(string input)
    {
        private readonly string _input = input;
        private int _position;
        private int _line = 1;
        private int _column = 1;

        public IEnumerable<Token> Tokenize()
        {
            while (!IsAtEnd())
            {
                var token = NextToken();
             
                if (token != null)
                {
                    yield return token;
                }
            }
        }

        private bool IsAtEnd() =>
            _position >= _input.Length;

        private char Peek() =>
            IsAtEnd()
            ?
            '\0'
            :
            _input[_position];

        private char PeekNext() =>
            (_position + 1 < _input.Length)
            ?
            _input[_position + 1]
            :
            '\0';

        private char Advance()
        {
            char current = _input[_position];
            _position++;

            if (current == '\n')
            {
                _line++;
                _column = 1;
            }
            else
            {
                _column++;
            }

            return current;
        }

        private Token NextToken()
        {
            // Capture the start location for this token.
            Location start = new(_line, _column);
            char current = Peek();

            // Handle whitespace and newlines
            if (char.IsWhiteSpace(current))
            {
                if (current == '\n')
                {
                    Advance();
                    Location end = new(_line, _column);
                    return new Token(TokenType.Newline, "\n", start, end);
                }
                else
                {
                    string whitespace = "";
                    while (!IsAtEnd() && char.IsWhiteSpace(Peek()) && Peek() != '\n')
                    {
                        whitespace += Advance();
                    }
                    Location end = new(_line, _column);
                    return new Token(TokenType.Whitespace, whitespace, start, end);
                }
            }

            // Handle comments (for example, starting with "//")
            if (current == '/' && PeekNext() == '/')
            {
                string comment = "";
                while (!IsAtEnd() && Peek() != '\n')
                {
                    comment += Advance();
                }
                Location end = new(_line, _column);
                return new Token(TokenType.Comment, comment, start, end);
            }

            // Handle string literals
            if (current == '"')
            {
                Advance(); // Consume the opening quote
                StringBuilder sb = new();
                while (!IsAtEnd() && Peek() != '"')
                {
                    // If we see a backslash, check for an escaped character.
                    if (Peek() == '\\')
                    {
                        Advance(); // Consume the backslash
                        if (!IsAtEnd())
                        {
                            char escaped = Advance(); // Consume the character after the backslash
                                                      // Handle specific escape sequences
                            if (escaped == '"')
                            {
                                sb.Append('"'); // Escaped quote
                            }
                            else if (escaped == 'n')
                            {
                                sb.Append('\n'); // Newline escape
                            }
                            else if (escaped == 't')
                            {
                                sb.Append('\t'); // Tab escape
                            }
                            else if (escaped == '\\')
                            {
                                sb.Append('\\'); // Backslash escape
                            }
                            else
                            {
                                // For any other escape, just append the character (or handle error)
                                sb.Append(escaped);
                            }
                        }
                    }
                    else
                    {
                        sb.Append(Advance());
                    }
                }
                if (IsAtEnd())
                {
                    // Unterminated string literal; here you might want to throw an error.
                    return new Token(TokenType.Unknown, sb.ToString(), start, new Location(_line, _column));
                }
                Advance(); // Consume the closing quote
                Location end = new(_line, _column);
                return new Token(TokenType.StringLiteral, sb.ToString(), start, end);
            }

            // Handle number literals
            if (char.IsDigit(current))
            {
                string number = "";
                while (!IsAtEnd() && char.IsDigit(Peek()))
                {
                    number += Advance();
                }
                // Optionally handle fractional parts
                if (!IsAtEnd() && Peek() == '.')
                {
                    number += Advance(); // Consume the dot
                    while (!IsAtEnd() && char.IsDigit(Peek()))
                    {
                        number += Advance();
                    }
                }
                Location end = new(_line, _column);
                return new Token(TokenType.NumberLiteral, number, start, end);
            }

            // Handle identifiers (start with letter or underscore, then letters, digits or underscores)
            if (char.IsLetter(current) || current == '_')
            {
                string identifier = "";
                while (!IsAtEnd() && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
                {
                    identifier += Advance();
                }
                Location end = new(_line, _column);
                return new Token(TokenType.Identifier, identifier, start, end);
            }

            // Handle single-character tokens and multi-character tokens like the arrow "->"
            switch (current)
            {
                case '(':
                    Advance();
                    return new Token(TokenType.OpenParen, "(", start, new Location(_line, _column));
                case ')':
                    Advance();
                    return new Token(TokenType.CloseParen, ")", start, new Location(_line, _column));
                case '{':
                    Advance();
                    return new Token(TokenType.OpenBrace, "{", start, new Location(_line, _column));
                case '}':
                    Advance();
                    return new Token(TokenType.CloseBrace, "}", start, new Location(_line, _column));
                case '[':
                    Advance();
                    return new Token(TokenType.OpenBracket, "[", start, new Location(_line, _column));
                case ']':
                    Advance();
                    return new Token(TokenType.CloseBracket, "]", start, new Location(_line, _column));
                case ',':
                    Advance();
                    return new Token(TokenType.Comma, ",", start, new Location(_line, _column));
                case '.':
                    Advance();
                    return new Token(TokenType.Dot, ".", start, new Location(_line, _column));
                case '=':
                    Advance();
                    return new Token(TokenType.Equal, "=", start, new Location(_line, _column));
                case '|':
                    Advance();
                    return new Token(TokenType.Pipe, "|", start, new Location(_line, _column));
                case '-':
                    // Check if this is part of an arrow token "->"
                    if (PeekNext() == '>')
                    {
                        Advance(); // Consume '-'
                        Advance(); // Consume '>'
                        return new Token(TokenType.Arrow, "->", start, new Location(_line, _column));
                    }
                    else
                    {
                        Advance();
                        return new Token(TokenType.Unknown, "-", start, new Location(_line, _column));
                    }
                default:
                    // For any unknown character, consume it and return an Unknown token.
                    Advance();
                    return new Token(TokenType.Unknown, current.ToString(), start, new Location(_line, _column));
            }
        }
    }

    private class Parser(ReadOnlyMemory<Token> tokens)
    {
        private int _current = 0;

        // Entry point: parse the entire file and return a File record.
        public Result<string, File> ParseFile()
        {
            try
            {
                ConsumeAllWhitespace(true);

                // Parse the module header
                Node<Module> moduleDefinition = ParseModule();

                // Parse the declarations until we've consumed all tokens.
                var declarations = new List<Node<Declaration>>();

                ConsumeAllWhitespace(true);

                while (!IsAtEnd())
                {
                    declarations.Add(ParseDeclaration());

                    ConsumeAllWhitespace(true);
                }

                return Result<string, File>.ok(new File(moduleDefinition, declarations));
            }
            catch (Exception ex)
            {
                return Result<string, File>.err(ex.Message);
            }
        }

        // Parses the module header and returns a Node<Module>
        private Node<Module> ParseModule()
        {
            // Expect the "module" keyword (this could be a token with Type Identifier "module")

            var keywordToken = ConsumeKeyword("module");

            ConsumeAllWhitespace(true);

            // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)
            var moduleNameParts = new List<string>();

            var firstModuleNamePart =
                ConsumeAnyIdentifier("module name");

            moduleNameParts.Add(firstModuleNamePart.Lexeme);

            while (Peek().Type is TokenType.Dot)
            {
                Consume(TokenType.Dot);

                var moduleNamePart = ConsumeAnyIdentifier("module name part");

                moduleNameParts.Add(moduleNamePart.Lexeme);
            }

            // Create a Node<IReadOnlyList<string>> for the module name.
            var moduleNameNode =
                new Node<IReadOnlyList<string>>(
                    new Range(firstModuleNamePart.Start, Peek().Start),
                    moduleNameParts.AsReadOnly());

            // Parse the exposing clause (here we use a dummy implementation).
            Node<Exposing> exposingNode = ParseExposing();

            // Build the module data and wrap it in a Module.NormalModule.
            var moduleData = new DefaultModuleData(moduleNameNode, exposingNode);
            var moduleNodeValue = new Module.NormalModule(moduleData);
            var moduleNode = new Node<Module>(new Range(keywordToken.Start, Peek().Start), moduleNodeValue);
            return moduleNode;
        }

        private Node<Exposing> ParseExposing()
        {
            ConsumeAllWhitespace(true);

            var keyword = ConsumeKeyword("exposing");

            ConsumeAllWhitespace(false);

            Consume(TokenType.OpenParen);

            ConsumeAllWhitespace(true);

            if (Peek().Type is TokenType.Dot)
            {
                var allFirstDot = Consume(TokenType.Dot);

                // Next token should be a dot as well

                var allSecondDot = Consume(TokenType.Dot);

                ConsumeAllWhitespace(true);

                var closeParen = Consume(TokenType.CloseParen);

                return new Node<Exposing>(
                    new Range(keyword.Start, closeParen.End),
                    new Exposing.All(new Range(allFirstDot.Start, allSecondDot.End)));
            }

            throw ExceptionForCurrentLocation("Unsupported exposing clause.");
        }

        // Parses a declaration (e.g. a function declaration)
        private Node<Declaration> ParseDeclaration()
        {
            // For example, a function declaration might start with a function name.
            Token start = Peek();

            Token identifierToken = ConsumeAnyIdentifier("function name");

            if (identifierToken.Lexeme is "type")
            {
                /*
                 * Parse type declaration, like:
                 * 
                type FileTreeNode blobStructure
                    = BlobNode blobStructure
                    | TreeNode (List ( String, FileTreeNode blobStructure ))
                 * */

                ConsumeAllWhitespace(true);

                // Parse type name

                Token typeNameToken = ConsumeAnyIdentifier("type name");

                ConsumeAllWhitespace(true);

                var typeParameters = new List<Node<string>>();

                // Type parameters are optional

                if (Peek().Type is TokenType.Identifier)
                {
                    if (Peek().Lexeme is "alias")
                    {
                        ConsumeKeyword("alias");

                        throw new NotImplementedException(
                            "Type alias not implemented.");
                    }

                    // Parse type parameters
                    while (Peek().Type is TokenType.Identifier)
                    {
                        var typeParameterToken = ConsumeAnyIdentifier("type parameter");

                        typeParameters.Add(
                            new Node<string>(
                                new Range(typeParameterToken.Start, typeParameterToken.End),
                                typeParameterToken.Lexeme));

                        ConsumeAllWhitespace(true);
                    }
                }

                // Parse the equal sign
                Consume(TokenType.Equal);

                ConsumeAllWhitespace(true);

                // Parse the type definition

                var constructors = new List<Node<ValueConstructor>>();

                while (true)
                {
                    ConsumeAllWhitespace(true);
                    var constructorNameToken = ConsumeAnyIdentifier("constructor name");

                    ConsumeAllWhitespace(true);

                    var constructorArguments = new List<Node<TypeAnnotation>>();

                    while (
                        (Peek().Type is TokenType.Identifier || Peek().Type is TokenType.OpenParen) &&
                        constructorNameToken.Start.Column <= Peek().Start.Column)
                    {
                        var argumentAnnotation =
                            ParseTypeAnnotation(constructorNameToken.Start.Column);

                        constructorArguments.Add(argumentAnnotation);
                    }

                    var constructorEnd =
                        constructorArguments.Count is 0
                        ?
                        constructorNameToken.End
                        :
                        constructorArguments.Last().Range.End;

                    constructors.Add(
                        new Node<ValueConstructor>(
                            new Range(constructorNameToken.Start, constructorEnd),
                            new ValueConstructor(
                                new Node<string>(
                                    new Range(constructorNameToken.Start, constructorNameToken.End),
                                    constructorNameToken.Lexeme),
                                constructorArguments)));

                    ConsumeAllWhitespace(true);

                    if (Peek().Type is TokenType.Pipe)
                    {
                        Consume(TokenType.Pipe);
                    }
                    else
                    {
                        break;
                    }
                }

                return
                    new Node<Declaration>(
                        new Range(start.Start, constructors.Last().Range.End),
                        new Declaration.CustomTypeDeclaration(
                            new TypeStruct(
                                new Node<string>(
                                    new Range(typeNameToken.Start, typeNameToken.End),
                                    typeNameToken.Lexeme),
                                typeParameters,
                                constructors)));
            }

            ConsumeAllWhitespace(true);

            // In a full implementation, you would parse parameters here.
            // Expect an '=' token.
            Consume(TokenType.Equal);

            ConsumeAllWhitespace(true);

            // Parse an expression (for now, assume it is a literal expression).
            Node<Expression> expression = ParseExpression();

            // Build a FunctionImplementation node.
            var functionImpl = new FunctionImplementation(
                new Node<string>(new Range(start.Start, start.End), identifierToken.Lexeme),
                [], // No arguments in this simple example
                expression);

            var functionStruct = new FunctionStruct(
                new Node<FunctionImplementation>(new Range(start.Start, expression.Range.End), functionImpl));

            // Wrap it in a Declaration.FunctionDeclaration.
            var declaration = new Declaration.FunctionDeclaration(functionStruct);
            return new Node<Declaration>(new Range(start.Start, expression.Range.End), declaration);
        }

        private Node<TypeAnnotation> ParseTypeAnnotation(
            int minIndent)
        {
            Token start = Peek();

            if (start.Type is TokenType.OpenParen)
            {
                // Is either Tupled or Typed

                var openToken = Consume(TokenType.OpenParen);

                ConsumeAllWhitespace(true);

                var firstTypeAnnotation = ParseTypeAnnotation(minIndent);

                ConsumeAllWhitespace(true);

                if (Peek().Type is TokenType.Comma)
                {
                    // | Tupled (List (Node TypeAnnotation))

                    Consume(TokenType.Comma);

                    ConsumeAllWhitespace(true);

                    var tupleItems = new List<Node<TypeAnnotation>>
                    {
                        firstTypeAnnotation
                    };

                    while (Peek().Type is not TokenType.CloseParen)
                    {
                        var typeAnnotation =
                            ParseTypeAnnotation(minIndent);

                        tupleItems.Add(typeAnnotation);

                        ConsumeAllWhitespace(true);
                    }

                    var closingToken = Consume(TokenType.CloseParen);

                    var range =
                        new Range(openToken.Start, closingToken.End);

                    if (tupleItems.Count is 1)
                    {
                        return tupleItems[0];
                    }

                    return
                        new Node<TypeAnnotation>(
                            range,
                            new TypeAnnotation.Tupled(tupleItems));
                }
                else
                {
                    // | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))

                    if (firstTypeAnnotation.Value is not TypeAnnotation.Typed instantiatedName)
                    {
                        throw ExceptionForCurrentLocation(
                            "Expected Typed type annotation.");
                    }

                    var arguments = new List<Node<TypeAnnotation>>();

                    while (
                        Peek().Type is not TokenType.CloseParen &&
                        minIndent <= Peek().Start.Column)
                    {
                        var argumentAnnotation =
                            ParseTypeAnnotation(minIndent);

                        arguments.Add(argumentAnnotation);

                        ConsumeAllWhitespace(true);
                    }

                    var closingToken = Consume(TokenType.CloseParen);

                    var range =
                        new Range(openToken.Start, closingToken.End);

                    if (arguments.Count is 0)
                    {
                        return
                            new Node<TypeAnnotation>(
                                range,
                                firstTypeAnnotation.Value);
                    }

                    return
                        new Node<TypeAnnotation>(
                            range,
                            new TypeAnnotation.Typed(
                                instantiatedName.TypeName,
                                arguments));
                }
            }

            if (start.Type is TokenType.Identifier)
            {
                var firstIdentifierToken =
                    ConsumeAnyIdentifier("first identifier");

                if (char.IsLower(firstIdentifierToken.Lexeme.First()))
                {
                    // GenericType String

                    return new Node<TypeAnnotation>(
                        new Range(start.Start, start.End),
                        new TypeAnnotation.GenericType(firstIdentifierToken.Lexeme));
                }

                // Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))

                // https://github.com/stil4m/elm-syntax/blob/c99a05ac96d3fa15fb3a8dc5ca39eaf78d1e510a/src/Elm/Parser/TypeAnnotation.elm#L336-L357

                var namespaces = new List<Token>();

                while (Peek().Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

                    namespaces.Add(namespaceToken);
                }

                ConsumeAllWhitespace(true);

                IReadOnlyList<Token> moduleName =
                    [.. namespaces.Prepend(firstIdentifierToken).SkipLast(1)];

                var typeNameToken =
                    namespaces.LastOrDefault() ?? firstIdentifierToken;

                var typeArguments =
                    new List<Node<TypeAnnotation>>();

                while (
                    !IsAtEnd() &&
                    firstIdentifierToken.Start.Column <= Peek().Start.Column &&
                    Peek().Type is not TokenType.Comma &&
                    Peek().Type is not TokenType.CloseParen &&
                    Peek().Type is not TokenType.CloseBracket &&
                    Peek().Type is not TokenType.CloseBrace)
                {
                    var typeArgument =
                        ParseTypeAnnotation(minIndent);

                    typeArguments.Add(typeArgument);

                    ConsumeAllWhitespace(true);
                }

                ConsumeAllWhitespace(true);

                var rangeEnd =
                    typeArguments.Count is 0
                    ?
                    typeNameToken.End
                    :
                    typeArguments.Last().Range.End;

                var range =
                    new Range(start.Start, rangeEnd);

                var instantiatedRangeStart =
                    moduleName.Count is 0
                    ?
                    typeNameToken.Start
                    :
                    moduleName[0].Start;

                return
                    new Node<TypeAnnotation>(
                        range,
                        new TypeAnnotation.Typed(
                            new Node<(IReadOnlyList<string> ModuleName, string Name)>(
                                new Range(instantiatedRangeStart, typeNameToken.End),
                                (
                                [.. moduleName.Select(t => t.Lexeme)],
                                typeNameToken.Lexeme)),
                            typeArguments));
            }

            throw ExceptionForCurrentLocation(
                "Unsupported type annotation type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Expression> ParseExpression()
        {
            Token start = Peek();

            if (start.Type is TokenType.StringLiteral)
            {
                string literal = start.Lexeme;

                Advance();

                var literalExpr = new Expression.Literal(literal);

                return new Node<Expression>(new Range(start.Start, start.End), literalExpr);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                return ParseRecordExpr();
            }

            if (start.Type is TokenType.Identifier)
            {
                var firstIdentifierToken = ConsumeAnyIdentifier("first identifier");

                // | FunctionOrValue ModuleName String
                // | Application (List (Node Expression))

                if (char.IsLower(firstIdentifierToken.Lexeme[0]) && Peek().Type is TokenType.Dot)
                {
                    // | RecordAccess (Node Expression) (Node String)

                    throw new NotImplementedException(
                        "Record access not implemented.");
                }

                var furtherIdentifiers = new List<Token>();

                while (Peek().Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var furtherNamePart =
                        ConsumeAnyIdentifier("function or value name part");

                    furtherIdentifiers.Add(furtherNamePart);
                }

                IReadOnlyList<Token> aggregateNameParts =
                    [firstIdentifierToken, .. furtherIdentifiers];

                var firstExpr =
                    new Node<Expression>(
                        new Range(
                            firstIdentifierToken.Start,
                            aggregateNameParts[aggregateNameParts.Count - 1].End),
                        new Expression.FunctionOrValue(
                            [.. aggregateNameParts.SkipLast(1).Select(t => t.Lexeme)],
                            aggregateNameParts[aggregateNameParts.Count - 1].Lexeme));

                ConsumeAllWhitespace(true);

                // Following expressions are arguments if indented at least as much as the first identifier

                var argumentIndentMin =
                    aggregateNameParts[0].Start.Column;

                var argumentsNodes =
                    new List<Node<Expression>>();

                while (
                    !IsAtEnd() &&
                    argumentIndentMin <= Peek().Start.Column &&
                    Peek().Type is not TokenType.Comma &&
                    Peek().Type is not TokenType.CloseParen &&
                    Peek().Type is not TokenType.CloseBrace)
                {
                    var argumentExpr = ParseExpression();

                    argumentsNodes.Add(argumentExpr);

                    ConsumeAllWhitespace(true);
                }

                if (0 < argumentsNodes.Count)
                {
                    var applicationRange =
                        new Range(
                            aggregateNameParts[0].Start,
                            argumentsNodes.Last().Range.End);

                    var applicationExpr =
                        new Expression.Application([firstExpr, .. argumentsNodes]);

                    firstExpr =
                        new Node<Expression>(
                            applicationRange,
                            applicationExpr);
                }

                return firstExpr;
            }

            if (start.Type is TokenType.OpenBracket)
            {
                // | ListExpr (List (Node Expression))

                var listOpenToken =
                    Consume(TokenType.OpenBracket);

                ConsumeAllWhitespace(true);

                var elements = new List<Node<Expression>>();

                while (Peek().Type is not TokenType.CloseBracket)
                {
                    var elementExpr = ParseExpression();

                    elements.Add(elementExpr);

                    ConsumeAllWhitespace(true);

                    if (Peek().Type is TokenType.Comma)
                    {
                        Consume(TokenType.Comma);
                    }
                }

                var listCloseToken =
                    Consume(TokenType.CloseBracket);

                var listRange =
                    new Range(listOpenToken.Start, listCloseToken.End);

                var listExpr =
                    new Expression.ListExpr(elements);

                return new Node<Expression>(listRange, listExpr);
            }

            if (start.Type is TokenType.OpenParen)
            {
                /*
                 * Can be either of:
                 * | TupledExpression (List (Node Expression))
                 * | ParenthesizedExpression (Node Expression)
                 * */

                var parenOpenToken =
                    Consume(TokenType.OpenParen);

                ConsumeAllWhitespace(true);

                var firstItemExpr = ParseExpression();

                ConsumeAllWhitespace(true);

                var furtherItems = new List<Node<Expression>>();

                while (Peek().Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);

                    ConsumeAllWhitespace(true);

                    var furtherItemExpr = ParseExpression();

                    furtherItems.Add(furtherItemExpr);

                    ConsumeAllWhitespace(true);
                }

                var parenCloseToken =
                    Consume(TokenType.CloseParen);

                var parenRange =
                    new Range(parenOpenToken.Start, parenCloseToken.End);

                if (furtherItems.Count is 0)
                {
                    var parenthesizedExpr =
                        new Expression.ParenthesizedExpression(firstItemExpr);

                    return new Node<Expression>(parenRange, parenthesizedExpr);
                }

                var tupledExpr =
                    new Expression.TupledExpression(
                        [firstItemExpr, .. furtherItems]);

                return new Node<Expression>(parenRange, tupledExpr);
            }

            throw ExceptionForCurrentLocation(
                "Unsupported expression type: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<Expression> ParseRecordExpr()
        {
            Token start = Peek();

            Consume(TokenType.OpenBrace);

            var fields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

            ConsumeAllWhitespace(true);

            while (Peek().Type is not TokenType.CloseBrace)
            {
                ConsumeAllWhitespace(true);

                var fieldName = ConsumeAnyIdentifier("field name");

                ConsumeAllWhitespace(true);

                Consume(TokenType.Equal);

                ConsumeAllWhitespace(true);

                var valueExpr = ParseExpression();

                fields.Add(
                    new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    new Range(fieldName.Start, valueExpr.Range.End),
                    (new Node<string>(new Range(fieldName.Start, fieldName.End), fieldName.Lexeme), valueExpr)));

                ConsumeAllWhitespace(true);

                if (Peek().Type is TokenType.Comma)
                {
                    Consume(TokenType.Comma);
                }
            }

            var end = Peek();

            Consume(TokenType.CloseBrace);

            return new Node<Expression>(
                new Range(start.Start, end.End),
                new Expression.RecordExpr(fields));
        }

        // Helper methods

        private bool IsAtEnd() =>
            _current >= tokens.Length;

        private Token Peek() =>
            tokens.Span[_current];

        private Token Advance() =>
            tokens.Span[_current++];

        private Token ConsumeKeyword(string expectedLexeme)
        {
            return
                Consume(TokenType.Identifier, expectedLexeme);
        }

        private Token ConsumeAnyIdentifier(string description)
        {
            return
                Consume(
                    TokenType.Identifier,
                    expectedLexeme: null,
                    tokenDescription: description);
        }

        // Consume a token of a given type, throwing an error if the token does not match.
        private Token Consume(
            TokenType expectedType,
            string? expectedLexeme = null,
            string? tokenDescription = null)
        {
            var nextToken = Peek();

            if (nextToken.Type != expectedType)
            {
                throw new ElmSyntaxParserException(
                    "Expected " + (tokenDescription ?? "token") + " of type " + expectedType +
                    " but found " + nextToken.Type,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
            }

            if (expectedLexeme is not null && nextToken.Lexeme != expectedLexeme)
            {
                var errorDescription =
                    (expectedType is TokenType.Identifier
                    ?
                    "Expected keyword '" + expectedLexeme
                    :
                    "Expected token with lexeme " + expectedLexeme) +
                    "' but found '" + nextToken.Lexeme + "'";

                throw new ElmSyntaxParserException(
                    errorDescription,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
            }

            return Advance();
        }

        private ElmSyntaxParserException ExceptionForCurrentLocation(string message)
        {
            var token =
                IsAtEnd() ?
                tokens.Span[tokens.Length - 1] :
                Peek();

            return new ElmSyntaxParserException(
                message,
                lineNumber: token.Start.Row,
                columnNumber: token.Start.Column);
        }

        private IReadOnlyList<Token> ConsumeAllWhitespace(
            bool includingNewline)
        {
            return [.. ConsumeAllWhitespaceLazy(includingNewline)];
        }

        private IEnumerable<Token> ConsumeAllWhitespaceLazy(
            bool includingNewline)
        {
            bool tokenMatches(Token token) =>
                (token.Type is TokenType.Whitespace)
                ||
                (includingNewline && token.Type is TokenType.Newline);

            while (!IsAtEnd() && tokenMatches(Peek()))
            {
                yield return Advance();
            }
        }
    }

    private static Result<string, File> ParseModuleText(
        string elmModuleText)
    {
        var tokenizer = new Tokenizer(elmModuleText);

        var tokens = tokenizer.Tokenize().ToArray();

        var parser = new Parser(tokens);

        return parser.ParseFile();
    }
}
