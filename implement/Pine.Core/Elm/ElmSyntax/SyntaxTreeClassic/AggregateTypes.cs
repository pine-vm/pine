using System.Collections.Generic;

/*
 * Types for modelling parsed concrete syntax, based on
 * https://github.com/stil4m/elm-syntax/tree/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax
 * */

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic
{
    /// <summary>
    /// Wrapper pairing a value with its source range in the parsed text.
    /// </summary>
    public record Node<T>(
        Range Range,
        T Value)
    {
        /// <summary>
        /// Casts the contained value to another type without changing range.
        /// </summary>
        public Node<TOther> Cast<TOther>()
        {
            return new Node<TOther>(Range, (TOther)(object)Value);
        }
    }

    /// <summary>
    /// Inclusive start / exclusive end positions spanning a source fragment.
    /// </summary>
    public record Range(
        Location Start,
        Location End);

    /// <summary>
    /// One-based line and column within source text.
    /// </summary>
    public record Location(
        int Row,
        int Column);

    /// <summary>
    /// Root of an Elm source file: module definition, imports, top declarations and comments.
    /// </summary>
    public record File(
        Node<Module> ModuleDefinition,
        IReadOnlyList<Node<Import>> Imports,
        IReadOnlyList<Node<Declaration>> Declarations,
        IReadOnlyList<Node<string>> Comments);

    /// <summary>
    /// Elm import statement containing target module name, optional alias and exposing list.
    /// </summary>
    public record Import(
        Node<ModuleName> ModuleName,
        Node<ModuleName>? ModuleAlias,
        Node<Exposing>? ExposingList);

    /// <summary>
    /// Elm module kinds: normal, port or effect (with possible commands/subscriptions).
    /// </summary>
    public abstract record Module
    {
        /// <summary>Standard module.</summary>
        public sealed record NormalModule(
            DefaultModuleData ModuleData)
            : Module;

        /// <summary>Port module exposing native interop.</summary>
        public sealed record PortModule(
            DefaultModuleData ModuleData)
            : Module;

        /// <summary>Effect module with commands and subscriptions.</summary>
        public sealed record EffectModule(
            EffectModuleData ModuleData)
            : Module;

        /// <summary>
        /// Retrieve the module name syntax node for the specified module instance.
        /// </summary>
        public static Node<ModuleName> GetModuleName(Module module) =>
            module switch
            {
                NormalModule normalModule =>
                    normalModule.ModuleData.ModuleName,

                PortModule portModule =>
                    portModule.ModuleData.ModuleName,

                EffectModule effectModule =>
                    effectModule.ModuleData.ModuleName,

                _ =>
                throw new System.NotImplementedException(
                    "Unexpected module type: " + module.GetType().Name),
            };
    }

    /// <summary>
    /// Shared data for normal/port modules: name and exposing list.
    /// </summary>
    public record DefaultModuleData(
        Node<ModuleName> ModuleName,
        Node<Exposing> ExposingList);

    /// <summary>
    /// Data for effect modules including optional command and subscription identifiers.
    /// </summary>
    public record EffectModuleData(
        Node<ModuleName> ModuleName,
        Node<Exposing> ExposingList,
        Node<string>? Command,
        Node<string>? Subscription);

    /// <summary>
    /// How a module exposes its definitions: all or explicit list.
    /// </summary>
    public abstract record Exposing
    {
        /// <summary>Expose everything (represented by .. in Elm).</summary>
        public sealed record All(
            Range Range)
            : Exposing;

        /// <summary>Explicit list of exposed top-level items.</summary>
        public sealed record Explicit(
            IReadOnlyList<Node<TopLevelExpose>> Nodes)
            : Exposing;
    }

    /// <summary>
    /// Individual item exposed from a module: operators, functions, types, or type with constructors.
    /// </summary>
    public abstract record TopLevelExpose
    {
        /// <summary>Exposed operator (infix).</summary>
        public sealed record InfixExpose(
            string Name)
            : TopLevelExpose;

        /// <summary>Exposed function or value.</summary>
        public sealed record FunctionExpose(
            string Name)
            : TopLevelExpose;

        /// <summary>Exposed type or type alias without constructors.</summary>
        public sealed record TypeOrAliasExpose(
            string Name)
            : TopLevelExpose;

        /// <summary>Exposed custom type including maybe constructors via exposed range.</summary>
        public sealed record TypeExpose(
            ExposedType ExposedType)
            : TopLevelExpose;
    }

    /// <summary>
    /// Custom type exposure specification: name and optional opening range for constructors.
    /// </summary>
    public record ExposedType(
        string Name,
        Range? Open);

    /// <summary>
    /// Top-level declarations in a module (functions, types, aliases, ports, infix directives).
    /// </summary>
    public abstract record Declaration
    {
        /// <summary>Function declaration including implementation.</summary>
        public sealed record FunctionDeclaration(
            FunctionStruct Function)
            : Declaration;

        /// <summary>Custom type declaration.</summary>
        public sealed record CustomTypeDeclaration(
            TypeStruct TypeDeclaration)
            : Declaration;

        /// <summary>Type alias declaration.</summary>
        public sealed record AliasDeclaration(
            TypeAlias TypeAlias)
            : Declaration;

        /// <summary>Port declaration with signature.</summary>
        public sealed record PortDeclaration(
            Signature Signature)
            : Declaration;

        /// <summary>Infix operator metadata declaration.</summary>
        public sealed record InfixDeclaration(
            Infix Infix)
            : Declaration;
    }

    /// <summary>
    /// Infix operator properties: direction, precedence, operator symbol and implementing function.
    /// </summary>
    public record Infix(
        Node<InfixDirection> Direction,
        Node<int> Precedence,
        Node<string> Operator,
        Node<string> FunctionName);

    /// <summary>
    /// Type alias with optional documentation, generics and underlying type annotation.
    /// </summary>
    public record TypeAlias(
        Node<string>? Documentation,
        Node<string> Name,
        IReadOnlyList<Node<string>> Generics,
        Node<TypeAnnotation> TypeAnnotation);

    /// <summary>
    /// Custom type declaration including constructors.
    /// </summary>
    public record TypeStruct(
        Node<string>? Documentation,
        Node<string> Name,
        IReadOnlyList<Node<string>> Generics,
        IReadOnlyList<Node<ValueConstructor>> Constructors);

    /// <summary>
    /// Constructor for a custom type with argument type annotations.
    /// </summary>
    public record ValueConstructor(
        Node<string> Name,
        IReadOnlyList<Node<TypeAnnotation>> Arguments);

    /// <summary>
    /// Elm type annotations: generics, concrete typed names, tuples, records, functions, etc.
    /// </summary>
    public abstract record TypeAnnotation
    {
        /// <summary>Reference to a generic type variable.</summary>
        public sealed record GenericType(
            string Name)
            : TypeAnnotation;

        /// <summary>Concrete type optionally namespaced and with arguments.</summary>
        public sealed record Typed(
            Node<(ModuleName ModuleName, string Name)> TypeName,
            IReadOnlyList<Node<TypeAnnotation>> TypeArguments)
            : TypeAnnotation;

        /// <summary>Unit type annotation.</summary>
        public sealed record Unit
            : TypeAnnotation;

        /// <summary>Tuple type annotation.</summary>
        public sealed record Tupled(
            IReadOnlyList<Node<TypeAnnotation>> TypeAnnotations)
            : TypeAnnotation;

        /// <summary>Record type annotation.</summary>
        public sealed record Record(
            RecordDefinition RecordDefinition)
            : TypeAnnotation;

        /// <summary>Record type annotation that extends a generic record.</summary>
        public sealed record GenericRecord(
            Node<string> GenericName,
            Node<RecordDefinition> RecordDefinition)
            : TypeAnnotation;

        /// <summary>Function type annotation mapping argument to return.</summary>
        public sealed record FunctionTypeAnnotation(
            Node<TypeAnnotation> ArgumentType,
            Node<TypeAnnotation> ReturnType)
            : TypeAnnotation;
    }

    /// <summary>
    /// Record type definition listing fields.
    /// </summary>
    public record RecordDefinition(
        IReadOnlyList<Node<RecordField>> Fields);

    /// <summary>
    /// Single record field definition pairing name with type annotation.
    /// </summary>
    public record RecordField(
        Node<string> FieldName,
        Node<TypeAnnotation> FieldType);

    /// <summary>
    /// Function declaration parts: optional docs and signature plus implementation.
    /// </summary>
    public record FunctionStruct(
        Node<string>? Documentation,
        Node<Signature>? Signature,
        Node<FunctionImplementation> Declaration);

    /// <summary>
    /// Function implementation name, argument patterns and body expression.
    /// </summary>
    public record FunctionImplementation(
        Node<string> Name,
        IReadOnlyList<Node<Pattern>> Arguments,
        Node<Expression> Expression);

    /// <summary>
    /// Type signature for a value/function: name and type annotation.
    /// </summary>
    public record Signature(
        Node<string> Name,
        Node<TypeAnnotation> TypeAnnotation);

    /// <summary>
    /// Pattern matching forms used in destructuring and case expressions.
    /// </summary>
    public abstract record Pattern
    {
        /// <summary>Pattern matching any value.</summary>
        public sealed record AllPattern
            : Pattern;

        /// <summary>Pattern binding a value to a variable name.</summary>
        public sealed record VarPattern(
            string Name)
            : Pattern;

        /// <summary>Pattern matching the unit value.</summary>
        public sealed record UnitPattern
            : Pattern;

        /// <summary>Pattern matching a specific character literal.</summary>
        public sealed record CharPattern(
            int Value)
            : Pattern;

        /// <summary>Pattern matching a specific string literal.</summary>
        public sealed record StringPattern(
            string Value)
            : Pattern;

        /// <summary>Pattern matching a decimal integer literal.</summary>
        public sealed record IntPattern(
            long Value)
            : Pattern;

        /// <summary>Pattern matching a hexadecimal integer literal.</summary>
        public sealed record HexPattern(
            long Value)
            : Pattern;

        /// <summary>Pattern matching a floating-point literal.</summary>
        public sealed record FloatPattern(
            float Value)
            : Pattern;

        /// <summary>Pattern matching a tuple.</summary>
        public sealed record TuplePattern(
            IReadOnlyList<Node<Pattern>> Elements)
            : Pattern;

        /// <summary>Pattern matching a record with specified fields.</summary>
        public sealed record RecordPattern(
            IReadOnlyList<Node<string>> Fields)
            : Pattern;

        /// <summary>List cons pattern separating head and tail.</summary>
        public sealed record UnConsPattern(
            Node<Pattern> Head,
            Node<Pattern> Tail)
            : Pattern;

        /// <summary>Pattern matching a list of elements.</summary>
        public sealed record ListPattern(
            IReadOnlyList<Node<Pattern>> Elements)
            : Pattern;

        /// <summary>Pattern matching a named constructor with arguments.</summary>
        public sealed record NamedPattern(
            QualifiedNameRef Name,
            IReadOnlyList<Node<Pattern>> Arguments)
            : Pattern;

        /// <summary>Pattern that aliases a match to a name.</summary>
        public sealed record AsPattern(
            Node<Pattern> Pattern,
            Node<string> Name)
            : Pattern;

        /// <summary>Pattern wrapped in parentheses.</summary>
        public sealed record ParenthesizedPattern(
            Node<Pattern> Pattern)
            : Pattern;
    }

    /// <summary>
    /// Qualified reference to a name with its module path.
    /// </summary>
    public record QualifiedNameRef(
        ModuleName ModuleName,
        string Name);

    /// <summary>
    /// Elm expressions: literals, applications, control flow, data structures, lambda, let, case, etc.
    /// </summary>
    public abstract record Expression
    {
        /// <summary>Expression producing unit.</summary>
        public sealed record UnitExpr
            : Expression;

        /// <summary>String literal expression.</summary>
        public sealed record Literal(
            string Value)
            : Expression;

        /// <summary>Character literal expression.</summary>
        public sealed record CharLiteral(
            int Value)
            : Expression;

        /// <summary>Decimal integer literal expression.</summary>
        public sealed record Integer(
            long Value)
            : Expression;

        /// <summary>Hexadecimal integer literal expression.</summary>
        public sealed record Hex(
            long Value)
            : Expression;

        /// <summary>Floating-point literal expression.</summary>
        public sealed record Floatable(
            double Value)
            : Expression;

        /// <summary>Arithmetic negation of an expression.</summary>
        public sealed record Negation(
            Node<Expression> Expression)
            : Expression;

        /// <summary>List literal expression.</summary>
        public sealed record ListExpr(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        /// <summary>Reference to a function or value.</summary>
        public sealed record FunctionOrValue(
            ModuleName ModuleName,
            string Name)
            : Expression;

        /// <summary>Conditional expression with then/else branches.</summary>
        public sealed record IfBlock(
            Node<Expression> Condition,
            Node<Expression> ThenBlock,
            Node<Expression> ElseBlock)
            : Expression;

        /// <summary>Prefix operator expression.</summary>
        public sealed record PrefixOperator(
            string Operator)
            : Expression;

        /// <summary>Parenthesized subexpression.</summary>
        public sealed record ParenthesizedExpression(
            Node<Expression> Expression)
            : Expression;

        /// <summary>Function application expression.</summary>
        public sealed record Application(
            IReadOnlyList<Node<Expression>> Arguments)
            : Expression;

        /// <summary>Operator application with explicit direction.</summary>
        public sealed record OperatorApplication(
            string Operator,
            InfixDirection Direction,
            Node<Expression> Left,
            Node<Expression> Right)
            : Expression;

        /// <summary>Tuple literal expression.</summary>
        public sealed record TupledExpression(
            IReadOnlyList<Node<Expression>> Elements)
            : Expression;

        /// <summary>Lambda expression.</summary>
        public sealed record LambdaExpression(
            LambdaStruct Lambda)
            : Expression;

        /// <summary>Case expression matching an expression over patterns.</summary>
        public sealed record CaseExpression(
            CaseBlock CaseBlock)
            : Expression;

        /// <summary>Let expression introducing local declarations.</summary>
        public sealed record LetExpression(
            LetBlock Value)
            : Expression;

        /// <summary>
        /// Let block containing declarations and final expression.
        /// </summary>
        public sealed record LetBlock(
            IReadOnlyList<Node<LetDeclaration>> Declarations,
            Node<Expression> Expression);

        /// <summary>
        /// Declarations inside a let: nested functions or destructuring bindings.
        /// </summary>
        public abstract record LetDeclaration
        {
            /// <summary>Local function declaration inside a let block.</summary>
            public sealed record LetFunction(
                FunctionStruct Function)
                : LetDeclaration;

            /// <summary>Local binding via pattern destructuring.</summary>
            public sealed record LetDestructuring(
                Node<Pattern> Pattern,
                Node<Expression> Expression)
                : LetDeclaration;
        }

        /// <summary>Record literal expression.</summary>
        public sealed record RecordExpr(
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
            : Expression;

        /// <summary>Expression accessing a record field.</summary>
        public sealed record RecordAccess(
            Node<Expression> Record,
            Node<string> FieldName)
            : Expression;

        /// <summary>Expression yielding a getter function for a record field.</summary>
        public sealed record RecordAccessFunction(
            string FunctionName)
            : Expression;

        /// <summary>Expression updating a record value.</summary>
        public sealed record RecordUpdateExpression(
            Node<string> RecordName,
            IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
            : Expression;
    }

    /// <summary>
    /// Lambda expression capturing argument patterns and body.
    /// </summary>
    public record LambdaStruct(
        IReadOnlyList<Node<Pattern>> Arguments,
        Node<Expression> Expression);

    /// <summary>
    /// Case expression block containing scrutinee and branches.
    /// </summary>
    public record CaseBlock(
        Node<Expression> Expression,
        IReadOnlyList<Case> Cases);

    /// <summary>
    /// Single case: pattern and resulting expression.
    /// </summary>
    public record Case(
        Node<Pattern> Pattern,
        Node<Expression> Expression);

    /// <summary>
    /// Direction of infix operator association.
    /// </summary>
    public enum InfixDirection
    {
        /// <summary>Left-associative operator.</summary>
        Left,
        /// <summary>Right-associative operator.</summary>
        Right,
        /// <summary>Non-associative operator.</summary>
        Non,
    }
}
