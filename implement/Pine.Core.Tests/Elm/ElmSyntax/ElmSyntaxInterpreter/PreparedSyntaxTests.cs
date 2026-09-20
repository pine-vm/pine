using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

using AbstractDeclaration = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Declaration;
using AbstractJson = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.ElmSyntaxAbstractJson;
using AbstractModule = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Module;
using ConvertFromAbstractConcrete = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.ConvertFromConcrete;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

public class PreparedSyntaxTests
{
    private sealed record LegacyPreparedJson(
        IReadOnlyDictionary<DeclQualifiedName, AbstractDeclaration> Declarations);

    private static DeclQualifiedName Name(string moduleName, string declarationName) =>
        DeclQualifiedName.Create([moduleName], declarationName);

    private static ElmInterpreter.Prepared Prepare(string moduleText) =>
        ElmInterpreter.PrepareModules([moduleText])
        .Extract(err => throw new Exception("Failed to prepare modules: " + err));

    private static string Evaluate(ElmInterpreter.Prepared prepared, string expression) =>
        ElmValue.RenderAsElmExpression(
            ElmInterpreter.InterpretAsElmValue(expression, prepared)
            .Extract(err => throw new Exception(err.ToString())))
        .expressionString;

    private static PineValueInProcess EvaluateInProcess(
        ElmInterpreter.Prepared prepared,
        string declarationName) =>
        ElmInterpreter.Interpret(
            Name("Test", declarationName),
            [],
            ElmInterpreter.BuildResolvers(prepared.Declarations))
        .Extract(err => throw new Exception(err.ToString()));

    private static ElmInterpreter.PreparedExpression.ValueLiteral GetLiteral(
        ElmInterpreter.Prepared prepared,
        string declarationName) =>
        prepared.Declarations[Name("Test", declarationName)]
        .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
        .Subject
        .Function
        .Declaration
        .Expression
        .Should().BeOfType<ElmInterpreter.PreparedExpression.ValueLiteral>()
        .Subject;

    private static ElmInterpreter.PreparedExpression.CaseExpression GetCaseExpression(
        ElmInterpreter.Prepared prepared,
        string declarationName) =>
        prepared.Declarations[Name("Test", declarationName)]
        .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
        .Subject
        .Function
        .Declaration
        .Expression
        .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
        .Subject;

    private static PineValue Tag(string tagName, params PineValue[] arguments) =>
        ElmValueEncoding.TagAsPineValue(tagName, arguments);

    private static PineValue Int(int value) =>
        IntegerEncoding.EncodeSignedInteger(new BigInteger(value));

    private static IReadOnlyDictionary<DeclQualifiedName, AbstractDeclaration> BuildLegacyDeclarations(
        string moduleText)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed to parse module: " + err));

        var abstractFile = ConvertFromAbstractConcrete.FromFile(parsed);
        var moduleName = AbstractModule.GetModuleName(abstractFile.ModuleDefinition);

        return
            abstractFile.Declarations.ToDictionary(
                declaration => declaration switch
                {
                    AbstractDeclaration.FunctionDeclaration functionDeclaration =>
                    DeclQualifiedName.Create(moduleName, functionDeclaration.Function.Declaration.Name),

                    AbstractDeclaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
                    DeclQualifiedName.Create(moduleName, choiceTypeDeclaration.TypeDeclaration.Name),

                    AbstractDeclaration.AliasDeclaration aliasDeclaration =>
                    DeclQualifiedName.Create(moduleName, aliasDeclaration.TypeAlias.Name),

                    AbstractDeclaration.PortDeclaration portDeclaration =>
                    DeclQualifiedName.Create(moduleName, portDeclaration.Signature.Name),

                    AbstractDeclaration.InfixDeclaration infixDeclaration =>
                    DeclQualifiedName.Create(moduleName, infixDeclaration.Infix.Operator),

                    _ =>
                    throw new NotImplementedException(
                        "BuildLegacyDeclarations does not handle declaration variant: " + declaration.GetType().Name),
                });
    }

    private static string BuildLegacyPreparedJson(string moduleText) =>
        AbstractJson.ToJsonString(new LegacyPreparedJson(BuildLegacyDeclarations(moduleText)));

    [Fact]
    public void Prepare_modules_uses_value_literals_and_case_dispatch_segments()
    {
        const string moduleText =
            """
            module Test exposing (..)


            literalValue =
                ()


            classify s =
                case s of
                    "0" ->
                        "zero"

                    "1" ->
                        "one"

                    _ ->
                        "many"


            bindValue s =
                case s of
                    "0" ->
                        "zero"

                    other ->
                        other
            """;

        var prepared = Prepare(moduleText);

        var literalDeclaration =
            prepared.Declarations[Name("Test", "literalValue")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject;

        literalDeclaration.Function.Declaration.Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ValueLiteral>();

        var classifyExpression =
            prepared.Declarations[Name("Test", "classify")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
            .Subject;

        classifyExpression.Dispatch.Steps.Should().HaveCount(2);

        classifyExpression.Dispatch.Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>()
            .Subject
            .Cases
            .Should().HaveCount(2);

        classifyExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.DiscardCase>();

        var bindExpression =
            prepared.Declarations[Name("Test", "bindValue")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
            .Subject;

        bindExpression.Dispatch.Steps.Should().HaveCount(2);

        bindExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.PatternCase>();
    }

    [Fact]
    public void Prepared_value_literals_reuse_prepared_in_process_values_and_metadata()
    {
        const string moduleText =
            """
            module Test exposing (..)


            unitValue =
                ()


            stringValue =
                "hello"


            charValue =
                'z'


            integerValue =
                42


            floatValue =
                3.14
            """;

        var prepared = Prepare(moduleText);

        var unitLiteral = GetLiteral(prepared, "unitValue");
        var stringLiteral = GetLiteral(prepared, "stringValue");
        var charLiteral = GetLiteral(prepared, "charValue");
        var integerLiteral = GetLiteral(prepared, "integerValue");
        var floatLiteral = GetLiteral(prepared, "floatValue");

        unitLiteral.Value.Should().BeSameAs(PineValueInProcess.EmptyList);
        integerLiteral.Value.IntegerOrNull.Should().Be(new BigInteger(42));
        stringLiteral.Value.ListItemsOrNull().Should().NotBeNull();
        floatLiteral.Value.ListItemsOrNull().Should().NotBeNull();
        charLiteral.Value.IsBlob().Should().BeTrue();

        EvaluateInProcess(prepared, "unitValue").Should().BeSameAs(unitLiteral.Value);
        EvaluateInProcess(prepared, "stringValue").Should().BeSameAs(stringLiteral.Value);
        EvaluateInProcess(prepared, "charValue").Should().BeSameAs(charLiteral.Value);
        EvaluateInProcess(prepared, "integerValue").Should().BeSameAs(integerLiteral.Value);
        EvaluateInProcess(prepared, "floatValue").Should().BeSameAs(floatLiteral.Value);
    }

    [Fact]
    public void Preparation_reduces_closed_expressions_to_cached_literals()
    {
        const string moduleText =
            """
            module Test exposing (..)


            baseValue =
                40


            closedValue =
                let
                    answer =
                        Pine_builtin.int_add [ baseValue, 2 ]
                in
                case answer of
                    42 ->
                        ( answer, Pine_builtin.skip [ 2, 0x0000000100000000 ] )

                    _ ->
                        ( 0, 0 )
            """;

        var prepared = Prepare(moduleText);
        var literal = GetLiteral(prepared, "closedValue");

        literal.Value.ListItemsOrNull().Should().NotBeNull();
        EvaluateInProcess(prepared, "closedValue").Should().BeSameAs(literal.Value);
        Evaluate(prepared, "Test.closedValue").Should().Be("(42, '\0')");
    }

    [Fact]
    public void Preparation_reduces_closed_subexpressions_in_char_kernel_functions()
    {
        const string moduleText =
            """
            module Test exposing (..)


            toCode char =
                Pine_builtin.int_add
                    [ Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
                    , 0
                    ]


            fromCode code =
                Pine_builtin.reverse
                    (Pine_builtin.take
                        [ 4
                        , Pine_builtin.concat
                            [ Pine_builtin.reverse (Pine_builtin.skip [ 1, code ])
                            , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                            ]
                        ]
                    )
            """;

        var prepared = Prepare(moduleText);

        var toCodeBody =
            prepared.Declarations[Name("Test", "toCode")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject.Function.Declaration.Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.Application>()
            .Subject;

        var toCodeArguments =
            toCodeBody.Arguments.Should().ContainSingle()
            .Subject
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ListExpr>()
            .Subject;

        var concatArguments =
            toCodeArguments.Elements[0]
            .Should().BeOfType<ElmInterpreter.PreparedExpression.Application>()
            .Subject.Arguments.Should().ContainSingle()
            .Subject
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ListExpr>()
            .Subject;

        concatArguments.Elements[0]
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ValueLiteral>();

        var fromCodeBody =
            prepared.Declarations[Name("Test", "fromCode")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject.Function.Declaration.Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.Application>()
            .Subject;

        var takeArguments =
            fromCodeBody.Arguments.Should().ContainSingle()
            .Subject
            .Should().BeOfType<ElmInterpreter.PreparedExpression.Application>()
            .Subject.Arguments.Should().ContainSingle()
            .Subject
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ListExpr>()
            .Subject;

        var concatInFromCode =
            takeArguments.Elements[1]
            .Should().BeOfType<ElmInterpreter.PreparedExpression.Application>()
            .Subject.Arguments.Should().ContainSingle()
            .Subject
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ListExpr>()
            .Subject;

        concatInFromCode.Elements[1]
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ValueLiteral>();

        Evaluate(prepared, "Test.toCode 65").Should().Be("1089");
        Evaluate(prepared, "Test.fromCode 65").Should().Be("'A'");
    }

    [Fact]
    public void Preparation_keeps_closed_function_values_as_expressions()
    {
        const string moduleText =
            """
            module Test exposing (..)


            identity =
                \value -> value


            functions =
                [ identity ]
            """;

        var prepared = Prepare(moduleText);

        prepared.Declarations[Name("Test", "identity")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject.Function.Declaration.Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.LambdaExpression>();

        prepared.Declarations[Name("Test", "functions")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject.Function.Declaration.Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ListExpr>();
    }

    [Fact]
    public void Prepared_case_dispatch_emits_constant_values_for_binding_free_composite_patterns()
    {
        const string moduleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            type CaseInput
                = TupleCase ( Int, Int )
                | ListCase (List Int)
                | MaybeCase (Maybe (Maybe String))


            classify input =
                case input of
                    TupleCase ( 1, 2 ) ->
                        "tuple"

                    ListCase [ 3, 4 ] ->
                        "list"

                    MaybeCase (Just (Just "0")) ->
                        "nested"

                    ListCase (1 :: 2 :: []) ->
                        "uncons"

                    _ ->
                        "other"
            """;

        var prepared = Prepare(moduleText);
        var caseExpression = GetCaseExpression(prepared, "classify");

        caseExpression.Dispatch.Steps.Should().HaveCount(2);

        var compositeConstants =
            caseExpression.Dispatch.Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>()
            .Subject;

        compositeConstants.Cases.Should().HaveCount(4);

        compositeConstants.Cases.Select(constantCase => constantCase.Value).Should().Equal(
            Tag("TupleCase", PineValue.List([Int(1), Int(2)])),
            Tag("ListCase", PineValue.List([Int(3), Int(4)])),
            Tag("MaybeCase", Tag("Just", Tag("Just", ElmValueEncoding.StringAsPineValue("0")))),
            Tag("ListCase", PineValue.List([Int(1), Int(2)])));

        caseExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.DiscardCase>();

        Evaluate(prepared, "Test.classify (Test.TupleCase ( 1, 2 ))").Should().Be("\"tuple\"");
        Evaluate(prepared, "Test.classify (Test.ListCase [ 3, 4 ])").Should().Be("\"list\"");
        Evaluate(prepared, "Test.classify (Test.MaybeCase (Test.Just (Test.Just \"0\")))").Should().Be("\"nested\"");
        Evaluate(prepared, "Test.classify (Test.ListCase [ 1, 2 ])").Should().Be("\"uncons\"");
        Evaluate(prepared, "Test.classify (Test.MaybeCase Test.Nothing)").Should().Be("\"other\"");
    }

    [Fact]
    public void Prepared_case_dispatch_preserves_contiguous_constant_segments_around_var_bearing_patterns()
    {
        const string moduleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            type CaseInput
                = TupleCase ( Int, Int )
                | ListCase (List Int)
                | MaybeCase (Maybe (Maybe String))
                | Any String
                | Other


            classify input =
                case input of
                    TupleCase ( 1, 2 ) ->
                        "tuple"

                    ListCase [ 3, 4 ] ->
                        "list"

                    MaybeCase (Just (Just "0")) ->
                        "nested"

                    ListCase [ 1, 2 ] ->
                        "list12"

                    ListCase (1 :: 2 :: []) ->
                        "uncons"

                    Any text ->
                        text

                    Any "constant" ->
                        "lateConstant"

                    MaybeCase Nothing ->
                        "none"

                    TupleCase ( 9, 9 ) ->
                        "tuple99"

                    _ ->
                        "discard"
            """;

        var prepared = Prepare(moduleText);
        var caseExpression = GetCaseExpression(prepared, "classify");

        caseExpression.Dispatch.Steps.Should().HaveCount(4);

        caseExpression.Dispatch.Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>()
            .Subject
            .Cases
            .Should().HaveCount(5);

        caseExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.PatternCase>()
            .Subject
            .Pattern
            .Should().BeOfType<Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Pattern.NamedPattern>();

        caseExpression.Dispatch.Steps[2]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>()
            .Subject
            .Cases
            .Should().HaveCount(3);

        caseExpression.Dispatch.Steps[3]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.DiscardCase>();

        Evaluate(prepared, "Test.classify (Test.TupleCase ( 1, 2 ))").Should().Be("\"tuple\"");
        Evaluate(prepared, "Test.classify (Test.ListCase [ 3, 4 ])").Should().Be("\"list\"");
        Evaluate(prepared, "Test.classify (Test.MaybeCase (Test.Just (Test.Just \"0\")))").Should().Be("\"nested\"");
        Evaluate(prepared, "Test.classify (Test.ListCase [ 1, 2 ])").Should().Be("\"list12\"");
        Evaluate(prepared, "Test.classify (Test.Any \"bound\")").Should().Be("\"bound\"");
        Evaluate(prepared, "Test.classify (Test.Any \"constant\")").Should().Be("\"constant\"");
        Evaluate(prepared, "Test.classify (Test.MaybeCase Test.Nothing)").Should().Be("\"none\"");
        Evaluate(prepared, "Test.classify (Test.TupleCase ( 9, 9 ))").Should().Be("\"tuple99\"");
        Evaluate(prepared, "Test.classify Test.Other").Should().Be("\"discard\"");
    }

    [Fact]
    public void Prepared_json_roundtrip_preserves_case_dispatch_and_semantics()
    {
        const string moduleText =
            """
            module Test exposing (..)


            intValue =
                42


            classify s =
                case s of
                    "0" ->
                        "zero"

                    "1" ->
                        "one"

                    _ ->
                        "many"
            """;

        var prepared = Prepare(moduleText);

        var json = ElmSyntaxInterpreterPreparedJson.ToJsonString(prepared);
        var roundtripped = ElmSyntaxInterpreterPreparedJson.FromJsonString(json);

        GetLiteral(roundtripped, "intValue").Value.IntegerOrNull.Should().Be(new BigInteger(42));

        var caseExpression =
            roundtripped.Declarations[Name("Test", "classify")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
            .Subject;

        caseExpression.Dispatch.Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>();

        caseExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.DiscardCase>();

        Evaluate(roundtripped, "Test.classify \"0\"").Should().Be("\"zero\"");
        Evaluate(roundtripped, "Test.classify \"9\"").Should().Be("\"many\"");
        ElmSyntaxInterpreterPreparedJson.ToJsonString(roundtripped).Should().Be(json);
    }

    [Fact]
    public void Prepared_json_legacy_abstract_declarations_load_as_prepared_shape_and_preserve_semantics()
    {
        const string moduleText =
            """
            module Test exposing (..)


            literalValue =
                "legacy"


            classify s =
                case s of
                    "0" ->
                        "zero"

                    _ ->
                        "many"
            """;

        var prepared = ElmSyntaxInterpreterPreparedJson.FromJsonString(BuildLegacyPreparedJson(moduleText));

        prepared.Declarations[Name("Test", "literalValue")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.ValueLiteral>()
            .Subject
            .Value
            .ListItemsOrNull()
            .Should().NotBeNull();

        var caseExpression =
            prepared.Declarations[Name("Test", "classify")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
            .Subject;

        caseExpression.Dispatch.Steps.Should().HaveCount(2);

        caseExpression.Dispatch.Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>();

        caseExpression.Dispatch.Steps[1]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.DiscardCase>();

        Evaluate(prepared, "Test.literalValue").Should().Be("\"legacy\"");
        Evaluate(prepared, "Test.classify \"0\"").Should().Be("\"zero\"");
        Evaluate(prepared, "Test.classify \"9\"").Should().Be("\"many\"");
    }

    [Fact]
    public void Prepared_legacy_constructor_prepares_abstract_declarations()
    {
        const string moduleText =
            """
            module Test exposing (..)


            classify s =
                case s of
                    "0" ->
                        "zero"

                    _ ->
                        "many"
            """;

        var prepared = new ElmInterpreter.Prepared(BuildLegacyDeclarations(moduleText));

        prepared.Declarations[Name("Test", "classify")]
            .Should().BeOfType<ElmInterpreter.PreparedDeclaration.FunctionDeclaration>()
            .Subject
            .Function
            .Declaration
            .Expression
            .Should().BeOfType<ElmInterpreter.PreparedExpression.CaseExpression>()
            .Subject
            .Dispatch
            .Steps[0]
            .Should().BeOfType<ElmInterpreter.PreparedCaseDispatchSegment.ConstantValuePatterns>();

        Evaluate(prepared, "Test.classify \"0\"").Should().Be("\"zero\"");
    }

    [Fact]
    public void Constant_cases_precede_binding_fallback_in_source_order()
    {
        const string moduleText =
            """
            module Test exposing (..)


            classify n =
                case n of
                    0 ->
                        ( "zero", 0 )

                    value ->
                        ( "other", value )
            """;

        var prepared = Prepare(moduleText);

        Evaluate(prepared, "Test.classify 0").Should().Be("(\"zero\", 0)");
        Evaluate(prepared, "Test.classify 7").Should().Be("(\"other\", 7)");
    }

    [Fact]
    public void Constant_discard_and_binding_fallback_cases_preserve_semantics()
    {
        const string moduleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            classifyText s =
                case s of
                    "0" ->
                        "zero"

                    "1" ->
                        "one"

                    _ ->
                        "many"


            classifyMaybe m =
                case m of
                    Just "0" ->
                        "exact"

                    Just text ->
                        text

                    Nothing ->
                        "none"


            classifyPair pair =
                case pair of
                    ( 0, x ) ->
                        x

                    ( a, b ) ->
                        Pine_builtin.int_add [ a, b ]
            """;

        var prepared = Prepare(moduleText);

        Evaluate(prepared, "Test.classifyText \"0\"").Should().Be("\"zero\"");
        Evaluate(prepared, "Test.classifyText \"9\"").Should().Be("\"many\"");
        Evaluate(prepared, "Test.classifyMaybe (Test.Just \"0\")").Should().Be("\"exact\"");
        Evaluate(prepared, "Test.classifyMaybe (Test.Just \"ab\")").Should().Be("\"ab\"");
        Evaluate(prepared, "Test.classifyMaybe Test.Nothing").Should().Be("\"none\"");
        Evaluate(prepared, "Test.classifyPair ( 0, 5 )").Should().Be("5");
        Evaluate(prepared, "Test.classifyPair ( 3, 4 )").Should().Be("7");
    }

    [Fact]
    public void Nested_case_dispatch_preserves_outer_bindings_and_discard_fallback()
    {
        const string moduleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            classify m =
                case m of
                    Just text ->
                        case text of
                            "0" ->
                                "digit"

                            _ ->
                                text

                    Nothing ->
                        "none"
            """;

        var prepared = Prepare(moduleText);

        Evaluate(prepared, "Test.classify (Test.Just \"0\")").Should().Be("\"digit\"");
        Evaluate(prepared, "Test.classify (Test.Just \"abc\")").Should().Be("\"abc\"");
        Evaluate(prepared, "Test.classify Test.Nothing").Should().Be("\"none\"");
    }
}
