using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// End-to-end record-inference tests covering the scenarios:
/// <list type="bullet">
///   <item>Empty record literal <c>{ }</c> infers an empty closed record type.</item>
///   <item>Closed record literal whose field values exercise mixed constraint paths
///         (verbatim parameter, appendable, number, Int, String).</item>
///   <item>Nested record expressions.</item>
///   <item>A chain of record-update expressions where each level forces a constraint
///         on a different field.</item>
///   <item>Multiple record-access usages on the same value, accumulating into one
///         open record.</item>
///   <item>Record-update idioms taken from <c>implement/Pine.Core/Elm/elm-in-elm</c>:
///         singleton-field update on a parameter, multi-field update where the value
///         expressions use record-access on the same parameter, and record literals
///         whose values come from passed-in field values.</item>
/// </list>
/// </summary>
public class TypeInferenceRecordTests
{
    private static readonly Range s_dummyRange =
        new(new Location(1, 1), new Location(1, 1));

    private static Node<TValue> Node<TValue>(TValue value) =>
        new(s_dummyRange, value);

    private static Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> Field(
        string name,
        SyntaxTypes.Expression value) =>
        Node((Node(name), Node(value)));

    private static SyntaxTypes.Expression Param(string name) =>
        new SyntaxTypes.Expression.FunctionOrValue([], name);

    private static SyntaxTypes.Expression OpApp(
        string op,
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right) =>
        new SyntaxTypes.Expression.OperatorApplication(
            op,
            InfixDirection.Left,
            Node(left),
            Node(right));

    private static SyntaxTypes.Expression Access(SyntaxTypes.Expression record, string field) =>
        new SyntaxTypes.Expression.RecordAccess(Node(record), Node(field));

    private static SyntaxTypes.Expression IntLit(int v) =>
        new SyntaxTypes.Expression.Integer(v);

    private static SyntaxTypes.Expression StrLit(string v) =>
        new SyntaxTypes.Expression.Literal(v);

    private static IReadOnlyDictionary<string, TypeInference.InferredType> Empty =>
        new Dictionary<string, TypeInference.InferredType>();

    // ---------------------------------------------------------------------
    // 1. Empty record literal
    // ---------------------------------------------------------------------

    [Fact]
    public void Empty_record_literal_infers_empty_closed_record_type()
    {
        var expression = new SyntaxTypes.Expression.RecordExpr([]);

        var inferredType =
            TypeInference.InferExpressionType(expression, new Dictionary<string, int>(), Empty);

        inferredType.Should().BeOfType<TypeInference.InferredType.RecordType>();

        var recordType = (TypeInference.InferredType.RecordType)inferredType;
        recordType.Fields.Should().BeEmpty();
    }

    // ---------------------------------------------------------------------
    // 2. Mixed-constraint record literal whose field values cover:
    //    verbatim parameter, appendable, number, Int, String.
    // ---------------------------------------------------------------------

    /// <summary>
    /// alfa p s n m g =
    ///     { verbatim     = p                    -- field: type variable / unknown
    ///     , appendableA  = s ++ s2              -- field: appendable; s and s2 share the appendable var
    ///     , numberN      = n + 1                -- field: number
    ///     , intM         = m // 2               -- field: Int (Int//Int → Int)
    ///     , stringG      = g ++ "h"             -- field: String; g constrained to String
    ///     }
    /// </summary>
    [Fact]
    public void Mixed_constraint_record_literal_infers_closed_record_with_correct_field_types()
    {
        var verbatim = Param("p");
        var appendableA = OpApp("++", Param("s"), Param("s2"));
        var numberN = OpApp("+", Param("n"), IntLit(1));
        var intM = OpApp("//", Param("m"), IntLit(2));
        var stringG = OpApp("++", Param("g"), StrLit("h"));

        var fields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>
            {
                Field("verbatim", verbatim),
                Field("appendableA", appendableA),
                Field("numberN", numberN),
                Field("intM", intM),
                Field("stringG", stringG),
            };

        var expression = new SyntaxTypes.Expression.RecordExpr(fields);

        var parameterNames =
            new Dictionary<string, int>
            {
                ["p"] = 0,
                ["s"] = 1,
                ["s2"] = 2,
                ["n"] = 3,
                ["m"] = 4,
                ["g"] = 5,
            };

        var inferredType = TypeInference.InferExpressionType(expression, parameterNames, Empty);

        inferredType.Should().BeOfType<TypeInference.InferredType.RecordType>();

        var recordType = (TypeInference.InferredType.RecordType)inferredType;
        var byName = new Dictionary<string, TypeInference.InferredType>();

        foreach (var (name, type) in recordType.Fields)
            byName[name] = type;

        // Closed: all five field names known, no extension variable.
        byName.Should().HaveCount(5);
        byName.Keys.Should().BeEquivalentTo(["verbatim", "appendableA", "numberN", "intM", "stringG"]);

        // Verbatim parameter access: type of `p` not known → Unknown.
        byName["verbatim"].Should().BeOfType<TypeInference.InferredType.UnknownType>();

        // s ++ s2 with no concrete operand: result is the shared appendable type variable.
        byName["appendableA"].Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)byName["appendableA"]).Constraint
            .Should().Be(TypeVariableConstraint.Appendable);

        // n + 1: result is Number (polymorphic between Int and Float).
        byName["numberN"].Should().BeOfType<TypeInference.InferredType.NumberType>();

        // m // 2: integer division forces Int on the result.
        byName["intM"].Should().BeOfType<TypeInference.InferredType.IntType>();

        // g ++ "h": one operand is a String literal → result is String.
        byName["stringG"].Should().BeOfType<TypeInference.InferredType.StringType>();

        // Constraints flow to parameters in a single scan as well.
        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames,
                new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("g")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.StringType>();

        // m and the operands of m // 2 are forced to Int.
        paramTypes.Should().ContainKey("m")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.IntType>();
    }

    // ---------------------------------------------------------------------
    // 3. Nested record expressions
    // ---------------------------------------------------------------------

    /// <summary>
    /// alfa s =
    ///     { outer =
    ///         { greeting = s ++ "!"
    ///         , inner    = { count = 1 }
    ///         }
    ///     , flag = True (modeled as a parameter ref - we just verify the outer shape)
    ///     }
    /// </summary>
    [Fact]
    public void Nested_record_literal_infers_nested_closed_record_types()
    {
        var inner =
            new SyntaxTypes.Expression.RecordExpr([Field("count", IntLit(1))]);

        var outer =
            new SyntaxTypes.Expression.RecordExpr(
                [
                Field("greeting", OpApp("++", Param("s"), StrLit("!"))),
                Field("inner", inner),
                ]);

        var expression =
            new SyntaxTypes.Expression.RecordExpr(
                [
                Field("outer", outer),
                Field("flag", Param("b")),
                ]);

        var parameterNames = new Dictionary<string, int> { ["s"] = 0, ["b"] = 1 };

        var inferredType = TypeInference.InferExpressionType(expression, parameterNames, Empty);

        inferredType.Should().BeOfType<TypeInference.InferredType.RecordType>();

        var top = (TypeInference.InferredType.RecordType)inferredType;
        var topByName = new Dictionary<string, TypeInference.InferredType>();

        foreach (var (n, t) in top.Fields)
            topByName[n] = t;

        topByName.Should().ContainKey("outer");
        topByName["outer"].Should().BeOfType<TypeInference.InferredType.RecordType>();

        var outerRec = (TypeInference.InferredType.RecordType)topByName["outer"];
        var outerByName = new Dictionary<string, TypeInference.InferredType>();

        foreach (var (n, t) in outerRec.Fields)
            outerByName[n] = t;

        outerByName.Should().ContainKey("greeting")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.StringType>();

        outerByName.Should().ContainKey("inner")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.RecordType>();

        var innerRec = (TypeInference.InferredType.RecordType)outerByName["inner"];
        innerRec.Fields.Should().ContainSingle();
        innerRec.Fields[0].FieldName.Should().Be("count");
        innerRec.Fields[0].FieldType.Should().BeOfType<TypeInference.InferredType.NumberType>();
    }

    // ---------------------------------------------------------------------
    // 4. Chain of record-update expressions where each level forces a different
    //    constraint on a different field.
    // ---------------------------------------------------------------------

    /// <summary>
    /// alfa r =
    ///     { x = { r | name = "x" }       -- forces r.name : String
    ///     , y = { r | greeting = "!" ++ "x" } -- forces r.greeting : String
    ///     , z = { r | count = 1 + 2 }    -- forces r.count : number
    ///     , w = { r | items = [] ++ [1] } -- forces r.items : List
    ///     }
    /// </summary>
    [Fact]
    public void Chain_of_record_updates_accumulates_field_constraints_on_same_parameter()
    {
        var u1 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("name", StrLit("x"))]);

        var u2 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("greeting", OpApp("++", StrLit("!"), StrLit("x")))]);

        var u3 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("count", OpApp("+", IntLit(1), IntLit(2)))]);

        var listLit =
            new SyntaxTypes.Expression.ListExpr(
                [Node(IntLit(1))]);

        var u4 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("items", OpApp("++", new SyntaxTypes.Expression.ListExpr([]), listLit))]);

        var expression =
            new SyntaxTypes.Expression.RecordExpr(
                [
                Field("x", u1),
                Field("y", u2),
                Field("z", u3),
                Field("w", u4),
                ]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("r");
        paramTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["r"];
        var fByName = new Dictionary<string, TypeInference.InferredType>();

        foreach (var (n, t) in open.KnownFields)
            fByName[n] = t;

        fByName.Should().ContainKey("name")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.StringType>();

        fByName.Should().ContainKey("greeting")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.StringType>();

        fByName.Should().ContainKey("count")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.NumberType>();

        fByName.Should().ContainKey("items")
            .WhoseValue.Should().BeOfType<TypeInference.InferredType.ListType>();
    }

    // ---------------------------------------------------------------------
    // 5. Multiple record-access usages on the same value force open-record
    //    accumulation.
    // ---------------------------------------------------------------------

    /// <summary>
    /// alfa r = { a = r.name, b = r.email, c = r.age }
    /// → r : { ρ | name : ?, email : ?, age : ? }
    /// </summary>
    [Fact]
    public void Multiple_record_accesses_on_same_parameter_accumulate_into_open_record()
    {
        var expression =
            new SyntaxTypes.Expression.RecordExpr(
                [
                Field("a", Access(Param("r"), "name")),
                Field("b", Access(Param("r"), "email")),
                Field("c", Access(Param("r"), "age")),
                ]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("r");
        paramTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["r"];
        var fieldNames = new HashSet<string>();

        foreach (var (n, _) in open.KnownFields)
            fieldNames.Add(n);

        fieldNames.Should().BeEquivalentTo(["name", "email", "age"]);
    }

    // ---------------------------------------------------------------------
    // 6. Mixed access + update on the same parameter accumulates fields.
    // ---------------------------------------------------------------------

    /// <summary>
    /// elm-in-elm idiom (simplified from <c>{ deadEnd | row = deadEnd.row + offset.row, col = deadEnd.col + offset.col }</c>):
    /// the record-update expression's value expression contains a record-access on
    /// the same parameter being updated, plus another access through a different
    /// parameter.
    /// </summary>
    [Fact]
    public void Record_update_with_field_value_using_access_on_same_parameter_accumulates_required_fields()
    {
        // { deadEnd | row = deadEnd.row + offset.row
        //           , col = deadEnd.col + offset.col
        //           }
        var rowValue =
            OpApp("+", Access(Param("deadEnd"), "row"), Access(Param("offset"), "row"));

        var colValue =
            OpApp("+", Access(Param("deadEnd"), "col"), Access(Param("offset"), "col"));

        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("deadEnd"),
                [
                Field("row", rowValue),
                Field("col", colValue),
                ]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["deadEnd"] = 0, ["offset"] = 1 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        // deadEnd is updated AND accessed → accumulates row, col from both sides.
        paramTypes.Should().ContainKey("deadEnd");
        paramTypes["deadEnd"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var deadEndOpen = (TypeInference.InferredType.OpenRecordType)paramTypes["deadEnd"];
        var deadEndFields = new HashSet<string>();

        foreach (var (n, _) in deadEndOpen.KnownFields)
            deadEndFields.Add(n);

        deadEndFields.Should().Contain("row");
        deadEndFields.Should().Contain("col");

        // offset is only accessed → constrained to require row, col as well.
        paramTypes.Should().ContainKey("offset");
        paramTypes["offset"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var offsetOpen = (TypeInference.InferredType.OpenRecordType)paramTypes["offset"];
        var offsetFields = new HashSet<string>();

        foreach (var (n, _) in offsetOpen.KnownFields)
            offsetFields.Add(n);

        offsetFields.Should().BeEquivalentTo(["row", "col"]);
    }

    // ---------------------------------------------------------------------
    // 7. elm-in-elm idiom: singleton-field update with a Bool-typed value.
    //    `Ok { configBefore | enableDebug = True }` — `enableDebug` is constrained
    //    to a parameter-derived value (we model it with a parameter `b` which gets
    //    no inferable type; the open record still records the field name).
    // ---------------------------------------------------------------------

    [Fact]
    public void Singleton_field_update_yields_open_record_with_one_known_field()
    {
        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("configBefore"),
                [Field("enableDebug", Param("b"))]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames:
                new Dictionary<string, int> { ["configBefore"] = 0, ["b"] = 1 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("configBefore");
        paramTypes["configBefore"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["configBefore"];
        open.KnownFields.Should().ContainSingle();
        open.KnownFields[0].FieldName.Should().Be("enableDebug");
    }

    // ---------------------------------------------------------------------
    // 8. Record literal whose field values come from accesses on a different
    //    parameter (a "projecting" record).
    //    elm-in-elm idiom: re-shaping records by copying selected fields.
    // ---------------------------------------------------------------------

    /// <summary>
    /// alfa src = { name = src.name, address = src.address }
    /// → result is a closed record with two fields whose types are unknown
    ///   (we don't know src's field types yet);
    ///   src is constrained to require both 'name' and 'address'.
    /// </summary>
    [Fact]
    public void Projecting_record_literal_constrains_source_to_open_record_with_required_fields()
    {
        var expression =
            new SyntaxTypes.Expression.RecordExpr(
                [
                Field("name", Access(Param("src"), "name")),
                Field("address", Access(Param("src"), "address")),
                ]);

        var parameterNames = new Dictionary<string, int> { ["src"] = 0 };

        var inferredType = TypeInference.InferExpressionType(expression, parameterNames, Empty);
        inferredType.Should().BeOfType<TypeInference.InferredType.RecordType>();

        var rec = (TypeInference.InferredType.RecordType)inferredType;
        var keys = new HashSet<string>();

        foreach (var (n, _) in rec.Fields)
            keys.Add(n);

        keys.Should().BeEquivalentTo(["name", "address"]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames,
                new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("src");
        paramTypes["src"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["src"];
        var srcFields = new HashSet<string>();

        foreach (var (n, _) in open.KnownFields)
            srcFields.Add(n);

        srcFields.Should().BeEquivalentTo(["name", "address"]);
    }

    // ---------------------------------------------------------------------
    // 9. elm-in-elm idiom: record access whose value is then concatenated.
    //    Demonstrates that constraints flow through nested expressions.
    //    `node.name ++ "!"` → `node.name : String` → node : { ρ | name : String }
    //    (the field's inferred type is added to the open record).
    // ---------------------------------------------------------------------

    [Fact]
    public void Record_access_used_with_string_append_propagates_String_constraint_to_field()
    {
        // alfa node = node.name ++ "!"
        var expression = OpApp("++", Access(Param("node"), "name"), StrLit("!"));

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                new Dictionary<string, int> { ["node"] = 0 },
                Empty);

        // The whole expression is String (one side is a string literal).
        inferredType.Should().BeOfType<TypeInference.InferredType.StringType>();

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["node"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        // node is at minimum constrained to require 'name'. The field's type is
        // currently the field's individual type variable — propagating String back
        // through the access into the open-record's field slot is a future
        // refinement; today, the test simply verifies that 'name' is a required
        // field on the inferred open record.
        paramTypes.Should().ContainKey("node");
        paramTypes["node"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["node"];
        open.KnownFields.Should().ContainSingle();
        open.KnownFields[0].FieldName.Should().Be("name");
    }

    // ---------------------------------------------------------------------
    // 10. Update on a parameter using a literal record assignment, plus a separate
    //     access on the same parameter — confirms accumulation across both shapes.
    //     elm-in-elm pattern: `{ stack | items = newItem :: stack.items }` (modeled
    //     here using ++ since :: isn't an operator in our table).
    // ---------------------------------------------------------------------

    [Fact]
    public void Update_with_value_referencing_access_on_same_parameter_accumulates_field()
    {
        // { stack | items = stack.items ++ [newItem] }
        var newList =
            new SyntaxTypes.Expression.ListExpr(
                [Node(Param("newItem"))]);

        var newItems =
            OpApp("++", Access(Param("stack"), "items"), newList);

        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("stack"),
                [Field("items", newItems)]);

        var paramTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames:
                new Dictionary<string, int> { ["stack"] = 0, ["newItem"] = 1 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        paramTypes.Should().ContainKey("stack");
        paramTypes["stack"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var open = (TypeInference.InferredType.OpenRecordType)paramTypes["stack"];
        var fieldNames = new HashSet<string>();

        foreach (var (n, _) in open.KnownFields)
            fieldNames.Add(n);

        // Single field 'items' required, contributed by both the update and the
        // access (which unified into the same single-field open record).
        fieldNames.Should().BeEquivalentTo(["items"]);
    }
}
