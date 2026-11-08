using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Linq;

using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Boundary projection of type-only APIs onto declaration-local nominal slots.</summary>
internal static class InterfaceShapes
{
    public static InterfaceDefinition OpaqueType(string name, int arity) =>
        new(
            new Declaration.ChoiceTypeDeclaration(
                new(
                    name,
                    [.. Enumerable.Range(0, arity).Select(index => "a" + index)],
                    [])),
            []);

    public static InterfaceDefinition Value(string name, Type signature)
    {
        var nominals = new Dictionary<(string Module, string Name), int>();
        var shapes = new List<NominalShape>();

        TypeAnnotation Convert(Type type)
        {
            switch (type)
            {
                case Type.ChoiceType choice:
                    var identity = (string.Join(".", choice.ModuleName), choice.TypeName);

                    if (!nominals.TryGetValue(identity, out var slot))
                    {
                        slot = nominals.Count;
                        nominals.Add(identity, slot);
                        shapes.Add(new(choice.TypeName, choice.TypeArguments.Count));
                    }

                    return
                        new TypeAnnotation.Typed(
                            ["PineNominal", "T" + slot],
                            choice.TypeName,
                            [.. choice.TypeArguments.Select(Convert)]);

                case Type.FunctionType function:
                    return
                        new TypeAnnotation.FunctionTypeAnnotation(
                            Convert(function.ArgumentType),
                            Convert(function.ReturnType));

                case Type.ListType list:
                    return new TypeAnnotation.Typed(["List"], "List", [Convert(list.ElementType)]);

                case Type.TupleType tuple:
                    return new TypeAnnotation.Tupled([.. tuple.ElementTypes.Select(Convert)]);

                case Type.RecordType record:
                    return
                        new TypeAnnotation.Record(
                            new(
                                [
                                .. record.Fields.Select(
                                    field => RecordField.Create(field.FieldName, Convert(field.FieldType)))
                                ]));

                case Type.OpenRecordType record:
                    return
                        new TypeAnnotation.GenericRecord(
                            record.ExtensionVariable,
                            new(
                                [
                                .. record.KnownFields.Select(
                                    field => RecordField.Create(field.FieldName, Convert(field.FieldType)))
                                ]));

                case Type.TypeVariable variable:
                    return new TypeAnnotation.GenericType(variable.Name);

                case Type.IntType:
                    return new TypeAnnotation.Typed(["Basics"], "Int", []);

                case Type.FloatType:
                    return new TypeAnnotation.Typed(["Basics"], "Float", []);

                case Type.BoolType:
                    return new TypeAnnotation.Typed(["Basics"], "Bool", []);

                case Type.CharType:
                    return new TypeAnnotation.Typed(["Char"], "Char", []);

                case Type.StringType:
                    return new TypeAnnotation.Typed(["String"], "String", []);

                case Type.NumberType:
                    return new TypeAnnotation.GenericType("number");

                case Type.UnknownType:
                    return new TypeAnnotation.GenericType("unknown");

                default:
                    throw new NotImplementedException(
                        nameof(Value) + " does not handle inferred type: " + type.GetType().Name);
            }
        }

        var syntax = new Declaration.PortDeclaration(new(name, Convert(signature)));
        return new(syntax, new(shapes));
    }
}
