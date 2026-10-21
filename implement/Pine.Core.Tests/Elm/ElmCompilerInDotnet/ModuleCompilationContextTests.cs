using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Frozen;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ModuleCompilationContextTests
{
    [Fact]
    public void Compiler_types_do_not_depend_on_legacy_syntax()
    {
        const BindingFlags flags =
            BindingFlags.Public | BindingFlags.NonPublic |
            BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly;

        var compilerNamespace = typeof(ElmCompiler).Namespace!;

        foreach (var type in typeof(ElmCompiler).Assembly.GetTypes()
            .Where(type => type.Namespace?.StartsWith(compilerNamespace, StringComparison.Ordinal) is true))
        {
            var referencedTypes =
                type.GetFields(flags).Select(field => field.FieldType)
                .Concat(type.GetProperties(flags).Select(property => property.PropertyType))
                .Concat(type.GetMethods(flags).Select(method => method.ReturnType))
                .Concat(
                    type.GetMethods(flags).SelectMany(
                        method => method.GetParameters().Select(parameter => parameter.ParameterType)))
                .Concat(
                    type.GetConstructors(flags).SelectMany(
                        constructor => constructor.GetParameters().Select(parameter => parameter.ParameterType)));

            foreach (var referencedType in referencedTypes)
            {
                referencedType.ToString().Should().NotContain(
                    "Stil4mElmSyntax7",
                    because: $"{type.FullName} must use concrete syntax or abstract syntax directly");
            }
        }
    }

    [Fact]
    public void Emission_contexts_do_not_contain_semantic_type_information()
    {
        foreach (var type in new[] { typeof(ModuleCompilationContext), typeof(ExpressionCompilationContext) })
        {
            foreach (var property in type.GetProperties())
            {
                property.PropertyType.ToString().Should().NotContain("InferredType");
                property.PropertyType.ToString().Should().NotContain("FunctionTypeInfo");
                property.PropertyType.ToString().Should().NotContain("TypeAliasDefinition");
            }
        }
    }

    [Fact]
    public void Constructor_metadata_has_one_qualified_identity()
    {
        var constructorName = DeclQualifiedName.Create(["Test"], "TagAlfa");
        var recordConstructorName = DeclQualifiedName.Create(["Test"], "RecordAlias");

        var context =
            new ModuleCompilationContext(
                PineKernelModuleNames: FrozenSet.Create<string>([]),
                ChoiceConstructorArities:
                ImmutableDictionary<DeclQualifiedName, int>.Empty.Add(constructorName, 2),
                RecordTypeAliasConstructors:
                ImmutableDictionary<DeclQualifiedName, Core.Elm.ElmCompilerInDotnet.CompilerModel.ValueList<string>>.Empty
                .Add(recordConstructorName, ["fieldA", "fieldB"]));

        context.TryGetChoiceTypeConstructorArgumentCount(constructorName).Should().Be(2);
        context.TryGetRecordConstructorFieldNames(recordConstructorName).Should().Equal("fieldA", "fieldB");
    }
}
