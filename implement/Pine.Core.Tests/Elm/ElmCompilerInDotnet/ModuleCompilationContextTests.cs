using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Frozen;
using System.Collections.Generic;
using Xunit;

using Stil4mElmSyntax7 = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ModuleCompilationContextTests
{
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
    public void Declaration_name_keyed_metadata_is_available_via_existing_string_helpers()
    {
        var constructorName = DeclQualifiedName.Create(["Test"], "TagAlfa");
        var recordConstructorName = DeclQualifiedName.Create(["Test"], "RecordAlias");

        var context =
            new ModuleCompilationContext(
                allFunctions:
                new Dictionary<DeclQualifiedName, (string moduleName, string functionName, Stil4mElmSyntax7.Declaration.FunctionDeclaration declaration)>(),
                compiledFunctionsCache: [],
                pineKernelModuleNames: FrozenSet.Create<string>([]),
                choiceConstructorArities: new Dictionary<DeclQualifiedName, int>
                {
                    [constructorName] = 2
                },
                recordTypeAliasConstructors: new Dictionary<DeclQualifiedName, IReadOnlyList<string>>
                {
                    [recordConstructorName] = ["fieldA", "fieldB"]
                });

        context.TryGetChoiceTypeConstructorArgumentCount(constructorName).Should().Be(2);
        context.TryGetChoiceTypeConstructorArgumentCount("Test.TagAlfa").Should().Be(2);
        context.TryGetRecordConstructorFieldNames(recordConstructorName).Should().Equal("fieldA", "fieldB");
        context.TryGetRecordConstructorFieldNames("Test.RecordAlias").Should().Equal("fieldA", "fieldB");
    }
}
