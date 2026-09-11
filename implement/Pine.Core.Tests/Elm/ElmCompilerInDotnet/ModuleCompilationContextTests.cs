using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Frozen;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ModuleCompilationContextTests
{
    [Fact]
    public void Merged_function_types_support_string_and_structured_lookup()
    {
        var qualifiedFunctionName = DeclQualifiedName.Create(["Test"], "alfa");
        var returnType = TypeInference.InferredType.Int();
        IReadOnlyList<TypeInference.InferredType> parameterTypes = [TypeInference.InferredType.String()];

        var context =
            new ModuleCompilationContext(
                allFunctions:
                new Dictionary<DeclQualifiedName, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>(),
                compiledFunctionsCache: [],
                pineKernelModuleNames: FrozenSet.Create<string>([]),
                functionTypes: new Dictionary<DeclQualifiedName, FunctionTypeInfo>
                {
                    [qualifiedFunctionName] = new(returnType, parameterTypes)
                });

        var typeInfoByRef = context.TryGetFunctionTypeInfo(qualifiedFunctionName);
        var typeInfoByString = context.TryGetFunctionTypeInfo("Test.alfa");

        typeInfoByRef.Should().NotBeNull();
        typeInfoByString.Should().NotBeNull();
        typeInfoByRef!.ReturnType.Should().Be(returnType);
        typeInfoByRef.ParameterTypes.Should().Equal(parameterTypes);
        typeInfoByString!.ReturnType.Should().Be(returnType);
        typeInfoByString.ParameterTypes.Should().Equal(parameterTypes);
    }

    [Fact]
    public void Declaration_name_keyed_metadata_is_available_via_existing_string_helpers()
    {
        var constructorName = DeclQualifiedName.Create(["Test"], "TagAlfa");
        var recordConstructorName = DeclQualifiedName.Create(["Test"], "RecordAlias");

        var context =
            new ModuleCompilationContext(
                allFunctions:
                new Dictionary<DeclQualifiedName, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>(),
                compiledFunctionsCache: [],
                pineKernelModuleNames: FrozenSet.Create<string>([]),
                choiceTagTypes: new Dictionary<DeclQualifiedName, FunctionTypeInfo>
                {
                    [constructorName] =
                    new(
                        new TypeInference.InferredType.UnknownType(),
                        [TypeInference.InferredType.Int(), TypeInference.InferredType.Bool()])
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
