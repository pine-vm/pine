using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
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
        var qualifiedFunctionName = new QualifiedNameRef(["Test"], "alfa");
        var returnType = TypeInference.InferredType.Int();
        IReadOnlyList<TypeInference.InferredType> parameterTypes = [TypeInference.InferredType.String()];

        var context =
            new ModuleCompilationContext(
                AllFunctions: new Dictionary<QualifiedNameRef, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>(),
                CompiledFunctionsCache: [],
                PineKernelModuleNames: FrozenSet.Create<string>([]),
                FunctionTypes: new Dictionary<QualifiedNameRef, FunctionTypeInfo>
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
    public void Qualified_name_ref_keyed_metadata_is_available_via_existing_string_helpers()
    {
        var constructorName = new QualifiedNameRef(["Test"], "TagAlfa");
        var recordConstructorName = new QualifiedNameRef(["Test"], "RecordAlias");

        var context =
            new ModuleCompilationContext(
                AllFunctions: new Dictionary<QualifiedNameRef, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>(),
                CompiledFunctionsCache: [],
                PineKernelModuleNames: FrozenSet.Create<string>([]),
                ChoiceTagArgumentTypes: new Dictionary<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>
                {
                    [constructorName] = [TypeInference.InferredType.Int(), TypeInference.InferredType.Bool()]
                },
                RecordTypeAliasConstructors: new Dictionary<QualifiedNameRef, IReadOnlyList<string>>
                {
                    [recordConstructorName] = ["fieldA", "fieldB"]
                });

        context.TryGetChoiceTypeConstructorArgumentCount(constructorName).Should().Be(2);
        context.TryGetChoiceTypeConstructorArgumentCount("Test.TagAlfa").Should().Be(2);
        context.TryGetRecordConstructorFieldNames(recordConstructorName).Should().Equal("fieldA", "fieldB");
        context.TryGetRecordConstructorFieldNames("Test.RecordAlias").Should().Equal("fieldA", "fieldB");
    }
}
