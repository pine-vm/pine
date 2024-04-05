using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class CompileElmCompilerTests
{
    static IReadOnlyList<string> CompilerPackageSources =>
        [
            /*
             * Maybe.Extra is used in the overall app but not in the Elm compiler.
             * 
            "https://github.com/elm-community/maybe-extra/tree/2a9e2c6143dcee04180b265c840b6052b0a053c2/",
            */

            /*
             * elm-syntax/tree/f7d9be0a1f346b22dfaa7b55679659874c72714b contains a module List.Extra
             * 
            "https://github.com/elm-community/list-extra/tree/5a083cf0400260537adef75f96fbd48bfcedc7c0/",
            */

            /*
            "https://github.com/Viir/result-extra/tree/e3b2e4358ac701d66e75ccbfdc4256513dc70694",
            */

            "https://github.com/Viir/elm-bigint/tree/d452b489c5795f8deed19658a7b8f7bf5ef1e9a4/",

            /*
             * Remove usages of Json.Decode and Json.Encode to speed up bootstrapping of the Elm compiler.
             * */
            "https://github.com/Viir/elm-syntax/tree/f7d9be0a1f346b22dfaa7b55679659874c72714b/"
        ];

    [TestMethod]
    public void Test_call_Basics_modBy()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var interactiveSession =
            new InteractiveSessionPine(
                compilerProgram,
                appCodeTree: null,
                caching: true,
                autoPGO: pgoShare);

        // Force integration of the 'Basics' module.
        var testSubmissionResult = interactiveSession.Submit(" Basics.modBy  13 17 ");

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("4", testSubmissionResponse.interactiveResponse.displayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var modByFunction =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Basics",
                declarationName: "modBy",
                pineVM: pineVM)
            .Extract(err => throw new Exception(err));

        var modByApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                modByFunction,
                arguments:
                [PineValueAsInteger.ValueFromSignedInteger(13),
                    PineValueAsInteger.ValueFromSignedInteger(17)]);

        Assert.AreEqual(
            PineValueAsInteger.ValueFromSignedInteger(4),
            modByApplicationResult.Extract(err => throw new Exception(err)));

        modByApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                modByFunction,
                arguments:
                [PineValueAsInteger.ValueFromSignedInteger(41),
                    PineValueAsInteger.ValueFromSignedInteger(47)]);

        Assert.AreEqual(
            PineValueAsInteger.ValueFromSignedInteger(6),
            modByApplicationResult.Extract(err => throw new Exception(err)));
    }

    [TestMethod]
    public void Test_parse_simple_Elm_module_and_encode_as_Pine_value()
    {
        var elmModuleText =
            """
            module Namespace.Beta exposing (..)

            import Dict


            type alias MaybeInt =
                Maybe Int


            type alias RecordType =
                { alfa : Int }


            type ChoiceType
                = Choice_Alfa
                | Choice_Beta Int


            greet : String -> String
            greet name =
                "Hello, " ++ name ++ " 👋"
            
            """;

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var compilerJavaScript =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var jsonEncodedElmModuleText =
            System.Text.Json.JsonSerializer.Serialize(elmModuleText);

        var responseJson =
            compilerJavaScript.CallFunction(
                "parseElmModuleTextToPineValue",
                jsonEncodedElmModuleText)
            .ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, ElmInteractive.PineValueJson>>(
                responseJson,
                new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        var parsedModulePineValue =
            ElmInteractive.ParsePineValueFromJson(
                responseStructure
                .Extract(err => throw new Exception(err)),
                dictionary: null);

        var responseAsElmValue =
            ElmValue.PineValueAsElmValue(parsedModulePineValue)
            .Extract(err => throw new Exception(err));

        var responseAsExpression =
            ElmValue.ElmValueAsExpression(responseAsElmValue);

        var importsNode =
            ((ElmValue.ElmRecord)responseAsElmValue).Fields.First(f => f.FieldName == "imports").Value;

        var importsNodeAsExpression =
            ElmValue.ElmValueAsExpression(importsNode);

        Assert.AreEqual(
            """[Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } ["Dict"] }]""",
            importsNodeAsExpression.expressionString);

        var declarationsList =
            (ElmValue.ElmList)((ElmValue.ElmRecord)responseAsElmValue)["declarations"]!;

        var declarations =
            declarationsList.Elements.Cast<ElmValue.ElmTag>()
            .OrderBy(declarationNode =>
            ((ElmValue.ElmInteger)
            ((ElmValue.ElmRecord)((ElmValue.ElmRecord)declarationNode.Arguments[0])["start"]!)["row"]!).Value)
            .ToImmutableArray();

        Assert.AreEqual(4, declarations.Length);

        var typeAliasDeclarationNode = declarations.ElementAt(0);
        var recordAliasDeclarationNode = declarations.ElementAt(1);
        var choiceTypeDeclarationNode = declarations.ElementAt(2);
        var functionDeclarationNode = declarations.ElementAt(3);

        var typeAliasDeclarationNodeAsExpression =
            ElmValue.ElmValueAsExpression(typeAliasDeclarationNode);

        Assert.AreEqual(
            """Node { end = { column = 14, row = 7 }, start = { column = 1, row = 6 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 6 }, start = { column = 12, row = 6 } } "MaybeInt", typeAnnotation = Node { end = { column = 14, row = 7 }, start = { column = 5, row = 7 } } (Typed (Node { end = { column = 10, row = 7 }, start = { column = 5, row = 7 } } ([],"Maybe")) [Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } (Typed (Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } ([],"Int")) [])]) })""",
            typeAliasDeclarationNodeAsExpression.expressionString);

        var recordAliasDeclarationNodeAsExpression =
            ElmValue.ElmValueAsExpression(recordAliasDeclarationNode);

        Assert.AreEqual(
            """Node { end = { column = 19, row = 11 }, start = { column = 1, row = 10 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 10 }, start = { column = 12, row = 10 } } "RecordType", typeAnnotation = Node { end = { column = 19, row = 11 }, start = { column = 5, row = 11 } } (Record [Node { end = { column = 17, row = 11 }, start = { column = 7, row = 11 } } [Node { end = { column = 11, row = 11 }, start = { column = 7, row = 11 } } "alfa",Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } (Typed (Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } ([],"Int")) [])]]) })""",
            recordAliasDeclarationNodeAsExpression.expressionString);

        var choiceTypeDeclarationNodeAsExpression =
            ElmValue.ElmValueAsExpression(choiceTypeDeclarationNode);

        Assert.AreEqual(
            """Node { end = { column = 22, row = 16 }, start = { column = 1, row = 14 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } { arguments = [], name = Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } "Choice_Alfa" },Node { end = { column = 22, row = 16 }, start = { column = 7, row = 16 } } { arguments = [Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } (Typed (Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } ([],"Int")) [])], name = Node { end = { column = 18, row = 16 }, start = { column = 7, row = 16 } } "Choice_Beta" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 14 }, start = { column = 6, row = 14 } } "ChoiceType" })""",
            choiceTypeDeclarationNodeAsExpression.expressionString);

        var declarationNodeAsExpression =
            ElmValue.ElmValueAsExpression(functionDeclarationNode);

        var declaration =
            (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

        Assert.AreEqual("FunctionDeclaration", declaration.TagName);

        var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

        var declarationSignatureNode =
            declarationRecord.Fields.First(f => f.FieldName == "signature").Value;

        var declarationSignatureNodeAsExpression =
            ElmValue.ElmValueAsExpression(declarationSignatureNode);

        Assert.AreEqual(
            """Just (Node { end = { column = 25, row = 19 }, start = { column = 1, row = 19 } } { name = Node { end = { column = 6, row = 19 }, start = { column = 1, row = 19 } } "greet", typeAnnotation = Node { end = { column = 25, row = 19 }, start = { column = 9, row = 19 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } (Typed (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } ([],"String")) [])) (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } ([],"String")) []))) })""",
            declarationSignatureNodeAsExpression.expressionString);

        var declarationDeclarationNode =
            declarationRecord.Fields.First(f => f.FieldName == "declaration").Value;

        var declarationDeclarationNodeAsExpression =
            ElmValue.ElmValueAsExpression(declarationDeclarationNode);

        Assert.AreEqual(
            """Node { end = { column = 30, row = 21 }, start = { column = 1, row = 20 } } { arguments = [Node { end = { column = 11, row = 20 }, start = { column = 7, row = 20 } } (VarPattern "name")], expression = Node { end = { column = 30, row = 21 }, start = { column = 5, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 14, row = 21 }, start = { column = 5, row = 21 } } (Literal "Hello, ")) (Node { end = { column = 30, row = 21 }, start = { column = 18, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 22, row = 21 }, start = { column = 18, row = 21 } } (FunctionOrValue [] "name")) (Node { end = { column = 30, row = 21 }, start = { column = 26, row = 21 } } (Literal " 👋"))))), name = Node { end = { column = 6, row = 20 }, start = { column = 1, row = 20 } } "greet" }""",
            declarationDeclarationNodeAsExpression.expressionString);
    }

    [TestMethod]
    public void Test_call_String_split()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var interactiveSession =
            new InteractiveSessionPine(
                compilerProgram,
                appCodeTree: null,
                caching: true,
                autoPGO: pgoShare);

        // Force integration of the 'String' module.
        var testSubmissionResult = interactiveSession.Submit(""" String.isEmpty  "test" """);

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("False", testSubmissionResponse.interactiveResponse.displayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var stringSplitFunction =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "String",
                declarationName: "split",
                pineVM: pineVM)
            .Extract(err => throw new Exception(err));

        var stringSplitApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                stringSplitFunction,
                arguments:
                [ElmValue.ElmValueAsPineValue(new ElmValue.ElmString(",")),
                    ElmValue.ElmValueAsPineValue(new ElmValue.ElmString("pizza,risotto,focaccia"))]);

        var stringSplitResultElmValue =
            ElmValue.PineValueAsElmValue(stringSplitApplicationResult.Extract(err => throw new Exception(err)))
            .Extract(err => throw new Exception(err));

        Assert.AreEqual(
            new ElmValue.ElmList(
                [new ElmValue.ElmString("pizza"),
                    new ElmValue.ElmString("risotto"),
                    new ElmValue.ElmString("focaccia")]),
            stringSplitResultElmValue);
    }

    [Ignore("Productive side not ready yet")]
    [TestMethod]
    public void Test_compile_elm_compiler()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var compileJavaScriptEngine =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compileElmProgramCodeFiles: compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var compilerPackageSourcesTrees =
            CompilerPackageSources
            .Select(LoadFromGitHubOrGitLab.LoadFromUrl)
            .ListCombine()
            .Extract(err => throw new Exception(err));

        var compilerPackageSourcesFiles =
            compilerPackageSourcesTrees
            .SelectMany(tree => tree.tree.EnumerateBlobsTransitive())
            .Where(blobAtPath =>
            blobAtPath.path.First() == "src" && blobAtPath.path.Last().ToLower().EndsWith(".elm"));

        var compilerAppCodeSourceFiles =
            compilerProgram.EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().ToLower().EndsWith(".elm"))
            .ToImmutableArray();

        var elmCoreLibraryModulesTexts =
            ElmInteractive.GetDefaultElmCoreModulesTexts(compileJavaScriptEngine);

        var elmModulesTexts = elmCoreLibraryModulesTexts;

        var compilerProgramOnlyElmJson =
            TreeNodeWithStringPath.FilterNodes(
                compilerProgram,
                nodePath => nodePath.SequenceEqual(["elm.json"]));

        var allAvailableElmFiles =
            compilerAppCodeSourceFiles
            .Concat(compilerPackageSourcesFiles)
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        var rootElmFile =
            allAvailableElmFiles
            .First(c => c.blobAtPath.path.SequenceEqual(["src", "ElmCompiler.elm"]));

        var elmModulesTextsForInteractiveCompiler =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [rootElmFile.moduleText],
                availableModulesTexts: [.. allAvailableElmFiles.Select(f => f.moduleText)]);

        var elmModulesForInteractiveCompiler =
            elmModulesTextsForInteractiveCompiler
            .Select(moduleText => allAvailableElmFiles.First(c => c.moduleText == moduleText).blobAtPath)
            .ToImmutableArray();

        var compilerWithPackagesTree =
            elmModulesForInteractiveCompiler
            .Aggregate(
                seed: compilerProgramOnlyElmJson,
                func: (aggregate, elmModule) =>
                aggregate.SetNodeAtPathSorted(elmModule.path, TreeNodeWithStringPath.Blob(elmModule.blobContent)));

        using var compilerInteractiveSession = new InteractiveSessionPine(
            compileElmProgramCodeFiles: compilerProgram,
            appCodeTree: compilerWithPackagesTree,
            caching: true,
            autoPGO: pgoShare);

        Assert.AreEqual(
            "4",
            compilerInteractiveSession.Submit("1 + 3")
            .Extract(err => throw new Exception(err))
            .ToString());
    }
}
