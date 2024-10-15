using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.Elm;
using Pine.ElmInteractive;
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
        ElmCompiler.CompilerPackageSources;

    [TestMethod]
    public void Test_call_Basics_modBy()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = ElmCompiler.CompilerSourceFilesDefault.Value;

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

        Assert.AreEqual("4", testSubmissionResponse.InteractiveResponse.DisplayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var (_, modByFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Basics",
                declarationName: "modBy",
                pineVM.parseCache)
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
            greet param_name =
                "Hello, " ++ param_name ++ " 👋"
            

            type String
                = String (List Char.Char)
            
                -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                | AnyOtherKind
            
            
            replicate : appendable -> appendable
            replicate a =
                case a of
                String stringA ->
                    String (Pine_kernel.concat [ stringA, stringA ])
            
                _ ->
                    Pine_kernel.concat [ a, a ]

            """;

        var compilerProgram = ElmCompiler.CompilerSourceContainerFilesDefault.Value;

        using var compilerJavaScript =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var parsedModulePineValue =
            ElmInteractive.ParseElmModuleTextToPineValue(elmModuleText, compilerJavaScript)
            .Extract(err => throw new Exception(err));

        var responseAsElmValue =
            ElmValueEncoding.PineValueAsElmValue(parsedModulePineValue)
            .Extract(err => throw new Exception(err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseAsElmValue).expressionString;

        var moduleDefinitionNode =
            ((ElmValue.ElmRecord)responseAsElmValue).Fields.First(f => f.FieldName is "moduleDefinition").Value;

        var moduleDefinitionNodeAsExpression =
            ElmValue.RenderAsElmExpression(moduleDefinitionNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 36, row = 1 }, start = { column = 23, row = 1 } } (All { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } }), moduleName = Node { end = { column = 22, row = 1 }, start = { column = 8, row = 1 } } ["Namespace","Beta"] })""",
            moduleDefinitionNodeAsExpression);

        var importsNode =
            ((ElmValue.ElmRecord)responseAsElmValue).Fields.First(f => f.FieldName is "imports").Value;

        var importsNodeAsExpression =
            ElmValue.RenderAsElmExpression(importsNode).expressionString;

        Assert.AreEqual(
            """[Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } ["Dict"] }]""",
            importsNodeAsExpression);

        var declarationsList =
            (ElmValue.ElmList)((ElmValue.ElmRecord)responseAsElmValue)["declarations"]!;

        var declarations =
            declarationsList.Elements.Cast<ElmValue.ElmTag>()
            .OrderBy(declarationNode =>
            ((ElmValue.ElmInteger)
            ((ElmValue.ElmRecord)((ElmValue.ElmRecord)declarationNode.Arguments[0])["start"]!)["row"]!).Value)
            .ToImmutableArray();

        Assert.AreEqual(6, declarations.Length);

        var typeAliasDeclarationNode = declarations.ElementAt(0);
        var recordAliasDeclarationNode = declarations.ElementAt(1);
        var choiceTypeDeclarationNode = declarations.ElementAt(2);

        var typeAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(typeAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 14, row = 7 }, start = { column = 1, row = 6 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 6 }, start = { column = 12, row = 6 } } "MaybeInt", typeAnnotation = Node { end = { column = 14, row = 7 }, start = { column = 5, row = 7 } } (Typed (Node { end = { column = 10, row = 7 }, start = { column = 5, row = 7 } } ([],"Maybe")) [Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } (Typed (Node { end = { column = 14, row = 7 }, start = { column = 11, row = 7 } } ([],"Int")) [])]) })""",
            typeAliasDeclarationNodeAsExpression);

        var recordAliasDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(recordAliasDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 19, row = 11 }, start = { column = 1, row = 10 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 22, row = 10 }, start = { column = 12, row = 10 } } "RecordType", typeAnnotation = Node { end = { column = 19, row = 11 }, start = { column = 5, row = 11 } } (Record [Node { end = { column = 17, row = 11 }, start = { column = 7, row = 11 } } [Node { end = { column = 11, row = 11 }, start = { column = 7, row = 11 } } "alfa",Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } (Typed (Node { end = { column = 17, row = 11 }, start = { column = 14, row = 11 } } ([],"Int")) [])]]) })""",
            recordAliasDeclarationNodeAsExpression);

        var choiceTypeDeclarationNodeAsExpression =
            ElmValue.RenderAsElmExpression(choiceTypeDeclarationNode).expressionString;

        Assert.AreEqual(
            """Node { end = { column = 22, row = 16 }, start = { column = 1, row = 14 } } (CustomTypeDeclaration { constructors = [Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } { arguments = [], name = Node { end = { column = 18, row = 15 }, start = { column = 7, row = 15 } } "Choice_Alfa" },Node { end = { column = 22, row = 16 }, start = { column = 7, row = 16 } } { arguments = [Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } (Typed (Node { end = { column = 22, row = 16 }, start = { column = 19, row = 16 } } ([],"Int")) [])], name = Node { end = { column = 18, row = 16 }, start = { column = 7, row = 16 } } "Choice_Beta" }], documentation = Nothing, generics = [], name = Node { end = { column = 16, row = 14 }, start = { column = 6, row = 14 } } "ChoiceType" })""",
            choiceTypeDeclarationNodeAsExpression);

        {
            var functionDeclarationNode = declarations.ElementAt(3);

            var declarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(functionDeclarationNode).expressionString;

            var declaration =
                (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

            Assert.AreEqual("FunctionDeclaration", declaration.TagName);

            var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

            var declarationSignatureNode =
                declarationRecord.Fields.First(f => f.FieldName is "signature").Value;

            var declarationSignatureNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationSignatureNode).expressionString;

            Assert.AreEqual(
                """Just (Node { end = { column = 25, row = 19 }, start = { column = 1, row = 19 } } { name = Node { end = { column = 6, row = 19 }, start = { column = 1, row = 19 } } "greet", typeAnnotation = Node { end = { column = 25, row = 19 }, start = { column = 9, row = 19 } } (FunctionTypeAnnotation (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } (Typed (Node { end = { column = 15, row = 19 }, start = { column = 9, row = 19 } } ([],"String")) [])) (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } (Typed (Node { end = { column = 25, row = 19 }, start = { column = 19, row = 19 } } ([],"String")) []))) })""",
                declarationSignatureNodeAsExpression);

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 36, row = 21 }, start = { column = 1, row = 20 } } { arguments = [Node { end = { column = 17, row = 20 }, start = { column = 7, row = 20 } } (VarPattern "param_name")], expression = Node { end = { column = 36, row = 21 }, start = { column = 5, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 14, row = 21 }, start = { column = 5, row = 21 } } (Literal "Hello, ")) (Node { end = { column = 36, row = 21 }, start = { column = 18, row = 21 } } (OperatorApplication "++" Right (Node { end = { column = 28, row = 21 }, start = { column = 18, row = 21 } } (FunctionOrValue [] "param_name")) (Node { end = { column = 36, row = 21 }, start = { column = 32, row = 21 } } (Literal " 👋"))))), name = Node { end = { column = 6, row = 20 }, start = { column = 1, row = 20 } } "greet" }""",
                declarationDeclarationNodeAsExpression);
        }

        {
            var functionDeclarationNode = declarations.ElementAt(5);

            var declarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(functionDeclarationNode).expressionString;

            var declaration =
                (ElmValue.ElmTag)functionDeclarationNode.Arguments.ElementAt(1);

            Assert.AreEqual("FunctionDeclaration", declaration.TagName);

            var declarationRecord = (ElmValue.ElmRecord)declaration.Arguments.Single();

            var declarationDeclarationNode =
                declarationRecord.Fields.First(f => f.FieldName is "declaration").Value;

            var declarationDeclarationNodeAsExpression =
                ElmValue.RenderAsElmExpression(declarationDeclarationNode).expressionString;

            Assert.AreEqual(
                """Node { end = { column = 36, row = 38 }, start = { column = 1, row = 32 } } { arguments = [Node { end = { column = 12, row = 32 }, start = { column = 11, row = 32 } } (VarPattern "a")], expression = Node { end = { column = 36, row = 38 }, start = { column = 5, row = 33 } } (CaseExpression { cases = [[Node { end = { column = 19, row = 34 }, start = { column = 5, row = 34 } } (NamedPattern { moduleName = [], name = "String" } [Node { end = { column = 19, row = 34 }, start = { column = 12, row = 34 } } (VarPattern "stringA")]),Node { end = { column = 57, row = 35 }, start = { column = 9, row = 35 } } (Application [Node { end = { column = 15, row = 35 }, start = { column = 9, row = 35 } } (FunctionOrValue [] "String"),Node { end = { column = 57, row = 35 }, start = { column = 16, row = 35 } } (ParenthesizedExpression (Node { end = { column = 56, row = 35 }, start = { column = 17, row = 35 } } (Application [Node { end = { column = 35, row = 35 }, start = { column = 17, row = 35 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 56, row = 35 }, start = { column = 36, row = 35 } } (ListExpr [Node { end = { column = 45, row = 35 }, start = { column = 38, row = 35 } } (FunctionOrValue [] "stringA"),Node { end = { column = 54, row = 35 }, start = { column = 47, row = 35 } } (FunctionOrValue [] "stringA")])])))])],[Node { end = { column = 6, row = 37 }, start = { column = 5, row = 37 } } AllPattern,Node { end = { column = 36, row = 38 }, start = { column = 9, row = 38 } } (Application [Node { end = { column = 27, row = 38 }, start = { column = 9, row = 38 } } (FunctionOrValue ["Pine_kernel"] "concat"),Node { end = { column = 36, row = 38 }, start = { column = 28, row = 38 } } (ListExpr [Node { end = { column = 31, row = 38 }, start = { column = 30, row = 38 } } (FunctionOrValue [] "a"),Node { end = { column = 34, row = 38 }, start = { column = 33, row = 38 } } (FunctionOrValue [] "a")])])]], expression = Node { end = { column = 11, row = 33 }, start = { column = 10, row = 33 } } (FunctionOrValue [] "a") }), name = Node { end = { column = 10, row = 32 }, start = { column = 1, row = 32 } } "replicate" }""",
                declarationDeclarationNodeAsExpression);
        }
    }

    [TestMethod]
    public void Test_call_String_split()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = ElmCompiler.CompilerSourceFilesDefault.Value;

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

        Assert.AreEqual("False", testSubmissionResponse.InteractiveResponse.DisplayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var (_, stringSplitFunction) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "String",
                declarationName: "split",
                pineVM.parseCache)
            .Extract(err => throw new Exception(err));

        var stringSplitApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                stringSplitFunction,
                arguments:
                [ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmString(",")),
                    ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmString("pizza,risotto,focaccia"))]);

        var stringSplitResultElmValue =
            ElmValueEncoding.PineValueAsElmValue(stringSplitApplicationResult.Extract(err => throw new Exception(err)))
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
    public void Elm_compiler_compiles_Elm_compiler()
    {
        /*
         * 2024-06-08: Switching from auto-PGO to manual PGO to improve readability of the test output.
         * 
        using var pgoShare = new DynamicPGOShare(
            compiledExpressionsCountLimit: 160,
            limitSampleCountPerSubmissionDefault: 100);
        */

        var compilerProgram = ElmCompiler.CompilerSourceContainerFilesDefault.Value;

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

        var elmModulesTextsForElmCompiler =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [rootElmFile.moduleText],
                availableModulesTexts: [.. allAvailableElmFiles.Select(f => f.moduleText)]);

        var elmModulesForElmCompiler =
            elmModulesTextsForElmCompiler
            .Select(moduleText => allAvailableElmFiles.First(c => c.moduleText == moduleText).blobAtPath)
            .ToImmutableArray();

        var compilerWithPackagesTree =
            elmModulesForElmCompiler
            .Aggregate(
                seed: compilerProgramOnlyElmJson,
                func: (aggregate, elmModule) =>
                aggregate.SetNodeAtPathSorted(elmModule.path, TreeNodeWithStringPath.Blob(elmModule.blobContent)));

        using var compilerInteractiveSession = new InteractiveSessionPine(
            compilerSourceFiles: compilerProgram,
            appCodeTree: compilerWithPackagesTree,
            caching: true,
            autoPGO: null);

        var interactiveInitialState = compilerInteractiveSession.CurrentEnvironmentValue();

        var interactiveParsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(interactiveInitialState)
            .Extract(err => throw new Exception(err));

        var moduleBasics =
            interactiveParsedEnv
            .Modules.Single(m => m.moduleName is "Basics");

        Assert.AreEqual(
            "4",
            compilerInteractiveSession.Submit("1 + 3")
            .Extract(err => throw new Exception(err))
            .InteractiveResponse.DisplayText);

        var pineVM = new PineVM();

        {
            // Test one of the declarations used for interning.

            var (_, declaration) =
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: interactiveInitialState,
                    moduleName: "Pine",
                    declarationName: "stringAsValue_Function",
                    pineVM.parseCache)
                .Extract(err => throw new Exception(err));

            var declarationValueResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    declaration,
                    arguments:
                    [])
                .Extract(err => throw new Exception(err));

            var declarationValueAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(declarationValueResult)
                .Extract(err => throw new Exception(err));

            var (declarationValueAsElmValueAsExpr, _) =
                ElmValue.RenderAsElmExpression(declarationValueAsElmValue);

            var expectedDeclValue =
                ElmValueInterop.PineValueEncodedAsInElmCompiler(
                    PineValueAsString.ValueFromString("Function"));

            var (expectedDeclValueAsExpr, _) =
                ElmValue.RenderAsElmExpression(expectedDeclValue);

            Assert.AreEqual(
                expectedDeclValueAsExpr,
                declarationValueAsElmValueAsExpr);

            Assert.AreEqual(
                expectedDeclValue,
                declarationValueAsElmValue);
        }

        Result<string, KeyValuePair<IReadOnlyList<string>, (string moduleText, PineValue parsed)>> TryParseModuleText(string moduleText)
        {
            return
                ElmTime.ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .MapError(err => "Failed parsing name for module " + moduleText.Split('\n', '\r').FirstOrDefault())
                .AndThen(moduleName =>
                ElmInteractive.ParseElmModuleTextToPineValue(moduleText, compileJavaScriptEngine)
                .MapError(err => "Failed parsing module " + moduleName + ": " + err)
                .Map(parsedModule => new KeyValuePair<IReadOnlyList<string>, (string moduleText, PineValue parsed)>(
                    moduleName, (moduleText, parsedModule))));
        }

        var elmModulesTextsForElmCompilerIncludingCore =
            elmCoreLibraryModulesTexts
            .Concat(elmModulesTextsForElmCompiler)
            .ToImmutableArray();

        var compilerModulesParseResults =
            elmModulesTextsForElmCompilerIncludingCore
            .Select(TryParseModuleText)
            .ToImmutableArray();

        var parsedCompilerModules =
            compilerModulesParseResults
            .Select(result => result.Extract(err => throw new Exception(err)))
            .ToImmutableArray();

        var (_, declExpandInteractiveEnv) =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveInitialState,
                moduleName: "ElmCompiler",
                declarationName: "expandElmInteractiveEnvironmentWithModules",
                pineVM.parseCache)
            .Extract(err => throw new Exception(err));

        /*

        type alias ProjectParsedElmFile =
            { fileText : String
            , parsedModule : Elm.Syntax.File.File
            }

         * */


        static PineValue ParsedElmFileRecordValue(
            string fileText,
            PineValue parsedModuleValue) =>
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("fileText", ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmString(fileText))),
                ("parsedModule", parsedModuleValue)
                ]);

        var compilerModulesParsedAsPineValues =
            parsedCompilerModules
            .Select(parsedModule =>
            ParsedElmFileRecordValue(
                fileText: parsedModule.Value.moduleText,
                parsedModuleValue: parsedModule.Value.parsed))
            .ToImmutableArray();

        var pineValueEmptyListElmValue =
            ElmValueInterop.PineValueEncodedAsInElmCompiler(PineValue.EmptyList);

        var pineValueEmptyListInCompiler =
            ElmValueEncoding.ElmValueAsPineValue(pineValueEmptyListElmValue);

        // var (optimizingPineVM, pineVMCache) = InteractiveSessionPine.BuildPineVM(caching: true, autoPGO: pgoShare);
        var (optimizingPineVM, pineVMCache) = InteractiveSessionPine.BuildPineVM(caching: true, autoPGO: null);

        {
            var compilerResponseValue =
                ElmInteractiveEnvironment.ApplyFunction(
                    optimizingPineVM,
                    declExpandInteractiveEnv,
                    arguments:
                    /*
                     * 
                        expandElmInteractiveEnvironmentWithModules :
                            Pine.Value
                            -> List ProjectParsedElmFile
                            -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
                     * */
                    // [pineValueEmptyListInCompiler, PineValue.List(compilerModulesParsedAsPineValues)]
                    [pineValueEmptyListInCompiler, PineValue.EmptyList]
                    )
                .Extract(err => throw new Exception(err));

            Assert.IsNotNull(compilerResponseValue, "compilerResponseValue");

            var compilerResponseElm =
                ElmValueEncoding.PineValueAsElmValue(compilerResponseValue)
                .Extract(err => throw new Exception(err));

            var compilerResponseValueString =
                compilerResponseValue switch
                {
                    PineValue.ListValue listValue =>
                    listValue.Elements.Count < 1
                    ?
                    null
                    :
                    PineValueAsString.StringFromValue(listValue.Elements[0]),

                    _ =>
                    null
                };

            Assert.AreEqual(
                "Ok { addedModules = [], environment = ListValue [] }",
                ElmValue.RenderAsElmExpression(compilerResponseElm).expressionString);
        }

        /*
         * Compile incrementally, one module at a time, like we did when using the JavaScript-based engine.
         * */

        Result<string, PineValue> CompileOneElmModule(
            PineValue prevEnvValue,
            string moduleText,
            PineValue parsedModuleValue,
            IPineVM pineVM)
        {
            var applyFunctionResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    declExpandInteractiveEnv,
                    arguments:
                    /*
                     * 
                        expandElmInteractiveEnvironmentWithModules :
                            Pine.Value
                            -> List ProjectParsedElmFile
                            -> Result String { addedModules : List ( List String, Pine.Value ), environment : Pine.Value }
                     * */
                    [
                        prevEnvValue,
                        PineValue.List([ParsedElmFileRecordValue(moduleText, parsedModuleValue)])
                    ]
                    );

            if (applyFunctionResult is Result<string, PineValue>.Err err)
                return "Failed to apply function: " + err.Value;

            if (applyFunctionResult is not Result<string, PineValue>.Ok applyFunctionOk)
                throw new Exception("Unexpected result type: " + applyFunctionResult.GetType().FullName);

            var parseAsTagResult = ElmValueEncoding.ParseAsTag(applyFunctionOk.Value);

            if (parseAsTagResult is Result<string, (string, IReadOnlyList<PineValue>)>.Err parseAsTagErr)
                return "Failed to parse result as tag: " + parseAsTagErr.Value;

            if (parseAsTagResult is not Result<string, (string, IReadOnlyList<PineValue>)>.Ok parseAsTagOk)
                throw new Exception("Unexpected result type: " + parseAsTagResult.GetType().FullName);

            if (parseAsTagOk.Value.Item1 is not "Ok")
                return
                    "Failed to extract environment: Tag not 'Ok': " +
                    ElmValueEncoding.PineValueAsElmValue(applyFunctionOk.Value)
                    .Unpack(
                        fromErr: err => "Failed to parse as Elm value: " + err,
                        fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString);

            if (parseAsTagOk.Value.Item2.Count is not 1)
                return "Failed to extract environment: Expected one element in the list, got " + parseAsTagOk.Value.Item2.Count;

            var parseAsRecordResult = ElmValueEncoding.ParsePineValueAsRecordTagged(parseAsTagOk.Value.Item2[0]);

            if (parseAsRecordResult is Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Err parseAsRecordErr)
                return "Failed to parse as record: " + parseAsRecordErr.Value;

            if (parseAsRecordResult is not Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>>.Ok parseAsRecordOk)
                throw new Exception("Unexpected result type: " + parseAsRecordResult.GetType().FullName);

            var environmentValueField =
                parseAsRecordOk.Value
                .SingleOrDefault(f => f.fieldName == "environment")
                .fieldValue;

            if (environmentValueField is not PineValue environmentValue)
                return "Failed to extract environment: not a Pine value: " + environmentValueField;

            return environmentValue;
        }

        {
            Console.WriteLine("Compiling simple module...");

            // Before attempting to compile the normal Basics module, test compiling a simple module.

            const string simpleElmModuleText =
                """
                module Namespace.Beta exposing (..)


                decl_name : String
                decl_name =
                    "Just a literal"
                
                """;

            var simpleElmModuleParsed =
                TryParseModuleText(simpleElmModuleText)
                .Extract(err => throw new Exception("Failed parsing simple module: " + err));

            var simpleModuleNameFlat = string.Join(".", simpleElmModuleParsed.Key);

            var simpleElmModuleAppCodeTree =
                compilerProgramOnlyElmJson
                .SetNodeAtPathSorted(
                    ["src", .. simpleElmModuleParsed.Key.SkipLast(1), simpleElmModuleParsed.Key.Last() + ".elm"],
                    TreeNodeWithStringPath.Blob(Encoding.UTF8.GetBytes(simpleElmModuleText)));

            using var newCompilerInteractiveSession =
                new InteractiveSessionPine(
                    compilerSourceFiles: compilerProgram,
                    appCodeTree: simpleElmModuleAppCodeTree,
                    caching: true,
                    autoPGO: null);

            var jsSessionState = newCompilerInteractiveSession.CurrentEnvironmentValue();

            var jsSessionParsedEnv =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(jsSessionState)
                .Extract(err => throw new Exception(err));

            var jsSimpleModuleCompiled =
                jsSessionParsedEnv
                .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

            var pineSessionStateWrapped =
                CompileOneElmModule(
                    prevEnvValue: ElmValueEncoding.ElmValueAsPineValue(pineValueEmptyListElmValue),
                    simpleElmModuleParsed.Value.moduleText,
                    simpleElmModuleParsed.Value.parsed,
                    pineVM: optimizingPineVM)
                .Extract(err => throw new Exception("Failed compiling simple module: " + err));

            var pineSessionStateWrappedElm =
                ElmValueEncoding.PineValueAsElmValue(pineSessionStateWrapped)
                .Extract(err => throw new Exception("Failed unwrapping pine session state: " + err));

            var pineSessionState =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(pineSessionStateWrappedElm)
                .Extract(err => throw new Exception("Failed unwrapping pine session state: " + err));

            var pineSessionParsedEnv =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(pineSessionState)
                .Extract(err => throw new Exception("Failed parsing environment: " + err));

            var pineSimpleModuleCompiled =
                pineSessionParsedEnv
                .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

            Assert.AreEqual(
                jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                pineSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                "Compiled simple module declarations count");

            foreach (var declName in jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Keys)
            {
                var jsDeclValue = jsSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];
                var pineDeclValue = pineSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];

                Assert.AreEqual(
                    jsDeclValue,
                    pineDeclValue,
                    "Compiled simple module declaration " + declName);
            }

            Assert.AreEqual(
                jsSimpleModuleCompiled.moduleValue,
                pineSimpleModuleCompiled.moduleValue,
                "Compiled simple module value");
        }

        /*
        {
            Console.WriteLine("Compile a slightly more complex module...");

            const string simpleElmModuleText =
                """
                module Namespace.Beta exposing (..)


                type Bool = True | False


                type String
                    = String (List Char.Char)

                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                    | AnyOtherKind


                {-| Represents the relative ordering of two things.
                The relations are less than, equal to, and greater than.
                -}
                type Order = LT | EQ | GT


                pow : Int -> Int -> Int
                pow base exponent =
                    if Pine_kernel.int_is_sorted_asc [ exponent, 0 ] then
                        1

                    else
                        powHelper base exponent 1

                
                powHelper : Int -> Int -> Int -> Int
                powHelper base exponent accumulator =
                    if Pine_kernel.equal [ exponent, 0 ] then
                        accumulator

                    else
                        powHelper base (Pine_kernel.int_add [ exponent, -1 ]) (Pine_kernel.int_mul [ base, accumulator ])


                replicate : appendable -> appendable
                replicate a =
                    case a of
                    String stringA ->
                        String (Pine_kernel.concat [ stringA, stringA ])

                    _ ->
                        Pine_kernel.concat [ a, a ]

                
                """;

            var simpleElmModuleParsed =
                TryParseModuleText(simpleElmModuleText)
                .Extract(err => throw new Exception("Failed parsing simple module: " + err));

            var simpleModuleNameFlat = string.Join(".", simpleElmModuleParsed.Key);

            var simpleElmModuleAppCodeTree =
                compilerProgramOnlyElmJson
                .SetNodeAtPathSorted(
                    ["src", .. simpleElmModuleParsed.Key.SkipLast(1), simpleElmModuleParsed.Key.Last() + ".elm"],
                    TreeNodeWithStringPath.Blob(Encoding.UTF8.GetBytes(simpleElmModuleText)));

            using var newCompilerInteractiveSession =
                new InteractiveSessionPine(
                    compileElmProgramCodeFiles: compilerProgram,
                    initialState: null,
                    appCodeTree: simpleElmModuleAppCodeTree,
                    caching: true,
                    autoPGO: null);

            var jsSessionState = newCompilerInteractiveSession.CurrentEnvironmentValue();

            var jsSessionParsedEnv =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(jsSessionState)
                .Extract(err => throw new Exception(err));

            var jsSimpleModuleCompiled =
                jsSessionParsedEnv
                .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

            var pineSessionStateWrapped =
                CompileOneElmModule(
                    prevEnvValue: pineValueEmptyListElmValue,
                    simpleElmModuleParsed.Value.moduleText,
                    simpleElmModuleParsed.Value.parsed,
                    pineVM: optimizingPineVM)
                .Extract(err => throw new Exception("Failed compiling simple module: " + err));

            var pineSessionState =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(pineSessionStateWrapped)
                .Extract(err => throw new Exception("Failed unwrapping pine session state: " + err));

            var pineSessionParsedEnv =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(pineSessionState)
                .Extract(err => throw new Exception("Failed parsing environment: " + err));

            var pineSimpleModuleCompiled =
                pineSessionParsedEnv
                .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

            Assert.AreEqual(
                jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                pineSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                "Compiled simple module declarations count");

            foreach (var declName in jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Keys)
            {
                var jsDeclValue = jsSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];
                var pineDeclValue = pineSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];

                Assert.AreEqual(
                    jsDeclValue,
                    pineDeclValue,
                    "Compiled simple module declaration " + declName);
            }

            Assert.AreEqual(
                jsSimpleModuleCompiled.moduleValue,
                pineSimpleModuleCompiled.moduleValue,
                "Compiled simple module value");
        }
        */

        /*
         * Begin a dedicated training phase, compiling a small Elm module and then doing code-analysis
         * and PGO for the entirety based on that simple scenario.
         * */

        /*

            Console.WriteLine("Begin training with Elm compiler...");

            var analysisEvalCache = new PineVMCache();

            var profilingVM =
                new ProfilingPineVM(
                overrideParseExpression: analysisEvalCache.BuildParseExprDelegate,
                    evalCache: analysisEvalCache.EvalCache,
                    analysisEvalCache: analysisEvalCache);

            var trainingProfilingStopwatch = System.Diagnostics.Stopwatch.StartNew();

            {
                // Compile reduced version of 'Basics' module

                const string simpleElmModuleText =
                    """
                module ReducedBasics exposing
                  ( Int
                  , (+), (-), (*), (/), (//)
                  , Bool(..), not
                  , (++)
                  )


                infix right 5 (++) = append
                infix left  6 (+)  = add
                infix left  6 (-)  = sub
                infix left  7 (*)  = mul
                infix left  7 (//) = idiv


                type Bool = True | False


                type String
                    = String (List Char.Char)

                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                    | AnyOtherKind


                append : appendable -> appendable -> appendable
                append a b =
                    case (a, b) of
                    (String stringA, String stringB) ->
                        String (Pine_kernel.concat [ stringA, stringB ])
                    _ -> Pine_kernel.concat [ a, b ]
                

                """;

                var simpleElmModuleParsed =
                    TryParseModuleText(simpleElmModuleText)
                    .Extract(err => throw new Exception("Failed parsing simple module: " + err));

                var simpleModuleNameFlat = string.Join(".", simpleElmModuleParsed.Key);

                var simpleElmModuleAppCodeTree =
                    compilerProgramOnlyElmJson
                    .SetNodeAtPathSorted(
                        ["src", .. simpleElmModuleParsed.Key.SkipLast(1), simpleElmModuleParsed.Key.Last() + ".elm"],
                        TreeNodeWithStringPath.Blob(Encoding.UTF8.GetBytes(simpleElmModuleText)));

                using var newCompilerInteractiveSession =
                    new InteractiveSessionPine(
                        compilerSourceFiles: compilerProgram,
                        initialState: null,
                        appCodeTree: simpleElmModuleAppCodeTree,
                        caching: true,
                        autoPGO: null);

                var jsSessionState = newCompilerInteractiveSession.CurrentEnvironmentValue();

                var jsSessionParsedEnv =
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(jsSessionState)
                    .Extract(err => throw new Exception(err));

                var jsSimpleModuleCompiled =
                    jsSessionParsedEnv
                    .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

                var pineSessionStateWrapped =
                    CompileOneElmModule(
                    prevEnvValue: pineValueEmptyListElmValue,
                        simpleElmModuleParsed.Value.moduleText,
                        simpleElmModuleParsed.Value.parsed,
                        pineVM: profilingVM.PineVM)
                    .Extract(err => throw new Exception("Failed compiling simple module: " + err));

                var pineSessionState =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(pineSessionStateWrapped)
                    .Extract(err => throw new Exception("Failed unwrapping pine session state: " + err));

                var pineSessionParsedEnv =
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(pineSessionState)
                    .Extract(err => throw new Exception("Failed parsing environment: " + err));

                var pineSimpleModuleCompiled =
                    pineSessionParsedEnv
                    .Modules.Single(m => m.moduleName == simpleModuleNameFlat);

                Assert.AreEqual(
                    jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                    pineSimpleModuleCompiled.moduleContent.FunctionDeclarations.Count,
                    "Compiled simple module declarations count");

                foreach (var declName in jsSimpleModuleCompiled.moduleContent.FunctionDeclarations.Keys)
                {
                    var jsDeclValue = jsSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];
                    var pineDeclValue = pineSimpleModuleCompiled.moduleContent.FunctionDeclarations[declName];

                    Assert.AreEqual(
                        jsDeclValue,
                        pineDeclValue,
                        "Compiled reduced Basics module declaration " + declName);
                }

                Assert.AreEqual(
                    jsSimpleModuleCompiled.moduleValue,
                    pineSimpleModuleCompiled.moduleValue,
                    "Compiled reduced Basics module value");
            }

            trainingProfilingStopwatch.Stop();

            var exprUsageCombinations = profilingVM.ExprEnvUsagesFlat;

        var totalSampleCount = exprUsageCombinations.Sum(sample => sample.Value.OrigEvalDurations.Count);

            Console.WriteLine(
                "Ran training scenario on profiling VM in " +
                trainingProfilingStopwatch.Elapsed.TotalSeconds.ToString("0.00") + " seconds and collected " +
                exprUsageCombinations.Count + " unique combinations from " + totalSampleCount + " execution samples.");

        var limitSampleCount = 100;

            var samplesUniqueExpressions =
                exprUsageCombinations
                .Select(sample => sample.Key)
                .Distinct()
                .ToImmutableArray();

            var exprUsageCombinationsAfterFilter =
                exprUsageCombinations
                .Where(report => report.Value.ParseAndEvalCountMax < 100)
                .ToImmutableArray();

            var includedSamples =
                DynamicPGOShare.SubsequenceWithEvenDistribution(
                    [.. exprUsageCombinationsAfterFilter], limitSampleCount);

            var trainingAnalysisStopwatch = System.Diagnostics.Stopwatch.StartNew();

            IReadOnlyList<ExpressionUsageAnalysis> expressionUsages =
                [..includedSamples
                .Select(sample => sample.Value.Analysis.Value.ToMaybe())
                .WhereNotNothing()
                .SelectMany(usage => usage)];

            trainingAnalysisStopwatch.Stop();

            Console.WriteLine(
                "Completed analysis in " +
                trainingAnalysisStopwatch.Elapsed.TotalSeconds.ToString("0.00") + " seconds and collected " +
                expressionUsages.Count + " usages");

        var usageProfiles = ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(expressionUsages);

            var compilation =
                DynamicPGOShare.GetOrCreateCompilationForProfiles(
                    inputProfiles: [usageProfiles],
                    limitCompiledExpressionsCount: 160,
                    previousCompilations: []);

            var compiledParseExpressionOverrides =
                compilation.DictionaryResult
                .Extract(err => throw new Exception("Failed compilation: " + err));

            Console.WriteLine(
                "Compiled dictionary contains " + compiledParseExpressionOverrides.Count + " entries");


            var compiledPineVMCache = new PineVMCache();

        var compiledPineVM = PineVM.Construct(
            parseExpressionOverrides: compiledParseExpressionOverrides,
                    evalCache: compiledPineVMCache.EvalCache);

        */

        /*
        if (false)
        {
            // Focus on test compiling the first module.

            var parsedModuleBasics =
                parsedCompilerModules
                .Single(m => m.Key.SequenceEqual(["Basics"]));

            var compileBasicsResult =
                CompileOneElmModule(
                    prevEnvValue: pineValueEmptyListElmValue,
                    parsedModuleBasics.Value.moduleText,
                    parsedModuleBasics.Value.parsed,
                    pineVM: optimizingPineVM)
                .Extract(err => throw new Exception("Failed compiling module Basics: " + err));

            var newInteractiveEnv =
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(compileBasicsResult)
                .Extract(err => throw new Exception(err));

            var newInteractiveParsedEnv =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(newInteractiveEnv)
                .Extract(err => throw new Exception("Failed parsing env after compiling module Basics: " + err));

            var newModuleBasics =
                newInteractiveParsedEnv
                .Modules.Single(m => m.moduleName is "Basics");

            Assert.AreEqual(
                moduleBasics.moduleContent.FunctionDeclarations.Count,
                newModuleBasics.moduleContent.FunctionDeclarations.Count,
                "Compiled module Basics declarations count");

            Assert.AreEqual(
                moduleBasics.moduleValue,
                newModuleBasics.moduleValue,
                "Compiled module Basics value");
        }
        */

        {
            PineValue compiledNewEnvInCompiler =
                ElmValueEncoding.ElmValueAsPineValue(pineValueEmptyListElmValue);

            foreach (var parsedModule in parsedCompilerModules)
            {
                var parsedModuleNameFlat = string.Join(".", parsedModule.Key);

                Console.WriteLine("\nBegin compile module " + parsedModuleNameFlat);

                var compileModuleStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var compileModuleResult =
                    CompileOneElmModule(
                        compiledNewEnvInCompiler,
                        parsedModule.Value.moduleText,
                        parsedModule.Value.parsed,
                        // pineVM: compiledPineVM
                        pineVM: optimizingPineVM
                        );

                if (compileModuleResult is not Result<string, PineValue>.Ok compileModuleOk)
                {
                    throw new Exception(
                        "Compiling module " + parsedModuleNameFlat + " failed: " +
                        compileModuleResult.Unpack(fromErr: err => err, fromOk: _ => "no err"));
                }

                Console.WriteLine(
                    "Compiled module " + parsedModuleNameFlat + " in " +
                    compileModuleStopwatch.Elapsed.TotalSeconds.ToString("0.00") + " seconds");

                compiledNewEnvInCompiler = compileModuleOk.Value;

                var javascriptCompiledModule =
                    interactiveParsedEnv
                    .Modules.Single(m => m.moduleName == parsedModuleNameFlat);

                /*
                 * 2024-06-10:
                 * Specialize parsing of the environment to reduce runtime of test:
                 * Instead of parsing the whole environment after each compilation iteration,
                 * use a more direct way to select the part representing the new compiled module.
                 * 
                var compiledNewEnvInCompilerElm =
                    ElmValueEncoding.PineValueAsElmValue(compileModuleOk.Value)
                    .Extract(err => throw new Exception(err));

                var compiledNewEnvElm =
                    ElmValueInterop.ElmValueDecodedAsInElmCompiler(compiledNewEnvInCompilerElm)
                    .Extract(err => throw new Exception(err));

                var pineCompiledInteractiveParsedEnv =
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledNewEnvElm)
                    .Extract(err => throw new Exception("Failed parsing env module Basics: " + err));

                var pineCompiledModule =
                    pineCompiledInteractiveParsedEnv
                    .Modules.Single(m => m.moduleName == parsedModuleNameFlat);
                
                 */

                var environmentList =
                    ShallowParseEnvironmentListEncodedInCompiler(compiledNewEnvInCompiler)
                    .Extract(err => throw new Exception("Failed parsing environment list: " + err));

                /*
                 * Expect the compiler appended the new Elm module as a single item at the end of the environment list.
                 * 
                 * Since the return value from the compiler is of type 'PineValue', the outermost list also contains the tag 'ListValue'.
                 * (as considered in ElmValueInterop.ElmValueDecodedAsInElmCompiler)
                 * */

                var pineCompiledModule =
                    environmentList.Last().Invoke()
                    .Extract(err => throw new Exception("Failed parsing last module from environment list: " + err));

                Assert.AreEqual(parsedModuleNameFlat, pineCompiledModule.moduleName, "parsed module name");

                /*
                 
                Assert.AreEqual(
                    javascriptCompiledModule.moduleContent.TypeDeclarations.Count,
                    pineCompiledModule.moduleContent.TypeDeclarations.Count,
                    "Compiled module " + parsedModuleNameFlat + " type declarations count");

                foreach (var typeName in javascriptCompiledModule.moduleContent.TypeDeclarations.Keys)
                {
                    var javascriptTypeValue = javascriptCompiledModule.moduleContent.TypeDeclarations[typeName];
                    var pineTypeValue = pineCompiledModule.moduleContent.TypeDeclarations[typeName];

                    var javascriptTypeSize = ElmInteractive.EstimatePineValueMemoryUsage(javascriptTypeValue);
                    var pineTypeSize = ElmInteractive.EstimatePineValueMemoryUsage(pineTypeValue);

                    Assert.AreEqual(
                        javascriptTypeValue,
                        pineTypeValue,
                        "Compiled module " + parsedModuleNameFlat + " type declaration " + typeName +
                        " (js size: " + javascriptTypeSize + ", pine size: " + pineTypeSize + ")");
                }

                Assert.AreEqual(
                    javascriptCompiledModule.moduleContent.FunctionDeclarations.Count,
                    pineCompiledModule.moduleContent.FunctionDeclarations.Count,
                    "Compiled module " + parsedModuleNameFlat + " function declarations count");

                foreach (var declName in javascriptCompiledModule.moduleContent.FunctionDeclarations.Keys)
                {
                    var javascriptDeclValue = javascriptCompiledModule.moduleContent.FunctionDeclarations[declName];
                    var pineDeclValue = pineCompiledModule.moduleContent.FunctionDeclarations[declName];

                    var javascriptDeclSize = ElmInteractive.EstimatePineValueMemoryUsage(javascriptDeclValue);
                    var pineDeclSize = ElmInteractive.EstimatePineValueMemoryUsage(pineDeclValue);

                    Assert.AreEqual(
                        javascriptDeclValue,
                        pineDeclValue,
                        "Compiled module " + parsedModuleNameFlat + " function declaration " + declName +
                        " (js size: " + javascriptDeclSize + ", pine size: " + pineDeclSize + ")");
                }
                */

                Console.WriteLine(
                    string.Join(
                        "\n",
                        ["\nCompiled module " + parsedModuleNameFlat + " comparison:",
                        ..ReportOnCompiledModule(
                            javascriptCompiledModule.moduleContent,
                            pineCompiledModule.moduleContent)]));

                Assert.AreEqual(
                    javascriptCompiledModule.moduleValue,
                    pineCompiledModule.moduleValue,
                    "Compiled module " + parsedModuleNameFlat + " value");
            }

            if (false)
            {
                var compiledNewEnvInCompilerElm =
                    ElmValueEncoding.PineValueAsElmValue(compiledNewEnvInCompiler)
                    .Extract(err => throw new Exception(err));

                var compiledNewEnvValue =
                    ElmValueInterop.ElmValueDecodedAsInElmCompiler(compiledNewEnvInCompilerElm)
                    .Extract(err => throw new Exception(err));

                Console.WriteLine(
                    "Compiled environment is " +
                    compiledNewEnvValue switch
                    {
                        PineValue.ListValue listValue =>
                        "List with " + listValue.Elements.Count + " elements",

                        _ =>
                        "not a list"
                    });

                Assert.AreEqual(
                    interactiveInitialState,
                    compiledNewEnvValue);
            }
        }
    }

    /// <summary>
    /// Shallow counterpart to <see cref="ElmInteractiveEnvironment.ParseInteractiveEnvironment(PineValue)"/>
    /// </summary>
    public static Result<string, IReadOnlyList<Func<Result<string, (string moduleName, PineValue moduleValue, ElmInteractiveEnvironment.ElmModule moduleContent)>>>>
        ShallowParseEnvironmentListEncodedInCompiler(PineValue interactiveEnvironment)
    {
        /*
         * Since the return value from the compiler is of type 'PineValue', the outermost list also contains the tag 'ListValue'.
         * (as considered in ElmValueInterop.ElmValueDecodedAsInElmCompiler)
         * */

        var parseAsTagResult =
            ElmValueEncoding.ParseAsTag(interactiveEnvironment);

        if (parseAsTagResult is Result<string, (string, IReadOnlyList<PineValue>)>.Err parseAsTagErr)
            return "Failed parsing outermost as tag: " + parseAsTagErr.Value;

        if (parseAsTagResult is not Result<string, (string, IReadOnlyList<PineValue>)>.Ok parseAsTagOk)
            throw new NotImplementedException("Unexpected result type: " + parseAsTagResult.GetType().FullName);

        if (parseAsTagOk.Value.Item1 is not "ListValue")
            return "Unexpected tag name: " + parseAsTagOk.Value.Item1;

        if (parseAsTagOk.Value.Item2.Count is not 1)
            return "Unexpected number of tag arguments: " + parseAsTagOk.Value.Item2.Count;

        if (parseAsTagOk.Value.Item2[0] is not PineValue.ListValue environmentList)
            return "interactive environment not a list";

        static Result<string, (string moduleName, PineValue moduleValue, ElmInteractiveEnvironment.ElmModule moduleContent)> ParseModuleEncodedInCompiler(
            PineValue moduleEncodedInCompiler)
        {
            return
                ElmValueEncoding.PineValueAsElmValue(moduleEncodedInCompiler)
                .AndThen(moduleAsElmValueEncodedInCompiler =>
                ElmValueInterop.ElmValueDecodedAsInElmCompiler(moduleAsElmValueEncodedInCompiler)
                .AndThen(modulePineValue =>
                ElmInteractiveEnvironment.ParseNamedElmModule(modulePineValue)));
        }

        return
        Result<string, IReadOnlyList<Func<Result<string, (string moduleName, PineValue moduleValue, ElmInteractiveEnvironment.ElmModule moduleContent)>>>>.ok(
            [..environmentList.Elements
                .Select(envItem =>
                new Func<Result<string, (string moduleName, PineValue moduleValue, ElmInteractiveEnvironment.ElmModule moduleContent)>>(
                    () => ParseModuleEncodedInCompiler(envItem)))]);
    }

    public static IEnumerable<string> ReportOnCompiledModule(
        ElmInteractiveEnvironment.ElmModule expectedModule,
        ElmInteractiveEnvironment.ElmModule actualModule) =>
        [ "Type declarations",
            ..ReportComparingDeclarations(
                expectedModule.TypeDeclarations,
                actualModule.TypeDeclarations),

            "Function declarations",
            ..ReportComparingDeclarations(
                expectedModule.FunctionDeclarations,
                actualModule.FunctionDeclarations),
            ];

    public static IEnumerable<string> ReportComparingDeclarations(
        IReadOnlyDictionary<string, PineValue> expectedDecls,
        IReadOnlyDictionary<string, PineValue> actualDecls)
    {
        yield return
            "Declarations count: " +
            expectedDecls.Count +
            " " +
            (expectedDecls.Count == actualDecls.Count ? "Ok" : "Mismatch (" + actualDecls.Count + ")");

        var expectedNames =
            expectedDecls.Keys.Order().ToImmutableArray();

        static (bool isOk, string description) CompareDecl(
            PineValue expected,
            PineValue? actual)
        {
            if (actual is null)
            {
                return (false, "Missing declaration");
            }

            var actualSize = ElmInteractive.EstimatePineValueMemoryUsage(actual);

            return
                expected == actual
                ? (true, "")
                : (false, "Mismatch (size: " + actualSize + ")");
        }

        foreach (var declName in expectedNames)
        {
            var expectedDeclValue = expectedDecls[declName];

            var expectedDeclSize =
                ElmInteractive.EstimatePineValueMemoryUsage(expectedDeclValue);

            actualDecls.TryGetValue(declName, out var actualDeclValue);

            var declActualReport = CompareDecl(expectedDeclValue, actualDeclValue);

            var passedIndicationSymbol =
                declActualReport.isOk ? "Ok" : "Mismatch";

            yield return
                passedIndicationSymbol + " " + declName + " (" + expectedDeclSize + "): " +
                declActualReport.description;
        }
    }
}
