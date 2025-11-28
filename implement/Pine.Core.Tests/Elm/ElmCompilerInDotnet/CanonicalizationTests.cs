using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class CanonicalizationTests
{
    // Helper to extract a canonicalized module by name from the result dictionary
    private static File GetCanonicalizedModule(
        Result<string, IReadOnlyDictionary<IReadOnlyList<string>, Result<string, File>>> canonicalizeResult,
        string[] moduleName)
    {
        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var moduleResult =
            modulesDict
            .FirstOrDefault(kvp => kvp.Key.SequenceEqual(moduleName))
            .Value;

        return
            moduleResult is null
            ?
            throw new System.Exception($"Module {string.Join(".", moduleName)} not found in canonicalization result")
            :
            moduleResult
            .Extract(err => throw new System.Exception($"Module {string.Join(".", moduleName)} has errors: " + err));
    }

    private static File CanonicalizeAndGetSingleModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName)
    {
        var parsedModules =
            elmModulesTexts
            .Select(text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .ToList();

        var canonicalizeResult =
            Canonicalization.Canonicalize(parsedModules);

        return
            GetCanonicalizedModule(canonicalizeResult, [.. moduleName]);
    }

    [Fact]
    public void Expands_reference_to_local_function_declaration()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                beta (x * 3)


            beta x =
                x + 1

            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        var asElmValue = EncodeAsElmValue.EncodeFile(canonicalizedModule);

        var asElmExpression =
            ElmValue.RenderAsElmExpression(asElmValue);

        asElmExpression.expressionString.Should().Be(
            """{ comments = [], declarations = [Node { end = { column = 17, row = 5 }, start = { column = 1, row = 4 } } (FunctionDeclaration { declaration = Node { end = { column = 17, row = 5 }, start = { column = 1, row = 4 } } { arguments = [Node { end = { column = 7, row = 4 }, start = { column = 6, row = 4 } } (VarPattern "x")], expression = Node { end = { column = 17, row = 5 }, start = { column = 5, row = 5 } } (Application [Node { end = { column = 9, row = 5 }, start = { column = 5, row = 5 } } (FunctionOrValue ["Test"] "beta"),Node { end = { column = 17, row = 5 }, start = { column = 10, row = 5 } } (ParenthesizedExpression (Node { end = { column = 16, row = 5 }, start = { column = 11, row = 5 } } (OperatorApplication "*" Left (Node { end = { column = 12, row = 5 }, start = { column = 11, row = 5 } } (FunctionOrValue [] "x")) (Node { end = { column = 16, row = 5 }, start = { column = 15, row = 5 } } (Integer 3)))))]), name = Node { end = { column = 5, row = 4 }, start = { column = 1, row = 4 } } "alfa" }, documentation = Nothing, signature = Nothing }),Node { end = { column = 10, row = 9 }, start = { column = 1, row = 8 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 9 }, start = { column = 1, row = 8 } } { arguments = [Node { end = { column = 7, row = 8 }, start = { column = 6, row = 8 } } (VarPattern "x")], expression = Node { end = { column = 10, row = 9 }, start = { column = 5, row = 9 } } (OperatorApplication "+" Left (Node { end = { column = 6, row = 9 }, start = { column = 5, row = 9 } } (FunctionOrValue [] "x")) (Node { end = { column = 10, row = 9 }, start = { column = 9, row = 9 } } (Integer 1))), name = Node { end = { column = 5, row = 8 }, start = { column = 1, row = 8 } } "beta" }, documentation = Nothing, signature = Nothing })], imports = [], moduleDefinition = Node { end = { column = 26, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 26, row = 1 }, start = { column = 13, row = 1 } } (All { end = { column = 25, row = 1 }, start = { column = 23, row = 1 } }), moduleName = Node { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } } ["Test"] }) }""");
    }

    [Fact]
    public void Expands_reference_to_foreign_function_declaration()
    {
        var module1Text =
            """"
            module OtherModule exposing (..)


            otherFunc x =
                x + 1
            """";

        var module2Text =
            """"
            module MainModule exposing (..)

            import OtherModule exposing (otherFunc)


            main x =
                otherFunc x
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["MainModule"]);

        // Find the function declaration for 'main'
        var mainFuncDecl = mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "main");

        // Get the expression (otherFunc x)
        var expr = mainFuncDecl.Function.Declaration.Value.Expression.Value;

        // It should be an Application
        expr.Should().BeOfType<SyntaxTypes.Expression.Application>();
        var application = (SyntaxTypes.Expression.Application)expr;

        // First argument should be FunctionOrValue with module name "OtherModule"
        var funcOrValue = application.Arguments[0].Value;
        funcOrValue.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>();
        var funcRef = (SyntaxTypes.Expression.FunctionOrValue)funcOrValue;

        funcRef.Should().Be(new SyntaxTypes.Expression.FunctionOrValue(
            ModuleName: ["OtherModule"],
            Name: "otherFunc"));
    }

    [Fact]
    public void Expands_reference_to_local_choice_type_declaration()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type MyType
                = OptionA
                | OptionB Int


            useType : MyType -> Int
            useType value =
                case value of
                    OptionA ->
                        0

                    OptionB x ->
                        x
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Find the function declaration for 'useType'
        var useTypeFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "useType");

        // Get the case expression
        var caseExpr = useTypeFunc.Function.Declaration.Value.Expression.Value;
        caseExpr.Should().BeOfType<SyntaxTypes.Expression.CaseExpression>();
        var caseBlock = ((SyntaxTypes.Expression.CaseExpression)caseExpr).CaseBlock;

        // Check the patterns - they should have module names
        var firstPattern = caseBlock.Cases[0].Pattern.Value;
        firstPattern.Should().BeOfType<Pattern.NamedPattern>();
        var firstNamed = (Pattern.NamedPattern)firstPattern;
        firstNamed.Name.Should().Be(new QualifiedNameRef(["Test"], "OptionA"));

        var secondPattern = caseBlock.Cases[1].Pattern.Value;
        secondPattern.Should().BeOfType<Pattern.NamedPattern>();
        var secondNamed = (Pattern.NamedPattern)secondPattern;
        secondNamed.Name.Should().Be(new QualifiedNameRef(["Test"], "OptionB"));
    }

    [Fact]
    public void Expands_reference_to_local_record_type_declaration()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type alias Person =
                { name : String
                , age : Int
                }


            getName : Person -> String
            getName person =
                person.name
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Find the function declaration for 'getName'
        var getNameFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "getName");

        // Get the type signature
        var signature = getNameFunc.Function.Signature;
        signature.Should().NotBeNull();

        var typeAnnotation = signature.Value.TypeAnnotation.Value;
        typeAnnotation.Should().BeOfType<TypeAnnotation.FunctionTypeAnnotation>();
        var funcType = (TypeAnnotation.FunctionTypeAnnotation)typeAnnotation;

        // The argument type should be 'Person' which should be resolved to ["Test"]
        var argType = funcType.ArgumentType.Value;
        argType.Should().BeOfType<TypeAnnotation.Typed>();
        var typedArg = (TypeAnnotation.Typed)argType;
        var (moduleName, name) = typedArg.TypeName.Value;
        moduleName.Should().Equal(["Test"]);
        name.Should().Be("Person");
    }

    [Fact]
    public void Expands_reference_to_foreign_choice_type_declaration()
    {
        var module1Text =
            """"
            module Types exposing (..)


            type Status
                = Active
                | Inactive
            """";

        var module2Text =
            """"
            module Main exposing (..)

            import Types exposing (Status(..))


            isActive : Status -> Bool
            isActive status =
                case status of
                    Active ->
                        True

                    Inactive ->
                        False
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["Main"]);

        // Find the function declaration for 'isActive'
        var isActiveFunc =
            mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "isActive");

        // Get the case expression
        var caseExpr = isActiveFunc.Function.Declaration.Value.Expression.Value;
        caseExpr.Should().BeOfType<SyntaxTypes.Expression.CaseExpression>();
        var caseBlock = ((SyntaxTypes.Expression.CaseExpression)caseExpr).CaseBlock;

        // Check the patterns - they should have module names from Types module
        var firstPattern = caseBlock.Cases[0].Pattern.Value;
        firstPattern.Should().BeOfType<Pattern.NamedPattern>();
        var firstNamed = (Pattern.NamedPattern)firstPattern;
        firstNamed.Name.Should().Be(new QualifiedNameRef(["Types"], "Active"));

        // Also check the type annotation
        var signature = isActiveFunc.Function.Signature;
        signature.Should().NotBeNull();
        var funcType = (TypeAnnotation.FunctionTypeAnnotation)signature.Value.TypeAnnotation.Value;
        var argType = (TypeAnnotation.Typed)funcType.ArgumentType.Value;
        var (moduleName, name) = argType.TypeName.Value;
        moduleName.Should().Equal(["Types"]);
        name.Should().Be("Status");
    }

    [Fact]
    public void Expands_reference_to_foreign_record_type_declaration()
    {
        var module1Text =
            """"
            module Types exposing (..)


            type alias User =
                { id : Int
                , name : String
                }
            """";

        var module2Text =
            """"
            module Main exposing (..)

            import Types exposing (User)


            getUserName : User -> String
            getUserName user =
                user.name
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["Main"]);

        // Find the function declaration for 'getUserName'
        var getUserNameFunc =
            mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "getUserName");

        // Get the type signature
        var signature = getUserNameFunc.Function.Signature;
        signature.Should().NotBeNull();

        var typeAnnotation = signature.Value.TypeAnnotation.Value;
        typeAnnotation.Should().BeOfType<TypeAnnotation.FunctionTypeAnnotation>();
        var funcType = (TypeAnnotation.FunctionTypeAnnotation)typeAnnotation;

        // The argument type should be 'User' which should be resolved to ["Types"]
        var argType = funcType.ArgumentType.Value;

        argType.Should().BeOfType<TypeAnnotation.Typed>();

        var typedArg = (TypeAnnotation.Typed)argType;
        var (moduleName, name) = typedArg.TypeName.Value;

        moduleName.Should().Equal(["Types"]);
        name.Should().Be("User");
    }

    [Fact]
    public void Removes_import_syntax_nodes()
    {
        var module1Text =
            """"
            module Module1 exposing (..)


            helper x =
                x + 1
            """";

        var module2Text =
            """"
            module Module2 exposing (..)

            import Module1 exposing (helper)


            main =
                helper 5
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        // Verify that all modules have empty imports lists
        foreach (var (moduleName, moduleResult) in modulesDict)
        {
            var module =
                moduleResult
                .Extract(err => throw new System.Exception($"Module {string.Join(".", moduleName)} has errors: " + err));

            module.Imports.Should().BeEmpty();
        }
    }

    [Fact]
    public void Reports_all_module_names_when_exposed_imports_clash()
    {
        // Add two modules, one with import from other. Verify collection of import syntax nodes is empty after processing.
        // Returned error string should contain the name of each module which contributes to the clash.

        var module1Text =
            """"
            module Module1 exposing (..)


            helper x =
                x + 1
            """";

        var module2Text =
            """"
            module Module2 exposing (..)


            helper x =
                x * 2
            """";

        var module3Text =
            """"
            module Module3 exposing (..)

            import Module1 exposing (helper)
            import Module2 exposing (helper)


            main =
                helper 5
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var parsedModule3 =
            ElmSyntaxParser.ParseModuleText(module3Text)
            .Extract(err => throw new System.Exception("Failed parsing module 3: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2, parsedModule3]);

        // With per-module error reporting, canonicalization returns Ok with a dictionary
        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Unexpected global error: " + err));

        // Module3 should have an error due to the clash
        var module3Result =
            modulesDict
            .First(kvp => kvp.Key.SequenceEqual(["Module3"]))
            .Value;

        // Extract the error message from Module3
        var errorMessage =
            module3Result.Unpack(
                fromErr: err => err,
                fromOk: _ => throw new System.Exception("Expected Module3 to have an error due to clashing imports"));

        // Error message should contain both module names
        errorMessage.Should().Contain("Module1");
        errorMessage.Should().Contain("Module2");
        errorMessage.Should().Contain("helper");
    }

    [Fact]
    public void Resolves_module_alias()
    {
        var module1Text =
            """"
            module Bytes.Decode exposing (int)


            int : Int
            int =
                42
            """";

        var module2Text =
            """"
            module Main exposing (..)

            import Bytes.Decode as Decode


            value =
                Decode.int
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["Main"]);

        // Find the value declaration
        var valueDecl = mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "value");

        // Get the expression (Decode.int)
        var expr = valueDecl.Function.Declaration.Value.Expression.Value;
        expr.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>();
        var funcOrValue = (SyntaxTypes.Expression.FunctionOrValue)expr;

        // Should be resolved from alias "Decode" to actual module "Bytes.Decode"
        funcOrValue.Should().Be(new SyntaxTypes.Expression.FunctionOrValue(
            ModuleName: ["Bytes", "Decode"],
            Name: "int"));
    }

    [Fact]
    public void Handles_exposing_all()
    {
        var module1Text =
            """"
            module Helper exposing (..)


            double x =
                x * 2


            triple x =
                x * 3
            """";

        var module2Text =
            """"
            module Main exposing (..)

            import Helper exposing (..)


            compute x =
                double x + triple x
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2]);

        var canonicalizedModules =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["Main"]);

        // Find the compute declaration
        var computeDecl = mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "compute");

        // Get the expression (double x + triple x)
        var expr = computeDecl.Function.Declaration.Value.Expression.Value;
        expr.Should().BeOfType<SyntaxTypes.Expression.OperatorApplication>();
        var opApp = (SyntaxTypes.Expression.OperatorApplication)expr;

        // Left side should be application of double
        var leftApp = opApp.Left.Value;
        leftApp.Should().BeOfType<SyntaxTypes.Expression.Application>();
        var doubleApp = (SyntaxTypes.Expression.Application)leftApp;
        var doubleFuncOrValue = (SyntaxTypes.Expression.FunctionOrValue)doubleApp.Arguments[0].Value;
        doubleFuncOrValue.Should().Be(new SyntaxTypes.Expression.FunctionOrValue(
            ModuleName: ["Helper"],
            Name: "double"));
    }

    [Fact]
    public void Resolves_nested_pattern_type_constructors()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type Maybe a
                = Just a
                | Nothing


            type Result a
                = Ok a
                | Err


            unwrap value =
                case value of
                    Just (Ok x) ->
                        x

                    _ ->
                        0
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Find the unwrap function
        var unwrapFunc = canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "unwrap");

        // Get the case expression
        var caseExpr = (SyntaxTypes.Expression.CaseExpression)unwrapFunc.Function.Declaration.Value.Expression.Value;
        var caseBlock = caseExpr.CaseBlock;

        // Check the first pattern - Just (Ok x)
        var firstPattern = caseBlock.Cases[0].Pattern.Value;
        firstPattern.Should().BeOfType<Pattern.NamedPattern>();
        var justPattern = (Pattern.NamedPattern)firstPattern;
        justPattern.Name.Should().Be(new QualifiedNameRef(["Test"], "Just"));

        // Check the nested pattern - Ok x (may be wrapped in ParenthesizedPattern)
        var nestedPatternNode = justPattern.Arguments[0];
        var nestedPattern = nestedPatternNode.Value;

        // Unwrap if parenthesized
        if (nestedPattern is Pattern.ParenthesizedPattern parenthesized)
        {
            nestedPattern = parenthesized.Pattern.Value;
        }

        nestedPattern.Should().BeOfType<Pattern.NamedPattern>();
        var okPattern = (Pattern.NamedPattern)nestedPattern;
        okPattern.Name.Should().Be(new QualifiedNameRef(["Test"], "Ok"));
    }

    [Fact]
    public void Resolves_nested_type_annotation()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type Optional a
                = Some a
                | None


            type Outcome a
                = Success a
                | Failure


            process : Optional (Outcome Int) -> Int
            process value =
                0
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Find the process function
        var processFunc = canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "process");

        // Get the type signature
        var signature = processFunc.Function.Signature;
        signature.Should().NotBeNull();

        var funcType = (TypeAnnotation.FunctionTypeAnnotation)signature.Value.TypeAnnotation.Value;
        var argType = (TypeAnnotation.Typed)funcType.ArgumentType.Value;

        // Should be Optional (not Maybe)
        var (moduleName, name) = argType.TypeName.Value;
        moduleName.Should().Equal(["Test"]);
        name.Should().Be("Optional");

        // Check the nested type argument - Outcome Int
        var optionalArg = (TypeAnnotation.Typed)argType.TypeArguments[0].Value;
        var (nestedModuleName, nestedName) = optionalArg.TypeName.Value;
        nestedModuleName.Should().Equal(["Test"]);
        nestedName.Should().Be("Outcome");
    }

    [Fact]
    public void Resolves_type_annotation_in_local_declaration()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type MyType
                = MyValue


            compute x =
                let
                    helper : MyType -> Int
                    helper value =
                        42
                in
                helper MyValue
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Find the compute function
        var computeFunc = canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "compute");

        // Get the let expression
        var letExpr = (SyntaxTypes.Expression.LetExpression)computeFunc.Function.Declaration.Value.Expression.Value;
        var letBlock = letExpr.Value;

        // Get the helper function from let declarations
        var helperDecl = letBlock.Declarations[0].Value;
        helperDecl.Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetFunction>();
        var helperFunc = ((SyntaxTypes.Expression.LetDeclaration.LetFunction)helperDecl).Function;

        // Check the type signature
        var helperSignature = helperFunc.Signature;
        helperSignature.Should().NotBeNull();

        var helperFuncType = (TypeAnnotation.FunctionTypeAnnotation)helperSignature.Value.TypeAnnotation.Value;
        var helperArgType = (TypeAnnotation.Typed)helperFuncType.ArgumentType.Value;

        // Should be MyType resolved to Test module
        var (moduleName, name) = helperArgType.TypeName.Value;
        moduleName.Should().Equal(["Test"]);
        name.Should().Be("MyType");
    }

    [Fact]
    public void Maps_core_types_to_modules()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            intValue : Int
            intValue =
                42


            floatValue : Float
            floatValue =
                3.14


            boolValue : Bool
            boolValue =
                True


            charValue : Char
            charValue =
                'a'


            stringValue : String
            stringValue =
                "hello"
            """";

        var parsedModule =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule]);

        var canonicalizedModule = GetCanonicalizedModule(canonicalizeResult, ["Test"]);

        // Check Int
        var intFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "intValue");
        var intType = (TypeAnnotation.Typed)intFunc.Function.Signature!.Value.TypeAnnotation.Value;
        var (intModuleName, intName) = intType.TypeName.Value;
        intModuleName.Should().Equal(["Basics"]);
        intName.Should().Be("Int");

        // Check Float
        var floatFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "floatValue");

        var floatType = (TypeAnnotation.Typed)floatFunc.Function.Signature!.Value.TypeAnnotation.Value;
        var (floatModuleName, floatName) = floatType.TypeName.Value;
        floatModuleName.Should().Equal(["Basics"]);
        floatName.Should().Be("Float");

        // Check Bool
        var boolFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "boolValue");

        var boolType = (TypeAnnotation.Typed)boolFunc.Function.Signature!.Value.TypeAnnotation.Value;
        var (boolModuleName, boolName) = boolType.TypeName.Value;
        boolModuleName.Should().Equal(["Basics"]);
        boolName.Should().Be("Bool");

        // Check Char
        var charFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "charValue");

        var charType = (TypeAnnotation.Typed)charFunc.Function.Signature!.Value.TypeAnnotation.Value;
        var (charModuleName, charName) = charType.TypeName.Value;
        charModuleName.Should().Equal(["Char"]);
        charName.Should().Be("Char");

        // Check String
        var stringFunc =
            canonicalizedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "stringValue");

        var stringType = (TypeAnnotation.Typed)stringFunc.Function.Signature!.Value.TypeAnnotation.Value;
        var (stringModuleName, stringName) = stringType.TypeName.Value;
        stringModuleName.Should().Equal(["String"]);
        stringName.Should().Be("String");
    }

    [Fact]
    public void Exposes_only_type_constructors_not_all_exports()
    {
        var moduleTypesText =
            """"
            module Types exposing (Status(..))


            type Status
                = Active
                | Inactive


            type MyResult
                = MyOk
                | MyErr


            helper x =
                x + 1
            """";

        var moduleMainText =
            """"
            module Main exposing (..)

            import Types exposing (Status(..))


            -- This should work - Active and Inactive are constructors of Status
            checkStatus status =
                case status of
                    Active ->
                        1

                    Inactive ->
                        0


            -- This should NOT work - MyOk is not a constructor of Status
            useWrongConstructor =
                MyOk
            """";

        var parsedModuleTypes =
            ElmSyntaxParser.ParseModuleText(moduleTypesText)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModuleMain =
            ElmSyntaxParser.ParseModuleText(moduleMainText)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModuleTypes, parsedModuleMain]);

        // Since the name 'MyOk' is not available in Main, canonicalization should return an error for module Main

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Unexpected global error: " + err));

        var moduleMainResult =
            modulesDict[["Main"]];

        var errorMessage =
            moduleMainResult.Unpack(
                fromErr: err => err,
                fromOk: _ => throw new System.Exception("Expected Main module to have an error due to unknown constructor"));

        errorMessage.Should().Contain("MyOk");
    }

    [Fact]
    public void No_clash_when_only_one_module_exposes_overlapping_name()
    {
        // Test that there's no import clash when two imported modules have the same declaration name,
        // but only one of them exposes it.

        var module1Text =
            """"
            module Module1 exposing (helper)


            helper x =
                x + 1


            internal x =
                x * 2
            """";

        var module2Text =
            """"
            module Module2 exposing (other)


            helper x =
                x * 3


            other x =
                x - 1
            """";

        var module3Text =
            """"
            module Main exposing (..)

            import Module1 exposing (..)
            import Module2 exposing (..)


            -- Should work fine - helper is only exposed by Module1
            -- Module2 has a helper function but doesn't expose it
            main =
                helper 5
            """";

        var parsedModule1 =
            ElmSyntaxParser.ParseModuleText(module1Text)
            .Extract(err => throw new System.Exception("Failed parsing module 1: " + err));

        var parsedModule2 =
            ElmSyntaxParser.ParseModuleText(module2Text)
            .Extract(err => throw new System.Exception("Failed parsing module 2: " + err));

        var parsedModule3 =
            ElmSyntaxParser.ParseModuleText(module3Text)
            .Extract(err => throw new System.Exception("Failed parsing module 3: " + err));

        var canonicalizeResult =
            Canonicalization.Canonicalize([parsedModule1, parsedModule2, parsedModule3]);

        var modulesDict = canonicalizeResult
            .Extract(err => throw new System.Exception("Unexpected global error: " + err));

        // All modules should succeed - no clash because Module2 doesn't expose helper
        var mainModule = GetCanonicalizedModule(canonicalizeResult, ["Main"]);

        // Find the main function
        var mainFunc = mainModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single(f => f.Function.Declaration.Value.Name.Value is "main");

        // Get the expression (helper 5)
        var application = (SyntaxTypes.Expression.Application)mainFunc.Function.Declaration.Value.Expression.Value;
        var helperRef = (SyntaxTypes.Expression.FunctionOrValue)application.Arguments[0].Value;

        // helper should resolve to Module1, not cause a clash
        helperRef.Should().Be(new SyntaxTypes.Expression.FunctionOrValue(
            ModuleName: ["Module1"],
            Name: "helper"));
    }

    [Fact]
    public void Distinguishes_type_name_from_choice_type_tag_name_exposing_all()
    {
        var nodeModuleText =
            """"
            module Node exposing (..)


            type alias Range = {}

            type Node a
                = Node Range a
            """";

        var appModuleText =
            """"
            module App exposing (..)
            
            import Node exposing (..)


            decl : Node Int
            decl =
                Node {} 5


            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            decl : Node.Node Basics.Int
            decl =
                Node.Node
                    {}
                    5

            """";

        var appModuleCanonicalized =
            CanonicalizeAndGetSingleModule(
                elmModulesTexts:
                [nodeModuleText, appModuleText],
                moduleName: ["App"]);

        var renderedAppModule =
            Rendering.ToString(
                appModuleCanonicalized,
                Rendering.ConfigNormalizeAllLocations(lineBreaking: Rendering.LineBreakingConfig.SnapshotTestsDefault));

        renderedAppModule.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void Distinguishes_type_name_from_choice_type_tag_name_exposing_named()
    {
        var nodeModuleText =
            """"
            module Node exposing (..)


            type alias Range = {}

            type Node a
                = Node Range a
            """";

        var appModuleText =
            """"
            module App exposing (..)
            
            import Node exposing (Node(..))


            decl : Node Int
            decl =
                Node {} 5

            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            decl : Node.Node Basics.Int
            decl =
                Node.Node
                    {}
                    5

            """";

        var appModuleCanonicalized =
            CanonicalizeAndGetSingleModule(
                elmModulesTexts:
                [nodeModuleText, appModuleText],
                moduleName: ["App"]);

        var renderedAppModule =
            Rendering.ToString(
                appModuleCanonicalized,
                Rendering.ConfigNormalizeAllLocations(lineBreaking: Rendering.LineBreakingConfig.SnapshotTestsDefault));

        renderedAppModule.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void Distinguishing_between_type_name_and_choice_type_tag_name_depends_on_context()
    {
        var alfaModuleText =
            """"
            module Alfa exposing (..)


            type alias Range = {}

            type ChoiceType a
                = Node Range a

            """";

        var betaModuleText =
            """"
            module Beta exposing (..)


            type alias Range = {}

            type Node a
                = SomeTag Range a

            """";

        var appModuleText =
            """"
            module App exposing (..)
            
            import Alfa exposing (ChoiceType(..))
            import Beta exposing (Node(..))


            decl : Node Int
            decl =
                Node {} 5

            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            decl : Beta.Node Basics.Int
            decl =
                Alfa.Node
                    {}
                    5

            """";

        var appModuleCanonicalized =
            CanonicalizeAndGetSingleModule(
                elmModulesTexts:
                [alfaModuleText, betaModuleText, appModuleText],
                moduleName: ["App"]);

        var renderedAppModule =
            Rendering.ToString(
                appModuleCanonicalized,
                Rendering.ConfigNormalizeAllLocations(lineBreaking: Rendering.LineBreakingConfig.SnapshotTestsDefault));

        renderedAppModule.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }
}
