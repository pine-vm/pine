using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System;
using ElmTime.ElmInteractive;
using Microsoft.CodeAnalysis;

namespace Pine.Pine.CompilePineToDotNet;

public static class CompileModuleToCSharp
{
    private const string paramEnvName = "env";

    public static CompileCSharpClassResult
        BuildCSharpClassStringFromModule(
        PineValue compiledModule,
        SyntaxContainerConfig containerConfig)
    {
        var parseCache = new PineVMParseCache();

        var parsedModule =
            ElmInteractiveEnvironment.ParseElmModule(compiledModule)
            .Extract(err => throw new Exception(err));

        var functions =
            parsedModule.FunctionDeclarations
            .Select(function => BuildCSharpMethodFromElmFunction(
                ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                    function.Value,
                    parseCache)
                .Extract(err => throw new Exception(err)),
                function.Key,
                parseCache))
            .ToImmutableList();

        var usingDirectivesTypes = new[]
        {
            typeof(PineValue),
            typeof(ImmutableArray),
            typeof(IReadOnlyDictionary<,>),
            typeof(Func<,>),
            typeof(Enumerable),
            typeof(GenericEvalException),
            typeof(ParseExpressionException),
        };

        var usingDirectives =
            usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Order()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
            .ToImmutableList();

        return
             new CompileCSharpClassResult(
                 SyntaxContainerConfig: containerConfig,
                 ClassDeclarationSyntax:
                 SyntaxFactory.ClassDeclaration(containerConfig.ContainerTypeName)
                .WithMembers(
                    SyntaxFactory.List<MemberDeclarationSyntax>([.. functions]))
                .WithModifiers(
                     SyntaxFactory.TokenList(
                         SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                         SyntaxFactory.Token(SyntaxKind.StaticKeyword))),
                 UsingDirectives: usingDirectives);
    }

    public static MethodDeclarationSyntax
        BuildCSharpMethodFromElmFunction(
        ElmInteractiveEnvironment.FunctionRecord functionRecord,
        string functionName,
        PineVMParseCache parseCache)
    {
        var irCompilationResult =
            PineIRCompiler.CompileExpression(
                functionRecord.innerFunction,
                rootExprAlternativeForms: [],
                envClass: null,
                parseCache: parseCache);

        var ssaInstructions =
            SSAInstructionsFromIRCompilationResult(irCompilationResult.Instructions)
            .ToImmutableList();

        IEnumerable<StatementSyntax> DerivationsStatements(
            SSAInstruction instruction)
        {
            var usedAlsoAsInt =
                ssaInstructions
                .Any(
                    otherInstruction =>
                        otherInstruction.Dependencies
                        .Any(
                            dep =>
                                dep.sourceIndex == instruction.AssignmentIndex &&
                                dep.usageType is EmitType.Integer));

            var usedAlsoAsGeneric =
                instruction == ssaInstructions.Last() ||
                ssaInstructions
                .Any(
                    otherInstruction =>
                        otherInstruction.Dependencies
                        .Any(
                            dep =>
                                dep.sourceIndex == instruction.AssignmentIndex &&
                                dep.usageType is null));

            if (instruction.ReturnType is null && usedAlsoAsInt)
            {
                var declName =
                    IdentifierNameFromAssignment(
                        instruction.AssignmentIndex);

                var asIntDeclName =
                    IdentifierNameFromAssignment(
                        instruction.AssignmentIndex,
                        EmitType.Integer);

                var statementAsInt =
                    SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            SyntaxFactory.IdentifierName(
                                SyntaxFactory.Identifier(
                                    SyntaxFactory.TriviaList(),
                                    SyntaxKind.VarKeyword,
                                    "var",
                                    "var",
                                    SyntaxFactory.TriviaList())))
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(asIntDeclName))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        SyntaxFactory.InvocationExpression(
                                            SyntaxFactory.MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                SyntaxFactory.IdentifierName(
                                                    nameof(KernelFunction)),
                                                SyntaxFactory.IdentifierName(
                                                    nameof(KernelFunction.SignedIntegerFromValueRelaxed))))
                                        .WithArgumentList(
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SingletonSeparatedList(
                                                    SyntaxFactory.Argument(
                                                        SyntaxFactory.IdentifierName(declName))))))))));

                yield return statementAsInt;
            }

            if (instruction.ReturnType is EmitType.Integer && usedAlsoAsGeneric)
            {
                var genericDeclName =
                    IdentifierNameFromAssignment(
                        instruction.AssignmentIndex);

                var asIntDeclName =
                    IdentifierNameFromAssignment(
                        instruction.AssignmentIndex,
                        EmitType.Integer);

                var declStatement =
                    SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            SyntaxFactory.IdentifierName("PineValue"))
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList<VariableDeclaratorSyntax>(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(genericDeclName))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        PineCSharpSyntaxFactory.PineValueEmptyListSyntax)))));

                var ifStatement =
                    SyntaxFactory.IfStatement(
                        SyntaxFactory.IsPatternExpression(
                            SyntaxFactory.IdentifierName(asIntDeclName),
                            SyntaxFactory.RecursivePattern()
                            .WithPropertyPatternClause(
                                SyntaxFactory.PropertyPatternClause())
                            .WithDesignation(
                                SyntaxFactory.SingleVariableDesignation(
                                    SyntaxFactory.Identifier(asIntDeclName + "_not_null")))),
                        SyntaxFactory.Block(
                            SyntaxFactory.SingletonList<StatementSyntax>(
                                SyntaxFactory.ExpressionStatement(
                                    SyntaxFactory.AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(genericDeclName),
                                        SyntaxFactory.InvocationExpression(
                                            ValueFromSignedIntegerFunctionRef)
                                        .WithArgumentList(
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                                                    SyntaxFactory.Argument(
                                                        SyntaxFactory.IdentifierName(asIntDeclName + "_not_null"))))))))));

                yield return declStatement;
                yield return ifStatement;
            }
        }

        var rootStatements =
            new List<StatementSyntax>();

        for (var i = 0; i < ssaInstructions.Count; ++i)
        {
            var instruction =
                ssaInstructions[i];

            var instructionDetails =
                StackInstruction.GetDetails(instruction.StackInstruction);

            if (instructionDetails.PushCount is 1)
            {
                var initExprSyntax =
                    ExpressionSyntaxFromStackInstruction(instruction);

                var declName =
                    IdentifierNameFromAssignment(
                        instruction.AssignmentIndex,
                        instruction.ReturnType);

                var declStatement =
                    SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            SyntaxFactory.IdentifierName(
                                SyntaxFactory.Identifier(
                                    SyntaxFactory.TriviaList(),
                                    SyntaxKind.VarKeyword,
                                    "var",
                                    "var",
                                    SyntaxFactory.TriviaList())))
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(declName))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        initExprSyntax)))));

                rootStatements.Add(declStatement);

                rootStatements.AddRange(
                    DerivationsStatements(instruction));
            }
        }

        {
            // Compose return statement

            var lastAssignmentStatement =
                ssaInstructions.Last();

            var lastAssignmentIndex =
                lastAssignmentStatement.AssignmentIndex;

            var lastAssignmentNameAsGeneric =
                IdentifierNameFromAssignment(
                    lastAssignmentIndex,
                    null);

            var returnStatement =
                SyntaxFactory.ReturnStatement(
                    SyntaxFactory.IdentifierName(lastAssignmentNameAsGeneric));

            rootStatements.Add(returnStatement);
        }

        var methodBody =
            SyntaxFactory.Block(rootStatements);

        return
            SyntaxFactory.MethodDeclaration(
                returnType: SyntaxFactory.IdentifierName(nameof(PineValue)),
                identifier: SyntaxFactory.Identifier(functionName))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
            SyntaxFactory.ParameterList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Parameter(
                        SyntaxFactory.Identifier(paramEnvName))
                    .WithType(
                        SyntaxFactory.IdentifierName(nameof(PineValue))))))
            .WithBody(methodBody);
    }

    private static ExpressionSyntax ExpressionSyntaxFromStackInstruction(
        SSAInstruction instruction)
    {
        var stackInstruction = instruction.StackInstruction;

        if (stackInstruction.Kind is StackInstructionKind.Push_Environment)
        {
            return SyntaxFactory.IdentifierName(paramEnvName);
        }

        if (stackInstruction.Kind is StackInstructionKind.Skip_Head_Const)
        {
            var skipCount =
                stackInstruction.SkipCount
                ??
                throw new Exception("Skip count not set for skip head const instruction.");

            var argName =
                IdentifierNameFromAssignment(instruction.Dependencies[0].sourceIndex);

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(KernelFunction)),
                        SyntaxFactory.IdentifierName(nameof(KernelFunction.skip))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.Argument(
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        SyntaxFactory.Literal(skipCount))),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName(argName))})));
        }

        if (stackInstruction.Kind is StackInstructionKind.Head_Generic)
        {
            var argName =
                IdentifierNameFromAssignment(
                    instruction.Dependencies[0].sourceIndex);

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(KernelFunction)),
                        SyntaxFactory.IdentifierName(nameof(KernelFunction.head))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName(argName)))));
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Add_Const)
        {
            var integerLiteral =
                stackInstruction.IntegerLiteral
                ??
                throw new Exception("Integer literal not set for int add const instruction.");

            var argName =
                IdentifierNameFromAssignment(
                    instruction.Dependencies[0]);

            var integerLiteralInt64 =
                (long)integerLiteral;

            return
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.AddExpression,
                    SyntaxFactory.IdentifierName(argName),
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.Literal(integerLiteralInt64)));
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Mul_Const)
        {
            var integerLiteral =
                stackInstruction.IntegerLiteral
                ??
                throw new Exception("Integer literal not set for int add const instruction.");

            var argName =
                IdentifierNameFromAssignment(
                    instruction.Dependencies[0]);

            var integerLiteralInt64 =
                (long)integerLiteral;

            return
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.MultiplyExpression,
                    SyntaxFactory.IdentifierName(argName),
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.Literal(integerLiteralInt64)));
        }

        throw new NotImplementedException(
            "Instruction type not implemented: " + stackInstruction);
    }

    private sealed record SSAInstruction(
        StackInstruction StackInstruction,
        int AssignmentIndex,
        EmitType? ReturnType,
        IReadOnlyList<(int sourceIndex, EmitType? usageType)> Dependencies);

    private enum EmitType
    {
        Integer = 1,
    }

    private static IEnumerable<SSAInstruction> SSAInstructionsFromIRCompilationResult(
        IReadOnlyList<StackInstruction> instructions)
    {
        int pushCount = 0;
        int popCount = 0;

        for (var i = 0; i < instructions.Count; ++i)
        {
            var instruction = instructions[i];

            var instructionDetails =
                StackInstruction.GetDetails(instruction);

            var ssaInstructionDetails =
                DependenciesAndReturnTypeFromStackInstruction(
                    instruction,
                    pushCount - 1);

            if (ssaInstructionDetails.Dependencies.Count != instructionDetails.PopCount)
            {
                throw new Exception(
                    "Dependency count mismatch for instruction: " + instruction);
            }

            var current =
                new SSAInstruction(
                    instruction,
                    AssignmentIndex: pushCount,
                    ReturnType: ssaInstructionDetails.ReturnType,
                    ssaInstructionDetails.Dependencies);

            popCount += instructionDetails.PopCount;
            pushCount += instructionDetails.PushCount;

            yield return current;
        }
    }

    private record DependenciesAndReturnType(
        IReadOnlyList<(int ssaSourceIndex, EmitType? usageType)> Dependencies,
        EmitType? ReturnType);

    private static DependenciesAndReturnType
        DependenciesAndReturnTypeFromStackInstruction(
        StackInstruction stackInstruction,
        int ssaOffset)
    {
        if (stackInstruction.Kind is StackInstructionKind.Push_Environment)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [],
                    ReturnType: null);
        }

        if (stackInstruction.Kind is StackInstructionKind.Skip_Head_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(ssaOffset, null)],
                    ReturnType: null);
        }

        if (stackInstruction.Kind is StackInstructionKind.Head_Generic)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(ssaOffset, null)],
                    ReturnType: null);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Add_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(ssaOffset, EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Add_Binary)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [(ssaOffset - 1, EmitType.Integer), (ssaOffset, EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Mul_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [(ssaOffset, EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Mul_Binary)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [(ssaOffset - 1, EmitType.Integer), (ssaOffset, EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        throw new NotImplementedException(
            "Instruction type not implemented: " + stackInstruction);
    }

    private static string IdentifierNameFromAssignment(
        (int ssaSourceIndex, EmitType? usageType) asTuple) =>
        IdentifierNameFromAssignment(asTuple.ssaSourceIndex, asTuple.usageType);

    private static string IdentifierNameFromAssignment(
        int assignmentIndex,
        EmitType? emitType)
    {
        var genericName =
            IdentifierNameFromAssignment(assignmentIndex);

        return
            emitType switch
            {
                null =>
                genericName,

                EmitType.Integer =>
                genericName + "_as_int",

                _ =>
                throw new NotImplementedException(
                    "Not implemented for type: " + emitType),
            };
    }

    private static string IdentifierNameFromAssignment(
        int assignmentIndex)
    {
        return "stack_" + assignmentIndex;
    }


    private static readonly MemberAccessExpressionSyntax ValueFromSignedIntegerFunctionRef =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName(nameof(PineValueAsInteger)),
            SyntaxFactory.IdentifierName(nameof(PineValueAsInteger.ValueFromSignedInteger)));
}
