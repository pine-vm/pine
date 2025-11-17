using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.DotNet;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System;
using Microsoft.CodeAnalysis;
using System.Text;

namespace Pine.Pine.CompilePineToDotNet;

using CoreSyntaxFactory =
    Core.DotNet.PineCSharpSyntaxFactory;

public static class CompileModuleToCSharp
{
    private const string paramEnvName = "env";

    private record ProcedureInterface(
        IReadOnlyList<ReadOnlyMemory<int>> ParamsPaths);

    public static CompileCSharpClassResult
        BuildCSharpClassStringFromModule(
        PineValue compiledModule,
        SyntaxContainerConfig containerConfig,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var parseCache = new PineVMParseCache();

        var parsedModule =
            ElmInteractiveEnvironment.ParseElmModule(compiledModule)
            .Extract(err => throw new Exception(err));

        var functions =
            parsedModule.FunctionDeclarations
            .SelectMany(function => BuildCSharpMethodsFromElmFunction(
                FunctionRecord.ParseFunctionRecordTagged(
                    function.Value,
                    parseCache)
                .Extract(err => throw new Exception(err)),
                function.Key,
                parseCache,
                declarationSyntaxContext))
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

    private const string UnpackedParamsMethodNameSuffix = "_uparam";

    public static IEnumerable<MethodDeclarationSyntax>
        BuildCSharpMethodsFromElmFunction(
        FunctionRecord functionRecord,
        string functionName,
        PineVMParseCache parseCache,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var procedureInterface = UnpackedParams(functionRecord);

        var genericMethod =
            BuildCSharpMethodFromElmFunctionGeneric(
                functionName,
                procedureInterface);

        var unpackedParamsMethod =
            BuildCSharpMethodFromElmFunctionUnpackedParams(
                functionRecord,
                functionName,
                procedureInterface,
                parseCache,
                declarationSyntaxContext);

        return [genericMethod, unpackedParamsMethod];
    }

    private static MethodDeclarationSyntax
        BuildCSharpMethodFromElmFunctionGeneric(
        string functionName,
        ProcedureInterface procedureInterface)
    {
        var pathsIncludingIntermediate =
            new HashSet<ReadOnlyMemory<int>>(procedureInterface.ParamsPaths);

        foreach (var path in procedureInterface.ParamsPaths)
        {
            for (var i = 1; i < path.Length; ++i)
            {
                var subPath = path[..i];

                pathsIncludingIntermediate.Add(subPath);
            }
        }

        var pathsWithLocalIds =
            pathsIncludingIntermediate
            .Order(IntPathMemoryComparer.Instance)
            .Select((path, index) => (path, index))
            .ToImmutableList();

        static ExpressionSyntax exprSyntaxForPath(ReadOnlyMemory<int> path)
        {
            var localName = DeclNameFromEnvPath(path);

            return SyntaxFactory.IdentifierName(localName);
        }

        var rootStatements =
            new List<StatementSyntax>();

        for (var i = 0; i < pathsWithLocalIds.Count; ++i)
        {
            var localPath =
                pathsWithLocalIds[i].path;

            if (localPath.Length is 0)
                continue;

            var skipCount =
                localPath.Span[localPath.Length - 1];

            var parentPath =
                localPath[..^1];

            var parentExpr = exprSyntaxForPath(parentPath);

            var skippedExpr =
                skipCount is 0
                ?
                parentExpr
                :
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
                                    parentExpr)})));

            var localExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(KernelFunction)),
                        SyntaxFactory.IdentifierName(nameof(KernelFunction.head))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.Argument(skippedExpr)
                            })));

            var declName = DeclNameFromEnvPath(localPath);

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
                                    localExpr)))));

            rootStatements.Add(declStatement);
        }

        {
            // Compose return statement

            var arguments = new ArgumentSyntax[procedureInterface.ParamsPaths.Count];

            for (var paramIndex = 0; paramIndex < arguments.Length; paramIndex++)
            {
                var paramPath =
                    procedureInterface.ParamsPaths[paramIndex];

                arguments[paramIndex] =
                    SyntaxFactory.Argument(
                        exprSyntaxForPath(paramPath));
            }

            var returnStatement =
                SyntaxFactory.ReturnStatement(
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.IdentifierName(functionName + UnpackedParamsMethodNameSuffix))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            arguments: SyntaxFactory.SeparatedList(arguments))));

            rootStatements.Add(returnStatement);
        }

        var methodBody = SyntaxFactory.Block(rootStatements);

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

    private static MethodDeclarationSyntax
        BuildCSharpMethodFromElmFunctionUnpackedParams(
        FunctionRecord functionRecord,
        string functionName,
        ProcedureInterface procedureInterface,
        PineVMParseCache parseCache,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var irCompilationResult =
            PineIRCompiler.CompileExpression(
                functionRecord.InnerFunction,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.Generic,
                parseCache: parseCache);

        var ssaInstructionsLessFilter =
            SSAInstructionsFromIRCompilationResult(irCompilationResult.Instructions)
            .ToImmutableList();

        var ssaInstructions =
            MapSSAInstructionsForParams(
                ssaInstructionsLessFilter,
                procedureInterface)
            .ToImmutableList();

        IEnumerable<StatementSyntax> DerivationsStatements(
            SSAInstruction instruction)
        {
            var usages =
                ssaInstructions
                .SelectMany(otherInstruction =>
                otherInstruction.Dependencies.Where(dep =>
                dep.source is SSAInstructionSource.Local localDep &&
                localDep.Index == instruction.AssignmentIndex));

            var usagesTypes =
                usages
                .Select(dep => dep.type)
                .ToHashSet();

            var usedAlsoAsInt =
                usagesTypes.Contains(EmitType.Integer);

            var usedAlsoAsGeneric =
                usagesTypes.Contains(null) ||
                instruction == ssaInstructions.Last();

            if (instruction.ReturnType is null && usedAlsoAsInt)
            {
                foreach (var declStatement in
                    DeclareFromGenericToInt(
                        IdentifierNameFromLocalAssignment(
                            instruction.AssignmentIndex,
                            null)))
                {
                    yield return declStatement;
                }
            }

            if (instruction.ReturnType is EmitType.Integer && usedAlsoAsGeneric)
            {
                foreach (var declStatement in
                    DeclareFromIntToGeneric(
                        IdentifierNameFromLocalAssignment(
                            instruction.AssignmentIndex,
                            null),
                        declarationSyntaxContext))
                {
                    yield return declStatement;
                }
            }
        }

        var rootStatements =
            new List<StatementSyntax>();

        for (var i = 0; i < procedureInterface.ParamsPaths.Count; ++i)
        {
            var paramPath =
                procedureInterface.ParamsPaths[i];

            var alsoUsedAsInt =
                ssaInstructions
                .Any(instruction =>
                    instruction.Dependencies.Any(dep =>
                        dep.source is SSAInstructionSource.Parameter paramDep &&
                        paramDep.EnvPath.Span.SequenceEqual(paramPath.Span)));

            if (alsoUsedAsInt)
            {
                rootStatements.AddRange(
                    DeclareFromGenericToInt(DeclNameFromEnvPath(paramPath)));
            }
        }

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
                        new SSAInstructionSource.Local(instruction.AssignmentIndex),
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
                    new SSAInstructionSource.Local(lastAssignmentIndex),
                    null);

            var returnStatement =
                SyntaxFactory.ReturnStatement(
                    SyntaxFactory.IdentifierName(lastAssignmentNameAsGeneric));

            rootStatements.Add(returnStatement);
        }

        var methodBody =
            SyntaxFactory.Block(rootStatements);

        var parameterList =
            procedureInterface.ParamsPaths
            .Select(path =>
                SyntaxFactory.Parameter(
                    SyntaxFactory.Identifier(DeclNameFromEnvPath(path)))
                .WithType(
                    SyntaxFactory.IdentifierName(nameof(PineValue))));

        return
            SyntaxFactory.MethodDeclaration(
                returnType: SyntaxFactory.IdentifierName(nameof(PineValue)),
                identifier: SyntaxFactory.Identifier(functionName + UnpackedParamsMethodNameSuffix))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
            SyntaxFactory.ParameterList(
                SyntaxFactory.SeparatedList(parameterList)))
            .WithBody(methodBody);
    }

    private static IEnumerable<StatementSyntax> DeclareFromGenericToInt(string declNameGeneric)
    {
        var declNameAsInt =
            declNameGeneric + SuffixFromEmitType(EmitType.Integer);

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
                            SyntaxFactory.Identifier(declNameAsInt))
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
                                                SyntaxFactory.IdentifierName(declNameGeneric))))))))));

        yield return statementAsInt;
    }

    private static IEnumerable<StatementSyntax> DeclareFromIntToGeneric(
        string declNameGeneric,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var declNameAsInt =
            declNameGeneric + SuffixFromEmitType(EmitType.Integer);

        var declStatement =
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(declNameGeneric))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                CoreSyntaxFactory.PineValueEmptyListSyntax(declarationSyntaxContext))))));

        var ifStatement =
            SyntaxFactory.IfStatement(
                SyntaxFactory.IsPatternExpression(
                    SyntaxFactory.IdentifierName(declNameAsInt),
                    SyntaxFactory.RecursivePattern()
                    .WithPropertyPatternClause(
                        SyntaxFactory.PropertyPatternClause())
                    .WithDesignation(
                        SyntaxFactory.SingleVariableDesignation(
                            SyntaxFactory.Identifier(declNameAsInt + "_not_null")))),
                SyntaxFactory.Block(
                    SyntaxFactory.SingletonList<StatementSyntax>(
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.IdentifierName(declNameGeneric),
                                SyntaxFactory.InvocationExpression(
                                    ValueFromSignedIntegerFunctionRef)
                                .WithArgumentList(
                                    SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.IdentifierName(declNameAsInt + "_not_null"))))))))));

        yield return declStatement;
        yield return ifStatement;
    }

    private static string DeclNameFromEnvPath(ReadOnlyMemory<int> path)
    {
        var sb = new StringBuilder(paramEnvName);

        for (var i = 0; i < path.Length; ++i)
        {
            sb.Append("_" + path.Span[i]);
        }

        return sb.ToString();
    }

    private static ProcedureInterface UnpackedParams(
        FunctionRecord functionRecord)
    {
        IReadOnlyList<ReadOnlyMemory<int>> paramPaths =
            [.. StaticFunctionInterface.UnpackedParamsFiltered(functionRecord.InnerFunction)];

        return new ProcedureInterface(paramPaths);
    }

    private static ExpressionSyntax ExpressionSyntaxFromStackInstruction(
        SSAInstruction instruction)
    {
        var stackInstruction = instruction.StackInstruction;

        if (stackInstruction.Kind is StackInstructionKind.Skip_Head_Const)
        {
            var skipCount =
                stackInstruction.SkipCount
                ??
                throw new Exception("Skip count not set for skip head const instruction.");

            var argName =
                IdentifierNameFromAssignment(instruction.Dependencies[0]);

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
                    instruction.Dependencies[0]);

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
        IReadOnlyList<(SSAInstructionSource source, EmitType? type)> Dependencies);

    private abstract record SSAInstructionSource
    {
        public sealed record Local(
            int Index)
            : SSAInstructionSource;

        public sealed record Parameter(
            ReadOnlyMemory<int> EnvPath)
            : SSAInstructionSource;
    }

    private enum EmitType
    {
        Integer = 1,
    }

    private static IEnumerable<SSAInstruction> MapSSAInstructionsForParams(
        IReadOnlyList<SSAInstruction> instructions,
        ProcedureInterface procedureInterface)
    {
        var replacements =
            ParamsPathsFromInstructionId(instructions, procedureInterface);

        var instructionsMapped = new List<SSAInstruction>();

        bool isUsed(SSAInstruction instruction)
        {
            for (var i = 0; i < instructionsMapped.Count; ++i)
            {
                var otherInstruction = instructionsMapped[i];

                if (otherInstruction is null)
                    continue;

                if (otherInstruction.Dependencies.Any(dep =>
                    dep.source is SSAInstructionSource.Local loc &&
                    loc.Index == instruction.AssignmentIndex))
                {
                    return true;
                }
            }

            return false;
        }

        for (var i = instructions.Count - 1; i >= 0; --i)
        {
            if (i < instructions.Count - 1)
            {
                if (!isUsed(instructions[i]))
                    continue;
            }

            var instruction = instructions[i];

            var mappedDependencies =
                instruction.Dependencies
                .Select(dep =>
                {
                    if (dep.source is SSAInstructionSource.Local loc &&
                    replacements.TryGetValue(loc.Index, out var path))
                    {
                        return (new SSAInstructionSource.Parameter(path), dep.type);
                    }

                    return dep;
                })
                .ToList();

            instructionsMapped.Insert(
                0,
                new SSAInstruction(
                    instruction.StackInstruction,
                    instruction.AssignmentIndex,
                    instruction.ReturnType,
                    mappedDependencies));
        }

        return instructionsMapped;
    }

    private static ImmutableDictionary<int, ReadOnlyMemory<int>> ParamsPathsFromInstructionId(
        IReadOnlyList<SSAInstruction> instructions,
        ProcedureInterface procedureInterface)
    {
        var paramPathByAssignment = new Dictionary<int, ReadOnlyMemory<int>>();

        for (var i = 0; i < instructions.Count; ++i)
        {
            var inst = instructions[i];

            if (inst.StackInstruction.Kind is StackInstructionKind.Skip_Head_Const)
            {
                var parent = paramPathByAssignment[inst.AssignmentIndex - 1];

                var k = inst.StackInstruction.SkipCount.Value;

                var newPath = parent.ToArray().Concat([k]).ToArray().AsMemory();

                paramPathByAssignment[inst.AssignmentIndex] = newPath;
            }

            if (inst.StackInstruction.Kind is StackInstructionKind.Head_Generic)
            {
                var parent = paramPathByAssignment[inst.AssignmentIndex - 1];

                var newPath = parent.ToArray().Concat([0]).ToArray().AsMemory();

                paramPathByAssignment[inst.AssignmentIndex] = newPath;
            }
        }

        return
            paramPathByAssignment
            .Where(kvp => procedureInterface.ParamsPaths.Any(path => path.Span.SequenceEqual(kvp.Value.Span)))
            .ToImmutableDictionary();
    }

    private static IEnumerable<SSAInstruction> SSAInstructionsFromIRCompilationResult(
        IReadOnlyList<StackInstruction> instructions)
    {
        var pushCount = 0;
        var popCount = 0;

        for (var i = 0; i < instructions.Count; ++i)
        {
            var inst = instructions[i];

            var assignmentIndex = pushCount;

            var instructionDetails =
                StackInstruction.GetDetails(inst);

            var ssaInstructionDetails =
                DependenciesAndReturnTypeFromStackInstruction(
                    inst,
                    pushCount - 1);

            if (ssaInstructionDetails.Dependencies.Count != instructionDetails.PopCount)
            {
                throw new Exception(
                    "Dependency count mismatch for instruction: " + inst);
            }

            var current =
                new SSAInstruction(
                    inst,
                    AssignmentIndex: assignmentIndex,
                    ReturnType: ssaInstructionDetails.ReturnType,
                    Dependencies: ssaInstructionDetails.Dependencies);

            popCount += instructionDetails.PopCount;
            pushCount += instructionDetails.PushCount;

            yield return current;
        }
    }

    private record DependenciesAndReturnType(
        IReadOnlyList<(SSAInstructionSource source, EmitType? type)> Dependencies,
        EmitType? ReturnType);

    private static DependenciesAndReturnType
        DependenciesAndReturnTypeFromStackInstruction(
        StackInstruction stackInstruction,
        int ssaOffset)
    {
        if (stackInstruction.Kind is StackInstructionKind.Skip_Head_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(new SSAInstructionSource.Local(ssaOffset), null)],
                    ReturnType: null);
        }

        if (stackInstruction.Kind is StackInstructionKind.Head_Generic)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(new SSAInstructionSource.Local(ssaOffset), null)],
                    ReturnType: null);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Add_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies: [(new SSAInstructionSource.Local(ssaOffset), EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Add_Binary)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [
                        (new SSAInstructionSource.Local(ssaOffset - 1), EmitType.Integer),
                        (new SSAInstructionSource.Local(ssaOffset), EmitType.Integer)
                        ],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Mul_Const)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [(new SSAInstructionSource.Local(ssaOffset), EmitType.Integer)],
                    ReturnType: EmitType.Integer);
        }

        if (stackInstruction.Kind is StackInstructionKind.Int_Mul_Binary)
        {
            return
                new DependenciesAndReturnType(
                    Dependencies:
                    [
                        (new SSAInstructionSource.Local(ssaOffset - 1), EmitType.Integer),
                        (new SSAInstructionSource.Local(ssaOffset), EmitType.Integer)
                        ],
                    ReturnType: EmitType.Integer);
        }

        throw new NotImplementedException(
            "Instruction type not implemented: " + stackInstruction);
    }

    private static string IdentifierNameFromAssignment(
        (SSAInstructionSource ssaSource, EmitType? usageType) asTuple) =>
        IdentifierNameFromAssignment(asTuple.ssaSource, asTuple.usageType);


    private static string IdentifierNameFromAssignment(
        SSAInstructionSource assignmentSource,
        EmitType? emitType)
    {
        if (assignmentSource is SSAInstructionSource.Local localSource)
        {
            return
                IdentifierNameFromLocalAssignment(
                    localSource.Index,
                    emitType);
        }

        if (assignmentSource is SSAInstructionSource.Parameter paramSource)
        {
            return
                DeclNameFromEnvPath(paramSource.EnvPath) +
                SuffixFromEmitType(emitType);
        }

        throw new NotImplementedException(
            "Unexpected source type: " + assignmentSource);
    }


    private static string IdentifierNameFromLocalAssignment(
        int assignmentIndex,
        EmitType? emitType)
    {
        return "stack_" + assignmentIndex + SuffixFromEmitType(emitType);
    }

    private static string? SuffixFromEmitType(EmitType? emitType)
    {
        return emitType switch
        {
            null =>
            null,

            EmitType.Integer =>
            "_as_int",

            _ =>
            throw new NotImplementedException("Unexpected emit type: " + emitType),
        };
    }


    private static readonly MemberAccessExpressionSyntax ValueFromSignedIntegerFunctionRef =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName(nameof(IntegerEncoding)),
            SyntaxFactory.IdentifierName(nameof(IntegerEncoding.EncodeSignedInteger)));
}
