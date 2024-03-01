using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.CompilePineToDotNet;

using CompileExpressionFunctionBlockResult =
    Result<string, (Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>;

public class CompilerMutableCache
{
    readonly ConcurrentDictionary<PineValue, Result<string, PineVM.Expression>> parseExpressionFromValueCache = new();

    readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

    readonly ConcurrentDictionary<CompileExpressionFunctionParameters, CompileExpressionFunctionBlockResult>
        compileExpressionCache = new();

    public record CompileExpressionFunctionParameters(
        PineVM.Expression Expression,
        PineVM.EnvConstraintId? ConstrainedEnvId,
        IReadOnlyList<PineVM.EnvConstraintId> BranchesConstrainedEnvIds,
        FunctionCompilationEnvironment CompilationEnv);

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        PineVM.ExpressionUsage expressionUsage,
        IReadOnlyList<PineVM.EnvConstraintId> branchesConstrainedEnvIds,
        FunctionCompilationEnvironment compilationEnv) =>
        CompileToCSharpFunctionBlockSyntax(
            new CompileExpressionFunctionParameters(
                expressionUsage.Expression,
                ConstrainedEnvId: expressionUsage.EnvId,
                BranchesConstrainedEnvIds: branchesConstrainedEnvIds,
                CompilationEnv: compilationEnv));

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        CompileExpressionFunctionParameters parameters) =>
        compileExpressionCache.GetOrAdd(
            parameters,
            valueFactory: _ => CompileToCSharp.CompileToCSharpFunctionBlockSyntax(
                parameters.Expression,
                constrainedEnvId: parameters.ConstrainedEnvId,
                branchesEnvIds: parameters.BranchesConstrainedEnvIds,
                compilationEnv: parameters.CompilationEnv));

    public Result<string, PineVM.Expression> ParseExpressionFromValue(PineValue pineValue) =>
        parseExpressionFromValueCache.GetOrAdd(
            pineValue,
            valueFactory: PineVM.PineVM.ParseExpressionFromValueDefault);

    public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        valueHashCache
        .GetOrAdd(
            pineValue,
            valueFactory: v => PineValueHashTree.ComputeHash(v, other => ComputeHash(other)));
}
