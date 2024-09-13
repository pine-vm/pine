using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using Pine.Core;

namespace Pine.CompilePineToDotNet;

using CompileExpressionFunctionBlockResult =
    Result<string, (Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>;

public class CompilerMutableCache
{
    readonly ConcurrentDictionary<PineValue, Result<string, Expression>> parseExpressionFromValueCache = new();

    readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

    readonly ConcurrentDictionary<CompileExpressionFunctionParameters, CompileExpressionFunctionBlockResult>
        compileExpressionCache = new();

    public record CompileExpressionFunctionParameters(
        Expression Expression,
        PineVM.EnvConstraintId? ConstrainedEnvId,
        IReadOnlyList<PineVM.EnvConstraintId> BranchesConstrainedEnvIds,
        FunctionCompilationEnv CompilationEnv);

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        PineVM.ExpressionUsageAnalysis expressionUsage,
        IReadOnlyList<PineVM.EnvConstraintId> branchesConstrainedEnvIds,
        FunctionCompilationEnv compilationEnv) =>
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

    public Result<string, Expression> ParseExpressionFromValue(PineValue pineValue) =>
        parseExpressionFromValueCache.GetOrAdd(
            pineValue,
            valueFactory: ExpressionEncoding.ParseExpressionFromValueDefault);

    public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        valueHashCache
        .GetOrAdd(
            pineValue,
            valueFactory: v => PineValueHashTree.ComputeHash(v, other => ComputeHash(other)));
}
