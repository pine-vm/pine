using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.PopularEncodings;

namespace Pine.CompilePineToDotNet;

using CompileExpressionFunctionBlockResult =
    Result<string, (Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>;

public class CompilerMutableCache
{
    readonly ConcurrentDictionary<PineValue, Result<string, Expression>> _parseExpressionFromValueCache = new();

    readonly ConcurrentPineValueHashCache _valueHashCache = new();

    readonly ConcurrentDictionary<CompileExpressionFunctionParameters, CompileExpressionFunctionBlockResult>
        compileExpressionCache = new();

    public record CompileExpressionFunctionParameters(
        Expression Expression,
        PineValueClass? ConstrainedEnvId,
        IReadOnlyList<PineValueClass> BranchesConstrainedEnvIds,
        FunctionCompilationEnv CompilationEnv);

    public CompileExpressionFunctionBlockResult
        CompileToCSharpFunctionBlockSyntax(
        PineVM.ExpressionUsageAnalysis expressionUsage,
        IReadOnlyList<PineValueClass> branchesConstrainedEnvIds,
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
        _parseExpressionFromValueCache.GetOrAdd(
            pineValue,
            valueFactory: ExpressionEncoding.ParseExpressionFromValue);

    public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        _valueHashCache
        .GetHash(pineValue);
}
