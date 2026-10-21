using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Linq;

using Reference = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel.Reference;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class ExpressionCompiler
{
    private static Result<CompilationError, Expression> CompileGlobalReference(
        DeclQualifiedName name, ExpressionCompilationContext context)
    {
        if (name.Namespaces is ["Basics"])
        {
            if (name.DeclName is "True")
                return Expression.LitralInst(EmitBooleanLiteral(true));

            if (name.DeclName is "False")
                return Expression.LitralInst(EmitBooleanLiteral(false));

            if (name.DeclName is "LT" or "EQ" or "GT")
                return Expression.LitralInst(ElmValueEncoding.TagAsPineValue(name.DeclName, []));
        }

        if (context.ModuleCompilationContext.TryGetRecordConstructorFieldNames(name) is { } fields)
            return EmitRecordConstructorFunctionValue(fields);

        if (context.ModuleCompilationContext.TryGetChoiceTypeConstructorArgumentCount(name) is { } arity)
        {
            return
                arity is 0
                ?
                Expression.LitralInst(ElmValueEncoding.TagAsPineValue(name.DeclName, []))
                :
                EmitChoiceTypeTagFunctionValue(name.DeclName, arity);
        }

        if (name.Namespaces is ["Basics"] && CoreLibraryModule.CoreBasics.GetFunctionValue(name.DeclName) is { } basics)
            return Expression.LitralInst(basics);

        if (name.Namespaces is ["Debug"] && name.DeclName is "toString")
            return Expression.LitralInst(CoreLibraryModule.CoreDebug.ToString_FunctionValue());

        if (name.Namespaces is ["List"] && name.DeclName is "cons")
            return Expression.LitralInst(s_consFunction.Value);

        return new CompilationError.FunctionNotInDependencyLayout(name.FullName);
    }

    private static Result<CompilationError, Expression> CompileApplication(
        IR.Expression.Application application,
        ExpressionCompilationContext context)
    {
        if (application.Arguments.Count is 0)
            return new CompilationError.ApplicationTooFewArguments(1);

        if (application.Function is IR.Expression.Identifier identifier)
        {
            return identifier.Reference switch
            {
                Reference.Function function => CompileResolvedApplication(function.Id, application.Arguments, context),

                Reference.Local =>
                CompileResolvedReference(identifier.Reference, context)
                .AndThen(function => CompileArgumentsForResolvedApplication(function, application.Arguments, context)),

                Reference.Native native => CompileGlobalApplication(native.Name, application.Arguments, context),

                Reference.Constructor constructor =>
                CompileGlobalApplication(constructor.Name, application.Arguments, context),

                _ =>
                throw new NotImplementedException(
                    $"{nameof(CompileApplication)} does not handle reference variant: {identifier.Reference.GetType().Name}")
            };
        }

        if (application.Function is IR.Expression.PrefixOperator prefix && application.Arguments.Count is 2 &&
            prefix.Operator is "&&" or "||")
        {
            return
                Compile(application.Arguments[0], context).AndThen(
                    left =>
                    Compile(application.Arguments[1], context).Map(
                        right => prefix.Operator is "&&"
                        ?
                        CoreLibraryModule.CoreBasics.Generic_And(left, right)
                        :
                        CoreLibraryModule.CoreBasics.Generic_Or(left, right)));
        }

        return
            Compile(application.Function, context)
            .AndThen(function => CompileArgumentsForResolvedApplication(function, application.Arguments, context));
    }

    private static Result<CompilationError, Expression> CompileGlobalApplication(
        DeclQualifiedName name, IReadOnlyList<IR.Expression> arguments, ExpressionCompilationContext context)
    {
        if (name.Namespaces.Count is 1 && context.ModuleCompilationContext.IsPineKernelModule(name.Namespaces[0]))
        {
            return
                Compile(arguments[0], context).Map(
                    argument => (Expression)Expression.BuiltinInst(name.DeclName, argument));
        }

        var compiled = new List<Expression>();

        foreach (var argument in arguments)
        {
            var result = Compile(argument, context);

            if (result.IsErrOrNull() is { } error)
                return error;

            compiled.Add(result.Extract(error => throw new InvalidOperationException(error.ToString())));
        }

        if (name.Namespaces is ["Debug"] && name.DeclName is "toString" && compiled.Count is 1)
            return CoreLibraryModule.CoreDebug.ToString(compiled[0]);

        if (context.ModuleCompilationContext.TryGetRecordConstructorFieldNames(name) is { } fields)
        {
            return
                compiled.Count < fields.Count
                ?
                EmitRecordConstructorPartialApplicationExpression(fields, compiled)
                :
                BuildRecordValueExpression(fields, compiled);
        }

        if (context.ModuleCompilationContext.TryGetChoiceTypeConstructorArgumentCount(name) is { } arity)
        {
            return
                compiled.Count < arity
                ?
                EmitChoiceTypeTagPartialApplicationExpression(name.DeclName, arity, compiled)
                :
                Expression.ListInst(
                    [
                        Expression.LitralInst(ElmValue.ElmChoiceTypeTagNameAsValue),
                        Expression.LitralInst(StringEncoding.ValueFromString(name.DeclName)), .. compiled
                    ]);
        }

        if (name.Namespaces is ["Basics"] &&
            CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(name.DeclName) is { } info &&
            info.ParameterCount == compiled.Count)
        {
            return info.CompileApplication(compiled);
        }

        if (name.Namespaces is ["List"] && name.DeclName is "cons" && compiled.Count >= 2)
        {
            var cons = BuiltinHelpers.ApplyBuiltinConcat([Expression.ListInst([compiled[0]]), compiled[1]]);
            return compiled.Count is 2 ? cons : CompileGenericFunctionApplication(cons, compiled.Skip(2).ToArray());
        }

        return CompileGlobalReference(name, context).Map(function => CompileGenericFunctionApplication(function, compiled));
    }
}
