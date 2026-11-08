using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    // Payloads live directly in Stack<Kont>. Typed constructors/views keep the tag and reference
    // slots consistent; Stack<T>.Pop clears all reference slots, including on tail-call replacement.
    private readonly struct Kont(
        Kont.Kind tag,
        object? first = null,
        object? second = null,
        object? third = null,
        object? fourth = null,
        object? fifth = null,
        int index = 0)
    {
        public Kind Tag { get; } = tag;

        public enum Kind
        {
            None,
            ObserveResult,
            MemoizeResult,
            CacheApplicationResult,
            Negate,
            BuildList,
            IfBranch,
            BuildRecord,
            BuildArgs,
            BuildArgsForValue,
            ApplyEvaluatedFunction,
            AfterCall,
            LetBindFunction,
            LetBindDestructure,
            CallFrame,
            MatchCase,
            AccessRecordField,
            BuildRecordUpdate
        }

        public readonly record struct ObserveResult(Action<PineValueInProcess> Observer);

        public readonly record struct MemoizeResult(NullaryValueCache.Entry Entry);

        public readonly record struct CacheApplicationResult(List<ApplicationCacheExecution.Entry> Entries);

        public readonly record struct Negate;

        public readonly record struct BuildList(
            IReadOnlyList<PreparedExpression> Elements, int NextIndex,
            PineValueInProcess[] Accumulated, ApplicationContext Env);

        public readonly record struct IfBranch(
            PreparedExpression ThenBranch, PreparedExpression ElseBranch, ApplicationContext Env);

        public readonly record struct BuildRecord(
            IReadOnlyList<PreparedRecordSetter> Fields, int NextIndex,
            (string FieldName, PineValue FieldNameValue, PineValueInProcess Value)[] Accumulated, ApplicationContext Env);

        public readonly record struct BuildArgs(
            PreparedExpression.Identifier FunctionOrValue, IReadOnlyList<PreparedExpression> Arguments,
            int NextIndex, PineValueInProcess[] Accumulated, ApplicationContext Env);

        public readonly record struct BuildArgsForValue(
            PreparedExpression FunctionExpr, IReadOnlyList<PreparedExpression> Arguments,
            int NextIndex, PineValueInProcess[] Accumulated, ApplicationContext Env);

        public readonly record struct ApplyEvaluatedFunction(IReadOnlyList<PineValueInProcess> Arguments, ApplicationContext Env);

        public readonly record struct AfterCall(IReadOnlyList<PineValueInProcess> ExtraArgs, ApplicationContext Env);

        public readonly record struct LetBindFunction(
            string BindingName, IReadOnlyList<PreparedLetDeclaration> Remaining, int NextIndex,
            Dictionary<string, PineValueInProcess> Extended, PreparedExpression Body, ApplicationContext Outer);

        public readonly record struct LetBindDestructure(
            Pattern Pattern, IReadOnlyList<PreparedLetDeclaration> Remaining, int NextIndex,
            Dictionary<string, PineValueInProcess> Extended, PreparedExpression Body, ApplicationContext Outer);

        public readonly record struct CallFrame(DeclQualifiedName FunctionName, IReadOnlyList<PineValueInProcess> Arguments);

        public readonly record struct MatchCase(
            PreparedCaseDispatch Dispatch, ApplicationContext Env, PreparedExpression? ScrutineeExpr = null);

        public readonly record struct AccessRecordField(string FieldName, PineValue FieldNameValue);

        public readonly record struct BuildRecordUpdate(
            PineValueInProcess? OriginalRecord, IReadOnlyList<PreparedRecordSetter> Fields, int NextIndex,
            (string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)[] Accumulated, ApplicationContext Env);

        public ObserveResult AsObserveResult => new((Action<PineValueInProcess>)first!);

        public MemoizeResult AsMemoizeResult => new((NullaryValueCache.Entry)first!);

        public CacheApplicationResult AsCacheApplicationResult => new((List<ApplicationCacheExecution.Entry>)first!);

        public BuildList AsBuildList =>
            new(
                (IReadOnlyList<PreparedExpression>)first!,
                index,
                (PineValueInProcess[])second!,
                (ApplicationContext)third!);

        public IfBranch AsIfBranch =>
            new(
                (PreparedExpression)first!,
                (PreparedExpression)second!,
                (ApplicationContext)third!);

        public BuildRecord AsBuildRecord =>
            new(
                (IReadOnlyList<PreparedRecordSetter>)first!,
                index,
                ((string, PineValue, PineValueInProcess)[])second!,
                (ApplicationContext)third!);

        public BuildArgs AsBuildArgs =>
            new(
                (PreparedExpression.Identifier)first!,
                (IReadOnlyList<PreparedExpression>)second!,
                index,
                (PineValueInProcess[])third!,
                (ApplicationContext)fourth!);

        public BuildArgsForValue AsBuildArgsForValue =>
            new(
                (PreparedExpression)first!,
                (IReadOnlyList<PreparedExpression>)second!,
                index,
                (PineValueInProcess[])third!,
                (ApplicationContext)fourth!);

        public ApplyEvaluatedFunction AsApplyEvaluatedFunction =>
            new((IReadOnlyList<PineValueInProcess>)first!, (ApplicationContext)second!);

        public AfterCall AsAfterCall => new((IReadOnlyList<PineValueInProcess>)first!, (ApplicationContext)second!);

        public LetBindFunction AsLetBindFunction =>
            new(
                (string)first!,
                (IReadOnlyList<PreparedLetDeclaration>)second!,
                index,
                (Dictionary<string, PineValueInProcess>)third!,
                (PreparedExpression)fourth!,
                (ApplicationContext)fifth!);

        public LetBindDestructure AsLetBindDestructure =>
            new(
                (Pattern)first!,
                (IReadOnlyList<PreparedLetDeclaration>)second!,
                index,
                (Dictionary<string, PineValueInProcess>)third!,
                (PreparedExpression)fourth!,
                (ApplicationContext)fifth!);

        public CallFrame AsCallFrame => new((DeclQualifiedName)first!, (IReadOnlyList<PineValueInProcess>)second!);

        public MatchCase AsMatchCase =>
            new(
                (PreparedCaseDispatch)first!,
                (ApplicationContext)second!,
                (PreparedExpression?)third);

        public AccessRecordField AsAccessRecordField => new((string)first!, (PineValue)second!);

        public BuildRecordUpdate AsBuildRecordUpdate =>
            new(
                (PineValueInProcess?)first,
                (IReadOnlyList<PreparedRecordSetter>)second!,
                index,
                ((string, PineValue, PineValueInProcess)[])third!,
                (ApplicationContext)fourth!);

        public static implicit operator Kont(ObserveResult value) => new(Kind.ObserveResult, value.Observer);

        public static implicit operator Kont(MemoizeResult value) => new(Kind.MemoizeResult, value.Entry);

        public static implicit operator Kont(CacheApplicationResult value) => new(Kind.CacheApplicationResult, value.Entries);

        public static implicit operator Kont(Negate value) => new(Kind.Negate);

        public static implicit operator Kont(BuildList value) => new(
            Kind.BuildList, value.Elements, value.Accumulated, value.Env, index: value.NextIndex);

        public static implicit operator Kont(IfBranch value) => new(Kind.IfBranch, value.ThenBranch, value.ElseBranch, value.Env);

        public static implicit operator Kont(BuildRecord value) => new(
            Kind.BuildRecord, value.Fields, value.Accumulated, value.Env, index: value.NextIndex);

        public static implicit operator Kont(BuildArgs value) => new(
            Kind.BuildArgs, value.FunctionOrValue, value.Arguments, value.Accumulated, value.Env, index: value.NextIndex);

        public static implicit operator Kont(BuildArgsForValue value) => new(
            Kind.BuildArgsForValue, value.FunctionExpr, value.Arguments, value.Accumulated, value.Env, index: value.NextIndex);

        public static implicit operator Kont(ApplyEvaluatedFunction value) => new(Kind.ApplyEvaluatedFunction, value.Arguments, value.Env);

        public static implicit operator Kont(AfterCall value) => new(Kind.AfterCall, value.ExtraArgs, value.Env);

        public static implicit operator Kont(LetBindFunction value) => new(
            Kind.LetBindFunction, value.BindingName, value.Remaining, value.Extended, value.Body, value.Outer, value.NextIndex);

        public static implicit operator Kont(LetBindDestructure value) => new(
            Kind.LetBindDestructure, value.Pattern, value.Remaining, value.Extended, value.Body, value.Outer, value.NextIndex);

        public static implicit operator Kont(CallFrame value) => new(Kind.CallFrame, value.FunctionName, value.Arguments);

        public static implicit operator Kont(MatchCase value) => new(Kind.MatchCase, value.Dispatch, value.Env, value.ScrutineeExpr);

        public static implicit operator Kont(AccessRecordField value) => new(Kind.AccessRecordField, value.FieldName, value.FieldNameValue);

        public static implicit operator Kont(BuildRecordUpdate value) => new(
            Kind.BuildRecordUpdate, value.OriginalRecord, value.Fields, value.Accumulated, value.Env, index: value.NextIndex);
    }
}
