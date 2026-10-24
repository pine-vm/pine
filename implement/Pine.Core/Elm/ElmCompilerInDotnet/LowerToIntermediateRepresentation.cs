using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System.Collections.Generic;
using System.Collections.Immutable;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Source type evidence for a generic record read.</summary>
public sealed record GenericRecordAccessEvidence(TypeInference.InferredType SubjectType, RecordOperationProvenance Provenance);

/// <summary>Convenience boundary from canonical source through resolved optimization expressions to emission IR.</summary>
public static class LowerToIntermediateRepresentation
{
    /// <summary>Lowers source declarations. No unresolved or string-bound IR is ever returned.</summary>
    public static ImmutableDictionary<Model.FunctionId, IR.FunctionStruct> Lower(
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations) =>
        Model.ToIntermediateRepresentation.Convert(
            Model.LambdaLifting.Lift(Model.FromCanonicalSyntax.Convert(declarations)));
}
