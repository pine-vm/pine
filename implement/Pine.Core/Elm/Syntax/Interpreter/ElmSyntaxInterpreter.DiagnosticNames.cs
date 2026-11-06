using Pine.Core.CodeAnalysis;
using System.Runtime.CompilerServices;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    private static readonly ConditionalWeakTable<DeclQualifiedName, PreparedDiagnosticNames> s_diagnosticNames = [];

    // Only source identities are retained. Captured bindings and other runtime values never
    // enter this cache, and the same prepared node can still be used under different owners.
    private sealed class PreparedDiagnosticNames
    {
        private readonly DeclQualifiedName _owner;

        private readonly ConditionalWeakTable<PreparedFunctionImplementation, DeclQualifiedName> _letNames = [];

        private readonly ConditionalWeakTable<PreparedFunctionImplementation, DeclQualifiedName>.CreateValueCallback _createLetName;

        public DeclQualifiedName LambdaName { get; }

        public PreparedDiagnosticNames(DeclQualifiedName owner)
        {
            _owner = owner;
            LambdaName = owner.ContainedDeclName("<lambda>");
            _createLetName = CreateLetName;
        }

        public DeclQualifiedName LetName(PreparedFunctionImplementation function) =>
            _letNames.GetValue(function, _createLetName);

        private DeclQualifiedName CreateLetName(PreparedFunctionImplementation function) =>
            DeclQualifiedName.Create(
                [.. _owner.Namespaces, _owner.DeclName, "<let>"],
                function.Name);
    }

    private static PreparedDiagnosticNames DiagnosticNames(DeclQualifiedName owner) =>
        s_diagnosticNames.GetValue(owner, static name => new PreparedDiagnosticNames(name));
}
