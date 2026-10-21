using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Checks structural lowering invariants without interpreting generated names.</summary>
public static class ProgramValidation
{
    /// <summary>Rejects residual functions, unbound captures, and dangling function references.</summary>
    public static void Validate(Program program)
    {
        foreach (var (id, function) in program.Functions)
        {
            var declared = new HashSet<BindingId>();

            void Declare(Pattern pattern)
            {
                foreach (var binding in ExpressionTransform.PatternBindings(pattern))
                {
                    if (!declared.Add(binding))
                    {
                        throw new InvalidOperationException(
                            $"Function originating at {id.SourceDeclaration} declares a lexical identity more than once.");
                    }
                }
            }

            foreach (var parameter in function.Parameters)
                Declare(parameter);

            var free = ExpressionTransform.FreeBindings(function.Body);
            free.ExceptWith(function.Parameters.SelectMany(ExpressionTransform.PatternBindings));

            if (free.Count is not 0)
            {
                throw new InvalidOperationException(
                    $"Function originating at {id.SourceDeclaration} has unbound lexical references.");
            }

            var pending = new Stack<Expression>([function.Body]);

            while (pending.TryPop(out var expression))
            {
                if (expression is Expression.Lambda)
                {
                    throw new InvalidOperationException(
                        $"Function originating at {id.SourceDeclaration} contains an unlifted function.");
                }

                if (expression is Expression.Let let)
                {
                    foreach (var binding in let.Bindings)
                        Declare(binding.Pattern);
                }

                if (expression is Expression.Match match)
                {
                    foreach (var branch in match.Branches)
                        Declare(branch.Pattern);
                }

                if (expression is Expression.ReferenceExpression { Reference: Reference.Function reference } &&
                    !program.Functions.ContainsKey(reference.Id))
                {
                    throw new InvalidOperationException(
                        $"Function originating at {id.SourceDeclaration} references a missing function.");
                }

                foreach (var child in ExpressionTransform.Children(expression))
                    pending.Push(child);
            }
        }
    }
}
