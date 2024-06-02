using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public interface IPineVM
{
    Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment);
}

public class PineVM : IPineVM
{
    public delegate Result<string, PineValue> EvalExprDelegate(Expression expression, PineValue environment);

    public delegate EvalExprDelegate OverrideEvalExprDelegate(EvalExprDelegate evalExprDelegate);

    public delegate Result<string, Expression> ParseExprDelegate(PineValue value);

    public delegate ParseExprDelegate OverrideParseExprDelegate(ParseExprDelegate parseExprDelegate);

    public long EvaluateExpressionCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    private readonly ParseExprDelegate parseExpressionDelegate;

    private readonly EvalExprDelegate evalExprDelegate;

    public static PineVM Construct(
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? parseExpressionOverrides = null,
        OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var parseExpressionOverridesDict =
            parseExpressionOverrides
            ?.ToFrozenDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        return new PineVM(
            overrideParseExpression:
            parseExpressionOverridesDict switch
            {
                null =>
                originalHandler => originalHandler,

                not null =>
                _ => value => ParseExpressionFromValue(value, parseExpressionOverridesDict)
            },
            overrideEvaluateExpression);
    }

    public PineVM(
        OverrideParseExprDelegate? overrideParseExpression = null,
        OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        parseExpressionDelegate =
            overrideParseExpression?.Invoke(ParseExpressionFromValueDefault) ?? ParseExpressionFromValueDefault;

        evalExprDelegate =
            overrideEvaluateExpression?.Invoke(EvaluateExpressionDefault) ?? EvaluateExpressionDefault;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) => evalExprDelegate(expression, environment);

    public Result<string, PineValue> EvaluateExpressionDefault(
        Expression expression,
        PineValue environment)
    {
        if (expression is Expression.LiteralExpression literalExpression)
            return literalExpression.Value;

        if (expression is Expression.ListExpression listExpression)
        {
            return EvaluateListExpression(listExpression, environment);
        }

        if (expression is Expression.ParseAndEvalExpression applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(applicationExpression, environment) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate parse and evaluate: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(environment, kernelApplicationExpression) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate kernel function application: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(environment, conditionalExpression);
        }

        if (expression is Expression.EnvironmentExpression)
        {
            return environment;
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpression(stringTagExpression.tagged, environment);
        }

        if (expression is Expression.DelegatingExpression delegatingExpr)
        {
            return delegatingExpr.Delegate.Invoke(evalExprDelegate, environment);
        }

        throw new NotImplementedException("Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public Result<string, PineValue> EvaluateListExpression(
        Expression.ListExpression listExpression,
        PineValue environment)
    {
        var listItems = new List<PineValue>(listExpression.List.Count);

        for (var i = 0; i < listExpression.List.Count; i++)
        {
            var item = listExpression.List[i];

            var itemResult = EvaluateExpression(item, environment);

            if (itemResult is Result<string, PineValue>.Err itemErr)
                return "Failed to evaluate list element [" + i + "]: " + itemErr.Value;

            if (itemResult is Result<string, PineValue>.Ok itemOk)
            {
                listItems.Add(itemOk.Value);
                continue;
            }

            throw new NotImplementedException("Unexpected result type: " + itemResult.GetType().FullName);
        }

        return PineValue.List(listItems);
    }

    private static readonly System.Threading.AsyncLocal<int> evalDepth = new();

    public Result<string, PineValue> EvaluateParseAndEvalExpression(
        Expression.ParseAndEvalExpression parseAndEval,
        PineValue environment)
    {
        Result<string, PineValue> continueWithEnvValueAndFunction(
            PineValue environmentValue,
            Expression functionExpression)
        {
            if (environmentValue is PineValue.ListValue list)
            {
                FunctionApplicationMaxEnvSize =
                FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
            }

            /*
             * 2024-05-19 work around for stack overflow:
             * After seeing some apps crash with stack overflow, reduce stack sizes by offloading to another thread.
             * 

            var evalResult = EvaluateExpression(environment: environmentValue, expression: functionExpression);

            return evalResult;
            */

            var evalDelegate =
                new Func<Result<string, PineValue>>(
                    () => EvaluateExpression(environment: environmentValue, expression: functionExpression));

            evalDepth.Value++;

            try
            {
                if (evalDepth.Value < 16)
                {
                    /*
                     * As long as we don't risk the stack becoming too large, reuse the same thread/stack.
                     * */
                    return evalDelegate();
                }

                /*
                 * Specify a cancellation token to prevent the Task framework from attempting synchronous execution.
                 * From the documentation on Task:
                 * > We will attempt inline execution only if an infinite wait was requested
                 * > [...]
                 * Source: https://github.com/dotnet/runtime/blob/087e15321bb712ef6fe8b0ba6f8bd12facf92629/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Task.cs#L2997-L3008
                 * */

                var cancellationTokenSource = new System.Threading.CancellationTokenSource();

                var task =
                    System.Threading.Tasks.Task.Run(
                        function: evalDelegate,
                        cancellationToken: cancellationTokenSource.Token);

                task.Wait(cancellationToken: cancellationTokenSource.Token);

                return task.Result;
            }
            finally
            {
                evalDepth.Value--;
            }
        }

        return
            EvaluateExpression(parseAndEval.environment, environment) switch
            {
                Result<string, PineValue>.Err envErr =>
                "Failed to evaluate argument: " + envErr.Value,

                Result<string, PineValue>.Ok environmentValue =>
                EvaluateExpression(parseAndEval.expression, environment) switch
                {
                    Result<string, PineValue>.Err exprErr =>
                    "Failed to evaluate expression: " + exprErr.Value,

                    Result<string, PineValue>.Ok expressionValue =>
                    ParseExpressionFromValue(expressionValue.Value) switch
                    {
                        Result<string, Expression>.Err parseErr =>
                        "Failed to parse expression from value: " + parseErr.Value +
                        " - expressionValue is " + DescribeValueForErrorMessage(expressionValue.Value) +
                        " - environmentValue is " + DescribeValueForErrorMessage(expressionValue.Value),

                        Result<string, Expression>.Ok functionExpression =>
                        continueWithEnvValueAndFunction(environmentValue.Value, functionExpression.Value),

                        var otherResult =>
                        throw new NotImplementedException("Unexpected result type for parse: " + otherResult.GetType().FullName)
                    },

                    var otherResult =>
                    throw new NotImplementedException("Unexpected result type for expr: " + otherResult.GetType().FullName)
                },

                var otherResult =>
                throw new NotImplementedException("Unexpected result type for env: " + otherResult.GetType().FullName)
            };
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");


    public Result<string, PineValue> EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application) =>
        EvaluateExpression(application.argument, environment) switch
        {
            Result<string, PineValue>.Ok argument =>
            application.function(argument.Value),

            Result<string, PineValue>.Err error =>
            "Failed to evaluate argument: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };


    public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional) =>
        EvaluateExpression(conditional.condition, environment) switch
        {
            Result<string, PineValue>.Ok conditionValue =>
            conditionValue == PineVMValues.TrueValue
            ?
            EvaluateExpression(conditional.ifTrue, environment)
            :
            conditionValue == PineVMValues.FalseValue
            ?
            EvaluateExpression(conditional.ifFalse, environment)
            :
            PineValue.EmptyList,

            Result<string, PineValue>.Err error =>
            "Failed to evaluate condition: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };

    private static readonly IReadOnlyDictionary<string, Func<PineValue, PineValue>> NamedKernelFunctions =
        ImmutableDictionary<string, Func<PineValue, PineValue>>.Empty
        .SetItem(nameof(KernelFunction.equal), KernelFunction.equal)
        .SetItem(nameof(KernelFunction.negate), KernelFunction.negate)
        .SetItem(nameof(KernelFunction.length), KernelFunction.length)
        .SetItem(nameof(KernelFunction.skip), KernelFunction.skip)
        .SetItem(nameof(KernelFunction.take), KernelFunction.take)
        .SetItem(nameof(KernelFunction.reverse), KernelFunction.reverse)
        .SetItem(nameof(KernelFunction.concat), KernelFunction.concat)
        .SetItem(nameof(KernelFunction.list_head), KernelFunction.list_head)
        .SetItem(nameof(KernelFunction.add_int), KernelFunction.add_int)
        .SetItem(nameof(KernelFunction.mul_int), KernelFunction.mul_int)
        .SetItem(nameof(KernelFunction.is_sorted_ascending_int), KernelFunction.is_sorted_ascending_int);

    public static PineValue ValueFromBool(bool b) => b ? PineVMValues.TrueValue : PineVMValues.FalseValue;

    public static Result<string, bool> ParseBoolFromValue(PineValue value) =>
        value == PineVMValues.TrueValue
        ?
        true
        :
        value == PineVMValues.FalseValue
        ?
        false
        :
        "Value is neither True nor False";

    public static Result<string, PineValue> EncodeExpressionAsValue(Expression expression) =>
        expression switch
        {
            Expression.LiteralExpression literal =>
            EncodeChoiceTypeVariantAsPineValue("Literal", literal.Value),

            Expression.EnvironmentExpression =>
            EncodeChoiceTypeVariantAsPineValue("Environment", PineValue.EmptyList),

            Expression.ListExpression list =>
            list.List.Select(EncodeExpressionAsValue)
            .ListCombine()
            .Map(listElements => EncodeChoiceTypeVariantAsPineValue("List", PineValue.List(listElements))),

            Expression.ConditionalExpression conditional =>
            EncodeConditionalExpressionAsValue(conditional),

            Expression.ParseAndEvalExpression parseAndEval =>
            EncodeParseAndEvalExpression(parseAndEval),

            Expression.KernelApplicationExpression kernelAppl =>
            EncodeKernelApplicationExpression(kernelAppl),

            Expression.StringTagExpression stringTag =>
            EncodeExpressionAsValue(stringTag.tagged)
            .Map(encodedTagged => EncodeChoiceTypeVariantAsPineValue(
                "StringTag",
                PineValue.List([PineValueAsString.ValueFromString(stringTag.tag), encodedTagged]))),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        IReadOnlyDictionary<PineValue, Expression.DelegatingExpression> parseExpressionOverrides)
    {
        if (parseExpressionOverrides.TryGetValue(value, out var delegatingExpression))
            return delegatingExpression;

        return
            ParseExpressionFromValueDefault(
                value,
                generalParser: value => ParseExpressionFromValue(value, parseExpressionOverrides));
    }

    public Result<string, Expression> ParseExpressionFromValue(PineValue value) =>
        parseExpressionDelegate(value);

    public static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value) =>
        ParseExpressionFromValueDefault(
            value,
            generalParser: ParseExpressionFromValueDefault);

    public static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser) =>
        value switch
        {
            PineValue.ListValue rootList =>
            rootList.Elements.Count is not 2
            ?
            "Unexpected number of items in list: Not 2 but " + rootList.Elements.Count
            :
            PineValueAsString.StringFromValue(rootList.Elements[0]) switch
            {
                Result<string, string>.Err err =>
                err.Value,

                Result<string, string>.Ok tag =>
                tag.Value switch
                {
                    "Literal" =>
                    new Expression.LiteralExpression(rootList.Elements[1]),

                    "List" =>
                    ParsePineListValue(rootList.Elements[1]) switch
                    {
                        Result<string, IReadOnlyList<PineValue>>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, IReadOnlyList<PineValue>>.Ok list =>
                        ResultListMapCombine(list.Value, generalParser) switch
                        {
                            Result<string, IReadOnlyList<Expression>>.Err err =>
                            (Result<string, Expression>)err.Value,

                            Result<string, IReadOnlyList<Expression>>.Ok expressionList =>
                            Result<string, Expression>.ok(new Expression.ListExpression(expressionList.Value)),

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        },

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "ParseAndEval" =>
                    ParseParseAndEvalExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.ParseAndEvalExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.ParseAndEvalExpression>.Ok parseAndEval =>
                        (Result<string, Expression>)parseAndEval.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "KernelApplication" =>
                    ParseKernelApplicationExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.KernelApplicationExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.KernelApplicationExpression>.Ok kernelApplication =>
                        (Result<string, Expression>)kernelApplication.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Conditional" =>
                    ParseConditionalExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.ConditionalExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.ConditionalExpression>.Ok conditional =>
                        (Result<string, Expression>)conditional.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Environment" =>
                    environmentExpr,

                    "StringTag" =>
                    ParseStringTagExpression(generalParser, rootList.Elements[1])
                    .Map(stringTag => (Expression)stringTag),

                    var otherTag =>
                    "Tag name does not match any known expression variant: " + otherTag
                },

                var other =>
                "Unexpected result type: " + other.GetType().FullName
            },

            var other =>
            "Unexpected value type, not a list: " + other.GetType().FullName
        };

    private static readonly Expression environmentExpr = new Expression.EnvironmentExpression();

    public static Result<string, PineValue> EncodeParseAndEvalExpression(Expression.ParseAndEvalExpression parseAndEval) =>
        EncodeExpressionAsValue(parseAndEval.expression)
        .AndThen(encodedExpression =>
        EncodeExpressionAsValue(parseAndEval.environment)
        .Map(encodedEnvironment =>
        EncodeChoiceTypeVariantAsPineValue("ParseAndEval",
            EncodeRecordToPineValue(
                (nameof(Expression.ParseAndEvalExpression.environment), encodedEnvironment),
                (nameof(Expression.ParseAndEvalExpression.expression), encodedExpression)))));

    public static Result<string, Expression.ParseAndEvalExpression> ParseParseAndEvalExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            ("environment", generalParser),
            ("expression", generalParser),
            (environment, expression) => new Expression.ParseAndEvalExpression(expression: expression, environment: environment));

    public static Result<string, PineValue> EncodeKernelApplicationExpression(Expression.KernelApplicationExpression kernelApplicationExpression) =>
        EncodeExpressionAsValue(kernelApplicationExpression.argument)
        .Map(encodedArgument =>
        EncodeChoiceTypeVariantAsPineValue("KernelApplication",
            EncodeRecordToPineValue(
                (nameof(Expression.KernelApplicationExpression.argument), encodedArgument),
                (nameof(Expression.KernelApplicationExpression.functionName), PineValueAsString.ValueFromString(kernelApplicationExpression.functionName)))));

    public static Result<string, Expression.KernelApplicationExpression> ParseKernelApplicationExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            (nameof(Expression.KernelApplicationExpression.functionName), decode: PineValueAsString.StringFromValue),
            (nameof(Expression.KernelApplicationExpression.argument), decode: generalParser),
            (functionName, argument) => (functionName, argument))
        switch
        {
            Result<string, (string functionName, Expression argument)>.Err err =>
            err.Value,

            Result<string, (string functionName, Expression argument)>.Ok functionNameAndArgument =>
            ParseKernelApplicationExpression(functionNameAndArgument.Value.functionName, functionNameAndArgument.Value.argument),

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static Expression.KernelApplicationExpression ParseKernelApplicationExpressionThrowOnUnknownName(
        string functionName,
        Expression argument) =>
        ParseKernelApplicationExpression(functionName, argument)
        .Extract(err => throw new Exception(err));

    public static Result<string, Expression.KernelApplicationExpression> ParseKernelApplicationExpression(
        string functionName,
        Expression argument)
    {
        if (!NamedKernelFunctions.TryGetValue(functionName, out var kernelFunction))
        {
            return "Did not find kernel function '" + functionName + "'";
        }

        return
            new Expression.KernelApplicationExpression(
                functionName: functionName,
                function: kernelFunction,
                argument: argument);
    }

    public static Result<string, PineValue> EncodeConditionalExpressionAsValue(Expression.ConditionalExpression conditionalExpression) =>
        EncodeExpressionAsValue(conditionalExpression.condition)
        .AndThen(encodedCondition =>
        EncodeExpressionAsValue(conditionalExpression.ifTrue)
        .AndThen(encodedIfTrue =>
        EncodeExpressionAsValue(conditionalExpression.ifFalse)
        .Map(encodedIfFalse =>
        EncodeChoiceTypeVariantAsPineValue("Conditional",
            EncodeRecordToPineValue(
                (nameof(Expression.ConditionalExpression.condition), encodedCondition),
                (nameof(Expression.ConditionalExpression.ifFalse), encodedIfFalse),
                (nameof(Expression.ConditionalExpression.ifTrue), encodedIfTrue))))));

    public static Result<string, Expression.ConditionalExpression> ParseConditionalExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        ParseRecord3FromPineValue(
            value,
            (nameof(Expression.ConditionalExpression.condition), generalParser),
            (nameof(Expression.ConditionalExpression.ifTrue), generalParser),
            (nameof(Expression.ConditionalExpression.ifFalse), generalParser),
            (condition, ifTrue, ifFalse) => new Expression.ConditionalExpression(condition: condition, ifTrue: ifTrue, ifFalse: ifFalse));

    public static Result<string, Expression.StringTagExpression> ParseStringTagExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        ParsePineListValue(value) switch
        {
            Result<string, IReadOnlyList<PineValue>>.Err err =>
            err.Value,

            Result<string, IReadOnlyList<PineValue>>.Ok list =>
            ParseListWithExactlyTwoElements(list.Value) switch
            {
                Result<string, (PineValue, PineValue)>.Err err =>
                err.Value,

                Result<string, (PineValue, PineValue)>.Ok tagValueAndTaggedValue =>
                PineValueAsString.StringFromValue(tagValueAndTaggedValue.Value.Item1) switch
                {
                    Result<string, string>.Err err =>
                    err.Value,

                    Result<string, string>.Ok tag =>
                    generalParser(tagValueAndTaggedValue.Value.Item2) switch
                    {
                        Result<string, Expression>.Err err =>
                        err.Value,

                        Result<string, Expression>.Ok tagged =>
                        new Expression.StringTagExpression(tag: tag.Value, tagged: tagged.Value),

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    var other =>
                    throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                },

                var other =>
                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static Result<ErrT, IReadOnlyList<MappedOkT>> ResultListMapCombine<ErrT, OkT, MappedOkT>(
        IReadOnlyList<OkT> list,
        Func<OkT, Result<ErrT, MappedOkT>> mapElement) =>
        list.Select(mapElement).ListCombine();

    public static Result<string, Record> ParseRecord3FromPineValue<FieldA, FieldB, FieldC, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        (string name, Func<PineValue, Result<string, FieldC>> decode) fieldC,
        Func<FieldA, FieldB, FieldC, Record> compose) =>
        ParseRecordFromPineValue(value) switch
        {
            Result<string, IReadOnlyDictionary<string, PineValue>>.Err err =>
            (Result<string, Record>)("Failed to decode as record: " + err.Value),

            Result<string, IReadOnlyDictionary<string, PineValue>>.Ok record =>
            record.Value.TryGetValue(fieldA.name, out var fieldAValue) switch
            {
                false =>
                "Did not find field " + fieldA.name,

                true =>
                record.Value.TryGetValue(fieldB.name, out var fieldBValue) switch
                {
                    false =>
                    "Did not find field " + fieldB.name,

                    true =>
                    record.Value.TryGetValue(fieldC.name, out var fieldCValue) switch
                    {
                        false =>
                        "Did not find field " + fieldC.name,

                        true =>
                        fieldA.decode(fieldAValue) switch
                        {
                            Result<string, FieldA>.Err err =>
                            (Result<string, Record>)err.Value,

                            Result<string, FieldA>.Ok fieldADecoded =>
                            fieldB.decode(fieldBValue) switch
                            {
                                Result<string, FieldB>.Err err =>
                                (Result<string, Record>)err.Value,

                                Result<string, FieldB>.Ok fieldBDecoded =>
                                fieldC.decode(fieldCValue) switch
                                {
                                    Result<string, FieldC>.Err err =>
                                    (Result<string, Record>)err.Value,

                                    Result<string, FieldC>.Ok fieldCDecoded =>
                                    Result<string, Record>.ok(compose(fieldADecoded.Value, fieldBDecoded.Value, fieldCDecoded.Value)),

                                    var other =>
                                    throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                                },

                                var other =>
                                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                            },

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        }
                    }
                }
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static Result<string, Record> DecodeRecord2FromPineValue<FieldA, FieldB, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        Func<FieldA, FieldB, Record> compose) =>
        ParseRecordFromPineValue(value) switch
        {
            Result<string, IReadOnlyDictionary<string, PineValue>>.Err err =>
            (Result<string, Record>)("Failed to decode as record: " + err.Value),

            Result<string, IReadOnlyDictionary<string, PineValue>>.Ok record =>
            record.Value.TryGetValue(fieldA.name, out var fieldAValue) switch
            {
                false =>
                "Did not find field " + fieldA.name,

                true =>
                record.Value.TryGetValue(fieldB.name, out var fieldBValue) switch
                {
                    false =>
                    "Did not find field " + fieldB.name,

                    true =>
                    fieldA.decode(fieldAValue) switch
                    {
                        Result<string, FieldA>.Err err =>
                        (Result<string, Record>)err.Value,

                        Result<string, FieldA>.Ok fieldADecoded =>
                        fieldB.decode(fieldBValue) switch
                        {
                            Result<string, FieldB>.Err err =>
                            (Result<string, Record>)err.Value,

                            Result<string, FieldB>.Ok fieldBDecoded =>
                            Result<string, Record>.ok(compose(fieldADecoded.Value, fieldBDecoded.Value)),

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        },

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    }
                }
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static PineValue EncodeRecordToPineValue(params (string fieldName, PineValue fieldValue)[] fields) =>
        PineValue.List(
            [..fields.Select(field => PineValue.List(
                [
                    PineValueAsString.ValueFromString(field.fieldName),
                    field.fieldValue
                ]))]);


    public static Result<string, IReadOnlyDictionary<string, PineValue>> ParseRecordFromPineValue(PineValue value)
    {
        if (ParsePineListValue(value) is not Result<string, IReadOnlyList<PineValue>>.Ok listResult)
            return "Failed to parse as list";

        var recordFields = new Dictionary<string, PineValue>(listResult.Value.Count);

        foreach (var listElement in listResult.Value)
        {
            if (ParsePineListValue(listElement) is not Result<string, IReadOnlyList<PineValue>>.Ok listElementResult)
                return "Failed to parse list element as list";

            if (ParseListWithExactlyTwoElements(listElementResult.Value) is not Result<string, (PineValue, PineValue)>.Ok fieldNameValueAndValue)
                return "Failed to parse list element as list with exactly two elements";

            if (PineValueAsString.StringFromValue(fieldNameValueAndValue.Value.Item1) is not Result<string, string>.Ok fieldName)
                return "Failed to parse field name as string";

            recordFields[fieldName.Value] = fieldNameValueAndValue.Value.Item2;
        }

        return recordFields;
    }

    public static PineValue EncodeChoiceTypeVariantAsPineValue(string tagName, PineValue tagArguments) =>
        PineValue.List(
            [
                PineValueAsString.ValueFromString(tagName),
                tagArguments,
            ]);

    public static Result<string, T> ParseChoiceFromPineValue<T>(
        Func<PineValue, Result<string, T>> generalParser,
        IReadOnlyDictionary<PineValue, Func<Func<PineValue, Result<string, T>>, PineValue, Result<string, T>>> variants,
        PineValue value) =>
        ParsePineListValue(value) switch
        {
            Result<string, IReadOnlyList<PineValue>>.Err err =>
            err.Value,

            Result<string, IReadOnlyList<PineValue>>.Ok list =>
            ParseListWithExactlyTwoElements(list.Value) switch
            {
                Result<string, (PineValue, PineValue)>.Err err =>
                err.Value,

                Result<string, (PineValue, PineValue)>.Ok tagNameValueAndValue =>
                PineValueAsString.StringFromValue(tagNameValueAndValue.Value.Item1) switch
                {
                    Result<string, string>.Err err =>
                    err.Value,

                    Result<string, string>.Ok tagName =>
                    variants.TryGetValue(tagNameValueAndValue.Value.Item1, out var variant) switch
                    {
                        false =>
                        "Unexpected tag name: " + tagName,

                        true =>
                        variant(generalParser, tagNameValueAndValue.Value.Item2)
                    },

                    var other =>
                    throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                },

                var other =>
                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static Result<string, IReadOnlyList<PineValue>> ParsePineListValue(PineValue value)
    {
        if (value is not PineValue.ListValue listValue)
            return "Not a list";

        return
            Result<string, IReadOnlyList<PineValue>>.ok(listValue.Elements);
    }

    public static Result<string, (T, T)> ParseListWithExactlyTwoElements<T>(IReadOnlyList<T> list)
    {
        if (list.Count is not 2)
            return "Unexpected number of items in list: Not 2 but " + list.Count;

        return (list[0], list[1]);
    }
}
