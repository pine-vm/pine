using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// An Elm module's type-only API. This is not source code, a runtime implementation, or an executable stub.
/// Types without public constructors are opaque and have no assumed runtime representation.
/// </summary>
public sealed record ElmModuleTypeInterface(
    string PackageName,
    ImmutableDictionary<string, int> Types,
    ImmutableDictionary<string, Type> Values)
{
    /// <summary>Public constructors, grouped by their owning type. Constructor signatures are in Values.</summary>
    public ImmutableDictionary<string, ImmutableList<string>> TypeConstructors { get; init; } = [];

    /// <summary>
    /// Standard interfaces in their original namespaces. Implemented source exports take precedence.
    /// Resolved builds select these only for the corresponding bundled package substitution.
    /// </summary>
    public static readonly ImmutableDictionary<string, ElmModuleTypeInterface> Standard = CreateStandard();

    internal static readonly ImmutableDictionary<string, Type> StandardFunctionSignatures =
        FunctionSignatures(Standard);

    /// <summary>Returns the value signatures under their compiler-qualified names.</summary>
    public static ImmutableDictionary<string, Type> FunctionSignatures(
        IReadOnlyDictionary<string, ElmModuleTypeInterface> modules) =>
        modules.SelectMany(
            module => module.Value.Values.Select(
                value =>
                KeyValuePair.Create(module.Key + "." + value.Key, value.Value))).ToImmutableDictionary();

    /// <summary>Rewrites nominal type identities along with module identities during package isolation.</summary>
    public ElmModuleTypeInterface RenameModules(Func<string, string> rename)
    {
        Type Rewrite(Type type) =>
            type switch
            {
                Type.ChoiceType choice =>
                new Type.ChoiceType(
                    rename(string.Join(".", choice.ModuleName)).Split('.'),
                    choice.TypeName,
                    [.. choice.TypeArguments.Select(Rewrite)]),

                Type.FunctionType function =>
                new Type.FunctionType(Rewrite(function.ArgumentType), Rewrite(function.ReturnType)),

                Type.ListType list => new Type.ListType(Rewrite(list.ElementType)),
                Type.TupleType tuple => new Type.TupleType([.. tuple.ElementTypes.Select(Rewrite)]),

                Type.RecordType record =>
                new Type.RecordType([.. record.Fields.Select(field => (field.FieldName, Rewrite(field.FieldType)))]),

                Type.OpenRecordType record =>
                new Type.OpenRecordType(
                    record.ExtensionVariable,
                    [.. record.KnownFields.Select(field => (field.FieldName, Rewrite(field.FieldType)))]),

                Type.IntType or Type.FloatType or Type.StringType or Type.CharType or Type.BoolType or
                Type.NumberType or Type.TypeVariable or Type.UnknownType =>
                type,

                _ =>
                throw new NotImplementedException(
                    $"{nameof(RenameModules)} does not handle inferred type variant: {type.GetType().Name}")
            };

        return this with { Values = Values.ToImmutableDictionary(value => value.Key, value => Rewrite(value.Value)) };
    }

    private static ImmutableDictionary<string, ElmModuleTypeInterface> CreateStandard()
    {
        var modules = ImmutableDictionary.CreateBuilder<string, ElmModuleTypeInterface>();
        var a = new Type.TypeVariable("a");
        var b = new Type.TypeVariable("b");
        var c = new Type.TypeVariable("c");
        var x = new Type.TypeVariable("x");
        var y = new Type.TypeVariable("y");
        var msg = new Type.TypeVariable("msg");
        var model = new Type.TypeVariable("model");
        var flags = new Type.TypeVariable("flags");
        var unit = new Type.TupleType([]);
        var integer = Type.Int();
        var floating = new Type.FloatType();
        var text = Type.String();

        Type Named(string module, string name, params Type[] arguments) =>
            new Type.ChoiceType(module.Split('.'), name, arguments);

        Type Fn(params Type[] types) =>
            types.SkipLast(1).Reverse().Aggregate(
                types[^1],
                (result, argument) => new Type.FunctionType(argument, result));

        Type Cmd(Type item) => Named("Platform.Cmd", "Cmd", item);
        Type Sub(Type item) => Named("Platform.Sub", "Sub", item);
        Type Task(Type error, Type value) => Named("Task", "Task", error, value);
        Type Never() => Named("Basics", "Never");
        Type Result(Type error, Type value) => Named("Result", "Result", error, value);
        Type Pair(Type left, Type right) => new Type.TupleType([left, right]);

        Type Record(params (string, Type)[] fields) =>
            new Type.RecordType([.. fields.OrderBy(field => field.Item1, StringComparer.Ordinal)]);

        void Module(
            string name,
            string package,
            (string name, int arity)[] types,
            params (string name, Type type)[] values) =>
            modules.Add(
                name,
                new(
                    package,
                    types.ToImmutableDictionary(type => type.name, type => type.arity),
                    values.ToImmutableDictionary(value => value.name, value => value.type)));

        Module(
            "Platform.Cmd",
            "elm/core",
            [("Cmd", 1)],
            ("none", Cmd(msg)),
            ("batch", Fn(new Type.ListType(Cmd(msg)), Cmd(msg))),
            ("map", Fn(Fn(a, b), Cmd(a), Cmd(b))));

        Module(
            "Platform.Sub",
            "elm/core",
            [("Sub", 1)],
            ("none", Sub(msg)),
            ("batch", Fn(new Type.ListType(Sub(msg)), Sub(msg))),
            ("map", Fn(Fn(a, b), Sub(a), Sub(b))));

        Module(
            "Platform",
            "elm/core",
            [("Program", 3)],
            ("worker",
            Fn(
                Record(
                    ("init", Fn(flags, Pair(model, Cmd(msg)))),
                    ("update", Fn(msg, model, Pair(model, Cmd(msg)))),
                    ("subscriptions", Fn(model, Sub(msg)))),
                Named("Platform", "Program", flags, model, msg))));

        var taskValues =
            new List<(string, Type)>
            {
                ("succeed", Fn(a, Task(x, a))),
                ("fail", Fn(x, Task(x, a))),
                ("map", Fn(Fn(a, b), Task(x, a), Task(x, b))),
                ("andThen", Fn(Fn(a, Task(x, b)), Task(x, a), Task(x, b))),
                ("sequence", Fn(new Type.ListType(Task(x, a)), Task(x, new Type.ListType(a)))),
                ("onError", Fn(Fn(x, Task(y, a)), Task(x, a), Task(y, a))),
                ("mapError", Fn(Fn(x, y), Task(x, a), Task(y, a))),
                ("perform", Fn(Fn(a, msg), Task(Never(), a), Cmd(msg))),
                ("attempt", Fn(Fn(Result(x, a), msg), Task(x, a), Cmd(msg)))
            };

        for (var arity = 2; arity <= 5; arity++)
        {
            var arguments =
                new Type[] { a, b, c, new Type.TypeVariable("d"), new Type.TypeVariable("e") }.Take(arity).ToArray();

            var output = new Type.TypeVariable("result");

            taskValues.Add(
                ("map" + arity,
                Fn([Fn([.. arguments, output]), .. arguments.Select(argument => Task(x, argument)), Task(x, output)])));
        }

        Module("Task", "elm/core", [("Task", 2)], [.. taskValues]);

        Module(
            "Process",
            "elm/core",
            [("Id", 0)],
            ("spawn", Fn(Task(x, a), Task(y, Named("Process", "Id")))),
            ("sleep", Fn(floating, Task(x, unit))),
            ("kill", Fn(Named("Process", "Id"), Task(x, unit))));

        var posix = Named("Time", "Posix");
        var zone = Named("Time", "Zone");
        var era = Record(("start", integer), ("offset", integer));

        Module(
            "Time",
            "elm/time",
            [("Posix", 0), ("Zone", 0), ("Month", 0), ("Weekday", 0), ("ZoneName", 0)],
            ("now", Task(x, posix)),
            ("here", Task(x, zone)),
            ("getZoneName", Task(x, Named("Time", "ZoneName"))),
            ("every", Fn(floating, Fn(posix, msg), Sub(msg))),
            ("utc", zone),
            ("customZone", Fn(integer, new Type.ListType(era), zone)),
            ("millisToPosix", Fn(integer, posix)),
            ("posixToMillis", Fn(posix, integer)),
            ("toYear", Fn(zone, posix, integer)),
            ("toMonth", Fn(zone, posix, Named("Time", "Month"))),
            ("toDay", Fn(zone, posix, integer)),
            ("toWeekday", Fn(zone, posix, Named("Time", "Weekday"))),
            ("toHour", Fn(zone, posix, integer)),
            ("toMinute", Fn(zone, posix, integer)),
            ("toSecond", Fn(zone, posix, integer)),
            ("toMillis", Fn(zone, posix, integer)));

        var time = modules["Time"];
        var constructors = time.TypeConstructors.ToBuilder();
        var values = time.Values.ToBuilder();

        foreach (var (typeName, names) in new (string, string[])[]
        {
            ("Month", ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]),
            ("Weekday", ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])
        })
        {
            constructors.Add(typeName, [.. names]);

            foreach (var name in names)
                values.Add(name, Named("Time", typeName));
        }

        constructors.Add("ZoneName", ["Name", "Offset"]);
        values.Add("Name", Fn(text, Named("Time", "ZoneName")));
        values.Add("Offset", Fn(integer, Named("Time", "ZoneName")));
        modules["Time"] = time with { TypeConstructors = constructors.ToImmutable(), Values = values.ToImmutable() };

        return modules.ToImmutable();
    }
}
