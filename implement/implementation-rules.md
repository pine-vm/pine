# Implementation Rules

Consider these rules for any work with and on the implementation found in the Pine repository. These rules also apply to any pull requests opened on the Pine repository.

## Running Tests in .NET

Since the .NET projects in this repository use the new [`Microsoft.Testing.Platform`](https://learn.microsoft.com/en-us/dotnet/core/testing/microsoft-testing-platform-intro?tabs=dotnetcli) the `dotnet test` command does not always work as usual.

To run tests in a .NET project, run the command `dotnet  run` from the directory containing the `.csproj` file.

### Running Specific Tests in .NET

To filter tests to run by C# method name, use a command like this:

```cmd
dotnet  run  --  --filter-method="*method_name*"
```

> Warning: Running a command like `dotnet  test  --filter=method_name` does not work as expected, as it will silently ignore the filter and run all tests instead. (This issue been reported for example at <https://github.com/dotnet/sdk/issues/45927> and <https://github.com/dotnet/sdk/issues/49210>)

The command-line interface of the test framework offers many more options, like filtering tests by various attributes, configuring a timeout and more. To see an overview of available options, run the following command from the directory containing the `.csproj` file:

```cmd
dotnet  run  --  --help
```

## Formatting C# Code

Before submitting a pull request, format any added or changed C# code using the command `dotnet  format`

## Exhaustive `switch` over Variant Hierarchies

The C# compiler does **not** statically check exhaustiveness of `switch` statements or `switch` expressions over an open record / class hierarchy (for example over `Pine.Core.Expression` and its variants, over `Pine.Core.Elm.ElmSyntax.SyntaxModel.Pattern` variants, etc.). Adding a new variant, therefore, silently slips through every existing `switch` that does not list it.

To compensate for this missing language feature, **every `switch` statement and `switch` expression that switches over the variants of such a hierarchy must satisfy both of the following requirements**:

1. **List every existing variant explicitly**. Do not rely on a wildcard `_`, on an `is`/`is not` guard, or on a `when` filter to "cover" any variant — every variant must appear by its concrete type name in a `case` label or switch arm.
2. **End the `switch` with a `default` arm that throws `NotImplementedException`**, naming the surrounding function and the actual runtime variant. Use the same wording as elsewhere in the codebase, for example:

   ```csharp
   default:
       throw new NotImplementedException(
           "<MethodName> does not handle expression variant: " + expression.GetType().Name);
   ```

   For `switch` expressions, the equivalent is a final `_ => throw new NotImplementedException(...)` arm.

This rule is mandatory whenever the discriminand belongs to a variant hierarchy in this repository. The rule applies even to selective switches whose body only acts on a few variants — every other variant must still be listed (typically as a no-op `case` label), and the `default` arm must still throw.

Multiple consecutive no-op variants **may** be grouped under a single shared body using stacked `case` labels, as long as each variant still appears by its concrete type name in a `case` label. For example, when only the variants `RecordAccess`, `RecordAccessFunction`, and `RecordUpdateExpression` need real handling, and every other `Expression` variant is a no-op:

```csharp
switch (expression)
{
    case SyntaxTypes.Expression.RecordAccess recordAccess:
        // ... handle ...
        break;

    case SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction:
        // ... handle ...
        break;

    case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
        // ... handle ...
        break;

    // No-op variants — grouped, but each variant is still named explicitly.
    case SyntaxTypes.Expression.UnitExpr:
    case SyntaxTypes.Expression.Literal:
    case SyntaxTypes.Expression.CharLiteral:
    case SyntaxTypes.Expression.Integer:
    // ... every remaining variant listed by its concrete type ...
    case SyntaxTypes.Expression.GLSLExpression:
        break;

    default:
        throw new NotImplementedException(
            "MyMethod does not handle expression variant: " + expression.GetType().Name);
}
```

The rule does **not** apply to switches over closed primitive sets (for example `int`, `bool`, `enum` types, or strings) where missing a value is not a forward-compatibility hazard.

The point of the rule is to **guarantee a runtime crash the first time a new variant flows into a function that has not been updated**, so that incomplete handling is detected immediately during testing rather than being silently misinterpreted as a no-op or a wildcard fallback.

