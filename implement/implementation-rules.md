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

