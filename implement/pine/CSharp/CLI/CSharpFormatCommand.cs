using Pine.CLI;
using Pine.Core;
using Pine.Core.DotNet;
using System;
using System.CommandLine;

namespace Pine.CSharp.CLI;

public class CSharpFormatCommand
{
    public static Command CreateCSharpFormatCommand()
    {
        var csharpCommand = new Command("csharp", "C# development tools.");

        var formatCommand = new Command("format", "Format C# source files.");

        var pathsArgument =
            new Argument<string[]>("paths")
            {
                Description = "Paths to C# files or directories to format",
                Arity =
                ArgumentArity.OneOrMore
            };

        var yesOption =
            new Option<bool>("--yes")
            {
                Description =
                "Overwrite files without prompting for confirmation"
            };

        var verifyNoChangesOption =
            new Option<bool>("--verify-no-changes")
            {
                Description =
                "Check if all C# files are already formatted (for CI/automated reviews)"
            };

        var colorOption = FormatCommandShared.CreateColorOption();

        formatCommand.Add(pathsArgument);
        formatCommand.Add(yesOption);
        formatCommand.Add(verifyNoChangesOption);
        formatCommand.Add(colorOption);

        formatCommand.SetAction(
            (parseResult) =>
            {
                var paths = parseResult.GetValue(pathsArgument);
                var yes = parseResult.GetValue(yesOption);
                var verifyNoChanges = parseResult.GetValue(verifyNoChangesOption);
                var colorMode = parseResult.GetValue(colorOption);

                return
                    FormatCommandShared.Execute(
                        paths: paths!,
                        fileExtension: ".cs",
                        formatFile: FormatCSharpFile,
                        skipPrompt: yes,
                        verifyNoChanges: verifyNoChanges,
                        commandLabel: "csharp-format",
                        colorMode: colorMode);
            });

        csharpCommand.Add(formatCommand);

        return csharpCommand;
    }

    private static FormatFileResult FormatCSharpFile(string fileContent)
    {
        try
        {
            var formatResult = CSharpFormat.FormatCSharpFile(fileContent);

            if (formatResult.IsErrOrNull() is { } errResult)
            {
                return
                    new FormatFileResult.Error(
                        "Formatting cycle detected. " +
                        "Cycle first string length: " + errResult.CycleFirstString.Length +
                        ", cycle last string length: " + errResult.CycleLastString.Length);
            }

            var formatted =
                formatResult.IsOkOrNull()
                ??
                throw new NotImplementedException(
                    "Unexpected result from CSharpFormat.FormatCSharpFile: " + formatResult);

            if (fileContent.TrimEnd() == formatted.TrimEnd())
            {
                return new FormatFileResult.Stable();
            }

            // Ensure the file ends with a newline
            var formattedWithNewline =
                formatted.EndsWith('\n')
                ?
                formatted
                :
                formatted + "\n";

            return new FormatFileResult.Changed(formattedWithNewline);
        }
        catch (Exception ex)
        {
            return new FormatFileResult.Error(ex.Message);
        }
    }
}
