using Pine.CLI;
using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.CommandLine;
using System.Linq;

namespace Pine.Elm.CLI;

public class ElmFormatCommand
{
    public static Command CreateElmFormatCommand()
    {
        var command = new Command("elm-format", "Format Elm module files.");

        var pathsArgument =
            new Argument<string[]>("paths")
            {
                Description = "Paths to Elm files or directories to format",
                Arity = ArgumentArity.OneOrMore
            };

        var yesOption =
            new Option<bool>("--yes")
            {
                Description = "Overwrite files without prompting for confirmation"
            };

        var verifyNoChangesOption =
            new Option<bool>("--verify-no-changes")
            {
                Description = "Check if all Elm modules are already formatted (for CI/automated reviews)"
            };

        var colorOption = FormatCommandShared.CreateColorOption();

        command.Add(pathsArgument);
        command.Add(yesOption);
        command.Add(verifyNoChangesOption);
        command.Add(colorOption);

        command.SetAction(
            parseResult =>
            {
                var paths = parseResult.GetValue(pathsArgument);
                var yes = parseResult.GetValue(yesOption);
                var verifyNoChanges = parseResult.GetValue(verifyNoChangesOption);
                var colorMode = parseResult.GetValue(colorOption);

                return
                    FormatCommandShared.Execute(
                        paths: paths!,
                        fileExtension: ".elm",
                        formatFile: FormatElmFile,
                        skipPrompt: yes,
                        verifyNoChanges: verifyNoChanges,
                        commandLabel: "elm-format",
                        colorMode: colorMode);
            });

        return command;
    }


    private static FormatFileResult FormatElmFile(string fileContent)
    {
        var formatResult =
            ElmFormat.FormatModuleTextReportingSyntaxErrors(fileContent);

        if (formatResult.IsErrOrNullable() is { } formatError)
        {
            return new FormatFileResult.Error(formatError.ToString());
        }

        var formatOk =
            formatResult.IsOkOrNull()
            ??
            throw new NotImplementedException(
                "Unexpected ElmFormat.FormatModuleTextReportingSyntaxErrors result: " +
                formatResult.GetType());

        var diagnostics =
            formatOk.SyntaxErrors
            .Select(
                error =>
                new FormatFileDiagnostic(
                    Message: error.Message,
                    Line: error.Location.Row,
                    Column: error.Location.Column))
            .ToList();

        return
            fileContent == formatOk.FormattedText
            ?
            new FormatFileResult.Stable(diagnostics)
            :
            new FormatFileResult.Changed(
                formatOk.FormattedText,
                diagnostics);
    }
}
