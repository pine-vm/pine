using Pine.Elm.CommonBinaries;
using System;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;

namespace Pine.CLI;

public static class ElmTestRsCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "elm-test-rs",
                "Compile and run tests using the interface of elm-test-rs. The compilation integrates interfaces such as SourceFiles.");

        var sourceArgument =
            new Argument<string?>("source")
            {
                Arity = ArgumentArity.ZeroOrOne
            };

        var elmTestRsOutputOption = new Option<string?>("--elm-test-rs-output");

        command.Add(sourceArgument);
        command.Add(elmTestRsOutputOption);

        command.SetAction(
            (parseResult) =>
            {
                var source = parseResult.GetValue(sourceArgument);
                var elmTestRsOutput = parseResult.GetValue(elmTestRsOutputOption);

                var elmTestResult = CompileAndElmTestRs(source: source ?? Environment.CurrentDirectory);

                static void saveTextToFileAndReportToConsole(string filePath, string text)
                {
                    filePath = Path.GetFullPath(filePath);

                    Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

                    File.WriteAllText(filePath, text ?? "", Encoding.UTF8);
                    Console.WriteLine("Saved " + text?.Length + " characters to " + filePath);
                }

                if (elmTestRsOutput != null)
                {
                    saveTextToFileAndReportToConsole(elmTestRsOutput + ".stdout", elmTestResult.ProcessOutput.StandardOutput ?? "");
                    saveTextToFileAndReportToConsole(elmTestRsOutput + ".stderr", elmTestResult.ProcessOutput.StandardError ?? "");
                }

                if (0 < elmTestResult.ProcessOutput.StandardError?.Length)
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.WriteLine(elmTestResult.ProcessOutput.StandardError);
                    Console.ResetColor();
                }

                var eventsOutputs =
                    ElmTestRs.OutputFromEvent(elmTestResult.ParseOutputResult);

                foreach (var eventOutput in eventsOutputs)
                {
                    if (eventOutput.text.Any())
                        Console.WriteLine("");

                    foreach (var coloredText in eventOutput.text)
                    {
                        switch (coloredText.color)
                        {
                            case ElmTestRsConsoleOutputColor.RedColor:
                                Console.ForegroundColor = ConsoleColor.Red;
                                break;

                            case ElmTestRsConsoleOutputColor.GreenColor:
                                Console.ForegroundColor = ConsoleColor.Green;
                                break;

                            default:
                                Console.ResetColor();
                                break;
                        }

                        Console.Write(coloredText.text);
                    }
                }

                Console.WriteLine("");

                // TODO: Report more details on timing.

                return elmTestResult.ProcessOutput.ExitCode;
            });

        return command;
    }

    public static ElmTestRs.ElmTestRsRunReport CompileAndElmTestRs(string source)
    {
        var (_, compiledAppFiles) = CompileCommand.CompileApp(source);

        if (compiledAppFiles == null)
            throw new Exception("Compilation failed");

        return ElmTestRs.Run(compiledAppFiles);
    }
}
