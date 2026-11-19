using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

namespace Pine.CLI.Elm;

public static class CompileCommand
{
    public static Command Create()
    {
        var command =
            new Command("compile", "Compile one Elm declaration to a raw binary Pine value, without invoking it.");

        var entryPoint = PineCliCommand.RequiredArgument("entry-point");

        entryPoint.Description =
            "Explicit declaration (.elm/name or .elm#Lnumber); no main or application type is required.";

        var inputDirectory =
            new Option<string?>("--input-directory")
            {
                Description = "Source directory for a relative local entry point."
            };

        var output = ValueCommand.OutputOption();
        var timeout = ValueCommand.TimeoutOption();
        command.Add(entryPoint);
        command.Add(inputDirectory);
        command.Add(output);
        command.Add(timeout);

        command.SetAction(
            result => Execute(
                result.GetRequiredValue(entryPoint),
                result.GetRequiredValue(output),
                result.InvocationConfiguration.Error,
                result.GetValue(inputDirectory),
                result.GetValue(timeout) is { } seconds ? TimeSpan.FromSeconds(seconds) : null));

        return command;
    }

    internal static int Execute(
        string entryPoint,
        string output,
        TextWriter error,
        string? inputDirectory = null,
        TimeSpan? timeout = null,
        CancellationToken cancellationToken = default)
    {
        using var cancellation = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);

        if (timeout is { } duration)
            cancellation.CancelAfter(duration);

        var cancelledByUser = false;

        ConsoleCancelEventHandler handler =
            (_, args) =>
            {
                args.Cancel = true;
                cancelledByUser = true;
                cancellation.Cancel();
            };

        Console.CancelKeyPress += handler;
        var token = cancellation.Token;

        try
        {
            var source = new RunSource.Loader(Filesystem.CacheDirectory, error).Read(entryPoint, inputDirectory, token);

            var file =
                source.Tree.GetNodeAtPath(source.FilePath) as FileTree.FileNode
                ?? throw new IOException("Selected Elm module is not a file.");

            var syntax =
                ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(file.Bytes.Span))
                .Extract(message => throw new ArgumentException("Cannot parse selected module: " + message));

            var declaration = source.Declaration;

            if (source.Line is { } line)
            {
                declaration =
                    syntax.Declarations
                    .Where(
                        node => node.Value is Declaration.FunctionDeclaration function &&
                            line >= (function.Function.Documentation?.Range.Start.Row ?? node.Range.Start.Row) &&
                            (line < node.Range.End.Row || line == node.Range.End.Row && node.Range.End.Column > 1))
                    .Select(node => ((Declaration.FunctionDeclaration)node.Value).Function.Declaration.Value.Name.Value)
                    .SingleOrDefault()
                    ?? throw new ArgumentException("The selected line is not inside a function or value declaration.");
            }

            if (declaration is null)
                throw new ArgumentException("Select a declaration explicitly using .elm/name or .elm#Lnumber.");

            var moduleName = string.Join(".", Module.GetModuleName(syntax.ModuleDefinition.Value).Value);
            var manifest = ElmAppDependencyResolution.FindElmJsonForEntryPoint(source.Tree, source.FilePath);
            ElmResolvedBuild? build = null;

            if (manifest is { } selected)
            {
                build =
                    ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                        source.Tree,
                        selected.filePath,
                        [source.FilePath],
                        ElmPackageSubstitutions.DefaultBuild.Value,
                        cancellationToken: token,
                        projectDirectory: selected.filePath.Count is 1 ? source.DirectoryPath : null)
                    .GetAwaiter().GetResult();

                moduleName =
                    build.CompilerModuleOrigins.Single(
                        entry =>
                        entry.Value is ElmModuleOrigin.Project project && project.ModuleName == moduleName).Key;
            }

            token.ThrowIfCancellationRequested();
            var root = DeclQualifiedName.Create(moduleName.Split('.'), declaration);

            var compiled =
                (build is null
                ?
                ElmCompiler.CompileInteractiveEnvironment(source.Tree, [root])
                :
                ElmCompiler.CompileResolvedEnvironment(build, [root]))
                .Extract(message => throw new InvalidOperationException(message));

            var environment =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
                .Extract(message => throw new InvalidDataException(message));

            var value =
                environment.Modules.Single(module => module.moduleName == moduleName)
                .moduleContent.FunctionDeclarations[declaration];

            token.ThrowIfCancellationRequested();
            ValueCommand.Write(output, value, token);
            error.WriteLine("Compiled " + moduleName + "." + declaration + " to '" + output + "'.");
            return 0;
        }
        catch (Exception) when (token.IsCancellationRequested)
        {
            var cancelled = cancelledByUser || cancellationToken.IsCancellationRequested;
            error.WriteLine(cancelled ? "Compilation cancelled." : "Compilation timed out (cooperative timeout).");
            return cancelled ? 130 : 124;
        }
        catch (Exception exception) when (ValueCommand.IsInputError(exception) || exception is ElmDependencyResolutionException)
        {
            error.WriteLine("Error: " + exception.Message);
            return 1;
        }
        finally
        {
            Console.CancelKeyPress -= handler;
        }
    }
}
