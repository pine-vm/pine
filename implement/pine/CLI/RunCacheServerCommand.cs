using System;
using System.CommandLine;

namespace Pine.CLI;

public static class RunCacheServerCommand
{
    public static Command Create()
    {
        var command =
            new Command("run-cache-server", "Run an HTTP server to cache popular parts of git repositories.")
            {
                Hidden = true // Equivalent to ShowInHelpText = false
            };

        var gitCloneUrlPrefixOption =
            new Option<string[]>("--git-clone-prefix")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var urlOption =
            new Option<string[]>("--url")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var fileCacheDirectoryOption = new Option<string>("--file-cache-directory");

        command.Add(gitCloneUrlPrefixOption);
        command.Add(urlOption);
        command.Add(fileCacheDirectoryOption);

        command.SetAction(
            (parseResult) =>
            {
                var urls = parseResult.GetValue(urlOption);
                var gitCloneUrlPrefixes = parseResult.GetValue(gitCloneUrlPrefixOption);
                var fileCacheDirectory = parseResult.GetValue(fileCacheDirectoryOption);

                Console.WriteLine("Starting HTTP server with git cache...");

                var serverTask =
                    GitPartialForCommitServer.Run(
                        urls: urls!,
                        gitCloneUrlPrefixes: gitCloneUrlPrefixes!,
                        fileCacheDirectory: fileCacheDirectory);

                Console.WriteLine("Completed starting HTTP server with git cache at '" + string.Join(", ", urls) + "'.");

                serverTask.Wait();

                return 0;
            });

        return command;
    }
}
