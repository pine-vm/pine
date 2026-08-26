using Pine.Core;
using Pine.Core.IO;
using Pine.PineVM;
using System;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using static ElmTime.Platform.WebService.Configuration;

namespace Pine.CLI;

public class PineCliCommand
{
    public static string AppVersionId => "0.5.4";

    internal static int AdminInterfaceDefaultPort => 4000;

    internal static int Invoke(string[] args)
    {
        return MainLessDispose(args, dynamicPGOShare: null);
    }

    private static int MainLessDispose(
        string[] args,
        DynamicPGOShare? dynamicPGOShare)
    {
        LoadFromGitHubOrGitLab.RepositoryFilesPartialForCommitCacheDefault =
            new CacheByFileName(
                new FileStoreFromSystemIOFile(
                    Path.Combine(Filesystem.CacheDirectory, "git", "partial-for-commit", "zip")));

        var rootCommand =
            new RootCommand(
                "Pine: Elm DevTools and runtime\nTo get help or report an issue, see https://github.com/pine-vm/pine/discussions");

        // Custom version option that shows "pine X.X.X" format
        var versionOption =
            new Option<bool>("--version", ["-V"])
            {
                Description =
                "Show version information"
            };

        var verboseOption =
            new Option<bool>("--verbose", ["-v"])
            {
                Description =
                "Use verbose output",
                Recursive = true
            };

        rootCommand.Add(versionOption);
        rootCommand.Add(verboseOption);

        // Install command
        var installCommand = InstallCommand.Create();
        rootCommand.Add(installCommand);

        // Core commands
        rootCommand.Add(SelfTestCommand.Create());
        rootCommand.Add(RunCommand.Create());
        rootCommand.Add(RunServerCommand.Create());
        rootCommand.Add(DeployCommand.Create());
        rootCommand.Add(CopyAppStateCommand.Create());
        rootCommand.Add(CopyProcessCommand.Create());
        rootCommand.Add(ListFunctionsCommand.Create());
        rootCommand.Add(ApplyFunctionCommand.Create());
        rootCommand.Add(TruncateProcessHistoryCommand.Create());
        rootCommand.Add(InteractiveCommand.Create(dynamicPGOShare));
        rootCommand.Add(CompileCommand.Create());
        rootCommand.Add(ElmTestRsCommand.Create());

        rootCommand.Add(ElmCommand.Create());

        rootCommand.Add(MakeCommand.Create());
        rootCommand.Add(ScreenshotCommand.Create());
        rootCommand.Add(Elm.FormatCommand.CreateBackwardCompatible());
        rootCommand.Add(CSharp.CLI.CSharpFormatCommand.CreateCSharpFormatCommand());
        rootCommand.Add(DescribeCommand.Create());
        rootCommand.Add(RunCacheServerCommand.Create());
        rootCommand.Add(RunFileServerCommand.Create());
        rootCommand.Add(CompileInteractiveEnvCommand.Create());
        rootCommand.Add(LangServerCommand.Create());
        rootCommand.Add(UserSecretsCommand.Create());
        rootCommand.Add(HelpCommand.Create(rootCommand));

        // Root command handler (show help when no command specified)
        rootCommand.SetAction(
            (parseResult) =>
            {
                var showVersion = parseResult.GetValue(versionOption);

                if (showVersion)
                {
                    Console.WriteLine("pine " + AppVersionId);
                    return 0;
                }

                // Show help when no command is specified
                HelpCommand.ShowCustomHelp(rootCommand);

                return 0;
            });

        var parseResult = rootCommand.Parse(args);
        return parseResult.Invoke();
    }

    public static void DotNetConsoleWriteLineUsingColor(string line, ConsoleColor color)
    {
        var colorBefore = Console.ForegroundColor;

        Console.ForegroundColor = color;

        Console.WriteLine(line);

        Console.ForegroundColor = colorBefore;
    }

    public static void DotNetConsoleWriteProblemCausingAbort(string line)
    {
        Console.WriteLine("");

        DotNetConsoleWriteLineUsingColor(line, ConsoleColor.Yellow);
    }

    public record ResponseFromServerReport(
        int? statusCode,
        object body);

    internal static async System.Threading.Tasks.Task<(System.Net.Http.HttpResponseMessage httpResponse, string? enteredPassword)>
        AttemptHttpRequest(
        Func<System.Net.Http.HttpRequestMessage> buildRequestBeforeAddingCommonHeaders,
        string? defaultPassword,
        bool promptForPasswordOnConsole)
    {
        System.Net.Http.HttpRequestMessage buildRequest() =>
            AddUserAgentHeader(buildRequestBeforeAddingCommonHeaders());

        using var httpClient = new System.Net.Http.HttpClient();

        httpClient.Timeout = TimeSpan.FromMinutes(4);

        void SetHttpClientPassword(string? password)
        {
            httpClient.DefaultRequestHeaders.Authorization =
                new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(password))));
        }

        SetHttpClientPassword(defaultPassword);

        var httpResponse = await httpClient.SendAsync(buildRequest());

        string? enteredPassword = null;

        if (promptForPasswordOnConsole &&
            httpResponse.StatusCode == System.Net.HttpStatusCode.Unauthorized &&
            httpResponse.Headers.WwwAuthenticate.Any())
        {
            Console.WriteLine(
                "The server at '" + httpResponse.RequestMessage?.RequestUri +
                "' is asking for authentication. Please enter the password we should use to authenticate there:");

            enteredPassword = ReadLine.ReadPassword("> ").Trim();

            Console.WriteLine("I retry using this password...");

            SetHttpClientPassword(enteredPassword);

            httpResponse = await httpClient.SendAsync(buildRequest());
        }

        return (httpResponse, enteredPassword);
    }

    internal static Uri MapUriForForAdminInterface(string uriString)
    {
        if (!Uri.IsWellFormedUriString(uriString, UriKind.Absolute))
        {
            if (!(uriString.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
                uriString.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase)))
            {
                uriString = "http://" + uriString;
            }
        }

        return MapUriForDefaultPort(uriString, AdminInterfaceDefaultPort);
    }

    private static Uri MapUriForDefaultPort(string uriString, int defaultPort)
    {
        var uri = new Uri(uriString);

        if (!uri.Authority.Contains(':'))
            return WithPort(uri, defaultPort);

        return uri;
    }

    public static bool LooksLikeLocalSite(string site)
    {
        if (site.StartsWith(".") || site.StartsWith("/"))
            return true;

        if (Regex.IsMatch(site, "^http(|s)://", RegexOptions.IgnoreCase))
            return false;

        try
        {
            return Directory.Exists(site) || File.Exists(site);
        }
        catch { }

        return false;
    }

    public static Uri WithPort(Uri uri, int newPort)
    {
        var builder =
            new UriBuilder(uri)
            {
                Port = newPort
            };

        return builder.Uri;
    }

    internal static (string commandName, Func<(bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath)> checkInstallation)
        CheckIfExecutableIsRegisteredOnPath()
    {
        var environmentVariableName = "PATH";

        var environmentVariableScope = EnvironmentVariableTarget.User;

        string? getCurrentValueOfEnvironmentVariable() =>
            Environment.GetEnvironmentVariable(environmentVariableName, environmentVariableScope);

        var executableFilePath = GetCurrentProcessExecutableFilePath()!;

        var executableDirectoryPath = Path.GetDirectoryName(executableFilePath);

        var commandName = Regex.Match(Path.GetFileName(executableFilePath)!, @"(.+?)(?=\.exe$|$)").Groups[1].Value;

        (bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath) checkInstallation()
        {
            if (System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
                System.Runtime.InteropServices.OSPlatform.Windows))
            {
                var executableIsRegisteredOnPath =
                    (getCurrentValueOfEnvironmentVariable() ?? "")
                    .Split(Path.PathSeparator).Contains(executableDirectoryPath);

                var registerExecutableForCurrentUser =
                    new Action(
                        () =>
                        {
                            var newValueForPathEnv =
                                executableDirectoryPath +
                                Path.PathSeparator +
                                getCurrentValueOfEnvironmentVariable();

                            Environment.SetEnvironmentVariable(
                                environmentVariableName,
                                newValueForPathEnv,
                                environmentVariableScope);

                            //  https://stackoverflow.com/questions/32650063/get-environment-variable-out-of-new-process-in-c-sharp/32650213#32650213
                            //  https://devblogs.microsoft.com/oldnewthing/?p=91591
                            //  https://docs.microsoft.com/en-us/previous-versions//cc723564(v=technet.10)?redirectedfrom=MSDN#XSLTsection127121120120

                            Console.WriteLine(
                                "I added the path '" + executableDirectoryPath + "' to the '" + environmentVariableName +
                                "' environment variable for the current user account. You will be able to use the '" +
                                commandName +
                                "' command in newer instances of the Command Prompt.");
                        });

                return (executableIsRegisteredOnPath, registerExecutableForCurrentUser);
            }
            else
            {
                var destinationExecutableFilePath = "/usr/local/bin/" + commandName;

                byte[]? currentRegisteredFileContent = null;

                if (File.Exists(destinationExecutableFilePath))
                {
                    currentRegisteredFileContent = File.ReadAllBytes(destinationExecutableFilePath);
                }

                var currentExecuableFileContent = File.ReadAllBytes(executableFilePath);

                var executableIsRegisteredOnPath =
                    currentRegisteredFileContent != null &&
                    currentRegisteredFileContent.SequenceEqual(currentExecuableFileContent);

                var registerExecutableForCurrentUser =
                    new Action(
                        () =>
                        {
                            ExecutableFile.CreateAndWriteFileToPath(
                                destinationExecutableFilePath,
                                currentExecuableFileContent,
                                makeExecutable: true);

                            Console.WriteLine(
                                "I copied the executable file to '" + destinationExecutableFilePath +
                                "'. You will be able to use the '" +
                                commandName +
                                "' command in newer terminal instances.");
                        });

                return (executableIsRegisteredOnPath, registerExecutableForCurrentUser);
            }
        }

        return (commandName, checkInstallation);
    }

    internal static string ReportFilePath => Path.Combine(Environment.CurrentDirectory, "pine-tool", "report");

    internal static readonly System.Text.Json.JsonSerializerOptions ReportJsonSerializerOptions =
        new()
        {
            WriteIndented = true
        };

    internal static void WriteReportToFileInReportDirectory(string reportContent, string reportKind)
    {
        var fileName = BytesConversions.TimeStringViewForReport(s_programStartTime) + "_" + reportKind;

        var filePath = Path.Combine(ReportFilePath, fileName);

        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

        File.WriteAllBytes(filePath, Encoding.UTF8.GetBytes(reportContent));

        Console.WriteLine("Saved report to file '" + filePath + "'.");
    }

    private static string? GetCurrentProcessExecutableFilePath() =>
        System.Diagnostics.Process.GetCurrentProcess().MainModule?.FileName;

    private static System.Net.Http.HttpRequestMessage AddUserAgentHeader(
        System.Net.Http.HttpRequestMessage httpRequest)
    {
        httpRequest.Headers.UserAgent.Add(
            new System.Net.Http.Headers.ProductInfoHeaderValue(
                new System.Net.Http.Headers.ProductHeaderValue("pine-cli", AppVersionId)));

        return httpRequest;
    }

    private static readonly DateTimeOffset s_programStartTime = DateTimeOffset.UtcNow;

}
