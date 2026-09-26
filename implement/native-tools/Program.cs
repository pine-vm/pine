using System.CommandLine;
using System.Threading.Tasks;

namespace native_tools;

public class Program
{
    static async Task<int> Main(string[] args)
    {
        var rootCommand = new RootCommand("Collection of native tools");

        var echoJsonCommand = new Command("echo-json", "Echoes the JSON string to the console.");

        echoJsonCommand.SetAction(
            _ =>
            {
                EchoJson.EchoJsonLoop();
            });

        rootCommand.Subcommands.Add(echoJsonCommand);

        return await rootCommand.Parse(args).InvokeAsync();
    }
}
