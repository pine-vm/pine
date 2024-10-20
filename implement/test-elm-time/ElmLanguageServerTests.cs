using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm;
using StreamJsonRpc;
using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class ElmLanguageServerTests
{
    [TestMethod]
    public async Task Language_server_reports_capabilities_Async()
    {
        var executablePath = FindPineExecutableFilePath();

        using var lspProcess = Process.Start(new ProcessStartInfo(executablePath)
        {
            Arguments = "  lsp",
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
        }) ?? throw new Exception("Failed starting process");

        try
        {
            var handler =
                new HeaderDelimitedMessageHandler(
                    sendingStream: lspProcess.StandardInput.BaseStream,
                    receivingStream: lspProcess.StandardOutput.BaseStream,
                    formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault());

            using var jsonRpc = new JsonRpc(handler);

            jsonRpc.StartListening();

            var initResponse =
                await jsonRpc.InvokeAsync<InitializeResult>(
                    "initialize",
                    new
                    {
                        processId = Environment.ProcessId,
                    });

            Assert.IsNotNull(initResponse.Capabilities);

            Assert.AreEqual(
                TextDocumentSyncKind.Full,
                initResponse.Capabilities.TextDocumentSync);

            Assert.IsTrue(initResponse.Capabilities.DocumentFormattingProvider);
        }
        finally
        {
            lspProcess.Kill();
        }
    }

    static string FindPineExecutableFilePath()
    {
        /*
         * Navigate from current working directory to the first parent named "implement", then to "pine", then to "pine.exe"
         * Then find the pine executable file in "/pine/ ** /bin/*"
         * */

        var currentDirectory = Directory.GetCurrentDirectory();

        var implementDirectory = new DirectoryInfo(currentDirectory);

        while (implementDirectory.Name != "implement")
        {
            implementDirectory =
                implementDirectory.Parent ?? throw new Exception("Could not find 'implement' directory");
        }

        var pineDirectoryPath = Path.Combine(implementDirectory.FullName, "pine");

        var pineDirectory =
            new FileStoreFromSystemIOFile(pineDirectoryPath);

        /*
         * The executable file name can differ depending on the platform, e.g. "pine.exe" on Windows, "pine" on Linux
         * */

        var allFiles =
            pineDirectory.ListFiles().ToImmutableArray();

        foreach (var fileSubPath in allFiles)
        {
            if (!fileSubPath.Contains("bin"))
                continue;

            var fileName = fileSubPath.Last();

            /*
             * The directory containing the executable file can also contain a file named "pine.pdb",
             * therefore, only accept the file if it has the name "pine" or "pine.exe"
             * */

            if (fileName is "pine" || fileName is "pine.exe")
                return Path.Combine(pineDirectoryPath, string.Join('/', fileSubPath));
        }

        throw new Exception("Could not find 'pine' executable");
    }
}
