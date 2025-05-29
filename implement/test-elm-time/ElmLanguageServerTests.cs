using Pine;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm;
using StreamJsonRpc;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Xunit;
using FluentAssertions;

namespace TestElmTime;

public class ElmLanguageServerTests
{
    [Fact]
    public async Task Language_server_reports_capabilities_Async()
    {
        var executablePath = FindPineExecutableFilePath();

        using var lspProcess = Process.Start(new ProcessStartInfo(executablePath)
        {
            Arguments = "  lsp  --log-dir=.",
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

            var initParams =
                new InitializeParams(
                    ProcessId: Environment.ProcessId,
                    Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                    RootPath: null,
                    RootUri: null,
                    WorkspaceFolders: [],
                    ClientInfo: null);

            Console.WriteLine(
                "initParams:\n" +
                System.Text.Json.JsonSerializer.Serialize(initParams));

            var initResponse =
                await jsonRpc.InvokeWithParameterObjectAsync<object>(
                    "initialize",
                    initParams);

            Console.WriteLine(
                "initResponse:\n" +
                System.Text.Json.JsonSerializer.Serialize(initResponse));

            /*
             * Example from deno lsp:
             * {"capabilities":{"textDocumentSync":{"openClose":true,"change":2,"save":{}},"selectionRangeProvider":true,"hoverProvider":true,"completionProvider":{"resolveProvider":true,"triggerCharacters":[".","\u0022","\u0027","\u0060","/","@","\u003C","#"],"allCommitCharacters":[".",";","("]},"signatureHelpProvider":{"triggerCharacters":[",","(","\u003C"],"retriggerCharacters":[")"]},"definitionProvider":true,"typeDefinitionProvider":true,"implementationProvider":true,"referencesProvider":true,"documentHighlightProvider":true,"documentSymbolProvider":{"label":"Deno"},"workspaceSymbolProvider":true,"codeActionProvider":true,"codeLensProvider":{"resolveProvider":true},"documentFormattingProvider":true,"renameProvider":true,"foldingRangeProvider":true,"executeCommandProvider":{"commands":["deno.cache","deno.reloadImportRegistries"]},"workspace":{"workspaceFolders":{"supported":true,"changeNotifications":true}},"callHierarchyProvider":true,"semanticTokensProvider":{"legend":{"tokenTypes":["class","enum","interface","namespace","typeParameter","type","parameter","variable","enumMember","property","function","method"],"tokenModifiers":["declaration","static","async","readonly","defaultLibrary","local"]},"range":true,"full":true},"inlayHintProvider":true,"experimental":{"denoConfigTasks":true,"testingApi":true}},"serverInfo":{"name":"deno-language-server","version":"2.0.0 (release, x86_64-pc-windows-msvc)"}}
             * */
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

        while (implementDirectory.Name is not "implement")
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

    public record TextEditTestCase(
        string OriginalText,
        IReadOnlyList<TextEdit> Edits,
        string ExpectedText);

    [Fact]
    public void Apply_text_edits_handles_various_scenarios()
    {
        IReadOnlyList<TextEditTestCase> testCases =
            [
            new(
                OriginalText: "Hello World",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 0),
                        End: new Position(Line: 0, Character: 1)),
                    NewText: "h")],
                ExpectedText: "hello World"
            ),

            // Test case 2: Insert at middle of line
            new(
                OriginalText: "Hello World",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 5),
                        End: new Position(Line: 0, Character: 5)),
                    NewText: " Beautiful")],
                ExpectedText: "Hello Beautiful World"
            ),

            // Test case 3: Replacement at end of line
            new(
                OriginalText: "Hello World",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 6),
                        End: new Position(Line: 0, Character: 11)),
                    NewText: "Pine")],
                ExpectedText: "Hello Pine"
            ),

            // Test case 4: Multi-line text with edits
            new(
                OriginalText: "Line 1\nLine 2\nLine 3",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 1, Character: 5),
                        End: new Position(Line: 1, Character: 6)),
                    NewText: "Two")],
                ExpectedText: "Line 1\nLine Two\nLine 3"
            ),

            // Test case 5: Multi-line edit (spanning lines)
            new(
                OriginalText: "Line 1\nLine 2\nLine 3",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 4),
                        End: new Position(Line: 2, Character: 4)),
                    NewText: " X\nNew Line")],
                ExpectedText: "Line X\nNew Line 3"
            ),

            // Test case 6: Multiple edits in different order (should be applied correctly)
            new(
                OriginalText: "ABCDEF",
                Edits: [
                    new TextEdit(
                        Range: new Pine.Core.LanguageServerProtocol.Range(
                            Start: new Position(Line: 0, Character: 3),
                            End: new Position(Line: 0, Character: 3)),
                        NewText: "X"),
                    new TextEdit(
                        Range: new Pine.Core.LanguageServerProtocol.Range(
                            Start: new Position(Line: 0, Character: 0),
                            End: new Position(Line: 0, Character: 1)),
                        NewText: "Z")
                ],
                ExpectedText: "ZBCXDEF"
            ),

            // Test case 7: Delete only (empty replacement)
            new(
                OriginalText: "Hello Beautiful World",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 5),
                        End: new Position(Line: 0, Character: 15)),
                    NewText: "")],
                ExpectedText: "Hello World"
            ),

            // Test case 8: Insert at start of document
            new(
                OriginalText: "World",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 0),
                        End: new Position(Line: 0, Character: 0)),
                    NewText: "Hello ")],
                ExpectedText: "Hello World"
            ),

            // Test case 9: Insert at end of document
            new(
                OriginalText: "Hello",
                Edits: [new TextEdit(
                    Range: new Pine.Core.LanguageServerProtocol.Range(
                        Start: new Position(Line: 0, Character: 5),
                        End: new Position(Line: 0, Character: 5)),
                    NewText: " World")],
                ExpectedText: "Hello World"
            ),

            // Test case 10: No edits (empty list)
            new(
                OriginalText: "No changes",
                Edits: [],
                ExpectedText: "No changes"
            )
            ];

        for (var testCaseIndex = 0; testCaseIndex < testCases.Count; testCaseIndex++)
        {
            var testCase = testCases[testCaseIndex];

            try
            {
                var result = LanguageServer.ApplyTextEdits(testCase.OriginalText, testCase.Edits);

                result.Should().Be(testCase.ExpectedText);
            }
            catch (Exception ex)
            {
                throw new Exception(
                    "Failed in test case " + testCaseIndex + ":\n" + testCase.OriginalText,
                    innerException: ex);
            }
        }
    }
}
