using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text.Json;

namespace TestElmTime
{
    [TestClass]
    public class TestElmEditor
    {
        private static string NormalizeStringTestingElmFormat(string originalString) =>
            originalString.Trim().Replace("\n\r", "\n").Replace("\r\n", "\n");

        [TestMethod]
        public void Elm_editor_backend_support_format_elm_module_text()
        {
            var webAppSource =
                TestSetup.AppConfigComponentFromFiles(
                    TestSetup.GetElmAppFromDirectoryPath(
                        ImmutableList.Create(".", "..", "..", "..", "..", "example-apps", "elm-editor")));

            var elmModuleTextBeforeFormatting = @"
module Common exposing (..)

a =
    let
        b =
            1
        c =
            2
    in
    b   +      c
";

            var expectedElmModuleTextAfterFormatting = @"
module Common exposing (..)


a =
    let
        b =
            1

        c =
            2
    in
    b + c
";
            using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
            using var server = testSetup.StartWebHost();
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            var formatRequest =
                new ElmEditorApi.ElmEditorApiRequestStructure
                (
                    FormatElmModuleTextRequest: ImmutableList.Create(elmModuleTextBeforeFormatting)
                );

            var httpResponse =
                publicAppClient
                .PostAsync("/api", new StringContent(JsonSerializer.Serialize(formatRequest))).Result;

            var responseContentAsString =
                httpResponse.Content.ReadAsStringAsync().Result;

            Assert.AreEqual(
                HttpStatusCode.OK,
                httpResponse.StatusCode,
                "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

            var responseStructure =
                JsonSerializer.Deserialize<ElmEditorApi.ElmEditorApiResponseStructure>(responseContentAsString)!;

            Assert.IsNull(
                responseStructure.ErrorResponse,
                "responseStructure.ErrorResponse should be null.\n" + responseStructure.ErrorResponse);

            Assert.AreEqual(
                NormalizeStringTestingElmFormat(expectedElmModuleTextAfterFormatting),
                NormalizeStringTestingElmFormat(responseStructure
                    ?.FormatElmModuleTextResponse
                    ?.FirstOrDefault()?.formattedText
                    .WithDefaultBuilder(() => throw new ArgumentNullException())!),
                "Response content");
        }
    }

    namespace ElmEditorApi
    {
        public record ElmEditorApiRequestStructure(
            IReadOnlyList<string> FormatElmModuleTextRequest);

        public record ElmEditorApiResponseStructure(
            IReadOnlyList<FormatElmModuleTextResponseStructure>? FormatElmModuleTextResponse = default,
            IReadOnlyList<string>? ErrorResponse = default);

        public record FormatElmModuleTextResponseStructure(
            Maybe<string> formattedText,
            ExecutableFile.ProcessOutput processOutput);
    }
}