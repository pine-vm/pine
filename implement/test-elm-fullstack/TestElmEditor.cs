using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net;
using System.Net.Http;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Pine;

namespace test_elm_fullstack
{
    [TestClass]
    public class TestElmEditor
    {
        static string NormalizeStringTestingElmFormat(string originalString) =>
            originalString?.Trim()?.Replace("\n\r", "\n")?.Replace("\r\n", "\n");

        [TestMethod]
        [Ignore("Depend on assembly from CI build")]
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
            using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: webAppSource);
            using var server = testSetup.StartWebHost();
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();

            var formatRequest =
                new ElmEditorApi.ElmEditorApiRequestStructure
                {
                    FormatElmModuleTextRequest = ImmutableList.Create(elmModuleTextBeforeFormatting)
                };

            var httpResponse =
                publicAppClient
                .PostAsync("/api", new StringContent(JsonConvert.SerializeObject(formatRequest))).Result;

            var responseContentAsString =
                httpResponse.Content.ReadAsStringAsync().Result;

            Assert.AreEqual(
                HttpStatusCode.OK,
                httpResponse.StatusCode,
                "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

            var responseStructure =
                JsonConvert.DeserializeObject<ElmEditorApi.ElmEditorApiResponseStructure>(responseContentAsString);

            Assert.IsNull(
                responseStructure.ErrorResponse,
                "responseStructure.ErrorResponse should be null.\n" + responseStructure.ErrorResponse);

            Assert.AreEqual(
                NormalizeStringTestingElmFormat(expectedElmModuleTextAfterFormatting),
                NormalizeStringTestingElmFormat(responseStructure
                    ?.FormatElmModuleTextResponse?.FirstOrDefault()
                    ?.formattedText.Just?.FirstOrDefault()),
                "Response content");
        }
    }

    namespace ElmEditorApi
    {
        public class ElmEditorApiRequestStructure
        {
            public IReadOnlyList<string> FormatElmModuleTextRequest;
        }

        public class ElmEditorApiResponseStructure
        {
            public IReadOnlyList<FormatElmModuleTextResponseStructure> FormatElmModuleTextResponse;

            public IReadOnlyList<string> ErrorResponse;
        }

        public class FormatElmModuleTextResponseStructure
        {
            public ElmFullstack.ElmValueCommonJson.Maybe<string> formattedText;

            public ExecutableFile.ProcessOutput processOutput;
        }
    }
}