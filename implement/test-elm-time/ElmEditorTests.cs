using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text.Json;

namespace TestElmTime
{
    [TestClass]
    public class ElmEditorTests
    {
        private static string NormalizeStringTestingElmFormat(string originalString) =>
            originalString.Trim().Replace("\n\r", "\n").Replace("\r\n", "\n");

        [TestMethod]
        public void Elm_editor_backend_support_format_elm_module_text()
        {
            var webAppSource =
                ExampleAppsTests.ExampleAppValueFromExampleName("elm-editor");

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
                    FormatElmModuleTextRequest: [elmModuleTextBeforeFormatting]
                );

            var httpResponse =
                publicAppClient
                .PostAsync("/api", new StringContent(JsonSerializer.Serialize(formatRequest))).Result;

            var responseContentAsString =
                httpResponse.Content.ReadAsStringAsync().Result;

            httpResponse.StatusCode.Should().Be(
                HttpStatusCode.OK,
                "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

            var responseStructure =
                JsonSerializer.Deserialize<ElmEditorApi.ElmEditorApiResponseStructure>(responseContentAsString)!;

            responseStructure.ErrorResponse.Should().BeNull(
                "responseStructure.ErrorResponse should be null.\n" + responseStructure.ErrorResponse);

            NormalizeStringTestingElmFormat(responseStructure
                ?.FormatElmModuleTextResponse
                ?.FirstOrDefault()?.formattedText
                .WithDefaultBuilder(() => throw new ArgumentNullException())!).Should().Be(
                NormalizeStringTestingElmFormat(expectedElmModuleTextAfterFormatting),
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