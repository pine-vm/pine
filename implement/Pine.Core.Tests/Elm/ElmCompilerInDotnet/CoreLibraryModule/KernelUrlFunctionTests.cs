using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class KernelUrlFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Url.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        kernelModulesTree,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling elm-kernel-modules: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));
            });

    private static PineValue GetUrlFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Url")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static readonly ElmValue s_http =
        ElmValue.TagInstance("Http", []);

    private static readonly ElmValue s_https =
        ElmValue.TagInstance("Https", []);

    /// <summary>
    /// Creates a Url record with fields in alphabetical order.
    /// </summary>
    private static ElmValue UrlRecord(
        ElmValue protocol,
        string host,
        ElmValue port_,
        string path,
        ElmValue query,
        ElmValue fragment) =>
        new ElmValue.ElmRecord(
            [
            ("fragment", fragment),
            ("host", String(host)),
            ("path", String(path)),
            ("port_", port_),
            ("protocol", protocol),
            ("query", query),
            ]);

    // ========== Tests for fromString ==========
    // fromString "https://example.com:443"
    //   == Just { protocol = Https, host = "example.com", port_ = Just 443, path = "/", query = Nothing, fragment = Nothing }

    [Fact]
    public void FromString_https_with_port()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("https://example.com:443"));

        result.Should().Be(
            JustOf(UrlRecord(s_https, "example.com", JustOf(Integer(443)), "/", s_nothing, s_nothing)));
    }

    // fromString "https://example.com/hats?q=top%20hat"
    //   == Just { protocol = Https, host = "example.com", port_ = Nothing, path = "/hats", query = Just "q=top%20hat", fragment = Nothing }

    [Fact]
    public void FromString_https_with_query()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("https://example.com/hats?q=top%20hat"));

        result.Should().Be(
            JustOf(UrlRecord(s_https, "example.com", s_nothing, "/hats", JustOf(String("q=top%20hat")), s_nothing)));
    }

    // fromString "http://example.com/core/List/#map"
    //   == Just { protocol = Http, host = "example.com", port_ = Nothing, path = "/core/List/", query = Nothing, fragment = Just "map" }

    [Fact]
    public void FromString_http_with_fragment()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("http://example.com/core/List/#map"));

        result.Should().Be(
            JustOf(UrlRecord(s_http, "example.com", s_nothing, "/core/List/", s_nothing, JustOf(String("map")))));
    }

    // fromString "example.com:443" == Nothing  (no protocol)

    [Fact]
    public void FromString_no_protocol()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("example.com:443"));

        result.Should().Be(s_nothing);
    }

    // fromString "http://tom@example.com" == Nothing  (userinfo disallowed)

    [Fact]
    public void FromString_userinfo_disallowed()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("http://tom@example.com"));

        result.Should().Be(s_nothing);
    }

    // fromString "http://#cats" == Nothing  (no host)

    [Fact]
    public void FromString_no_host()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("http://#cats"));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void FromString_simple_http()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("http://example.com"));

        result.Should().Be(
            JustOf(UrlRecord(s_http, "example.com", s_nothing, "/", s_nothing, s_nothing)));
    }

    [Fact]
    public void FromString_https_with_path()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("https://example.com/a/b/c"));

        result.Should().Be(
            JustOf(UrlRecord(s_https, "example.com", s_nothing, "/a/b/c", s_nothing, s_nothing)));
    }

    [Fact]
    public void FromString_all_parts()
    {
        var result =
            ApplyUnary(GetUrlFunction("fromString"), String("https://example.com:8042/over/there?name=ferret#nose"));

        result.Should().Be(
            JustOf(
                UrlRecord(
                    s_https,
                    "example.com",
                    JustOf(Integer(8042)),
                    "/over/there",
                    JustOf(String("name=ferret")),
                    JustOf(String("nose")))));
    }

    [Fact]
    public void FromString_empty_string()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String(""));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void FromString_http_empty_after_protocol()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("http://"));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void FromString_ftp_protocol()
    {
        var result = ApplyUnary(GetUrlFunction("fromString"), String("ftp://example.com"));

        result.Should().Be(s_nothing);
    }

    // ========== Tests for toString ==========
    // toString is the inverse of fromString for valid URLs

    [Fact]
    public void ToString_simple_https()
    {
        var url = UrlRecord(s_https, "example.com", s_nothing, "/", s_nothing, s_nothing);

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("https://example.com/"));
    }

    [Fact]
    public void ToString_with_port()
    {
        var url = UrlRecord(s_https, "example.com", JustOf(Integer(443)), "/", s_nothing, s_nothing);

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("https://example.com:443/"));
    }

    [Fact]
    public void ToString_with_path()
    {
        var url = UrlRecord(s_https, "example.com", s_nothing, "/hats", s_nothing, s_nothing);

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("https://example.com/hats"));
    }

    [Fact]
    public void ToString_with_query()
    {
        var url = UrlRecord(s_https, "example.com", s_nothing, "/hats", JustOf(String("q=top%20hat")), s_nothing);

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("https://example.com/hats?q=top%20hat"));
    }

    [Fact]
    public void ToString_with_fragment()
    {
        var url = UrlRecord(s_http, "example.com", s_nothing, "/core/List/", s_nothing, JustOf(String("map")));

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("http://example.com/core/List/#map"));
    }

    [Fact]
    public void ToString_all_parts()
    {
        var url =
            UrlRecord(
                s_https,
                "example.com",
                JustOf(Integer(8042)),
                "/over/there",
                JustOf(String("name=ferret")),
                JustOf(String("nose")));

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("https://example.com:8042/over/there?name=ferret#nose"));
    }

    [Fact]
    public void ToString_http_simple()
    {
        var url = UrlRecord(s_http, "localhost", s_nothing, "/", s_nothing, s_nothing);

        var result = ApplyUnary(GetUrlFunction("toString"), url);

        result.Should().Be(String("http://localhost/"));
    }

    // ========== Tests for percentEncode ==========
    // percentEncode "hat" == "hat"
    // percentEncode "to be" == "to%20be"
    // percentEncode "99%" == "99%25"
    // percentEncode "$" == "%24"
    // percentEncode "¢" == "%C2%A2"
    // percentEncode "€" == "%E2%82%AC"

    [Fact]
    public void PercentEncode_hat()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("hat"));

        result.Should().Be(String("hat"));
    }

    [Fact]
    public void PercentEncode_space()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("to be"));

        result.Should().Be(String("to%20be"));
    }

    [Fact]
    public void PercentEncode_percent()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("99%"));

        result.Should().Be(String("99%25"));
    }

    [Fact]
    public void PercentEncode_dollar()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("$"));

        result.Should().Be(String("%24"));
    }

    [Fact]
    public void PercentEncode_cent()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("¢"));

        result.Should().Be(String("%C2%A2"));
    }

    [Fact]
    public void PercentEncode_euro()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("€"));

        result.Should().Be(String("%E2%82%AC"));
    }

    [Fact]
    public void PercentEncode_empty()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String(""));

        result.Should().Be(String(""));
    }

    [Fact]
    public void PercentEncode_unreserved_chars()
    {
        var result = ApplyUnary(GetUrlFunction("percentEncode"), String("abc123-_.~"));

        result.Should().Be(String("abc123-_.~"));
    }

    // ========== Tests for percentDecode ==========
    // percentDecode "hat" == Just "hat"
    // percentDecode "to%20be" == Just "to be"
    // percentDecode "99%25" == Just "99%"
    // percentDecode "%24" == Just "$"
    // percentDecode "%C2%A2" == Just "¢"
    // percentDecode "%E2%82%AC" == Just "€"
    // percentDecode "%" == Nothing
    // percentDecode "%XY" == Nothing
    // percentDecode "%C2" == Nothing

    [Fact]
    public void PercentDecode_hat()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("hat"));

        result.Should().Be(JustOf(String("hat")));
    }

    [Fact]
    public void PercentDecode_space()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("to%20be"));

        result.Should().Be(JustOf(String("to be")));
    }

    [Fact]
    public void PercentDecode_percent()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("99%25"));

        result.Should().Be(JustOf(String("99%")));
    }

    [Fact]
    public void PercentDecode_dollar()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%24"));

        result.Should().Be(JustOf(String("$")));
    }

    [Fact]
    public void PercentDecode_cent()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%C2%A2"));

        result.Should().Be(JustOf(String("¢")));
    }

    [Fact]
    public void PercentDecode_euro()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%E2%82%AC"));

        result.Should().Be(JustOf(String("€")));
    }

    [Fact]
    public void PercentDecode_incomplete_percent()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%"));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void PercentDecode_invalid_hex()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%XY"));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void PercentDecode_incomplete_utf8()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String("%C2"));

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void PercentDecode_empty()
    {
        var result = ApplyUnary(GetUrlFunction("percentDecode"), String(""));

        result.Should().Be(JustOf(String("")));
    }
}
