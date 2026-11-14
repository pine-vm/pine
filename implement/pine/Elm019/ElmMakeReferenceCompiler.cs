using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Elm019.Testing;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

/// <summary>Replays the reference compiler using only the corpus' archives and registry, never the user's Elm cache.</summary>
public sealed class ElmMakeReferenceCompiler(string testDataDirectory)
{
    public async Task<string> CompileAsync(
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        CancellationToken cancellationToken = default) =>
        ElmMakeErrorSnapshots.SnapshotText(
            expectation,
            await CompileOutcomeAsync(scenario, expectation, cancellationToken));

    public async Task<ElmMakeCompilation> CompileOutcomeAsync(
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        CancellationToken cancellationToken = default)
    {
        RequireSupportedCompilerVersion(expectation);
        var snapshots = new ElmMakePackageSnapshots(testDataDirectory);
        var inputs = await snapshots.PrepareAsync(scenario, expectation, cancellationToken: cancellationToken);
        var registry = snapshots.ReadRegistry();

        foreach (var identity in inputs.Packages.Keys)
        {
            if (!registry.Contains(identity))
            {
                throw new InvalidDataException(
                    $"Registry snapshot '{snapshots.RegistryPath}' is missing {identity}. Run update-packages.");
            }
        }

        var temporary = Path.Combine(Path.GetTempPath(), "pine-elm-reference-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(temporary);

        try
        {
            var app = Path.Combine(temporary, "application");
            var home = Path.Combine(temporary, "elm-home");
            var packages = Path.Combine(home, "0.19.1", "packages");
            var packageRoots = new Dictionary<ElmPackageIdentity, string>();
            await WriteTreeAsync(app, scenario.AppFiles, cancellationToken);

            foreach (var (identity, files) in inputs.Packages)
            {
                var root =
                    Path.Combine(
                        packages,
                        identity.Name.Split('/')[0],
                        identity.Name.Split('/')[1],
                        identity.Version.ToString());

                packageRoots.Add(identity, root);
                await WriteTreeAsync(root, files, cancellationToken);
            }

            Directory.CreateDirectory(packages);
            File.Copy(snapshots.RegistryPath, Path.Combine(packages, "registry.dat"));

            var output =
                Path.Combine(temporary, expectation.OutputType is ElmMakeOutputType.Html ? "output.html" : "output.js");

            var manifestDirectory = Path.Combine([app, .. inputs.ManifestPath.SkipLast(1)]);
            Directory.CreateDirectory(manifestDirectory);

            var source =
                Path.GetRelativePath(manifestDirectory, Path.Combine([app, .. expectation.SourcePath.Split('/')]));

            var start =
                new ProcessStartInfo
                {
                    FileName = ElmMakeRunner.ExecutableFilePath,
                    WorkingDirectory = manifestDirectory,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    CreateNoWindow = true,
                };

            start.ArgumentList.Add("make");
            start.ArgumentList.Add(source);
            start.ArgumentList.Add("--output=" + output);
            start.ArgumentList.Add("--report=json");

            switch (expectation.Flavor)
            {
                case ElmMakeFlavor.Development:
                    break;

                case ElmMakeFlavor.Debug:
                    start.ArgumentList.Add("--debug");
                    break;

                case ElmMakeFlavor.Optimize:
                    start.ArgumentList.Add("--optimize");
                    break;

                default:
                    throw new ArgumentOutOfRangeException(nameof(expectation));
            }

            start.Environment["ELM_HOME"] = home;

            // Reserve a non-listening loopback port: Elm's HTTP client cannot connect or forward traffic.
            // Override both cases and NO_PROXY so inherited proxy/bypass settings cannot reach the registry.
            using var blockedProxy = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            blockedProxy.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            var proxy = "http://127.0.0.1:" + ((IPEndPoint)blockedProxy.LocalEndPoint!).Port;

            foreach (var name in new[] { "http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY", "all_proxy", "ALL_PROXY" })
                start.Environment[name] = proxy;

            start.Environment["no_proxy"] = "";
            start.Environment["NO_PROXY"] = "";

            using var timeout = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            timeout.CancelAfter(TimeSpan.FromMinutes(2));
            using var process = new Process { StartInfo = start };

            if (!process.Start())
                throw new InvalidOperationException("Failed to start Elm 0.19.1 reference compiler.");

            var stdout = process.StandardOutput.ReadToEndAsync(cancellationToken);
            var stderr = process.StandardError.ReadToEndAsync(cancellationToken);

            try
            {
                await process.WaitForExitAsync(timeout.Token);
            }
            catch (OperationCanceledException)
            {
                if (!process.HasExited)
                    process.Kill(entireProcessTree: true);

                await process.WaitForExitAsync(CancellationToken.None);
                await Task.WhenAll(stdout, stderr);
                cancellationToken.ThrowIfCancellationRequested();
                throw new TimeoutException("Elm 0.19.1 reference compilation timed out after two minutes.");
            }

            var standardOutput = await stdout;
            var standardError = await stderr;

            if (process.ExitCode is 1)
            {
                try
                {
                    if (File.Exists(output))
                        throw new InvalidDataException("Reference compiler produced output while reporting errors.");

                    return
                        new ElmMakeCompilation.CompilerErrors(
                            ElmMakeErrorSnapshots.ReadReferenceReport(
                                standardError,
                                app,
                                manifestDirectory,
                                packageRoots));
                }
                catch (Exception exception) when (exception is System.Text.Json.JsonException or InvalidDataException or
                    KeyNotFoundException or InvalidOperationException or FormatException or OverflowException)
                {
                    throw new InvalidDataException(
                        $"Invalid reference compiler failure for '{expectation.RelativePath}': {exception.Message}\n{standardOutput}\n{standardError}",
                        exception);
                }
            }

            if (process.ExitCode is not 0 || !File.Exists(output) || !string.IsNullOrWhiteSpace(standardError))
            {
                throw new InvalidOperationException(
                    $"Elm 0.19.1 reference compilation failed for '{expectation.RelativePath}' (exit {process.ExitCode}):\n" +
                    standardOutput + "\n" + standardError);
            }

            return
                new ElmMakeCompilation.Output(
                    await File.ReadAllTextAsync(output, new UTF8Encoding(false, true), cancellationToken));
        }
        finally
        {
            Directory.Delete(temporary, recursive: true);
        }
    }

    public async Task GenerateAsync(
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        TextWriter output,
        CancellationToken cancellationToken = default)
    {
        var destination = ElmMakeEcmascriptScenarios.ExpectationPath(testDataDirectory, expectation);

        if (File.Exists(destination))
            throw new IOException("Snapshot already exists; remove it manually before generating: " + destination);

        RequireSupportedCompilerVersion(expectation);
        ElmMakeEcmascriptScenarios.ValidateExpectations([.. scenario.Expectations, expectation]);

        var snapshots = new ElmMakePackageSnapshots(testDataDirectory);
        await snapshots.UpdateAsync([scenario with { Expectations = [expectation] }], output, cancellationToken);
        var text = await CompileAsync(scenario, expectation, cancellationToken);
        Directory.CreateDirectory(Path.GetDirectoryName(destination)!);

        using (var stream = new FileStream(destination, FileMode.CreateNew, FileAccess.Write))
        {
            var bytes = new UTF8Encoding(false, true).GetBytes(text);
            await stream.WriteAsync(bytes, cancellationToken);
        }

        await output.WriteLineAsync("Created reference snapshot: " + destination);
    }

    private static void RequireSupportedCompilerVersion(ElmMakeExpectation expectation)
    {
        if (expectation.CompilerVersion != new ElmPackageVersion(0, 19, 1))
        {
            throw new NotSupportedException(
                $"Reference snapshot compilation for Elm {expectation.CompilerVersion} is not supported. Only Elm 0.19.1 is available.");
        }
    }

    private static async Task WriteTreeAsync(string directory, FileTree tree, CancellationToken cancellationToken)
    {
        Directory.CreateDirectory(directory);

        foreach (var (path, content) in tree.EnumerateFilesTransitive())
        {
            ElmMakeEcmascriptScenarios.ValidatePath(path);

            if (path.Any(segment => segment is "elm-stuff" or ".git") || path[^1] is "artifacts.dat")
                continue;

            var destination = Path.Combine([directory, .. path]);
            Directory.CreateDirectory(Path.GetDirectoryName(destination)!);
            await File.WriteAllBytesAsync(destination, content.ToArray(), cancellationToken);
        }
    }
}
