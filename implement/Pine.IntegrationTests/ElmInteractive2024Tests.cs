using AwesomeAssertions;
using Pine.Core.Files;
using System;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmInteractive2024Tests
{
    public static FileTree CompileElmProgramCodeFiles =>
        Core.Elm.ElmInElm.BundledFiles.CompilerSourceContainerFilesDefault.Value;

    [Fact(Timeout = 1000 * 60 * 8, Skip = "Skip temporary while performance optimizations are WIP")]
    public void First_submission_in_interactive_benefits_from_dynamic_PGO()
    {
        using var dynamicPGOShare = new PineVM.DynamicPGOShare();

        var console = (IConsole)StaticConsole.Instance;

        ElmTime.ElmInteractive.IInteractiveSession newInteractiveSessionFromAppCode(FileTree? appCodeTree) =>
            new ElmTime.ElmInteractive.InteractiveSessionPine2024(
                compilerSourceFiles: CompileElmProgramCodeFiles,
                appCodeTree: appCodeTree,
                overrideSkipLowering: true,
                entryPointsFilePaths: null,
                dynamicPGOShare.GetVMAutoUpdating());

        {
            var warmupStopwatch = System.Diagnostics.Stopwatch.StartNew();

            using var session = newInteractiveSessionFromAppCode(null);

            session.Submit("1 + 3");

            console.WriteLine(
                "Warmup completed in " +
                warmupStopwatch.Elapsed.TotalSeconds.ToString("0.##") + " seconds.");
        }

        {
            /*
             * The scenario with 'String.fromInt' exhibited exponential runtime on the interpreter compared to the optimized version.
             * TODO: Replace with a more challenging scenario that is also particular enough to be covered outside of core libraries and prior training.
             * */

            using var session = newInteractiveSessionFromAppCode(null);

            var submissionStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var submissionResult = session.Submit("[String.fromInt 123, String.fromInt 34567834567]");

            submissionStopwatch.Stop();

            var responseDisplayText =
                submissionResult.Unpack(
                    fromErr: err => throw new Exception(err),
                    fromOk: ok => ok.InteractiveResponse.DisplayText);

            responseDisplayText.Should().Be("""[ "123", "34567834567" ]""");
        }
    }
}
