using AwesomeAssertions;
using Pine.Core.CodeAnalysis;

namespace Pine.IntegrationTests.CodeAnalysis;

public class CodeAnalysisTestHelper
{

    public static StaticProgram ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnvironment,
                includeDeclaration: includeDeclaration,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        foreach (var decl in declsFailed)
        {
            throw new System.Exception("Failed to parse declaration " + decl.Key.FullName + ": " + decl.Value);
        }

        staticProgram.Should().NotBeNull();

        return staticProgram;
    }
}
