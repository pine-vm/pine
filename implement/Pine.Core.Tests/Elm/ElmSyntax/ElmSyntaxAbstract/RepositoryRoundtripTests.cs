using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

public class RepositoryRoundtripTests
{
    [Fact]
    public void All_parsable_elm_files_roundtrip_through_concrete_syntax()
    {
        var repositoryRoot = FindRepositoryRoot();
        var failures = new List<string>();

        foreach (var filePath in
            Directory.EnumerateFiles(repositoryRoot, "*.elm", SearchOption.AllDirectories)
            .Order(StringComparer.Ordinal))
        {
            var relativePath = Path.GetRelativePath(repositoryRoot, filePath);
            var parseResult = ElmSyntaxParser.ParseModuleText(File.ReadAllText(filePath));

            if (parseResult.IsOkOrNull() is not { } concreteFile)
            {
                continue;
            }

            if (concreteFile.IncompleteDeclarations.Count is not 0)
            {
                continue;
            }

            var roundtripStage = "convert parsed concrete syntax to the first abstract representation";

            try
            {
                var firstAbstract = Abstract.ConvertFromConcrete.FromFile(concreteFile);

                roundtripStage = "convert and render the first abstract representation";

                var rendered =
                    ElmSyntaxAbstractTestHelper.RenderModuleForSyntaxRoundtrip(firstAbstract);

                roundtripStage = "parse the rendered concrete syntax";

                var reparsedConcrete =
                    ElmSyntaxParser.ParseModuleText(rendered)
                    .Extract(error => throw new Exception(error + "\n\n" + rendered));

                if (reparsedConcrete.IncompleteDeclarations.Count is not 0)
                {
                    throw new Exception("Rendered syntax contains incomplete declarations:\n\n" + rendered);
                }

                roundtripStage = "convert the reparsed concrete syntax to the second abstract representation";
                var secondAbstract = Abstract.ConvertFromConcrete.FromFile(reparsedConcrete);

                var normalizedFirst =
                    ElmSyntaxAbstractTestHelper.NormalizeForSemanticComparison(firstAbstract);

                var normalizedSecond =
                    ElmSyntaxAbstractTestHelper.NormalizeForSemanticComparison(secondAbstract);

                if (!normalizedFirst.Equals(normalizedSecond))
                {
                    throw new Exception(DescribeFirstDifference(normalizedFirst, normalizedSecond));
                }
            }
            catch (Exception exception)
            {
                failures.Add(
                    relativePath + ": Failed to " + roundtripStage + " (" +
                    exception.GetType().Name + "): " + exception.Message);
            }
        }

        failures.Should().BeEmpty(
            "every parsable Elm file should roundtrip through the abstract and concrete syntax models:\n" +
            string.Join("\n", failures));
    }

    private static string DescribeFirstDifference(Abstract.File first, Abstract.File second)
    {
        var firstJson = Abstract.ElmSyntaxAbstractJson.FileToJsonString(first);
        var secondJson = Abstract.ElmSyntaxAbstractJson.FileToJsonString(second);
        var commonLength = 0;

        while (commonLength < firstJson.Length &&
            commonLength < secondJson.Length &&
            firstJson[commonLength] == secondJson[commonLength])
        {
            commonLength++;
        }

        const int contextLength = 120;
        var contextStart = Math.Max(0, commonLength - contextLength);

        return
            "Abstract syntax differs at JSON offset " + commonLength +
            ". First: " +
            firstJson.Substring(contextStart, Math.Min(contextLength * 2, firstJson.Length - contextStart)) +
            ". Second: " +
            secondJson.Substring(contextStart, Math.Min(contextLength * 2, secondJson.Length - contextStart));
    }

    private static string FindRepositoryRoot()
    {
        foreach (var startingPath in
            new[] { Directory.GetCurrentDirectory(), AppContext.BaseDirectory }.Distinct())
        {
            for (var directory = new DirectoryInfo(startingPath);
                directory is not null;
                directory = directory.Parent)
            {
                if (File.Exists(Path.Combine(directory.FullName, "pine.slnx")))
                {
                    return directory.FullName;
                }
            }
        }

        throw new DirectoryNotFoundException("Could not find the repository root containing pine.slnx.");
    }
}
