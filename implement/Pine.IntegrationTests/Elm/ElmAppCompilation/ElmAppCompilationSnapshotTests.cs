using AwesomeAssertions;
using ElmTime;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Elm019;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.Elm.ElmAppCompilation;

public class ElmAppCompilationSnapshotTests
{
    /*
     * Test cases and snapshots are modelled after the reference implementation found at
     * https://github.com/pine-vm/pine/blob/6941eea326703c4c3a66ffa143440849b2141561/implement/Pine.Core/Elm/elm-in-elm/src/CompileElmApp.elm
     * 
     * While the Elm-based implementation works, we tempararily replace it to ease the transition to the 2026 Elm compiler and Pine language.
     * */

    [Fact]
    public void ElmAppCompilation_Run_all_snapshots()
    {
        var parseCache = new PineVMParseCache();

        var results =
            TestResultSummary.RunFileBasedTestCases(
                "ElmAppCompilationSnapshot",
                caseDir =>
                {
                    var sourceDir = System.IO.Path.Combine(caseDir, "source");

                    var sourceFiles = GetElmAppFromDirectoryPath(sourceDir);

                    var compiledFiles = Compile(sourceFiles);

                    var expectedDir = System.IO.Path.Combine(caseDir, "expected");

                    var overwrittenFiles = GetElmAppFromDirectoryPath(expectedDir);

                    // Merge expected overwritten files into source files
                    var expectedFiles =
                    overwrittenFiles.Aggregate(
                        seed: sourceFiles,
                        func: (files, next) => files.SetItem(next.Key, next.Value));

                    var differences =
                    DescribeFilesDifferences(
                        actual: FilterForElmModules(compiledFiles),
                        expected: FilterForElmModules(expectedFiles));

                    if (differences is not null)
                    {
                        // Dump an archive containing all files that need to be in the 'expected'
                        // directory to make this scenario pass, to ease updating the snapshot.
                        var archivePath =
                            WriteMismatchArchive(
                                caseName: System.IO.Path.GetFileName(caseDir),
                                filesToCopy: FilterForElmModules(compiledFiles));

                        differences += "\nWrote mismatch archive to: " + archivePath;
                    }

                    differences.Should().BeNull(differences);

                    return (expected: "pass", actual: "pass");
                },
                trimWhitespace: true);

        var summary = TestResultSummary.RenderSummary(results);

        results.Where(r => !r.Passed).Should().BeEmpty(summary);
    }

    [Fact]
    public void Render_differences_found_comparing_files()
    {
        var testCases =
            new[]
            {
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("World").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory()),

                    expectedDifferences=
                    "1 missing file comparing 2 expected files to 1 actual files:\n" +
                    "Missing file: b.txt"
                },

                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("World").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("World!").AsMemory()),

                    expectedDifferences=
                    """
                    1 file with different content comparing 2 expected files to 2 actual files:
                    Different content: b.txt (expected: 5 bytes, actual: 6 bytes)
                    Text contents in file b.txt differ at char index 5:
                          ↓ (actual)
                    "World!"
                    <vs>
                    "World"
                          ↑ (expected)
                    """
                },

                // Only an unexpected file.
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
                    .Add(["c.txt"], System.Text.Encoding.UTF8.GetBytes("Extra").AsMemory()),

                    expectedDifferences=
                    "1 unexpected file comparing 1 expected files to 2 actual files:\n" +
                    "Unexpected file: c.txt"
                },

                // Combination of a missing file, a file with different content and an unexpected file.
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("World").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hallo").AsMemory())
                    .Add(["c.txt"], System.Text.Encoding.UTF8.GetBytes("Extra").AsMemory()),

                    expectedDifferences =
                    """
                    1 missing file, 1 file with different content, 1 unexpected file comparing 2 expected files to 2 actual files:
                    Missing file: b.txt
                    Different content: a.txt (expected: 5 bytes, actual: 5 bytes)
                    Text contents in file a.txt differ at char index 1:
                      ↓ (actual)
                    "Hallo"
                    <vs>
                    "Hello"
                      ↑ (expected)
                    Unexpected file: c.txt
                    """
                },

                // Multiple missing and multiple unexpected files (plural wording).
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("A").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("B").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["x.txt"], System.Text.Encoding.UTF8.GetBytes("X").AsMemory())
                    .Add(["y.txt"], System.Text.Encoding.UTF8.GetBytes("Y").AsMemory()),

                    expectedDifferences=
                    "2 missing files, 2 unexpected files comparing 2 expected files to 2 actual files:\n" +
                    "Missing file: a.txt\n" +
                    "Missing file: b.txt\n" +
                    "Unexpected file: x.txt\n" +
                    "Unexpected file: y.txt"
                },

                // Multiple files with different content (plural wording), differing mid-string.
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("cat").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("dog").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("car").AsMemory())
                    .Add(["b.txt"], System.Text.Encoding.UTF8.GetBytes("dig").AsMemory()),

                    expectedDifferences=
                    """
                    2 files with different content comparing 2 expected files to 2 actual files:
                    Different content: a.txt (expected: 3 bytes, actual: 3 bytes)
                    Text contents in file a.txt differ at char index 2:
                       ↓ (actual)
                    "car"
                    <vs>
                    "cat"
                       ↑ (expected)
                    Different content: b.txt (expected: 3 bytes, actual: 3 bytes)
                    Text contents in file b.txt differ at char index 1:
                      ↓ (actual)
                    "dig"
                    <vs>
                    "dog"
                      ↑ (expected)
                    """
                },

                // Missing file located in a subdirectory (path elements joined with '/').
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["src", "Main.elm"], System.Text.Encoding.UTF8.GetBytes("module Main").AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty,

                    expectedDifferences=
                    "1 missing file comparing 1 expected files to 0 actual files:\n" +
                    "Missing file: src/Main.elm"
                },

                // Binary file (not valid UTF-8 or UTF-16, or UTF-32) with differing content.
                new
                {
                    expected=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["data.bin"], new byte[] { 0x00, 0x00, 0x01, 0xFF, 0xFF }.AsMemory()),

                    actual=
                    ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
                    .Add(["data.bin"], new byte[] { 0x00, 0x00, 0x01, 0xFE, 0x10 }.AsMemory()),

                    expectedDifferences=
                    "1 file with different content comparing 1 expected files to 1 actual files:\n" +
                    "Different content: data.bin (expected: 5 bytes, actual: 5 bytes)\n" +
                    "Binary contents in file data.bin differ at byte index 3: expected 0xFF, actual 0xFE"
                }
            };

        foreach (var testCase in testCases)
        {
            var differences =
                DescribeFilesDifferences(
                    actual: testCase.actual,
                    expected: testCase.expected);

            differences.Should().Be(testCase.expectedDifferences);
        }

        // Identical sets of files must not produce any differences.
        var identicalFiles =
            ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
            .Add(["a.txt"], System.Text.Encoding.UTF8.GetBytes("Hello").AsMemory())
            .Add(["src", "Main.elm"], System.Text.Encoding.UTF8.GetBytes("module Main").AsMemory());

        DescribeFilesDifferences(
            actual: identicalFiles,
            expected: identicalFiles)
            .Should().BeNull();
    }

    private static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> FilterForElmModules(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files)
    {
        return files.Where(f => f.Key.Last().EndsWith(".elm")).ToImmutableDictionary();
    }

    /// <summary>
    /// Writes the given files into a zip archive whose name contains the scenario/case directory
    /// name and the marker <c>__mismatched__</c>. The archive holds all files that need to be in
    /// the case's <c>expected</c> directory to make the scenario pass, and can be extracted there.
    /// </summary>
    /// <param name="caseName">The scenario/case directory name.</param>
    /// <param name="filesToCopy">
    /// All files that need to be in the <c>expected</c> directory, keyed by their path relative to
    /// that directory.
    /// </param>
    /// <returns>The full path of the written zip archive.</returns>
    private static string WriteMismatchArchive(
        string caseName,
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> filesToCopy)
    {
        var outputDir =
            System.IO.Path.Combine(AppContext.BaseDirectory, "snapshot-mismatches");

        System.IO.Directory.CreateDirectory(outputDir);

        var timestamp =
            DateTimeOffset.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss", System.Globalization.CultureInfo.InvariantCulture);

        var archivePath =
            System.IO.Path.Combine(outputDir, caseName + "__mismatched__" + timestamp + ".zip");

        using var stream =
            new System.IO.FileStream(archivePath, System.IO.FileMode.Create, System.IO.FileAccess.Write);

        using var archive = new System.IO.Compression.ZipArchive(stream, System.IO.Compression.ZipArchiveMode.Create);

        var filesOrdered =
            filesToCopy
            .OrderBy(kv => kv.Key, EnumerableExtensions.Comparer<IReadOnlyList<string>>());

        foreach (var file in filesOrdered)
        {
            var entry = archive.CreateEntry(string.Join("/", file.Key));

            using var entryStream = entry.Open();

            entryStream.Write(file.Value.Span);
        }

        return archivePath;
    }

    private static string? DescribeFilesDifferences(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> actual,
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> expected)
    {
        var missingFiles = new List<string>();

        var differentContentFiles = new List<string>();

        var unexpectedFiles = new List<string>();

        // Rebuild both dictionaries with a structural path comparer so that keys are
        // matched by their contents rather than by reference identity.

        var expectedWithComparer =
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                expected.Select(kv => (kv.Key, kv.Value)));

        var actualWithComparer =
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                actual.Select(kv => (kv.Key, kv.Value)));

        var expectedKeysOrdered =
            expectedWithComparer.Keys
            .OrderBy(k => k, EnumerableExtensions.Comparer<IReadOnlyList<string>>())
            .ToList();

        foreach (var expectedKey in expectedKeysOrdered)
        {
            var expectedFile = expectedWithComparer[expectedKey];

            var pathText = string.Join("/", expectedKey);

            if (!actualWithComparer.TryGetValue(expectedKey, out var actualFile))
            {
                missingFiles.Add("Missing file: " + pathText);
                continue;
            }

            if (DescribeFileDifferences(actualFile, expectedFile, pathText) is string fileDifferences)
            {
                differentContentFiles.Add(fileDifferences);
            }
        }

        var unexpectedKeysOrdered =
            actualWithComparer.Keys
            .Where(k => !expectedWithComparer.ContainsKey(k))
            .OrderBy(k => k, EnumerableExtensions.Comparer<IReadOnlyList<string>>())
            .ToList();

        foreach (var key in unexpectedKeysOrdered)
        {
            unexpectedFiles.Add("Unexpected file: " + string.Join("/", key));
        }

        if (missingFiles.Count + differentContentFiles.Count + unexpectedFiles.Count is 0)
            return null;

        var summaryParts = new List<string>();

        if (missingFiles.Count > 0)
        {
            summaryParts.Add(
                missingFiles.Count + " missing file" + (missingFiles.Count is 1 ? "" : "s"));
        }

        if (differentContentFiles.Count > 0)
        {
            summaryParts.Add(
                differentContentFiles.Count + (differentContentFiles.Count is 1 ? " file" : " files") +
                " with different content");
        }

        if (unexpectedFiles.Count > 0)
        {
            summaryParts.Add(
                unexpectedFiles.Count + " unexpected file" + (unexpectedFiles.Count is 1 ? "" : "s"));
        }

        var header =
            string.Join(", ", summaryParts) +
            " comparing " + expected.Count + " expected files to " + actual.Count + " actual files:";

        var bodyLines =
            missingFiles
            .Concat(differentContentFiles)
            .Concat(unexpectedFiles);

        return header + "\n" + string.Join("\n", bodyLines);
    }

    private static string? DescribeFileDifferences(
        ReadOnlyMemory<byte> actual,
        ReadOnlyMemory<byte> expected,
        string pathText)
    {
        if (MemoryExtensions.SequenceEqual(actual.Span, expected.Span))
            return null;

        var header =
            "Different content: " + pathText +
            " (expected: " + expected.Length + " bytes, actual: " + actual.Length + " bytes)";

        var actualText = TryViewAsText(actual);

        var expectedText = TryViewAsText(expected);

        if (actualText is null || expectedText is null)
        {
            // Find index of first byte that differs
            for (var i = 0; i < Math.Min(actual.Length, expected.Length); i++)
            {
                if (actual.Span[i] != expected.Span[i])
                {
                    return
                        header + "\n" +
                        "Binary contents in file " + pathText + " differ at byte index " + i + ": " +
                        $"expected 0x{expected.Span[i]:X2}, actual 0x{actual.Span[i]:X2}";
                }
            }

            return
                header + "\n" +
                "Binary contents in file " + pathText + " differ: one file is a prefix of the other.";
        }

        if (actualText == expectedText)
        {
            return
                header + "\n" +
                "Text contents in file " + pathText + " differ only in encoding (e.g., UTF-8 vs UTF-16).";
        }

        if (RenderTextDifference(actualText: actualText, expectedText: expectedText, pathText: pathText) is not { } textDifference)
        {
            throw new NotImplementedException(
                "Could not render text difference for file " + pathText +
                " even though the contents are known to differ.");
        }

        return header + "\n" + textDifference;
    }

    private static string RenderTextDifference(
        string actualText,
        string expectedText,
        string pathText)
    {
        var diffIndex = Testing.GetFirstStringDifferenceIndex(actualText, expectedText);

        return
            "Text contents in file " + pathText + " differ at char index " + diffIndex + ":\n" +
            Testing.RenderCoreStringDifference(
                actualText: actualText,
                expectedText: expectedText,
                diffIndex: diffIndex);
    }

    private static readonly System.Text.UTF8Encoding s_strictUtf8Encoding =
        new(encoderShouldEmitUTF8Identifier: false, throwOnInvalidBytes: true);

    private static readonly System.Text.UnicodeEncoding s_strictUtf16Encoding =
        new(bigEndian: false, byteOrderMark: true, throwOnInvalidBytes: true);

    private static readonly System.Text.UTF32Encoding s_strictUtf32Encoding =
        new(bigEndian: false, byteOrderMark: false, throwOnInvalidCharacters: true);

    private static string? TryViewAsText(ReadOnlyMemory<byte> bytes)
    {
        var charBuffer = new char[bytes.Length];

        try
        {
            if (s_strictUtf8Encoding.TryGetChars(bytes.Span, charBuffer, out var charsWritten))
            {
                return new string(charBuffer, 0, charsWritten);
            }
        }
        catch (System.Text.DecoderFallbackException)
        {
            // Not valid UTF-8
        }

        try
        {
            if (s_strictUtf16Encoding.TryGetChars(bytes.Span, charBuffer, out var charsWritten))
            {
                return new string(charBuffer, 0, charsWritten);
            }
        }
        catch (System.Text.DecoderFallbackException)
        {
            // Not valid UTF-16
        }

        try
        {
            if (s_strictUtf32Encoding.TryGetChars(bytes.Span, charBuffer, out var charsWritten))
            {
                return new string(charBuffer, 0, charsWritten);
            }
        }
        catch (System.Text.DecoderFallbackException)
        {
            // Not valid UTF-32
        }

        return null;
    }

    private static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> Compile(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles)
    {
        if (!sourceFiles.TryGetValue(["elm.json"], out var elmJsonFile))
            throw new Exception("Missing elm.json file in source files");

        var elmJsonFileParsed =
            System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Span);

        IReadOnlyList<IReadOnlyList<string>> elmJsonSourceDirectories =
            [..elmJsonFileParsed?.SourceDirectories
            .Select(flat => flat.Split('/', '\\'))
            ];

        bool FilePathIsUnderElmJsonSourceDirectories(IReadOnlyList<string> filePath)
        {
            return
                elmJsonSourceDirectories
                .Any(sourceDir => filePath.Take(sourceDir.Count).SequenceEqual(sourceDir));
        }

        var compilationRootFilePath =
            sourceFiles
            .Where(c => c.Key[c.Key.Count - 1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .OrderBy(c => c.Key.Count)
            .OrderBy(c => FilePathIsUnderElmJsonSourceDirectories(c.Key) ? 0 : 1)
            .OrderBy(c => c.Key.SequenceEqual(ElmAppInterfaceConfig.Default.CompilationRootFilePath) ? 0 : 1)
            .FirstOrDefault()
            .Key;

        var compilationResult =
            ElmTime.ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                workingDirectoryRelative: [],
                ElmAppInterfaceConfig.Default with { CompilationRootFilePath = compilationRootFilePath });

        return
            compilationResult
            .Unpack(
                fromErr: compilationError =>
                {
                    var errorMessage = "\n" + ElmTime.ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationError) + "\n";

                    Console.WriteLine(errorMessage);

                    throw new Exception(errorMessage);
                },
                fromOk: compilationOk => compilationOk.Result.CompiledFiles);
    }

    private static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetElmAppFromDirectoryPath(
        string directoryPath)
    {
        if (!System.IO.Directory.Exists(directoryPath))
        {
            return ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty;
        }

        var files = Filesystem.GetAllFilesFromDirectory(directoryPath);

        var filesFiltered =
            LoadFromLocalFilesystem.RemoveNoiseFromTree(
                FileTree.FromSetOfFilesWithStringPath(files),
                discardGitDirectory: true);

        return
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(filesFiltered);
    }
}
