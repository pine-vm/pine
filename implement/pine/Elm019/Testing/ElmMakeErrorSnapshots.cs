using Pine.Core.Elm;
using Pine.Core.Elm.Elm019;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace Pine.Elm019.Testing;

public sealed record ElmMakeSnapshotProblem(string? File, ElmMakeRegion? Region, string Title);

public abstract record ElmMakeCompilation
{
    public sealed record Output(string Text) : ElmMakeCompilation;

    public sealed record CompilerErrors(IReadOnlyList<ElmMakeSnapshotProblem> Problems) : ElmMakeCompilation;
}

/// <summary>Lossless projection of diagnostic file, region and title, independent of message rendering.</summary>
public static partial class ElmMakeErrorSnapshots
{
    public static ElmMakeSnapshotProblem Project(ElmMakeProblem problem)
    {
        var (file, region) =
            problem.Location switch
            {
                ElmMakeLocation.Project => ((string?)null, (ElmMakeRegion?)null),
                ElmMakeLocation.File location => (FilePath(location.FileId), null),
                ElmMakeLocation.Source location => (FilePath(location.FileId), location.Region),

                _ =>
                throw new NotImplementedException(
                    nameof(Project) + " does not handle location variant: " + problem.Location.GetType().Name),
            };

        var title =
            problem.Kind.Code switch
            {
                ElmMakeProblemCode.Syntax => "SYNTAX PROBLEM",
                ElmMakeProblemCode.Manifest => "ELM.JSON PROBLEM",
                ElmMakeProblemCode.NoManifest => "NO elm.json FILE",
                ElmMakeProblemCode.EntryPointNotFound => "FILE NOT FOUND",
                ElmMakeProblemCode.NoMain => "NO MAIN",
                ElmMakeProblemCode.CyclicTypeAlias => "ALIAS PROBLEM",
                ElmMakeProblemCode.DebugRemnants => "DEBUG REMNANTS",

                ElmMakeProblemCode.UnknownValue or ElmMakeProblemCode.UnknownType or
                ElmMakeProblemCode.NotExposed or ElmMakeProblemCode.AmbiguousName or
                ElmMakeProblemCode.NotAConstructor or ElmMakeProblemCode.InvalidExposure or
                ElmMakeProblemCode.RestrictedKernelImport =>
                "NAMING ERROR",

                ElmMakeProblemCode.MissingModule => "MODULE NOT FOUND",
                ElmMakeProblemCode.AmbiguousModule => "AMBIGUOUS IMPORT",
                ElmMakeProblemCode.DuplicateModule => "MODULE NAME CLASH",
                ElmMakeProblemCode.DuplicateDeclaration or ElmMakeProblemCode.DuplicateBinding => "NAME CLASH",
                ElmMakeProblemCode.ShadowedBinding => "SHADOWING",

                ElmMakeProblemCode.WrongConstructorArity =>
                problem.Kind.ActualArity < problem.Kind.ExpectedArity ? "TOO FEW ARGS" : "TOO MANY ARGS",

                _ =>
                throw new NotImplementedException("Unknown diagnostic code: " + problem.Kind.Code),
            };

        return new(file, region, title);
    }

    private static string FilePath(ElmMakeFile file) =>
        file switch
        {
            ElmMakeFile.Application application => string.Join("/", application.Path),
            ElmMakeFile.Package package => "package:" + package.Identity + "/" + string.Join("/", package.Path),

            _ =>
            throw new NotImplementedException(
                nameof(FilePath) + " does not handle file variant: " + file.GetType().Name),
        };

    private static readonly JsonSerializerOptions s_reportOptions =
        new(ElmMakeReportConverter.SerializerOptions)
        {
            RespectRequiredConstructorParameters = true,
            RespectNullableAnnotations = true,
        };

    public static string Render(IEnumerable<ElmMakeSnapshotProblem> problems)
    {
        var ordered =
            problems
            .OrderBy(problem => problem.File, StringComparer.Ordinal)
            .ThenBy(problem => problem.Region?.Start.Line)
            .ThenBy(problem => problem.Region?.Start.Column)
            .ThenBy(problem => problem.Region?.End.Line)
            .ThenBy(problem => problem.Region?.End.Column)
            .ThenBy(problem => problem.Title, StringComparer.Ordinal)
            .ToArray();

        if (ordered.Length is 0)
            throw new InvalidDataException("A compiler-error expectation requires at least one problem.");

        return
            string.Join(
                "\n",
                ordered.Select(
                    problem =>
                    {
                        Validate(problem);

                        return
                            "File: " + (problem.File is null ? "<none>" : Escape(problem.File)) +
                            "\nRegion: " +
                            (problem.Region is null
                            ?
                            "<none>"
                            :
                            FormattableString.Invariant(
                                $"{problem.Region.Start.Line}:{problem.Region.Start.Column}-{problem.Region.End.Line}:{problem.Region.End.Column}")) +
                            "\nTitle: " + Escape(problem.Title) + "\n";
                    }));
    }

    public static IReadOnlyList<ElmMakeSnapshotProblem> Parse(string text)
    {
        var normalized = ElmMakeEcmascriptScenarios.NormalizeLineEndings(text);

        if (!normalized.EndsWith('\n') || normalized.StartsWith('\uFEFF'))
            throw new InvalidDataException("Error snapshots must be UTF-8 without a BOM and end with one newline.");

        var problems = new List<ElmMakeSnapshotProblem>();

        foreach (var record in normalized[..^1].Split("\n\n", StringSplitOptions.None))
        {
            var lines = record.Split('\n');

            if (lines.Length is not 3 || !lines[0].StartsWith("File: ", StringComparison.Ordinal) ||
                !lines[1].StartsWith("Region: ", StringComparison.Ordinal) ||
                !lines[2].StartsWith("Title: ", StringComparison.Ordinal))
            {
                throw new InvalidDataException("Expected File, Region and Title lines in each compiler-error record.");
            }

            var file = lines[0]["File: ".Length..];
            var region = lines[1]["Region: ".Length..];
            ElmMakeRegion? location = null;

            if (region is not "<none>")
            {
                var match = RegionRegex().Match(region);

                if (!match.Success)
                    throw new InvalidDataException("Invalid compiler-error region: " + region);

                var values = new int[4];

                for (var index = 0; index < values.Length; ++index)
                {
                    if (!int.TryParse(match.Groups[index + 1].Value, NumberStyles.None, CultureInfo.InvariantCulture, out values[index]))
                        throw new InvalidDataException("Compiler-error coordinate is out of range: " + region);
                }

                if (values[2] < values[0] || values[2] == values[0] && values[3] < values[1])
                    throw new InvalidDataException("Compiler-error region ends before it starts: " + region);

                location = new(new(values[0], values[1]), new(values[2], values[3]));
            }

            problems.Add(
                new(
                    file is "<none>" ? null : Unescape(file),
                    location,
                    Unescape(lines[2]["Title: ".Length..])));
        }

        if (Render(problems) != normalized)
            throw new InvalidDataException("Compiler-error expectation is not in canonical location order or text format.");

        return problems;
    }

    public static string SnapshotText(ElmMakeExpectation expectation, ElmMakeCompilation compilation) =>
        compilation switch
        {
            ElmMakeCompilation.Output output =>
            expectation.Kind is ElmMakeExpectationKind.Output
            ?
            output.Text
            :
            throw new InvalidDataException("Expected compiler errors, but compilation succeeded."),

            ElmMakeCompilation.CompilerErrors errors =>
            expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails
            ?
            Render(errors.Problems)
            :
            throw new InvalidDataException(
                "Expected successful output, but compilation reported errors:\n" + Render(errors.Problems)),

            _ =>
            throw new NotImplementedException(
                nameof(SnapshotText) + " does not handle compilation variant: " + compilation.GetType().Name),
        };

    public static IReadOnlyList<ElmMakeSnapshotProblem> ReadReferenceReport(
        string json,
        string applicationRoot,
        string workingDirectory,
        IReadOnlyDictionary<ElmPackageIdentity, string> packageRoots)
    {
        var report =
            JsonSerializer.Deserialize<ElmMakeReport>(json, s_reportOptions)
            ?? throw new InvalidDataException("Missing reference compiler report.");

        var problems = new List<ElmMakeSnapshotProblem>();

        string? MapPath(string? path) =>
            path is null ? null : NormalizeReferencePath(path, applicationRoot, workingDirectory, packageRoots);

        switch (report)
        {
            case ElmMakeReport.ElmMakeReportCompileErrors compilation:
                if (compilation.Errors is null)
                    throw new InvalidDataException("Missing reference compiler errors.");

                foreach (var file in compilation.Errors)
                {
                    if (file is null)
                        throw new InvalidDataException("Invalid reference compiler file entry.");

                    var path =
                        MapPath(file.Path)
                        ?? throw new InvalidDataException("A source diagnostic must have a file.");

                    if (file.Name is null || file.Problems is null || file.Problems.Count is 0)
                        throw new InvalidDataException("Reference compiler reported a file with no problems.");

                    foreach (var problem in file.Problems)
                    {
                        if (problem is null)
                            throw new InvalidDataException("Invalid reference compiler problem.");

                        var region = problem.Region;

                        if (region?.Start is null || region.End is null)
                            throw new InvalidDataException("Missing reference diagnostic region.");

                        ValidateMessage(problem.Message);

                        problems.Add(
                            new(
                                path,
                                new(
                                    new(region.Start.Line, region.Start.Column),
                                    new(region.End.Line, region.End.Column)),
                                problem.Title));
                    }
                }

                break;

            case ElmMakeReport.ElmMakeReportError project:
                ValidateMessage(project.Message);

                if (!IsProjectDiagnostic(project.Title))
                {
                    throw new InvalidDataException(
                        "Reference compiler infrastructure failure or unrecognized project report: " + project.Title);
                }

                problems.Add(new(MapPath(project.Path), null, project.Title));
                break;

            default:
                throw new NotImplementedException(
                    nameof(ReadReferenceReport) + " does not handle report variant: " + report.GetType().Name);
        }

        _ = Render(problems);
        return problems;
    }

    private static bool IsProjectDiagnostic(string title) =>
        title is "NO elm.json FILE" or "CLASHING FLAGS" or "NO INPUT" or "TOO MANY FILES" or "NO MAIN" or
            "FILE NOT FOUND" or "UNEXPECTED FILE EXTENSION" or "CONFUSING FILE" or "CONFUSING FILES" or
            "MODULE NAME CLASH" or "UNEXPECTED FILE NAME" or "IMPORT CYCLE" or "MISSING MODULE" or
            "AMBIGUOUS MODULE NAME" or "DEBUG REMNANTS" or "MISSING SOURCE DIRECTORY" or
            "MISSING SOURCE DIRECTORIES" or "REDUNDANT SOURCE DIRECTORIES" or "MISSING DEPENDENCY" or
            "INCOMPATIBLE DEPENDENCIES" or "ELM VERSION MISMATCH" or "ERROR IN DEPENDENCIES" or
            "UNEXPECTED TYPE" or "INVALID PACKAGE NAME" or "PROBLEM WITH VERSION" or "PROBLEM WITH CONSTRAINT" or
            "PROBLEM WITH MODULE NAME" or "HEADER TOO LONG" or "PROBLEM WITH DEPENDENCY NAME" or "UNKNOWN LICENSE" or
            "SUMMARY TOO LONG" or "NO SOURCE DIRECTORIES" or "EXPECTING A VALUE" or "EXTRA COMMA" or "EXPECTING COLON" or
            "UNFINISHED OBJECT" or "UNFINISHED ARRAY" or "ENDLESS STRING" or "UNEXPECTED CONTROL CHARACTER" or
            "UNKNOWN ESCAPE" or "BAD HEX ESCAPE" or "BAD NUMBER" or "UNEXPECTED NUMBER" or "JSON PROBLEM" or
            "EXPECTING OBJECT" or "EXPECTING ARRAY" or "EXPECTING STRING" or "EXPECTING BOOL" or "EXPECTING INT" or
            "MISSING FIELD" or "EXPECTING PAIR";

    public static string NormalizeReferencePath(
        string path, string applicationRoot, string workingDirectory, IReadOnlyDictionary<ElmPackageIdentity, string> packageRoots)
    {
        var absolute =
            Path.GetFullPath(
                path.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar),
                workingDirectory);

        foreach (var (identity, root) in packageRoots)
        {
            if (RelativeWithin(root, absolute) is { } relative)
                return "package:" + identity + "/" + relative;
        }

        return
            RelativeWithin(applicationRoot, absolute)
            ?? throw new InvalidDataException("Reference diagnostic path is outside known application/package roots: " + path);
    }

    private static string? RelativeWithin(string root, string path)
    {
        var relative = Path.GetRelativePath(Path.GetFullPath(root), path).Replace('\\', '/');

        return
            relative is ".." || relative.StartsWith("../", StringComparison.Ordinal) || Path.IsPathRooted(relative)
            ?
            null
            :
            relative;
    }

    private static void Validate(ElmMakeSnapshotProblem problem)
    {
        if (string.IsNullOrEmpty(problem.Title))
            throw new InvalidDataException("A compiler problem must have a title.");

        if (problem.File is { } file)
        {
            var relative = file;

            if (file.StartsWith("package:", StringComparison.Ordinal))
            {
                var parts = file["package:".Length..].Split('/');

                if (parts.Length < 3)
                    throw new InvalidDataException("Invalid package diagnostic path: " + file);

                var identity = parts[0] + "/" + parts[1];
                var at = identity.LastIndexOf('@');

                if (at < 0)
                    throw new InvalidDataException("Missing diagnostic package version: " + file);

                ElmDependencyResolver.ValidatePackageName(identity[..at]);
                _ = ElmPackageVersion.Parse(identity[(at + 1)..]);
                relative = string.Join("/", parts.Skip(2));
            }

            if (relative is "" or "<none>" || relative.Contains('\\') || relative.Contains(':') ||
                relative.Split('/').Any(segment => segment is "" or "." or ".."))
            {
                throw new InvalidDataException("Expected a normalized logical diagnostic file path: " + file);
            }
        }

        if (problem.Region is { } region &&
            (problem.File is null || region.Start.Line < 1 || region.Start.Column < 1 || region.End.Line < 1 ||
            region.End.Column < 1 || region.End.Line < region.Start.Line ||
            region.End.Line == region.Start.Line && region.End.Column < region.Start.Column))
        {
            throw new InvalidDataException("Invalid compiler problem region.");
        }
    }

    private static void ValidateMessage(IReadOnlyList<MessageItem> message)
    {
        if (message is null)
            throw new InvalidDataException("Invalid reference diagnostic message.");

        foreach (var part in message)
        {
            if (part is null)
                throw new InvalidDataException("Invalid reference diagnostic message fragment.");

            switch (part)
            {
                case MessageItem.StringMessage text:
                    if (text.Value is null)
                        throw new InvalidDataException("Invalid reference diagnostic text.");

                    break;

                case MessageItem.StyledMessage styled:
                    if (styled.String is null)
                        throw new InvalidDataException("Invalid reference diagnostic styled text.");

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(ValidateMessage) + " does not handle message variant: " + part?.GetType().Name);
            }
        }
    }

    private static string Escape(string value) =>
        value.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\r", "\\r", StringComparison.Ordinal)
        .Replace("\n", "\\n", StringComparison.Ordinal).Replace("\t", "\\t", StringComparison.Ordinal);

    private static string Unescape(string value)
    {
        var result = new StringBuilder();

        for (var index = 0; index < value.Length; ++index)
        {
            if (value[index] is not '\\')
            {
                result.Append(value[index]);
                continue;
            }

            if (++index == value.Length)
                throw new InvalidDataException("Incomplete escape in compiler-error snapshot.");

            result.Append(
                value[index] switch
                {
                    '\\' => '\\',
                    'r' => '\r',
                    'n' => '\n',
                    't' => '\t',

                    _ =>
                    throw new InvalidDataException("Unknown escape in compiler-error snapshot."),
                });
        }

        return result.ToString();
    }

    [GeneratedRegex(@"^([1-9][0-9]*):([1-9][0-9]*)-([1-9][0-9]*):([1-9][0-9]*)$", RegexOptions.CultureInvariant)]
    private static partial Regex RegionRegex();
}
