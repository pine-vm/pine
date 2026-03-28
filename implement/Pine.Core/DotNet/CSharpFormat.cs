using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace Pine.Core.DotNet;

/// <summary>
/// Provides methods for formatting C# source code by parsing and rewriting the syntax tree.
/// Supports both C# script files (top-level statements) and regular C# files.
/// <para>
/// For the formatting specifications, see 'csharp-coding-guidelines.md'
/// </para>
/// </summary>
public class CSharpFormat
{
    /// <summary>
    /// Formats C# source text parsed as a script (top-level statements allowed).
    /// Returns <c>Ok</c> with the formatted text, or <c>Err</c> with a <see cref="FormatCSharpFile.CycleError"/>
    /// if formatting does not converge.
    /// </summary>
    /// <param name="inputSyntaxText">The C# source text to format.</param>
    public static Result<FormatCSharpFile.CycleError, string> FormatCSharpScript(string inputSyntaxText) =>
        DotNet.FormatCSharpFile.FormatSyntaxTree(ParseAsCSharpScript(inputSyntaxText))
        .Map(tree => tree.GetRoot().ToFullString());


    /// <summary>
    /// Formats C# source text parsed as a regular file (requires valid compilation unit structure).
    /// Returns <c>Ok</c> with the formatted text, or <c>Err</c> with a <see cref="FormatCSharpFile.CycleError"/>
    /// if formatting does not converge.
    /// </summary>
    /// <param name="inputSyntaxText">The C# source text to format.</param>
    public static Result<FormatCSharpFile.CycleError, string> FormatCSharpFile(string inputSyntaxText) =>
        DotNet.FormatCSharpFile.FormatSyntaxTree(ParseAsCSharpFile(inputSyntaxText))
        .Map(tree => tree.GetRoot().ToFullString());


    /// <summary>
    /// Parses the given text as a C# script syntax tree.
    /// </summary>
    private static SyntaxTree ParseAsCSharpScript(string inputSyntaxText) =>
        SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Script));


    /// <summary>
    /// Parses the given text as a regular C# file syntax tree.
    /// </summary>
    private static SyntaxTree ParseAsCSharpFile(string inputSyntaxText) =>
        SyntaxFactory.ParseSyntaxTree(
            inputSyntaxText,
            options: new CSharpParseOptions().WithKind(SourceCodeKind.Regular));
}
