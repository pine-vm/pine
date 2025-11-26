using System;
using System.Collections.Generic;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class Rendering
{
    public record Config(
        LineBreakingConfig LineBreaking);

    public abstract record LineBreakingConfig()
    {
        public static LineBreakingConfig SnapshotTestsDefault =>
            new CanonicalBasedOnComplexity();

        /// <summary>
        /// Insert line breaks based on the complexity of expressions.
        /// </summary>
        public sealed record CanonicalBasedOnComplexity
            : LineBreakingConfig;
    }

    /// <summary>
    /// Do not use any location information from the given nodes, but normalize
    /// all locations for a consistent layout.
    /// 
    /// <para>
    /// The output should be stable with the use of elm-format.
    /// </para>
    /// </summary>
    public static Config ConfigNormalizeAllLocations(
        LineBreakingConfig lineBreaking) =>
        new(LineBreaking: lineBreaking);

    public static string ToString(
        File file,
        Config config)
    {
        var asLines = ToLines(file, config);

        return
            string.Join(
                '\n',
                asLines);
    }

    public static IEnumerable<string> ToLines(
        File file,
        Config config)
    {
        throw new NotImplementedException();
    }
}
