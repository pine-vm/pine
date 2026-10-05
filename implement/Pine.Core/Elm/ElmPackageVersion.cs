using System;
using System.Globalization;
using System.Text.RegularExpressions;

namespace Pine.Core.Elm;

/// <summary>An Elm registry version, compared numerically rather than lexicographically.</summary>
public readonly record struct ElmPackageVersion(int Major, int Minor, int Patch) : IComparable<ElmPackageVersion>
{
    private static readonly Regex s_version =
        new(@"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$", RegexOptions.CultureInvariant);

    /// <summary>Parses three non-negative numeric components; rejects tags, leading zeroes and prerelease suffixes.</summary>
    public static ElmPackageVersion Parse(string text)
    {
        var match = s_version.Match(text);

        if (!match.Success ||
            !int.TryParse(match.Groups[1].Value, NumberStyles.None, CultureInfo.InvariantCulture, out var major) ||
            !int.TryParse(match.Groups[2].Value, NumberStyles.None, CultureInfo.InvariantCulture, out var minor) ||
            !int.TryParse(match.Groups[3].Value, NumberStyles.None, CultureInfo.InvariantCulture, out var patch))
        {
            throw new FormatException(
                $"Invalid Elm version '{text}'. Expected three non-negative integers, for example '1.0.5'.");
        }

        return new(major, minor, patch);
    }

    /// <inheritdoc/>
    public int CompareTo(ElmPackageVersion other)
    {
        var major = Major.CompareTo(other.Major);
        var minor = Minor.CompareTo(other.Minor);
        return major != 0 ? major : minor != 0 ? minor : Patch.CompareTo(other.Patch);
    }

    /// <inheritdoc/>
    public override string ToString() =>
        string.Create(CultureInfo.InvariantCulture, $"{Major}.{Minor}.{Patch}");
}

/// <summary>An exact version or an interval in Elm's constraint syntax.</summary>
public sealed record ElmPackageVersionConstraint(
    ElmPackageVersion Lower,
    bool IncludeLower,
    ElmPackageVersion Upper,
    bool IncludeUpper)
{
    private static readonly Regex s_range =
        new(@"^(\S+) (<|<=) v (<|<=) (\S+)$", RegexOptions.CultureInvariant);

    /// <summary>Whether the interval denotes a single exact version.</summary>
    public bool IsExact => Lower == Upper && IncludeLower && IncludeUpper;

    /// <summary>Creates a singleton constraint for an application pin or resolution lock.</summary>
    public static ElmPackageVersionConstraint Exact(ElmPackageVersion version) =>
        new(version, true, version, true);

    /// <summary>Parses an exact version or an Elm range, preserving inclusive/exclusive bounds.</summary>
    public static ElmPackageVersionConstraint Parse(string text)
    {
        var match = s_range.Match(text);

        if (!match.Success)
            return Exact(ElmPackageVersion.Parse(text));

        var lower = ElmPackageVersion.Parse(match.Groups[1].Value);
        var upper = ElmPackageVersion.Parse(match.Groups[4].Value);

        if (lower.CompareTo(upper) >= 0)
            throw new FormatException($"Invalid Elm range '{text}': the lower bound must be less than the upper bound.");

        return new(lower, match.Groups[2].Value is "<=", upper, match.Groups[3].Value is "<=");
    }

    /// <summary>Tests all interval boundaries using numeric version comparison.</summary>
    public bool Contains(ElmPackageVersion version) =>
        (version.CompareTo(Lower) > 0 || IncludeLower && version == Lower) &&
        (version.CompareTo(Upper) < 0 || IncludeUpper && version == Upper);

    /// <summary>Returns the shared interval, or null when the constraints genuinely conflict.</summary>
    public ElmPackageVersionConstraint? Intersect(ElmPackageVersionConstraint other)
    {
        var lowerComparison = Lower.CompareTo(other.Lower);
        var upperComparison = Upper.CompareTo(other.Upper);
        var lower = lowerComparison >= 0 ? Lower : other.Lower;
        var upper = upperComparison <= 0 ? Upper : other.Upper;

        var includeLower =
            lowerComparison is 0
            ?
            IncludeLower && other.IncludeLower
            :
            lowerComparison > 0 ? IncludeLower : other.IncludeLower;

        var includeUpper =
            upperComparison is 0
            ?
            IncludeUpper && other.IncludeUpper
            :
            upperComparison < 0 ? IncludeUpper : other.IncludeUpper;

        return
            lower.CompareTo(upper) > 0 || lower == upper && !(includeLower && includeUpper)
            ?
            null
            :
            new(lower, includeLower, upper, includeUpper);
    }

    /// <inheritdoc/>
    public override string ToString() =>
        IsExact
        ?
        Lower.ToString()
        :
        $"{Lower} {(IncludeLower ? "<=" : "<")} v {(IncludeUpper ? "<=" : "<")} {Upper}";
}
