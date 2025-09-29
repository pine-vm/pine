using System.Collections.Generic;
using System.Linq;
using static Pine.CompilePineToDotNet.CompileToCSharp;

namespace Pine.CompilePineToDotNet;

public class KernelFunctionSpecializedSpecificityComparer :
    IComparer<KernelFunctionSpecializedInfo>, IEqualityComparer<KernelFunctionSpecializedInfo>
{
    public static readonly KernelFunctionSpecializedSpecificityComparer Instance = new();

    private static int SpecificityRank(KernelFunctionParameterType t) => t switch
    {
        // More specific gets a higher rank

        KernelFunctionParameterType.Integer =>
        3,

        KernelFunctionParameterType.SpanGeneric =>
        2,

        KernelFunctionParameterType.Generic =>
        1,

        _ =>
        0
    };

    private static int ReturnTypeSpecificityRank(KernelFunctionSpecializedReturnType t) => t switch
    {
        // More specific gets a higher rank

        KernelFunctionSpecializedReturnType.Boolean =>
        2,

        KernelFunctionSpecializedReturnType.Generic =>
        1,

        _ => 0
    };

    public int Compare(KernelFunctionSpecializedInfo? x, KernelFunctionSpecializedInfo? y)
    {
        if (ReferenceEquals(x, y))
            return 0;

        if (x is null)
            return 1; // null is least specific

        if (y is null)
            return -1;

        var xParams = x.ParameterTypes;
        var yParams = y.ParameterTypes;

        // Primary: total specificity (descending)
        var xSum = xParams.Sum(SpecificityRank);
        var ySum = yParams.Sum(SpecificityRank);
        if (xSum != ySum)
        {
            return ySum < xSum ? -1 : 1;
        }

        // Secondary: lexicographic compare by per-parameter specificity (descending)
        var commonLen = xParams.Count < yParams.Count ? xParams.Count : yParams.Count;
        for (var i = 0; i < commonLen; i++)
        {
            var xr = SpecificityRank(xParams[i]);
            var yr = SpecificityRank(yParams[i]);

            if (xr != yr)
            {
                // Higher rank (more specific) sorts first
                return yr < xr ? -1 : 1;
            }
        }

        // Tertiary: more parameters first (tend to be more specialized)
        if (xParams.Count != yParams.Count)
        {
            return yParams.Count < xParams.Count ? -1 : 1;
        }

        // Quaternary: return type specificity (descending)
        var xRetRank = ReturnTypeSpecificityRank(x.ReturnType);
        var yRetRank = ReturnTypeSpecificityRank(y.ReturnType);

        if (xRetRank != yRetRank)
        {
            return yRetRank < xRetRank ? -1 : 1;
        }

        // Final: stable order by parameter enum values (just in case different enums with same ranks)
        for (var i = 0; i < xParams.Count; i++)
        {
            var cmp = ((int)xParams[i]).CompareTo((int)yParams[i]);

            if (cmp is not 0)
                return cmp;
        }

        // As ultimate tie-break, stable order by return type enum value
        if (x.ReturnType != y.ReturnType)
        {
            return ((int)x.ReturnType).CompareTo((int)y.ReturnType);
        }

        return 0;
    }

    public bool Equals(KernelFunctionSpecializedInfo? x, KernelFunctionSpecializedInfo? y)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null || y is null)
            return false;

        if (x.ReturnType != y.ReturnType)
            return false;

        if (x.ParameterTypes.Count != y.ParameterTypes.Count)
            return false;

        for (var i = 0; i < x.ParameterTypes.Count; i++)
        {
            if (x.ParameterTypes[i] != y.ParameterTypes[i])
                return false;
        }

        return true;
    }

    public int GetHashCode(KernelFunctionSpecializedInfo obj)
    {
        unchecked
        {
            var hash = 17;

            foreach (var p in obj.ParameterTypes)
            {
                hash = hash * 31 + (int)p;
            }

            hash = hash * 31 + (int)obj.ReturnType;

            return hash;
        }
    }
}
