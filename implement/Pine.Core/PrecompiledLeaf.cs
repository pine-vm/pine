using Pine.Core.Internal;

namespace Pine.Core;

/// <summary>
/// Computes a precompiled Pine leaf from its in-process environment.
/// </summary>
public delegate PineValueInProcess? PrecompiledLeaf(PineValueInProcess environment);
