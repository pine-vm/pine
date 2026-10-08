using System;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Expected compilation diagnostics, distinguished from implementation failures at host boundaries.</summary>
public sealed class ElmCompilationException(string message) : Exception(message);
