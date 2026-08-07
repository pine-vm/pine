namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class ElmSyntaxOptimization
{
    /// <summary>
    /// Configuration for the wrap/unwrap cancellation rewrite.
    /// </summary>
    public sealed record Config(
        bool WrapUnwrapCancellationEnabled = true)
    {
        /// <summary>
        /// Enables the wrap/unwrap cancellation rewrite.
        /// </summary>
        public static readonly Config WrapUnwrapCancellationOnly = new();
    }
}
