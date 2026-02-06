# Parsing as a Static Program

Since Pine is an inherently dynamic language, not every Pine program can be mapped to a static program.

Compilers that translate from static source languages, such as Elm, will often produce purely static Pine programs. (A typical case of an exception to this rule is when the application developer selects a higher-order function as the entry point for compilation)

But why do we even care about translating Pine code into a static program representation?

+ Supporting inspection and snapshot tests to verify the behavior of compilers and code transformations in general.
+ Supporting compilation to lower-level representations, especially static programming languages.

While the first use case allows some leniency, the latter is more demanding, requiring fidelity that includes all information needed to construct a program that behaves the same way.

The incompleteness of this parser doesn't compromise correctness; it only limits optimization. For performance-optimization use cases, the parser's completeness is somewhat negotiable. When the parser fails, we can fall back to the interpreter to run the program. If the parser does not yet cover an edge case, that just prevents up-tiering.

The parser's coverage evolves with the demands of snapshot testing and performance optimization.

## Unifying Function Identifiers

One of the most important challenges in parsing Pine programs is unifying function identifiers in function applications. Compilers produce different forms when translating function applications from source programs to Pine.

That is, for a given function application in the source program, a compiler might choose different representations in the emitted Pine code. These choices can depend, for example, on whether dependencies of the invoked function are available for reuse in the current Pine environment.

To achieve the goals detailed above, we want the parser to treat equivalent function applications as using the same function identifier. That means the parser must develop proofs for the equivalence of various concrete forms.

The goal of the parser implementation is not to develop comprehensive proofs that cover and unify all equivalent representations. Instead, our parser is limited to proofs for a few classes of representations. This means the parser works well with compilers that limit themselves to only emitting function applications in one of these supported shapes.

