# 2025-04-27 - Considering Compilation from Elm to C# and .NET

Some apps and workloads cannot yet migrate from JavaScript. We can group the blockers into two categories: Coverage of language elements and kernel code and runtime efficiency.
In the case of BotLab apps, this usage of JavaScript limits performance and UX with long response times.

For apps already running on PineVM, runtime efficiency improved a lot in 2024, but there is still a lot to do even to reach the level of efficiency we have with JavaScript in V8. The challenges of improving efficiency in PineVM are partially due to the support of meta-programming and multiple frontend languages and partially due to the high aspirations for simplicity.
2023 saw the development of the first compiler from Pine to C#, but that design ultimately turned out to be unfit. Currently, the default execution path uses a stack-machine bytecode interpreter.

In light of the challenges in implementing profiling, analysis, and compilers for Pine, taking a shortcut to get a partial solution faster is tempting. As an intermediate solution, we could add a compiler that does not support the metaprogramming parts and is limited to the Elm programming language.

## Coverage

How would the coverage and features of such an intermediate solution differ?

+ No metaprogramming or data as code.
+ No durable representation of programs as data.
+ Not as easy to experiment with changes to the Elm compiler and different language flavors.
+ No support for other application languages, limited to Elm.
+ No migrations as we use them with web services deployments.
+ `Int`: limited to `Int64`
+ `Float`: limited to `double`
+ No automatic distribution of work across threads/CPUs/machines.
+ No automatic optimization of (main) memory usage with fine-grained offloading.
+ No optimization via memoization.
+ No lenient evaluation.
+ None of the optimizations that enable automatic concurrency.
+ No latency optimizations by automatic assignment of owning datacenter/region.
+ Reduced support for generic records (more on that below)

## Generic Records

One challenge in compiling from Elm to C# is generic records (row polymorphism). One way of dealing with this would be using a generic dictionary for any record. However, I prefer to emit specific types for better runtime performance. For internal usages of functions working with generic records, we could add type inference to enumerate all concrete record types used there and then emit concretized instances of that function accordingly. That does not work for exposed functions, so these might be unsupported.


## Expected performance

While such a design cannot come close to the efficiency enabled by profiling-informed optimizations as planned for Pine, such a compilation to C# could yield response times even slightly better than with V8.

Most popular JavaScript engines do not support tail-call optimization, which sometimes leads to nasty surprises in the form of app crashes due to stack overflow. Unfortunately, the mitigations to work around this limitation also affect library authors and application programmers. Adding tail-call optimization seems a relatively cheap way to offer at least better ergonomics than V8.

## Unique benefits

It may not all be extra work; there could be some unique benefits to such a specialized tool:

+ An automated translation into a language like C# could be helpful in learning/teaching.
+ Some projects might benefit from the more direct integration between Elm code and .NET programs enabled this way.


related:

+ <https://forum.botlab.org/t/c-net-and-elm-operations/3224>

## 2025-09 Update

When I mentioned this idea on the Elm Slack server, Martin Janiczek pointed out that Lue is working on something like this. Indeed, Lue went on a great exploration of compilation to F#/.NET:

+ <https://github.com/lue-bird/elm-syntax-to-fsharp/blob/decedecadda18b606650e51e67f5ecf05ef8e9c9/README.md>
+ <https://discourse.elm-lang.org/t/elm-to-f-transpiler/10313>

How Awesome! Lue also explored compilation from Elm to a few other programming languages. Here are some examples:

+ Rust: <https://github.com/lue-bird/elm-syntax-to-rust/blob/2aee732f9da5af24abfd1b8e476bbbfc1600b78a/README.md>
+ Swift: <https://github.com/lue-bird/elm-syntax-to-swift/blob/4712f2d54056397c683efdb65f0743caa65223d7/README.md>
+ MoonBit: <https://github.com/lue-bird/elm-syntax-to-moonbit/blob/d5144e22fd8c12a22793c3d35276cc581275f07c/README.md>

I eventually decided to prioritize the non-Elm-specific compilation, landing code analysis for Pine programs, and further compilation to C# in September:

+ [2025-07-27-pine-project-siracusa.md](./2025-07-27-pine-project-siracusa.md)
+ [Changelog - 2025-09-27](./../implement/CHANGELOG.md#2025-09-27---new-pine-to-cs-compiler)
