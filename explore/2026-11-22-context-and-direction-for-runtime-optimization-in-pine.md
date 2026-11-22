# 2026-11-22 - Context and Direction for Runtime Optimization in Pine

I have a hunch we should record this here so the proper context is available and easy to access for the next implementation steps.
There isn't much new in here, but I don't see that we have a concise overview recorded yet to guide development.
I will point to this file (among others) to keep design and architecture from going in nonsensical directions.

Before I list requirements, I want to illustrate how the bits to be developed will be used.
Let me show you around Fastworld, where software development is optimized for velocity.

## Context - Intuitive Software Development Process

Below is a quick overview of the intuitive idea for a software development environment and tools expected for ninety percent of development work.

> For remaining 1 - 10 percent of development work we might use different processes, using classic tools like GitHub Copilot, that depend on invoking other binaries. That is not the mode covered here.

Edits to program code will often be done with a tool that replaces a set of declarations.

Any edit to a file in the repository creates a tracked history entry that can be navigated to later in the timeline. This does not require storing a snapshot of the file tree. Most of revisions are defined by applications of repository edits in the WAL (event sourcing), and the runtime occasionaly adds a snapshot of the plain repository file contents.

Version control does not depend on git as classic projects often did. Whats written syncronously and waited on are the WAL entries. The snapshots to enable faster navigation are added asyncronously later. The state used to obtain the feedback, what was 'compile' / 'build' / 'run tests' / 'run app and send user input to get a screenshot' in classic projects lives in main memory, so there is no dependency on a file system.

Since no tool used for getting feedback on the code depends on a file system, continuing on a different revision is instant.

For that ninety percent of development work we focus on here, tools are reimplemented as simple functions. That means isolation is always given by design and working on multiple branches at the same time is bottlenecked by main memory capacity. Tool call just means that the current state of the repository is an argument given to the function, and the function produces a new repository file tree as part of the return value.

For any edit to a file in the repository, we want to have some feedback:

+ Diagnostics as we get them typically from compilers, build processes, style checkers and code formatters.
+ Test results as we get them from test runners. Beyond the simpler classic tests, that could also include checks on rasterized renderings of graphical user interfaces, such as raster snapshot diffs.
+ We maintain a configuration in the repository that defines the derivation of all such diagnostics as well as build artifacts. While that is a pure function, it can refer to external dependencies that are then pinned (mapped to a concrete file tree) as part of the repository build.
+ Artifacts produced by a repository can contain deployables, like binaries or WASM modules, snapshot mismatch files, or bundles prepared for deployment on various platforms.
+ For simplicity, we model the feedback to show for a file edit as a derivation from the entire repository output.
+ For a tool call for an edit of the repository, we compose the response by deriving a comparison between diagnostics derived from the new state and diagnostics derived from earlier states. Comparing only to the last state is not sufficient in many cases. A tool might offer marking a particular revision as reference for comparison. Another tool might offer returning the diagnostic diff between a selected revision and the current state. Whats important is that the immediate response to the edit shows the direction (more or less of errors/test failures/linter warnings, compared to an earlier state?)
+ While classic processes used separate turns to collect all feedack (edit tool response, CLI tool 'compile' response, CLI tool 'test run' response), this is overhead we dont want here. Thats why we fuse all relevant feedback into the response to the edit.

> In some cases we will prefer rejecting edits in program code files that introduce syntax errors. It remains to be seen if such a rejection leads to better overall efficiency. In case of rejection obviously the diagnostics over the repository dont change, therefore we dont to show a diagnostics diff.


Classic dev setups often had tight coupling between main interface and the processes used to compute some of the test results. Test runs where often bottlenecked by computing resources available on the same machine that happened to hold the main database or repository containing the project state.

To avoid such bottlenecks, we will often want to delegate work to another machine.

For example, we might have one machine serving as the main interface with something like an IDE.
While this machine can run in principle all tests itself, we dont want velocity be limited by that hardware.

So we offload computations such as test runs to other machines. That can be a workstation at home or a VM in hosted in a remote datacenter.
Such computation workers do not own any state informing the result of a computation request, but receive all inputs with each request.

The state-owning machine hands tasks to computation workers as generic `Eval`, using the generic Pine value encoding.

Subject to individual policies, external hash references used in a request enable the reuse of values already known to the worker:

+ The client speculates which values are already / still known to the worker due to some caching and uses external references for these when encoding the request.
+ The server lists the hashes of all unknown values in its response, so that the client can deliver the missing subset.

In principle this offloading can be done for individual tests defined in the project repository. To minimize overhead, we will often send a single computation request for the whole repository:

+ Root expression is said composed function to derive the diagnostics from the repository state (`diagnosticsFromRepo = buildFromRepo >> diagnosticsFromBuildResult`)
+ The environment contains the repository file tree and other input that might have been resolved as part of external dependencies.

Since the root expression is shared across many requests to the same worker, most requests include it using an external reference.

In most projects, the value modeling package contents from external references is shared across many requests, so most requests don't need to send a lot for these.

In most cases, the part of the `Eval` environment changing most frequently is the file tree of the project repository.

Q: If the client sends the repository state to the computation worker, does it need to materialize the file tree before each request?

A: No, the client does not need to know or materialize file contents for sending the request. Using the `Eval` nodes from the standard protocol, it can define the repository state using a chain of function applications for edits on a previous state. What kind of `Eval` nodes a worker accepts depends on project-specific or service-specific policy.

Q: If the client sends package contents to the computation worker, does it need to duplicate parsing to retrieve package references?

A: No, that can vary depending on the preferences of the project. The implementation of the [Elm app build used in the last five years](https://github.com/pine-vm/pine/commit/2c6770c9cf9f8b461397eef77df3a4380b04019d) shows an alternative implementation: In that design, the result models missing part (`MissingDependencyError`) and the client retries the request supplying those.


What could be the biggest challenges?

+ For apps targeting web browsers for GUI, implementing the rendering to emulate these closely enough. HTML and CSS are messy, and font rendering influencing the layout could be a big project.
+ Where the reader of the response is a human operator, we want to provide fast feedback. This can become a challenge in cases where new work not cached yet is genuinely expensive. To better support that, we would need to ensure lenient evaluation is fine-grained enough to stream for example first items from a list before the complete list is computed. When a project has sorted checks producing diagnostics roughly by how expensive they will be to compute, a display than can read a list of diagnostics from the beginning up to where items are already computed. We might also want the protocol to support a client querying specific paths in the tree of the return value to retrieve nodes that are computed already, independent of any order. The client could then render contents of the already completed branches without waiting for the complete return value. The key is that asking what’s ready should not force or wait for evaluation. To support this, the worker server assigns an ID to the evaluation. The client then can use that ID in subsequent requests to try query specific parts. The client can also specify how long a request should be kept open, before the server responds with 'computation not completed yet, here is an ID for that evaluation'

## Resulting Requirements for Runtime Optimizations

Remember this part of what we mentioned above:

> For simplicity, we model the feedback to show for a file edit as a derivation from the entire repository output.

From that design follows, for most real-world projects:

+ We discard most of the output in terms of size in the canonical representation
+ We discard the results of expensive computations.

We want the simplicity of reusing the build function to compose the diagnostics function, but we dont want to pay for the computations which are then discarded as part of the diagnostics function.

There are different ways to achieve this:

+ Lenient or lazy evaluation at runtime.
+ Something more compiler side: Analyze data flow for the final diagnostics function, and prune parts of the program proven to be never observed.

## Related Specifications

+ guide/pine-language.md
+ guide/pine-value-encoding-and-content-addressing.md
