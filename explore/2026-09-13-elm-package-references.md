# 2026-09-13 Elm Package References

The goal is to introduce robust and scalable package references with a design that also supports the format used by Elm 0.19 projects.

## Current Limitations of Other Tools

Tools like the one from <https://github.com/mpizenberg/elm-test-rs> and <https://github.com/elm/compiler> frequently failed on attempts to connect to services via the network:

For example, using a fresh `ELM_HOME` to force a package-list download, Elm 0.19.1 failed for this repository's `implement/Pine.Core/Elm/elm-in-elm` project:

```
ELM_HOME=/tmp/elm-home-compiler /tmp/elm-tools/elm make src/Main.elm --output=/dev/null
```

```
-- PROBLEM LOADING PACKAGE LIST ------------------------------------------------

I need the list of published packages to verify your dependencies, so I tried to
fetch:

    https://package.elm-lang.org/all-packages

But my HTTP library is giving me the following error message:

    ConnectionTimeout

Are you somewhere with a slow internet connection? Or no internet? Does the link
I am trying to fetch work in your browser? Maybe the site is down? Does your
internet connection have a firewall that blocks certain domains? It is usually
something like that!

Dependencies ready!
```

Under the same conditions, elm-test-rs 3.0.1 failed with:

```
ELM_HOME=/tmp/elm-home-test-rs elm-test-rs make --compiler /tmp/elm-tools/elm
```

```
Error: Failed to solve dependencies for tests to run

Caused by:
    0: Combining the project dependencies with the ones of the test runner failed
    1: Failed to initialize the online provider.
       failed to fetch "https://package.elm-lang.org/all-packages"
```

## Current Limitations in The `elm test` Command

Package references are not yet implemented:

For example in the directory <https://github.com/pine-vm/pine/tree/97f87736b3c051598af8a09bb37021c69661c1ff/implement/Pine.Core/Elm/elm-in-elm> the `elm  test` command failed like this:

```
Unhandled exception: System.InvalidOperationException: Failed compiling Elm tests: Failed to compile SCC [CompileElmAppTests.backend_state_type_name_from_root_elm_module__lifted__lambda2]: In scope 'Failed compiling declaration 'CompileElmAppTests.backend_state_type_name_from_root_elm_module__lifted__lambda2'': Function 'Result.Extra.unpack' not found in dependency layout
   at Pine.Core.Elm.Testing.ElmTestRunner.<>c.<CompileAndRunTests>b__3_10(String error) in D:\a\pine\pine\implement\Pine.Core\Elm\Testing\ElmTestRunner.cs:line 163
   at Pine.Core.Result`2.Extract(Func`2 fromErr) in D:\a\pine\pine\implement\Pine.Core\Result.cs:line 162
   at Pine.Core.Elm.Testing.ElmTestRunner.CompileAndRunTests(String appDirectory, IPineVM pineVm, String filter, Boolean listTests, Int32 workers, Func`3 pineVmFactory, Action`1 onTestsDiscovered) in D:\a\pine\pine\implement\Pine.Core\Elm\Testing\ElmTestRunner.cs:line 158
   at Pine.Core.Elm.Testing.ElmTestRunner.CompileAndRunTests(String appDirectory, Int32 workers, Func`3 pineVmFactory, String filter, Boolean listTests, Action`1 onTestsDiscovered) in D:\a\pine\pine\implement\Pine.Core\Elm\Testing\ElmTestRunner.cs:line 71
   at Pine.CLI.Elm.TestCommand.Execute(String source, Nullable`1 colorMode, IAnsiConsole console, IAnsiConsole errorConsole, String filter, Boolean listTests, Nullable`1 workers, Boolean reportDurations) in D:\a\pine\pine\implement\pine\CLI\Elm\TestCommand.cs:line 122
   at Pine.CLI.Elm.TestCommand.<>c__DisplayClass0_0.<Create>b__0(ParseResult parseResult) in D:\a\pine\pine\implement\pine\CLI\Elm\TestCommand.cs:line 64
   at System.CommandLine.Invocation.AnonymousSynchronousCommandLineAction.Invoke(ParseResult parseResult)
   at System.CommandLine.Invocation.InvocationPipeline.Invoke(ParseResult parseResult)
```

## How Elm 0.19 Does Package References

Let's look at a [concrete example of an elm.json file](https://github.com/pine-vm/pine/blob/97f87736b3c051598af8a09bb37021c69661c1ff/implement/Pine.Core/Elm/elm-in-elm/elm.json):

```json
{
    "type": "application",
    "source-directories": [
        "src",
        "elm-syntax/src",
        "elm-syntax-encode-json/src",
        "pine-elm-syntax/src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "cmditch/elm-bigint": "2.0.1",
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "elm/json": "1.1.3",
            "elm/parser": "1.1.0",
            "elm-community/result-extra": "2.4.0"
        },
        "indirect": {
            "elm/regex": "1.0.0",
            "elm-community/list-extra": "8.7.0",
            "elm-community/maybe-extra": "5.3.0",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm-explorations/test": "2.2.0"
        },
        "indirect": {
            "elm/html": "1.0.0",
            "elm/random": "1.0.0",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    }
}
```

It looks like the package identifiers correspond to GitHub usernames and repository names, and the version identifiers correspond to tags in those repositories.

Researching the Elm compiler at commit [`1bd5b36915a38335195ca7792fe3995f53d84d5e`](https://github.com/elm/compiler/tree/1bd5b36915a38335195ca7792fe3995f53d84d5e) confirms that this is true for packages published through Elm's tooling:

- [`Elm.Package.Name`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Package.hs#L57-L68) stores an `author` and a `project`. Its [parser requires the two parts to be separated by `/`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Package.hs#L313-L319), and [`toUrl` renders the name as `author/project`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Package.hs#L89-L97).
- [`Elm.Version.Version`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Version.hs#L39-L45) contains major, minor, and patch numbers. The [parser accepts `major.minor.patch`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Version.hs#L146-L154), and [`toChars` renders that same form](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Elm/Version.hs#L97-L100).
- When publishing, the compiler [first runs `git show` with the rendered version as the revision](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/terminal/src/Publish.hs#L222-L231). It then checks `https://api.github.com/repos/{author}/{project}/git/refs/tags/{version}` using the rendered package name and version ([`verifyTag` and `toTagUrl`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/terminal/src/Publish.hs#L222-L243)).
- The compiler subsequently downloads `https://github.com/{author}/{project}/zipball/{version}/` and verifies that the downloaded package builds ([`verifyZip` and `toZipUrl`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/terminal/src/Publish.hs#L269-L293)). Therefore the package's author and project are the GitHub repository owner and name, while the Elm version string is used verbatim as its Git tag.
- Finally, publication [registers the package name, version, tag commit hash, and GitHub archive hash](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/terminal/src/Publish.hs#L379-L395) with the Elm package server.

Package installation is indirect rather than a fresh reconstruction of the GitHub URL. The compiler defines its package service as [`https://package.elm-lang.org`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/builder/src/Deps/Website.hs#L14-L26), obtains the package registry from [`/all-packages`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/builder/src/Deps/Registry.hs#L63-L72), and fetches `/packages/{author}/{project}/{version}/endpoint.json`. It decodes the URL and expected hash from that response, downloads the archive from the returned URL, and verifies its hash ([`downloadPackage`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/builder/src/Elm/Details.hs#L748-L779)). For the example above, [`elm-community/result-extra` version `2.4.0` currently resolves](https://package.elm-lang.org/packages/elm-community/result-extra/2.4.0/endpoint.json) to the GitHub zipball URL ending in `/elm-community/result-extra/zipball/2.4.0/`.

## Design Idea

Since these names are already on GitHub, can we connect directly to GitHub and avoid additional dependencies?

Zig's approach seems simple, and I like that. At <https://ziglang.org/download/0.11.0/release-notes.html> they write:

> There is no "official" package repository: packages are simply arbitrary directory trees which can be local directories or archives from the Internet.

### Tag Mutability And Caching

Since we want to support regular `elm.json` files, we will have to support git tags as references. A Git tag as a reference means the author can change the contents at any time. This mutability creates complications, and there are different ways to address it, so let's clarify how we want to handle it for the initial implementation.

To improve response times and reduce network roundtrips, we want to cache what a tag points to.

We might want to eventually offer a command to reset a cache that maps a tag to contents, or have it refresh based on time. However, for the initial version we require the user to clear the file system directory containing the mappings from tag names to contents.

Also in the future, we might want to offer a command which takes the cache from the current machine or user profile and packages that up for easy consumption without internet connection on a build server.

### Reusing Existing Cache in 2026

We already have a caching implementation that stores the package contents in files like `cmditch-elm-bigint@2.0.1.zip`

For now, we want to reuse most of that. However, we change the file path to `github.com/cmditch/elm-bigint@2.0.1.zip`

### Future

#### Future Tag Cache

For example, for the package `cmditch/elm-bigint`, we will place the tags in the directory `<pine-cache-dir>/git-tag/github.com/cmditch/elm-bigint`, so there is one directory for each repository on GitHub.

In this directory, we will use the name of the git tag as file name. The file contains the commit SHA as printed by the git CLI.

#### Future Git Object Cache

For git objects, we will use a shared cache of loose objects (canonical encoding) in directory `<pine-cache-dir>/git/objects` with the same subdirectory division (`00` - `ff`) we find in local git repositories.
