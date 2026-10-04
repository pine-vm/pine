# 2026-10-04 Intro Elm Test Profiling

## Motivation

We want to use existing Elm tests to explore further opportunities to improve efficiency and response times of Elm apps. Using an Elm test as an entry point is an easy way to model a scenario to profile as a proxy for production workloads, such as the Elm language server.

## Elm Compiler Expansion to Support Attribution

Expand the Elm compiler to return a map of compiled declarations to support attribution of Pine expressions.

+ Expand `ElmCompiler.CompileInteractiveEnvironment` and other APIs where appropriate to return all information necessary to support attribution of executed Pine code to source Elm declarations.
+ Elm declaration identifier consists of:
  + module name
  + module-level declaration name
  + list of local declaration names
  + specialization model describing how that particular instance was specialized from the source. Elm compiler returns this in a structured strongly typed form, rendering to a string happens only when compiling a instrumentation report.
+ Note: Names on the Elm side are not unique: A top-level declaration can contain multiple local declarations with the same name, and declarations originating from packages are not qualified but merged into the same namespace as application modules.

## CLI Command Elm Test Profile

+ Add a new CLI command `elm  test  profile  instrument`

### Instrument Report

### Option `--output-location`

Specifies a location to write the report files to. Defaults to the current directory.

+ If the given location string ends with a forward slash or backslash its meant to be a directory.
+ If the given location exists as a directory, its meant to be a directory.
+ If the `--output-form` is `files`, its meant to be a directory.
+ If the `--output-form` is `zip` and the given location looks like a directory, generate a file name that is prefixed with current data and time in format `yyyy-mm-ddTHH-MM-ss`.

### Option `--output-form`

+ `zip` (default): bundle the report files in a zip archive.
+ `files`: write the report files individually to the file system.

## Smoke Integration Test

+ Add simple, lightweight integration test to verify the CLI command works. Use output to ZIP Archive and load from ZIP Archive to assert expected contents.
