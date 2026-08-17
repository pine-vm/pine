# Pine CLI Elm Test Command

Run Elm tests using the [`elm-explorations/test` package](https://github.com/elm-explorations/test)

## Planned

From Simons PR at <https://github.com/elm-explorations/test/pull/260>

> This deprecates the `Test.Runner` module and adds `Test.RunnerV2`. This is a backwards-compatible change.
> 
> node-test-runner PR: https://github.com/rtfeldman/node-test-runner/pull/686. The idea is to _require_ a recent enough version of elm-explorations/test in node-test-runner (updating the package in projects is a no-brainer), but keep compatibility in this package, so we don’t leave elm-test-rs behind.
> 
> Changes in `Test.RunnerV2`:
> 
> - Removed seed distribution. All fuzz tests now run with the same seed. This means that moving a test, adding another fuzz test, or commenting out a fuzz test no longer can cause fuzz tests to behave differently, even though you passed a fixed seed. This also got rid of a lot of complexity.
> - Unit tests and fuzz tests are now returned separately. This allows a runner to more cleverly distribute tests across threads. For example, spread the fuzz tests on multiple threads, while running all unit tests single threaded. This also allowed for more precise types. Previously it looked like you could get a `DistributionReport` for unit tests, while in practice you can’t, for example.
> - Make it possible for runners to collect `Debug.log` from the run that caused a fuzz test to fail (and ignore `Debug.log` from exploratory runs).
> - Make it possible for runners to run fuzz tests with the “fuzzer ints” (`RandomRun`) from a previous failure, allowing for an instant reproduction of a fuzz test failure.
