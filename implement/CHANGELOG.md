# Changelog - implement

## 2024-03-10 - Change return type of compiled Pine expressions

Changed the return type from the compiled representations of expressions from `Result<string, PineValue>` to `PineValue`.
Cases that previously resulted in an error Result are now mapped to runtime exceptions locally.

In which cases did we use the 'Err' case of that 'Result' type?
In the past, kernel functions could return errors, but in 2023, we changed kernel functions to never fail and always return a plain `PineValue`. Since then, only parsing errors from `ParseAndEval` expressions could return an error. The original motivation to use a `Result` was to support finding causes of program errors. However, it might be better to repeat the execution in an interpreter configured for that purpose instead of mixing this support into the compiled code.
