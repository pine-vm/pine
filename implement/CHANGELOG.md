# Changelog - implement

## 2024-03-10 - Pine to CS Compiler - Change return type of compiled Pine expressions

Changed the return type from the compiled representations of expressions from `Result<string, PineValue>` to `PineValue`.
Cases that previously resulted in an error Result are now mapped to runtime exceptions locally.

In which cases did we use the 'Err' case of that 'Result' type?
In the past, kernel functions could return errors, but in 2023, we changed kernel functions to never fail and always return a plain `PineValue`. Since then, only parsing errors from `ParseAndEval` expressions could return an error. The original motivation to use a `Result` was to support finding causes of program errors. However, it might be better to repeat the execution in an interpreter configured for that purpose instead of mixing this support into the compiled code.

## 2024-03-15 Pine to CS Compiler - Introduce specialized parameter lists

Up to March, all the C# methods generated to represent compiled expressions shared the same parameter list. This parameter list contained a single entry for the environment corresponding to the `environment` property of the Pine ParseAndEval expression.
The change 2024-03-15 introduces specialized parameter lists. The compiler now generates parameter lists that result in lower runtime expenses. Since these specialized parameters correspond to parts of the environment, the invoked function needs less processing to retrieve them. At the call sites, the specialized parameters result in fewer heap allocations and processing to package all used parts into a single argument. Besides the better runtime efficiency, another benefit is that the generated C# code is less cluttered and easier to read.

## 2024-05-06 - Elm Compiler - Add runtime type checks and crashes for invalid record access or update

+ Added test cases to check we get specific error messages for four distinct classes of errors: Record access on non-record value, record access with invalid field name, record update on non-record value, and record update with invalid field name.
+ Change the Elm compiler implementation to emit runtime checks to distinguish these four cases and crash the program with the corresponding message. (Checking whether a value represents a record is possible because we use wrap record fields in the "Elm_Record" tag)

