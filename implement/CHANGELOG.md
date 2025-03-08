# Pine Changelog - Implement

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

## 2024-07-14 - (R)Evolution of Code Analysis

The previous approach to code analysis aimed at identifying the parts of the environment used to parse expressions. Looking closer at optimizing execution in PineVM this month, it became clear we want to add more parts of the environment constraints collected for specialized compilation. Expanding the environment constraint to cover parts that conditions of conditional expressions depend on allows us to resolve the condition when compiling the specialized representation. This compile-time reduction saves us the check at runtime and shrinks the code size because we eliminate some branches.

But how do we expand the code analysis to cover the condition expressions? Initially, I expected to expand the existing code, adding something similar to what we did so far to identify the subexpressions used in parsing. However, this commit adds an approach to collecting the environment constraints that is entirely different from the one used so far. Instead of looking into the contents of the expression and recursively descending to collect all relevant paths, this new approach does not look into the expression. Instead, it generates classes only by computing intersections of the different concrete environments observed for a given expression. It then computes a score for each class by counting how many of the observed concrete environments match the class. This approach seems preferable for now because it is much simpler. However, the concrete implementation still needs to be refined.

## 2024-08-31 - Optimize Pine Expression Encoding

So far, I expected to develop the standard expression encoding into a form completely representable in a file system tree. With such an encoding, we could take a tree from the file system and directly feed it into Parse&Eval. However, I changed my mind about the tradeoffs and now change the encoding in the opposite direction, optimizing for compactness and symmetry with the Elm value encoding.
If we still want to (re)use file systems to model syntax trees, we will add a mapping for such a form later, meaning there would be another hash code for that encoding.

## 2024-09-19 - Basic Support for Elm Float Type

Expand the Elm compiler and core libraries to support `Float` literals and `String.toFloat`

## 2024-10-05 - Basic Support for Elm Command-Line Applications

+ Added a framework to integrate the various interfaces we want to use in command-line interface applications (`Platform.CommandLineApp`), including stdIn/stdOut/stdErr streams and environment variables.
+ Introduced the `pine  run` command as a common way to run an app.

## 2024-10-26 - Added VS Code Extension and Language Server

Published the first version of the Elm developer tools VS Code extension, with a command to format Elm modules.

## 2024-12-08 - Expanded VS Code Extension and Language Server

+ Added feature: Completions: Shows completion suggestions matching the current context
+ Added feature: Hover tips: Shows type annotations and documentation for a type alias, module, custom type or function

## 2024-12-15 - Expanded VS Code Extension and Language Server

+ Added feature: 'Go to Definition'

## 2024-12-19 - Expanded VS Code Extension and Language Server

+ Added feature: 'Find All References'

## 2024-12-22 - Expanded VS Code Extension and Language Server

+ Added feature: 'Rename Symbol'

## 2025-01-03 - Introduced New Execution Engine And IR

+ Changed the internal intermediate representation used to run Pine programs, introducing a new compiler for that IR and a corresponding execution engine.

## 2025-03-06 - Freed Web Service Platform from JS

+ Resolved dependency on JavaScript in the web service platform. (https://github.com/pine-vm/pine/issues/20#issuecomment-1816129166)

## 2025-03-08 - Resolved Remaining Dependencies on V8 JavaScript Engine 🚀🔥✅

+ Resolved the last usage of the V8 JavaScript engine by switching applications over to Pine.

