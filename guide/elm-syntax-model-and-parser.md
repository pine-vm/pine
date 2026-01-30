# Elm Syntax Model and Parser

There are multiple applications depending on the parsing of Elm syntax, and the syntax model and parser are designed to accommodate their varied needs.

The Elm syntax model and Elm syntax parser in Pine support the following applications:

+ Compilation of Elm programs, translation into a lower-level form.

+ Generation of diagnostics pointing to parts of the source code, including all compilation errors.

+ Production of semantic information for functionality in language servers and IDEs (e.g., ‘Find All References’, ‘Rename Symbol’)

+ Formatting and pretty printing: Need to preserve literal forms. Some numbers and strings can be written in more than one form, and we do not always want the formatter to erase these differences.



To ensure application developers have a good experience, we have additional requirements for usability and robustness:

+ If multiple declarations in a file contain syntax errors, we want to report all of them, not just one.

+ Features such as code formatting or code analysis should not stop working just because a module contains a syntax error.

To make this work, the parser does not stop when it finds a syntax error. Instead, it notes where the error is and which part of the code is affected, then continues through the rest of the module. With this partially finished syntax model, the formatting tool can still format the module's parts without errors, while preserving incomplete code sections, as it does with comments.


## Origin and Evolution

Initially, we used parsed Elm syntax only for compilation. We used the parser and syntax model from [`stil4m/elm-syntax`](https://github.com/stil4m/elm-syntax/tree/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax) version 7 for a long time, and it has provided all the information we needed for the Elm compiler. In 2025, we began rendering generated Elm syntax for snapshot tests and formatting Elm module files.

Starting from `stil4m/elm-syntax` v7, syntax model and parser evolved to meet the needs of the applications detailed above:

+ <https://github.com/pine-vm/pine/blob/06c2abb959d818589f1f2579a1c2a3af7b003842/explore/2025-12-12-elm-formatter-and-syntax-model-design-challenge.md>
+ <https://github.com/pine-vm/pine/blob/06c2abb959d818589f1f2579a1c2a3af7b003842/guide/stil4m-concretized-syntax-model.md>
+ <https://discourse.elm-lang.org/t/elm-syntax-the-rough-edges/10507>


tags:area-tooling-authors