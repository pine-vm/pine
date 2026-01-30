# Elm Syntax Model and Parser

There are multiple applications depending on the parsing of Elm syntax, and the syntax model and parser are designed to accommodate their varied needs.

The Elm syntax model and Elm syntax parser in Pine support the following applications:

+ Compilation of Elm programs, translation into a lower-level form.

+ Generation of diagnostics pointing to parts of the source code, including all compilation errors.

+ Production of semantic information for functionality in language servers and IDEs (e.g., ‘Find All References’, ‘Rename Symbol’)

+ Formatting and pretty printing: Need to preserve literal forms. Some numbers and strings can be written in more than one form, and we do not always want the formatter to erase these differences.



To ensure application developers have a good experience, we have additional requirements for usability and robustness:

+ If multiple declarations in a file contain syntax errors, we want to report all of them, not just one.

+ Features like formatting or analyzing code should not stop working just because there is a syntax error in a module.

To make this work, the parser does not stop when it finds a syntax error. Instead, it notes where the error is and which part of the code is affected, then continues through the rest of the module. With this partially finished syntax model, the formatting tool can still format the module's parts without errors, while preserving incomplete code sections, as it does with comments.



tags:area-tooling-authors