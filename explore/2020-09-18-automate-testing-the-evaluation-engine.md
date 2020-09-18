# 2020-09-18 Automate Testing the Evaluation Engine

After the recent work on automated testing, new questions emerged. When building earlier automated tests for the Elm evaluation, I modeled scenarios as an Elm app together with the convention of using a declaration with a particular name in a specific module as the entry point. This declaration needs to be a value, not a function.
In the Elm based branch of the Evaluation engine, I used a different scenario model: The text of the expression to evaluate, with an optional list of Elm modules.

As the consolidation of the two approaches to Elm evaluation made progress recently, I started to wonder: How to further simplify the scenario models? At the moment, we have some scenarios modeled in each of these two ways. Is any of those two good enough mid-term? Which is the interface closer to productive use? When illustrating how the language works, I often turned to the REPL that comes with the elm executable file. The REPL gets us fast feedback on our ideas. Is that correct syntax? Do the types match in this snippet?

Is there an even more effective way of training than working in such a REPL? At the moment, I do not remember one. The latter approach to modeling the test scenarios seems at least closer to the REPL use case. Mid-term, I see modeling evolve to cover the whole REPL scenario. That also means switching from a single expression to a sequence. Besides, the REPL also supports entering declarations, not just expressions. Some of the functions I remember for a REPL are:

+ Option to seed the session with an Elm app. I often want to explore based on the functions I already have in an app. Check that all modules compile to warn the user early? Allow the user to use an app as a seed even if not all modules currently compile. That should work as long as the user does not reference any broken module.
+ Option to expose everything in the modules of the seed app. But only as seen from the REPL, this should not affect the outcome of compiling module of the app.
+ Collect and export the declarations from a REPL session.

Looking back at the text above, I notice a lot of mentioning 'compilation'. What I meant was checking the code and computing the errors and static analysis diagnostics. No emitting implied.

For context, a typical case using a REPL seems to seed it not just from an app's code but from a node in an evaluation tree. So there will be bindings from functions and let blocks too. How do we model picking that node? It seems to be too far off to worry about this today.
