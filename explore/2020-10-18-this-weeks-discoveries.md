# 2020-10-18 This Week's Discoveries

Some discoveries around Elm-fullstack / pineVM from this week.

## Elm Editor - Composition Model and User Interface

(This is for the scenarios where we load large files or file trees fixed by a content hash into our Elm app. Think training data sets)

Previously there was an idea to model content from the internet as a lazily evaluated tree in a particular module (Probably under the `ElmFullstackCompilerInterface` prefix).

However, today I found an alternative that looks nicer to me so far: We could reuse the compiler interface to read file contents instead (Module `ElmFullstackCompilerInterface.SourceFiles`). We don't need to include the actual blob contents with the serialized representation as long as we have another way to find the given hash's contents. The user can specify a set of URLs into git repositories (like https://github.com/elm-fullstack/elm-fullstack/tree/b0d1087e30b0e67ad2dbc3e8b9ab9ec14dbdff16) to use as dictionaries. For any node hash, we can search these dictionaries to find the file contents.
The system shows the user an error message if any of the specified URLs to git repositories are not valid anymore.
This approach to modeling an experiment implies we can reuse the same infrastructure used to share program code files: The lookup works the same way for all the referenced elements. There is no difference anymore if we reference them in `ElmFullstackCompilerInterface.SourceFiles` or use them as input for `elm make`.

## Unify Inspection Scenarios with Interactive Scenarios

The inspection scenarios are about inspecting the process of evaluating a given Elm expression. Part of inspection is to help newcomers learn the language: By reading the intermediate values in an evaluation tree, we can learn much more than reading the whole program code. The inspection supports learning about individual language elements by focusing on the enclosing expression. For advanced users, inspection helps in understanding the inner workings of large programs faster.

In contrast to the interactive scenarios, no inspection scenario is modeled in the tests so far. Beginning to build the framework to test inspection scenarios and modeling the first scenarios revealed similarities to the interactive scenarios. We could model inspection as part of the interactive environment.

A particular name in the interactive environment could provide access to the inspection of the evaluation in the last submission. An alternative way to access inspection would be to switch to a containing session that contains a reflection of the whole previous session.
