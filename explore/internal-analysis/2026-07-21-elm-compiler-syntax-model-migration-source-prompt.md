# 2026-07-21-elm-compiler-syntax-model-migration-source-prompt

Following the prompt given to GitHub Copilot in a new session:

```
Refactor the Elm compiler in .NET to use the abstract syntax model in the later compilation stages.
ElmSyntaxOptimization.cs, CompilationPipelineStageResults and related functions must use the abstract syntax model from namespace `Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract`

Canonicalization, as an earlier stage, must use the concrete syntax model (e.g. `Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression`)

So far, many parts of that compiler use `Stil4mElmSyntax7` because that was the first Elm syntax model historically implemented. However, in the future this compiler must only use the concrete and the abstract syntax models, and `Stil4mElmSyntax7` will only be kept in the repo for other applications and interfaces that use that specific model.

One of the reasons we use an abstract syntax model in the later compiler stages is that this is much more cache-friendly: Cache entries keyed on source syntax are valid independent of where in a source text a declaration appears, so any shift or formatting does not invalidate such caches. This should help speed up compilation a lot. Currently no such cache is implemented, but we will get caching for free when reimplementing the Elm compiler in Pine (in Elm) in the future.

Create a markdown file with an analysis of this projected change, as well as an implementation plan.

The analysis and implementation plan must also cover:

+ where we currently waste resources by converting between syntax models repeatedly in two directions.
+ where we will need to convert between syntax models after implementing the changes described above.
+ what are reasons and likeliness we will need or want another model that has type information attached for future expansions, such as expansion of type inference and introduction of complete type checking analog to the original Elm compiler. What would be implications of such an additional model and where would it be placed?
+ what would be pros and cons of having canonicalization also use the concrete syntax model.

Note: In case we need any locations of source tokens to generate error messages, we can convert a graph-based location description that we would get out of an abstract syntax model (e.g. index of list item, index or name of let declaration) back to source ranges and locations by combining such a path with the concrete syntax model. This way, we can combine the benefits of the abstract syntax model with the ability to report exact source locations, if that becomes necessary.
```

GitHub Copilot then produced `2026-07-21-elm-compiler-abstract-syntax-model-migration.md`

