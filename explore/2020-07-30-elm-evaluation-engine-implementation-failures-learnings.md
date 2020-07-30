# 2020-07-30 Elm Evaluation Engine Implementation Failures/Learnings

On the day before, made progress with implementing the Elm evaluation engine, adding more supported scenarios (https://github.com/elm-fullstack/elm-fullstack/commit/5f41e77d8ea323a99de788dd36ddcbe63becccf9, https://github.com/elm-fullstack/elm-fullstack/commit/84f44abf92f86ee03a0a6accc8bf6afa031e300f)

On Tuesday, I wanted to expand the engine further to support partial application. Started by coding the Elm app defining the scenario as illustrated in commit https://github.com/elm-fullstack/elm-fullstack/commit/d60c6ca4aa9ef41266d35f2ef48ba5c2737c6b1b

After spending five hours adapting the evaluation engine to support this scenario, I still did not have something I wanted to merge into main.

Of the versions of the productive side that I tested running the whole test scenario, some failed in a way that would have been detected earlier by using proper sum types as we have them in Elm. Another factor in wasting time was pondering how much strictness to use in the evaluation for now. (It looks like mid-term the evaluation will be much less strict than what we get using the official Elm-compiler) Considering different levels of strictness for the next merged version has increased the number of experiments and implementations on Tuesday.

Overall, it felt somewhat exhausting. Now I want to try to change the approach and see if I make progress faster when switching to implementation in Elm.
Another related change I see coming is shifting priorities to scenarios for inspection, driven by the applications in DevTools. Switching to Elm should also make it easier to get the coverage of core/kernel functions for the DevTools scenarios.
