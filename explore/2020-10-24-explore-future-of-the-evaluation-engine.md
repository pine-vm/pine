# 2020-10-24 Explore Future of the Evaluation Engine

Begin to explore an evaluation engine that considers more of the observations from applications than the last iteration:

+ Eager evaluation causes long response times: Eager evaluation causes response times to multiply for many requests in common backend scenarios. For many requests, less than 10% of the work caused is to compute the response. Often the effects from a user's request are limited to their account. We can continue to serve other users even when one user has triggered a more expensive computation on their account.
+ Caching can reduce expenses in processing time to a fraction in many scenarios.

To reduce operating expenses and improve response times, reuse a standard engine for non-strict evaluation that supports optimizing based on the actual workload.

To speed up the development, consider which parts we can skip in earlier versions:

+ We still only consider valid Elm programs for now, which means we skip all work to derive compilation error messages.
+ Type inference: Type inference is not needed to store values (e.g., persisting application state). We need type inference at interfaces. Initially, we rely on users supplying type annotations with their functions and making a bit of superficial type inference at the interfaces. Looking at the example apps, we see the types of observed values are usually type `String` or a tuple with the observed element being a String.

