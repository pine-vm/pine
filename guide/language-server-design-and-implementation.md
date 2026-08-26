# Language Server Design and Implementation

## Optimizations for Response Times

The Pine language server implements various optimizations for response times. For one, it benefits from the general memoization infrastructure available in Pine, which helps both efficiency and response times. Beyond that, the language server optimizes response times specifically by distributing work across multiple threads.

The ways we employ concurrent processing of the requests from the language clients are broadly grouped into three categories:

+ Parallel computation for read-only requests ('queries')
+ Partial concurrent execution of mutating requests ('transactions')
+ Lenient evaluation

> Note: Another approach to optimizing response times is the parallel execution of work inside of a single request (think `List.map`), but we don't cover that here.

### Maintaining Simplicity for Application Programmers

Crucially, none of these optimizations depend on the program code that implements the language services. It's still plain Elm code without any notion of concurrency.

Identifying parallelizable work, enabling concurrency, and choosing synchronization strategies, etc., are automated by the compiler and virtual machine.

This is also important because the same approach to improving response times through concurrency can be applied to other Elm applications where optimizing for faster responses or higher throughput is desirable.

### Parallel Processing Read-Only Requests

Some requests from language clients will not change the application state/database.

One way to prove this is to have the Elm app process the request, then check whether the returned state is the same as the previous state.

How can we use this knowledge to improve response times? One way to do that is through speculative execution: instead of waiting for each request to finish processing, we could start processing it as soon as it arrives. As long as none of these requests executed concurrently produces a new state, the results are the same as with a serial execution. When one request's processing produces a new state, this can invalidate the results from processes started for a previous state.

These invalidated results are then discarded, and processing of the corresponding requests is restarted from the latest state.

### Partial Concurrent Execution of Mutating Requests

For some combinations of requests, we need to go a bit further to capitalize on concurrent execution. 

For example, a client might change the contents of multiple documents, sending a [`DidChangeTextDocument` Notification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_didChange) for each changed document.

When opening a workspace, we are in a similar situation: the new document content does not arrive via a `textDocument_didChange` request, but we need to process many new document contents to support queries like `Go to Definition` or `Hover`.

The processing of new document content also entails parsing from a string into a syntax tree. The language service then produces a new state to make a parsed representation of the document available in a dictionary for future queries.

Since the overall handling of new document content updates the Elm application state, we must eventually run all of these requests in sequence.

Meanwhile, the part parsing the syntax tree is a great candidate for concurrent processing:

+ It does not depend on the previous state, only on the new document content.
+ It typically makes up more than 90 percent of the total cost of processing the request.

Can we offload the parsing to a separate thread, even though the overall request requires serial processing?

It turns out, we can, and it's not even complicated. Since the caching functionality enables reuse of results across threads, we implement this concurrent execution in two stages as follows:

+ First, we process the new document content event on a separate thread. While processing the event, the interpreter creates cache entries for the computationally expensive parts, as usual. These cache entries are merged into a shared dictionary, where they will be available for future event processing.
+ Second, we process the new document content event again, based on the Elm app's latest state. In this second pass, the interpreter will encounter the same function application again and pick up the previously computed result from the cache.

There are multiple ways to implement the merging of cache entries:

+ One is to use fine-grained synchronization on every cache write.
+ Another approach is to direct all new cache entries to a thread-owned collection, then merge them once processing of the entire event is complete.

In any case, this approach means that some work, such as building a new dictionary in the overall app state, is done multiple times, causing overhead in CPU cycles.

### Lenient Evaluation

The tool of lenient evaluation can help improve both response times and efficiency.

TODO
