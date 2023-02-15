## Backlog

### Elm Interactive

#### Detect and display images in Elm Interactive

An idea that came up during a session introducing the basics of programming. We were playing around with simple expressions in Elm Explorer. Then I heard the question: 'how do I make an image?' I had not thought about supporting images before. Now I suspect it might be simple: For example, we could do this when a submission results in a [sequence of bytes](https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Bytes.elm). Many popular image file formats seem to have a specific header at the beginning, which should help build a cheap heuristic. For integrations where the UI runs in the web browser, we might get away without making a parser, using a `img` tag in the DOM to offload parsing to the browser.

### Application Operation

+ Support application to configure tradeoff between the cost of persisting and cost of restoring.
+ Reduce load on storage: Provide automation to remove reductions which are not needed anymore from the store.

### Collaboration on Elm-Time

+ Simplify modeling of tests: Support modeling Elm app using a string for the main (and only) module contents, using the default `elm.json`.

Ensure people can easily understand for a given change how well it would fit into the project:

+ Increase test coverage: The process store can reliably model values as offered on the interface. (E.g. line-breaks (or similar UTF sequences) in the serialized event do not damage the composition store)
