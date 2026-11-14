# 2026-11-14 Reuse Pine Value Binary Encoding

Following the exploration recorded in [2026-11-13-reuse-pine-value-binary-encoding-for-syntax-model.md](./2026-11-13-reuse-pine-value-binary-encoding-for-syntax-model.md), we now implement this generalization to binary encodings for tree structures.

It turns out yesterdays spec was not precise enough, and there are some ambiguities and challenges remaining:

## Clarifying Environment to `Eval`

An implementer could misunderstand the goal for the environment given to `Eval`, and opt to use a tree shape corresponding to the canonical representation. This is not necessary and not desired. For better runtime efficiency, we prefer a shallow list.

Lets look at an application expression from the target syntax model: It contains a 'function' property and an 'arguments' property. Thats two items and the second item is itself a list. For the shallow representation, we define the 'function' be at index zero, and all following items to be the 'arguments'.

The 'function' and 'argument' items are natural candidates to remain individual nodes, since these are of type `NodeType`.

In other cases, the node in the target model has more than one component with varying size. To solve this, we use items encoding integers to let the decoder know how many following items belong to the next component.

For a concrete example from the AST, consider `NamedPattern QualifiedNameRef (List Pattern)` in [ElmSyntax.Abstract.Pattern](../implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Abstract/Pattern.elm). Its `QualifiedNameRef` contains a `moduleName : List String` and a `name : String`. If we flatten that record as well, the environment needs to carry a variable-length module path, the constructor name, and a variable-length list of pattern arguments.

For the Elm pattern `Acme.Tree.Branch x y`, these components are:

```text
moduleName = ["Acme", "Tree"]
name       = "Branch"
arguments  = [VarPattern "x", VarPattern "y"]
```

One possible shallow layout prefixes the module path with its length:

```text
Environment = [2, "Acme", "Tree", "Branch", p_x, p_y]
```

Here, `p_x` and `p_y` are the individual `Pattern` nodes for `VarPattern "x"` and `VarPattern "y"`, and `2` stands for an item encoding the integer two. The decoder reads that count, takes the next two items as the module path, takes the next item as the constructor name, and takes the remaining items as pattern arguments. The count explicitly marks where the module path ends and the constructor name begins; both contain strings, so a change of item type does not mark that boundary. No argument count is needed because the arguments occupy the remainder.

The same layout supports an empty module path and no arguments: the unqualified pattern `Nothing` becomes `[0, "Nothing"]`.

This preserves the component boundaries without requiring an intermediate module-path list node or a separate environment node for the qualified-name record. Collections and records required by the retained result model may still need storage. The particular integer encoding is left to the implementation; the goal here is to illustrate a shallow environment layout that enables reuse of the decoder while minimizing temporary heap allocations.

An implementation might instead keep `QualifiedNameRef` as a separate internal node if it is already a separate heap object in the retained model. In that case, its constructor can decode directly to that retained object, rather than introducing a temporary wrapper. For example, the two constructor environments could be:

```text
QualifiedNameRef environment = ["Acme", "Tree", "Branch"]
NamedPattern environment    = [q, p_x, p_y]
```

Here, `q` is the decoded `QualifiedNameRef` object. Its constructor takes the final item as the name and all preceding items as the module path. The `NamedPattern` constructor takes its first item as the qualified name and all remaining items as pattern arguments. Neither environment needs a length item with this layout.

The decoded qualified-name object can also be stored in the internal reference dictionary and reused by multiple patterns. The choice of where to stop flattening should therefore follow the retained model and allocation costs, not the canonical representation: keeping a node that is already a retained heap object need not add a temporary allocation.

## Generalizing Reuse of Decoded Items by Widening the Internal Type

Using the `NodeType` of the target model also for items in the internal dictionary of decoded value might not be compatible with our goal of minimizing allocation overhead.

A second reason for storing more diverse values in that dictionary is to enable their reuse in multiple `NodeType` nodes.

An example would be the `String` values used in the mentioned AST model. `String` is not a variant of the `NodeType` (`Expression`) in this case, but it occurs in properties of `NodeType` variants of the retained result model.

One way to solve this is to relax the type for the decoded entries in the internal dictionary. Furthermore, efficient decoding of these (avoiding short-lived heap allocations) can require adding more decoding expressions to use in the `Eval` nodes.

When decoding admits a larger set of types for the internal dictionary, the constructor method needs to do some type checking and reject the `Eval` instance in case of a type mismatch.

Note: On a platform like .NET, value type entries could require boxing, leading to accidental heap allocations. In such a case its preferable to use a decoding mapping directly from the raw byte sequence up to the first reference-typed entry in the retained model, via nested flattening.

To implement this solution, we add a second type argument `DictItemType` to the generic decoder.

