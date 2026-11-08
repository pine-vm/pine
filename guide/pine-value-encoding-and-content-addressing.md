# Pine Value Encoding and Content Addressing

Pine values are serialized in various places for purposes including:

+ Storing in a file system to load again in the future.
+ Exchange with other nodes across a network.
+ Derivation of cryptographic hashes for global identification and cache keys.

These representations are independent of the storage or transport medium: the same encoded byte sequence can be stored in a file or carried in a network message.

## Canonical Representation

The canonical representation of a Pine value follows the content-addressed model of loose objects in Git:
Each value is represented independently, and lists refer to their items using a cryptographic hashes.

The identifier hash from this representation is the durable, canonical name for the value.

This representation offers high storage efficiency where we need to store many trees with overlapping nodes.
A disadvantage of this representation is overhead for the computation of the hashes.

## Self-contained Representation

On the other end of the spectrum, we can completely avoid any hash based references, by encoding all nodes that make up a composite value in a single byte sequence. To reference a value encoded within that sequence, we only need an index.

## Deterministic Serialization

We derive the stable hashes we need for cache keys from a stable sequence of bytes encoding the value.

When packing multiple nodes into a single byte sequence, we use a deterministic ordering to ensure the same value is encoded in the same byte sequence.

## Mixed Representation

When computing hashes for cache keys for `Eval` results, neither of the two extremes are optimal.

Hashing for cache keys uses a representation that mixes both external references via hashes and internal references via indices.

Mixed representation also means we have many different representation identifiers for the same value/composition.

Therefore we couple the policy deciding between internal and external reference with a cache scope.

## Eval Nodes

In contrast to Git, the serialized representation also supports using `Eval` nodes.

Usage of `Eval`, combined with specialized internal representations of values enables reducing overhead for computing representations for exchange.

Since an `Eval` could in general also contain unbounded work, nodes accepting messages from remote systems typically enforce custom limitations and reject some instances of `Eval`.
A very common limitation is to only accept a fixed set of encoded expressions.
