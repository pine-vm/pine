# Sequential IR boolean branch forwarding

## Summary

The sequence in frame
[`007-7b9871f5`](2026-09-09-document-symbol-complex-syntax-sequential-ir-report.generated.md#frame-007-7b9871f5)
computes a Boolean with a switch and immediately branches on that Boolean:

```text
195: Int_Mul_Const (4)
196: Switch_Jump_If_Slice_Skip_Var_Equal_Const (53)
  case "A" ... "Z", "_", "a" ... "z": jump (3, 199)
197: Push_Literal (False)
198: Jump_Const (2, 200)
199: Push_Literal (True)
200: Jump_If_Equal_Const (True, 25, 225)
```

This is a Boolean materialization diamond followed by a Boolean-consuming
branch. The intermediate `False` or `True` value is not otherwise used. The
switch can therefore branch directly to the final destinations:

```text
195: Int_Mul_Const (4)
196: Switch_Jump_If_Slice_Skip_Var_Equal_Const (53)
  case "A" ... "Z", "_", "a" ... "z": jump (to former 225)
     otherwise: fall through to former 201
```

The existing switch instruction already has the required semantics: every
case has a destination and a miss falls through. No new VM instruction is
needed for this first optimization.

The safest first implementation is a control-flow optimization over
`PineControlFlowGraph`. It should recognize a conditional whose input is a
Boolean selected solely by predecessor edges, redirect those edges to the
conditional's successors, remove the now-unused Boolean-producing blocks, and
then lower the graph. A smaller compiler-local implementation is possible, but
it would duplicate control-flow reasoning and offset bookkeeping. Pine
expression reduction can cover selected source shapes, but is not the best
general layer for this problem because Pine expressions are trees and do not
make shared control-flow destinations explicit.

There is a second, independent opportunity in this example. The VM currently
tests the 53 switch literals one at a time. The character set is representable
as three ranges (`A`-`Z`, `_`, and `a`-`z`), so a range-membership instruction
or a better set-membership implementation may save substantially more work
than branch forwarding. That change has more semantic and implementation risk
and should be measured separately.

## The observed case

Frame `007-7b9871f5` has 439 static instructions and executed 162 instructions
in the captured invocation. At the relevant location:

- instruction 196 consumes the source value and variable skip count;
- a matching letter or underscore jumps to instruction 199;
- a miss falls through to instruction 197;
- both paths produce an encoded Pine Boolean;
- instruction 200 immediately consumes that Boolean and selects instruction
  225 for `True` or instruction 201 for `False`.

The effective control-flow graph is:

```text
                   +----------------+
                   | membership     |
                   | switch         |
                   +---+--------+---+
                     no|        |yes
                       v        v
                 +---------+  +---------+
                 | push    |  | push    |
                 | False   |  | True    |
                 +----+----+  +----+----+
                      +-----+-----+
                            v
                   +----------------+
                   | branch if True |
                   +------+---------+
                     false| |true
                          v v
                  former 201/225
```

The Boolean is equivalent to a one-bit block parameter (an SSA `phi` value)
whose incoming values are constants:

```text
isMember = phi(False from miss, True from match)
branch isMember, trueTarget, falseTarget
```

The branch can be threaded through that parameter:

```text
membership switch:
  match -> trueTarget
  miss  -> falseTarget
```

This is valid when the literal blocks and the Boolean value have no other
observable use. Pine values are immutable, pushing and popping these literals
has no side effect, and the two paths arrive at instruction 200 with the same
remaining evaluation-stack shape.

### Runtime savings

For this exact diamond:

- a matching input avoids `Push_Literal (True)` and
  `Jump_If_Equal_Const`: two executed instructions;
- a nonmatching input avoids `Push_Literal (False)`, `Jump_Const`, and
  `Jump_If_Equal_Const`: three executed instructions;
- four static instructions disappear;
- one unconditional transfer and one conditional test disappear;
- no encoded Boolean needs to enter and leave the evaluation stack.

The static switch table remains unchanged. Branch forwarding alone does not
reduce its 53 comparisons in the current VM implementation.

## Alternative sequential IR representations

### 1. Direct switch to the consumer's destinations

```text
source
skip
Int_Mul_Const (4)
Switch_Jump_If_Slice_Skip_Var_Equal_Const
  matching cases -> trueTarget
  miss           -> falseTarget by fallthrough
```

This is the preferred representation for the existing instruction set.

Advantages:

- removes all redundant Boolean construction and the second test;
- adds no instruction kind or VM dispatch path;
- preserves the switch predicate's current behavior exactly, including
  malformed integer encodings and slice-boundary behavior;
- lets all matching literals share one destination, as they already do.

Disadvantages:

- the false destination must be laid out as fallthrough, or an unconditional
  jump remains necessary;
- the compiler needs symbolic destinations or careful offset recomputation;
- the 53-case predicate is still evaluated as before.

### 2. A fused membership predicate that returns a Boolean

```text
Slice_Skip_Var_In_Const_Set (53 literals)
Jump_If_Equal_Const (True, trueTarget)
```

This removes the Boolean diamond but still materializes and retests a Boolean.
It can be useful if the result has non-control-flow consumers, but it is
inferior to a direct switch when the only consumer is a branch.

Advantages:

- reusable where the membership result is stored, returned, or combined;
- can encapsulate a faster set representation.

Disadvantages:

- retains two instructions and stack traffic;
- introduces a new instruction unless expressed as an existing builtin;
- needs exact definitions for malformed skip counts and out-of-range slices.

### 3. A two-destination conditional membership terminator

Conceptually:

```text
Branch_If_Slice_Skip_Var_In_Const_Set
  matches -> trueTarget
  miss    -> falseTarget
```

This is a clean graph-level representation, but the linear IR does not need a
new instruction for it. `Switch_Jump_If_Slice_Skip_Var_Equal_Const` plus
fallthrough already represents the same operation. A two-destination form is
most useful before final block layout, where neither edge should be privileged
as fallthrough.

### 4. Range or character-class branching

The literal set in this example is not arbitrary:

```text
'A' <= c <= 'Z' || c == '_' || 'a' <= c <= 'z'
```

A range-oriented form could be:

```text
Branch_If_UTF32_At_Skip_In_Ranges
  ranges: ['A'..'Z'], ['_'..'_'], ['a'..'z']
```

Advantages:

- reduces 53 equality checks to a few range checks;
- greatly shrinks static literal metadata;
- directly models common parser character classes.

Disadvantages:

- is more specialized than branch forwarding;
- must preserve UTF-32 width, blob/list behavior, skip clamping, invalid
  integer behavior, and short-input behavior;
- recognizing dense ranges from arbitrary Pine values is more involved;
- a character-specific instruction may not generalize to list slices or
  multi-element literals.

A more general alternative is an optimized constant-set switch. For
single-element, equal-width blob literals, the compiler or VM can build a
lookup table, bitmap, interval set, or hash set. This retains the existing IR
kind while changing its compiled metadata or execution strategy.

### 5. Fuse the index scaling with the switch

`Int_Mul_Const (4)` converts a character index to a UTF-32 byte offset before
the slice switch. A further fused form could accept the unscaled index and a
scale:

```text
Switch_Jump_If_Slice_Skip_Var_Mul_Const_Equal_Const (scale: 4)
```

This saves one instruction and one encoded-integer round trip. It is
orthogonal to eliminating the Boolean diamond. As with the range form, it must
preserve the behavior of `Int_Mul_Const` for invalid integer encodings and
overflow/conversion boundaries rather than assuming all inputs are valid.

## Where to implement the optimization

### Pine expression reduction

At the expression level the broad identity is:

```text
Conditional(
    Conditional(innerCondition, innerFalse, innerTrue),
    outerFalse,
    outerTrue)
```

If the inner branches are known Booleans, the outer branches can be selected
directly from `innerCondition`. More generally, an equality chain returning
`True` for every case and `False` by default can be composed with the outer
conditional.

This layer has some benefits:

- optimization happens before instruction selection;
- subsequent inlining and reduction can simplify the selected branches;
- the sequential compiler receives a simpler expression;
- the rule can benefit non-sequential evaluators of reduced expressions.

However, it is not the best general solution:

- Pine expressions describe values as trees, not a graph of blocks and edges;
- distributing an outer conditional through an inner conditional can
  duplicate large branch expressions;
- repeated distribution can cause exponential growth;
- preserving sharing requires an additional representation or environment
  binding;
- compiler-generated diamonds and similar patterns from other producers are
  invisible to the reducer;
- a narrowly shaped reducer rule will miss semantically equivalent forms
  involving `equal`, `negate`, or nonliteral Boolean producers.

A safe reducer addition would therefore be narrow: fold only when the inner
conditional's leaves are independently known `True`/`False`, and only when the
rewrite does not duplicate nontrivial outer branches. It would complement,
not replace, a control-flow optimization.

### Directly in `PineIRCompiler`

`CompileConditional` currently first recognizes a direct equality chain with
`TryCompileSwitchEqual`. Otherwise, `CompileConditionalWithoutFusion`
compiles the condition as a value and then emits `Jump_If_Equal_Const`.
`TryCompileSwitchEqual` independently emits its own branches and values. When
an equality-chain conditional is itself the condition of another conditional,
these two compilation decisions compose into the observed diamond.

The compiler could introduce a branch-oriented condition API:

```text
CompileCondition(condition, trueDestination, falseDestination)
```

Instead of asking every condition to leave a Boolean on the stack, it would:

- peel `equal` and `negate` while swapping or selecting destinations;
- compile equality chains as switches directly to the destinations;
- recursively compile Boolean-returning conditionals in branch context;
- fall back to value compilation plus `Jump_If_Equal_Const` when necessary.

Advantages:

- prevents the redundant sequence from being emitted;
- exposes source-expression structure and existing switch recognition;
- can preserve tree-level sharing by targeting common branch blocks.

Disadvantages:

- the current compiler constructs flat instruction lists and numeric relative
  offsets during recursive compilation;
- direct destination threading increases already delicate offset arithmetic;
- the same optimization must be reimplemented for every Boolean producer;
- it cannot clean up diamonds introduced after instruction selection.

This approach becomes attractive if the compiler first emits symbolic block
identifiers and defers layout. Without that change, a targeted special case in
`CompileConditional` is feasible but less maintainable.

### Peephole optimization on linear instructions

A local pass could search for:

```text
switch -> push False/jump, push True -> branch on True
```

and rewrite jump offsets.

Advantages:

- small initial implementation;
- directly addresses already emitted code.

Disadvantages:

- textual adjacency is not enough: blocks can have additional predecessors;
- deleting instructions requires updating every ordinary and switch offset;
- stack equivalence and absence of other uses must be proven;
- harmless changes in block layout can hide the pattern;
- extensions tend to recreate a control-flow graph and data-flow analysis
  poorly.

A peephole pass is reasonable only if it first computes leaders,
predecessors, stack effects, and symbolic targets. At that point it is
effectively a CFG pass.

### Optimization on `PineControlFlowGraph`

The repository already converts the final instruction list through
`PineControlFlowGraph.FromInstructions` and
`LowerToStackInstructions`. The graph has explicit `Switch`,
`ConditionalJump`, and `Jump` terminators, virtual values, block parameters,
and symbolic block IDs. Today this stage validates stack flow and reassigns
physical offsets but does not perform this optimization.

This is the most natural general-purpose layer. A pass can:

1. find a conditional terminator consuming a block parameter or virtual value;
2. inspect every incoming edge and resolve the value supplied for that
   parameter;
3. when an incoming value is a known Boolean, redirect that predecessor edge
   to the corresponding conditional successor;
4. preserve any remaining incoming edges that cannot be resolved;
5. remove unreachable literal and forwarding blocks;
6. merge trivial blocks and choose a profitable fallthrough layout;
7. lower once, recomputing all relative offsets from symbolic block targets.

For the exact graph, the match edge is redirected to the true successor and
the miss edge to the false successor. The literal blocks, merge parameter, and
conditional block become unreachable.

Advantages:

- reasons about predecessors and uses rather than adjacency;
- avoids manual relative-offset repair;
- applies to switches, ordinary branches, and compiler-generated control flow;
- scales to jump threading, unreachable-block removal, and block merging;
- matches common compiler architecture.

Disadvantages:

- the graph API needs mutation/rebuilding utilities and predecessor/use maps;
- virtual-value provenance must be retained or reconstructed precisely;
- block ordering and fallthrough constraints need a layout step;
- care is needed around `Eval` continuations, tail invokes, and stack values
  carried on edges.

### A richer intermediate representation

The current graph is already close to a stack-to-SSA bridge, but
`PineControlFlowOperation` still embeds physical `StackInstruction` objects.
A richer mid-level IR could represent operations such as slice membership,
integer scaling, constants, and branches independently of final stack
instructions.

Such an IR would make standard analyses straightforward:

- constant propagation and sparse conditional constant propagation;
- def-use chains and dead-code elimination;
- branch folding and jump threading;
- common-subexpression elimination;
- range analysis;
- instruction selection and fusion after optimization;
- block layout based on profile weights.

It also has the largest implementation cost. Pine semantics for malformed
values need to remain explicit in operation definitions, and lowering must
reconstruct the exact evaluation-stack protocol. Introducing this IR is
justified if many optimizations are blocked by the current tree-to-linear
compiler, not solely for this diamond.

## Common compiler techniques and how they apply

### Constant folding

Constant folding removes a branch only when its condition is globally
constant. The membership result here varies with input, so ordinary constant
folding cannot remove the switch or final branch.

### Sparse conditional constant propagation

SCCP tracks constants on executable CFG edges. The merged Boolean is not one
global constant: it is `True` on one predecessor and `False` on another.
Basic SCCP therefore keeps the `phi` and branch. SCCP combined with
predecessor-sensitive simplification can expose this case.

### Jump threading

Jump threading uses facts known on a predecessor edge to bypass a later
branch. Here each predecessor knows the exact Boolean supplied to instruction
200. Threading the match predecessor to 225 and the miss predecessor to 201 is
the canonical optimization.

### Branch folding through `phi`

In SSA terminology, branching on `phi(True, False)` can move the branch choice
onto the incoming edges. This describes the transformation most precisely.
When a predecessor has multiple outgoing edges, redirecting individual switch
edges is sufficient; critical-edge splitting may be needed for more complex
cases.

### CFG simplification

After threading, unreachable-block elimination removes the two literal blocks
and the conditional block. Block merging removes trivial forwarding blocks,
and branch inversion or block reordering chooses one successor as fallthrough.
These cleanup steps are important because threading alone can leave redundant
jumps.

### If-conversion

If-conversion does the opposite: it replaces branches with selected values.
The current IR has effectively materialized a selected Boolean and then
branched again. Since the eventual use is control flow, converting back to
direct branches is preferable.

### Superinstructions and instruction combining

Fusing the multiply, slice, membership test, and branch is instruction
combining. It reduces dispatch and intermediate stack traffic but is more
specialized than CFG simplification. It should follow semantic branch
forwarding so instruction selection sees the simplest graph.

## Recommended implementation sequence

### Step 1: Add CFG branch forwarding

Add an optimization phase between `FromInstructions` and
`LowerToStackInstructions`. Start with the exact, provably safe form:

- a conditional compares its sole consumed value with `True` or `False`;
- all relevant incoming edges provide literal Pine Booleans;
- the literal-producing operations have no other uses or side effects;
- redirecting the edges preserves all other stack arguments;
- remove unreachable blocks and merge trivial fallthroughs.

Test both orientations:

- `True` selects the jump target;
- `False` selects the jump target;
- match and miss paths;
- shared and distinct switch case destinations;
- additional predecessors that prevent all or part of the rewrite;
- unrelated values carried on the evaluation stack;
- backward edges and loop accounting;
- malformed values in the slice-switch inputs.

The test should assert semantic equivalence and the absence of the
`Push_Literal`/`Jump_Const`/`Jump_If_Equal_Const` diamond.

### Step 2: Make condition compilation branch-oriented

Once symbolic block construction is convenient, let `CompileConditional`
request true and false destinations directly. This prevents common diamonds
and reduces work left for cleanup. Keep the CFG pass because it catches
patterns from inlining, compiler-generated guards, and future instruction
selection.

### Step 3: Optimize switch membership independently

Measure how many literal comparisons are executed by
`Switch_Jump_If_Slice_Skip_Var_Equal_Const`. The current VM loops over every
entry until a match. Consider:

1. cached hash/set lookup for arbitrary equal-width literals;
2. a bitmap for one-byte or one-code-unit domains;
3. interval compression for dense ordered literals;
4. a specialized UTF-32 character-class branch;
5. fusing the multiply-by-four offset only if profiling shows dispatch or
   integer encoding remains material.

Keep branch forwarding and membership acceleration as separate changes so
their performance and semantic effects can be tested independently.

## Comparison

| Approach | Coverage | Code-growth risk | Offset risk | Runtime result | Recommended role |
| --- | --- | ---: | ---: | --- | --- |
| Narrow Pine reduction | Source shapes only | Medium | None | Can remove whole diamond | Supplemental |
| Compiler special case | Recognized Boolean conditions | Low | High in current flat compiler | Prevents diamond | Short-term alternative |
| Linear peephole | Exact emitted layouts | Low | High | Removes diamond | Avoid beyond prototype |
| CFG jump threading | All equivalent control flow | Low | Low after symbolic lowering | Removes diamond generally | Preferred first implementation |
| Rich mid-level/SSA IR | Broad optimization platform | Controlled by passes | Low | Enables many optimizations | Strategic investment |
| New fused predicate instruction | Predicate users | None | Low | Reduces dispatch/stack traffic | Only when Boolean value is needed |
| Range/set switch optimization | Large literal switches | None | Low | Reduces predicate cost | High-value independent follow-up |

## Conclusion

The redundant work is not inherent in Pine semantics or in the sequential IR
instruction set. It results from compiling a Boolean-producing conditional in
value context and then consuming that value in branch context. The direct
sequential IR representation is already available: point the membership
switch's matching edges at the outer true branch and let its miss edge reach
the outer false branch.

Implementing this as CFG jump threading gives the best balance of generality,
safety, and maintainability. A branch-oriented compiler API can prevent the
same pattern earlier, while a Pine reducer rule is useful only for carefully
bounded expression shapes. Finally, the 53-entry linear membership test is a
separate and likely larger runtime opportunity; range or set-based dispatch
should be evaluated after the redundant Boolean control flow is removed.
