# Pine Language

The Pine language is a side‑effect‑free compilation target designed to unlock the next level of automation in software development.

The design of Pine enables runtimes to automatically distribute work across local threads, heterogeneous hardware, and multiple machines, without requiring developers to code for concurrency, memory management, or network boundaries.

## Value Model

The Pine value model consists of two kinds of values:

```Elm
type Value
    = BlobValue Bytes
    | ListValue (List Value)
```

## Expression Model

The Pine expression model consists of seven kinds of expressions:

```Elm
type Expression

    = LitralExpression Value

    | ListExpression (List Expression)

    | EvalExpression
        -- Encoded expression
        Expression
        -- Environment
        Expression

    | BuiltinExpression
        -- Built-in function name
        BuiltinFunctionName
        -- Input value
        Expression

    | ConditionalExpression
        -- Condition
        Expression
        -- False Branch
        Expression
        -- True Branch
        Expression

    | EnvironmentExpression

    | LabelExpression Value Expression
```

### Eval Expression

An `Eval` expression parses an expression from a given value and then evaluates that expression for a given environment value. In that new evaluation context, the `Environment` expression variant evaluates to that given environment value.

If the value given for the encoded expression is not a valid encoding of an expression, the program crashes.

#### Expression Encoding

Every expression is encoded as a `ListValue`. The first item is a tag that
identifies the expression variant, and the remaining items contain its
arguments.

In the definitions below:

- `[ a, b, ... ]` denotes a Pine `ListValue` containing the listed Pine
  values in order.
- `String(s)` denotes a Pine `BlobValue` containing the Unicode scalar values
  of `s`, with each scalar encoded as four bytes in big-endian order.
- `Encode(e)` denotes the recursive encoding of expression `e`.

The encoding of each expression variant is:

| Expression | Encoded Pine value |
| --- | --- |
| `LitralExpression value` | `[ String("Litral"), value ]` |
| `ListExpression [ item₀, ..., itemₙ ]` | `[ String("List"), Encode(item₀), ..., Encode(itemₙ) ]` |
| `BuiltinExpression function input` | `[ String("Builtin"), String(function), Encode(input) ]` |
| `ConditionalExpression condition falseBranch trueBranch` | `[ String("Conditional"), Encode(condition), Encode(falseBranch), Encode(trueBranch) ]` |
| `EnvironmentExpression` | `[ String("Environment") ]` |
| `EvalExpression encoded environment` | `[ String("Eval"), Encode(encoded), Encode(environment) ]` |
| `LabelExpression label expression` | `[ String("Label"), label, Encode(expression) ]` |

`value` and `label` are arbitrary Pine values and are embedded directly,
without expression encoding. A list expression can contain any number of
items; therefore, the empty list expression is encoded as
`[ String("List") ]`.

An encoded expression is valid exactly when:

1. It is a nonempty `ListValue`.
2. Its first item is the string encoding of one of the tags in the table.
3. It has exactly the shape and number of items specified for that tag. The
   `List` tag is the only variable-length form.
4. Every item shown as `Encode(...)` is itself a valid encoded expression.
5. For `Builtin`, the function item is a valid encoded string.

The order of items is significant. In particular, `Conditional` stores the
false branch before the true branch, and `Eval` stores the encoded-expression
operand before the environment operand.

#### Deferring Branch Decoding with Eval

Decoding an expression is strict. A decoder validates the complete recursive
encoding before evaluating the decoded expression. Consequently, an invalid
encoding in either branch of an encoded `Conditional` makes the whole encoded
expression invalid, even if evaluation would not select that branch.

A frontend language can nevertheless avoid decoding the contents of branches
that are not selected. It can embed the encoding of each branch as literal data
and use `Eval` to decode and evaluate the selected branch:

```Elm
defer : Expression -> Expression
defer expression =
    EvalExpression
        (LitralExpression (Encode(expression)))
        EnvironmentExpression

compiledConditional : Expression
compiledConditional =
    ConditionalExpression
        compiledCondition
        (defer compiledFalseBranch)
        (defer compiledTrueBranch)
```

The outer expression has a valid encoding. Each `Encode(expression)` appears as
the value of a `LitralExpression`, so it is embedded as data and is not part of
the recursive encoding of the outer expression. Because a `Conditional`
evaluates only its selected branch, only the selected branch needs to be
decoded. `EnvironmentExpression` passes the current environment to that branch.

A lazy crash follows as a consequence of this pattern. A frontend can use a
value that is not a valid expression encoding instead of `Encode(expression)`:

```Elm
crashExpression : Expression
crashExpression =
    EvalExpression
        (LitralExpression (BlobValue []))
        EnvironmentExpression
```

Every expression encoding is a `ListValue`, so `BlobValue []` is invalid. The
containing expression remains valid because the blob is literal data. The
program crashes only if evaluation reaches `crashExpression` and `Eval` tries
to decode the blob.

An implementation that eagerly decodes expressions and reuses their parsed
representations can avoid the runtime cost of the `Eval` wrapper. While parsing,
it may recognize an expression of this form:

```Elm
EvalExpression
    (LitralExpression encodedExpression)
    EnvironmentExpression
```

If `encodedExpression` is a valid expression encoding, the implementation may
replace the `Eval` internally with the decoded expression. This inlining
preserves behavior because the decoded expression receives the same environment
as the `Eval`. If `encodedExpression` is invalid, parsing the containing
expression must still succeed; the implementation must retain the `Eval` or an
equivalent internal `Crash` representation so that the program crashes only if
evaluation reaches it. An internal `Crash` representation is an implementation
detail, not an additional Pine expression variant, and has no expression
encoding.


### Builtin Expression

A `Builtin` expression maps the given input value with one of the 17 built-in functions of the Pine language:

+ `equal`
+ `negate`
+ `length`
+ `head`
+ `skip`
+ `take`
+ `reverse`
+ `concat`
+ `int_add`
+ `int_mul`
+ `int_is_sorted_asc`
+ `bit_and`
+ `bit_or`
+ `bit_xor`
+ `bit_not`
+ `bit_shift_left`
+ `bit_shift_right`
