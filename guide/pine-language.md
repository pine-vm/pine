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
        String
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
| `ConditionalExpression condition falseBranch trueBranch` | `[ String("Condition"), Encode(condition), Encode(falseBranch), Encode(trueBranch) ]` |
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

The order of items is significant. In particular, `Condition` stores the
false branch before the true branch, and `Eval` stores the encoded-expression
operand before the environment operand.

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
