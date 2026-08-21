# Why the floating-point normalization test displayed equal values but failed

## Symptom

Running `pine elm test` in
`implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax` reported one failure:

```text
✗ normalizes floating-point literals

    [<blob>,[350]]
    ╷
    │ Expect.equal
    ╵
    [<blob>,[350]]
```

The two rendered values appeared identical even though `Expect.equal` returned a
failure.

## The two values were numerically equal but structurally different

The test compares an expected
`ElmSyntax.Abstract.Expression.FloatLiteral 350` with a value produced by
parsing the source literal `3.5e2`.

Pine represents an Elm `Float` as an `Elm_Float` tag containing a rational
numerator and denominator. The expected literal uses the integer representation
`350`. `String.toFloat "3.5e2"` preserves the parsed decimal components and
produces the mathematically equivalent, but non-normalized, rational
`Elm_Float 3500 10`.

`Debug.toString` renders both numeric representations as `350`. It does not
expose the internal numerator and denominator, and it renders the surrounding
custom type using Pine's fallback tagged-value notation. Consequently, both
sides appeared as `[<blob>,[350]]` in the failure report.

## Why the earlier operator-lowering fix was insufficient

Pine's structural equality builtin compares the encoded representation, so it
cannot implement Elm equality for values that contain equivalent numbers with
different encodings. The earlier fix correctly stopped lowering `==` to
structural equality when a value might contain a `Float`.

That left the comparison to `Basics.eq`, which performs recursive Elm-aware
equality. Its float-to-float branch already used cross multiplication, so it
accepted equivalent rational encodings. However, its mixed integer/float branch
only accepted a float whose denominator was exactly `1`. Therefore it accepted
`350 == Elm_Float 350 1` but rejected
`350 == Elm_Float 3500 10`.

The existing regression fixture compared `350` with the literal `350.0`.
Compilation had already reduced that literal to the normalized `350/1` form, so
the fixture did not exercise the failing representation produced by
`String.toFloat`.

## Fix

Mixed integer/float equality now compares the rational values by cross
multiplication:

- `Elm_Float numerator denominator == integer` compares
  `numerator == denominator * integer`.
- `integer == Elm_Float numerator denominator` compares
  `integer * denominator == numerator`.

The same rule is implemented in all equality execution paths:

- the generated Pine expression for `Basics.eq`;
- the .NET precompiled leaf for that expression;
- the direct Elm syntax interpreter builtin;
- both maintained Elm copies of the `Basics` module.

## Regression coverage

The tests now use the non-normalized value corresponding to `3500/10` and check
both operand orders. Coverage includes:

- the generated `Basics.eq` expression with precompiled leaves disabled;
- the precompiled `Basics.eq` leaf;
- the direct Elm syntax interpreter builtin;
- the complete `pine elm test` pipeline using
  `String.toFloat "3.5e2"`.

This reproduces the representation from the original failing parser test rather
than using a float literal that compilation normalizes before comparison.
