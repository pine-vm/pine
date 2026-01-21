# Elm Compiler Implementation Guide

## Encoding of Elm Values as Pine Values

For the encoding of Elm values as Pine values, the compiler follows the model established in the `ElmValueEncoding.cs` file. The definitions in `ElmValueEncoding.cs` cover the encoding of all Elm values except functions.

## Function Applications

To compile a function application from Elm syntax, we use the 'ParseAndEval' expression variant of the Pine language. This expression creates a new environment for evaluating an expression, similar to the 'eval' functions found in other languages, such as Python. Since there are no symbolic references in Pine, we transport any other functions that the called function depends on into this new environment.

Function applications are grouped into full applications and partial applications. Partial applications are all usages of functions in which fewer arguments are supplied than the number of parameters. These partial applications produce function values.

> Note: The approach using function values and a generic application works in any case. The reason we use a different implementation for the full application case is to emit more efficient code.

### Full Function Applications

For function applications where the number of arguments equals the number of parameters of the function (non-partial application), we use the following pattern to compile Elm function applications, as a convention:
The environment is a list with two items. The first item in this list contains all the encoded function bodies needed for further applications. This set contains all the transitively referenced functions that have not been inlined. The second item in the list contains the arguments from the source Elm code.

For example, a recursive function, when calling itself via 'ParseAndEval', composes the new environment so that it also contains a representation of the function itself in encoded form, to enable continuing recursion in the non-terminating branch.

The following example illustrates the pattern using a concrete recursive function:

```Elm
factorial : Int -> Int
factorial n =
    if Pine_builtin.int_is_sorted_asc [ n, 1 ] then
        1

    else
        Pine_builtin.int_mul
            [ factorial (Pine_builtin.int_add [ n, -1 ])
            , n
            ]
```

Here, we have one entry for the encoded function and one entry for the parameter defined in the Elm source code.
Following the pattern established earlier, our expression to create the new environment value looks as follows:

```txt
[ [ value_encoding_factorial ]
, [ Pine_builtin.int_add [ n, -1 ] ]
]
```

Because of the direct recursion, the calling function happens to be the called function, so placement of the components in the environment is symmetrical. The parameter named "n" is the first in the parameter list, and therefore is placed at index 0:

```txt
[ [ current_env[0][0] ]
, [ Pine_builtin.int_add [ current_env[1][0], -1 ] ]
]
```

Following this pattern ensures that common inspection and profiling tooling can parse and analyze the invocations. We also use these tools to derive symbols for pseudo-functions rendered as part of snapshot tests.

### Composition of the Environment Functions List

The following are general rules we use to compose the list of functions in the eval environment introduced above.

#### What to Include in the Environment Functions List

+ All functions in a group of mutually recursive functions must be included.
+ Other functions might be included as well. (The alternative to retrieving it from the environment is to contain the function directly in the expression representing the function application, as a literal)

#### How to Order Entries in the Environment Functions List

+ All functions in a group of mutually recursive functions use the same order. This makes it easier to model invocations in the recursive set, since we can forward the entire list rather than individual items.
+ Recursive functions are placed at the beginning of the list.

### Function Values And Generic Function Application

The Elm programming language supports first-class functions and partial application. When a function whose declaration we know (it is not a parameter in the current context) escapes the current context, the Elm compiler emits a representation of this function as a value that can then be freely passed around to other parts of the program and applied sometime later.

A function value encodes a function in a way that enables the incremental addition of further arguments. For each argument, the applying side uses a `ParseAndEval` expression.

This function value contains not only an encoding of the function body, but also a list of the encoded function bodies the wrapped function depends on, including transitive dependencies.

For function applications where the function is a value of unknown origin, the compiler emits an expression that adds the given arguments using a form that allows for generic partial application. It emits this partial application as `ParseAndEvalExpression`, where the `Environment` contains the argument value. (If the Elm application expression contains multiple arguments, the compiler nests this pattern recursively)

When emitting a function value, the compiler creates a corresponding wrapper matching the number of parameters. On application of the last argument, the wrapper uses an environment structure as described in the 'Full Function Applications' section.

## Closures

The Elm programming language supports closures that capture the values of bindings in scope. Following is an example:

```Elm
map : (a -> b) -> List a -> List b
map f xs =
    foldr (\x acc -> f x :: acc) [] xs
```

The compiler resolves closures via lambda lifting in an early step in the compilation pipeline. The code below shows the result of the lambda lifting stage for the earlier example of a closure:

```Elm
map : (a -> b) -> List a -> List b
map f xs =
    foldr (map__lifted__lambda1 f) [] xs


map__lifted__lambda1 f x acc =
    f x :: acc
```

## Type Inference

This Elm compiler implements only partial type inference. Since it is not sufficient to detect all type mismatches, users also want to use an additional type checker to verify that a given Elm program is valid.

Another implication of this partial type inference is that adding type annotations can yield more efficient code.

The limited type inference here supports:

+ Type of binding from type annotation, if it does not contain any type variable or open record.
  + This also covers infix operators that constrain to a concrete type, like `//` and `/`.
+ Type of function argument from function type.
  + This function type does not have to come from an annotation, but could also have been inferred.
  + Includes function applications using choice type tags (e.g., `Maybe.Just`, `Result.Ok`)
  + Includes applications of functions implied by record type alias declarations.
  + When a variable is used as an argument, its type is inferred from the function's parameter type at that position.
+ Picking up primitive types from int pattern, hex pattern, string pattern, etc.
+ Type of tuple from tuple expression. (item -> tuple)
+ Type of tuple item from tuple expression. (tuple -> item)
+ Type of tuple item from tuple pattern.
+ Type of list item from list pattern.
+ Type of list item from uncons pattern.
+ Type of list from list expression.
+ Type of argument from 'named' pattern (deconstruction of choice type tag).
  + This also works in expression contexts: when a variable is used as an argument to a choice type constructor, its type is inferred from the constructor's argument type.
+ Type of record from record expression.
+ Type of record field from record access.
+ Type of record field from record update.
+ Distinction between `String` and `List a` to emit specialized code where the `++` operator (`appendable` type class) is applied.
+ Propagation via arithmetic operators like `+` `-` (`number` type class) to distinguish between `Int` and `Float` and emit specialized code for `Int` arithmetic. When one operand has a known type, the type propagates to the other operand in the expression.
+ Type inference through let expressions: Types are propagated from parameters through let bindings. When a let binding's expression has an inferred type, that type is associated with the binding name for use in subsequent expressions.
+ Type unification between branches: In if/case expressions, the types of all branches are unified to determine the most specific common type. For example, if one branch returns `Int` and another returns a `number` literal, the result type is `Int`.

## Arithmetic Operations

The Elm core library offers arithmetic operations that are polymorphic and work on the `number` type class. The core library exposes these only as operator symbols.
For example, it [exposes `Basics.add` as an infix operator `+`](https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/Basics.elm#L82), which also implies the availability as a function named `(+)`.

For operations where we prove the type must be `Int`, we emit specialized code accordingly. In cases where the type is `Float` or an unconstrained `number`, we emit a function that works with any `number` values.

Where the type is constrained to `Int`, we map `Basics.add` and `Basics.mul` operations from Elm directly to the `int_add` and `int_mul` Pine builtin functions. For other operations, we attach the corresponding function in Pine code locally, so that code analysis does not need to follow references to the environment when parsing these operations. Tooling for inspection, such as snapshot tests for the compiler, maps these particular Pine expressions to identifiers like `ElmCore.int_div`, `ElmCore.number_mul`, etc. for better readability.

## Records

For Elm code producing record values, we always produce records with fields sorted alphabetically by field name. This makes subsequent operations on these values, like equality checks, simpler.

### Record Access and Record Update

How we handle record access and record updates depends on how much we know about the record type at compile time: when we have inferred a closed set of field names, we use the field-name index to perform an index-based lookup or insertion.

However, due to row polymorphism, that index can vary across call sites if the function we emit has not been narrowed to a closed set of field names. In these cases, we emit a function that recursively scans the record to find the field with the matching name.

### Record Constructors

In Elm, a `type alias` declaration of a record type also implies the creation of a function of the same name. These record constructors can be used like other functions, including partial application.

The order of arguments is the same as the order in which the fields appear in the type alias declaration code. This means that multiple record type alias declarations can produce the same type, but different constructors.

## Composition of Module Values

The compiler interface offers functions that return module values, or a composition comprising multiple named modules, such as `CompileInteractiveEnvironment`

Each module value in turn encodes a list of named values. The declarations included here fall into two categories:

1. Declarations the caller selected as entry points to be compiled.
2. Additional entries to support inspection and debugging use cases.

Tooling for inspection and parsing uses these additional names when transforming from Pine program code into a human-readable representation. Some of these names correspond directly to declarations in the source program code. Other names are derived from source declaration names by adding a suffix to uniquely distinguish specialized variants of functions from the source code. That suffix consists of a plus sign, followed by a hexadecimal representation of a hash code. That hash code is derived from a description of all the specializations applied to the source function to arrive at the named variant.

Examples for specializations that can result in varied forms of a function:

+ Fixing a function parameter to a concrete value.
+ Constraining the type of a parameter from an open record to a closed record.
+ Constraining a parameter from a type class like `number` to a concrete type like `Int`.

## Future Exploration

### Future Exploration - Monomorphizing Extensible Records

The following is an idea to further optimize programs with row polymorphism for throughput:

```
The Elm language supports records that can be extended with additional fields, often called “extensible records” or “row polymorphism”.
Instead of only using fixed shapes like `{ name : String, age : Int }`, Elm also allows types such as `{ r | name : String }`, which means “any record that has at least a name : String field, plus possibly more fields collected in r.” This feature lets a function say, in its type, “I need these fields, but I do not care what else is in the record.”

The compiler monomorphizes functions that accept extensible records. This monomorphization has the following implications:

+ When emitting a record access or record update, the concrete record fields are known from prior type inference, allowing the compiler to use an index to access the field value.
+ The compiler does not support entry points accepting extensible records. If the application author selects such a function as a compilation entry point, the compiler returns an error message.

```
