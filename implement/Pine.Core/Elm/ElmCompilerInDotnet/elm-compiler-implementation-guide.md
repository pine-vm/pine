# Elm Compiler Implementation Guide

## Encoding of Elm Values as Pine Values

For the encoding of Elm values as Pine values, the Elm compiler follows the model established in the `ElmValueEncoding.cs` file. The definitions in `ElmValueEncoding.cs` cover the encoding of all Elm values except functions.

## Emitting Function Applications

To compile a function application from Elm syntax, we use the 'ParseAndEval' expression variant of the Pine language. This expression creates a new environment for evaluating an expression, similar to the 'eval' functions found in other languages, such as Python. Since there are no symbolic references in Pine, we transport any other functions that the called function depends on into this new environment.
For example, a recursive function, when calling itself via 'ParseAndEval', composes the new environment so that it also contains a representation of the function itself in encoded form, to enable continuing recursion in the non-terminating branch.

### Full Function Applications

For function applications where the number of arguments equals the number of parameters of the function (non-partial application), we use the following pattern to compile Elm function applications, as a convention:
The environment is a list with two items. The first item in this list contains all the encoded functions needed for further applications. The second item in the list contains the arguments from the source Elm code.

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

### Function Values And Partial Application

The Elm programming language supports closures and partial application. When a function (with remaining parameters) escapes a scope as a value, the Elm compiler emits a representation of this partially applied function that allows adding more arguments sometime later.

For function applications where the function is a value of unknown origin, the Elm compiler emits an expression that adds the given arguments using a form that allows for generic partial application. It emits this partial application as `ParseAndEvalExpression`, where the `Environment` contains the argument value. (If the Elm application expression contains multiple arguments, the compiler nests this pattern recursively)

When emitting a function value, the compiler creates a corresponding wrapper matching the number of parameters. On application of the last argument, the wrapper uses an environment structure as described in the 'Full Function Applications' section.

### Closures

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

## Extensible Records

The Elm language supports records that can be extended with additional fields, often called “extensible records” or “row polymorphism”.
Instead of only using fixed shapes like `{ name : String, age : Int }`, Elm also allows types such as `{ r | name : String }`, which means “any record that has at least a name : String field, plus possibly more fields collected in r.” This feature lets a function say, in its type, “I need these fields, but I do not care what else is in the record.”

The compiler monomorphizes functions that accept extensible records. This monomorphization has the following implications:

+ When emitting a record access or record update, the concrete record fields are known from prior type inference, allowing the compiler to use an index to access the field value.
+ The compiler does not support entry points accepting extensible records. If the application author selects such a function as a compilation entry point, the compiler returns an error message.

