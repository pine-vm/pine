# Elm Compiler Implementation Guide

## Emitting Function Applications

To compile a function application from Elm syntax, we use the 'ParseAndEval' expression variant of the Pine language. This expression creates a new environment for evaluating an expression, similar to the 'eval' functions found in other languages, such as Python. Since there are no symbolic references in Pine, we transport any other functions that the called function depends on into this new environment.
For example, a recursive function, when calling itself via 'ParseAndEval', composes the new environment so that it also contains a representation of the function itself in encoded form, to enable continuing recursion in the non-terminating branch.

### Full Function Applications

For function applications where the number of arguments equals the number of parameters of the function (non-partial application), we use the following pattern to compile Elm function applications, as a convention:
The environment is a list with two items. The first item of this list contains a list of all the encoded functions needed for further function applications. The second item in the list contains the arguments from the source Elm code.

The following example illustrates the pattern using a concrete recursive function:

```Elm
factorial : Int -> Int
factorial n =
    if Pine_kernel.int_is_sorted_asc [ n, 1 ] then
        1

    else
        Pine_kernel.int_mul
            [ factorial (Pine_kernel.int_add [ n, -1 ])
            , n
            ]
```

Here, we have one entry for the encoded function and one entry for the parameter defined in the Elm source code.
Following the pattern established earlier, our expression to create the new environment value looks as follows:

```txt
[ [ value_encoding_factorial ]
, [ Pine_kernel.int_add [ n, -1 ] ]
]
```

Because of the direct recursion, the calling function happens to be the called function, so placement of the components in the environment is symmetrical. The parameter named "n" is the first in the parameter list, and therefore is placed at index 0:

```txt
[ [ current_env[0][0] ]
, [ Pine_kernel.int_add [ current_env[1][0], -1 ] ]
]
```

Following this pattern ensures that common inspection and profiling tooling can parse and analyze the invocations. We also use these tools to derive symbols for pseudo-functions rendered as part of snapshot tests.

### Function Values And Partial Application

The Elm programming language supports closures and partial application. When a function (with remaining parameters) escapes a scope as a value, the Elm compiler emits a representation of this partially applied function that allows adding more arguments sometime later.

For function applications where the function is a value of unknown origin, the Elm compiler emits an expression that adds the given arguments using a form that allows for generic partial application. It emits this partial application as `ParseAndEvalExpression`, where the `Environment` contains the argument value. (If the Elm application expression contains multiple arguments, the compiler nests this pattern recursively)

When emitting a function value, the compiler creates a corresponding wrapper matching the number of parameters. On application of the last argument, the wrapper uses an environment structure as described in the 'Full Function Applications' section.
