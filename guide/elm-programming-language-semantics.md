# Elm Programming Language Semantics

This document explains semantics of the Elm programming language. The specifications are derived from the implementation of the original compiler.

Discussion of the language semantics can also be found on the following sites:

+ <https://github.com/elm/compiler/blob/cce7a8bbd8fe690fc83fa795f8d7e02505d1f25f/hints/imports.md>
+ <https://github.com/elm/compiler/blob/cce7a8bbd8fe690fc83fa795f8d7e02505d1f25f/hints/shadowing.md>
+ <https://github.com/jfmengels/blog/blob/e9c701ebffc6dfdec5c9c1038bfcca2e3f0bdd07/content/blog/safe-unsafe-operations-in-elm.md> (<https://jfmengels.net/safe-unsafe-operations-in-elm/>)

## Imports and References

Elm's module system provides fine-grained control over which names are brought into scope when importing modules. This section explains how Elm handles imports, name resolution, and what happens when names from different modules overlap.

### Basic Import Syntax

Elm modules can be imported in several ways:

```elm
-- Import a module, making its contents available via qualified access
import List

-- Import a module with an alias
import Bytes.Decode as Decode

-- Import a module and expose specific names for unqualified use
import Html exposing (div, text)

-- Import a module and expose all its public declarations
import Html.Attributes exposing (..)
```

### Name Resolution

When you reference a name in your code, Elm resolves it in the following order:

1. **Local Variables**: Names bound by function parameters, let expressions, or pattern matches take highest priority. These are not qualified with a module name.

2. **Module-Level Declarations**: Names declared in the current module (functions, types, type aliases).

3. **Exposed Imports**: Names explicitly exposed from imported modules using `exposing (name)` or `exposing (..)`.

4. **Qualified Access**: You can always access any public declaration from an imported module using its full or aliased name (e.g., `List.map` or `Decode.int`).

### Overlapping Names in Imports

When two imported modules both expose a name that you bring into scope with `exposing (..)`, Elm handles this situation carefully:

#### No Error When Overlapping Names Are Not Referenced

If you import two modules that both export a name with the same identifier, but you never actually use that name unqualified in your code, **no error is produced**. For example:

```elm
module Test exposing (..)

import Alfa exposing (..)   -- Alfa exports: uniqueAlfa, shared, anotherShared
import Beta exposing (..)   -- Beta exports: uniqueBeta, shared, anotherShared

-- This is fine - we only use non-overlapping names
result =
    uniqueAlfa 5 + uniqueBeta 3
```

Both `Alfa` and `Beta` export `shared` and `anotherShared`, but since `Test` only references `uniqueAlfa` and `uniqueBeta`, there is no ambiguity and the code compiles successfully.

#### Error When Overlapping Names Are Referenced

However, if you try to use a name that is exposed by multiple imports without qualification, Elm produces an error:

```elm
module Test exposing (..)

import Alfa exposing (..)   -- Alfa exports: shared
import Beta exposing (..)   -- Beta exports: shared

-- ERROR: 'shared' is exposed by multiple imports: Alfa, Beta
result =
    shared 5
```

The error message identifies:
- The name that caused the conflict (`shared`)
- All modules that expose this name (`Alfa`, `Beta`)

#### Resolving Overlapping Names

To resolve this ambiguity, you can:

1. **Use qualified access**: Reference the name with its module prefix
   ```elm
   result =
       Alfa.shared 5
   ```

2. **Expose only specific names**: Instead of `exposing (..)`, list the specific names you need
   ```elm
   import Alfa exposing (uniqueAlfa)
   import Beta exposing (uniqueBeta)
   ```

3. **Define a local alias**: Create a local binding that explicitly chooses one version
   ```elm
   myShared =
       Alfa.shared
   ```

### Type and Value Namespaces

Elm maintains separate namespaces for types and values. This means:

- A type name and a value name can share the same identifier without conflict
- Import clashes are checked independently in each namespace
- Type constructors (like `Just`, `Nothing`) exist in the value namespace

```elm
-- This is valid Elm - 'Node' is both a type alias and a constructor
type alias Range = {}

type Node a
    = Node Range a  -- Constructor 'Node' takes Range and 'a'

-- Usage:
myNode : Node Int
myNode =
    Node {} 5
```

### Implicit Imports

Elm automatically imports certain core modules with specific names exposed. The `Basics` module is implicitly imported, exposing all its contents, including common types, functions, and infix operators such as `Int`, `Float`, `Bool`, `True`, `False`, `+`, `-`, etc.

This is why you can write:

```elm
myValue : Int
myValue =
    if True then 42 else 0
```

Without explicitly importing `Basics` or qualifying these names.

The following other modules in the core library are also implicitly imported:

+ `List`, `Char`, `String`, `Maybe`, and `Result` each with their canonical name and specific declarations exposed (listed below).
+ `Debug` and `Tuple` each with their canonical name but with no declarations exposed. This means they only support qualified access (e.g., `Debug.log`, `Tuple.first`) without an explicit import statement.
+ `Platform` with its canonical name, exposing the `Program` type.
+ `Platform.Cmd` with an module name alias `Cmd`, exposing the `Cmd` type.
+ `Platform.Sub` with an module name alias `Sub`, exposing the `Sub` type.

The following declarations from the core library are also implicitly exposed:

+ The type `List` and the infix operator `::` from `List`
+ The type `Char` from `Char`
+ The type `String` from `String`
+ The type `Maybe` and its choices `Nothing` and `Just` from `Maybe`
+ The type `Result` and its choices `Err` and `Ok` from `Result`
+ The type `Program` from `Platform`
+ The type `Cmd` from `Platform.Cmd`
+ The type `Sub` from `Platform.Sub`

### Shadowing Imported Names

Elm allows local declarations to shadow names that were brought into scope via imports. This follows a natural scoping hierarchy where more local bindings take precedence over less local ones.

#### Module-Level Declarations Shadow Imports

A module-level declaration (function, type, or type alias) shadows any imported name with the same identifier. The local declaration takes precedence:

```elm
module Main exposing (..)

import Helper exposing (..)  -- Helper exports: compute


-- This local declaration shadows the imported 'compute' from Helper
compute x =
    x + 1


result =
    compute 5  -- Uses Main.compute, not Helper.compute
```

After canonicalization, `compute 5` resolves to `Main.compute 5`, not `Helper.compute 5`.

#### Parameters and Let Bindings Shadow Imports

Function parameters and let bindings can also shadow imported names. The shadowing is scoped to where the binding is visible:

```elm
module Main exposing (..)

import Helper exposing (..)  -- Helper exports: value = 100


-- Here 'value' refers to Helper.value (appears as Helper.value after canonicalization)
usesImportedValue =
    value + 1


-- Here parameter 'value' shadows the import within this function body
usesParameterValue value =
    value + 2  -- 'value' remains unqualified (refers to the parameter)


-- Here the let binding 'value' shadows the import within the let expression
usesLetValue =
    let
        value =
            50
    in
    value + 3  -- 'value' remains unqualified (refers to the let binding)
```

After canonicalization:
- In `usesImportedValue`, the reference to `value` becomes `Helper.value`
- In `usesParameterValue`, the parameter `value` remains as `value` (unqualified local variable)
- In `usesLetValue`, the let-bound `value` remains as `value` (unqualified local variable)

This demonstrates the scoping rules:
1. Local variables (parameters, let bindings, pattern matches) are never qualified
2. References to local variables do not resolve to imports with the same name
3. Outside of shadowing scopes, imported names resolve normally

### Disallowed Shadowing

Elm **disallows local variable shadowing** to prevent bugs where a programmer accidentally refers to the wrong variable. This rule applies broadly:

- Function parameters **cannot** shadow module-level declarations
- Let bindings **cannot** shadow module-level declarations or outer local variables
- Case pattern bindings **cannot** shadow any existing variable in scope

For detailed rationale, see the official Elm compiler documentation: [Variable Shadowing](https://github.com/elm/compiler/blob/cce7a8bbd8fe690fc83fa795f8d7e02505d1f25f/hints/shadowing.md)

**Note**: While module-level declarations *can* shadow imported names (as described in the previous section), local declarations within a function cannot shadow anything.

#### Example: Case Pattern Shadowing Function Parameter

The following code is **invalid** in Elm because `name` in the `Just name` pattern shadows the function parameter `name`:

```elm
module Test exposing (..)


viewName name =
    case name of
        Nothing ->
            "anonymous"

        Just name ->  -- ERROR: Shadows the 'name' parameter
            name
```

This produces an error like:
> This `name` pattern is shadowing an existing `name` variable. Rename one of them to avoid the ambiguity.

#### Example: Local Declarations Shadowing Module-Level Declarations

Local declarations (parameters, let bindings, pattern bindings) also cannot shadow module-level declarations:

```elm
module Test exposing (..)


-- A module-level declaration
helper =
    42


-- ERROR: Parameter 'helper' shadows the module-level 'helper'
usesParameterShadow helper =
    helper + 1


-- ERROR: Let binding 'helper' shadows the module-level 'helper'
usesLetShadow =
    let
        helper = 100
    in
    helper + 2


-- ERROR: Pattern binding 'helper' shadows the module-level 'helper'
usesPatternShadow x =
    case x of
        Just helper ->
            helper

        Nothing ->
            0
```

Each of these produces a shadowing error.

