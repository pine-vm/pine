# Elm Compilation to CSharp

Pine offers a compilation of Elm programs to C#. This representation makes it easy to embed Elm programs in .NET-based applications and to directly invoke Elm functions from .NET.

While some projects might benefit from tracking C# code generated this way in source control, most of the time it is used only with a C# compiler to produce .NET assemblies or another lower-level representation.

The intermediate representation in C# is used mostly for:

+ Better human-readable representations for inspection, debugging, and profiling.
+ Modelling snapshot tests for the implementation of the compilation from Elm source code.

## Usage and Interoperability

The compiled Elm types offer a public API that supports easy construction, so function invocations from .NET into the Elm side are not restricted to primitive types.

Integrating applications can invoke an Elm function that takes a complex composite Elm type as input, such as an application-specific event type. The same freedom applies to return types, so applications can use a custom `Cmd` model here.

We aim for the standard architecture, so calls in the other direction are not supported.

The compiled code is inherently thread-safe because all types in the interfaces are immutable.

### `equatable` Type Class and Equality Checks

The `equatable` type class is typically not visible in Elm program code. Since it contains every type, and Elm supports equality checks for any type, showing it would be redundant.
Since C# does not offer automatic value equality semantics to this extent, compilation from Elm to C# also implies generating additional C# code that implements equality checks.
+ The compiler expands emitted type declarations to override the `Equals` and `GetHashCode` methods where necessary. For records and tuples, this is not necessary, since C# compilers automatically generate the code in these cases.
+ For collection types like `List`, `Array`, `Dict`, and `Set`, these are wrapped in a corresponding generic C# type, that implements equality and delegates equality checks for items.
+ When emitting types, we override the `==` and `!=` operators in C#: <https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/how-to-define-value-equality-for-a-type>
+ When emitting functions, we replace occurrences of the `==` and `/=` infix operators with the infix operators in C#.

### `comparable` Type Class and Comparison Operations

Where the source code uses operations on the `comparable` type class, such as `<` or `>=`, we constrain the parameters to implement the [`IComparable<T>` interface](https://learn.microsoft.com/en-us/dotnet/api/system.icomparable-1).

Where possible, occurrences of the infix operators `<`, `<=`, `>`, `>=` are mapped to the infix operators in C#.

For C# tuples and generic types, these infix operators are not available, so we replace them with an expression containing an invocation of `IComparable.CompareTo`.

`Basics.compare` invokes the `IComparable.CompareTo` function and maps the integer result to the `Order` enum value.

### `appendable` Type Class

Where the source code uses operations on the `appendable` type class, such as `++`, we emit an invocation of a function that checks the type at runtime to decide between `String` and `List a`.

However, in cases where the involved type is narrowed to `String` or `List a`, we emit a specialized representation to avoid the runtime type-check.

### `number` Type Class

The `number` type class in Elm unifies `Int` and `Float`. It appears in the signatures of arithmetic operators and functions such as `(+)`, `(-)`, `(*)`, `negate`, `abs`, and `clamp`.

Since C# maps `Int` to `System.Numerics.BigInteger` and `Float` to `System.Decimal`, there is no single C# type that covers both. We resolve this with a monomorphizing design, analogous to how we handle open records:

Where a function in the source code uses a `number`-constrained type variable, the compiler enumerates all concrete types (`Int` or `Float`) used at each call site, and emits a specialized variant of that function for each.

+ A function with signature `number -> number -> number` produces up to two specializations: one for `BigInteger` and one for `Decimal`.
+ The specialized variant names use a suffix indicating the concrete type, e.g., `add_Int` and `add_Float`.
+ At each call site, the compiler selects the appropriate specialization based on the inferred concrete type.
+ If the compiler cannot determine whether a `number` is `Int` or `Float` at a given call site, it exits with an error message.

For example, for this Elm function:

```Elm
double : number -> number
double x = x + x
```

The compiler emits two specializations:

```C#
public static BigInteger double_Int(BigInteger x) => x + x;

public static decimal double_Float(decimal x) => x + x;
```

A call site like `double 5` resolves to `double_Int(5)`, and `double 3.0` resolves to `double_Float(3.0m)`.

When a `number`-polymorphic function calls another `number`-polymorphic function, the specialization propagates: each specialization of the outer function calls the matching specialization of the inner function.

The same monomorphizing approach applies to `Basics.negate`, `Basics.abs`, and other `number`-constrained functions from the core libraries. For these, the compiler ships both `Int` and `Float` specializations in the .NET assembly, and call sites reference the appropriate one.

### `List a` Core Type

The wrapper for the `List` type in the Elm core library (which provides equality and comparability) uses [`ImmutableList<T>`](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutablelist-1) for storage.

### `Array a` Core Type

The wrapper for the `Array` type in the Elm core library (which provides equality and comparability) uses [`ImmutableArray<T>`](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutablearray-1) for storage.

### `Dict k v` Core Type

The wrapper for the `Dict k v` type in the Elm core library (which provides equality and comparability) uses [`ImmutableDictionary<TKey,TValue>`](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutabledictionary-2) for storage.

### `Set t` Core Type

The wrapper for the `Set t` type in the Elm core library (which provides equality and comparability) uses [`ImmutableHashSet<T>`](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutablehashset) for storage.

### `Bytes.Bytes` Type

The wrapper for the `Bytes.Bytes` type from the Elm kernel libraries implements equality and contains a `System.ReadOnlyMemory<byte>` for storage.

### Choice Types

For choice types in which each tag has zero arguments, we emit an `enum` in C#.

For example, the following type declaration from Elm:

```Elm
type Color
    = Red
    | Green
    | Blue
```

Is compiled to the following C# code:

```C#
enum Color
{
    Red,
    Green,
    Blue
}
```

For all other choice types, we emit the common form of an abstract record type which contains a subtype for each tag:

For example, the following type declaration from Elm:

```Elm
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
```

Is compiled to the following C# code:

```C#
abstract record Pattern
{
    private Pattern() { }

    public sealed record AllPattern
        : Pattern;

    public sealed record UnitPattern
        : Pattern;

    public sealed record CharPattern(char First)
        : Pattern;

    public sealed record StringPattern(string First)
        : Pattern;
}
```

#### Single-Tag Choice Types

A choice type with a single tag is often used for domain modelling and supporting static verification. For this special case, we emit a record type declaration directly without subtyping.

For example, the following single-tag type in Elm:

```Elm
type Wrapper a
    = Wrapped a
```

Is compiled to the following C# code:

```C#
record Wrapped<T>(T First);
```

#### Resolution of Tag Name Collisions

C# forbids a member (including a nested type) from having the same name as its enclosing type. This means the general choice-type pattern breaks when one of the tags shares its name with the Elm type.

For example, this Elm declaration:

```Elm
type Request
    = Request String
    | Cancel
```

Would naively compile to:

```C#
abstract record Request
{
    private Request() { }

    // C# error CS0542: member names cannot be the same as their enclosing type
    public sealed record Request(string First) : Request;

    public sealed record Cancel : Request;
}
```

The nested `Request` record has the same name as the enclosing `Request` type, which is a C# compile error.

In addition, a tag name could collide with other existing identifiers, such as `ToString`

To resolve this, we add a suffix to the colliding tag name. Since the tag is a nested type inside the abstract record, the suffix only appears in the nested position. The parent type keeps its original name.

Applied to the example:

```C#
abstract record Request
{
    private Request() { }

    public sealed record Request_Tag(string First)
        : Request;

    public sealed record Cancel
        : Request;
}
```

Only the colliding tag is renamed. All other tags in the same type keep their original names. In the example above, `Cancel` is unaffected — only `Request` (the tag) becomes `Request_Tag`.

### Function Values

Function values are mapped to C# as nested instances of [`System.Func<T,TResult>`](https://learn.microsoft.com/dotnet/api/system.func-2) to support incremental partial application.

For example, a function of type `Int -> String -> Int` appears in C# with type `Func<BigInteger, Func<string, BigInteger>>`

Where a function is referenced without applying an argument, escaping as a value, we produce such a wrapper at the site of the reference.

For readability, these conversion sites use a `Curry` helper function that is overloaded for multiple arities.

### Tuples

C# tuples implement the `IComparable<>` interfaces: <https://learn.microsoft.com/en-us/dotnet/api/system.valuetuple-2>

Therefore we represent Elm tuples as standard .NET tuples.

### Records

Records from Elm source code are emitted as C# records.

It would be convenient to have a C# API that supports referencing record types under the same namespace as other declarations from a module. However, C# does not have the structural equality of records defined under different names. To resolve this problem, we emit one canonical C# record per unique record shape and place it in a shared namespace.

+ For each unique record shape, the compiler creates a dedicated file in the namespace `UniqueRecords`
+ The name of the shared C# record declaration starts with `Record_` followed by the identifier of the unique record shape, truncated to 10 characters. The record shape identifier is represented as an SHA256 over the field declarations.
+ The fields in the shared C# record declaration are ordered by name.

#### Record Constructors

The order of parameters in a record constructor depends on the order of appearance in the `type alias`.

For each `type alias RecordName = { ... }` declaration, we emit a declaration named `RecordName_Constructor_Curried` which returns the curried function representation.

For example, for this Elm declaration:

```Elm
type alias HttpRequestEventStruct =
    { httpRequestId : String
    , posixTimeMilli : Int
    }
```

We emit a C# declaration as follows:

```C#
public static readonly Func<string, Func<BigInteger, UniqueRecords.Record_0123456789>> HttpRequestEventStruct_Construct_Curried =
    httpRequestId =>
    posixTimeMilli =>
    new UniqueRecords.Record_0123456789(httpRequestId, posixTimeMilli);
```

#### Record Field Name Collisions

Elm allows any lowercase identifier as a record field name, but some of these collide with reserved words or built-in members in C#.

+ Field names that are C# reserved words (e.g., `class`, `string`, `int`, `event`) are escaped with the `@` prefix in C#.
+ Field names that collide with members inherited from `System.Object` (e.g., `ToString`, `GetHashCode`, `Equals`) do not need escaping because C# record positional parameters and the corresponding properties live in a different declaration space than inherited methods. The new property hides the inherited member, which is acceptable here because the inherited members are not used directly on Elm-compiled records.

For example, for this Elm declaration:

```Elm
type alias Config =
    { class : String
    , value : Int
    }
```

The shared C# record is emitted as:

```C#
public record Record_abcdef1234(string @class, BigInteger value);
```

#### Generic Records

When a record type alias has type variables, the shared C# record declaration is generic. The record shape identifier (SHA256) is computed over the field names and their types, with type variables normalized to positional placeholders.

+ Type parameters on the shared C# record are named `T0`, `T1`, `T2`, etc., assigned in the order they first appear when iterating the fields alphabetically.

For example, for this Elm declaration:

```Elm
type alias Pair a b =
    { first : a
    , second : b
    }
```

The fields sorted alphabetically are `first` (type `a`) and `second` (type `b`). Type variable `a` appears first, so it maps to `T0`; `b` appears second, so it maps to `T1`. The shared C# record is:

```C#
public record Record_fedcba9876<T0, T1>(T0 first, T1 second);
```

The constructor declaration follows the same rules as non-generic records, but the parameter order follows the original `type alias` field order:

```C#
public static Func<T0, Func<T1, Record_fedcba9876<T0, T1>>> Pair_Construct_Curried<T0, T1>() =>
    first =>
    second =>
    new Record_fedcba9876<T0, T1>(first, second);
```

When two type aliases in different modules share the same record shape (same field names in the same order, same type variable structure), they both reference the same shared generic record type.

For example, if module A declares `type alias Pair a b = { first : a, second : b }` and module B declares `type alias Entry k v = { first : k, second : v }`, both emit constructors returning `Record_fedcba9876<T0, T1>`.

#### Record Update

For a record update expression, we emit the `with` expression in C#.

#### Open Records

Open records pose challenges regarding record access and record update expressions.

To resolve this, we use a monomorphizing design: Where a function in the source code uses an open record type, the compiler must enumerate all concrete record types (field name sets) used with this function, and then emit a specialized variant of that function for each of those record types.

In case the implementation of type inference is insufficient to compute this set of record types, the compiler exits with an error message.

### Primitive Types

Primitive types from Elm are emitted as follows:

+ `Char`: `System.Char`
+ `String`: `System.String`
+ `Int`: `System.Numerics.BigInteger`
+ `Float`: `System.Decimal`

### `case   of` Blocks

Depending on the context, we might use either a switch statement or a switch expression to implement branching. In any case, we add a branch that throws `throw new UnreachableException` to account for the lack of exhaustiveness checking in C#.

### Core and Kernel Modules

For modules with kernel dependencies, as well as some other modules from the core libraries, we ship representations in .NET assemblies and have the Elm compiler reference them instead of compiling them from source.

We ship the following libraries in .NET assemblies in the namespace `Pine.Core.Elm.ElmInDotNet`:

+ `elm/core`: The `Platform`, `Process` and `Task` modules are excluded.
+ `elm/json`
+ `elm/parser`
+ `elm/bytes`
+ `elm/url`
+ `elm/regex`

#### `Debug.todo`

For applications of `Debug.todo`, we emit a `throw` expression or `throw` statement packaging the `String` argument in an instance of `Pine.Core.Elm.ElmInDotNet.DebugTodoException`

#### `Debug.log`

For functions that use `Debug.log`, we emit a variant of that function that takes an additional parameter, a delegate `System.Action<string, object>` to invoke for each application of `Debug.log`. This parameter is placed first in the parameter list in C#.

The same applies to any functions that indirectly depend on a function using `Debug.log`: The caller supplies the delegate at the entry point, and it's propagated through the call stack.

Partial application of `Debug.log` is not supported. If the source code contains a partial application of `Debug.log`, we reject this with an error message.

### Tail Recursion

We emit tail recursive calls as C# loops to reduce stack overflows.

#### Proper Exit for Infinite Recursion

Some libraries use an infinite-recursion pattern for error cases to simplify return types of functions. This pattern assumes that such a case will lead to a stack overflow, crashing the program.
Our compiler proves infinite recursion in trivial cases and throws an exception of type `Pine.Core.Elm.ElmInDotNet.InfiniteRecursionException` in this case, so that programs don’t just get stuck in such a case.

### Namespaces of Modules and Declarations

Designing an Elm-to-C# compiler requires a deliberate naming scheme because Elm’s module system and declaration naming rules do not map cleanly onto C#’s unified identifier spaces and type-centric organization. Elm treats the module name as a qualification path (e.g., `Json.Decode`) while allowing module names to coexist with same-spelled declarations.

The collision scenarios that make Elm-to-C# name mapping hard stem from the following constraints:

1. **C# requires all code to live inside types.** Elm top-level functions have no natural home.
2. **C# forbids a nested type from sharing its enclosing type's name.** The `Result.Result` pattern is a compile error.
3. **C# conflates namespaces and types.**

From the docs on [Compiler Error CS0101](https://learn.microsoft.com/dotnet/csharp/misc/cs0101):

> A CS0101 is also generated when your class name clashes with your namespace name.

+ To resolve the collisions between namespace names and declaration names, we add the prefix `Module_` on each module name and the prefix `Namespace_` on each module namespace name.
+ To solve the hosting of function declarations, we create a `public static class` as container.

For example, the following Elm code:

```Elm
module Bytes.Encode


getStringWidth : String -> Int

```

Is compiled to the following C# code:

```C#
namespace Namespace_Bytes


public static class Module_Encode
{
    public static BigInteger getStringWidth(string first) { }
}
```

