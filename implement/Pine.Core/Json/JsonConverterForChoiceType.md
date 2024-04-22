# JsonConverterForChoiceType

The `JsonConverterForChoiceType` generates JSON converters for C# types representing choice types (aka discriminated unions).

The discussions on how to add sum types to C# are ongoing, and Microsoft has not yet settled on a design.

For updates on the progress around sum types in C#, see also  <https://github.com/dotnet/csharplang/tree/main/meetings/working-groups/discriminated-unions> and <https://github.com/dotnet/csharplang/issues/113>

A common solution to represent choice types already in C# 12 is using record types as follows:

```csharp
public abstract record Shape
{
    public sealed record CircleShape(CircleStruct Circle)
        : Shape;

    public record CircleStruct(int Radius);

    public sealed record RectangleShape(RectangleStruct Rectangle)
        : Shape;

    public record RectangleStruct(int Width, int Height);
}
```

In languages supporting choice types directly, the corresponding type declaration looks like this:

Elm:

```elm
type Shape
    = CircleShape { radius : Int }
    | RectangleShape { width : Int, height : Int }
```

Rust:

```rust
enum Shape {
    CircleShape { radius: i32 },
    RectangleShape { width: i32, height: i32 },
}
```

The `JsonConverterForChoiceType` supports mapping the form using C# records to a typical representation in JSON.

```json
{
  "CircleShape": [
    {
      "Radius": 123
    }
  ]
}
```

```json
{
  "RectangleShape": [
    {
      "Width": 123,
      "Height": 456
    }
  ]
}
```

To enable this mapping for a C# type in the standard .NET [`JsonSerializer`](https://learn.microsoft.com/en-us/dotnet/api/system.text.json.jsonserializer?view=net-8.0), add a `JsonConverter` attribute on the type declaration as follows:

```csharp
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Shape
{
    public sealed record CircleShape(CircleStruct Circle)
        : Shape;

    public record CircleStruct(int Radius);

    public sealed record RectangleShape(RectangleStruct Rectangle)
        : Shape;

    public record RectangleStruct(int Width, int Height);
}
```

