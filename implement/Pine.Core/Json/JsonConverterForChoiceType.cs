using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Json;

/// <summary>
/// <para>
/// Generates JSON converters for C# types representing choice types (aka discriminated unions, tagged unions).
/// </para>
/// 
/// <para>
/// The discussions on how to add sum types to C# are ongoing, and Microsoft has not yet settled on a design.
/// For updates on the progress around sum types in C#, see also <see href="https://github.com/dotnet/csharplang/tree/main/meetings/working-groups/discriminated-unions"/> and <see href="https://github.com/dotnet/csharplang/issues/113"/>
/// </para>
/// This JSON converter factory supports the approach of representing choice types as records in C#.
/// <para>
/// For more information, see the guide in the file <see href="https://github.com/pine-vm/pine/blob/main/implement/Pine.Core/Json/JsonConverterForChoiceType.md"/>
/// </para>
/// </summary>
public class JsonConverterForChoiceType : JsonConverterFactory
{
    /// <summary>
    /// Type information about the variants of a choice type, for fast lookup during serialization and deserialization.
    /// </summary>
    /// <param name="Variants"></param>
    public record ParsedChoiceType(IReadOnlyDictionary<string, ParsedChoiceType.Variant> Variants)
    {
        /// <summary>
        /// Type information about a single variant of a choice type.
        /// </summary>
        public record Variant(
            string Name,
            Type ClrType,
            ConstructorInfo Constructor,
            IReadOnlyList<ConstructorParameter> ConstructorParameters);

        /// <summary>
        /// Creates a <see cref="ParsedChoiceType"/> from a list of variants.
        /// </summary>
        public static ParsedChoiceType Create(
            IReadOnlyList<Variant> variants) =>
            new(
                variants.ToImmutableDictionary(
                    variant => variant.Name,
                    variant => variant));
    }

    /// <summary>
    /// A constructor parameter of a variant of a choice type.
    /// </summary>
    public record ConstructorParameter(
        PropertyInfo PropertyInfo,
        JsonIgnore? JsonIgnore);

    /// <summary>
    /// An attribute to mark a property as ignored during serialization and deserialization.
    /// </summary>
    /// <param name="Default">Value to use when constructing a variant during deserialization</param>
    public record JsonIgnore(object? Default);

    private static readonly ConcurrentDictionary<Type, Result<string, ParsedChoiceType>> parseTypeToConvertCache = new();

    /// <summary>
    /// Checks if the given type is a choice type of a supported declaration format.
    /// </summary>
    public override bool CanConvert(Type typeToConvert) => CachedParseTypeToConvert(typeToConvert).IsOk();

    /// <summary>
    /// Creates a JSON converter for the given choice type.
    /// </summary>
    public override JsonConverter CreateConverter(
        Type typeToConvert, JsonSerializerOptions options)
    {
        var converter = (JsonConverter)Activator.CreateInstance(
            typeof(JsonConverterForChoiceType<>)
            .MakeGenericType(typeToConvert),
            BindingFlags.Instance | BindingFlags.Public,
            binder: null,
            args: null,
            culture: null)!;

        return converter;
    }

    /// <summary>
    /// Caches the result of <see cref="ParseTypeToConvert(Type)"/> for the given type.
    /// </summary>
    public static Result<string, ParsedChoiceType> CachedParseTypeToConvert(Type typeToConvert) =>
        parseTypeToConvertCache.GetOrAdd(typeToConvert, valueFactory: ParseTypeToConvert);

    /// <summary>
    /// Inspects the given type and returns a <see cref="ParsedChoiceType"/> with information about the variants of the choice type.
    /// </summary>
    public static Result<string, ParsedChoiceType> ParseTypeToConvert(Type typeToConvert)
    {
        var matchingNestedTypes = new List<Type>();

        foreach (var nestedType in typeToConvert.GetNestedTypes())
        {
            if (typeToConvert.IsAssignableFrom(nestedType))
            {
                matchingNestedTypes.Add(nestedType);
                continue;
            }

            if (typeToConvert.IsGenericType && nestedType.IsGenericTypeDefinition)
            {
                var typeToConvertGenericTypeDefinition = typeToConvert.GetGenericTypeDefinition();

                var typeToConvertGenericArguments = typeToConvert.GetGenericArguments();

                var genericFromNestedType = nestedType.MakeGenericType(typeToConvertGenericArguments);

                if (!genericFromNestedType.ContainsGenericParameters)
                {
                    matchingNestedTypes.Add(genericFromNestedType);
                    continue;
                }
            }
        }

        var variants = new List<ParsedChoiceType.Variant>();

        foreach (var nestedType in matchingNestedTypes)
        {
            var variantResult = ParseChoiceTypeVariant(nestedType);

            if (variantResult.IsErrOrNull() is { } error)
            {
                return "Failed for nested type " + nestedType.Name + " :" + error;
            }

            if (variantResult.IsOkOrNull() is not { } variant)
            {
                throw new NotImplementedException(
                    "Unexpected result from ParseChoiceTypeVariant: " + variantResult);
            }

            variants.Add(variant);
        }

        if (variants.Count < 1)
        {
            return "Did not find any variant declaration in this type";
        }

        return ParsedChoiceType.Create(variants);
    }

    private static Result<string, ParsedChoiceType.Variant> ParseChoiceTypeVariant(Type variantType)
    {
        static object? DefaultValueFromType(Type type)
        {
            if (type.IsValueType)
            {
                return Activator.CreateInstance(type);
            }

            return null;
        }

        var allProperties = variantType.GetProperties();

        var constructorsResults =
            variantType.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
            .OrderBy(c => c.IsPublic ? 0 : 1)
            .ThenByDescending(c => c.GetParameters().Length)
            .Select(constructor =>
            {
                var constructorParams = constructor.GetParameters();

                var constructorParamPropertiesResults =
                    constructorParams
                    .Select((constructorParam, i) =>
                    {
                        var constructorParamProperty =
                            allProperties
                            .FirstOrDefault(p => string.Equals(p.Name, constructorParams[i].Name, StringComparison.OrdinalIgnoreCase));

                        if (constructorParamProperty is null)
                        {
                            return
                                (Result<string, ConstructorParameter>)
                                "Did not find a matching property for constructor param " + constructorParam.Name;
                        }

                        JsonIgnore? jsonIgnore = null;

                        if (constructorParamProperty.CustomAttributes.Any(ca => ca.AttributeType.Equals(typeof(JsonIgnoreAttribute))))
                        {
                            jsonIgnore = new JsonIgnore(DefaultValueFromType(constructorParamProperty.PropertyType));
                        }

                        return
                            (Result<string, ConstructorParameter>)
                            new ConstructorParameter(
                                PropertyInfo: constructorParamProperty,
                                JsonIgnore: jsonIgnore);

                    })
                    .ToImmutableList();

                return
                constructorParamPropertiesResults
                .ListCombine()
                .Map(arguments => (constructor, arguments));
            })
            .ToImmutableList();

        foreach (var constructorResult in constructorsResults)
        {
            if (constructorResult.IsOkOrNullable() is { } constructorMatch)
            {
                return new ParsedChoiceType.Variant(
                    variantType.Name,
                    variantType,
                    constructorMatch.constructor,
                    constructorMatch.arguments);
            }
        }

        return "Did not find a matching constructor";
    }
}

/// <summary>
/// A JSON converter for the choice type given with <typeparamref name="T"/>.
/// 
/// Prepares for efficient serialization and deserialization of choice types, by inspecting the given type <typeparamref name="T"/>
/// and caching information about the variants and their constructors.
/// </summary>
public class JsonConverterForChoiceType<T> : JsonConverter<T>
{
    private static readonly JsonConverterForChoiceType.ParsedChoiceType ParsedType =
        JsonConverterForChoiceType.CachedParseTypeToConvert(typeof(T))
        .Extract(error => throw new Exception(error));

    /// <inheritdoc/>
    public override T Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is not JsonTokenType.StartObject)
        {
            throw new JsonException("Expected start object");
        }

        reader.Read();

        if (reader.TokenType is not JsonTokenType.PropertyName)
        {
            throw new JsonException("Expected property name next");
        }

        var propertyName =
            reader.GetString()
            ?? throw new JsonException("Expected property name");

        reader.Read();

        if (reader.TokenType is not JsonTokenType.StartArray)
        {
            throw new JsonException("Expected start array");
        }

        reader.Read();

        ParsedType.Variants.TryGetValue(propertyName, out var variant);

        if (variant is null)
        {
            throw new JsonException(
                "Unexpected variant name: " + propertyName +
                ". Expected one of these " + ParsedType.Variants.Count + " names: " +
                string.Join(", ", ParsedType.Variants.Keys));
        }

        var constructorArguments = new object?[variant.ConstructorParameters.Count];

        for (var argumentIndex = 0; argumentIndex < constructorArguments.Length; ++argumentIndex)
        {
            var constructorParameter = variant.ConstructorParameters[argumentIndex];

            if (constructorParameter.JsonIgnore is { } JsonIgnore)
            {
                constructorArguments[argumentIndex] = JsonIgnore.Default;
            }
            else
            {
                constructorArguments[argumentIndex] =
                    JsonSerializer.Deserialize(ref reader, constructorParameter.PropertyInfo.PropertyType, options);

                reader.Read();
            }
        }

        var result = (T)variant.Constructor.Invoke(constructorArguments);

        if (reader.TokenType is not JsonTokenType.EndArray)
        {
            throw new JsonException("Expected end array");
        }

        reader.Read();
        if (reader.TokenType is not JsonTokenType.EndObject)
        {
            throw new JsonException("Expected end object");
        }

        return result;
    }

    /// <inheritdoc/>
    public override void Write(
        Utf8JsonWriter writer, T value, JsonSerializerOptions options)
    {
        if (value is null)
        {
            writer.WriteNullValue();
            return;
        }

        writer.WriteStartObject();

        JsonConverterForChoiceType.ParsedChoiceType.Variant? variant = null;

        for (var i = 0; i < ParsedType.Variants.Count; ++i)
        {
            var v = ParsedType.Variants.ElementAt(i);

            if (v.Value.ClrType.IsInstanceOfType(value))
            {
                variant = v.Value;
                break;
            }
        }

        if (variant is null)
        {
            throw new JsonException(
                "Type " + value.GetType().FullName + " not registered as variant of " + typeof(T).FullName);
        }

        writer.WritePropertyName(variant.Name);

        writer.WriteStartArray();

        foreach (var constructorParameter in variant.ConstructorParameters)
        {
            if (constructorParameter.JsonIgnore is not null)
                continue;

            JsonSerializer.Serialize(
                writer,
                constructorParameter.PropertyInfo.GetValue(value),
                inputType: constructorParameter.PropertyInfo.PropertyType,
                options);
        }

        writer.WriteEndArray();

        writer.WriteEndObject();
    }
}
