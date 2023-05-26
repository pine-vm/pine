using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Json;

public class JsonConverterForChoiceType : JsonConverterFactory
{
    public record ParsedType(IReadOnlyList<ParsedType.Variant> Variants)
    {
        public record Variant(
            string Name,
            Type ClrType,
            ConstructorInfo Constructor,
            IReadOnlyList<ConstructorParameter> ConstructorParameters);
    }

    public record ConstructorParameter(
        PropertyInfo PropertyInfo,
        JsonIgnore? JsonIgnore);

    public record JsonIgnore(object Default);

    private static readonly ConcurrentDictionary<Type, Result<string, ParsedType>> parseTypeToConvertCache = new();

    public override bool CanConvert(Type typeToConvert) => CachedParseTypeToConvert(typeToConvert).IsOk();

    public override JsonConverter CreateConverter(
        Type typeToConvert, JsonSerializerOptions options)
    {
        JsonConverter converter = (JsonConverter)Activator.CreateInstance(
            typeof(JsonConverterForChoiceType<>)
            .MakeGenericType(typeToConvert),
            BindingFlags.Instance | BindingFlags.Public,
            binder: null,
            args: null,
            culture: null)!;

        return converter;
    }

    public static Result<string, ParsedType> CachedParseTypeToConvert(Type typeToConvert) =>
        parseTypeToConvertCache.GetOrAdd(typeToConvert, valueFactory: ParseTypeToConvert);

    public static Result<string, ParsedType> ParseTypeToConvert(Type typeToConvert)
    {
        var matchingNestedTypes =
            typeToConvert.GetNestedTypes()
            .Select(nestedType =>
            {
                if (typeToConvert.IsAssignableFrom(nestedType))
                    return nestedType;

                if (typeToConvert.IsGenericType && nestedType.IsGenericTypeDefinition)
                {
                    var typeToConvertGenericTypeDefinition = typeToConvert.GetGenericTypeDefinition();

                    var typeToConvertGenericArguments = typeToConvert.GetGenericArguments();

                    var genericFromNestedType = nestedType.MakeGenericType(typeToConvertGenericArguments);

                    if (!genericFromNestedType.ContainsGenericParameters)
                        return genericFromNestedType;
                }

                return null;
            })
            .WhereNotNull()
            .ToImmutableList();

        var variantsResults =
            matchingNestedTypes
            .Select(nestedType => ParseUnionTypeVariant(nestedType).MapError(error => "Failed for nested type " + nestedType.Name + " :" + error))
            .ToImmutableList();

        return
            variantsResults
            .ListCombine()
            .AndThen(variants =>
            variants.Count < 1 ?
            Result<string, ParsedType>.err("Did not find any variant declaration in this type")
            : Result<string, ParsedType>.ok(new ParsedType(variants)));
    }

    private static Result<string, ParsedType.Variant> ParseUnionTypeVariant(Type variantType)
    {
        static object DefaultValueFromType(Type type)
        {
            if (type.IsValueType)
            {
                return Activator.CreateInstance(type);
            }

            return null;
        }

        var allProperties = variantType.GetProperties();

        var constructorsResults =
            variantType.GetConstructors().OrderBy(c => c.IsPublic ? 0 : 1).ThenByDescending(c => c.GetParameters().Length)
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
                            return Result<string, ConstructorParameter>.err(
                                "Did not find a matching property for constructor param " + constructorParam.Name);

                        JsonIgnore? jsonIgnore = null;

                        if (constructorParamProperty.CustomAttributes.Any(ca => ca.AttributeType.Equals(typeof(JsonIgnoreAttribute))))
                            jsonIgnore = new JsonIgnore(DefaultValueFromType(constructorParamProperty.PropertyType));

                        return Result<string, ConstructorParameter>.ok(
                            new ConstructorParameter(
                                PropertyInfo: constructorParamProperty,
                                JsonIgnore: jsonIgnore));

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
            if (constructorResult is Result<string, (ConstructorInfo, IReadOnlyList<ConstructorParameter>)>.Ok constructorMatch)
                return Result<string, ParsedType.Variant>.ok(
                    new ParsedType.Variant(variantType.Name, variantType, constructorMatch.Value.Item1, constructorMatch.Value.Item2));
        }

        return Result<string, ParsedType.Variant>.err("Did not find a matching constructor");
    }
}

public class JsonConverterForChoiceType<T> : JsonConverter<T>
{
    private static readonly JsonConverterForChoiceType.ParsedType ParsedType =
        JsonConverterForChoiceType.CachedParseTypeToConvert(typeof(T)).Extract(error => throw new Exception(error));

    public override T Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType != JsonTokenType.StartObject)
        {
            throw new JsonException("Expected start object");
        }
        reader.Read();

        var variantsNames = ParsedType.Variants.Select(v => v.Name).ToImmutableList();

        if (reader.TokenType != JsonTokenType.PropertyName)
        {
            throw new JsonException("Expected property name next");
        }

        string? propertyName = reader.GetString();

        reader.Read();
        if (reader.TokenType != JsonTokenType.StartArray)
        {
            throw new JsonException("Expected start array");
        }

        reader.Read();

        var variant =
            ParsedType.Variants.Where(c => c.Name == propertyName)
            .DefaultIfEmpty(null)
            .FirstOrDefault();

        if (variant is null)
            throw new JsonException(
                "Unexpected variant name: " + propertyName +
                ". Expected one of these " + variantsNames.Count + " names: " + string.Join(", ", variantsNames));

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

        if (reader.TokenType != JsonTokenType.EndArray)
        {
            throw new JsonException("Expected end array");
        }

        reader.Read();
        if (reader.TokenType != JsonTokenType.EndObject)
        {
            throw new JsonException("Expected end object");
        }

        return result;
    }

    public override void Write(
        Utf8JsonWriter writer, T value, JsonSerializerOptions options)
    {
        if (value is null)
        {
            writer.WriteNullValue();
            return;
        }

        writer.WriteStartObject();

        var variant =
            ParsedType.Variants
            .Where(v => v.ClrType.IsAssignableFrom(value.GetType()))
            .DefaultIfEmpty(null)
            .FirstOrDefault();

        if (variant == null)
        {
            throw new JsonException("Type " + value.GetType().FullName + " not registered as variant of " + typeof(T).FullName);
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
