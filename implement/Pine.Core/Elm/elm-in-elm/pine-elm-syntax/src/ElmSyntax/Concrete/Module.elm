module ElmSyntax.Concrete.Module exposing (..)

import ElmSyntax.Concrete.Exposing exposing (Exposing)
import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)



{-


/// <summary>
/// Elm module kinds: normal, port or effect (with possible commands/subscriptions).
/// </summary>
public abstract record Module
{
    /// <summary>Standard module.</summary>
    public sealed record NormalModule(
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Port module exposing native interop.</summary>
    public sealed record PortModule(
        Location PortTokenLocation,
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Effect module with commands and subscriptions.</summary>
    public sealed record EffectModule(
        Location EffectTokenLocation,
        Location ModuleTokenLocation,
        EffectModuleData ModuleData)
        : Module;

    /// <summary>
    /// Retrieve the module name syntax node for the specified module instance.
    /// </summary>
    public static Node<ModuleName> GetModuleName(Module module) =>
        module switch
        {
            NormalModule normalModule =>
            normalModule.ModuleData.ModuleName,

            PortModule portModule =>
            portModule.ModuleData.ModuleName,

            EffectModule effectModule =>
            effectModule.ModuleData.ModuleName,

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };
}

/// <summary>
/// Shared data for normal/port modules: name and exposing list.
/// </summary>
public record DefaultModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList);

/// <summary>
/// Data for effect modules including optional command and subscription identifiers.
/// </summary>
public record EffectModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList,
    Node<string>? Command,
    Node<string>? Subscription);

/// <summary>
/// How a module exposes its definitions: all or explicit list.
/// </summary>
public abstract record Exposing
{
    /// <summary>Expose everything (represented by .. in Elm).</summary>
    public sealed record All(
        Range Range)
        : Exposing;

    /// <summary>Explicit list of exposed top-level items.</summary>
    public sealed record Explicit(
        Location OpenParenLocation,
        SeparatedSyntaxList<Node<TopLevelExpose>> Nodes,
        Location CloseParenLocation)
        : Exposing;
}

/// <summary>
/// Individual item exposed from a module: operators, functions, types, or type with constructors.
/// </summary>
public abstract record TopLevelExpose
{
    /// <summary>Exposed operator (infix).</summary>
    public sealed record InfixExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed function or value.</summary>
    public sealed record FunctionExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed type or type alias without constructors.</summary>
    public sealed record TypeOrAliasExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed choice type including maybe constructors via exposed range.</summary>
    public sealed record TypeExpose(
        ExposedType ExposedType)
        : TopLevelExpose;
}

/// <summary>
/// Choice type exposure specification: name and optional opening range for constructors.
/// </summary>
public record ExposedType(
    string Name,
    Range? Open);

-}


type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


type alias DefaultModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    }


type alias EffectModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    , command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }


type alias ModuleName =
    List String
