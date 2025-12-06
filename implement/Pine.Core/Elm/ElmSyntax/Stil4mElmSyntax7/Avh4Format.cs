using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Format Elm syntax trees following the style of https://github.com/avh4/elm-format
/// </summary>
public class Avh4Format
{
    public static File Format(File file)
    {
        var defaultRange = new Range(
            Start: new Location(Row: 1, Column: 1),
            End: new Location(Row: 1, Column: 1));

        var formattedModuleDefinition = FormatModuleDefinition(file.ModuleDefinition, defaultRange);

        var formattedImports = file.Imports
            .Select(imp => FormatImport(imp, defaultRange))
            .ToList();

        var formattedDeclarations = file.Declarations
            .Select(decl => FormatDeclaration(decl, defaultRange))
            .ToList();

        return new File(
            ModuleDefinition: formattedModuleDefinition,
            Imports: formattedImports,
            Declarations: formattedDeclarations,
            Comments: []);
    }

    private static Node<Module> FormatModuleDefinition(Node<Module> module, Range defaultRange)
    {
        var formatted = module switch
        {
            { Value: Module.NormalModule normalModule } => new Module.NormalModule(
                new DefaultModuleData(
                    ModuleName: new Node<IReadOnlyList<string>>(defaultRange, normalModule.ModuleData.ModuleName.Value),
                    ExposingList: FormatExposing(normalModule.ModuleData.ExposingList, defaultRange))),
            { Value: Module.PortModule portModule } => new Module.PortModule(
                new DefaultModuleData(
                    ModuleName: new Node<IReadOnlyList<string>>(defaultRange, portModule.ModuleData.ModuleName.Value),
                    ExposingList: FormatExposing(portModule.ModuleData.ExposingList, defaultRange))),
            { Value: Module.EffectModule effectModule } => new Module.EffectModule(
                new EffectModuleData(
                    ModuleName: new Node<IReadOnlyList<string>>(defaultRange, effectModule.ModuleData.ModuleName.Value),
                    ExposingList: FormatExposing(effectModule.ModuleData.ExposingList, defaultRange),
                    Command: effectModule.ModuleData.Command,
                    Subscription: effectModule.ModuleData.Subscription)),
            _ => module.Value
        };

        return new Node<Module>(defaultRange, formatted);
    }

    private static Node<Exposing> FormatExposing(Node<Exposing> exposing, Range defaultRange)
    {
        var formattedItem = exposing.Value switch
        {
            Exposing.All all => new Exposing.All(defaultRange),
            Exposing.Explicit explicitList => new Exposing.Explicit(
                explicitList.Nodes
                    .Select(node => new Node<TopLevelExpose>(
                        defaultRange,
                        FormatTopLevelExpose(node.Value, defaultRange)))
                    .ToList()),
            _ => exposing.Value
        };

        return new Node<Exposing>(defaultRange, formattedItem);
    }

    private static TopLevelExpose FormatTopLevelExpose(TopLevelExpose expose, Range defaultRange)
    {
        return expose switch
        {
            TopLevelExpose.InfixExpose infix => infix,
            TopLevelExpose.FunctionExpose func => func,
            TopLevelExpose.TypeOrAliasExpose typeOrAlias => typeOrAlias,
            TopLevelExpose.TypeExpose typeExpose => new TopLevelExpose.TypeExpose(
                new ExposedType(
                    typeExpose.ExposedType.Name,
                    typeExpose.ExposedType.Open is null ? null : defaultRange)),
            _ => expose
        };
    }

    private static Node<Import> FormatImport(Node<Import> import, Range defaultRange)
    {
        return new Node<Import>(
            defaultRange,
            new Import(
                import.Value.ModuleName,
                import.Value.ModuleAlias,
                import.Value.ExposingList is null ? null :
                    FormatExposing(import.Value.ExposingList, defaultRange)));
    }

    private static Node<Declaration> FormatDeclaration(Node<Declaration> decl, Range defaultRange)
    {
        // For now, just return the declaration with default range
        // Full formatting would require recursively processing all expression and type nodes
        return new Node<Declaration>(defaultRange, decl.Value);
    }
}
