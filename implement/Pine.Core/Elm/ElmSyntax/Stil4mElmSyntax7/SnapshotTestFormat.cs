using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class SnapshotTestFormat
{
    /// <summary>
    /// Format an Elm file with proper location assignment for multi-line canonical format.
    /// </summary>
    public static File Format(File file)
    {
        var context = new FormattingContext(currentRow: 1, currentColumn: 1, indentLevel: 0);
        
        // Format module definition
        var (formattedModule, contextAfterModule) = FormatModuleDefinition(file.ModuleDefinition, context);
        
        // Add blank line after module
        var contextAfterModuleBlank = contextAfterModule.NextRow().NextRow();
        
        // Format imports
        var (formattedImports, contextAfterImports) = FormatImports(file.Imports, contextAfterModuleBlank);
        
        // Add blank lines after imports (double blank line before first declaration)
        var contextBeforeDecls = formattedImports.Any()
            ? contextAfterImports.NextRow().NextRow().NextRow()
            : contextAfterModuleBlank;
        
        // For now, keep declarations as-is (TODO: implement declaration formatting)
        var formattedDeclarations = file.Declarations
            .Select(decl => new Node<Declaration>(
                Range: new Range(contextBeforeDecls.ToLocation(), contextBeforeDecls.ToLocation()),
                Value: decl.Value))
            .ToList();
        
        return new File(
            ModuleDefinition: formattedModule,
            Imports: formattedImports,
            Declarations: formattedDeclarations,
            Comments: []);
    }
    
    private static (Node<Module>, FormattingContext) FormatModuleDefinition(
        Node<Module> module,
        FormattingContext context)
    {
        return module.Value switch
        {
            Module.NormalModule nm => FormatNormalModule(nm, context),
            Module.PortModule pm => FormatPortModule(pm, context),
            Module.EffectModule em => FormatEffectModule(em, context),
            _ => (module, context)
        };
    }
    
    private static (Node<Module>, FormattingContext) FormatNormalModule(
        Module.NormalModule module,
        FormattingContext context)
    {
        // "module " (7 chars)
        var afterModuleKeyword = context.Advance(7);
        
        // Module name
        var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
        var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1); // +1 for space
        
        // Format exposing list
        var (formattedExposing, contextAfterExposing) = FormatExposingList(
            module.ModuleData.ExposingList,
            afterModuleName,
            isModuleExposing: true
        );
        
        var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
        var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);
        
        var formattedModuleData = new DefaultModuleData(
            ModuleName: moduleNameNode,
            ExposingList: formattedExposing
        );
        
        return (new Node<Module>(
            range,
            new Module.NormalModule(formattedModuleData)
        ), contextAfterExposing);
    }
    
    private static (Node<Module>, FormattingContext) FormatPortModule(
        Module.PortModule module,
        FormattingContext context)
    {
        // "port module " (12 chars)
        var afterKeyword = context.Advance(12);
        
        var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
        var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);
        
        var (formattedExposing, contextAfterExposing) = FormatExposingList(
            module.ModuleData.ExposingList,
            afterModuleName,
            isModuleExposing: true
        );
        
        var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
        var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);
        
        var formattedModuleData = new DefaultModuleData(
            ModuleName: moduleNameNode,
            ExposingList: formattedExposing
        );
        
        return (new Node<Module>(
            range,
            new Module.PortModule(formattedModuleData)
        ), contextAfterExposing);
    }
    
    private static (Node<Module>, FormattingContext) FormatEffectModule(
        Module.EffectModule module,
        FormattingContext context)
    {
        // "effect module " (14 chars)
        var afterKeyword = context.Advance(14);
        
        var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
        var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);
        
        var (formattedExposing, contextAfterExposing) = FormatExposingList(
            module.ModuleData.ExposingList,
            afterModuleName,
            isModuleExposing: true
        );
        
        var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
        var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);
        
        var formattedModuleData = new EffectModuleData(
            ModuleName: moduleNameNode,
            ExposingList: formattedExposing,
            Command: module.ModuleData.Command,
            Subscription: module.ModuleData.Subscription
        );
        
        return (new Node<Module>(
            range,
            new Module.EffectModule(formattedModuleData)
        ), contextAfterExposing);
    }
    
    private static (Node<Exposing>, FormattingContext) FormatExposingList(
        Node<Exposing> exposing,
        FormattingContext context,
        bool isModuleExposing)
    {
        return exposing.Value switch
        {
            Exposing.All all => FormatExposingAll(all, context),
            Exposing.Explicit explicitList => FormatExposingExplicit(explicitList, context, isModuleExposing),
            _ => (exposing, context)
        };
    }
    
    private static (Node<Exposing>, FormattingContext) FormatExposingAll(
        Exposing.All all,
        FormattingContext context)
    {
        // "exposing (..)"
        var afterExposing = context.Advance(9); // "exposing "
        var afterOpenParen = afterExposing.Advance(1); // "("
        var afterDots = afterOpenParen.Advance(2); // ".."
        var afterCloseParen = afterDots.Advance(1); // ")"
        
        var allRange = new Range(afterOpenParen.ToLocation(), afterDots.ToLocation());
        var exposingRange = new Range(context.ToLocation(), afterCloseParen.ToLocation());
        
        return (new Node<Exposing>(
            exposingRange,
            new Exposing.All(allRange)
        ), afterCloseParen);
    }
    
    private static (Node<Exposing>, FormattingContext) FormatExposingExplicit(
        Exposing.Explicit explicitList,
        FormattingContext context,
        bool isModuleExposing)
    {
        var nodes = explicitList.Nodes;
        
        if (nodes.Count == 0)
        {
            // "exposing ()"
            var afterExposing = context.Advance(11); // "exposing ()"
            return (new Node<Exposing>(
                new Range(context.ToLocation(), afterExposing.ToLocation()),
                new Exposing.Explicit([])
            ), afterExposing);
        }
        
        // For module exposing with multiple items, use multi-line format
        var useMultiLine = isModuleExposing && nodes.Count > 1;
        
        if (useMultiLine)
        {
            // "exposing"
            var afterExposing = context.Advance(8); // "exposing"
            
            // Next line, indented 4 spaces: "    ( item1"
            var nextLine = afterExposing.NextRow().SetColumn(context.currentColumn + 4);
            var afterOpenParen = nextLine.Advance(2); // "( "
            
            var formattedNodes = new List<Node<TopLevelExpose>>();
            var currentContext = afterOpenParen;
            
            for (int i = 0; i < nodes.Count; i++)
            {
                if (i > 0)
                {
                    // Comma at start of line: "    , item"
                    currentContext = currentContext.NextRow().SetColumn(context.currentColumn + 4);
                    currentContext = currentContext.Advance(2); // ", "
                }
                
                var (formattedNode, contextAfter) = FormatTopLevelExpose(nodes[i], currentContext);
                formattedNodes.Add(formattedNode);
                currentContext = contextAfter;
            }
            
            // Closing paren on next line: "    )"
            var closeLine = currentContext.NextRow().SetColumn(context.currentColumn + 4);
            var afterCloseParen = closeLine.Advance(1); // ")"
            
            return (new Node<Exposing>(
                new Range(context.ToLocation(), afterCloseParen.ToLocation()),
                new Exposing.Explicit(formattedNodes)
            ), afterCloseParen);
        }
        else
        {
            // Single-line: "exposing (item1, item2)"
            var afterExposing = context.Advance(9); // "exposing "
            var afterOpenParen = afterExposing.Advance(1); // "("
            
            var formattedNodes = new List<Node<TopLevelExpose>>();
            var currentContext = afterOpenParen;
            
            for (int i = 0; i < nodes.Count; i++)
            {
                if (i > 0)
                {
                    currentContext = currentContext.Advance(2); // ", "
                }
                
                var (formattedNode, contextAfter) = FormatTopLevelExpose(nodes[i], currentContext);
                formattedNodes.Add(formattedNode);
                currentContext = contextAfter;
            }
            
            var afterCloseParen = currentContext.Advance(1); // ")"
            
            return (new Node<Exposing>(
                new Range(context.ToLocation(), afterCloseParen.ToLocation()),
                new Exposing.Explicit(formattedNodes)
            ), afterCloseParen);
        }
    }
    
    private static (Node<TopLevelExpose>, FormattingContext) FormatTopLevelExpose(
        Node<TopLevelExpose> expose,
        FormattingContext context)
    {
        return expose.Value switch
        {
            TopLevelExpose.InfixExpose infix => FormatInfixExpose(infix, context),
            TopLevelExpose.FunctionExpose func => FormatFunctionExpose(func, context),
            TopLevelExpose.TypeOrAliasExpose typeOrAlias => FormatTypeOrAliasExpose(typeOrAlias, context),
            TopLevelExpose.TypeExpose typeExpose => FormatTypeExpose(typeExpose, context),
            _ => (expose, context)
        };
    }
    
    private static (Node<TopLevelExpose>, FormattingContext) FormatInfixExpose(
        TopLevelExpose.InfixExpose infix,
        FormattingContext context)
    {
        // "(op)"
        var afterOpenParen = context.Advance(1);
        var afterOp = afterOpenParen.Advance(infix.Name.Length);
        var afterCloseParen = afterOp.Advance(1);
        
        var range = new Range(context.ToLocation(), afterCloseParen.ToLocation());
        
        return (new Node<TopLevelExpose>(
            range,
            infix
        ), afterCloseParen);
    }
    
    private static (Node<TopLevelExpose>, FormattingContext) FormatFunctionExpose(
        TopLevelExpose.FunctionExpose func,
        FormattingContext context)
    {
        var afterName = context.Advance(func.Name.Length);
        
        var range = new Range(context.ToLocation(), afterName.ToLocation());
        
        return (new Node<TopLevelExpose>(
            range,
            func
        ), afterName);
    }
    
    private static (Node<TopLevelExpose>, FormattingContext) FormatTypeOrAliasExpose(
        TopLevelExpose.TypeOrAliasExpose typeOrAlias,
        FormattingContext context)
    {
        var afterName = context.Advance(typeOrAlias.Name.Length);
        
        var range = new Range(context.ToLocation(), afterName.ToLocation());
        
        return (new Node<TopLevelExpose>(
            range,
            typeOrAlias
        ), afterName);
    }
    
    private static (Node<TopLevelExpose>, FormattingContext) FormatTypeExpose(
        TopLevelExpose.TypeExpose typeExpose,
        FormattingContext context)
    {
        // "TypeName" or "TypeName(..)"
        var afterName = context.Advance(typeExpose.ExposedType.Name.Length);
        
        if (typeExpose.ExposedType.Open != null)
        {
            // "(..))"
            var afterOpenParen = afterName.Advance(1);
            var dotsStart = afterOpenParen;
            var afterDots = afterOpenParen.Advance(2);
            var afterCloseParen = afterDots.Advance(1);
            
            var dotsRange = new Range(dotsStart.ToLocation(), afterDots.ToLocation());
            var range = new Range(context.ToLocation(), afterCloseParen.ToLocation());
            
            var formatted = new TopLevelExpose.TypeExpose(
                new ExposedType(typeExpose.ExposedType.Name, dotsRange)
            );
            
            return (new Node<TopLevelExpose>(range, formatted), afterCloseParen);
        }
        
        var rangeNoOpen = new Range(context.ToLocation(), afterName.ToLocation());
        var formattedNoOpen = new TopLevelExpose.TypeExpose(
            new ExposedType(typeExpose.ExposedType.Name, null)
        );
        
        return (new Node<TopLevelExpose>(rangeNoOpen, formattedNoOpen), afterName);
    }
    
    private static (List<Node<Import>>, FormattingContext) FormatImports(
        IReadOnlyList<Node<Import>> imports,
        FormattingContext context)
    {
        if (imports.Count == 0)
        {
            return ([], context);
        }
        
        var formatted = new List<Node<Import>>();
        var currentContext = context;
        
        for (int i = 0; i < imports.Count; i++)
        {
            if (i > 0)
            {
                currentContext = currentContext.NextRow();
            }
            
            var (formattedImport, contextAfter) = FormatImport(imports[i], currentContext);
            formatted.Add(formattedImport);
            currentContext = contextAfter;
        }
        
        return (formatted, currentContext);
    }
    
    private static (Node<Import>, FormattingContext) FormatImport(
        Node<Import> import,
        FormattingContext context)
    {
        // "import ModuleName"
        var afterImport = context.Advance(7); // "import "
        var moduleName = string.Join(".", import.Value.ModuleName);
        var afterModuleName = afterImport.Advance(moduleName.Length);
        
        var currentContext = afterModuleName;
        
        // Handle module alias
        if (import.Value.ModuleAlias != null)
        {
            currentContext = currentContext.Advance(4); // " as "
            var aliasName = string.Join(".", import.Value.ModuleAlias);
            currentContext = currentContext.Advance(aliasName.Length);
        }
        
        // Handle exposing
        Node<Exposing>? formattedExposing = null;
        var contextAfterExposing = currentContext;
        
        if (import.Value.ExposingList != null)
        {
            currentContext = currentContext.Advance(1); // space before "exposing"
            (formattedExposing, contextAfterExposing) = FormatExposingList(
                import.Value.ExposingList,
                currentContext,
                isModuleExposing: false
            );
        }
        
        var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
        
        return (new Node<Import>(
            range,
            new Import(
                import.Value.ModuleName,
                import.Value.ModuleAlias,
                formattedExposing
            )
        ), contextAfterExposing);
    }
    
    // Helper class to track current position while formatting
    private record FormattingContext(int currentRow, int currentColumn, int indentLevel)
    {
        public Location ToLocation() => new Location(currentRow, currentColumn);
        
        public FormattingContext NextRow() => this with { currentRow = currentRow + 1, currentColumn = 1 };
        
        public FormattingContext SetColumn(int column) => this with { currentColumn = column };
        
        public FormattingContext Advance(int count) => this with { currentColumn = currentColumn + count };
        
        public FormattingContext Indent() => this with { indentLevel = indentLevel + 1 };
        
        public FormattingContext Dedent() => this with { indentLevel = indentLevel - 1 };
    }
}
