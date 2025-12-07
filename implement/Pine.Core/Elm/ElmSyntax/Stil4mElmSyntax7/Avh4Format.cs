using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Format Elm syntax trees following the style of https://github.com/avh4/elm-format
/// Uses FormattingContext to track position and preserve locations for rendering.
/// </summary>
public class Avh4Format
{
    public static File Format(File file)
    {
        var visitor = new Avh4FormatVisitor();
        var context = new FormattingContext(CurrentRow: 1, CurrentColumn: 1, IndentLevel: 0);

        return visitor.FormatFile(file, context);
    }

    /// <summary>
    /// Formatting context that tracks current position and indentation level.
    /// </summary>
    private record FormattingContext(
        int CurrentRow,
        int CurrentColumn,
        int IndentLevel,
        bool AddHalfIndent = false)
    {
        public Location ToLocation() =>
            new(CurrentRow, CurrentColumn);

        public FormattingContext NextRow() =>
            this with
            {
                CurrentRow = CurrentRow + 1,
                CurrentColumn = 1
            };

        public FormattingContext SetColumn(int column) =>
            this with
            {
                CurrentColumn = column
            };

        public FormattingContext Advance(int count) =>

            this with
            {
                CurrentColumn = CurrentColumn + count
            };

        public FormattingContext Indent() =>
            this with
            {
                IndentLevel = IndentLevel + 1,
                AddHalfIndent = false
            };

        public FormattingContext Dedent() =>
            this with
            {
                IndentLevel = IndentLevel - 1
            };

        public FormattingContext SetIndentColumn() =>
            this with
            {
                CurrentColumn = 1 + (IndentLevel * 4) + (AddHalfIndent ? 4 : 0)
            };

        public FormattingContext WithHalfIndent(bool addHalf) =>
            this with
            {
                AddHalfIndent = addHalf
            };
    }

    /// <summary>
    /// Visitor implementation for AVH4 formatting.
    /// </summary>
    private class Avh4FormatVisitor : ExpressionVisitorBase<FormattingContext, (Node<Expression>, FormattingContext)>
    {
        private IReadOnlyList<Node<string>> _comments = [];

        public File FormatFile(File file, FormattingContext context)
        {
            // Store comments for use during formatting
            _comments = file.Comments;

            // Format module definition
            var (formattedModule, contextAfterModule) = FormatModuleDefinition(file.ModuleDefinition, context);

            // Format imports
            FormattingContext contextAfterImports;
            IReadOnlyList<Node<Import>> formattedImports;

            if (file.Imports.Any())
            {
                // Add 1 blank line after module (2 newlines total)
                var contextBeforeImports = contextAfterModule.NextRow().NextRow();
                (formattedImports, contextAfterImports) = FormatImports(file.Imports, contextBeforeImports);
            }
            else
            {
                formattedImports = [];
                contextAfterImports = contextAfterModule;
            }

            // Add blank lines before declarations
            var contextBeforeDecls = formattedImports.Any()
                ? contextAfterImports.NextRow().NextRow()  // 1 more blank line after imports (already have 1 from last import)
                : contextAfterModule.NextRow().NextRow().NextRow();   // 2 blank lines after module if no imports

            // Format declarations
            var (formattedDeclarations, _) = FormatDeclarations(file.Declarations, contextBeforeDecls);

            return new File(
                ModuleDefinition: formattedModule,
                Imports: formattedImports,
                Declarations: formattedDeclarations,
                Comments: file.Comments);
        }

        private static (Node<Module>, FormattingContext) FormatModuleDefinition(
            Node<Module> module,
            FormattingContext context)
        {
            return module.Value switch
            {
                Module.NormalModule nm =>
                FormatNormalModule(nm, context),

                Module.PortModule pm =>
                FormatPortModule(pm, context),

                Module.EffectModule em =>
                FormatEffectModule(em, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for module type '{module.Value.GetType().Name}' is not implemented.")
            };
        }

        private static (Node<Module>, FormattingContext) FormatNormalModule(
            Module.NormalModule module,
            FormattingContext context)
        {
            // "module "
            var afterModuleKeyword = context.Advance(7);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterModuleKeyword.Advance(moduleName.Length + 1); // +1 for space

            var (formattedExposing, contextAfterExposing) = FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing
            );

            return (new Node<Module>(range, new Module.NormalModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatPortModule(
            Module.PortModule module,
            FormattingContext context)
        {
            // "port module "
            var afterKeyword = context.Advance(12);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);

            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new DefaultModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing
            );

            return (new Node<Module>(range, new Module.PortModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Module>, FormattingContext) FormatEffectModule(
            Module.EffectModule module,
            FormattingContext context)
        {
            // "effect module "
            var afterKeyword = context.Advance(14);

            var moduleName = string.Join(".", module.ModuleData.ModuleName.Value);
            var afterModuleName = afterKeyword.Advance(moduleName.Length + 1);

            var (formattedExposing, contextAfterExposing) =
                FormatExposing(module.ModuleData.ExposingList, afterModuleName);

            var range = new Range(context.ToLocation(), contextAfterExposing.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, module.ModuleData.ModuleName.Value);

            var formattedModuleData = new EffectModuleData(
                ModuleName: moduleNameNode,
                ExposingList: formattedExposing,
                Command: module.ModuleData.Command,
                Subscription: module.ModuleData.Subscription
            );

            return (new Node<Module>(range, new Module.EffectModule(formattedModuleData)), contextAfterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposing(
            Node<Exposing> exposing,
            FormattingContext context)
        {
            return exposing.Value switch
            {
                Exposing.All =>
                FormatExposingAll(context),

                Exposing.Explicit explicitList =>
                FormatExposingExplicit(explicitList, context, IsExposingListMultiLine(explicitList)),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for exposing type '{exposing.Value.GetType().Name}' is not implemented.")
            };
        }

        private static (Node<Exposing>, FormattingContext) FormatExposingAll(FormattingContext context)
        {
            // "exposing (..)"
            var afterExposing = context.Advance(13);
            var range = new Range(context.ToLocation(), afterExposing.ToLocation());
            return (new Node<Exposing>(range, new Exposing.All(range)), afterExposing);
        }

        private static (Node<Exposing>, FormattingContext) FormatExposingExplicit(
            Exposing.Explicit explicitList,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "exposing (item1, item2, item3)"
                var afterOpenParen = context.Advance(10); // "exposing ("

                var currentContext = afterOpenParen;
                var formattedNodes = new List<Node<TopLevelExpose>>();

                for (var i = 0; i < explicitList.Nodes.Count; i++)
                {
                    var node = explicitList.Nodes[i];
                    var (formattedNode, nextContext) = FormatTopLevelExpose(node, currentContext);
                    formattedNodes.Add(formattedNode);

                    currentContext = i < explicitList.Nodes.Count - 1
                        ? nextContext.Advance(2) // ", "
                        : nextContext.Advance(1); // ")"
                }

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), currentContext);
            }
            else
            {
                // Multi-line: first item on same line as paren, rest on new lines
                var afterExposingKeyword = context.Advance(8); // "exposing"

                // Move to next line and indent for opening paren
                var parenLineContext = afterExposingKeyword.NextRow().Indent().SetIndentColumn();

                // "( "
                var afterOpenParen = parenLineContext.Advance(2);

                var formattedNodes = new List<Node<TopLevelExpose>>();

                // First item on same line as opening paren
                var (firstNode, afterFirst) = FormatTopLevelExpose(explicitList.Nodes[0], afterOpenParen);
                formattedNodes.Add(firstNode);

                // Subsequent items on new lines with ", " prefix
                var itemContext = afterFirst.NextRow().SetIndentColumn();

                for (var i = 1; i < explicitList.Nodes.Count; i++)
                {
                    // ", "
                    var afterComma = itemContext.Advance(2);

                    var (formattedNode, nextContext) = FormatTopLevelExpose(explicitList.Nodes[i], afterComma);
                    formattedNodes.Add(formattedNode);

                    // Move to next line
                    itemContext = nextContext.NextRow().SetIndentColumn();
                }

                // ")"
                var afterCloseParen = itemContext.Advance(1);

                var finalContext = afterCloseParen.Dedent();
                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                return (new Node<Exposing>(range, new Exposing.Explicit(formattedNodes)), finalContext);
            }
        }

        /// <summary>
        /// Helper method to check if a collection of nodes spans multiple rows.
        /// Returns false for collections with fewer than 2 items.
        /// </summary>
        private static bool NodesSpanMultipleRows<T>(IReadOnlyList<Node<T>> nodes)
        {
            if (nodes.Count < 2)
                return false;

            var firstRow = nodes[0].Range.Start.Row;
            return nodes.Skip(1).Any(node => node.Range.Start.Row != firstRow);
        }

        /// <summary>
        /// Check if an exposing list should be formatted as multiline.
        /// </summary>
        private static bool IsExposingListMultiLine(Exposing.Explicit explicitList)
        {
            return NodesSpanMultipleRows(explicitList.Nodes);
        }

        private static string GetTopLevelExposeName(TopLevelExpose expose)
        {
            return expose switch
            {
                TopLevelExpose.InfixExpose infix =>
                $"({infix.Name})",

                TopLevelExpose.FunctionExpose func =>
                func.Name,

                TopLevelExpose.TypeOrAliasExpose type =>
                type.Name,

                TopLevelExpose.TypeExpose typeExpose =>
                typeExpose.ExposedType.Open is not null
                ?
                $"{typeExpose.ExposedType.Name}(..)"
                :
                typeExpose.ExposedType.Name,

                _ =>
                throw new System.NotImplementedException(
                    $"Getting name for expose type '{expose.GetType().Name}' is not implemented.")
            };
        }

        private static (Node<TopLevelExpose>, FormattingContext) FormatTopLevelExpose(
            Node<TopLevelExpose> node,
            FormattingContext context)
        {
            var exposeName = GetTopLevelExposeName(node.Value);
            var afterExpose = context.Advance(exposeName.Length);
            var range = new Range(context.ToLocation(), afterExpose.ToLocation());

            return (new Node<TopLevelExpose>(range, node.Value), afterExpose);
        }

        private static (IReadOnlyList<Node<Import>>, FormattingContext) FormatImports(
            IReadOnlyList<Node<Import>> imports,
            FormattingContext context)
        {
            if (!imports.Any())
                return ([], context);

            // Sort imports by module name in ascending order (alphabetical)
            var sortedImports =
                imports
                .OrderBy(import => string.Join(".", import.Value.ModuleName.Value))
                .ToList();

            var formattedImports = new List<Node<Import>>();
            var currentContext = context;

            foreach (var import in sortedImports)
            {
                var (formattedImport, nextContext) =
                    FormatImport(import, currentContext);

                formattedImports.Add(formattedImport);

                // Single newline between imports (no blank lines)
                currentContext = nextContext.NextRow();
            }

            return (formattedImports, currentContext);
        }

        private static (Node<Import>, FormattingContext) FormatImport(Node<Import> import, FormattingContext context)
        {
            // "import "
            var afterImportKeyword = context.Advance(7);

            // Module name
            var moduleName = string.Join(".", import.Value.ModuleName.Value);
            var afterModuleName = afterImportKeyword.Advance(moduleName.Length);

            var currentContext = afterModuleName;

            // Handle module alias if present
            if (import.Value.ModuleAlias is { } alias)
            {
                // " as "
                currentContext = currentContext.Advance(4);
                // Alias name
                var aliasName = string.Join(".", alias.Value);
                currentContext = currentContext.Advance(aliasName.Length);
            }

            // Handle exposing list if present
            if (import.Value.ExposingList is { } exposingList)
            {
                // " exposing "
                currentContext = currentContext.Advance(10);

                // Calculate the length of the exposing list text
                // For now, we'll calculate based on the original range
                var exposingTextLength = exposingList.Range.End.Column - exposingList.Range.Start.Column;
                currentContext = currentContext.Advance(exposingTextLength);
            }

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            var moduleNameNode = new Node<IReadOnlyList<string>>(range, import.Value.ModuleName.Value);

            var formattedImport = new Import(
                ModuleName: moduleNameNode,
                ModuleAlias: import.Value.ModuleAlias,  // Preserved as-is for now
                ExposingList: import.Value.ExposingList  // Preserved as-is for now
            );

            return (new Node<Import>(range, formattedImport), currentContext);
        }

        private (IReadOnlyList<Node<Declaration>>, FormattingContext) FormatDeclarations(
            IReadOnlyList<Node<Declaration>> declarations,
            FormattingContext context)
        {
            if (!declarations.Any())
                return ([], context);

            var formattedDeclarations = new List<Node<Declaration>>();
            var currentContext = context;

            for (var i = 0; i < declarations.Count; i++)
            {
                var decl = declarations[i];

                // Ensure we start each declaration at indent level 0
                var declContext = currentContext with { IndentLevel = 0 };
                var (formattedDecl, nextContext) = FormatDeclaration(decl, declContext);
                formattedDeclarations.Add(formattedDecl);

                // Spacing after declaration (except for the last one):
                // - Consecutive infix declarations: 1 newline (no blank lines)
                // - All other cases: 3 newlines (2 blank lines)
                if (i < declarations.Count - 1)
                {
                    var nextDecl = declarations[i + 1];
                    if (formattedDecl.Value is Declaration.InfixDeclaration && nextDecl.Value is Declaration.InfixDeclaration)
                    {
                        currentContext = nextContext.NextRow();
                    }
                    else
                    {
                        currentContext = nextContext.NextRow().NextRow().NextRow();
                    }
                }
                else
                {
                    currentContext = nextContext;
                }
            }

            return (formattedDeclarations, currentContext);
        }

        private (Node<Declaration>, FormattingContext) FormatDeclaration(Node<Declaration> decl, FormattingContext context)
        {
            return decl.Value switch
            {
                Declaration.FunctionDeclaration funcDecl =>
                    FormatFunctionDeclaration(funcDecl, context),

                Declaration.AliasDeclaration aliasDecl =>
                    FormatAliasDeclaration(aliasDecl, context),

                Declaration.CustomTypeDeclaration customTypeDecl =>
                    FormatCustomTypeDeclaration(customTypeDecl, context),

                Declaration.InfixDeclaration infixDecl =>
                    FormatInfixDeclaration(infixDecl, decl.Range, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for declaration type '{decl.Value.GetType().Name}' is not implemented.")
            };
        }

        private (Node<Declaration>, FormattingContext) FormatFunctionDeclaration(
            Declaration.FunctionDeclaration funcDecl,
            FormattingContext context)
        {
            var currentContext = context;

            // Format signature if present
            Node<Signature>? formattedSignature = null;
            if (funcDecl.Function.Signature is { } signature)
            {
                // Check if signature is multi-line
                var isMultiLine = IsSignatureMultiLine(signature.Value);

                if (isMultiLine)
                {
                    // For multi-line signatures, preserve the input format
                    // Just create a new node with updated range
                    var sigStartRow = currentContext.CurrentRow;
                    var sigEndRow = signature.Range.End.Row - signature.Range.Start.Row + sigStartRow;
                    var sigEnd = new Location(sigEndRow, signature.Range.End.Column);
                    var sigRange = new Range(currentContext.ToLocation(), sigEnd);
                    formattedSignature = new Node<Signature>(sigRange, signature.Value);
                    currentContext = currentContext with { CurrentRow = sigEndRow + 1, CurrentColumn = 1 };
                }
                else
                {
                    // Single-line signature
                    var sigName = signature.Value.Name.Value;
                    var afterSigName = currentContext.Advance(sigName.Length);
                    // " : "
                    var afterColon = afterSigName.Advance(3);

                    var typeText = FormatTypeAnnotationText(signature.Value.TypeAnnotation.Value);
                    var afterType = afterColon.Advance(typeText.Length);

                    // Create new TypeAnnotation node with updated range
                    var typeRange = new Range(afterColon.ToLocation(), afterType.ToLocation());
                    var formattedTypeAnnotation = new Node<TypeAnnotation>(
                        typeRange,
                        signature.Value.TypeAnnotation.Value);

                    // Create new Signature with updated TypeAnnotation range
                    var formattedSig = new Signature(
                        Name: signature.Value.Name,
                        TypeAnnotation: formattedTypeAnnotation);

                    var sigRange = new Range(currentContext.ToLocation(), afterType.ToLocation());
                    formattedSignature = new Node<Signature>(sigRange, formattedSig);

                    // Move to next line for the implementation
                    currentContext = afterType.NextRow();
                }
            }

            var impl = funcDecl.Function.Declaration.Value;

            // Function name
            var afterName = currentContext.Advance(impl.Name.Value.Length);

            // Arguments
            var afterArgs = impl.Arguments.Aggregate(afterName, (ctx, arg) =>
            {
                var patternText = FormatPatternText(arg.Value);
                return ctx.Advance(1 + patternText.Length); // space + pattern
            });

            // " ="
            var afterEquals = afterArgs.Advance(2);

            // Move to next line and indent for the expression
            var exprContext = afterEquals.NextRow().Indent().SetIndentColumn();

            // Format the expression
            var (formattedExpr, contextAfterExpr) = VisitExpressionNode(impl.Expression, exprContext);

            // Build formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: new Node<string>(impl.Name.Range, impl.Name.Value),
                Arguments: impl.Arguments,
                Expression: formattedExpr
            );

            var formattedFunc = new FunctionStruct(
                Documentation: funcDecl.Function.Documentation,
                Signature: formattedSignature,
                Declaration: new Node<FunctionImplementation>(
                    new Range(currentContext.ToLocation(), contextAfterExpr.ToLocation()),
                    formattedImpl)
            );

            var range = new Range(context.ToLocation(), contextAfterExpr.ToLocation());
            return (new Node<Declaration>(range, new Declaration.FunctionDeclaration(formattedFunc)), contextAfterExpr);
        }

        private (Node<Declaration>, FormattingContext) FormatAliasDeclaration(
            Declaration.AliasDeclaration aliasDecl,
            FormattingContext context)
        {
            // "type alias "
            var afterTypeAlias = context.Advance(11);

            // Name
            var aliasName = aliasDecl.TypeAlias.Name.Value;
            var afterName = afterTypeAlias.Advance(aliasName.Length);

            // Generics (if any)
            var currentContext = afterName;
            foreach (var generic in aliasDecl.TypeAlias.Generics)
            {
                currentContext = currentContext.Advance(1); // space
                currentContext = currentContext.Advance(generic.Value.Length);
            }

            // " ="
            var afterEquals = currentContext.Advance(2);

            // Move to next line and indent for the type annotation
            var typeContext = afterEquals.NextRow().Indent().SetIndentColumn();

            // Format the type annotation (for now, keep it simple for single-line records)
            var (formattedTypeAnnotation, afterType) = FormatTypeAnnotation(aliasDecl.TypeAlias.TypeAnnotation, typeContext);

            var formattedTypeAlias = new TypeAlias(
                Documentation: aliasDecl.TypeAlias.Documentation,
                Name: aliasDecl.TypeAlias.Name,
                Generics: aliasDecl.TypeAlias.Generics,
                TypeAnnotation: formattedTypeAnnotation
            );

            var finalContext = afterType.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Declaration>(range, new Declaration.AliasDeclaration(formattedTypeAlias)), finalContext);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatTypeAnnotation(Node<TypeAnnotation> typeAnnot, FormattingContext context)
        {
            return typeAnnot.Value switch
            {
                TypeAnnotation.Record recordDef =>
                FormatRecordTypeAnnotation(recordDef, typeAnnot.Range, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for type annotation '{typeAnnot.Value.GetType().Name}' is not implemented.")
            };
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotation(TypeAnnotation.Record recordType, Range originalRange, FormattingContext context)
        {
            // Check if the record should be formatted as multiline
            var isMultiline = originalRange.Start.Row < originalRange.End.Row;

            if (isMultiline)
            {
                return FormatRecordTypeAnnotationMultiline(recordType, context);
            }
            else
            {
                return FormatRecordTypeAnnotationSingleLine(recordType, context);
            }
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotationSingleLine(TypeAnnotation.Record recordType, FormattingContext context)
        {
            // "{ "
            var afterOpenBrace = context.Advance(2);

            var currentContext = afterOpenBrace;
            var formattedFields = new List<Node<RecordField>>();

            for (var i = 0; i < recordType.RecordDefinition.Fields.Count; i++)
            {
                var field = recordType.RecordDefinition.Fields[i];
                var fieldStartContext = currentContext;

                // field name
                var fieldName = field.Value.FieldName.Value;
                currentContext = currentContext.Advance(fieldName.Length);

                // " : "
                currentContext = currentContext.Advance(3);

                // field type - create a new node with normalized text
                var fieldTypeText = FormatTypeAnnotationText(field.Value.FieldType.Value);
                var fieldTypeContext = currentContext;
                currentContext = currentContext.Advance(fieldTypeText.Length);

                // Create new field node with proper range
                var fieldNameRange = new Range(fieldStartContext.ToLocation(), fieldStartContext.Advance(fieldName.Length).ToLocation());
                var fieldTypeRange = new Range(fieldTypeContext.ToLocation(), currentContext.ToLocation());

                var newField = new RecordField(
                    FieldName: new Node<string>(fieldNameRange, fieldName),
                    FieldType: new Node<TypeAnnotation>(fieldTypeRange, field.Value.FieldType.Value)
                );

                var fieldRange = new Range(fieldStartContext.ToLocation(), currentContext.ToLocation());
                formattedFields.Add(new Node<RecordField>(fieldRange, newField));

                // comma and space between fields (except after last)
                if (i < recordType.RecordDefinition.Fields.Count - 1)
                {
                    currentContext = currentContext.Advance(2); // ", "
                }
            }

            // " }"
            var afterCloseBrace = currentContext.Advance(2);

            var formattedRecordDef = new RecordDefinition(formattedFields);
            var range = new Range(context.ToLocation(), afterCloseBrace.ToLocation());
            return (new Node<TypeAnnotation>(range, new TypeAnnotation.Record(formattedRecordDef)), afterCloseBrace);
        }

        private (Node<TypeAnnotation>, FormattingContext) FormatRecordTypeAnnotationMultiline(TypeAnnotation.Record recordType, FormattingContext context)
        {
            // "{ "
            var afterOpenBrace = context.Advance(2);

            var formattedFields = new List<Node<RecordField>>();

            // First field on same line as opening brace
            var field0 = recordType.RecordDefinition.Fields[0];
            var fieldName0 = field0.Value.FieldName.Value;
            var afterName0 = afterOpenBrace.Advance(fieldName0.Length);
            var afterColon0 = afterName0.Advance(3); // " : "
            var fieldTypeText0 = FormatTypeAnnotationText(field0.Value.FieldType.Value);
            var afterType0 = afterColon0.Advance(fieldTypeText0.Length);

            var field0NameRange = new Range(afterOpenBrace.ToLocation(), afterName0.ToLocation());
            var field0TypeRange = new Range(afterColon0.ToLocation(), afterType0.ToLocation());
            var newField0 = new RecordField(
                FieldName: new Node<string>(field0NameRange, fieldName0),
                FieldType: new Node<TypeAnnotation>(field0TypeRange, field0.Value.FieldType.Value)
            );
            var field0Range = new Range(afterOpenBrace.ToLocation(), afterType0.ToLocation());
            formattedFields.Add(new Node<RecordField>(field0Range, newField0));

            // Subsequent fields on new lines with ", " prefix
            var fieldContext = afterType0.NextRow().SetIndentColumn();

            for (var i = 1; i < recordType.RecordDefinition.Fields.Count; i++)
            {
                var field = recordType.RecordDefinition.Fields[i];

                // ", "
                var afterComma = fieldContext.Advance(2);

                // field name
                var fieldName = field.Value.FieldName.Value;
                var afterName = afterComma.Advance(fieldName.Length);

                // " : "
                var afterColon = afterName.Advance(3);

                // field type
                var fieldTypeText = FormatTypeAnnotationText(field.Value.FieldType.Value);
                var afterType = afterColon.Advance(fieldTypeText.Length);

                // Create new field node
                var fieldNameRange = new Range(afterComma.ToLocation(), afterName.ToLocation());
                var fieldTypeRange = new Range(afterColon.ToLocation(), afterType.ToLocation());
                var newField = new RecordField(
                    FieldName: new Node<string>(fieldNameRange, fieldName),
                    FieldType: new Node<TypeAnnotation>(fieldTypeRange, field.Value.FieldType.Value)
                );
                var fieldRange = new Range(fieldContext.ToLocation(), afterType.ToLocation());
                formattedFields.Add(new Node<RecordField>(fieldRange, newField));

                // Move to next line
                fieldContext = afterType.NextRow().SetIndentColumn();
            }

            // "}"
            var afterCloseBrace = fieldContext.Advance(1);

            var formattedRecordDef = new RecordDefinition(formattedFields);
            var range = new Range(context.ToLocation(), afterCloseBrace.ToLocation());
            return (new Node<TypeAnnotation>(range, new TypeAnnotation.Record(formattedRecordDef)), afterCloseBrace);
        }

        private (Node<Declaration>, FormattingContext) FormatCustomTypeDeclaration(
            Declaration.CustomTypeDeclaration customTypeDecl,
            FormattingContext context)
        {
            var typeDecl = customTypeDecl.TypeDeclaration;
            var currentContext = context;

            // "type "
            currentContext = currentContext.Advance(5);

            // type name
            var typeName = typeDecl.Name.Value;
            currentContext = currentContext.Advance(typeName.Length);

            // generics
            foreach (var generic in typeDecl.Generics)
            {
                currentContext = currentContext.Advance(1); // space
                currentContext = currentContext.Advance(generic.Value.Length);
            }

            // Move to next row and indent for "="
            currentContext = currentContext.NextRow().Indent().SetIndentColumn();
            currentContext = currentContext.Advance(2); // "= "

            // Format constructors
            var formattedConstructors = new List<Node<ValueConstructor>>();

            for (var i = 0; i < typeDecl.Constructors.Count; i++)
            {
                var constructor = typeDecl.Constructors[i];
                var constructorStartContext = currentContext;

                if (i > 0)
                {
                    var prevConstructor = typeDecl.Constructors[i - 1];

                    // Check for comments between previous and current constructor
                    var commentsBetween =
                        _comments
                        .Where(c =>
                        c.Range.Start.Row > prevConstructor.Range.End.Row &&
                        c.Range.Start.Row < constructor.Range.Start.Row)
                        .ToList();

                    // Move to new line and add "| " prefix for subsequent constructors
                    currentContext = currentContext.NextRow();

                    // Account for comments between constructors
                    foreach (var comment in commentsBetween)
                    {
                        // Count newlines in the comment to determine how many rows it occupies
                        var commentRowCount = comment.Value.Count(ch => ch is '\n') + 1;
                        for (var j = 0; j < commentRowCount; j++)
                        {
                            currentContext = currentContext.NextRow();
                        }
                    }

                    currentContext = currentContext.SetIndentColumn();
                    currentContext = currentContext.Advance(2);
                }

                // constructor name
                var constructorName = constructor.Value.Name.Value;
                var nameStart = currentContext;
                currentContext = currentContext.Advance(constructorName.Length);
                var constructorNameRange = new Range(nameStart.ToLocation(), currentContext.ToLocation());

                // constructor arguments
                var formattedArgs = new List<Node<TypeAnnotation>>();
                foreach (var arg in constructor.Value.Arguments)
                {
                    currentContext = currentContext.Advance(1); // space before argument
                    var argStart = currentContext;
                    var argText = FormatTypeAnnotationText(arg.Value);
                    currentContext = currentContext.Advance(argText.Length);
                    var argRange = new Range(argStart.ToLocation(), currentContext.ToLocation());
                    formattedArgs.Add(new Node<TypeAnnotation>(argRange, arg.Value));
                }

                // Create formatted constructor
                var formattedConstructor = new ValueConstructor(
                    Name: new Node<string>(constructorNameRange, constructorName),
                    Arguments: formattedArgs
                );

                var constructorRange = new Range(constructorStartContext.ToLocation(), currentContext.ToLocation());
                formattedConstructors.Add(new Node<ValueConstructor>(constructorRange, formattedConstructor));
            }

            // Create formatted type declaration
            var typeNameRange = new Range(context.Advance(5).ToLocation(), context.Advance(5 + typeName.Length).ToLocation());
            var formattedTypeDecl = new TypeStruct(
                Documentation: typeDecl.Documentation,
                Name: new Node<string>(typeNameRange, typeName),
                Generics: typeDecl.Generics,
                Constructors: formattedConstructors
            );

            var declRange = new Range(context.ToLocation(), currentContext.ToLocation());
            var formattedDecl = new Declaration.CustomTypeDeclaration(formattedTypeDecl);
            return (new Node<Declaration>(declRange, formattedDecl), currentContext);
        }

        private (Node<Declaration>, FormattingContext) FormatInfixDeclaration(
            Declaration.InfixDeclaration infixDecl,
            Range originalRange,
            FormattingContext context)
        {
            // Infix declarations are formatted on a single line as:
            // "infix <direction> <precedence> (<operator>) = <functionName>"
            // Example: "infix right 0 (<|) = apL"

            var infix = infixDecl.Infix;

            // Calculate the full text length to advance context
            var direction = infix.Direction.Value switch
            {
                InfixDirection.Left => "left ",
                InfixDirection.Right => "right",
                InfixDirection.Non => "non  ",
                _ => throw new System.NotImplementedException($"Unknown infix direction: {infix.Direction.Value}")
            };

            var textLength = 6 + direction.Length + 1 + infix.Precedence.Value.ToString().Length + 1 +
                            1 + infix.Operator.Value.Length + 1 + 1 + 1 + 1 + infix.FunctionName.Value.Length;
            // "infix " + direction + " " + precedence + " " + "(" + operator + ")" + " " + "=" + " " + functionName

            var currentContext = context.Advance(textLength);

            var declRange = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<Declaration>(declRange, infixDecl), currentContext);
        }

        /// <summary>
        /// Check if a function signature should be formatted as multiline.
        /// Uses the TypeAnnotation's range to detect if it spans multiple rows.
        /// </summary>
        private static bool IsSignatureMultiLine(Signature signature)
        {
            return signature.TypeAnnotation.Range.Start.Row != signature.TypeAnnotation.Range.End.Row;
        }

        private string FormatTypeAnnotationText(TypeAnnotation typeAnnotation)
        {
            // Simplified type annotation rendering for spacing calculation
            return typeAnnotation switch
            {
                TypeAnnotation.GenericType generic =>
                generic.Name,

                TypeAnnotation.Unit => "()",

                TypeAnnotation.Typed typed =>
                    FormatTypedAnnotationText(typed),

                TypeAnnotation.FunctionTypeAnnotation funcType =>
                    FormatTypeAnnotationText(funcType.ArgumentType.Value) + " -> " +
                    FormatTypeAnnotationText(funcType.ReturnType.Value),

                TypeAnnotation.Tupled tupled =>
                    "( " + string.Join(", ", tupled.TypeAnnotations.Select(t => FormatTypeAnnotationText(t.Value))) + " )",

                TypeAnnotation.GenericRecord genericRecord =>
                    FormatGenericRecordTypeAnnotationText(genericRecord),

                TypeAnnotation.Record record =>
                    FormatRecordTypeAnnotationFieldsText(record.RecordDefinition),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for type annotation '{typeAnnotation.GetType().Name}' is not implemented.")
            };
        }

        private string FormatGenericRecordTypeAnnotationText(TypeAnnotation.GenericRecord genericRecord)
        {
            var fields = string.Join(", ", genericRecord.RecordDefinition.Value.Fields.Select(f =>
                f.Value.FieldName.Value + " : " + FormatTypeAnnotationText(f.Value.FieldType.Value)));
            return "{ " + genericRecord.GenericName.Value + " | " + fields + " }";
        }

        private string FormatRecordTypeAnnotationFieldsText(RecordDefinition recordDef)
        {
            var fields = string.Join(", ", recordDef.Fields.Select(f =>
                f.Value.FieldName.Value + " : " + FormatTypeAnnotationText(f.Value.FieldType.Value)));
            return "{ " + fields + " }";
        }

        private string FormatTypedAnnotationText(TypeAnnotation.Typed typed)
        {
            var moduleName = typed.TypeName.Value.ModuleName.Count > 0
                ? string.Join(".", typed.TypeName.Value.ModuleName) + "."
                : "";
            var typeName = moduleName + typed.TypeName.Value.Name;

            if (typed.TypeArguments.Count is 0)
                return typeName;

            return typeName + " " + string.Join(" ", typed.TypeArguments.Select(a =>
            {
                // Parenthesize complex type arguments
                var argText = FormatTypeAnnotationText(a.Value);
                return a.Value is TypeAnnotation.FunctionTypeAnnotation ||
                       (a.Value is TypeAnnotation.Typed t && t.TypeArguments.Count > 0)
                    ? "(" + argText + ")"
                    : argText;
            }));
        }

        private static string FormatPatternText(Pattern pattern)
        {
            return pattern switch
            {
                Pattern.VarPattern varPattern =>
                varPattern.Name,

                Pattern.AllPattern =>
                "_",

                Pattern.UnitPattern =>
                "()",

                Pattern.CharPattern charPattern =>
                $"'{(char)charPattern.Value}'",

                Pattern.StringPattern stringPattern =>
                $"\"{stringPattern.Value}\"",

                Pattern.IntPattern intPattern =>
                intPattern.Value.ToString(),

                Pattern.HexPattern hexPattern =>
                FormatHexPattern(hexPattern.Value),

                Pattern.FloatPattern floatPattern =>
                floatPattern.Value.ToString(),

                Pattern.TuplePattern tuplePattern =>
                FormatTuplePattern(tuplePattern),

                Pattern.ListPattern listPattern =>
                FormatListPattern(listPattern),

                Pattern.RecordPattern recordPattern =>
                FormatRecordPattern(recordPattern),

                Pattern.UnConsPattern unConsPattern =>
                FormatUnConsPattern(unConsPattern),

                Pattern.AsPattern asPattern =>
                FormatAsPattern(asPattern),

                Pattern.ParenthesizedPattern parenPattern =>
                $"({FormatPatternText(parenPattern.Pattern.Value)})",

                Pattern.NamedPattern namedPattern =>
                FormatNamedPattern(namedPattern),

                _ =>
                /*
                 * https://github.com/stil4m/elm-syntax/commit/6a78fc2f04b79d72141ae8bc6068a3cb042219c6
                 * Do not support parsing FloatPattern: https://github.com/stil4m/elm-syntax/pull/200
                 * */
                throw new System.NotImplementedException(
                    $"Formatting for pattern type '{pattern.GetType().Name}' is not implemented.")
            };
        }

        private static string FormatNamedPattern(Pattern.NamedPattern namedPattern)
        {
            // Get the constructor name (could be qualified like "Maybe.Just" or simple like "Just")
            var nameParts = new List<string>();
            if (namedPattern.Name.ModuleName is not null && namedPattern.Name.ModuleName.Count > 0)
            {
                nameParts.AddRange(namedPattern.Name.ModuleName);
            }
            nameParts.Add(namedPattern.Name.Name);
            var constructorName = string.Join(".", nameParts);

            // Format arguments if any
            if (namedPattern.Arguments.Count is 0)
            {
                return constructorName;
            }

            var argTexts = namedPattern.Arguments.Select(arg => FormatPatternText(arg.Value));
            return constructorName + " " + string.Join(" ", argTexts);
        }

        private static string FormatTuplePattern(Pattern.TuplePattern tuplePattern)
        {
            var patterns = tuplePattern.Elements.Select(p => FormatPatternText(p.Value));
            return $"( {string.Join(", ", patterns)} )";
        }

        private static string FormatListPattern(Pattern.ListPattern listPattern)
        {
            if (listPattern.Elements.Count is 0)
            {
                return "[]";
            }

            var patterns = listPattern.Elements.Select(p => FormatPatternText(p.Value));
            return $"[ {string.Join(", ", patterns)} ]";
        }

        private static string FormatRecordPattern(Pattern.RecordPattern recordPattern)
        {
            if (recordPattern.Fields.Count is 0)
            {
                return "{}";
            }

            var fields = recordPattern.Fields.Select(f => f.Value);
            return $"{{ {string.Join(", ", fields)} }}";
        }

        private static string FormatUnConsPattern(Pattern.UnConsPattern unConsPattern)
        {
            var head = FormatPatternText(unConsPattern.Head.Value);
            var tail = FormatPatternText(unConsPattern.Tail.Value);
            return $"{head} :: {tail}";
        }

        private static string FormatAsPattern(Pattern.AsPattern asPattern)
        {
            var pattern = FormatPatternText(asPattern.Pattern.Value);
            return $"{pattern} as {asPattern.Name.Value}";
        }

        /// <summary>
        /// Formats a hexadecimal pattern with padding to match elm-format behavior.
        /// Pads to lengths of 2, 4, or multiples of 4 (e.g., 8, 12, 16...).
        /// </summary>
        private static string FormatHexPattern(long value)
        {
            // Convert to hex string (uppercase)
            var hex = value.ToString("X");

            // Determine target length: 2, 4, 8, 12, 16, etc.
            int targetLength;
            if (hex.Length <= 2)
                targetLength = 2;
            else if (hex.Length <= 4)
                targetLength = 4;
            else
                // Round up to next multiple of 4
                targetLength = ((hex.Length + 3) / 4) * 4;

            // Pad with leading zeros
            var paddedHex = hex.PadLeft(targetLength, '0');

            return $"0x{paddedHex}";
        }

        // Expression visitor methods

        public override (Node<Expression>, FormattingContext) VisitInteger(Expression.Integer expr, FormattingContext context)
        {
            var text = expr.Value.ToString();
            var afterExpr = context.Advance(text.Length);
            var range = new Range(context.ToLocation(), afterExpr.ToLocation());
            return (new Node<Expression>(range, expr), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitLiteral(Expression.Literal expr, FormattingContext context)
        {
            var text = $"\"{expr.Value}\"";
            var afterExpr = context.Advance(text.Length);
            var range = new Range(context.ToLocation(), afterExpr.ToLocation());
            return (new Node<Expression>(range, expr), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitFunctionOrValue(Expression.FunctionOrValue expr, FormattingContext context)
        {
            var currentContext = context;

            // Format module name if present (e.g., "OtherModule" in "OtherModule.otherDecl")
            if (expr.ModuleName.Count > 0)
            {
                foreach (var modulePart in expr.ModuleName)
                {
                    currentContext = currentContext.Advance(modulePart.Length + 1); // module name + dot
                }
            }

            // Format function/value name
            currentContext = currentContext.Advance(expr.Name.Length);

            var range = new Range(context.ToLocation(), currentContext.ToLocation());
            return (new Node<Expression>(range, expr), currentContext);
        }

        public override (Node<Expression>, FormattingContext) VisitApplication(Expression.Application expr, FormattingContext context)
        {
            return FormatApplication(expr, context, IsApplicationMultiLine(expr));
        }

        /// <summary>
        /// Check if a function application should be formatted as multiline.
        /// </summary>
        private static bool IsApplicationMultiLine(Expression.Application expr)
        {
            return NodesSpanMultipleRows(expr.Arguments);
        }

        private (Node<Expression>, FormattingContext) FormatApplication(
            Expression.Application expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: format all args with spaces between them
                var currentContext = context;
                var formattedArgs = new List<Node<Expression>>();

                for (var i = 0; i < expr.Arguments.Count; i++)
                {
                    var arg = expr.Arguments[i];
                    var (formattedArg, nextContext) = Visit(arg.Value, currentContext);
                    formattedArgs.Add(formattedArg);

                    // Add space after this arg only if there are more args to come
                    currentContext = i < expr.Arguments.Count - 1
                        ? nextContext.Advance(1)
                        : nextContext;
                }

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(range, new Expression.Application(formattedArgs)), currentContext);
            }
            else
            {
                // Multi-line: first arg on current line, rest indented on new lines
                var formattedArgs = new List<Node<Expression>>();

                // First argument (function) on current line
                var (firstArg, afterFirst) = Visit(expr.Arguments[0].Value, context);
                formattedArgs.Add(firstArg);

                // Subsequent arguments each on their own line, indented
                var argContext = afterFirst.NextRow().Indent().SetIndentColumn();

                for (var i = 1; i < expr.Arguments.Count; i++)
                {
                    var (formattedArg, nextContext) = Visit(expr.Arguments[i].Value, argContext);
                    formattedArgs.Add(formattedArg);

                    // Only move to next row if there are more arguments
                    argContext = i < expr.Arguments.Count - 1
                        ? nextContext.NextRow().SetIndentColumn()
                        : nextContext;
                }

                // Context after all arguments (last line)
                var finalContext = argContext.Dedent();

                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                return (new Node<Expression>(range, new Expression.Application(formattedArgs)), finalContext);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitOperatorApplication(Expression.OperatorApplication expr, FormattingContext context)
        {
            // Check if this operator application should be multiline
            // An operator application is multiline if the operands are on different rows
            var isMultiline = expr.Left.Range.End.Row < expr.Right.Range.Start.Row;

            if (isMultiline)
            {
                // Multiline: left on one line, operator and right on next line
                var (leftFormatted, afterLeft) = VisitExpressionNode(expr.Left, context);

                // Move to next line for operator
                var opLineContext = afterLeft.NextRow().Indent().SetIndentColumn();
                var afterOp = opLineContext.Advance(expr.Operator.Length);
                var afterOpSpace = afterOp.Advance(1); // space after operator

                var (rightFormatted, afterRight) = VisitExpressionNode(expr.Right, afterOpSpace);

                var finalContext = afterRight.Dedent();

                var formattedExpr = new Expression.OperatorApplication(
                    Operator: expr.Operator,
                    Direction: expr.Direction,
                    Left: leftFormatted,
                    Right: rightFormatted
                );

                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                return (new Node<Expression>(range, formattedExpr), finalContext);
            }
            else
            {
                // Single-line: left op right with spaces
                var (leftFormatted, afterLeft) = VisitExpressionNode(expr.Left, context);
                var afterLeftSpace = afterLeft.Advance(1); // space before operator
                var afterOp = afterLeftSpace.Advance(expr.Operator.Length);
                var afterOpSpace = afterOp.Advance(1); // space after operator
                var (rightFormatted, afterRight) = VisitExpressionNode(expr.Right, afterOpSpace);

                var formattedExpr = new Expression.OperatorApplication(
                    Operator: expr.Operator,
                    Direction: expr.Direction,
                    Left: leftFormatted,
                    Right: rightFormatted
                );

                var range = new Range(context.ToLocation(), afterRight.ToLocation());
                return (new Node<Expression>(range, formattedExpr), afterRight);
            }
        }

        /// <summary>
        /// Helper method to visit an expression Node with access to its original Range.
        /// This is crucial for detecting multiline status at each nesting level independently,
        /// particularly for single-element lists where the closing bracket may be on a new line.
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitExpressionNode(
            Node<Expression> node,
            FormattingContext context)
        {
            // For ListExpr, use the Node's Range to detect multiline at this level
            if (node.Value is Expression.ListExpr listExpr)
            {
                return VisitListExprWithRange(listExpr, node.Range, context);
            }

            // For RecordExpr, use the Node's Range to detect multiline at this level
            if (node.Value is Expression.RecordExpr recordExpr)
            {
                return VisitRecordExprWithRange(recordExpr, node.Range, context);
            }

            // For other expressions, use the regular Visit
            return Visit(node.Value, context);
        }

        /// <summary>
        /// Format a list expression with access to its original Range for accurate multiline detection.
        /// The original Range is essential for single-element lists to detect if the closing bracket
        /// is on a different row than the element (indicating multiline format).
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitListExprWithRange(
            Expression.ListExpr expr,
            Range originalRange,
            FormattingContext context)
        {
            if (expr.Elements.Count is 0)
            {
                // Empty list: "[]"
                var afterList = context.Advance(2);
                var emptyListRange = new Range(context.ToLocation(), afterList.ToLocation());
                return (new Node<Expression>(emptyListRange, expr), afterList);
            }

            // Detect if this specific list is multiline by checking:
            // 1. For multiple elements: if they span different rows
            // 2. For single element: if the parent list Range (including closing bracket) spans multiple rows
            //    OR if the element itself spans multiple rows (indicating multiline formatting)
            bool isMultiLine;
            if (expr.Elements.Count >= 2)
            {
                // Multiple elements: use helper to check if they span different rows
                isMultiLine = NodesSpanMultipleRows(expr.Elements);
            }
            else
            {
                // Single element: check if the parent list Range spans multiple rows
                // OR if the element itself spans multiple rows
                // This detects cases like: [ [ 1, 2, 3 ]\n] where inner list is single-line
                // but outer closing bracket is on a new line, or [ { a = 1\n    }\n] where the
                // element Range has been marked as multiline by a preprocessor
                var element = expr.Elements[0];
                var elementEndRow = element.Range.End.Row;
                var elementStartRow = element.Range.Start.Row;
                var listEndRow = originalRange.End.Row;
                isMultiLine = listEndRow > elementEndRow || elementEndRow > elementStartRow;
            }

            return FormatList(expr, context, isMultiLine);
        }

        public override (Node<Expression>, FormattingContext) VisitListExpr(Expression.ListExpr expr, FormattingContext context)
        {
            // This method is called by the base visitor pattern when we don't have direct access
            // to the original Node's Range (only the Expression value). This occurs when called
            // from external code or the base Visit() dispatcher.
            // We synthesize a Range based on element positions for multiline detection.

            if (expr.Elements.Count is 0)
            {
                // For empty lists, use a minimal placeholder Range (doesn't affect multiline detection)
                var emptyRange = new Range(new Location(1, 1), new Location(1, 3));  // "[]" is 2 chars
                return VisitListExprWithRange(expr, emptyRange, context);
            }

            // Synthesize a Range that encompasses all elements
            // This provides fallback multiline detection when the original Range isn't available
            var startLoc = expr.Elements[0].Range.Start;
            var endLoc = expr.Elements[expr.Elements.Count - 1].Range.End;
            var synthesizedRange = new Range(startLoc, endLoc);

            return VisitListExprWithRange(expr, synthesizedRange, context);
        }

        private (Node<Expression>, FormattingContext) FormatList(
            Expression.ListExpr expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "[ elem1, elem2, elem3 ]"
                var afterOpen = context.Advance(2); // "[ "
                var currentContext = afterOpen;
                var formattedElements = new List<Node<Expression>>();

                for (var i = 0; i < expr.Elements.Count; i++)
                {
                    var (formattedElem, nextContext) = VisitExpressionNode(expr.Elements[i], currentContext);
                    formattedElements.Add(formattedElem);
                    currentContext = i < expr.Elements.Count - 1
                        ? nextContext.Advance(2) // ", "
                        : nextContext.Advance(2); // " ]"
                }

                var listRange = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(listRange, new Expression.ListExpr(formattedElements)), currentContext);
            }
            else
            {
                // Multi-line: first element on same line as "[", rest on new lines with commas
                var openingBracketColumn = context.CurrentColumn;
                var afterOpen = context.Advance(2); // "[ "
                var formattedElements = new List<Node<Expression>>();

                // First element on same line as "["
                var (firstElem, afterFirst) = VisitExpressionNode(expr.Elements[0], afterOpen);
                formattedElements.Add(firstElem);

                // Subsequent elements on new lines with ", " prefix
                var nextRow = afterFirst.CurrentRow + 1;

                for (var i = 1; i < expr.Elements.Count; i++)
                {
                    // Create context at opening bracket column for comma
                    // When creating new contexts for children, convert parent's AddHalfIndent to a full IndentLevel
                    var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    var commaContext = new FormattingContext(
                        nextRow,
                        openingBracketColumn,
                        baseIndentLevel);

                    // ", " at the start of the line (at opening bracket column)
                    var afterComma = commaContext.Advance(2);

                    // Add half indent for subsequent list elements to support nested multiline expressions
                    var elemContext = afterComma.WithHalfIndent(true);

                    var (formattedElem, nextContext) = VisitExpressionNode(expr.Elements[i], elemContext);
                    formattedElements.Add(formattedElem);

                    // Move to next line
                    nextRow = nextContext.CurrentRow + 1;
                }

                // Closing bracket "]" should be at opening bracket column
                var closingBracketContext = new FormattingContext(
                    nextRow,
                    openingBracketColumn,
                    context.IndentLevel);
                var afterClose = closingBracketContext.Advance(1);

                var listRange = new Range(context.ToLocation(), afterClose.ToLocation());
                return (new Node<Expression>(listRange, new Expression.ListExpr(formattedElements)), afterClose);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitIfBlock(Expression.IfBlock expr, FormattingContext context)
        {
            // "if"
            var afterIf = context.Advance(2);
            // " " after if
            afterIf = afterIf.Advance(1);
            var (condFormatted, afterCond) = Visit(expr.Condition.Value, afterIf);

            // " then"
            var afterThen = afterCond.Advance(5);
            var thenContext = afterThen.NextRow().Indent().SetIndentColumn();
            var (thenFormatted, afterThenExpr) = Visit(expr.ThenBlock.Value, thenContext);

            // Check if else block is an if-expression (for "else if" formatting)
            var isElseIf = expr.ElseBlock.Value is Expression.IfBlock;

            // Blank line before else
            var elseLineContext = afterThenExpr.Dedent().NextRow().NextRow().SetIndentColumn();

            // "else"
            var afterElseKeyword = elseLineContext.Advance(4);

            FormattingContext elseContext;
            if (isElseIf)
            {
                // "else if" - keep on same line with space
                elseContext = afterElseKeyword.Advance(1);
            }
            else
            {
                // Regular else - move to next line and indent
                elseContext = afterElseKeyword.NextRow().Indent().SetIndentColumn();
            }

            var (elseFormatted, afterElseExpr) = Visit(expr.ElseBlock.Value, elseContext);

            var formattedIf = new Expression.IfBlock(
                Condition: condFormatted,
                ThenBlock: thenFormatted,
                ElseBlock: elseFormatted
            );

            var finalContext = isElseIf ? afterElseExpr : afterElseExpr.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Expression>(range, formattedIf), finalContext);
        }

        public override (Node<Expression>, FormattingContext) VisitCaseExpression(Expression.CaseExpression expr, FormattingContext context)
        {
            // "case "
            var afterCase = context.Advance(5);
            var (scrutinee, afterScrutinee) = Visit(expr.CaseBlock.Expression.Value, afterCase);

            // " of"
            var afterOf = afterScrutinee.Advance(3);

            // Format cases
            var caseContext = afterOf.NextRow().Indent().SetIndentColumn();
            var formattedCases = new List<Case>();

            for (var i = 0; i < expr.CaseBlock.Cases.Count; i++)
            {
                var caseItem = expr.CaseBlock.Cases[i];

                // Add blank line before cases after the first
                if (i > 0)
                {
                    // The blank line comes before the pattern
                    // Move to new row and set column to case indent level
                    caseContext = caseContext.NextRow().SetIndentColumn();
                }

                // Pattern - create new range for the formatted pattern
                var patternStartContext = caseContext;
                var patternText = FormatPatternText(caseItem.Pattern.Value);
                var afterPattern = caseContext.Advance(patternText.Length);

                // " ->"
                var afterArrow = afterPattern.Advance(3);
                var exprContext = afterArrow.NextRow().Indent().SetIndentColumn();

                var (formattedExpr, afterExpr) = Visit(caseItem.Expression.Value, exprContext);

                // Create pattern node with proper range
                var patternRange = new Range(patternStartContext.ToLocation(), afterPattern.ToLocation());
                var formattedPattern = new Node<Pattern>(patternRange, caseItem.Pattern.Value);

                formattedCases.Add(new Case(
                    Pattern: formattedPattern,
                    Expression: formattedExpr
                ));

                // Move to next line after expression and reset to case indent level
                // Only add NextRow if this is not the last case
                caseContext = afterExpr.Dedent();
                if (i < expr.CaseBlock.Cases.Count - 1)
                {
                    caseContext = caseContext.NextRow().SetIndentColumn();
                }
            }

            var formattedCaseBlock = new CaseBlock(
                Expression: scrutinee,
                Cases: formattedCases
            );

            var finalContext = caseContext.Dedent();
            var range = new Range(context.ToLocation(), finalContext.ToLocation());
            return (new Node<Expression>(range, new Expression.CaseExpression(formattedCaseBlock)), finalContext);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordUpdateExpression(Expression.RecordUpdateExpression expr, FormattingContext context)
        {
            return FormatRecordUpdate(expr, context, IsRecordUpdateMultiLine(expr));
        }

        /// <summary>
        /// Check if a record update expression should be formatted as multiline.
        /// A record update is multiline if any field is on a different row than the record name.
        /// </summary>
        private static bool IsRecordUpdateMultiLine(Expression.RecordUpdateExpression expr)
        {
            if (expr.Fields.Count is 0)
                return false;

            var recordNameRow = expr.RecordName.Range.Start.Row;
            return expr.Fields.Any(field => field.Value.fieldName.Range.Start.Row != recordNameRow);
        }

        private (Node<Expression>, FormattingContext) FormatRecordUpdate(
            Expression.RecordUpdateExpression expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "{ recordName | field1 = value1, field2 = value2 }"
                var afterOpenBrace = context.Advance(2); // "{ "

                // Record name
                var recordName = expr.RecordName.Value;
                var afterRecordName = afterOpenBrace.Advance(recordName.Length);

                // " | "
                var afterPipe = afterRecordName.Advance(3);

                var currentContext = afterPipe;
                var formattedFields = new List<Node<(Node<string>, Node<Expression>)>>();

                for (var i = 0; i < expr.Fields.Count; i++)
                {
                    var (fieldName, valueExpr) = expr.Fields[i].Value;

                    // Field name
                    var afterFieldName = currentContext.Advance(fieldName.Value.Length);

                    // " = "
                    var afterEquals = afterFieldName.Advance(3);

                    // Format value expression
                    var (formattedValue, afterValue) = Visit(valueExpr.Value, afterEquals);

                    // Create formatted field name node
                    var fieldNameRange = new Range(currentContext.ToLocation(), afterFieldName.ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, fieldName.Value);

                    // Create formatted field node
                    var fieldRange = new Range(currentContext.ToLocation(), afterValue.ToLocation());
                    formattedFields.Add(new Node<(Node<string>, Node<Expression>)>(fieldRange, (formattedFieldName, formattedValue)));

                    currentContext = i < expr.Fields.Count - 1
                        ? afterValue.Advance(2) // ", "
                        : afterValue.Advance(2); // " }"
                }

                // Create record name node
                var recordNameRange = new Range(afterOpenBrace.ToLocation(), afterRecordName.ToLocation());
                var formattedRecordName = new Node<string>(recordNameRange, recordName);

                var range = new Range(context.ToLocation(), currentContext.ToLocation());
                var formattedExpr = new Expression.RecordUpdateExpression(formattedRecordName, formattedFields);
                return (new Node<Expression>(range, formattedExpr), currentContext);
            }
            else
            {
                // Multi-line: record name on same line, pipe and fields indented on new lines
                var afterOpenBrace = context.Advance(2); // "{ "

                // Record name on same line as opening brace
                var recordName = expr.RecordName.Value;
                var afterRecordName = afterOpenBrace.Advance(recordName.Length);

                // Move to next line and indent for pipe
                var pipeLineContext = afterRecordName.NextRow().Indent().SetIndentColumn();

                // "| "
                var afterPipe = pipeLineContext.Advance(2);

                var formattedFields = new List<Node<(Node<string>, Node<Expression>)>>();

                // First field on same line as pipe
                var (firstFieldName, firstValueExpr) = expr.Fields[0].Value;
                var afterFirstFieldName = afterPipe.Advance(firstFieldName.Value.Length);
                var afterFirstEquals = afterFirstFieldName.Advance(3); // " = "
                var (firstFormattedValue, afterFirstValue) = Visit(firstValueExpr.Value, afterFirstEquals);

                var firstFieldNameRange = new Range(afterPipe.ToLocation(), afterFirstFieldName.ToLocation());
                var firstFormattedFieldName = new Node<string>(firstFieldNameRange, firstFieldName.Value);
                var firstFieldRange = new Range(afterPipe.ToLocation(), afterFirstValue.ToLocation());

                formattedFields.Add(
                    new Node<(Node<string>, Node<Expression>)>(firstFieldRange, (firstFormattedFieldName, firstFormattedValue)));

                // Subsequent fields on new lines with ", " prefix
                var fieldContext = afterFirstValue.NextRow().SetIndentColumn();

                for (var i = 1; i < expr.Fields.Count; i++)
                {
                    // ", "
                    var afterComma = fieldContext.Advance(2);

                    var (fieldName, valueExpr) = expr.Fields[i].Value;
                    var afterFieldName = afterComma.Advance(fieldName.Value.Length);
                    var afterEquals = afterFieldName.Advance(3); // " = "
                    var (formattedValue, afterValue) = Visit(valueExpr.Value, afterEquals);

                    var fieldNameRange = new Range(afterComma.ToLocation(), afterFieldName.ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, fieldName.Value);
                    var fieldRange = new Range(afterComma.ToLocation(), afterValue.ToLocation());
                    formattedFields.Add(new Node<(Node<string>, Node<Expression>)>(fieldRange, (formattedFieldName, formattedValue)));

                    // Move to next line
                    fieldContext = afterValue.NextRow().SetIndentColumn();
                }

                // Dedent before placing closing brace
                var closingBraceContext = fieldContext.Dedent().SetIndentColumn();

                // "}"
                var afterCloseBrace = closingBraceContext.Advance(1);

                var finalContext = afterCloseBrace.Dedent();

                // Create record name node
                var recordNameRange = new Range(afterOpenBrace.ToLocation(), afterRecordName.ToLocation());
                var formattedRecordName = new Node<string>(recordNameRange, recordName);

                var range = new Range(context.ToLocation(), finalContext.ToLocation());
                var formattedExpr = new Expression.RecordUpdateExpression(formattedRecordName, formattedFields);
                return (new Node<Expression>(range, formattedExpr), finalContext);
            }
        }

        /// <summary>
        /// Format a let expression with proper spacing between declarations.
        /// Ensures exactly one blank line between let declarations.
        /// </summary>
        public override (Node<Expression>, FormattingContext) VisitLetExpression(Expression.LetExpression expr, FormattingContext context)
        {
            // "let"
            var afterLet = context.Advance(3);
            var afterLetNewline = afterLet.NextRow();

            // Indent for let declarations
            var declContext = afterLetNewline.Indent().SetIndentColumn();

            var formattedDeclarations = new List<Node<Expression.LetDeclaration>>();
            var currentContext = declContext;

            for (var i = 0; i < expr.Value.Declarations.Count; i++)
            {
                var decl = expr.Value.Declarations[i];
                var (formattedDecl, afterDecl) = FormatLetDeclaration(decl.Value, currentContext);

                // Create node for the declaration
                var declRange = new Range(currentContext.ToLocation(), afterDecl.ToLocation());
                formattedDeclarations.Add(new Node<Expression.LetDeclaration>(declRange, formattedDecl));

                // Move to next line and add blank line between declarations
                if (i < expr.Value.Declarations.Count - 1)
                {
                    // One blank line between declarations (2 newlines total)
                    currentContext = afterDecl.NextRow().NextRow().SetIndentColumn();
                }
                else
                {
                    // After last declaration, just move to next line for "in"
                    currentContext = afterDecl.NextRow();
                }
            }

            // Dedent for "in"
            var inContext = currentContext.Dedent().SetIndentColumn();

            // "in"
            var afterIn = inContext.Advance(2);
            var afterInNewline = afterIn.NextRow();

            // Format the expression after "in"
            var exprContext = afterInNewline.SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNode(expr.Value.Expression, exprContext);

            // Create the formatted let block
            var formattedLetBlock = new Expression.LetBlock(formattedDeclarations, formattedExpr);

            var range = new Range(context.ToLocation(), afterExpr.ToLocation());
            return (new Node<Expression>(range, new Expression.LetExpression(formattedLetBlock)), afterExpr);
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetDeclaration(Expression.LetDeclaration decl, FormattingContext context)
        {
            return decl switch
            {
                Expression.LetDeclaration.LetFunction letFunc =>
                    FormatLetFunction(letFunc, context),

                Expression.LetDeclaration.LetDestructuring letDestr =>
                    FormatLetDestructuring(letDestr, context),

                _ =>
                throw new System.NotImplementedException(
                    $"Formatting for let declaration type '{decl.GetType().Name}' is not implemented.")
            };
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetFunction(Expression.LetDeclaration.LetFunction letFunc, FormattingContext context)
        {
            // Format like a regular function declaration but without the full signature handling
            var func = letFunc.Function;

            var currentContext = context;
            Node<Signature>? formattedSignature = null;

            // Format type signature if present
            if (func.Signature is { } signature)
            {
                // Function name for signature
                var sigName = signature.Value.Name.Value;
                var afterSigName = currentContext.Advance(sigName.Length);

                // " : "
                var afterColon = afterSigName.Advance(3);

                // Type annotation (always single-line in formatted output)
                var typeText = FormatTypeAnnotationText(signature.Value.TypeAnnotation.Value);
                var afterType = afterColon.Advance(typeText.Length);

                // Create new TypeAnnotation node with updated range
                var typeRange = new Range(afterColon.ToLocation(), afterType.ToLocation());
                var formattedTypeAnnotation = new Node<TypeAnnotation>(
                    typeRange,
                    signature.Value.TypeAnnotation.Value);

                // Create new Signature with updated TypeAnnotation range
                var formattedSig = new Signature(
                    Name: signature.Value.Name,
                    TypeAnnotation: formattedTypeAnnotation);

                var sigRange = new Range(context.ToLocation(), afterType.ToLocation());
                formattedSignature = new Node<Signature>(sigRange, formattedSig);

                // Move to next line for the function declaration
                currentContext = afterType.NextRow().SetIndentColumn();
            }

            // Remember where the declaration starts (for the Range)
            var declStartContext = currentContext;

            // Function name
            var funcName = func.Declaration.Value.Name.Value;
            var afterName = currentContext.Advance(funcName.Length);

            // Format arguments - estimate total length from original range
            var argContext = afterName;
            if (func.Declaration.Value.Arguments.Count > 0)
            {
                // Estimate argument section length from first to last argument's range
                var firstArg = func.Declaration.Value.Arguments[0];
                var lastArg = func.Declaration.Value.Arguments[func.Declaration.Value.Arguments.Count - 1];
                var argSectionLength = lastArg.Range.End.Column - firstArg.Range.Start.Column + 1;
                argContext = argContext.Advance(argSectionLength);
            }

            // " ="
            var afterEquals = argContext.Advance(2);
            var afterEqualsNewline = afterEquals.NextRow();

            // Format the expression (indented)
            var exprContext = afterEqualsNewline.Indent().SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNode(func.Declaration.Value.Expression, exprContext);

            // Create formatted implementation
            var formattedImpl = new FunctionImplementation(
                Name: new Node<string>(func.Declaration.Value.Name.Range, funcName),
                Arguments: func.Declaration.Value.Arguments,
                Expression: formattedExpr);

            var formattedDeclRange = new Range(declStartContext.ToLocation(), afterExpr.ToLocation());
            var formattedDeclNode = new Node<FunctionImplementation>(formattedDeclRange, formattedImpl);

            var formattedFunc = new FunctionStruct(
                func.Documentation,
                formattedSignature,
                formattedDeclNode);

            var finalContext = afterExpr.Dedent();

            return (new Expression.LetDeclaration.LetFunction(formattedFunc), finalContext);
        }

        private (Expression.LetDeclaration, FormattingContext) FormatLetDestructuring(Expression.LetDeclaration.LetDestructuring letDestr, FormattingContext context)
        {
            // For now, preserve the pattern and format the expression
            // TODO: Implement proper pattern formatting

            // Estimate pattern length (simplified)
            var patternText = "pattern"; // Placeholder
            var afterPattern = context.Advance(patternText.Length);

            // " ="
            var afterEquals = afterPattern.Advance(2);
            var afterEqualsNewline = afterEquals.NextRow();

            // Format the expression (indented)
            var exprContext = afterEqualsNewline.Indent().SetIndentColumn();
            var (formattedExpr, afterExpr) = VisitExpressionNode(letDestr.Expression, exprContext);

            var finalContext = afterExpr.Dedent();

            return (new Expression.LetDeclaration.LetDestructuring(letDestr.Pattern, formattedExpr), finalContext);
        }

        public override (Node<Expression>, FormattingContext) VisitUnitExpr(
            Expression.UnitExpr expr,
            FormattingContext context)
        {
            // Unit expression is "()"
            var startLocation = context.ToLocation();
            var afterUnit = context.Advance(2); // "()"

            var exprRange = new Range(
                Start: startLocation,
                End: afterUnit.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterUnit);
        }

        public override (Node<Expression>, FormattingContext) VisitCharLiteral(
            Expression.CharLiteral expr,
            FormattingContext context)
        {
            // Char literals are like 't' or '\n' - we need to render them properly
            var startLocation = context.ToLocation();
            var charText = Rendering.RenderCharLiteral(expr.Value);
            var afterChar = context.Advance(charText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterChar.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterChar);
        }

        public override (Node<Expression>, FormattingContext) VisitHex(
            Expression.Hex expr,
            FormattingContext context)
        {
            // Hex literals like 0x81 or 0x00012345
            var startLocation = context.ToLocation();
            var hexText = Rendering.RenderHexPattern(expr.Value);
            var afterHex = context.Advance(hexText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterHex.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterHex);
        }

        public override (Node<Expression>, FormattingContext) VisitFloatable(
            Expression.Floatable expr,
            FormattingContext context)
        {
            // Float literals like 1.0 or 3.14
            // Use FormatFloatForElm to avoid scientific notation (e.g., 1E-16)
            var startLocation = context.ToLocation();
            var floatText = Rendering.FormatFloatForElm(expr.Value);
            var afterFloat = context.Advance(floatText.Length);

            var exprRange = new Range(
                Start: startLocation,
                End: afterFloat.ToLocation());

            return (new Node<Expression>(exprRange, expr), afterFloat);
        }

        public override (Node<Expression>, FormattingContext) VisitTupledExpression(
            Expression.TupledExpression expr,
            FormattingContext context)
        {
            // Tupled expressions like ( 1, 2, 3 )
            var startLocation = context.ToLocation();

            // "( "
            var afterOpen = context.Advance(2);

            var formattedElements = new List<Node<Expression>>();
            var currentContext = afterOpen;

            for (var i = 0; i < expr.Elements.Count; i++)
            {
                var (formattedElem, afterElem) = VisitExpressionNode(expr.Elements[i], currentContext);
                formattedElements.Add(formattedElem);

                currentContext = i < expr.Elements.Count - 1
                    ? afterElem.Advance(2) // ", "
                    : afterElem.Advance(2); // " )"
            }

            var exprRange = new Range(startLocation, currentContext.ToLocation());
            return (new Node<Expression>(exprRange, new Expression.TupledExpression(formattedElements)), currentContext);
        }

        /// <summary>
        /// Format a record expression with access to its original Range for accurate multiline detection.
        /// The original Range is essential for single-field records to detect if the closing brace
        /// is on a different row than the field (indicating multiline format).
        /// </summary>
        private (Node<Expression>, FormattingContext) VisitRecordExprWithRange(
            Expression.RecordExpr expr,
            Range originalRange,
            FormattingContext context)
        {
            if (expr.Fields.Count is 0)
            {
                // Empty record: "{}"
                var afterRecord = context.Advance(2);
                var emptyRecordRange = new Range(context.ToLocation(), afterRecord.ToLocation());
                return (new Node<Expression>(emptyRecordRange, expr), afterRecord);
            }

            // Detect if this specific record is multiline by checking:
            // 1. For multiple fields: if they span different rows
            // 2. For single field: if the parent record Range (including closing brace) spans multiple rows
            //    OR if the field itself spans multiple rows (indicating multiline formatting)
            bool isMultiLine;
            if (expr.Fields.Count >= 2)
            {
                // Multiple fields: use helper to check if they span different rows
                isMultiLine = NodesSpanMultipleRows(expr.Fields);
            }
            else
            {
                // Single field: check if the parent record Range spans multiple rows
                // OR if the field itself spans multiple rows
                // This detects cases like: { a = []\n    } where the field is single-line
                // but the closing brace is on a new line, or { a = 13\n    } where the
                // field Range has been marked as multiline by a preprocessor
                var field = expr.Fields[0];
                var fieldEndRow = field.Range.End.Row;
                var fieldStartRow = field.Range.Start.Row;
                var recordEndRow = originalRange.End.Row;
                isMultiLine = recordEndRow > fieldEndRow || fieldEndRow > fieldStartRow;
            }

            return FormatRecord(expr, context, isMultiLine);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordExpr(
            Expression.RecordExpr expr,
            FormattingContext context)
        {
            // This method is called by the base visitor pattern when we don't have direct access
            // to the original Node's Range (only the Expression value). This occurs when called
            // from external code or the base Visit() dispatcher.
            // We synthesize a Range based on field positions for multiline detection.

            if (expr.Fields.Count is 0)
            {
                // For empty records, use a minimal placeholder Range (doesn't affect multiline detection)
                var emptyRange = new Range(new Location(1, 1), new Location(1, 3));  // "{}" is 2 chars
                return VisitRecordExprWithRange(expr, emptyRange, context);
            }

            // Synthesize a Range that encompasses all fields
            // This provides fallback multiline detection when the original Range isn't available
            var startLoc = expr.Fields[0].Range.Start;
            var endLoc = expr.Fields[expr.Fields.Count - 1].Range.End;
            var synthesizedRange = new Range(startLoc, endLoc);

            return VisitRecordExprWithRange(expr, synthesizedRange, context);
        }

        private (Node<Expression>, FormattingContext) FormatRecord(
            Expression.RecordExpr expr,
            FormattingContext context,
            bool isMultiLine)
        {
            if (!isMultiLine)
            {
                // Single-line: "{ field = value, field2 = value2 }"
                var afterOpen = context.Advance(2); // "{ "
                var currentContext = afterOpen;
                var formattedFields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

                for (var i = 0; i < expr.Fields.Count; i++)
                {
                    var field = expr.Fields[i].Value;

                    // field name
                    var fieldNameLength = field.fieldName.Value.Length;
                    var fieldNameRange = new Range(currentContext.ToLocation(), currentContext.Advance(fieldNameLength).ToLocation());
                    var formattedFieldName = new Node<string>(fieldNameRange, field.fieldName.Value);
                    var afterFieldName = currentContext.Advance(fieldNameLength);

                    // " = "
                    var afterEquals = afterFieldName.Advance(3);

                    // value
                    var (formattedValue, afterValue) = VisitExpressionNode(field.valueExpr, afterEquals);

                    var fieldRange = new Range(currentContext.ToLocation(), afterValue.ToLocation());
                    var formattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                        fieldRange,
                        (formattedFieldName, formattedValue));
                    formattedFields.Add(formattedField);

                    currentContext = i < expr.Fields.Count - 1
                        ? afterValue.Advance(2) // ", "
                        : afterValue.Advance(2); // " }"
                }

                var exprRange = new Range(context.ToLocation(), currentContext.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.RecordExpr(formattedFields)), currentContext);
            }
            else
            {
                // Multi-line: fields each on their own line
                var openingBraceColumn = context.CurrentColumn;
                var afterOpen = context.Advance(2); // "{ "
                var formattedFields = new List<Node<(Node<string> fieldName, Node<Expression> valueExpr)>>();

                // First field on same line as "{"
                var firstField = expr.Fields[0].Value;
                var fieldNameLength = firstField.fieldName.Value.Length;
                var fieldNameRange = new Range(afterOpen.ToLocation(), afterOpen.Advance(fieldNameLength).ToLocation());
                var formattedFieldName = new Node<string>(fieldNameRange, firstField.fieldName.Value);
                var afterFieldName = afterOpen.Advance(fieldNameLength);
                var afterEquals = afterFieldName.Advance(3); // " = "

                // Check if the first field value is multiline
                var isFirstValueMultiline = firstField.valueExpr.Range.End.Row > firstField.valueExpr.Range.Start.Row;

                FormattingContext firstValueContext;
                if (isFirstValueMultiline)
                {
                    // Put multiline value on a new line, indented
                    // When creating context for multiline values, convert parent's AddHalfIndent to a full IndentLevel
                    var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                    firstValueContext = new FormattingContext(
                        afterEquals.CurrentRow + 1,
                        1,
                        baseIndentLevel + 1).SetIndentColumn();
                }
                else
                {
                    // Single-line value stays on the same line
                    firstValueContext = afterEquals;
                }

                var (formattedValue, afterFirstValue) = VisitExpressionNode(firstField.valueExpr, firstValueContext);

                // Dedent if we indented for multiline value
                if (isFirstValueMultiline)
                {
                    afterFirstValue = afterFirstValue.Dedent();
                }

                var firstFieldRange = new Range(afterOpen.ToLocation(), afterFirstValue.ToLocation());
                var firstFormattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                    firstFieldRange,
                    (formattedFieldName, formattedValue));
                formattedFields.Add(firstFormattedField);

                // Subsequent fields on new lines with ", " prefix
                var nextRow = afterFirstValue.CurrentRow + 1;

                for (var i = 1; i < expr.Fields.Count; i++)
                {
                    var field = expr.Fields[i].Value;

                    // Create context at opening brace column for comma
                    var commaContext = new FormattingContext(
                        nextRow,
                        openingBraceColumn,
                        context.IndentLevel);

                    // ", " at the start of the line
                    var afterComma = commaContext.Advance(2);

                    // field name
                    fieldNameLength = field.fieldName.Value.Length;
                    fieldNameRange = new Range(afterComma.ToLocation(), afterComma.Advance(fieldNameLength).ToLocation());
                    formattedFieldName = new Node<string>(fieldNameRange, field.fieldName.Value);
                    afterFieldName = afterComma.Advance(fieldNameLength);

                    // " = "
                    afterEquals = afterFieldName.Advance(3);

                    // Check if the field value is multiline (spans multiple rows in the original source)
                    var isValueMultiline = field.valueExpr.Range.End.Row > field.valueExpr.Range.Start.Row;

                    FormattingContext valueContext;
                    if (isValueMultiline)
                    {
                        // Put multiline value on a new line, indented
                        // When creating context for multiline values, convert parent's AddHalfIndent to a full IndentLevel
                        var baseIndentLevel = context.IndentLevel + (context.AddHalfIndent ? 1 : 0);
                        valueContext = new FormattingContext(
                            afterEquals.CurrentRow + 1,
                            1,
                            baseIndentLevel + 1).SetIndentColumn();
                    }
                    else
                    {
                        // Single-line value stays on the same line
                        valueContext = afterEquals;
                    }

                    // value
                    var (formattedFieldValue, afterValue) = VisitExpressionNode(field.valueExpr, valueContext);

                    // Dedent if we indented for multiline value
                    if (isValueMultiline)
                    {
                        afterValue = afterValue.Dedent();
                    }

                    var fieldRange = new Range(afterComma.ToLocation(), afterValue.ToLocation());
                    var formattedField = new Node<(Node<string> fieldName, Node<Expression> valueExpr)>(
                        fieldRange,
                        (formattedFieldName, formattedFieldValue));
                    formattedFields.Add(formattedField);

                    // Move to next line
                    nextRow = afterValue.CurrentRow + 1;
                }

                // Closing brace "}" should be at opening brace column
                var closingBraceContext = new FormattingContext(
                    nextRow,
                    openingBraceColumn,
                    context.IndentLevel);
                var afterClose = closingBraceContext.Advance(1);

                var exprRange = new Range(context.ToLocation(), afterClose.ToLocation());
                return (new Node<Expression>(exprRange, new Expression.RecordExpr(formattedFields)), afterClose);
            }
        }

        public override (Node<Expression>, FormattingContext) VisitRecordAccess(
            Expression.RecordAccess expr,
            FormattingContext context)
        {
            // Record access like record.field
            var startLocation = context.ToLocation();

            // Format the record expression
            var (formattedRecord, afterRecord) = VisitExpressionNode(expr.Record, context);

            // "."
            var afterDot = afterRecord.Advance(1);

            // field name
            var fieldLength = expr.FieldName.Value.Length;
            var afterField = afterDot.Advance(fieldLength);

            var exprRange = new Range(startLocation, afterField.ToLocation());
            var formattedFieldName = new Node<string>(
                new Range(afterDot.ToLocation(), afterField.ToLocation()),
                expr.FieldName.Value);
            return (new Node<Expression>(exprRange, new Expression.RecordAccess(formattedRecord, formattedFieldName)), afterField);
        }

        public override (Node<Expression>, FormattingContext) VisitRecordAccessFunction(
            Expression.RecordAccessFunction expr,
            FormattingContext context)
        {
            // Record access function like .field
            var startLocation = context.ToLocation();

            // ".field"
            var accessLength = 1 + expr.FunctionName.Length; // "." + field name
            var afterAccess = context.Advance(accessLength);

            var exprRange = new Range(startLocation, afterAccess.ToLocation());
            return (new Node<Expression>(exprRange, expr), afterAccess);
        }

        public override (Node<Expression>, FormattingContext) VisitLambdaExpression(
            Expression.LambdaExpression expr,
            FormattingContext context)
        {
            // Lambda like \x -> x + 1
            var startLocation = context.ToLocation();

            // "\"
            var currentContext = context.Advance(1);

            // Format patterns
            var formattedPatterns = new List<Node<Pattern>>();
            for (var i = 0; i < expr.Lambda.Arguments.Count; i++)
            {
                var pattern = expr.Lambda.Arguments[i];
                var patternText = FormatPatternText(pattern.Value);
                var afterPattern = currentContext.Advance(patternText.Length);
                var patternRange = new Range(currentContext.ToLocation(), afterPattern.ToLocation());
                formattedPatterns.Add(new Node<Pattern>(patternRange, pattern.Value));

                // Space after pattern (except before arrow)
                if (i < expr.Lambda.Arguments.Count - 1)
                {
                    currentContext = afterPattern.Advance(1);
                }
                else
                {
                    currentContext = afterPattern;
                }
            }

            // Check if expression body is on a different row than the arrow would be
            // This happens with parenthesized lambdas with multiline bodies
            var originalExprStartRow = expr.Lambda.Expression.Range.Start.Row;
            var arrowContext = currentContext.Advance(4); // After " -> "

            // If the original expression started on a different row, add newline after arrow
            FormattingContext exprStartContext;
            var isMultiLine = originalExprStartRow > currentContext.CurrentRow;
            if (isMultiLine)
            {
                // Multiline: move to next row and indent
                exprStartContext = currentContext.Advance(4).NextRow().Indent().SetIndentColumn();
            }
            else
            {
                // Single line: just advance past arrow
                exprStartContext = arrowContext;
            }

            // Expression
            var (formattedExpr, afterExpr) = VisitExpressionNode(expr.Lambda.Expression, exprStartContext);

            // If we indented for multiline, we need to dedent after
            if (isMultiLine)
            {
                afterExpr = afterExpr.Dedent();
            }

            var exprRange = new Range(startLocation, afterExpr.ToLocation());
            var formattedLambda = new LambdaStruct(formattedPatterns, formattedExpr);
            return (new Node<Expression>(exprRange, new Expression.LambdaExpression(formattedLambda)), afterExpr);
        }

        public override (Node<Expression>, FormattingContext) VisitPrefixOperator(
            Expression.PrefixOperator expr,
            FormattingContext context)
        {
            // Prefix operator like (+) or (-)
            var startLocation = context.ToLocation();

            // "(op)"
            var opLength = 2 + expr.Operator.Length; // "(" + operator + ")"
            var afterOp = context.Advance(opLength);

            var exprRange = new Range(startLocation, afterOp.ToLocation());
            return (new Node<Expression>(exprRange, expr), afterOp);
        }

        public override (Node<Expression>, FormattingContext) VisitNegation(
            Expression.Negation expr,
            FormattingContext context)
        {
            // Negation is represented as "-" followed by the expression
            var startLocation = context.ToLocation();

            // Move past the "-" operator (1 character)
            var afterMinus = context.Advance(1);

            // Format the inner expression
            var (formattedInner, afterInner) = VisitExpressionNode(expr.Expression, afterMinus);

            // Create a new Negation with the formatted inner expression
            var formattedExpr = new Expression.Negation(formattedInner);

            // Create range from start (before "-") to end
            var exprRange = new Range(
                Start: startLocation,
                End: formattedInner.Range.End);

            return (new Node<Expression>(exprRange, formattedExpr), afterInner);
        }

        public override (Node<Expression>, FormattingContext) VisitParenthesizedExpression(
            Expression.ParenthesizedExpression expr,
            FormattingContext context)
        {
            // Check if the parenthesized expression is multiline based on the original Range
            var isMultiLine = expr.Expression.Range.Start.Row != expr.Expression.Range.End.Row;

            if (isMultiLine)
            {
                // Multiline format: parentheses on their own positions, content indented
                // Example:
                // (\b ->
                //     a + b
                // )
                var startLocation = context.ToLocation();

                // Opening paren on current line
                var afterOpenParen = context.Advance(1);

                // Format inner expression on same line as opening paren
                var (formattedInner, afterInner) = VisitExpressionNode(expr.Expression, afterOpenParen);

                // Closing paren needs to be on a new line, at the same indentation as the opening paren
                var closingParenContext = afterInner.NextRow().SetColumn(context.CurrentColumn);
                var afterClosingParen = closingParenContext.Advance(1);

                var formattedExpr = new Expression.ParenthesizedExpression(formattedInner);
                var exprRange = new Range(
                    Start: startLocation,
                    End: afterClosingParen.ToLocation());

                return (new Node<Expression>(exprRange, formattedExpr), afterClosingParen);
            }
            else
            {
                // Single-line format: (expr)
                var startLocation = context.ToLocation();

                // Move past the opening paren (1 character)
                var innerContext = context.Advance(1);

                // Format the inner expression
                var (formattedInner, afterInner) = VisitExpressionNode(expr.Expression, innerContext);

                // The closing paren is one column after the inner expression
                var endContext = afterInner.Advance(1);

                // Create a new ParenthesizedExpression with the formatted inner expression
                var formattedExpr = new Expression.ParenthesizedExpression(formattedInner);

                // Create range from start (before opening paren) to end (after closing paren)
                var exprRange = new Range(
                    Start: startLocation,
                    End: endContext.ToLocation());

                return (new Node<Expression>(exprRange, formattedExpr), endContext);
            }
        }

        // For unsupported expressions, preserve as-is
        protected override (Node<Expression>, FormattingContext) VisitUnknown(Expression expr, FormattingContext context)
        {
            // Ensure variants are encoded explicitly: Fail if an override is missing.

            throw new System.NotImplementedException(
                $"Formatting for expression type '{expr.GetType().Name}' is not implemented.");
        }
    }
}
