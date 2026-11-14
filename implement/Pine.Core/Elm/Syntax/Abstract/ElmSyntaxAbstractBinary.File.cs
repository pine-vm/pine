using System;

namespace Pine.Core.Elm.Syntax.Abstract;

public static partial class ElmSyntaxAbstractBinary
{
    public sealed partial class Encoder
    {
        /// <summary>Writes a file, sharing retained syntax across its declarations.</summary>
        public void WriteFile(File file)
        {
            Start(Constructor.File, checked(2 + file.Imports.Count + file.Declarations.Count));
            WriteModule(file.ModuleDefinition);
            _writer.Integer(file.Imports.Count);

            for (var index = 0; index < file.Imports.Count; ++index)
                WriteImport(file.Imports[index]);

            for (var index = 0; index < file.Declarations.Count; ++index)
                WriteDeclaration(file.Declarations[index]);
        }

        /// <summary>Writes a module definition in the current reference scope.</summary>
        public void WriteModule(Module module)
        {
            switch (module)
            {
                case Module.NormalModule normal:
                    Start(Constructor.NormalModule, 1);
                    ModuleData(normal.ModuleData);
                    break;

                case Module.PortModule port:
                    Start(Constructor.PortModule, 1);
                    ModuleData(port.ModuleData);
                    break;

                case Module.EffectModule effect:
                    Start(Constructor.EffectModule, 1);
                    Start(Constructor.EffectModuleData, checked(3 + effect.ModuleData.ModuleName.Count));
                    WriteExposing(effect.ModuleData.ExposingList);
                    OptionalString(effect.ModuleData.Command);
                    OptionalString(effect.ModuleData.Subscription);
                    Strings(effect.ModuleData.ModuleName);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteModule) + " does not handle module variant: " + module.GetType().Name);
            }
        }

        private void ModuleData(DefaultModuleData data)
        {
            Start(Constructor.DefaultModuleData, checked(1 + data.ModuleName.Count));
            WriteExposing(data.ExposingList);
            Strings(data.ModuleName);
        }

        private void OptionalString(string? value)
        {
            Start(value is null ? Constructor.None : Constructor.Some, value is null ? 0 : 1);

            if (value is not null)
                String(value);
        }

        /// <summary>Writes an import, preserving null and empty aliases distinctly.</summary>
        public void WriteImport(Import import)
        {
            Start(Constructor.Import, checked(2 + import.ModuleName.Count));
            Start(import.ModuleAlias is null ? Constructor.None : Constructor.Some, import.ModuleAlias is null ? 0 : 1);

            if (import.ModuleAlias is { } alias)
            {
                Start(Constructor.ModuleName, alias.Count);
                Strings(alias);
            }

            Start(
                import.ExposingList is null ? Constructor.None : Constructor.Some,
                import.ExposingList is null ? 0 : 1);

            if (import.ExposingList is { } exposing)
                WriteExposing(exposing);

            Strings(import.ModuleName);
        }

        /// <summary>Writes an exposing list in the current reference scope.</summary>
        public void WriteExposing(Exposing exposing)
        {
            switch (exposing)
            {
                case Exposing.All:
                    Start(Constructor.All, 0);
                    break;

                case Exposing.Explicit explicitExposing:
                    Start(Constructor.Explicit, explicitExposing.Exposes.Count);

                    for (var index = 0; index < explicitExposing.Exposes.Count; ++index)
                        WriteTopLevelExpose(explicitExposing.Exposes[index]);

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteExposing) + " does not handle exposing variant: " + exposing.GetType().Name);
            }
        }

        /// <summary>Writes an exposed name in the current reference scope.</summary>
        public void WriteTopLevelExpose(TopLevelExpose expose)
        {
            switch (expose)
            {
                case TopLevelExpose.InfixExpose infix:
                    Start(Constructor.InfixExpose, 1);
                    String(infix.Name);
                    break;

                case TopLevelExpose.FunctionExpose function:
                    Start(Constructor.FunctionExpose, 1);
                    String(function.Name);
                    break;

                case TopLevelExpose.TypeOrAliasExpose type:
                    Start(Constructor.TypeOrAliasExpose, 1);
                    String(type.Name);
                    break;

                case TopLevelExpose.TypeExpose type:
                    Start(Constructor.TypeExpose, 1);
                    Start(Constructor.ExposedType, 2);
                    String(type.ExposedType.Name);
                    _writer.Integer(type.ExposedType.ExposesConstructors ? 1 : 0);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteTopLevelExpose) + " does not handle expose variant: " + expose.GetType().Name);
            }
        }
    }
}
