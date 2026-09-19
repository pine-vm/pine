using AwesomeAssertions;
using Microsoft.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class CompileToAssemblyTests
{
    [Fact]
    public void Loads_legacy_dispatcher_dictionary()
    {
        const string source =
            """
            using Pine.Core;
            using System;
            using System.Collections.Generic;

            namespace HistoricalBundle;

            public static class Dispatcher
            {
                public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue>>
                    BuildDispatcherDictionary() =>
                    new Dictionary<PineValue, Func<PineValue, PineValue>>
                    {
                        [PineValue.EmptyList] = environment => PineValue.List(environment),
                        [PineValue.EmptyBlob] = environment => null,
                    };
            }
            """;

        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> csharpFiles =
            new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
            {
                [["HistoricalBundle", "Dispatcher.cs"]] = Encoding.UTF8.GetBytes(source),
            };

        var compileResult =
            CompileToAssembly.Compile(
                csharpFiles,
                OptimizationLevel.Debug);

        var compiledAssembly =
            compileResult.Extract(error => throw new InvalidOperationException(error));

        var dictionary =
            compiledAssembly.BuildCompiledExpressionsDictionary();

        var environment =
            PineValueInProcess.Create(PineValue.Blob([42]));

        dictionary[PineValue.EmptyList](environment)!.Evaluate()
            .Should().Be(PineValue.List(environment.Evaluate()));

        dictionary[PineValue.EmptyBlob](environment)
            .Should().BeNull();
    }
}
