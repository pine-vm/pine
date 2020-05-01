using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Kalmit
{
    public class Composition
    {
        public class Component : IEquatable<Component>
        {
            public IImmutableList<byte> BlobContent;

            public IImmutableList<Component> ListContent;

            static public Component Blob(ImmutableList<byte> blobContent) =>
                new Component { BlobContent = blobContent };

            static public Component Blob(IReadOnlyList<byte> blobContent) =>
                Blob(blobContent.ToImmutableList());

            public bool Equals(Component other)
            {
                if (BlobContent != null || other.BlobContent != null)
                {
                    if (BlobContent == null || other.BlobContent == null)
                        return false;

                    return BlobContent.SequenceEqual(other.BlobContent);
                }

                if (ListContent == null || other.ListContent == null)
                    return false;

                if (ListContent.Count != other.ListContent.Count)
                    return false;

                return
                    Enumerable.Range(0, ListContent.Count)
                    .All(i => ListContent.ElementAt(i).Equals(other.ListContent.ElementAt(i)));
            }

            override public bool Equals(object obj) => Equals(obj as Component);

            public override int GetHashCode()
            {
                return HashCode.Combine(BlobContent, ListContent);
            }
        }

        public class TreeComponent : IEquatable<TreeComponent>
        {
            public IImmutableList<byte> BlobContent;

            public IImmutableList<(IImmutableList<byte> name, TreeComponent component)> TreeContent;

            public IImmutableList<(IImmutableList<IImmutableList<byte>> path, IImmutableList<byte> blobContent)> EnumerateBlobsTransitive() =>
                TreeContent == null ? null :
                EnumerateBlobsRecursive(TreeContent)
                .ToImmutableList();

            static IEnumerable<(IImmutableList<IImmutableList<byte>> path, IImmutableList<byte> content)> EnumerateBlobsRecursive(
                IImmutableList<(IImmutableList<byte> name, TreeComponent obj)> tree)
            {
                foreach (var treeEntry in tree)
                {
                    if (treeEntry.obj.BlobContent != null)
                        yield return (ImmutableList.Create(treeEntry.name), treeEntry.obj.BlobContent);

                    if (treeEntry.obj.TreeContent != null)
                    {
                        foreach (var subTreeEntry in EnumerateBlobsRecursive(treeEntry.obj.TreeContent))
                        {
                            yield return (subTreeEntry.path.Insert(0, treeEntry.name), subTreeEntry.content);
                        }
                    }
                }
            }

            public bool Equals(TreeComponent other)
            {
                if (BlobContent != null || other.BlobContent != null)
                {
                    if (BlobContent == null || other.BlobContent == null)
                        return false;

                    return BlobContent.SequenceEqual(other.BlobContent);
                }

                if (TreeContent == null || other.TreeContent == null)
                    return false;

                if (TreeContent.Count != other.TreeContent.Count)
                    return false;

                return
                    Enumerable.Range(0, TreeContent.Count)
                    .All(i =>
                    {
                        var thisElement = TreeContent.ElementAt(i);
                        var otherElement = other.TreeContent.ElementAt(i);

                        return thisElement.name.SequenceEqual(otherElement.name) &&
                            thisElement.component.Equals(otherElement.component);
                    });
            }

            override public bool Equals(object obj) => Equals(obj as TreeComponent);

            public override int GetHashCode()
            {
                return HashCode.Combine(BlobContent, TreeContent);
            }
        }

        static public ParseAsTreeResult ParseAsTree(
            Component composition)
        {
            if (composition == null)
                return null;

            if (composition.BlobContent != null)
            {
                return new ParseAsTreeResult
                {
                    Ok = new TreeComponent { BlobContent = composition.BlobContent }
                };
            }

            var compositionResults =
                composition.ListContent
                .Select((component, componentIndex) =>
                {
                    if (!(component.ListContent?.Count == 2) || component.ListContent.ElementAt(0).BlobContent == null)
                    {
                        return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                        {
                            Err = ImmutableList<(int index, IImmutableList<byte> name)>.Empty
                        };
                    }

                    var currentIndexAndName = (index: componentIndex, name: component.ListContent.ElementAt(0).BlobContent);

                    var parseResult = ParseAsTree(component.ListContent.ElementAt(1));

                    if (parseResult.Ok == null)
                    {
                        return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                        {
                            Err = ImmutableList.Create(currentIndexAndName).AddRange(parseResult.Err)
                        };
                    }

                    return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                    {
                        Ok = (name: currentIndexAndName.name, parseResult.Ok)
                    };
                })
                .ToImmutableList();

            var firstError =
                compositionResults
                .Select(componentResult => componentResult.Err)
                .Where(componentError => componentError != null)
                .FirstOrDefault();

            if (firstError != null)
                return new ParseAsTreeResult { Err = firstError };

            return
                new ParseAsTreeResult
                {
                    Ok = new TreeComponent
                    { TreeContent = compositionResults.Select(compositionResult => compositionResult.Ok).ToImmutableList() }
                };
        }

        static public Component FromTree(TreeComponent tree)
        {
            if (tree == null)
                return null;

            if (tree.BlobContent != null)
                return new Component { BlobContent = tree.BlobContent };

            var listContent =
                tree.TreeContent
                .Select(treeComponent =>
                    new Component
                    {
                        ListContent = ImmutableList.Create(
                            new Component { BlobContent = treeComponent.name },
                            FromTree(treeComponent.component))
                    })
                .ToImmutableList();

            return new Component
            {
                ListContent = listContent
            };
        }

        static public TreeComponent TreeFromSetOfBlobsWithCommonFilePath(
            IEnumerable<(string path, IImmutableList<byte> blobContent)> blobsWithPath) =>
            TreeFromSetOfBlobs(
                blobsWithPath.Select(blobWithPath =>
                {
                    var pathComponents =
                        blobWithPath.path.Split("/").SelectMany(pathComponent => pathComponent.Split(@"\"))
                        .Select(pathComponent => (IImmutableList<byte>)System.Text.Encoding.UTF8.GetBytes(pathComponent).ToImmutableList())
                        .ToImmutableList();

                    return (path: (IImmutableList<IImmutableList<byte>>)pathComponents, blobContent: blobWithPath.blobContent);
                })
            );

        static public TreeComponent TreeFromSetOfBlobsWithCommonFilePath(
            IEnumerable<(string path, byte[] blobContent)> blobsWithPath) =>
            TreeFromSetOfBlobsWithCommonFilePath(
                blobsWithPath.Select(blobWithPath => (blobWithPath.path, (IImmutableList<byte>)blobWithPath.blobContent.ToImmutableList())));

        static public TreeComponent TreeFromSetOfBlobs<PathT>(
            IEnumerable<(IImmutableList<PathT> path, IImmutableList<byte> blobContent)> blobsWithPath,
            Func<PathT, IImmutableList<byte>> mapPathComponent) =>
            TreeFromSetOfBlobs(
                blobsWithPath.Select(blobWithPath =>
                    (path: (IImmutableList<IImmutableList<byte>>)blobWithPath.path.Select(mapPathComponent).ToImmutableList(),
                    blobContent: blobWithPath.blobContent)));

        static public TreeComponent TreeFromSetOfBlobsWithStringPath(
            IEnumerable<(IImmutableList<string> path, IImmutableList<byte> blobContent)> blobsWithPath) =>
            TreeFromSetOfBlobs(
                blobsWithPath, pathComponent => System.Text.Encoding.UTF8.GetBytes(pathComponent).ToImmutableList());

        static public TreeComponent TreeFromSetOfBlobsWithStringPath(
            IReadOnlyDictionary<IImmutableList<string>, IImmutableList<byte>> blobsWithPath) =>
            TreeFromSetOfBlobsWithStringPath(
                blobsWithPath.Select(pathAndBlobContent => (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

        static public TreeComponent TreeFromSetOfBlobs(
            IEnumerable<(IImmutableList<IImmutableList<byte>> path, IImmutableList<byte> blobContent)> blobsWithPath) =>
            new TreeComponent
            {
                TreeContent = TreeContentFromSetOfBlobs(blobsWithPath)
            };

        static public IImmutableList<(IImmutableList<byte> name, TreeComponent obj)> TreeContentFromSetOfBlobs(
            IEnumerable<(IImmutableList<IImmutableList<byte>> path, IImmutableList<byte> blobContent)> blobsWithPath)
        {
            var groupedByDirectory =
                blobsWithPath
                .GroupBy(
                    pathAndContent => 1 < pathAndContent.path.Count ? pathAndContent.path.First() : null,
                    new ByteListComparer())
                .ToImmutableList();

            var currentLevelBlobs =
                groupedByDirectory
                .FirstOrDefault(group => group.Key == null)
                .EmptyIfNull()
                .Select(pathAndContent =>
                    (name: pathAndContent.path.First(),
                    blobContent: new TreeComponent { BlobContent = pathAndContent.blobContent.ToImmutableList() }))
                .OrderBy(nameAndContent => nameAndContent.name, new ByteListComparer())
                .ToImmutableList();

            var subTrees =
                groupedByDirectory
                .Where(group => group.Key != null)
                .Select(directoryGroup =>
                {
                    var blobsWithRelativePaths =
                        directoryGroup.Select(pathAndContent =>
                            (path: (IImmutableList<IImmutableList<byte>>)pathAndContent.path.Skip(1).ToImmutableList(),
                            blobContent: pathAndContent.blobContent));

                    return (name: (IImmutableList<byte>)directoryGroup.Key.ToImmutableList(), content: new TreeComponent
                    {
                        TreeContent = TreeContentFromSetOfBlobs(blobsWithRelativePaths)
                    });
                })
                .OrderBy(nameAndContent => nameAndContent.name, new ByteListComparer())
                .ToImmutableList();

            return currentLevelBlobs.AddRange(subTrees).ToImmutableList();
        }

        static public Result<String, Component> Deserialize(
            byte[] serializedComponent,
            Func<IReadOnlyList<byte>, IReadOnlyList<byte>> loadSerializedComponentByHash) =>
            Deserialize(serializedComponent.ToImmutableList(), loadSerializedComponentByHash);

        static public Result<String, Component> Deserialize(
            IImmutableList<byte> serializedComponent,
            Func<IReadOnlyList<byte>, IReadOnlyList<byte>> loadSerializedComponentByHash)
        {
            var asciiStringUpToNull =
                System.Text.Encoding.ASCII.GetString(serializedComponent.TakeWhile(c => c != '\0').ToArray());

            var asciiStringUpToFirstSpace =
                asciiStringUpToNull.Split(' ').First();

            if (asciiStringUpToFirstSpace == "blob")
            {
                var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

                var expectedCount = serializedComponent.Count - beginningToRemoveLength;

                var count = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

                if (count != expectedCount)
                    return Result<string, Component>.err("Unexpected count: got " + count + ", but I expected " + expectedCount);

                return Result<string, Component>.ok(Component.Blob(serializedComponent.RemoveRange(0, beginningToRemoveLength)));
            }

            if (asciiStringUpToFirstSpace == "list")
            {
                var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

                var remainingBytes = serializedComponent.RemoveRange(0, beginningToRemoveLength);

                var parsedElementCount = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

                var elementHashLength = 32;

                var expectedRemainingLength = parsedElementCount * elementHashLength;

                if (remainingBytes.Count != expectedRemainingLength)
                    return Result<string, Component>.err(
                        "Unexpected remaining length: " + remainingBytes.Count + " instead of " + expectedRemainingLength);

                var elementsHashes =
                    Enumerable.Range(0, parsedElementCount)
                    .Select(elementIndex => remainingBytes.Skip(elementIndex * elementHashLength).Take(elementHashLength).ToImmutableList())
                    .ToImmutableList();

                Result<string, Component> TryLoadElementForHash(IImmutableList<byte> elementHash)
                {
                    var loadedElementSerialRepresentation = loadSerializedComponentByHash(elementHash);

                    if (loadedElementSerialRepresentation == null)
                        return Result<string, Component>.err(
                            "Failed to load list element " + CommonConversion.StringBase16FromByteArray(elementHash.ToArray()));

                    if (!CommonConversion.HashSHA256(loadedElementSerialRepresentation.ToArray()).SequenceEqual(elementHash))
                        return Result<string, Component>.err(
                            "Hash for loaded element does not match " + CommonConversion.StringBase16FromByteArray(elementHash.ToArray()));

                    return Deserialize(loadedElementSerialRepresentation.ToImmutableList(), loadSerializedComponentByHash);
                }

                var loadElementsResults =
                    elementsHashes
                    .Select(elementHash => (elementHash, loadResult: TryLoadElementForHash(elementHash)))
                    .ToImmutableList();

                var firstFailed =
                    loadElementsResults
                    .FirstOrDefault(elementResult => elementResult.loadResult.Ok == null);

                if (firstFailed.elementHash != null)
                    return Result<string, Component>.err(
                        "Failed to load element " + CommonConversion.StringBase16FromByteArray(firstFailed.elementHash.ToArray()) + ": " + firstFailed.loadResult.Err);

                return Result<string, Component>.ok(
                    new Component
                    {
                        ListContent = loadElementsResults.Select(elementResult => elementResult.loadResult.Ok).ToImmutableList(),
                    });
            }

            return Result<string, Component>.err("Invalid prefix: '" + asciiStringUpToFirstSpace + "'.");
        }

        static public byte[] GetSerialRepresentation(Component component) =>
            GetSerialRepresentationAndDependencies(component).serialRepresentation;

        static public (byte[] serialRepresentation, IReadOnlyCollection<Component> dependencies)
            GetSerialRepresentationAndDependencies(Component component)
        {
            if (component.BlobContent != null)
            {
                var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + component.BlobContent.Count.ToString() + "\0");

                return
                    (serialRepresentation: prefix.Concat(component.BlobContent).ToArray(),
                    dependencies: ImmutableHashSet<Component>.Empty);
            }

            {
                var componentsHashes =
                    component.ListContent.Select(GetHash).ToList();

                var prefix = System.Text.Encoding.ASCII.GetBytes("list " + componentsHashes.Count.ToString() + "\0");

                return
                    (serialRepresentation: prefix.Concat(componentsHashes.SelectMany(t => t)).ToArray(),
                    dependencies: component.ListContent);
            }
        }

        static public byte[] GetHash(Component component) =>
            GetHashAndDependencies(component).hash;

        static public (byte[] hash, IReadOnlyCollection<Component> dependencies)
            GetHashAndDependencies(Component component)
        {
            var (serialRepresentation, dependencies) = GetSerialRepresentationAndDependencies(component);

            return (hash: CommonConversion.HashSHA256(serialRepresentation), dependencies: dependencies);
        }

        static public byte[] GetHash(TreeComponent component) =>
            CommonConversion.HashSHA256(GetSerialRepresentation(FromTree(component)));

        public class Result<ErrT, OkT> : IEquatable<Result<ErrT, OkT>>
        {
            public ErrT Err;

            public OkT Ok;

            static public Result<ErrT, OkT> err(ErrT err) =>
                new Result<ErrT, OkT> { Err = err };

            static public Result<ErrT, OkT> ok(OkT ok) =>
                new Result<ErrT, OkT> { Ok = ok };

            public bool Equals(Result<ErrT, OkT> other)
            {
                if (Err != null || other.Err != null)
                {
                    if (Err == null || other.Err == null)
                        return false;

                    return Err.Equals(other.Err);
                }

                if (Ok == null || other.Ok == null)
                    return false;

                return Ok.Equals(other.Ok);
            }

            override public bool Equals(object obj) => Equals(obj as Result<ErrT, OkT>);

            public override int GetHashCode()
            {
                return HashCode.Combine(Err, Ok);
            }
        }

        public class ParseAsTreeResult : Result<IImmutableList<(int index, IImmutableList<byte> name)>, TreeComponent>
        {
        }

        public class ByteListComparer : IComparer<IReadOnlyList<byte>>, IEqualityComparer<IReadOnlyList<byte>>
        {
            public int Compare(IReadOnlyList<byte> x, IReadOnlyList<byte> y)
            {
                if (x == null && y == null)
                    return 0;

                if (x == null)
                    return -1;

                if (y == null)
                    return 1;

                int result;
                for (int index = 0; index < Math.Min(x.Count, y.Count); index++)
                {
                    result = x[index].CompareTo(y[index]);
                    if (result != 0) return result;
                }
                return x.Count.CompareTo(y.Count);
            }

            public bool Equals(IReadOnlyList<byte> x, IReadOnlyList<byte> y) =>
                Compare(x, y) == 0;

            public int GetHashCode(IReadOnlyList<byte> obj) => obj.Count;
        }
    }
}