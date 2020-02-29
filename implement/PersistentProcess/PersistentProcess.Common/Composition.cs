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
                    ok = new TreeComponent { BlobContent = composition.BlobContent }
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
                            err = ImmutableList<(int index, IImmutableList<byte> name)>.Empty
                        };
                    }

                    var currentIndexAndName = (index: componentIndex, name: component.ListContent.ElementAt(0).BlobContent);

                    var parseResult = ParseAsTree(component.ListContent.ElementAt(1));

                    if (parseResult.ok == null)
                    {
                        return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                        {
                            err = ImmutableList.Create(currentIndexAndName).AddRange(parseResult.err)
                        };
                    }

                    return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                    {
                        ok = (name: currentIndexAndName.name, parseResult.ok)
                    };
                })
                .ToImmutableList();

            var firstError =
                compositionResults
                .Select(componentResult => componentResult.err)
                .Where(componentError => componentError != null)
                .FirstOrDefault();

            if (firstError != null)
                return new ParseAsTreeResult { err = firstError };

            return
                new ParseAsTreeResult
                {
                    ok = new TreeComponent
                    { TreeContent = compositionResults.Select(compositionResult => compositionResult.ok).ToImmutableList() }
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
                        .Select(pathComponent => (IImmutableList<byte>)System.Text.Encoding.BigEndianUnicode.GetBytes(pathComponent).ToImmutableList())
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
                blobsWithPath, pathComponent => System.Text.Encoding.BigEndianUnicode.GetBytes(pathComponent).ToImmutableList());

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

        static public byte[] GetHash(Component component)
        {
            if (component.BlobContent != null)
            {
                var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + component.BlobContent.Count.ToString() + "\0");

                return CommonConversion.HashSHA256(prefix.Concat(component.BlobContent).ToArray());
            }

            {
                var componentsHashes =
                    component.ListContent.Select(GetHash).ToList();

                var prefix = System.Text.Encoding.ASCII.GetBytes("list " + componentsHashes.Count.ToString() + "\0");

                return CommonConversion.HashSHA256(prefix.Concat(componentsHashes.SelectMany(t => t)).ToArray());
            }
        }

        public class Result<Err, Ok> : IEquatable<Result<Err, Ok>>
        {
            public Err err;

            public Ok ok;

            public bool Equals(Result<Err, Ok> other)
            {
                if (err != null || other.err != null)
                {
                    if (err == null || other.err == null)
                        return false;

                    return err.Equals(other.err);
                }

                if (ok == null || other.ok == null)
                    return false;

                return ok.Equals(other.ok);
            }

            override public bool Equals(object obj) => Equals(obj as Result<Err, Ok>);

            public override int GetHashCode()
            {
                return HashCode.Combine(err, ok);
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