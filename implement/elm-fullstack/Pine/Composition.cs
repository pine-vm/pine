using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine;

public class Composition
{
    public record Component(ReadOnlyMemory<byte>? BlobContent = null, IImmutableList<Component>? ListContent = null)
    {
        static public Component Blob(ReadOnlyMemory<byte> blobContent) =>
            new(BlobContent: blobContent);

        static public Component List(IImmutableList<Component> listContent) =>
            new(ListContent: listContent);

        static public readonly Component EmptyList = List(ImmutableList<Component>.Empty);

        public virtual bool Equals(Component? other)
        {
            if (other is null)
                return false;

            if (BlobContent != null || other.BlobContent != null)
            {
                if (BlobContent == null || other.BlobContent == null)
                    return false;

                return BlobContent.Value.Span.SequenceEqual(other.BlobContent.Value.Span);
            }

            if (ListContent == null || other.ListContent == null)
                return false;

            if (ListContent.Count != other.ListContent.Count)
                return false;

            return
                Enumerable.Range(0, ListContent.Count)
                .All(i => ListContent.ElementAt(i).Equals(other.ListContent.ElementAt(i)));
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(BlobContent, ListContent);
        }
    }

    static public Component ComponentFromString(string str) =>
        Component.List(ListValueFromString(str).ToImmutableList());

    static public IImmutableList<Component> ListValueFromString(string str) =>
        ToCodePoints(str)!
        .Select(charAsInteger => new System.Numerics.BigInteger(charAsInteger))
        .Select(ComponentFromUnsignedInteger)
        .Select(charValue => charValue.Ok!).ToImmutableList();


    // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130
    static public int[]? ToCodePoints(string str)
    {
        if (str == null)
            return null;

        var codePoints = new List<int>(str.Length);
        for (int i = 0; i < str.Length; i++)
        {
            codePoints.Add(char.ConvertToUtf32(str, i));
            if (char.IsHighSurrogate(str[i]))
                i += 1;
        }

        return codePoints.ToArray();
    }

    static public Result<string, string> StringFromComponent(Component component)
    {
        if (component.ListContent == null)
            return Result<string, string>.err("Only a ListValue can represent a string.");

        var charsIntegersResults =
            component.ListContent
            .Select(UnsignedIntegerFromComponent)
            .ToImmutableList();

        if (charsIntegersResults.Any(toIntResult => toIntResult.Ok == null))
            return Result<string, string>.err("Failed to map list elements to unsigned integers.");

        return Result<string, string>.ok(
            string.Join("", charsIntegersResults.Select(toIntResult => char.ConvertFromUtf32((int)toIntResult.Ok!))));
    }

    static public Result<string, Component> ComponentFromUnsignedInteger(System.Numerics.BigInteger integer) =>
        BlobValueFromUnsignedInteger(integer)
        .map(blob => Component.Blob(blob!.Value));

    static public Result<string, ReadOnlyMemory<byte>?> BlobValueFromUnsignedInteger(System.Numerics.BigInteger integer)
    {
        var signedBlobValue = BlobValueFromSignedInteger(integer);

        if (signedBlobValue.Span[0] != 0)
            return Result<string, ReadOnlyMemory<byte>?>.err("Argument is a negative integer.");

        return Result<string, ReadOnlyMemory<byte>?>.ok(signedBlobValue[1..]);
    }

    static public Component ComponentFromSignedInteger(System.Numerics.BigInteger integer) =>
        Component.Blob(BlobValueFromSignedInteger(integer));

    static public ReadOnlyMemory<byte> BlobValueFromSignedInteger(System.Numerics.BigInteger integer)
    {
        var absoluteValue = System.Numerics.BigInteger.Abs(integer);

        var signByte =
            (byte)(absoluteValue == integer ? 0 : 0x80);

        var absoluteArray = absoluteValue.ToByteArray(isUnsigned: true, isBigEndian: true);

        var memory = new byte[1 + absoluteArray.Length];

        memory[0] = signByte;
        absoluteArray.CopyTo(memory, 1);

        return memory;
    }

    static public Result<string, System.Numerics.BigInteger?> SignedIntegerFromComponent(Component component)
    {
        if (component.BlobContent == null)
            return Result<string, System.Numerics.BigInteger?>.err(
                "Only a BlobValue can represent an integer.");

        return SignedIntegerFromBlobValue(component.BlobContent.Value.Span);
    }

    static public Result<string, System.Numerics.BigInteger?> SignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue)
    {
        if (blobValue.Length < 1)
            return Result<string, System.Numerics.BigInteger?>.err(
                "Empty blob is not a valid integer because the sign byte is missing. Did you mean to use an unsigned integer?");

        var signByte = blobValue[0];

        if (signByte != 0 && signByte != 0x80)
            return Result<string, System.Numerics.BigInteger?>.err(
                "Unexpected value for sign byte of integer: " + signByte);

        var isNegative = signByte != 0;

        var integerValue =
            UnsignedIntegerFromBlobValue(blobValue.Slice(1));

        return
            Result<string, System.Numerics.BigInteger?>.ok(
                integerValue * new System.Numerics.BigInteger(isNegative ? -1 : 1));
    }

    static public Result<string, System.Numerics.BigInteger?> UnsignedIntegerFromComponent(Component component)
    {
        if (component.BlobContent == null)
            return Result<string, System.Numerics.BigInteger?>.err(
                "Only a BlobValue can represent an integer.");

        return Result<string, System.Numerics.BigInteger?>.ok(
            UnsignedIntegerFromBlobValue(component.BlobContent.Value.Span));
    }

    static public System.Numerics.BigInteger UnsignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue) =>
        new(blobValue, isUnsigned: true, isBigEndian: true);

    public record TreeWithStringPath : IEquatable<TreeWithStringPath>
    {
        public ReadOnlyMemory<byte>? BlobContent { private init; get; }

        public IImmutableList<(string name, TreeWithStringPath component)>? TreeContent { private init; get; }

        static public readonly IComparer<string> TreeEntryNameComparer = StringComparer.Ordinal;

        static public TreeWithStringPath Blob(ReadOnlyMemory<byte> blobContent) =>
            new() { BlobContent = blobContent };

        static public TreeWithStringPath SortedTree(IImmutableList<(string name, TreeWithStringPath component)> treeContent) =>
            Sort(new() { TreeContent = treeContent });

        static public TreeWithStringPath NonSortedTree(IImmutableList<(string name, TreeWithStringPath component)> treeContent) =>
            new() { TreeContent = treeContent };

        static public TreeWithStringPath EmptyTree => SortedTree(ImmutableList<(string name, TreeWithStringPath component)>.Empty);


        public IImmutableList<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> EnumerateBlobsTransitive()
        {
            if (BlobContent != null)
            {
                return ImmutableList.Create<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)>(
                    (ImmutableList<string>.Empty, BlobContent.Value));
            }

            return
                TreeContent!.SelectMany(treeEntry =>
                    treeEntry.component.EnumerateBlobsTransitive()
                    .Select(child => (child.path.Insert(0, treeEntry.name), child.blobContent)))
                .ToImmutableList();
        }

        public ReadOnlyMemory<byte>? GetBlobAtPath(IReadOnlyList<string> path) =>
            GetNodeAtPath(path)?.BlobContent;

        public TreeWithStringPath? GetNodeAtPath(IReadOnlyList<string> path)
        {
            if (path.Count == 0)
                return this;

            if (TreeContent == null)
                return null;

            var pathFirstElement = path[0];

            return
                TreeContent
                .Where(treeNode => treeNode.name == pathFirstElement)
                .Select(treeNode => treeNode.component.GetNodeAtPath(path.Skip(1).ToImmutableList()))
                .FirstOrDefault();
        }

        public TreeWithStringPath? RemoveNodeAtPath(IReadOnlyList<string> path)
        {
            if (path.Count == 0)
                return null;

            if (TreeContent == null)
                return null;

            var pathFirstElement = path[0];

            var treeContent =
                TreeContent.SelectMany(treeNode =>
                {
                    if (treeNode.name != pathFirstElement)
                        return ImmutableList.Create(treeNode);

                    if (path.Count == 1)
                        return ImmutableList<(string name, TreeWithStringPath component)>.Empty;

                    var componentAfterRemoval =
                        treeNode.component.RemoveNodeAtPath(path.Skip(1).ToImmutableArray());

                    if (componentAfterRemoval == null)
                        return ImmutableList<(string name, TreeWithStringPath component)>.Empty;

                    return ImmutableList.Create((treeNode.name, componentAfterRemoval));
                }).ToImmutableList();

            return SortedTree(treeContent);
        }

        public TreeWithStringPath SetNodeAtPathSorted(IReadOnlyList<string> path, TreeWithStringPath node)
        {
            if (path.Count == 0)
                return node;

            var pathFirstElement = path[0];

            var childNodeBefore = GetNodeAtPath(new[] { pathFirstElement });

            var childNode =
                (childNodeBefore ?? EmptyTree).SetNodeAtPathSorted(path.Skip(1).ToImmutableList(), node);

            var treeEntries =
                (TreeContent?.Where(treeNode => treeNode.name != pathFirstElement) ?? ImmutableList<(string name, TreeWithStringPath component)>.Empty)
                .Concat(new[] { (pathFirstElement, childNode) })
                .OrderBy(treeEntry => treeEntry.Item1, TreeEntryNameComparer)
                .ToImmutableList();

            return SortedTree(treeEntries);
        }

        static TreeWithStringPath Sort(TreeWithStringPath node) =>
            node.TreeContent == null
            ?
            node
            :
            new TreeWithStringPath
            {
                TreeContent = node.TreeContent.OrderBy(child => child.name).Select(child => (child.name, Sort(child.component))).ToImmutableList()
            };

        public virtual bool Equals(TreeWithStringPath? other)
        {
            if (other is null)
                return false;

            if (BlobContent != null || other.BlobContent != null)
            {
                if (BlobContent == null || other.BlobContent == null)
                    return false;

                return BlobContent.Value.Span.SequenceEqual(other.BlobContent.Value.Span);
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

                    return thisElement.name == otherElement.name &&
                        thisElement.component.Equals(otherElement.component);
                });
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(BlobContent, TreeContent);
        }
    }

    static public ParseAsTreeWithStringPathResult ParseAsTreeWithStringPath(Component composition)
    {
        if (composition.BlobContent != null)
        {
            return ParseAsTreeWithStringPathResult.ok(TreeWithStringPath.Blob(composition.BlobContent.Value));
        }

        if (composition.ListContent != null)
        {
            var compositionResults =
            composition.ListContent
            .Select((component, componentIndex) =>
            {
                if (!(component.ListContent?.Count == 2))
                {
                    return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)?>.err(
                        ImmutableList<(int index, string name)>.Empty);
                }

                var nameResult = StringFromComponent(component.ListContent.ElementAt(0));

                if (nameResult.Ok == null)
                {
                    return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)?>.err(
                        ImmutableList<(int index, string name)>.Empty);
                }

                var currentIndexAndName =
                    (index: componentIndex, name: nameResult.Ok);

                var parseResult = ParseAsTreeWithStringPath(component.ListContent.ElementAt(1));

                if (parseResult.Ok == null)
                {
                    return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)?>.err(
                        ImmutableList.Create(currentIndexAndName).AddRange(parseResult.Err!));
                }

                return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)?>.ok(
                    (name: currentIndexAndName.name, parseResult.Ok));
            })
            .ToImmutableList();

            var firstError =
                compositionResults
                .Select(componentResult => componentResult.Err)
                .Where(componentError => componentError != null)
                .FirstOrDefault();

            if (firstError != null)
                return ParseAsTreeWithStringPathResult.err(firstError);

            return
                ParseAsTreeWithStringPathResult.ok(
                    TreeWithStringPath.SortedTree(
                        treeContent: compositionResults.Select(compositionResult => compositionResult.Ok!.Value).ToImmutableList()));
        }

        throw new Exception("Incomplete match on sum type.");
    }

    static public Component FromTreeWithStringPath(TreeWithStringPath tree)
    {
        if (tree.BlobContent != null)
            return Component.Blob(tree.BlobContent.Value);

        if (tree.TreeContent != null)
        {
            var listContent =
                tree.TreeContent
                .Select(treeComponent =>
                    Component.List(
                        ImmutableList.Create(
                            ComponentFromString(treeComponent.name),
                            FromTreeWithStringPath(treeComponent.component))
                    ))
                .ToImmutableList();

            return Component.List(listContent);
        }

        throw new Exception("Incomplete match on sum type.");
    }

    static public TreeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
        IEnumerable<(string path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
            {
                var pathComponents =
                    blobWithPath.path.Split("/").SelectMany(pathComponent => pathComponent.Split(@"\"))
                    .ToImmutableList();

                return (path: (IImmutableList<string>)pathComponents, blobContent: blobWithPath.blobContent);
            })
        );

    static public TreeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
        IEnumerable<(IImmutableList<PathT> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath,
        Func<PathT, string> mapPathComponent) =>
        SortedTreeFromSetOfBlobs(
            blobsWithPath.Select(blobWithPath =>
                (path: (IImmutableList<string>)blobWithPath.path.Select(mapPathComponent).ToImmutableList(),
                blobContent: blobWithPath.blobContent)));

    static public TreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        SortedTreeFromSetOfBlobs(blobsWithPath, pathComponent => pathComponent);

    static public TreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
        IReadOnlyDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> blobsWithPath) =>
        SortedTreeFromSetOfBlobsWithStringPath(
            blobsWithPath.Select(pathAndBlobContent => (path: pathAndBlobContent.Key, blobContent: pathAndBlobContent.Value)));

    static public TreeWithStringPath SortedTreeFromTree(
        TreeWithStringPath tree) =>
        tree.BlobContent != null
        ?
        tree
        :
        SortedTreeFromSetOfBlobs(tree.EnumerateBlobsTransitive());

    static public TreeWithStringPath SortedTreeFromSetOfBlobs(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        TreeWithStringPath.SortedTree(treeContent: SortedTreeContentFromSetOfBlobs(blobsWithPath));

    static public IImmutableList<(string name, TreeWithStringPath obj)> SortedTreeContentFromSetOfBlobs(
        IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> blobsWithPath) =>
        blobsWithPath
        .Aggregate(
            (IImmutableList<(string name, TreeWithStringPath obj)>)
            ImmutableList<(string name, TreeWithStringPath obj)>.Empty,
            (intermediateResult, nextBlob) => SetBlobAtPathSorted(intermediateResult, nextBlob.path, nextBlob.blobContent));

    static public IImmutableList<(string name, TreeWithStringPath obj)> SetBlobAtPathSorted(
        IImmutableList<(string name, TreeWithStringPath obj)> treeContentBefore,
        IImmutableList<string> path,
        ReadOnlyMemory<byte> blobContent)
    {
        var pathFirstElement = path.First();

        var componentBefore =
            treeContentBefore.FirstOrDefault(c => c.name.SequenceEqual(pathFirstElement)).obj;

        var component =
            path.Count < 2
            ?
            TreeWithStringPath.Blob(blobContent: blobContent)
            :
            TreeWithStringPath.SortedTree(
                treeContent:
                    SetBlobAtPathSorted(
                        componentBefore?.TreeContent ?? ImmutableList<(string name, TreeWithStringPath obj)>.Empty,
                        path.RemoveAt(0),
                        blobContent));

        return
            treeContentBefore
            .RemoveAll(c => c.name == pathFirstElement)
            .Add((pathFirstElement, component))
            .OrderBy(c => c.name, TreeWithStringPath.TreeEntryNameComparer)
            .ToImmutableList();
    }

    static public Result<string, Component> Deserialize(
        ReadOnlyMemory<byte> serializedComponent,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>> loadSerializedComponentByHash) =>
        Deserialize(serializedComponent, loadSerializedComponentByHash);

    static public Result<string, Component> Deserialize(
        ReadOnlyMemory<byte> serializedComponent,
        Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?> loadSerializedComponentByHash)
    {
        var asciiStringUpToNull =
            System.Text.Encoding.ASCII.GetString(serializedComponent.Span).Split('\0').First();

        var asciiStringUpToFirstSpace =
            asciiStringUpToNull.Split(' ').First();

        if (asciiStringUpToFirstSpace == "blob")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var expectedCount = serializedComponent.Length - beginningToRemoveLength;

            var count = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            if (count != expectedCount)
                return Result<string, Component>.err("Unexpected count: got " + count + ", but I expected " + expectedCount);

            return Result<string, Component>.ok(Component.Blob(serializedComponent.Slice(beginningToRemoveLength)));
        }

        if (asciiStringUpToFirstSpace == "list")
        {
            var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

            var remainingBytes = serializedComponent.Slice(beginningToRemoveLength);

            var parsedElementCount = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

            var elementHashLength = 32;

            var expectedRemainingLength = parsedElementCount * elementHashLength;

            if (remainingBytes.Length != expectedRemainingLength)
                return Result<string, Component>.err(
                    "Unexpected remaining length: " + remainingBytes.Length + " instead of " + expectedRemainingLength);

            var elementsHashes =
                Enumerable.Range(0, parsedElementCount)
                .Select(elementIndex => remainingBytes.Slice(elementIndex * elementHashLength, elementHashLength))
                .ToImmutableList();

            Result<string, Component> TryLoadElementForHash(ReadOnlyMemory<byte> elementHash)
            {
                var loadedElementSerialRepresentation = loadSerializedComponentByHash(elementHash);

                if (loadedElementSerialRepresentation == null)
                    return Result<string, Component>.err(
                        "Failed to load list element " + CommonConversion.StringBase16(elementHash));

                if (!SHA256.HashData(loadedElementSerialRepresentation.Value.Span).AsSpan().SequenceEqual(elementHash.Span))
                    return Result<string, Component>.err(
                        "Hash for loaded element does not match " + CommonConversion.StringBase16(elementHash));

                return Deserialize(loadedElementSerialRepresentation.Value, loadSerializedComponentByHash);
            }

            var loadElementsResults =
                elementsHashes
                .Select(elementHash => (elementHash, loadResult: TryLoadElementForHash(elementHash)))
                .ToImmutableList();

            var firstFailed =
                loadElementsResults
                .FirstOrDefault(elementResult => elementResult.loadResult.Ok == null);

            if (firstFailed.loadResult != null)
                return Result<string, Component>.err(
                    "Failed to load element " + CommonConversion.StringBase16(firstFailed.elementHash) + ": " + firstFailed.loadResult.Err);

            return Result<string, Component>.ok(
                Component.List(loadElementsResults.Select(elementResult => elementResult.loadResult.Ok!).ToImmutableList()));
        }

        return Result<string, Component>.err("Invalid prefix: '" + asciiStringUpToFirstSpace + "'.");
    }

    static public ReadOnlyMemory<byte> GetSerialRepresentation(Component component) =>
        GetSerialRepresentationAndDependencies(component).serialRepresentation;

    static public (ReadOnlyMemory<byte> serialRepresentation, IReadOnlyCollection<Component> dependencies)
        GetSerialRepresentationAndDependencies(Component component)
    {
        if (component.BlobContent != null)
        {
            var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + component.BlobContent.Value.Length.ToString() + "\0");

            var serialRepresentation = new byte[prefix.Length + component.BlobContent.Value.Length];

            var componentBlobContentArray = component.BlobContent.Value.ToArray();

            Buffer.BlockCopy(prefix, 0, serialRepresentation, 0, prefix.Length);
            Buffer.BlockCopy(componentBlobContentArray, 0, serialRepresentation, prefix.Length, componentBlobContentArray.Length);

            return (serialRepresentation, ImmutableHashSet<Component>.Empty);
        }

        if (component.ListContent != null)
        {
            var componentsHashes =
                component.ListContent.Select(GetHash).ToList();

            var prefix = System.Text.Encoding.ASCII.GetBytes("list " + componentsHashes.Count.ToString() + "\0");

            return
                (serialRepresentation: CommonConversion.Concat(new[] { (ReadOnlyMemory<byte>)prefix }.Concat(componentsHashes).ToList()).ToArray(),
                dependencies: component.ListContent);
        }

        throw new System.Exception("Incomplete match on sum type.");
    }

    static public ReadOnlyMemory<byte> GetHash(Component component) =>
        GetHashAndDependencies(component).hash;

    static public (ReadOnlyMemory<byte> hash, IReadOnlyCollection<Component> dependencies)
        GetHashAndDependencies(Component component)
    {
        var (serialRepresentation, dependencies) = GetSerialRepresentationAndDependencies(component);

        return (hash: CommonConversion.HashSHA256(serialRepresentation), dependencies: dependencies);
    }

    static public ReadOnlyMemory<byte> GetHash(TreeWithStringPath component) =>
        CommonConversion.HashSHA256(GetSerialRepresentation(FromTreeWithStringPath(component)!));

    static public Component? FindComponentByHash(Component component, ReadOnlyMemory<byte> hash)
    {
        if (GetHash(component).Span.SequenceEqual(hash.Span))
            return component;

        if (component?.ListContent != null)
        {
            foreach (var item in component.ListContent)
            {
                var matchInItem = FindComponentByHash(item, hash);

                if (matchInItem != null)
                    return matchInItem;
            }
        }

        return null;
    }

    public record ParseAsTreeWithStringPathResult : Result<IImmutableList<(int index, string name)>, TreeWithStringPath>
    {
        protected ParseAsTreeWithStringPathResult(IImmutableList<(int index, string name)>? Err = null, TreeWithStringPath? Ok = null)
            :
            base(Err: Err, Ok: Ok)
        {
        }

        static public new ParseAsTreeWithStringPathResult err(IImmutableList<(int index, string name)> err) =>
            new(Err: err);

        static public new ParseAsTreeWithStringPathResult ok(TreeWithStringPath ok) =>
            new(Ok: ok);
    }

    public class ByteListComparer : IComparer<IReadOnlyList<byte>>, IEqualityComparer<IReadOnlyList<byte>>
    {
        public int Compare(IReadOnlyList<byte>? x, IReadOnlyList<byte>? y) =>
            CompareStatic(x, y);

        static public int CompareStatic(IReadOnlyList<byte>? x, IReadOnlyList<byte>? y)
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

        public bool Equals(IReadOnlyList<byte>? x, IReadOnlyList<byte>? y) =>
            Compare(x, y) == 0;

        public int GetHashCode(IReadOnlyList<byte> obj) => obj.Count;
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> TreeToFlatDictionaryWithPathComparer(
        TreeWithStringPath tree) =>
        ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> ToFlatDictionaryWithPathComparer(
        IEnumerable<(IImmutableList<string> filePath, ReadOnlyMemory<byte> fileContent)> filesBeforeSorting) =>
        filesBeforeSorting.ToImmutableSortedDictionary(
            entry => entry.filePath,
            entry => entry.fileContent,
            EnumerableExtension.Comparer<IImmutableList<string>>());
}
