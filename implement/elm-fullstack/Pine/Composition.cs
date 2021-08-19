using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine
{
    public class Composition
    {
        public record Component(byte[] BlobContent = null, IImmutableList<Component> ListContent = null)
        {
            static public Component Blob(IReadOnlyList<byte> blobContent) =>
                new(BlobContent: blobContent as byte[] ?? blobContent.ToArray());

            static public Component List(ImmutableList<Component> listContent) =>
                new(ListContent: listContent);

            public virtual bool Equals(Component other)
            {
                if (other == null)
                    return false;

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

            public override int GetHashCode()
            {
                return HashCode.Combine(BlobContent, ListContent);
            }
        }

        static public Component ComponentFromString(string str) =>
            str == null
            ?
            null
            :
            Component.List(ListValueFromString(str).ToImmutableList());

        static public IImmutableList<Component> ListValueFromString(string str) =>
            str == null
            ?
            null
            :
            ToCodePoints(str)
            .Select(charAsInteger => new System.Numerics.BigInteger(charAsInteger))
            .Select(ComponentFromUnsignedInteger)
            .Select(charValue => charValue.Ok).ToImmutableList();


        // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130
        static public int[] ToCodePoints(string str)
        {
            if (str == null)
                return null;

            var codePoints = new List<int>(str.Length);
            for (int i = 0; i < str.Length; i++)
            {
                codePoints.Add(Char.ConvertToUtf32(str, i));
                if (Char.IsHighSurrogate(str[i]))
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
                .Select(charValue => UnsignedIntegerFromComponent(charValue))
                .ToImmutableList();

            if (charsIntegersResults.Any(toIntResult => toIntResult.Ok == null))
                return Result<string, string>.err("Failed to map list elements to unsigned integers.");

            return Result<string, string>.ok(
                string.Join("", charsIntegersResults.Select(toIntResult => Char.ConvertFromUtf32((int)toIntResult.Ok))));
        }

        static public Result<string, Component> ComponentFromUnsignedInteger(System.Numerics.BigInteger integer) =>
            BlobValueFromUnsignedInteger(integer)
            .map(Component.Blob);

        static public Result<string, IReadOnlyList<byte>> BlobValueFromUnsignedInteger(System.Numerics.BigInteger integer)
        {
            var signedBlobValue = BlobValueFromSignedInteger(integer);

            if (signedBlobValue[0] != 0)
                return Result<string, IReadOnlyList<byte>>.err("Argument is a negative integer.");

            return Result<string, IReadOnlyList<byte>>.ok(signedBlobValue.Skip(1).ToArray());
        }

        static public Component ComponentFromSignedInteger(System.Numerics.BigInteger integer) =>
            Component.Blob(BlobValueFromSignedInteger(integer));

        static public IReadOnlyList<byte> BlobValueFromSignedInteger(System.Numerics.BigInteger integer)
        {
            var integerValue = System.Numerics.BigInteger.Abs(integer);

            var signByte =
                (byte)(integerValue == integer ? 0 : 0x80);

            return
                ImmutableList.Create(signByte)
                .AddRange(integerValue.ToByteArray(isUnsigned: true, isBigEndian: true));
        }

        static public Result<string, System.Numerics.BigInteger> SignedIntegerFromComponent(Component component)
        {
            if (component.BlobContent == null)
                return Result<string, System.Numerics.BigInteger>.err(
                    "Only a BlobValue can represent an integer.");

            return SignedIntegerFromBlobValue(component.BlobContent);
        }

        static public Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValue(byte[] blobValue)
        {
            if (blobValue.Length < 1)
                return Result<string, System.Numerics.BigInteger>.err(
                    "Empty blob is not a valid integer because the sign byte is missing. Did you mean to use an unsigned integer?");

            var signByte = blobValue[0];

            if (signByte != 0 && signByte != 0x80)
                return Result<string, System.Numerics.BigInteger>.err(
                    "Unexpected value for sign byte of integer: " + signByte);

            var isNegative = signByte != 0;

            var integerValue =
                UnsignedIntegerFromBlobValue(blobValue.AsSpan(1));

            return
                Result<string, System.Numerics.BigInteger>.ok(
                    integerValue * new System.Numerics.BigInteger(isNegative ? -1 : 1));
        }

        static public Result<string, System.Numerics.BigInteger> UnsignedIntegerFromComponent(Component component)
        {
            if (component.BlobContent == null)
                return Result<string, System.Numerics.BigInteger>.err(
                    "Only a BlobValue can represent an integer.");

            return Result<string, System.Numerics.BigInteger>.ok(
                UnsignedIntegerFromBlobValue(component.BlobContent));
        }

        static public System.Numerics.BigInteger UnsignedIntegerFromBlobValue(Span<byte> blobValue) =>
            new System.Numerics.BigInteger(blobValue.ToArray(), isUnsigned: true, isBigEndian: true);

        public class TreeWithStringPath : IEquatable<TreeWithStringPath>
        {
            static public readonly IComparer<string> TreeEntryNameComparer = StringComparer.Ordinal;

            public readonly byte[] BlobContent;

            public readonly IImmutableList<(string name, TreeWithStringPath component)> TreeContent;

            public TreeWithStringPath(byte[] blobContent)
            {
                BlobContent = blobContent;
            }

            public TreeWithStringPath(IImmutableList<(string name, TreeWithStringPath component)> treeContent)
            {
                TreeContent = treeContent;
            }

            static public TreeWithStringPath Blob(byte[] blobContent) =>
                new TreeWithStringPath(blobContent: blobContent);

            static public TreeWithStringPath Blob(IReadOnlyList<byte> blobContent) =>
                new TreeWithStringPath(blobContent: blobContent as byte[] ?? blobContent.ToArray());

            static public TreeWithStringPath Tree(IImmutableList<(string name, TreeWithStringPath component)> treeContent) =>
                new TreeWithStringPath(treeContent: treeContent);


            public IImmutableList<(IImmutableList<string> path, IReadOnlyList<byte> blobContent)> EnumerateBlobsTransitive()
            {
                if (TreeContent == null)
                {
                    return ImmutableList.Create<(IImmutableList<string> path, IReadOnlyList<byte> blobContent)>(
                        (ImmutableList<string>.Empty, BlobContent));
                }

                return
                    TreeContent.SelectMany(treeEntry =>
                        treeEntry.component.EnumerateBlobsTransitive()
                        .Select(child => (child.path.Insert(0, treeEntry.name), child.blobContent)))
                    .ToImmutableList();
            }

            public bool Equals(TreeWithStringPath other)
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

            override public bool Equals(object obj) => Equals(obj as TreeWithStringPath);

            public override int GetHashCode()
            {
                return HashCode.Combine(BlobContent, TreeContent);
            }
        }

        static public ParseAsTreeWithStringPathResult ParseAsTreeWithStringPath(
            Component composition)
        {
            if (composition == null)
                return null;

            if (composition.BlobContent != null)
            {
                return new ParseAsTreeWithStringPathResult
                {
                    Ok = new TreeWithStringPath(blobContent: composition.BlobContent)
                };
            }

            var compositionResults =
                composition.ListContent
                .Select((component, componentIndex) =>
                {
                    if (!(component.ListContent?.Count == 2))
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    var nameResult =
                        StringFromComponent(component.ListContent.ElementAt(0));

                    if (nameResult.Ok == null)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)>.err(
                            ImmutableList<(int index, string name)>.Empty);
                    }

                    var currentIndexAndName =
                        (index: componentIndex, name: nameResult.Ok);

                    var parseResult = ParseAsTreeWithStringPath(component.ListContent.ElementAt(1));

                    if (parseResult.Ok == null)
                    {
                        return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)>.err(
                            ImmutableList.Create(currentIndexAndName).AddRange(parseResult.Err));
                    }

                    return Result<IImmutableList<(int index, string name)>, (string name, TreeWithStringPath component)>.ok(
                        (name: currentIndexAndName.name, parseResult.Ok));
                })
                .ToImmutableList();

            var firstError =
                compositionResults
                .Select(componentResult => componentResult.Err)
                .Where(componentError => componentError != null)
                .FirstOrDefault();

            if (firstError != null)
                return new ParseAsTreeWithStringPathResult { Err = firstError };

            return
                new ParseAsTreeWithStringPathResult
                {
                    Ok = TreeWithStringPath.Tree(treeContent: compositionResults.Select(compositionResult => compositionResult.Ok).ToImmutableList())
                };
        }

        static public Component FromTreeWithStringPath(TreeWithStringPath tree)
        {
            if (tree == null)
                return null;

            if (tree.BlobContent != null)
                return Component.Blob(tree.BlobContent);

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

        static public TreeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
            IEnumerable<(string path, IReadOnlyList<byte> blobContent)> blobsWithPath) =>
            SortedTreeFromSetOfBlobs(
                blobsWithPath.Select(blobWithPath =>
                {
                    var pathComponents =
                        blobWithPath.path.Split("/").SelectMany(pathComponent => pathComponent.Split(@"\"))
                        .ToImmutableList();

                    return (path: (IImmutableList<string>)pathComponents, blobContent: blobWithPath.blobContent);
                })
            );

        static public TreeWithStringPath SortedTreeFromSetOfBlobsWithCommonFilePath(
            IEnumerable<(string path, byte[] blobContent)> blobsWithPath) =>
            SortedTreeFromSetOfBlobsWithCommonFilePath(
                blobsWithPath.Select(blobWithPath => (blobWithPath.path, (IReadOnlyList<byte>)blobWithPath.blobContent)));

        static public TreeWithStringPath SortedTreeFromSetOfBlobs<PathT>(
            IEnumerable<(IImmutableList<PathT> path, IReadOnlyList<byte> blobContent)> blobsWithPath,
            Func<PathT, string> mapPathComponent) =>
            SortedTreeFromSetOfBlobs(
                blobsWithPath.Select(blobWithPath =>
                    (path: (IImmutableList<string>)blobWithPath.path.Select(mapPathComponent).ToImmutableList(),
                    blobContent: blobWithPath.blobContent)));

        static public TreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
            IEnumerable<(IImmutableList<string> path, IReadOnlyList<byte> blobContent)> blobsWithPath) =>
            SortedTreeFromSetOfBlobs(blobsWithPath, pathComponent => pathComponent);

        static public TreeWithStringPath SortedTreeFromSetOfBlobsWithStringPath(
            IReadOnlyDictionary<IImmutableList<string>, IReadOnlyList<byte>> blobsWithPath) =>
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
            IEnumerable<(IImmutableList<string> path, IReadOnlyList<byte> blobContent)> blobsWithPath) =>
            TreeWithStringPath.Tree(treeContent: SortedTreeContentFromSetOfBlobs(blobsWithPath));

        static public IImmutableList<(string name, TreeWithStringPath obj)> SortedTreeContentFromSetOfBlobs(
            IEnumerable<(IImmutableList<string> path, IReadOnlyList<byte> blobContent)> blobsWithPath) =>
            blobsWithPath
            .Aggregate(
                (IImmutableList<(string name, TreeWithStringPath obj)>)
                ImmutableList<(string name, TreeWithStringPath obj)>.Empty,
                (intermediateResult, nextBlob) => SetBlobAtPathSorted(intermediateResult, nextBlob.path, nextBlob.blobContent));

        static public IImmutableList<(string name, TreeWithStringPath obj)> SetBlobAtPathSorted(
            IImmutableList<(string name, TreeWithStringPath obj)> treeContentBefore,
            IImmutableList<string> path,
            IReadOnlyList<byte> blobContent)
        {
            var pathFirstElement = path.First();

            var componentBefore =
                treeContentBefore.FirstOrDefault(c => c.name.SequenceEqual(pathFirstElement)).obj;

            var component =
                path.Count < 2
                ?
                TreeWithStringPath.Blob(blobContent: blobContent)
                :
                TreeWithStringPath.Tree(
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
            byte[] serializedComponent,
            Func<IReadOnlyList<byte>, IReadOnlyList<byte>> loadSerializedComponentByHash) =>
            Deserialize(serializedComponent.ToImmutableList(), loadSerializedComponentByHash);

        static public Result<string, Component> Deserialize(
            IReadOnlyList<byte> serializedComponent,
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

                return Result<string, Component>.ok(Component.Blob(serializedComponent.Skip(beginningToRemoveLength).ToList()));
            }

            if (asciiStringUpToFirstSpace == "list")
            {
                var beginningToRemoveLength = asciiStringUpToNull.Length + 1;

                var remainingBytes = serializedComponent.Skip(beginningToRemoveLength).ToList();

                var parsedElementCount = int.Parse(asciiStringUpToNull.Split(' ').ElementAt(1));

                var elementHashLength = 32;

                var expectedRemainingLength = parsedElementCount * elementHashLength;

                if (remainingBytes.Count != expectedRemainingLength)
                    return Result<string, Component>.err(
                        "Unexpected remaining length: " + remainingBytes.Count + " instead of " + expectedRemainingLength);

                var elementsHashes =
                    Enumerable.Range(0, parsedElementCount)
                    .Select(elementIndex => (IReadOnlyList<byte>)remainingBytes.Skip(elementIndex * elementHashLength).Take(elementHashLength).ToList())
                    .ToImmutableList();

                Result<string, Component> TryLoadElementForHash(IReadOnlyList<byte> elementHash)
                {
                    var loadedElementSerialRepresentation = loadSerializedComponentByHash(elementHash);

                    if (loadedElementSerialRepresentation == null)
                        return Result<string, Component>.err(
                            "Failed to load list element " + CommonConversion.StringBase16FromByteArray(elementHash));

                    if (!CommonConversion.HashSHA256(loadedElementSerialRepresentation.ToArray()).SequenceEqual(elementHash))
                        return Result<string, Component>.err(
                            "Hash for loaded element does not match " + CommonConversion.StringBase16FromByteArray(elementHash));

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
                        "Failed to load element " + CommonConversion.StringBase16FromByteArray(firstFailed.elementHash) + ": " + firstFailed.loadResult.Err);

                return Result<string, Component>.ok(
                    Component.List(loadElementsResults.Select(elementResult => elementResult.loadResult.Ok).ToImmutableList()));
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
                var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + component.BlobContent.Length.ToString() + "\0");

                var serialRepresentation = new byte[prefix.Length + component.BlobContent.Length];

                var componentBlobContentArray = component.BlobContent.ToArray();

                Buffer.BlockCopy(prefix, 0, serialRepresentation, 0, prefix.Length);
                Buffer.BlockCopy(componentBlobContentArray, 0, serialRepresentation, prefix.Length, componentBlobContentArray.Length);

                return (serialRepresentation, ImmutableHashSet<Component>.Empty);
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

        static public byte[] GetHash(TreeWithStringPath component) =>
            CommonConversion.HashSHA256(GetSerialRepresentation(FromTreeWithStringPath(component)));

        static public Component FindComponentByHash(Component component, byte[] hash)
        {
            if (GetHash(component).SequenceEqual(hash))
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

        public record Result<ErrT, OkT>(ErrT Err = default, OkT Ok = default)
        {
            static public Result<ErrT, OkT> err(ErrT err) =>
                new() { Err = err };

            static public Result<ErrT, OkT> ok(OkT ok) =>
                new() { Ok = ok };

            public Result<ErrT, MappedOkT> map<MappedOkT>(Func<OkT, MappedOkT> okMap)
            {
                if (Ok == null)
                    return Result<ErrT, MappedOkT>.err(Err);

                return Result<ErrT, MappedOkT>.ok(okMap(Ok));
            }
        }

        public record ParseAsTreeWithStringPathResult : Result<IImmutableList<(int index, string name)>, TreeWithStringPath>
        {
        }

        public class ByteListComparer : IComparer<IReadOnlyList<byte>>, IEqualityComparer<IReadOnlyList<byte>>
        {
            public int Compare(IReadOnlyList<byte> x, IReadOnlyList<byte> y) =>
                CompareStatic(x, y);

            static public int CompareStatic(IReadOnlyList<byte> x, IReadOnlyList<byte> y)
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

        static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> TreeToFlatDictionaryWithPathComparer(
            TreeWithStringPath tree) =>
            ToFlatDictionaryWithPathComparer(tree.EnumerateBlobsTransitive());

        static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> ToFlatDictionaryWithPathComparer(
            IEnumerable<(IImmutableList<string> filePath, IReadOnlyList<byte> fileContent)> filesBeforeSorting) =>
            filesBeforeSorting.ToImmutableSortedDictionary(
                entry => entry.filePath,
                entry => entry.fileContent,
                EnumerableExtension.Comparer<IImmutableList<string>>());
    }
}