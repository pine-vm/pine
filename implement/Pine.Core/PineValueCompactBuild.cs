using System.Collections.Generic;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// Builds a compact table representation for Pine values and reconstructs values from that representation.
/// </summary>
public class PineValueCompactBuild
{

    /// <summary>
    /// Represents one named value entry in the compact Pine value table.
    /// </summary>
    public record ListEntry
    {
        /// <summary>
        /// Gets the stable key used to reference this entry from other compact entries.
        /// </summary>
        public string Key { get; init; }

        /// <summary>
        /// Gets the compact payload used to rebuild the Pine value for this entry.
        /// </summary>
        public ListEntryValue Value { get; init; }

        /// <summary>
        /// Constructs a compact table entry from its stable key and its compact payload.
        /// </summary>
        public ListEntry(
            string Key,
            ListEntryValue Value)
        {
            this.Key = Key;
            this.Value = Value;
        }

        /// <summary>
        /// Deconstructs this entry into its stable key and its compact payload.
        /// </summary>
        public void Deconstruct(
            out string Key,
            out ListEntryValue Value)
        {
            Key = this.Key;
            Value = this.Value;
        }
    }

    /// <summary>
    /// Represents either base64-encoded blob bytes or references to child entries for a list value.
    /// </summary>
    public record ListEntryValue
    {
        /// <summary>
        /// Gets the base64-encoded bytes when this entry stores a blob value.
        /// </summary>
        public string? BlobBytesBase64 { get; init; }

        /// <summary>
        /// Gets the referenced entry keys when this entry stores a list value.
        /// </summary>
        public IReadOnlyList<string>? ListItemsKeys { get; init; }

        /// <summary>
        /// Constructs a compact payload from either blob bytes or a list of referenced entry keys.
        /// </summary>
        public ListEntryValue(
            string? BlobBytesBase64,
            IReadOnlyList<string>? ListItemsKeys)
        {
            this.BlobBytesBase64 = BlobBytesBase64;
            this.ListItemsKeys = ListItemsKeys;
        }

        /// <summary>
        /// Deconstructs this payload into its blob bytes and its list of referenced entry keys.
        /// </summary>
        public void Deconstruct(
            out string? BlobBytesBase64,
            out IReadOnlyList<string>? ListItemsKeys)
        {
            BlobBytesBase64 = this.BlobBytesBase64;
            ListItemsKeys = this.ListItemsKeys;
        }
    }


    /// <summary>
    /// Reconstructs Pine values from compact entries whose referenced children appear earlier in the sequence.
    /// </summary>
    public static IReadOnlyDictionary<string, PineValue>
        BuildDictionaryFromEntries(
        IReadOnlyList<ListEntry> entries)
    {
        var mutatedDict = new Dictionary<string, PineValue>();

        PineValue contructValue(ListEntryValue entryValue)
        {
            if (entryValue.BlobBytesBase64 is { } bytesBase64)
            {
                var bytes = System.Convert.FromBase64String(bytesBase64);

                return PineValue.Blob(bytes);
            }

            if (entryValue.ListItemsKeys is { } listItemsKeys)
            {
                var items = new PineValue[listItemsKeys.Count];

                for (var i = 0; i < items.Length; ++i)
                {
                    items[i] = mutatedDict[listItemsKeys[i]];
                }

                return PineValue.List(items);
            }

            throw new System.NotImplementedException(
                "Unexpected entry type: " + entryValue.GetType());
        }

        for (var i = 0; i < entries.Count; ++i)
        {
            var entry = entries[i];

            mutatedDict[entry.Key] = contructValue(entry.Value);
        }

        return mutatedDict;
    }

    /// <summary>
    /// Collects every blob and list reachable from a root value and returns their compact entries.
    /// </summary>
    public static (IReadOnlyList<ListEntry> listEntries,
        System.Func<System.ReadOnlyMemory<PineValue>, ListEntryValue> entryListFromItems)
        PrebuildListEntriesAllFromRoot(PineValue root) =>
        PrebuildListEntriesAllFromRoots(new HashSet<PineValue> { root });

    /// <summary>
    /// Collects every blob and list reachable from the supplied roots and returns their compact entries.
    /// </summary>
    public static
        (IReadOnlyList<ListEntry> listEntries,
        System.Func<System.ReadOnlyMemory<PineValue>, ListEntryValue> entryValueFromListItems)
        PrebuildListEntriesAllFromRoots(IReadOnlySet<PineValue> roots)
    {
        var (allLists, allBlobs) = PineValue.CollectAllComponentsFromRoots(roots);

        return PrebuildListEntries(allBlobs, allLists);
    }

    /// <summary>
    /// Creates compact entries for the supplied blobs and lists, ordering them so list references only point to known entry keys.
    /// </summary>
    public static
        (IReadOnlyList<ListEntry> listEntries,
        System.Func<System.ReadOnlyMemory<PineValue>, ListEntryValue> entryValueFromListItems)
        PrebuildListEntries(
        IReadOnlySet<PineValue.BlobValue> blobValues,
        IReadOnlySet<PineValue.ListValue> listValues)
    {
        var mutatedBlobsDict = new Dictionary<PineValue.BlobValue, string>();

        var blobEntriesList =
            blobValues
            .OrderBy(blob => blob.Bytes.Length)
            .Select(
                (blobValue, blobIndex) =>
                {
                    var entryKey = "blob-" + blobIndex.ToString();

                    mutatedBlobsDict[blobValue] = entryKey;

                    return
                        new ListEntry(
                            Key: entryKey,
                            new ListEntryValue(
                                BlobBytesBase64: System.Convert.ToBase64String(blobValue.Bytes.Span),
                                ListItemsKeys: null));
                })
            .ToList();

        var listsOrdered =
            listValues
            .OrderBy(l => l.NodesCount)
            .ToList();

        var mutatedListDict = new Dictionary<PineValue.ListValue, string>();

        string itemId(PineValue itemValue)
        {
            if (itemValue is PineValue.BlobValue itemBlob)
                return mutatedBlobsDict[itemBlob];

            if (itemValue is PineValue.ListValue itemList)
                return mutatedListDict[itemList];

            throw new System.NotImplementedException(
                "Unexpected item value type: " + itemValue.GetType());
        }

        ListEntryValue entryValueFromListItems(
            System.ReadOnlyMemory<PineValue> itemValues)
        {
            var itemsIds = new string[itemValues.Length];

            for (var i = 0; i < itemValues.Length; ++i)
            {
                itemsIds[i] = itemId(itemValues.Span[i]);
            }

            return
                new ListEntryValue(
                    BlobBytesBase64: null,
                    ListItemsKeys: itemsIds);
        }

        var listEntriesList =
            listsOrdered
            .Select(
                (listInstance, index) =>
                {
                    var entryKey = "list-" + index.ToString();

                    mutatedListDict[listInstance] = entryKey;

                    return
                        new ListEntry(
                            Key: entryKey,
                            Value: entryValueFromListItems(listInstance.Items));
                })
            .ToList();

        return
            ([
            ..blobEntriesList,
            ..listEntriesList
            ],
            entryValueFromListItems);
    }
}
