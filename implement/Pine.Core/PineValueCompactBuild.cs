using System.Collections.Generic;
using System.Linq;

namespace Pine.Core;

public class PineValueCompactBuild
{

    public record ListEntry(
        string Key,
        ListEntryValue Value);

    public record ListEntryValue(
        string? BlobBytesBase64,
        IReadOnlyList<string>? ListItemsKeys);


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

                for (int i = 0; i < items.Length; ++i)
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

    public static (IReadOnlyList<ListEntry> listEntries,
        System.Func<IReadOnlyList<PineValue>, ListEntryValue> entryListFromItems)
        PrebuildListEntriesAllFromRoot(PineValue root) =>
        PrebuildListEntriesAllFromRoots(new HashSet<PineValue> { root });

    public static
        (IReadOnlyList<ListEntry> listEntries,
        System.Func<IReadOnlyList<PineValue>, ListEntryValue> entryValueFromListItems)
        PrebuildListEntriesAllFromRoots(IReadOnlySet<PineValue> roots)
    {
        var (allLists, allBlobs) = PineValue.CollectAllComponentsFromRoots(roots);

        return PrebuildListEntries(allBlobs, allLists);
    }

    public static
        (IReadOnlyList<ListEntry> listEntries,
        System.Func<IReadOnlyList<PineValue>, ListEntryValue> entryValueFromListItems)
        PrebuildListEntries(
        IReadOnlySet<PineValue.BlobValue> blobValues,
        IReadOnlySet<PineValue.ListValue> listValues)
    {
        var mutatedBlobsDict = new Dictionary<PineValue.BlobValue, string>();

        var blobEntriesList =
            blobValues
            .OrderBy(blob => blob.Bytes.Length)
            .Select((blobValue, blobIndex) =>
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
            IReadOnlyList<PineValue> itemValues)
        {
            var itemsIds = itemValues.Select(itemId).ToArray();

            return new ListEntryValue(
                BlobBytesBase64: null,
                ListItemsKeys: itemsIds);
        }

        var listEntriesList =
            listsOrdered
            .Select((listInstance, index) =>
            {
                var entryKey = "list-" + index.ToString();

                mutatedListDict[listInstance] = entryKey;

                return
                    new ListEntry(
                        Key: entryKey,
                        Value: entryValueFromListItems(listInstance.Elements));
            })
            .ToList();

        return
            ([..blobEntriesList
            ,..listEntriesList],
            entryValueFromListItems);
    }
}
