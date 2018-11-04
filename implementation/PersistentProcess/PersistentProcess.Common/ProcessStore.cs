using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Kalmit.ProcessStore
{
    public class CompositionRecord
    {
        public byte[] ParentHash;

        public IReadOnlyList<string> AppendedEvents;

        static public byte[] HashFromSerialRepresentation(byte[] serialized) =>
            new System.Security.Cryptography.SHA256Managed().ComputeHash(serialized);
    }

    public class ReductionRecord
    {
        public byte[] ReducedCompositionHash;

        public string ReducedValue;
    }

    public interface IProcessStoreWriter
    {
        void AppendSerializedCompositionRecord(byte[] serializedCompositionRecord);

        void StoreReduction(ReductionRecord reduction);
    }

    public interface IProcessStoreReader
    {
        IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse();

        ReductionRecord GetReduction(byte[] reducedCompositionHash);
    }

    public class ProcessStoreInFileDirectory : IProcessStoreWriter, IProcessStoreReader
    {
        string directory;

        string CompositionFilePath => Path.Combine(directory, "composition");

        string ReductionDirectoryPath => Path.Combine(directory, "reduction");

        static public string ReductionFileNameFromReducedCompositionHash(byte[] hash) =>
            BitConverter.ToString(hash).Replace("-", "");

        string ReductionFilePathFromReducedCompositionHash(byte[] hash) =>
            Path.Combine(ReductionDirectoryPath, ReductionFileNameFromReducedCompositionHash(hash));

        public ProcessStoreInFileDirectory(string directory)
        {
            this.directory = directory;
        }

        public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
            (File.Exists(CompositionFilePath) ?
            File.ReadAllLines(CompositionFilePath, Encoding.UTF8).Reverse() : new string[0])
            .Select(Encoding.UTF8.GetBytes);

        public ReductionRecord GetReduction(byte[] reducedCompositionHash)
        {
            var filePath = ReductionFilePathFromReducedCompositionHash(reducedCompositionHash);

            if (!File.Exists(filePath))
                return null;

            var reduction =
                JsonConvert.DeserializeObject<ReductionRecord>(
                    File.ReadAllText(filePath, Encoding.UTF8));

            if (!reducedCompositionHash.SequenceEqual(reduction.ReducedCompositionHash))
                throw new Exception("Unexpected content in file " + filePath + ", hash does not match");

            return reduction;
        }

        public void AppendSerializedCompositionRecord(byte[] record)
        {
            var filePath = CompositionFilePath;

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.AppendAllLines(CompositionFilePath, new[] { Encoding.UTF8.GetString(record) });
        }

        public void StoreReduction(ReductionRecord record)
        {
            var filePath = ReductionFilePathFromReducedCompositionHash(record.ReducedCompositionHash);

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.WriteAllText(filePath, JsonConvert.SerializeObject(record), Encoding.UTF8);
        }

        public IEnumerable<string> ReductionsFilePaths() =>
            Directory.Exists(ReductionDirectoryPath) ?
            Directory.EnumerateFiles(ReductionDirectoryPath, "*", SearchOption.AllDirectories) :
            Array.Empty<string>();
    }
}
