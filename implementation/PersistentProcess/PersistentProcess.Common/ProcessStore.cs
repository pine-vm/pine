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

        public string[] AppendedEvents;
    }

    public class ReductionRecord
    {
        public byte[] ReducedCompositionHash;

        public string ReducedValue;
    }

    public interface IProcessStoreSink
    {
        byte[] AppendComposition(CompositionRecord composition);

        void StoreReduction(ReductionRecord reduction);
    }

    public interface IProcessStoreSource
    {
        IEnumerable<(byte[] hash, CompositionRecord composition)> EnumerateCompositionsReverse();

        ReductionRecord GetReduction(byte[] reducedCompositionHash);
    }

    public class ProcessStoreInFileDirectory : IProcessStoreSink, IProcessStoreSource
    {
        string directory;

        string CompositionFilePath => Path.Combine(directory, "composition");

        string ReductionDirectoryPath => Path.Combine(directory, "reduction");

        string ReductionFilePathFromReducedCompositionHash(byte[] hash) =>
            Path.Combine(ReductionDirectoryPath, BitConverter.ToString(hash).Replace("-", ""));

        static byte[] HashFromSerialRepresentation(byte[] serialized) =>
            new System.Security.Cryptography.SHA256Managed().ComputeHash(serialized);

        static string Serialize(CompositionRecord composition) =>
            JsonConvert.SerializeObject(composition);

        public ProcessStoreInFileDirectory(string directory)
        {
            this.directory = directory;
        }

        public IEnumerable<(byte[] hash, CompositionRecord composition)> EnumerateCompositionsReverse()
        {
            var recordsLinesReverse =
                File.Exists(CompositionFilePath) ?
                File.ReadAllLines(CompositionFilePath, Encoding.UTF8).Reverse() : new string[0];

            return
                recordsLinesReverse
                .Select(recordLine =>
                    (HashFromSerialRepresentation(Encoding.UTF8.GetBytes(recordLine)),
                    JsonConvert.DeserializeObject<CompositionRecord>(recordLine)));
        }

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

        public byte[] AppendComposition(CompositionRecord record)
        {
            var serializedRecord =
                Encoding.UTF8.GetBytes(Serialize(record));

            var filePath = CompositionFilePath;

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.AppendAllLines(CompositionFilePath, new[] { Encoding.UTF8.GetString(serializedRecord) });

            return HashFromSerialRepresentation(serializedRecord);
        }

        public void StoreReduction(ReductionRecord record)
        {
            var filePath = ReductionFilePathFromReducedCompositionHash(record.ReducedCompositionHash);

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));
            File.WriteAllText(filePath, JsonConvert.SerializeObject(record), Encoding.UTF8);
        }
    }
}
