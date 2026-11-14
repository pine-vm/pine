using Pine.Core.Elm;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Pine.Elm019;

/// <summary>
/// The Data.Binary format used by Elm 0.19.1's Deps.Registry, Elm.Package and Elm.Version.
/// This deliberately stores a subset registry, not a live all-packages update cursor.
/// </summary>
public static class Elm019Registry
{
    public static IReadOnlyList<ElmPackageIdentity> Read(byte[] bytes)
    {
        using var stream = new MemoryStream(bytes, writable: false);
        using var reader = new BinaryReader(stream, new UTF8Encoding(false, true));

        long ReadCount()
        {
            var count = BinaryPrimitives.ReadInt64BigEndian(ReadExactly(8));

            if (count < 0 || count > bytes.Length)
                throw new InvalidDataException("Invalid Elm registry count: " + count);

            return count;
        }

        byte[] ReadExactly(int count)
        {
            var value = reader.ReadBytes(count);

            if (value.Length != count)
                throw new InvalidDataException("Truncated Elm 0.19.1 registry.");

            return value;
        }

        string ReadName() => Encoding.UTF8.GetString(ReadExactly(reader.ReadByte()));

        ElmPackageVersion ReadVersion()
        {
            var major = reader.ReadByte();

            if (major != 255)
                return new(major, reader.ReadByte(), reader.ReadByte());

            return
                new(
                    BinaryPrimitives.ReadUInt16BigEndian(ReadExactly(2)),
                    BinaryPrimitives.ReadUInt16BigEndian(ReadExactly(2)),
                    BinaryPrimitives.ReadUInt16BigEndian(ReadExactly(2)));
        }

        try
        {
            var versionCount = ReadCount();
            var packageCount = ReadCount();
            var packages = new List<ElmPackageIdentity>();
            string? previousAuthor = null;
            string? previousProject = null;

            for (long index = 0; index < packageCount; index++)
            {
                var author = ReadName();
                var project = ReadName();
                var name = author + "/" + project;
                ElmDependencyResolver.ValidatePackageName(name);

                if (previousAuthor is not null &&
                    (StringComparer.Ordinal.Compare(previousAuthor, author) > 0 ||
                    previousAuthor == author && StringComparer.Ordinal.Compare(previousProject, project) >= 0))
                {
                    throw new InvalidDataException("Elm registry package names are not strictly ordered.");
                }

                previousAuthor = author;
                previousProject = project;
                var newest = ReadVersion();
                packages.Add(new(name, newest));
                var olderCount = ReadCount();
                var previousVersion = newest;

                for (long older = 0; older < olderCount; older++)
                {
                    var version = ReadVersion();

                    if (previousVersion.CompareTo(version) <= 0)
                        throw new InvalidDataException("Elm registry versions are not strictly descending for " + name);

                    packages.Add(new(name, version));
                    previousVersion = version;
                }
            }

            if (packages.Count != versionCount || stream.Position != stream.Length)
                throw new InvalidDataException("Elm registry count or trailing bytes do not match its contents.");

            return packages;
        }
        catch (EndOfStreamException exception)
        {
            throw new InvalidDataException("Truncated Elm 0.19.1 registry.", exception);
        }
    }

    public static byte[] Write(IEnumerable<ElmPackageIdentity> packages)
    {
        var distinct = packages.Distinct().ToArray();

        foreach (var package in distinct)
            ElmDependencyResolver.ValidatePackageName(package.Name);

        var groups =
            distinct.GroupBy(package => package.Name)
            .OrderBy(group => group.Key.Split('/')[0], StringComparer.Ordinal)
            .ThenBy(group => group.Key.Split('/')[1], StringComparer.Ordinal)
            .ToArray();

        using var stream = new MemoryStream();

        void WriteCount(long count)
        {
            Span<byte> bytes = stackalloc byte[8];
            BinaryPrimitives.WriteInt64BigEndian(bytes, count);
            stream.Write(bytes);
        }

        void WriteVersion(ElmPackageVersion version)
        {
            if (version.Major < 0 || version.Minor < 0 || version.Patch < 0 ||
                version.Major > ushort.MaxValue || version.Minor > ushort.MaxValue || version.Patch > ushort.MaxValue)
            {
                throw new ArgumentOutOfRangeException(nameof(packages), "Elm 0.19.1 version components must fit in UInt16.");
            }

            if (version.Major < 255 && version.Minor < 256 && version.Patch < 256)
            {
                stream.Write([(byte)version.Major, (byte)version.Minor, (byte)version.Patch]);
                return;
            }

            Span<byte> bytes = stackalloc byte[7];
            bytes[0] = 255;
            BinaryPrimitives.WriteUInt16BigEndian(bytes[1..], (ushort)version.Major);
            BinaryPrimitives.WriteUInt16BigEndian(bytes[3..], (ushort)version.Minor);
            BinaryPrimitives.WriteUInt16BigEndian(bytes[5..], (ushort)version.Patch);
            stream.Write(bytes);
        }

        WriteCount(distinct.Length);
        WriteCount(groups.Length);

        foreach (var group in groups)
        {
            foreach (var name in group.Key.Split('/'))
            {
                var bytes = Encoding.UTF8.GetBytes(name);

                if (bytes.Length is 0 or > 255)
                    throw new ArgumentException("Elm registry name segments must contain 1 to 255 UTF-8 bytes.");

                stream.WriteByte((byte)bytes.Length);
                stream.Write(bytes);
            }

            var versions = group.Select(package => package.Version).OrderDescending().ToArray();
            WriteVersion(versions[0]);
            WriteCount(versions.Length - 1);

            foreach (var version in versions.Skip(1))
                WriteVersion(version);
        }

        return stream.ToArray();
    }
}
