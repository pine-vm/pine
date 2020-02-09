using System;
using System.Collections.Immutable;
using System.Linq;

namespace Kalmit
{
    public class Composition
    {
        public class Component : IEquatable<Component>
        {
            public IImmutableList<byte> Literal;

            public IImmutableList<Component> Composition;

            public bool Equals(Component other)
            {
                if (Literal != null || other.Literal != null)
                {
                    if (Literal == null || other.Literal == null)
                        return false;

                    return Literal.SequenceEqual(other.Literal);
                }

                if (Composition == null || other.Composition == null)
                    return false;

                if (Composition.Count != other.Composition.Count)
                    return false;

                return
                    Enumerable.Range(0, Composition.Count)
                    .All(i =>
                    {
                        var thisElement = Composition.ElementAt(i);
                        var otherElement = other.Composition.ElementAt(i);

                        return thisElement.Equals(otherElement);
                    });
            }

            override public bool Equals(object obj) => Equals(obj as Component);
        }

        public class TreeComponent : IEquatable<TreeComponent>
        {
            public IImmutableList<byte> Literal;

            public IImmutableList<(IImmutableList<byte> name, TreeComponent component)> Composition;

            public bool Equals(TreeComponent other)
            {
                if (Literal != null || other.Literal != null)
                {
                    if (Literal == null || other.Literal == null)
                        return false;

                    return Literal.SequenceEqual(other.Literal);
                }

                if (Composition == null || other.Composition == null)
                    return false;

                if (Composition.Count != other.Composition.Count)
                    return false;

                return
                    Enumerable.Range(0, Composition.Count)
                    .All(i =>
                    {
                        var thisElement = Composition.ElementAt(i);
                        var otherElement = other.Composition.ElementAt(i);

                        return thisElement.name.SequenceEqual(otherElement.name) &&
                            thisElement.component.Equals(otherElement.component);
                    });
            }

            override public bool Equals(object obj) => Equals(obj as TreeComponent);
        }

        static public ParseAsTreeResult ParseAsTree(
            Component composition)
        {
            if (composition == null)
                return null;

            if (composition.Literal != null)
            {
                return new ParseAsTreeResult
                {
                    ok = new TreeComponent { Literal = composition.Literal }
                };
            }

            var compositionResults =
                composition.Composition
                .Select((component, componentIndex) =>
                {
                    if (!(component.Composition?.Count == 2) || component.Composition.ElementAt(0).Literal == null)
                    {
                        return new Result<IImmutableList<(int index, IImmutableList<byte> name)>, (IImmutableList<byte> name, TreeComponent component)>
                        {
                            err = ImmutableList<(int index, IImmutableList<byte> name)>.Empty
                        };
                    }

                    var currentIndexAndName = (index: componentIndex, name: component.Composition.ElementAt(0).Literal);

                    var parseResult = ParseAsTree(component.Composition.ElementAt(1));

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
                    { Composition = compositionResults.Select(compositionResult => compositionResult.ok).ToImmutableList() }
                };
        }

        static public Component FromTree(TreeComponent tree)
        {
            if (tree == null)
                return null;

            if (tree.Literal != null)
                return new Component { Literal = tree.Literal };

            var composition =
                tree.Composition
                .Select(treeComponent =>
                    new Component
                    {
                        Composition = ImmutableList.Create(
                            new Component { Literal = treeComponent.name },
                            FromTree(treeComponent.component))
                    })
                .ToImmutableList();

            return new Component
            {
                Composition = composition
            };
        }

        static public byte[] GetHash(Component component)
        {
            if (component.Literal != null)
            {
                var prefix = System.Text.Encoding.ASCII.GetBytes("blob " + component.Literal.Count.ToString() + "\0");

                return CommonConversion.HashSHA256(prefix.Concat(component.Literal).ToArray());
            }

            {
                var componentsHashes =
                    component.Composition.Select(GetHash).ToList();

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
        }

        public class ParseAsTreeResult : Result<IImmutableList<(int index, IImmutableList<byte> name)>, TreeComponent>
        {
        }
    }
}