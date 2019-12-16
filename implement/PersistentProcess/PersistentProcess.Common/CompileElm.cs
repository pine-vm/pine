using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public class CompileElm
    {
        class ModuleExposeSyntax
        {
            public string everything;

            public IImmutableList<ModuleExposeSyntaxNode> something;

            override public string ToString() =>
                everything ?? string.Join("", something.Select(node => node.ToString()));

            public class ModuleExposeSyntaxNode
            {
                public string otherTokens;

                public ModuleExposeValue exposeValue;

                override public string ToString() =>
                    otherTokens ?? exposeValue.ToString();
            }

            public class ModuleExposeValue
            {
                public string name;

                public string tokensBetweenNameAndDetails;

                public ModuleExposeValueSubvalues subvalues;

                public string tokensAfterDetails;

                override public string ToString() =>
                    name + tokensBetweenNameAndDetails + subvalues.ToString() + tokensAfterDetails;
            }

            public class ModuleExposeValueSubvalues
            {
                public string everything;

                public string nothing;

                public string something;

                override public string ToString() =>
                    everything ?? nothing ?? something;
            }
        }

        static (int beginIndex, int parsedLength, ModuleExposeSyntax parsedSyntax)? FindAndParseModuleExposeSyntax(string moduleText)
        {
            var startMatch = Regex.Match(moduleText, @"^module\s+[\w\d\._]+\s+exposing\s*\(", RegexOptions.Multiline);

            if (!startMatch.Success)
                return null;

            var parseStartLocation = startMatch.Index + startMatch.Length;

            var startRemainingText = moduleText.Substring(parseStartLocation);
            var remainingText = startRemainingText;

            var everythingMatch = Regex.Match(remainingText, @"^\s*\.\.\s*");

            if (everythingMatch.Success)
                return (parseStartLocation, everythingMatch.Length, new ModuleExposeSyntax { everything = everythingMatch.Value });

            var something = new List<ModuleExposeSyntax.ModuleExposeSyntaxNode>();

            while (0 < remainingText?.Length)
            {
                var nextSymbolBeginOrEndMatch = Regex.Match(remainingText, @"[\w\d_\)]");

                var otherTokensLength =
                    nextSymbolBeginOrEndMatch.Success ? nextSymbolBeginOrEndMatch.Index : remainingText.Length;

                something.Add(new ModuleExposeSyntax.ModuleExposeSyntaxNode { otherTokens = remainingText.Substring(0, otherTokensLength) });

                remainingText = remainingText.Substring(otherTokensLength);

                if (!nextSymbolBeginOrEndMatch.Success || nextSymbolBeginOrEndMatch.Value.StartsWith(")"))
                    break;

                var nameMatch = Regex.Match(remainingText, @"^[\w\d_]+");

                remainingText = remainingText.Substring(nameMatch.Length);

                var detailsMatch = Regex.Match(remainingText, @"^\s*\(");

                string tokensBetweenNameAndDetails = "";
                var subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { nothing = "" };
                string tokensAfterDetails = "";

                if (detailsMatch.Success)
                {
                    tokensBetweenNameAndDetails = remainingText.Substring(0, detailsMatch.Length);
                    remainingText = remainingText.Substring(detailsMatch.Length);
                    var restMatch = Regex.Match(remainingText, @"([^\)]*)\)");

                    if (restMatch.Groups[1].Value.Trim().Equals(".."))
                        subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { everything = restMatch.Groups[1].Value };
                    else
                        subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { something = restMatch.Groups[1].Value };

                    tokensAfterDetails = ")";

                    remainingText = remainingText.Substring(restMatch.Length);
                }

                something.Add(new ModuleExposeSyntax.ModuleExposeSyntaxNode
                {
                    exposeValue = new ModuleExposeSyntax.ModuleExposeValue
                    {
                        name = nameMatch.Value,
                        tokensBetweenNameAndDetails = tokensBetweenNameAndDetails,
                        subvalues = subvalues,
                        tokensAfterDetails = tokensAfterDetails,
                    },
                });
            }

            return (parseStartLocation, startRemainingText.Length - remainingText.Length, new ModuleExposeSyntax { something = something.ToImmutableList() });
        }

        static public string ExposeValueInElmModule(string originalElmModuleText, string nameToExpose)
        {
            return AdaptModuleExposeSyntax(originalElmModuleText, originalExposeSyntax =>
            {
                if (originalExposeSyntax.everything != null)
                    return originalExposeSyntax;

                var nodesToAppend =
                    originalExposeSyntax.something.Any(node => node?.exposeValue?.name == nameToExpose)
                    ?
                    new ModuleExposeSyntax.ModuleExposeSyntaxNode[] { }
                    :
                    new ModuleExposeSyntax.ModuleExposeSyntaxNode[]{
                        new ModuleExposeSyntax.ModuleExposeSyntaxNode
                        {
                            otherTokens = ", ",
                        },
                        new ModuleExposeSyntax.ModuleExposeSyntaxNode
                        {
                            exposeValue =
                                new ModuleExposeSyntax.ModuleExposeValue
                                {
                                    name = nameToExpose,
                                    tokensBetweenNameAndDetails = "",
                                    subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { nothing = "" },
                                    tokensAfterDetails = ""
                                }
                        }
                    };


                return new ModuleExposeSyntax
                {
                    something = originalExposeSyntax.something.AddRange(nodesToAppend),
                };
            });
        }

        static public string ExposeCustomTypeAllTagsInElmModule(string originalElmModuleText, string customTypeName)
        {
            return AdaptModuleExposeSyntax(originalElmModuleText, originalExposeSyntax =>
            {
                if (originalExposeSyntax.everything != null)
                    return originalExposeSyntax;

                var expandedNodes =
                    originalExposeSyntax.something
                    .Select(originalNode =>
                    {
                        if (originalNode.otherTokens != null)
                            return originalNode;

                        if (originalNode.exposeValue.name != customTypeName)
                            return originalNode;

                        if (originalNode.exposeValue.subvalues?.everything != null)
                            return originalNode;

                        return new ModuleExposeSyntax.ModuleExposeSyntaxNode
                        {
                            exposeValue = new ModuleExposeSyntax.ModuleExposeValue
                            {
                                name = originalNode.exposeValue.name,
                                tokensBetweenNameAndDetails = "(",
                                subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { everything = ".." },
                                tokensAfterDetails = ")"
                            }
                        };
                    }).ToImmutableList();

                var nodesToAppend =
                    expandedNodes.Any(node => node?.exposeValue?.name == customTypeName)
                    ?
                    new ModuleExposeSyntax.ModuleExposeSyntaxNode[] { }
                    :
                    new[]{
                        new ModuleExposeSyntax.ModuleExposeSyntaxNode
                        {
                            otherTokens = ", ",
                        },
                        new ModuleExposeSyntax.ModuleExposeSyntaxNode
                        {
                            exposeValue =
                                new ModuleExposeSyntax.ModuleExposeValue
                                {
                                    name = customTypeName,
                                    tokensBetweenNameAndDetails = "(",
                                    subvalues = new ModuleExposeSyntax.ModuleExposeValueSubvalues { everything = ".." },
                                    tokensAfterDetails = ")"
                                }
                        }
                    };

                return new ModuleExposeSyntax
                {
                    something = expandedNodes.AddRange(nodesToAppend),
                };
            });
        }

        static string AdaptModuleExposeSyntax(
            string originalElmModuleText,
            Func<ModuleExposeSyntax, ModuleExposeSyntax> adaptExposeSyntax)
        {
            var findAndParseResult = FindAndParseModuleExposeSyntax(originalElmModuleText);

            if (findAndParseResult == null)
                throw new Exception("Failed to find and parse original expose syntax for module:\n" + originalElmModuleText);

            return
                originalElmModuleText.Substring(0, findAndParseResult.Value.beginIndex) +
                adaptExposeSyntax(findAndParseResult.Value.parsedSyntax).ToString() +
                originalElmModuleText.Substring(findAndParseResult.Value.beginIndex + findAndParseResult.Value.parsedLength);
        }
    }
}